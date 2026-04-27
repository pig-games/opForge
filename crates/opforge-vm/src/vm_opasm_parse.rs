use std::cell::RefCell;
use std::rc::Rc;

use opcore::parser::{AssignOp, Expr, Label, LineAst, ParseError};
use opcore::tokenizer::{OperatorKind, Span, Token, TokenKind};
use registry::syntax::{register_checker_none, RegisterChecker};
use types::processing::{ProcessingOutcome, ProcessingRequestKind};

use crate::portable_contract::PortableLineAst;
use crate::runtime_diagnostics::RuntimeBridgeDiagnostic;
use crate::runtime_error::RuntimeBridgeError;
use crate::runtime_parse_utils::{parse_span_at_end, runtime_bridge_error_to_parse_error};
use crate::tokenizer_runtime_utils;
use crate::vm_opcore::parse_expr_with_vm_contract;
use crate::vm_opcore::HierarchyExecutionModel;

use crate::execution_model::directives::parse_dot_directive_line_from_tokens;
use crate::execution_model::parser_vm::parse_line_with_parser_vm;
use types::line_ast::{AssignmentAst, StatementAst};

pub trait ExprProcessingHandler {
    fn process_expr_request(
        &mut self,
        request: ProcessingRequestKind,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> ProcessingOutcome<Expr, ParseError>;
}

pub type DynExprProcessingHandler<'a> = Rc<RefCell<Box<dyn ExprProcessingHandler + 'a>>>;

#[derive(Clone)]
pub(crate) struct VmExprParseContext<'a> {
    pub(crate) model: &'a HierarchyExecutionModel,
    pub(crate) cpu_id: &'a str,
    pub(crate) dialect_override: Option<&'a str>,
    pub(crate) expr_handler: Option<DynExprProcessingHandler<'a>>,
}

#[derive(Clone)]
pub(crate) struct ParserVmExecContext<'a> {
    pub(crate) source_line: &'a str,
    pub(crate) line_num: u32,
    pub(crate) expr_parse_ctx: VmExprParseContext<'a>,
}

pub(super) fn parse_portable_line_for_assembler(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
) -> Result<PortableLineAst, ParseError> {
    let register_checker = register_checker_none();
    let (line_ast, _, _) = parse_line_with_model(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        &register_checker,
    )?;
    Ok(PortableLineAst::from_core_line_ast(&line_ast))
}

pub fn tokenize_parser_tokens_with_model(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> Result<(Vec<Token>, Span, Option<String>), ParseError> {
    tokenizer_runtime_utils::validate_line_column_one(line, line_num)?;
    let portable_tokens = model
        .tokenize_portable_statement_for_assembler(cpu_id, dialect_override, line, line_num)
        .map_err(|err| {
            runtime_bridge_error_to_parse_error(err, parse_span_at_end(line, line_num))
        })?;

    let core_tokens = tokenizer_runtime_utils::runtime_tokens_to_core_tokens(
        &portable_tokens,
        Some(line),
        register_checker,
    )?;
    let (end_span, end_token_text) =
        tokenizer_runtime_utils::parser_end_metadata(line, line_num, &core_tokens);
    Ok((core_tokens, end_span, end_token_text))
}

pub fn parse_line_with_model(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> Result<(LineAst, Span, Option<String>), ParseError> {
    parse_line_with_model_with_expr_handler(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
        None,
    )
}

pub fn parse_line_with_model_with_expr_handler<'a>(
    model: &'a HierarchyExecutionModel,
    cpu_id: &'a str,
    dialect_override: Option<&'a str>,
    line: &'a str,
    line_num: u32,
    register_checker: &RegisterChecker,
    expr_handler: Option<DynExprProcessingHandler<'a>>,
) -> Result<(LineAst, Span, Option<String>), ParseError> {
    let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
    )?;
    let parser_contract = model
        .validate_parser_contract_for_assembler(cpu_id, dialect_override, tokens.len())
        .map_err(|err| {
            runtime_bridge_error_to_parse_error(err, parse_span_at_end(line, line_num))
        })?;
    let parser_vm_program = model
        .resolve_parser_vm_program(cpu_id, dialect_override)
        .map_err(|err| runtime_bridge_error_to_parse_error(err, parse_span_at_end(line, line_num)))?
        .ok_or_else(|| {
            runtime_bridge_error_to_parse_error(
                RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic::new(
                    parser_contract.diagnostics.invalid_statement.as_str(),
                    "missing parser VM program for active CPU pipeline",
                    None,
                )),
                parse_span_at_end(line, line_num),
            )
        })?;
    model
        .enforce_parser_vm_program_budget_for_assembler(&parser_contract, &parser_vm_program)
        .map_err(|err| {
            runtime_bridge_error_to_parse_error(err, parse_span_at_end(line, line_num))
        })?;
    let line_ast = parse_line_with_parser_vm(
        tokens,
        end_span,
        end_token_text.clone(),
        &parser_contract,
        &parser_vm_program,
        ParserVmExecContext {
            source_line: line,
            line_num,
            expr_parse_ctx: VmExprParseContext {
                model,
                cpu_id,
                dialect_override,
                expr_handler,
            },
        },
    )?;
    Ok((line_ast, end_span, end_token_text))
}

pub(super) fn parse_dot_directive_envelope_from_tokens(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    expr_parse_ctx: &VmExprParseContext<'_>,
) -> Result<Option<LineAst>, ParseError> {
    if tokens.is_empty() {
        return Ok(None);
    }
    let (label, idx) = parse_optional_leading_label(tokens);
    if !matches!(
        tokens.get(idx),
        Some(Token {
            kind: TokenKind::Dot,
            ..
        })
    ) {
        return Ok(None);
    }
    if match_assignment_op_at(tokens, idx).is_some() {
        return Ok(None);
    }
    parse_dot_directive_line_from_tokens(
        tokens,
        idx,
        label,
        end_span,
        end_token_text,
        expr_parse_ctx,
    )
    .map(Some)
}

pub(super) fn parse_star_org_envelope_from_tokens(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    expr_parse_ctx: &VmExprParseContext<'_>,
) -> Result<Option<LineAst>, ParseError> {
    if tokens.is_empty() {
        return Ok(None);
    }
    let (label, idx) = parse_optional_leading_label(tokens);
    parse_star_org_at(tokens, idx, label, end_span, end_token_text, expr_parse_ctx)
}

fn parse_star_org_at(
    tokens: &[Token],
    idx: usize,
    label: Option<Label>,
    end_span: Span,
    end_token_text: Option<String>,
    expr_parse_ctx: &VmExprParseContext<'_>,
) -> Result<Option<LineAst>, ParseError> {
    if label.is_some() || !is_star_org_assignment(tokens, idx) {
        return Ok(None);
    }
    if idx.saturating_add(2) >= tokens.len() {
        return Err(ParseError {
            message: "Expected expression".to_string(),
            span: end_span,
        });
    }
    let expr = parse_expr_with_vm_contract(
        expr_parse_ctx,
        &tokens[idx.saturating_add(2)..],
        end_span,
        end_token_text,
    )?;
    Ok(Some(LineAst::Statement(StatementAst {
        label: None,
        mnemonic: Some(".org".to_string()),
        operands: vec![expr],
    })))
}

pub(super) fn parse_assignment_envelope_from_tokens(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    expr_parse_ctx: &VmExprParseContext<'_>,
) -> Result<Option<LineAst>, ParseError> {
    if tokens.is_empty() {
        return Ok(None);
    }
    let (label, idx) = parse_optional_leading_label(tokens);
    parse_assignment_at(tokens, idx, label, end_span, end_token_text, expr_parse_ctx)
}

fn parse_assignment_at(
    tokens: &[Token],
    idx: usize,
    label: Option<Label>,
    end_span: Span,
    end_token_text: Option<String>,
    expr_parse_ctx: &VmExprParseContext<'_>,
) -> Result<Option<LineAst>, ParseError> {
    let Some(label) = label else {
        return Ok(None);
    };
    let Some((op, span, consumed)) = match_assignment_op_at(tokens, idx) else {
        return Ok(None);
    };
    let expr = match tokens.get(idx.saturating_add(consumed)) {
        Some(_) => match parse_expr_with_vm_contract(
            expr_parse_ctx,
            &tokens[idx.saturating_add(consumed)..],
            end_span,
            end_token_text,
        ) {
            Ok(expr) => expr,
            Err(err) => Expr::Error(err.message, err.span),
        },
        None => Expr::Error("Expected expression".to_string(), end_span),
    };
    Ok(Some(LineAst::Assignment(AssignmentAst {
        label,
        op,
        expr,
        span,
    })))
}

fn parse_optional_leading_label(tokens: &[Token]) -> (Option<Label>, usize) {
    let Some(first) = tokens.first() else {
        return (None, 0);
    };
    let label_name = match &first.kind {
        TokenKind::Identifier(name) | TokenKind::Register(name) => Some(name.clone()),
        _ => None,
    };
    let Some(name) = label_name else {
        return (None, 0);
    };
    if first.span.col_start != 1 {
        return (None, 0);
    }
    if let Some(colon) = tokens.get(1) {
        if matches!(colon.kind, TokenKind::Colon) && colon.span.col_start == first.span.col_end {
            return (
                Some(Label {
                    name,
                    span: first.span,
                }),
                2,
            );
        }
        return (
            Some(Label {
                name,
                span: first.span,
            }),
            1,
        );
    }
    (
        Some(Label {
            name,
            span: first.span,
        }),
        1,
    )
}

fn is_star_org_assignment(tokens: &[Token], idx: usize) -> bool {
    matches!(
        tokens.get(idx),
        Some(Token {
            kind: TokenKind::Operator(OperatorKind::Multiply),
            ..
        })
    ) && matches!(
        tokens.get(idx.saturating_add(1)),
        Some(Token {
            kind: TokenKind::Operator(OperatorKind::Eq),
            ..
        })
    )
}

fn match_assignment_op_at(tokens: &[Token], idx: usize) -> Option<(AssignOp, Span, usize)> {
    let token = tokens.get(idx)?;
    let next = tokens.get(idx.saturating_add(1));
    let next2 = tokens.get(idx.saturating_add(2));
    match &token.kind {
        TokenKind::Operator(OperatorKind::Eq) => Some((AssignOp::Const, token.span, 1)),
        TokenKind::Colon => {
            if matches!(
                next,
                Some(Token {
                    kind: TokenKind::Question,
                    ..
                })
            ) && matches!(
                next2,
                Some(Token {
                    kind: TokenKind::Operator(OperatorKind::Eq),
                    ..
                })
            ) {
                Some((AssignOp::VarIfUndef, token.span, 3))
            } else if matches!(
                next,
                Some(Token {
                    kind: TokenKind::Operator(OperatorKind::Eq),
                    ..
                })
            ) {
                Some((AssignOp::Var, token.span, 2))
            } else {
                None
            }
        }
        TokenKind::Operator(kind) => {
            if *kind == OperatorKind::RangeInclusive {
                return Some((AssignOp::Concat, token.span, 1));
            }
            let op = match kind {
                OperatorKind::Plus => AssignOp::Add,
                OperatorKind::Minus => AssignOp::Sub,
                OperatorKind::Multiply => AssignOp::Mul,
                OperatorKind::Divide => AssignOp::Div,
                OperatorKind::Mod => AssignOp::Mod,
                OperatorKind::Power => AssignOp::Pow,
                OperatorKind::BitOr => AssignOp::BitOr,
                OperatorKind::BitXor => AssignOp::BitXor,
                OperatorKind::BitAnd => AssignOp::BitAnd,
                OperatorKind::LogicOr => AssignOp::LogicOr,
                OperatorKind::LogicAnd => AssignOp::LogicAnd,
                OperatorKind::Shl => AssignOp::Shl,
                OperatorKind::Shr => AssignOp::Shr,
                OperatorKind::Lt => {
                    if matches!(
                        next,
                        Some(Token {
                            kind: TokenKind::Question,
                            ..
                        })
                    ) && matches!(
                        next2,
                        Some(Token {
                            kind: TokenKind::Operator(OperatorKind::Eq),
                            ..
                        })
                    ) {
                        return Some((AssignOp::Min, token.span, 3));
                    }
                    return None;
                }
                OperatorKind::Gt => {
                    if matches!(
                        next,
                        Some(Token {
                            kind: TokenKind::Question,
                            ..
                        })
                    ) && matches!(
                        next2,
                        Some(Token {
                            kind: TokenKind::Operator(OperatorKind::Eq),
                            ..
                        })
                    ) {
                        return Some((AssignOp::Max, token.span, 3));
                    }
                    return None;
                }
                _ => return None,
            };
            if matches!(
                next,
                Some(Token {
                    kind: TokenKind::Operator(OperatorKind::Eq),
                    ..
                })
            ) {
                Some((op, token.span, 2))
            } else {
                None
            }
        }
        TokenKind::Dot => {
            if matches!(
                next,
                Some(Token {
                    kind: TokenKind::Dot,
                    ..
                })
            ) && matches!(
                next2,
                Some(Token {
                    kind: TokenKind::Operator(OperatorKind::Eq),
                    ..
                })
            ) {
                Some((AssignOp::Concat, token.span, 3))
            } else if matches!(
                next,
                Some(Token {
                    kind: TokenKind::Operator(OperatorKind::Eq),
                    ..
                })
            ) {
                Some((AssignOp::Member, token.span, 2))
            } else {
                None
            }
        }
        TokenKind::Identifier(name) => {
            if name.eq_ignore_ascii_case("x")
                && matches!(
                    next,
                    Some(Token {
                        kind: TokenKind::Operator(OperatorKind::Eq),
                        ..
                    })
                )
            {
                Some((AssignOp::Repeat, token.span, 2))
            } else {
                None
            }
        }
        _ => None,
    }
}
