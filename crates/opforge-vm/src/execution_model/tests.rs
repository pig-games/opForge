use super::directives::parse_use_directive_from_tokens;
use super::parser_vm::parse_line_with_parser_vm;
use super::*;
use crate::vm_opasm::{
    parse_statement_line_with_model as parse_line_with_model,
    tokenize_statement_line_with_model as tokenize_parser_tokens_with_model,
};
use crate::vm_opasm_parse::{ParserVmExecContext, VmExprParseContext};
use crate::vm_opcore::{parse_expr_program_ref_with_vm_contract, parse_expr_with_vm_contract};
use families::{
    register_intel8080_family_stack, register_mos6502_family_stack,
    register_motorola6800_family_stack,
};
use opcore::parser::BinaryOp;
use opcore::parser::{AssignOp, Expr, LineAst, ParseError};
use opcore::tokenizer::{Span, Token, TokenKind};
use package::{ParserVmOpcodeV2, PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT};
use registry::registry::ModuleRegistry;
use registry::{
    parse_pack_directive_from_tokens, parse_place_directive_from_tokens_with,
    parser_from_line_with_registers, register_checker_none,
};
use std::{fs, path::PathBuf, sync::OnceLock};
use types::line_ast::{ConditionalAst, PackAst, PlaceAst, UseAst};

const DEFAULT_TOKENIZER_CPU_ID: &str = "m6502";

#[derive(Debug, PartialEq, Eq)]
enum NormalizedExprDiag {
    None,
    ParseError { message: String, span: Span },
    ExprError { message: String, span: Span },
}

fn first_expr_error_from_ast(ast: &LineAst) -> Option<(String, Span)> {
    fn find_in_exprs(exprs: &[Expr]) -> Option<(String, Span)> {
        for expr in exprs {
            if let Expr::Error(message, span) = expr {
                return Some((message.clone(), *span));
            }
        }
        None
    }

    match ast {
        LineAst::Statement(statement) => find_in_exprs(&statement.operands),
        LineAst::Assignment(assignment) => {
            if let Expr::Error(message, span) = &assignment.expr {
                Some((message.clone(), *span))
            } else {
                None
            }
        }
        LineAst::Conditional(ConditionalAst { exprs, .. }) => find_in_exprs(exprs),
        LineAst::Place(PlaceAst { align, .. }) => align.as_ref().and_then(|expr| {
            if let Expr::Error(message, span) = expr {
                Some((message.clone(), *span))
            } else {
                None
            }
        }),
        LineAst::Use(UseAst { params, .. }) => {
            for param in params {
                if let Expr::Error(message, span) = &param.value {
                    return Some((message.clone(), *span));
                }
            }
            None
        }
        _ => None,
    }
}

fn normalize_expr_diag(result: Result<LineAst, ParseError>) -> NormalizedExprDiag {
    match result {
        Ok(ast) => first_expr_error_from_ast(&ast).map_or(NormalizedExprDiag::None, |diag| {
            NormalizedExprDiag::ExprError {
                message: diag.0,
                span: diag.1,
            }
        }),
        Err(err) => NormalizedExprDiag::ParseError {
            message: err.message,
            span: err.span,
        },
    }
}

fn build_default_registry_for_tests() -> ModuleRegistry {
    let mut registry = ModuleRegistry::new();
    register_intel8080_family_stack(&mut registry);
    register_mos6502_family_stack(&mut registry);
    register_motorola6800_family_stack(&mut registry);
    registry
}

fn default_runtime_model() -> Option<&'static HierarchyExecutionModel> {
    static MODEL: OnceLock<Option<HierarchyExecutionModel>> = OnceLock::new();
    MODEL.get_or_init(build_default_runtime_model).as_ref()
}

fn build_default_runtime_model() -> Option<HierarchyExecutionModel> {
    HierarchyExecutionModel::from_registry(&build_default_registry_for_tests()).ok()
}

fn parse_line_with_default_model(line: &str, line_num: u32) -> Result<LineAst, ParseError> {
    let model = default_runtime_model().ok_or_else(|| ParseError {
        message: "VM tokenizer runtime model is unavailable".to_string(),
        span: Span {
            line: line_num,
            col_start: 1,
            col_end: 1,
        },
    })?;
    let register_checker = register_checker_none();
    let (line_ast, _, _) = parse_line_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        line,
        line_num,
        &register_checker,
    )?;
    Ok(line_ast)
}

#[test]
fn default_model_resolves_bridge_cpu_to_mos6502_family() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let resolved = model
        .resolve_pipeline(DEFAULT_TOKENIZER_CPU_ID, None)
        .expect("default tokenizer cpu should resolve");
    assert_eq!(resolved.family_id.to_ascii_lowercase(), "mos6502");
    assert!(model
        .resolve_parser_contract(DEFAULT_TOKENIZER_CPU_ID, None)
        .expect("parser contract resolution")
        .is_some());
}

#[test]
fn parse_line_with_default_model_smoke() {
    let line = parse_line_with_default_model("    LDA #$42", 1).expect("line should parse");
    match line {
        LineAst::Statement(statement) => {
            let Some(mnemonic) = statement.mnemonic else {
                panic!("expected mnemonic in instruction line ast");
            };
            assert_eq!(mnemonic.to_ascii_lowercase(), "lda");
        }
        other => panic!("expected instruction line ast, got {other:?}"),
    }
}

#[test]
fn parse_line_with_default_model_parses_use_directive() {
    let line =
        parse_line_with_default_model("    .use math as m", 1).expect(".use line should parse");
    match line {
        LineAst::Use(UseAst {
            module_id,
            alias,
            items,
            params,
            ..
        }) => {
            assert_eq!(module_id, "math");
            assert_eq!(alias.as_deref(), Some("m"));
            assert!(items.is_empty());
            assert!(params.is_empty());
        }
        other => panic!("expected .use AST, got {other:?}"),
    }
}

#[test]
fn parse_line_with_default_model_parses_place_directive() {
    let line = parse_line_with_default_model("    .place code in ram, align=16", 1)
        .expect(".place line should parse");
    match line {
        LineAst::Place(PlaceAst {
            section,
            region,
            align,
            ..
        }) => {
            assert_eq!(section, "code");
            assert_eq!(region, "ram");
            assert!(align.is_some());
        }
        other => panic!("expected .place AST, got {other:?}"),
    }
}

#[test]
fn parse_line_with_default_model_parses_pack_directive() {
    let line = parse_line_with_default_model("    .pack in rom: code,data", 1)
        .expect(".pack line should parse");
    match line {
        LineAst::Pack(PackAst {
            region, sections, ..
        }) => {
            assert_eq!(region, "rom");
            assert_eq!(sections, vec!["code".to_string(), "data".to_string()]);
        }
        other => panic!("expected .pack AST, got {other:?}"),
    }
}

#[test]
fn parse_line_with_default_model_parses_for_directive_head() {
    let line = parse_line_with_default_model(".for i in 0..8", 1).expect(".for line should parse");
    match line {
        LineAst::Statement(statement) => {
            assert_eq!(statement.mnemonic.as_deref(), Some(".for"));
            assert_eq!(statement.operands.len(), 2);
            assert!(matches!(statement.operands[0], Expr::Identifier(ref name, _) if name == "i"));
            assert!(matches!(statement.operands[1], Expr::Range { .. }));
        }
        other => panic!("expected .for AST, got {other:?}"),
    }
}

#[test]
fn parse_line_with_default_model_parses_bwhile_directive_head() {
    let line = parse_line_with_default_model(".bwhile addr < $c100", 1)
        .expect(".bwhile line should parse");
    match line {
        LineAst::Statement(statement) => {
            assert_eq!(statement.mnemonic.as_deref(), Some(".bwhile"));
            assert_eq!(statement.operands.len(), 1);
            assert!(matches!(
                statement.operands[0],
                Expr::Binary {
                    op: BinaryOp::Lt,
                    ..
                }
            ));
        }
        other => panic!("expected .bwhile AST, got {other:?}"),
    }
}

#[test]
fn parse_line_with_default_model_parses_struct_directive_with_operand() {
    let line = parse_line_with_default_model(".struct Point", 1)
        .expect(".struct with operand should parse");
    match line {
        LineAst::Statement(statement) => {
            assert_eq!(statement.mnemonic.as_deref(), Some(".struct"));
            assert_eq!(statement.operands.len(), 1);
            assert!(
                matches!(statement.operands[0], Expr::Identifier(ref name, _) if name == "Point")
            );
        }
        other => panic!("expected .struct AST, got {other:?}"),
    }
}

#[test]
fn parse_line_with_default_model_rejects_trailing_tokens_after_endfor() {
    let err = parse_line_with_default_model(".endfor 1", 1)
        .expect_err("trailing tokens after .endfor should fail");
    assert!(err.message.contains("Unexpected trailing tokens"));
}

#[test]
fn parse_use_directive_from_tokens_parses_selective_alias_and_params() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    .use math(foo as f,bar) with(width=1+2, mask=$ff)";
    let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("tokenization should succeed");
    let mut cursor = 2;
    let expr_parse_ctx = VmExprParseContext {
        model,
        cpu_id: DEFAULT_TOKENIZER_CPU_ID,
        dialect_override: None,
        expr_handler: None,
    };

    let parsed = parse_use_directive_from_tokens(
        &tokens,
        &mut cursor,
        tokens[1].span,
        end_span,
        end_token_text,
        &expr_parse_ctx,
    )
    .expect(".use directive parse should succeed");

    match parsed {
        LineAst::Use(UseAst {
            module_id,
            alias,
            items,
            params,
            ..
        }) => {
            assert_eq!(module_id, "math");
            assert_eq!(alias, None);
            assert_eq!(items.len(), 2);
            assert_eq!(items[0].name, "foo");
            assert_eq!(items[0].alias.as_deref(), Some("f"));
            assert_eq!(items[1].name, "bar");
            assert_eq!(items[1].alias, None);
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "width");
            assert_eq!(params[1].name, "mask");
            assert!(!matches!(params[0].value, Expr::Error(_, _)));
            assert!(!matches!(params[1].value, Expr::Error(_, _)));
        }
        other => panic!("expected .use AST, got {other:?}"),
    }
}

#[test]
fn parse_use_directive_from_tokens_rejects_wildcard_alias() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    .use math(* as all)";
    let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("tokenization should succeed");
    let mut cursor = 2;
    let expr_parse_ctx = VmExprParseContext {
        model,
        cpu_id: DEFAULT_TOKENIZER_CPU_ID,
        dialect_override: None,
        expr_handler: None,
    };

    let err = parse_use_directive_from_tokens(
        &tokens,
        &mut cursor,
        tokens[1].span,
        end_span,
        end_token_text,
        &expr_parse_ctx,
    )
    .expect_err("wildcard alias should be rejected");

    assert!(
        err.message.contains("Wildcard import cannot have an alias"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn parse_place_directive_from_tokens_rejects_unknown_option_key() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    .place code in ram, wrong=16";
    let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("tokenization should succeed");
    let mut cursor = 2;
    let expr_parse_ctx = VmExprParseContext {
        model,
        cpu_id: DEFAULT_TOKENIZER_CPU_ID,
        dialect_override: None,
        expr_handler: None,
    };

    let err = parse_place_directive_from_tokens_with(
        &tokens,
        &mut cursor,
        tokens[1].span,
        end_span,
        |tail| parse_expr_with_vm_contract(&expr_parse_ctx, tail, end_span, end_token_text),
    )
    .expect_err("unknown option should be rejected");

    assert!(
        err.message.contains("Unknown .place option key"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn parse_pack_directive_from_tokens_requires_at_least_one_section() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    .pack in rom:";
    let (tokens, end_span, _) = tokenize_parser_tokens_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("tokenization should succeed");
    let mut cursor = 2;

    let err = parse_pack_directive_from_tokens(&tokens, &mut cursor, tokens[1].span, end_span)
        .expect_err("missing section list should be rejected");

    assert!(
        err.message
            .contains("Expected at least one section in .pack directive"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn parse_line_with_model_handles_instruction_line_through_default_v2_program() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let (parsed, _, _) = parse_line_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        "label: LDA ($10),Y",
        1,
        &register_checker,
    )
    .expect("default v2 parser should parse instruction line");
    match parsed {
        LineAst::Statement(statement) => {
            let label = statement.label.expect("expected label");
            let mnemonic = statement.mnemonic.expect("expected mnemonic");
            let operands = statement.operands;
            assert_eq!(label.name.to_ascii_lowercase(), "label");
            assert_eq!(mnemonic.to_ascii_lowercase(), "lda");
            assert_eq!(operands.len(), 2);
        }
        other => panic!("expected statement line ast, got {other:?}"),
    }
}

#[test]
fn parse_line_with_model_keeps_deferred_directive_line_working() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let (parsed, _, _) = parse_line_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        "    .if 1",
        1,
        &register_checker,
    )
    .expect("default v2 parser should defer directive line");
    assert!(
        matches!(parsed, LineAst::Conditional(..)),
        "directive line should parse as conditional, got {parsed:?}"
    );
}

#[test]
fn parse_line_with_parser_vm_rejects_retired_parse_core_line_opcode() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    LDA #$42";
    let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("tokenization should succeed");
    let parser_contract = model
        .validate_parser_contract_for_assembler(DEFAULT_TOKENIZER_CPU_ID, None, tokens.len())
        .expect("parser contract should validate");
    let parser_vm_program = RuntimeParserVmProgram {
        opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
        program: vec![0x07, ParserVmOpcodeV2::End as u8],
    };

    let err = parse_line_with_parser_vm(
        tokens,
        end_span,
        end_token_text,
        &parser_contract,
        &parser_vm_program,
        ParserVmExecContext {
            source_line: source,
            line_num: 1,
            expr_parse_ctx: VmExprParseContext {
                model,
                cpu_id: DEFAULT_TOKENIZER_CPU_ID,
                dialect_override: None,
                expr_handler: None,
            },
        },
    )
    .expect_err("retired opcode should fail");
    assert!(err.message.contains("cross-contract opcode 0x07"));
}

#[test]
fn parse_line_with_parser_vm_rejects_incompatible_contract_opcode_version() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    LDA #$42";
    let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("tokenization should succeed");
    let mut parser_contract = model
        .validate_parser_contract_for_assembler(DEFAULT_TOKENIZER_CPU_ID, None, tokens.len())
        .expect("parser contract should validate");
    parser_contract.opcode_version = PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT.saturating_add(1);
    let parser_vm_program = RuntimeParserVmProgram {
        opcode_version: parser_contract.opcode_version,
        program: vec![ParserVmOpcodeV2::Fail as u8, ParserVmOpcodeV2::End as u8],
    };

    let err = parse_line_with_parser_vm(
        tokens,
        end_span,
        end_token_text,
        &parser_contract,
        &parser_vm_program,
        ParserVmExecContext {
            source_line: source,
            line_num: 1,
            expr_parse_ctx: VmExprParseContext {
                model,
                cpu_id: DEFAULT_TOKENIZER_CPU_ID,
                dialect_override: None,
                expr_handler: None,
            },
        },
    )
    .expect_err("incompatible parser contract opcode version must fail");
    assert!(err
        .message
        .contains("unsupported parser contract opcode version"));
}

#[test]
fn parse_line_with_parser_vm_rejects_contract_program_opcode_version_mismatch() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    LDA #$42";
    let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("tokenization should succeed");
    let parser_contract = model
        .validate_parser_contract_for_assembler(DEFAULT_TOKENIZER_CPU_ID, None, tokens.len())
        .expect("parser contract should validate");
    let parser_vm_program = RuntimeParserVmProgram {
        opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT.saturating_add(1),
        program: vec![ParserVmOpcodeV2::Fail as u8, ParserVmOpcodeV2::End as u8],
    };

    let err = parse_line_with_parser_vm(
        tokens,
        end_span,
        end_token_text,
        &parser_contract,
        &parser_vm_program,
        ParserVmExecContext {
            source_line: source,
            line_num: 1,
            expr_parse_ctx: VmExprParseContext {
                model,
                cpu_id: DEFAULT_TOKENIZER_CPU_ID,
                dialect_override: None,
                expr_handler: None,
            },
        },
    )
    .expect_err("parser contract/program opcode version mismatch must fail");
    assert!(err
        .message
        .contains("parser contract/program opcode version mismatch"));
}

#[test]
fn parse_line_with_model_handles_assignment_through_default_v2_program() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "var2 += 1";
    let (line, _, _) = parse_line_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("default v2 parser should parse assignment");
    assert!(
        matches!(
            line,
            LineAst::Assignment(types::line_ast::AssignmentAst {
                op: AssignOp::Add,
                ..
            })
        ),
        "expected add assignment from assignment primitive, got {line:?}"
    );
}

#[test]
fn parse_line_with_model_handles_star_org_through_default_v2_program() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    * = $2000";
    let (line, _, _) = parse_line_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("default v2 parser should parse star org");
    assert!(
        matches!(
            line,
            LineAst::Statement(types::line_ast::StatementAst {
                mnemonic: Some(ref m),
                ..
            }) if m.eq_ignore_ascii_case(".org")
        ),
        "expected .org statement from star-org primitive, got {line:?}"
    );
}

#[test]
fn parse_line_with_model_handles_dot_assignment_ops_through_default_v2_program() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "cat ..= $3456";
    let (line, _, _) = parse_line_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("default v2 parser should parse dot assignment operator");
    assert!(
        matches!(
            line,
            LineAst::Assignment(types::line_ast::AssignmentAst {
                op: AssignOp::Concat,
                ..
            })
        ),
        "expected concat assignment to be parsed by assignment primitive, got {line:?}"
    );
}

#[test]
fn parse_line_with_parser_vm_emit_diag_if_no_result_reports_unexpected_token_slot() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    ?";
    let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("tokenization should succeed");
    let parser_contract = model
        .validate_parser_contract_for_assembler(DEFAULT_TOKENIZER_CPU_ID, None, tokens.len())
        .expect("parser contract should validate");
    let parser_vm_program = RuntimeParserVmProgram {
        opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
        program: vec![
            ParserVmOpcodeV2::EmitDiagIfNoResult as u8,
            0,
            ParserVmOpcodeV2::End as u8,
        ],
    };

    let err = parse_line_with_parser_vm(
        tokens,
        end_span,
        end_token_text,
        &parser_contract,
        &parser_vm_program,
        ParserVmExecContext {
            source_line: source,
            line_num: 1,
            expr_parse_ctx: VmExprParseContext {
                model,
                cpu_id: DEFAULT_TOKENIZER_CPU_ID,
                dialect_override: None,
                expr_handler: None,
            },
        },
    )
    .expect_err("unmatched line should emit terminal parser VM diagnostic");
    assert_eq!(
        err.message,
        format!(
            "{}: parser VM v2 emitted diagnostic slot 0",
            parser_contract.diagnostics.unexpected_token
        )
    );
}

#[test]
fn parse_line_with_parser_vm_emit_diag_if_no_result_requires_slot_operand() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    NOP";
    let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("tokenization should succeed");
    let parser_contract = model
        .validate_parser_contract_for_assembler(DEFAULT_TOKENIZER_CPU_ID, None, tokens.len())
        .expect("parser contract should validate");
    let parser_vm_program = RuntimeParserVmProgram {
        opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
        program: vec![ParserVmOpcodeV2::EmitDiagIfNoResult as u8],
    };

    let err = parse_line_with_parser_vm(
        tokens,
        end_span,
        end_token_text,
        &parser_contract,
        &parser_vm_program,
        ParserVmExecContext {
            source_line: source,
            line_num: 1,
            expr_parse_ctx: VmExprParseContext {
                model,
                cpu_id: DEFAULT_TOKENIZER_CPU_ID,
                dialect_override: None,
                expr_handler: None,
            },
        },
    )
    .expect_err("missing EmitDiagIfNoResult slot must fail");
    assert!(err
        .message
        .contains("parser VM v2 missing EmitDiagIfNoResult slot"));
}

#[test]
fn parse_line_with_parser_vm_emit_diag_requires_slot_operand() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let source = "    NOP";
    let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        source,
        1,
        &register_checker,
    )
    .expect("tokenization should succeed");
    let parser_contract = model
        .validate_parser_contract_for_assembler(DEFAULT_TOKENIZER_CPU_ID, None, tokens.len())
        .expect("parser contract should validate");
    let parser_vm_program = RuntimeParserVmProgram {
        opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
        program: vec![ParserVmOpcodeV2::EmitDiag as u8],
    };

    let err = parse_line_with_parser_vm(
        tokens,
        end_span,
        end_token_text,
        &parser_contract,
        &parser_vm_program,
        ParserVmExecContext {
            source_line: source,
            line_num: 1,
            expr_parse_ctx: VmExprParseContext {
                model,
                cpu_id: DEFAULT_TOKENIZER_CPU_ID,
                dialect_override: None,
                expr_handler: None,
            },
        },
    )
    .expect_err("missing EmitDiag slot must fail");
    assert!(err.message.contains("parser VM v2 missing EmitDiag slot"));
}

#[test]
fn opasm_parser_source_has_no_retired_envelope_helpers() {
    let vm_manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let sources = [
        vm_manifest.join("src/vm_opasm_parse.rs"),
        vm_manifest.join("src/execution_model/parser_vm.rs"),
        vm_manifest.join("src/execution_model/parser_vm_v2.rs"),
        vm_manifest.join("../opforge-package/src/package.rs"),
    ];
    let retired_suffix = ["_envelope", "_from_tokens"].concat();

    for source in sources {
        let text = fs::read_to_string(&source).expect("parser source should be readable");
        assert!(
            !text.contains(&retired_suffix),
            "retired parser helper suffix found in {}",
            source.display()
        );
    }
}

#[test]
fn parse_line_with_model_preserves_expression_diagnostic_shape_and_span_parity() {
    let model = default_runtime_model().expect("default runtime model should be available");
    let register_checker = register_checker_none();
    let corpus = [
        "label = 1 +",
        "    LDA #(",
        "    .if 1 +",
        "    .place code in ram, align=1+",
        "    .use foo with(x=1+)",
    ];

    for (idx, line) in corpus.iter().enumerate() {
        let line_num = (idx + 1) as u32;
        let bridge = parse_line_with_model(
            model,
            DEFAULT_TOKENIZER_CPU_ID,
            None,
            line,
            line_num,
            &register_checker,
        )
        .map(|(ast, _, _)| ast);
        let host = parser_from_line_with_registers(line, line_num, register_checker.clone())
            .and_then(|mut parser| parser.parse_compat_mixed_line());
        let bridge_diag = normalize_expr_diag(bridge);
        let host_diag = normalize_expr_diag(host);
        assert_eq!(
            bridge_diag, host_diag,
            "expression diagnostic parity mismatch for line {:?}",
            line
        );
    }
}

#[test]
fn parse_line_with_model_requires_expression_contract_compatibility() {
    let registry = build_default_registry_for_tests();
    let mut chunks = crate::builder::build_hierarchy_chunks_from_registry(&registry)
        .expect("hierarchy chunks build");
    for contract in &mut chunks.parser_contracts {
        if matches!(
            contract.owner,
            crate::hierarchy::ScopedOwner::Family(ref family_id)
                if family_id.eq_ignore_ascii_case("mos6502")
        ) {
            contract.grammar_id = "opforge.line.v0".to_string();
        }
    }
    let model = HierarchyExecutionModel::from_chunks(chunks).expect("execution model should build");
    let register_checker = register_checker_none();
    let err = parse_line_with_model(
        &model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        "    NOP",
        1,
        &register_checker,
    )
    .expect_err("incompatible parser contract should fail before AST parsing");
    let message = err.message;
    assert_eq!(
        message,
        "otp004: unsupported parser grammar id 'opforge.line.v0'"
    );
}

#[test]
fn parse_line_with_model_requires_parser_ast_schema_compatibility() {
    let registry = build_default_registry_for_tests();
    let mut chunks = crate::builder::build_hierarchy_chunks_from_registry(&registry)
        .expect("hierarchy chunks build");
    for contract in &mut chunks.parser_contracts {
        if matches!(
            contract.owner,
            crate::hierarchy::ScopedOwner::Family(ref family_id)
                if family_id.eq_ignore_ascii_case("mos6502")
        ) {
            contract.ast_schema_id = "opforge.ast.line.v0".to_string();
        }
    }
    let model = HierarchyExecutionModel::from_chunks(chunks).expect("execution model should build");
    let register_checker = register_checker_none();
    let err = parse_line_with_model(
        &model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        "    NOP",
        1,
        &register_checker,
    )
    .expect_err("incompatible parser contract should fail before AST parsing");
    let message = err.message;
    assert_eq!(
        message,
        "otp004: unsupported parser AST schema id 'opforge.ast.line.v0'"
    );
}

#[test]
fn parse_line_with_model_enforces_parser_vm_program_byte_budget() {
    let registry = build_default_registry_for_tests();
    let mut chunks = crate::builder::build_hierarchy_chunks_from_registry(&registry)
        .expect("hierarchy chunks build");
    for program in &mut chunks.parser_vm_programs {
        if matches!(
            program.owner,
            crate::hierarchy::ScopedOwner::Family(ref family_id)
                if family_id.eq_ignore_ascii_case("mos6502")
        ) {
            program.program = vec![ParserVmOpcodeV2::BeginStatement as u8; 100];
        }
    }
    let mut model =
        HierarchyExecutionModel::from_chunks(chunks).expect("execution model should build");
    model.set_runtime_budget_profile(
        crate::runtime_model_types::RuntimeBudgetProfile::RetroConstrained,
    );
    let register_checker = register_checker_none();
    let err = parse_line_with_model(
        &model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        "    NOP",
        1,
        &register_checker,
    )
    .expect_err("oversized parser VM program should fail in retro profile");
    assert!(err
        .message
        .contains("parser VM program byte budget exceeded"));
}

#[test]
fn parse_line_with_model_parser_vm_budget_error_is_deterministic() {
    let registry = build_default_registry_for_tests();
    let mut chunks = crate::builder::build_hierarchy_chunks_from_registry(&registry)
        .expect("hierarchy chunks build");
    for program in &mut chunks.parser_vm_programs {
        if matches!(
            program.owner,
            crate::hierarchy::ScopedOwner::Family(ref family_id)
                if family_id.eq_ignore_ascii_case("mos6502")
        ) {
            program.program = vec![ParserVmOpcodeV2::BeginStatement as u8; 100];
        }
    }
    let mut model =
        HierarchyExecutionModel::from_chunks(chunks).expect("execution model should build");
    model.set_runtime_budget_profile(
        crate::runtime_model_types::RuntimeBudgetProfile::RetroConstrained,
    );
    let register_checker = register_checker_none();
    let first = parse_line_with_model(
        &model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        "    NOP",
        1,
        &register_checker,
    )
    .expect_err("oversized parser VM program should fail in retro profile");
    let second = parse_line_with_model(
        &model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        "    NOP",
        1,
        &register_checker,
    )
    .expect_err("oversized parser VM program should fail in retro profile");
    assert_eq!(first.message, second.message);
    assert_eq!(first.span, second.span);
}

#[test]
fn parse_expr_with_vm_contract_rejects_slice_above_parser_token_budget() {
    let registry = build_default_registry_for_tests();
    let mut model = HierarchyExecutionModel::from_chunks(
        crate::builder::build_hierarchy_chunks_from_registry(&registry)
            .expect("hierarchy chunks build"),
    )
    .expect("execution model should build");
    model.set_runtime_budget_profile(
        crate::runtime_model_types::RuntimeBudgetProfile::RetroConstrained,
    );
    let token_budget = model.runtime_budget_limits().max_parser_tokens_per_line;
    let span = Span {
        line: 1,
        col_start: 1,
        col_end: 2,
    };
    let tokens = vec![
        Token {
            kind: TokenKind::Identifier("A".to_string()),
            span,
        };
        token_budget.saturating_add(1)
    ];
    let err = parse_expr_with_vm_contract(
        &VmExprParseContext {
            model: &model,
            cpu_id: DEFAULT_TOKENIZER_CPU_ID,
            dialect_override: None,
            expr_handler: None,
        },
        tokens.as_slice(),
        span,
        None,
    )
    .expect_err("expression slice above parser token budget should fail");
    assert!(
        err.message
            .starts_with("otp004: parser token budget exceeded"),
        "unexpected parser token budget diagnostic: {}",
        err.message
    );
}

#[test]
fn parse_expr_program_ref_with_vm_contract_enforces_vm_contract_for_intel_family() {
    let registry = build_default_registry_for_tests();
    let mut chunks = crate::builder::build_hierarchy_chunks_from_registry(&registry)
        .expect("hierarchy chunks build");
    for contract in &mut chunks.expr_parser_contracts {
        if matches!(
            contract.owner,
            crate::hierarchy::ScopedOwner::Family(ref family_id)
                if family_id.eq_ignore_ascii_case("intel8080")
        ) {
            contract.opcode_version = package::EXVM_OPCODE_VERSION_V1.saturating_add(1);
        }
    }
    let model = HierarchyExecutionModel::from_chunks(chunks).expect("execution model should build");

    let span = Span {
        line: 1,
        col_start: 1,
        col_end: 3,
    };
    let tokens = vec![Token {
        kind: TokenKind::Identifier("value".to_string()),
        span,
    }];

    let err = parse_expr_program_ref_with_vm_contract(
        &VmExprParseContext {
            model: &model,
            cpu_id: "8085",
            dialect_override: None,
            expr_handler: None,
        },
        tokens.as_slice(),
        span,
        None,
        None,
    )
    .expect_err("intel family should enforce EXVM contract compatibility");
    assert!(
        err.message
            .to_ascii_lowercase()
            .contains("unsupported expression parser contract opcode version"),
        "expected expression parser contract compatibility failure, got: {}",
        err.message
    );
}

#[test]
fn parse_expr_program_ref_with_vm_contract_uses_vm_path_for_enabled_family() {
    let registry = build_default_registry_for_tests();
    let mut chunks = crate::builder::build_hierarchy_chunks_from_registry(&registry)
        .expect("hierarchy chunks build");
    for contract in &mut chunks.expr_parser_contracts {
        if matches!(
            contract.owner,
            crate::hierarchy::ScopedOwner::Family(ref family_id)
                if family_id.eq_ignore_ascii_case("mos6502")
        ) {
            contract.opcode_version = package::EXVM_OPCODE_VERSION_V1.saturating_add(1);
        }
    }
    let model = HierarchyExecutionModel::from_chunks(chunks).expect("execution model should build");

    let span = Span {
        line: 1,
        col_start: 1,
        col_end: 3,
    };
    let tokens = vec![Token {
        kind: TokenKind::Identifier("value".to_string()),
        span,
    }];

    let err = parse_expr_program_ref_with_vm_contract(
        &VmExprParseContext {
            model: &model,
            cpu_id: DEFAULT_TOKENIZER_CPU_ID,
            dialect_override: None,
            expr_handler: None,
        },
        tokens.as_slice(),
        span,
        None,
        None,
    )
    .expect_err("enabled family should enforce EXVM contract compatibility");
    assert!(
        err.message
            .contains("unsupported expression parser contract opcode version"),
        "expected VM-path expression parser contract compatibility error, got: {}",
        err.message
    );
}
