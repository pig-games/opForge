// SPDX-License-Identifier: GPL-3.0-or-later

use opcore::parser::{BinaryOp, Expr, LineAst, ParseError, Parser, UnaryOp};
use opcore::services::parse_expression_tokens as parse_stable_opcore_expression_tokens;
use opcore::tokenizer::{Span, Token};
use registry::syntax::{register_checker_none, RegisterChecker};
use types::lockstep::{
    ContinuationHead, ExecutionMode, LockstepCheckpoint, LockstepComparisonCategory,
    LockstepDivergence, LockstepMatch, LockstepReport, LockstepStage,
};
use types::processing::{
    LineProcessingTrace, OpcoreRequestKind, ProcessingOutcome, ProcessingRequestKind,
    ProcessingReturn, ProcessorError, ProcessorErrorKind, ProcessorFailureDetail,
};
use vm::vm_opasm::HierarchyExecutionModel;
use vm::vm_opcore::parse_expression_tokens as parse_vm_expression_tokens;
use vm::vm_opcore::process_module_item_request_with_model as process_module_item_request_vm;

#[derive(Debug, Clone)]
pub enum EngineError {
    Core(ParseError),
    Processor(ProcessorError),
}

impl EngineError {
    fn invalid_request(
        processor_id: impl Into<String>,
        code: impl Into<String>,
        summary: impl Into<String>,
        field: Option<impl Into<String>>,
    ) -> Self {
        let summary = summary.into();
        let detail = ProcessorFailureDetail::new(code.into(), summary.clone(), field);
        Self::Processor(ProcessorError::new(
            processor_id,
            ProcessorErrorKind::InvalidRequest,
            detail.code().to_string(),
            summary,
            vec![detail],
        ))
    }

    fn processor_diagnostic(
        processor_id: impl Into<String>,
        code: impl Into<String>,
        summary: impl Into<String>,
        field: Option<impl Into<String>>,
    ) -> Self {
        let summary = summary.into();
        let detail = ProcessorFailureDetail::new(code.into(), summary.clone(), field);
        Self::Processor(ProcessorError::new(
            processor_id,
            ProcessorErrorKind::ProcessorDiagnostic,
            detail.code().to_string(),
            summary,
            vec![detail],
        ))
    }
}

pub fn process_opcore_expression_request(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> ProcessingOutcome<Expr, ParseError> {
    process_opcore_expression_request_with_mode(
        ExecutionMode::Rust,
        tokens,
        end_span,
        end_token_text,
    )
    .0
}

pub fn process_opcore_expression_request_with_mode(
    mode: ExecutionMode,
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> (ProcessingOutcome<Expr, ParseError>, LockstepReport) {
    let request = ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr);
    match mode {
        ExecutionMode::Rust => (
            process_opcore_expression_request_rust(tokens, end_span, end_token_text),
            LockstepReport::default(),
        ),
        ExecutionMode::Vm => (
            process_opcore_expression_request_vm(tokens, end_span, end_token_text),
            LockstepReport::default(),
        ),
        ExecutionMode::Lockstep { continuation_head } => {
            process_opcore_expression_request_lockstep(
                request,
                continuation_head,
                tokens,
                end_span,
                end_token_text,
            )
        }
    }
}

fn process_opcore_expression_request_rust(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> ProcessingOutcome<Expr, ParseError> {
    match parse_stable_opcore_expression_tokens(tokens, end_span, end_token_text) {
        Ok(expr) => ProcessingOutcome::Done(expr),
        Err(err) => ProcessingOutcome::Error(err),
    }
}

fn process_opcore_expression_request_vm(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> ProcessingOutcome<Expr, ParseError> {
    match parse_vm_expression_tokens(tokens, end_span, end_token_text) {
        Ok(expr) => ProcessingOutcome::Done(expr),
        Err(err) => ProcessingOutcome::Error(err),
    }
}

pub fn route_module_item_line(
    line: &str,
    line_num: u32,
) -> Result<(Option<LineAst>, LineProcessingTrace), EngineError> {
    route_module_item_line_with_default_model(crate::editor_default_runtime_model(), line, line_num)
}

fn route_module_item_line_with_default_model(
    model: Option<&HierarchyExecutionModel>,
    line: &str,
    line_num: u32,
) -> Result<(Option<LineAst>, LineProcessingTrace), EngineError> {
    let model = default_runtime_model_or_err(model, line_num)?;
    let register_checker = register_checker_none();
    route_module_item_line_with_model(
        model,
        crate::DEFAULT_TOKENIZER_CPU_ID,
        None,
        line,
        line_num,
        &register_checker,
    )
}

pub fn route_module_item_line_with_model(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> Result<(Option<LineAst>, LineProcessingTrace), EngineError> {
    let mut trace = LineProcessingTrace::default();
    let request = ProcessingRequestKind::Opcore(OpcoreRequestKind::ModuleItem);
    trace.push(request);
    finish_module_item_route(
        process_module_item_request_vm(
            model,
            cpu_id,
            dialect_override,
            line,
            line_num,
            register_checker,
        ),
        trace,
        line_num,
    )
}

fn finish_module_item_route(
    outcome: ProcessingOutcome<LineAst, ParseError>,
    mut trace: LineProcessingTrace,
    line_num: u32,
) -> Result<(Option<LineAst>, LineProcessingTrace), EngineError> {
    match outcome {
        ProcessingOutcome::Done(ast) => Ok((Some(ast), trace)),
        ProcessingOutcome::Return(ProcessingReturn::Unknown) => Ok((None, trace)),
        ProcessingOutcome::Return(ProcessingReturn::Request { request }) => {
            trace.push(request);
            Err(EngineError::invalid_request(
                "engine",
                "processing.request.unsupported",
                "Unsupported processor return for module-item routing",
                Some(format!("line:{line_num}")),
            ))
        }
        ProcessingOutcome::Error(err) => Err(EngineError::Core(err)),
    }
}

pub fn editor_route_line(
    line: &str,
    line_num: u32,
) -> Result<(LineAst, LineProcessingTrace), EngineError> {
    editor_route_line_with_default_model(crate::editor_default_runtime_model(), line, line_num)
}

fn editor_route_line_with_default_model(
    model: Option<&HierarchyExecutionModel>,
    line: &str,
    line_num: u32,
) -> Result<(LineAst, LineProcessingTrace), EngineError> {
    let model = default_runtime_model_or_err(model, line_num)?;
    let register_checker = register_checker_none();
    editor_route_line_with_model(
        model,
        crate::DEFAULT_TOKENIZER_CPU_ID,
        None,
        line,
        line_num,
        &register_checker,
    )
}

fn default_runtime_model_or_err(
    model: Option<&HierarchyExecutionModel>,
    line_num: u32,
) -> Result<&HierarchyExecutionModel, EngineError> {
    model.ok_or_else(|| runtime_model_unavailable_error(line_num))
}

fn runtime_model_unavailable_error(line_num: u32) -> EngineError {
    EngineError::invalid_request(
        "asm",
        "processing.runtime_model.unavailable",
        "VM tokenizer runtime model is unavailable",
        Some(format!("line:{line_num}")),
    )
}

pub fn editor_route_line_with_model(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> Result<(LineAst, LineProcessingTrace), EngineError> {
    let (ast, trace, _) = editor_route_line_with_model_in_mode(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
        ExecutionMode::Vm,
    )?;
    Ok((ast, trace))
}

pub fn editor_route_line_with_model_in_mode(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
    execution_mode: ExecutionMode,
) -> Result<(LineAst, LineProcessingTrace, LockstepReport), EngineError> {
    let mut trace = LineProcessingTrace::default();
    let mut lockstep_report = LockstepReport::default();
    let request = ProcessingRequestKind::Opcore(OpcoreRequestKind::Statement);
    trace.push(request.clone());

    match process_opcore_statement_request(line, line_num) {
        ProcessingOutcome::Done(ast) => Ok((ast, trace, lockstep_report)),
        ProcessingOutcome::Error(err) => Err(EngineError::Core(err)),
        ProcessingOutcome::Return(ProcessingReturn::Request { request }) => {
            let ctx = ProcessorLineRequestContext {
                model,
                cpu_id,
                dialect_override,
                line,
                line_num,
                register_checker,
                execution_mode,
                trace: &mut trace,
                lockstep_report: &mut lockstep_report,
            };
            route_processor_line_request(ctx, request).map(|ast| (ast, trace, lockstep_report))
        }
        ProcessingOutcome::Return(ProcessingReturn::Unknown) => Err(EngineError::invalid_request(
            "engine",
            "processing.request.unclaimed",
            "No processor claimed the line",
            Some(format!("line:{line_num}")),
        )),
    }
}

fn process_opcore_statement_request(
    line: &str,
    line_num: u32,
) -> ProcessingOutcome<LineAst, ParseError> {
    // Temporary bypass: the stable `opcore` surface intentionally exposes
    // expression and module-item services, but not a full-line statement
    // classifier yet. Engine-managed line routing still needs this opcore-first
    // compatibility classifier until that narrower stable entrypoint exists.
    Parser::process_opcore_line_request(line, line_num)
}

struct ProcessorLineRequestContext<'a> {
    model: &'a HierarchyExecutionModel,
    cpu_id: &'a str,
    dialect_override: Option<&'a str>,
    line: &'a str,
    line_num: u32,
    register_checker: &'a RegisterChecker,
    execution_mode: ExecutionMode,
    trace: &'a mut LineProcessingTrace,
    lockstep_report: &'a mut LockstepReport,
}

fn route_processor_line_request(
    ctx: ProcessorLineRequestContext<'_>,
    request: ProcessingRequestKind,
) -> Result<LineAst, EngineError> {
    match request {
        ProcessingRequestKind::Processor {
            ref processor,
            ref kind,
        } if processor == "asm" && kind == "statement" => route_opasm_statement_request(ctx),
        other => Err(EngineError::invalid_request(
            "engine",
            "processing.request.unsupported",
            format!("Unsupported processor request: {other:?}"),
            Some(format!("line:{}", ctx.line_num)),
        )),
    }
}

struct EngineExprProcessingHandler {
    execution_mode: ExecutionMode,
}

impl asm::opasm::StatementExprProcessor for EngineExprProcessingHandler {
    fn process_expr_request(
        &mut self,
        request: ProcessingRequestKind,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> (ProcessingOutcome<Expr, ParseError>, LockstepReport) {
        match request {
            ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr) => {
                process_opcore_expression_request_with_mode(
                    self.execution_mode,
                    tokens,
                    end_span,
                    end_token_text,
                )
            }
            other => (
                ProcessingOutcome::Return(ProcessingReturn::Request { request: other }),
                LockstepReport::default(),
            ),
        }
    }
}

fn route_opasm_statement_request(
    ctx: ProcessorLineRequestContext<'_>,
) -> Result<LineAst, EngineError> {
    let mut expr_handler = EngineExprProcessingHandler {
        execution_mode: ctx.execution_mode,
    };
    let result = asm::opasm::process_statement(
        asm::opasm::StatementRequest::new(ctx.line, ctx.line_num)
            .with_execution_mode(ctx.execution_mode)
            .with_model(ctx.model, ctx.cpu_id, ctx.dialect_override)
            .with_register_checker(ctx.register_checker),
        Some(&mut expr_handler),
    )
    .map_err(|err| {
        EngineError::processor_diagnostic(
            "asm",
            "processing.processor_diagnostic",
            err.message,
            None::<String>,
        )
    })?;
    for request in result.trace.requests() {
        ctx.trace.push(request.clone());
    }
    ctx.lockstep_report.extend(result.lockstep_report);
    Ok(result.parsed.ast)
}

fn process_opcore_expression_request_lockstep(
    request: ProcessingRequestKind,
    continuation_head: ContinuationHead,
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> (ProcessingOutcome<Expr, ParseError>, LockstepReport) {
    let rust_outcome =
        process_opcore_expression_request_rust(tokens.clone(), end_span, end_token_text.clone());
    let vm_outcome = process_opcore_expression_request_vm(tokens, end_span, end_token_text);
    let report = record_expr_lockstep_result(
        request,
        continuation_head,
        Some(end_span.line),
        &rust_outcome,
        &vm_outcome,
    );

    let selected = match continuation_head {
        ContinuationHead::Rust => rust_outcome,
        ContinuationHead::Vm => vm_outcome,
    };
    (selected, report)
}

fn record_expr_lockstep_result(
    request: ProcessingRequestKind,
    continuation_head: ContinuationHead,
    source_line: Option<u32>,
    rust_outcome: &ProcessingOutcome<Expr, ParseError>,
    vm_outcome: &ProcessingOutcome<Expr, ParseError>,
) -> LockstepReport {
    let mut report = LockstepReport::default();
    let (left, right, category, reason_code) =
        build_expr_lockstep_comparison(rust_outcome, vm_outcome);

    if left == right {
        report.push_match(LockstepMatch {
            stage: LockstepStage::OpcoreExpr,
            request,
            category,
        });
    } else {
        report.push_divergence(LockstepDivergence {
            stage: LockstepStage::OpcoreExpr,
            processor_domain: "opcore".to_string(),
            request,
            continuation_head,
            source_line,
            active_cpu: None,
            active_dialect: None,
            left,
            right,
            category,
            reason_code: reason_code.to_string(),
        });
    }

    report
}

fn build_expr_lockstep_comparison(
    rust_outcome: &ProcessingOutcome<Expr, ParseError>,
    vm_outcome: &ProcessingOutcome<Expr, ParseError>,
) -> (
    LockstepCheckpoint,
    LockstepCheckpoint,
    LockstepComparisonCategory,
    &'static str,
) {
    match (rust_outcome, vm_outcome) {
        (ProcessingOutcome::Done(left), ProcessingOutcome::Done(right)) => (
            LockstepCheckpoint::CoreExprAst {
                normalized: normalize_expr_checkpoint(left),
            },
            LockstepCheckpoint::CoreExprAst {
                normalized: normalize_expr_checkpoint(right),
            },
            LockstepComparisonCategory::Ast,
            "expr-ast-mismatch",
        ),
        (ProcessingOutcome::Error(left), ProcessingOutcome::Error(right)) => (
            LockstepCheckpoint::Diagnostic {
                normalized: normalize_parse_error_checkpoint(left),
            },
            LockstepCheckpoint::Diagnostic {
                normalized: normalize_parse_error_checkpoint(right),
            },
            LockstepComparisonCategory::Diagnostics,
            "expr-diagnostic-mismatch",
        ),
        (ProcessingOutcome::Done(left), ProcessingOutcome::Error(right)) => (
            LockstepCheckpoint::CoreExprAst {
                normalized: normalize_expr_checkpoint(left),
            },
            LockstepCheckpoint::Diagnostic {
                normalized: normalize_parse_error_checkpoint(right),
            },
            LockstepComparisonCategory::Ast,
            "expr-success-vs-error",
        ),
        (ProcessingOutcome::Error(left), ProcessingOutcome::Done(right)) => (
            LockstepCheckpoint::Diagnostic {
                normalized: normalize_parse_error_checkpoint(left),
            },
            LockstepCheckpoint::CoreExprAst {
                normalized: normalize_expr_checkpoint(right),
            },
            LockstepComparisonCategory::Ast,
            "expr-error-vs-success",
        ),
        _ => (
            LockstepCheckpoint::Diagnostic {
                normalized: "unsupported-return".to_string(),
            },
            LockstepCheckpoint::Diagnostic {
                normalized: "unsupported-return".to_string(),
            },
            LockstepComparisonCategory::Diagnostics,
            "unsupported-processing-return",
        ),
    }
}

fn normalize_parse_error_checkpoint(err: &ParseError) -> String {
    format!(
        "{}@{}:{}-{}",
        err.message, err.span.line, err.span.col_start, err.span.col_end
    )
}

fn normalize_expr_checkpoint(expr: &Expr) -> String {
    match expr {
        Expr::Number(text, _) => format!("num({text})"),
        Expr::Identifier(name, _) => format!("ident({name})"),
        Expr::Register(name, _) => format!("reg({name})"),
        Expr::List(items, _) => format!(
            "list({})",
            items
                .iter()
                .map(normalize_expr_checkpoint)
                .collect::<Vec<_>>()
                .join(",")
        ),
        Expr::Index { base, index, .. } => format!(
            "index({}, {})",
            normalize_expr_checkpoint(base),
            normalize_expr_checkpoint(index)
        ),
        Expr::Member { base, field, .. } => {
            format!("member({}, {field})", normalize_expr_checkpoint(base))
        }
        Expr::StructLiteral {
            type_name, fields, ..
        } => format!(
            "struct({type_name}, {})",
            fields
                .iter()
                .map(|(name, value)| format!("{name}={}", normalize_expr_checkpoint(value)))
                .collect::<Vec<_>>()
                .join(",")
        ),
        Expr::Call { name, args, .. } => format!(
            "call({name}, {})",
            args.iter()
                .map(normalize_expr_checkpoint)
                .collect::<Vec<_>>()
                .join(",")
        ),
        Expr::Placeholder(_) => "placeholder".to_string(),
        Expr::Indirect(inner, _) => format!("indirect({})", normalize_expr_checkpoint(inner)),
        Expr::Immediate(inner, _) => format!("immediate({})", normalize_expr_checkpoint(inner)),
        Expr::IndirectLong(inner, _) => {
            format!("indirect-long({})", normalize_expr_checkpoint(inner))
        }
        Expr::Tuple(items, _) => format!(
            "tuple({})",
            items
                .iter()
                .map(normalize_expr_checkpoint)
                .collect::<Vec<_>>()
                .join(",")
        ),
        Expr::Dollar(_) => "dollar".to_string(),
        Expr::String(bytes, _) => format!("string({bytes:?})"),
        Expr::Error(message, _) => format!("expr-error({message})"),
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => format!(
            "ternary({}, {}, {})",
            normalize_expr_checkpoint(cond),
            normalize_expr_checkpoint(then_expr),
            normalize_expr_checkpoint(else_expr)
        ),
        Expr::Unary { op, expr, .. } => {
            format!(
                "unary({}, {})",
                normalize_unary_op(*op),
                normalize_expr_checkpoint(expr)
            )
        }
        Expr::Binary {
            op, left, right, ..
        } => format!(
            "binary({}, {}, {})",
            normalize_binary_op(*op),
            normalize_expr_checkpoint(left),
            normalize_expr_checkpoint(right)
        ),
        Expr::Range {
            start,
            end,
            step,
            inclusive,
            ..
        } => format!(
            "range({}, {}, {}, {})",
            normalize_expr_checkpoint(start),
            normalize_expr_checkpoint(end),
            step.as_deref()
                .map(normalize_expr_checkpoint)
                .unwrap_or_else(|| "none".to_string()),
            inclusive
        ),
    }
}

fn normalize_unary_op(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Plus => "plus",
        UnaryOp::Minus => "minus",
        UnaryOp::BitNot => "bit-not",
        UnaryOp::LogicNot => "logic-not",
        UnaryOp::High => "high",
        UnaryOp::Low => "low",
    }
}

fn normalize_binary_op(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Multiply => "mul",
        BinaryOp::Divide => "div",
        BinaryOp::Mod => "mod",
        BinaryOp::Power => "pow",
        BinaryOp::Shl => "shl",
        BinaryOp::Shr => "shr",
        BinaryOp::Add => "add",
        BinaryOp::Subtract => "sub",
        BinaryOp::Eq => "eq",
        BinaryOp::Ne => "ne",
        BinaryOp::Ge => "ge",
        BinaryOp::Gt => "gt",
        BinaryOp::Le => "le",
        BinaryOp::Lt => "lt",
        BinaryOp::BitAnd => "bit-and",
        BinaryOp::BitOr => "bit-or",
        BinaryOp::BitXor => "bit-xor",
        BinaryOp::LogicAnd => "logic-and",
        BinaryOp::LogicOr => "logic-or",
        BinaryOp::LogicXor => "logic-xor",
    }
}

#[cfg(test)]
mod tests {
    use super::{
        editor_route_line_with_model, editor_route_line_with_model_in_mode,
        finish_module_item_route, process_opcore_expression_request,
        process_opcore_expression_request_with_mode, record_expr_lockstep_result,
        route_module_item_line, route_module_item_line_with_model, ContinuationHead, EngineError,
        ExecutionMode, LineProcessingTrace, LockstepStage, OpcoreRequestKind, ProcessingOutcome,
        ProcessingRequestKind, ProcessingReturn, ProcessorErrorKind,
    };
    use opcore::parser::{Expr, LineAst};
    use opcore::tokenizer::{Span, Token, TokenKind, Tokenizer};
    use registry::syntax::register_checker_none;

    fn collect_tokens(line: &str) -> (Vec<Token>, Span) {
        let mut tokenizer = Tokenizer::new(line, 1);
        let mut tokens = Vec::new();
        let end_span = loop {
            let token = tokenizer.next_token().expect("tokenization should succeed");
            if matches!(token.kind, TokenKind::End) {
                break token.span;
            }
            tokens.push(token);
        };
        (tokens, end_span)
    }

    #[test]
    fn process_opcore_expression_request_parses_expression_tokens() {
        let (tokens, end_span) = collect_tokens("value + 1");
        let outcome = process_opcore_expression_request(tokens, end_span, None);
        match outcome {
            ProcessingOutcome::Done(Expr::Binary { .. }) => {}
            other => panic!("expected parsed binary expression, got {other:?}"),
        }
    }

    #[test]
    fn process_opcore_expression_request_lockstep_records_match() {
        let (tokens, end_span) = collect_tokens("value + 1");
        let (outcome, report) = process_opcore_expression_request_with_mode(
            ExecutionMode::Lockstep {
                continuation_head: ContinuationHead::Rust,
            },
            tokens,
            end_span,
            None,
        );

        match outcome {
            ProcessingOutcome::Done(Expr::Binary { .. }) => {}
            other => panic!("expected parsed binary expression, got {other:?}"),
        }
        assert_eq!(report.matches().len(), 1);
        assert_eq!(report.divergences().len(), 0);
        assert_eq!(report.matches()[0].stage, LockstepStage::OpcoreExpr);
    }

    #[test]
    fn record_expr_lockstep_result_captures_divergence() {
        let span = Span {
            line: 7,
            col_start: 3,
            col_end: 8,
        };
        let rust_outcome = ProcessingOutcome::Done(Expr::Number("1".to_string(), span));
        let vm_outcome = ProcessingOutcome::Done(Expr::Number("2".to_string(), span));

        let report = record_expr_lockstep_result(
            ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr),
            ContinuationHead::Rust,
            Some(span.line),
            &rust_outcome,
            &vm_outcome,
        );

        assert_eq!(report.matches().len(), 0);
        assert_eq!(report.divergences().len(), 1);
        let divergence = &report.divergences()[0];
        assert_eq!(divergence.stage, LockstepStage::OpcoreExpr);
        assert_eq!(divergence.reason_code, "expr-ast-mismatch");
        assert_eq!(divergence.source_line, Some(7));
    }

    #[test]
    fn editor_route_line_with_model_keeps_use_in_opcore() {
        let model = crate::editor_default_runtime_model().expect("default runtime model");
        let register_checker = register_checker_none();
        let (ast, trace) = editor_route_line_with_model(
            model,
            crate::DEFAULT_TOKENIZER_CPU_ID,
            None,
            ".use math as m",
            1,
            &register_checker,
        )
        .expect("line should route");

        assert!(matches!(ast, LineAst::Use(..)));
        assert_eq!(
            trace.requests(),
            &[ProcessingRequestKind::Opcore(OpcoreRequestKind::Statement)]
        );
    }

    #[test]
    fn editor_route_line_with_model_falls_back_to_asm_for_instruction() {
        let model = crate::editor_default_runtime_model().expect("default runtime model");
        let register_checker = register_checker_none();
        let (ast, trace) = editor_route_line_with_model(
            model,
            "8085",
            None,
            "    MVI A,1+2",
            1,
            &register_checker,
        )
        .expect("line should route");

        match ast {
            LineAst::Statement(statement) => {
                let Some(mnemonic) = statement.mnemonic else {
                    panic!("expected instruction mnemonic");
                };
                assert_eq!(mnemonic.to_ascii_lowercase(), "mvi")
            }
            other => panic!("expected instruction AST, got {other:?}"),
        }
        assert_eq!(
            trace.requests(),
            &[
                ProcessingRequestKind::Opcore(OpcoreRequestKind::Statement),
                ProcessingRequestKind::Processor {
                    processor: "asm".to_string(),
                    kind: "statement".to_string(),
                },
                ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr),
                ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr),
            ]
        );
    }

    #[test]
    fn editor_route_line_with_model_in_mode_records_lockstep_expression_match() {
        let model = crate::editor_default_runtime_model().expect("default runtime model");
        let register_checker = register_checker_none();
        let (ast, trace, report) = editor_route_line_with_model_in_mode(
            model,
            "8085",
            None,
            "    MVI A,1+2",
            1,
            &register_checker,
            ExecutionMode::Lockstep {
                continuation_head: ContinuationHead::Rust,
            },
        )
        .expect("line should route");

        assert!(matches!(ast, LineAst::Statement(..)));
        assert_eq!(
            trace.requests(),
            &[
                ProcessingRequestKind::Opcore(OpcoreRequestKind::Statement),
                ProcessingRequestKind::Processor {
                    processor: "asm".to_string(),
                    kind: "statement".to_string(),
                },
                ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr),
                ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr),
            ]
        );
        assert!(report
            .matches()
            .iter()
            .any(|entry| entry.stage == LockstepStage::OpcoreExpr));
        assert!(report
            .matches()
            .iter()
            .any(|entry| entry.stage == LockstepStage::OpasmStatementParse));
        assert_eq!(report.divergences().len(), 0);
    }

    #[test]
    fn route_module_item_line_traces_module_item_request() {
        let (ast, trace) = route_module_item_line(".use math as m", 1).expect("line should route");
        assert!(matches!(ast, Some(LineAst::Use(..))));
        assert_eq!(
            trace.requests(),
            &[ProcessingRequestKind::Opcore(OpcoreRequestKind::ModuleItem)]
        );
    }

    #[test]
    fn route_module_item_line_without_runtime_model_returns_shared_error() {
        let err = super::route_module_item_line_with_default_model(None, ".module demo", 7)
            .expect_err("missing runtime model should error");

        match err {
            super::EngineError::Processor(err) => {
                assert_eq!(err.processor_id(), "asm");
                assert_eq!(err.kind(), super::ProcessorErrorKind::InvalidRequest);
                assert_eq!(err.code(), "processing.runtime_model.unavailable");
                assert_eq!(err.summary(), "VM tokenizer runtime model is unavailable");
                assert_eq!(err.details().len(), 1);
            }
            other => panic!("expected processor error, got {other:?}"),
        }
    }

    #[test]
    fn default_processing_helpers_split_core_and_processor_runtime_model_contracts() {
        let route_err = super::route_module_item_line_with_default_model(None, ".module demo", 9)
            .expect_err("module-item routing should require a runtime model");
        let editor_err = super::editor_route_line_with_default_model(None, ".module demo", 9)
            .expect_err("editor routing should require a runtime model");

        match route_err {
            super::EngineError::Processor(err) => {
                assert_eq!(err.processor_id(), "asm");
                assert_eq!(err.kind(), super::ProcessorErrorKind::InvalidRequest);
                assert_eq!(err.code(), "processing.runtime_model.unavailable");
                assert_eq!(err.summary(), "VM tokenizer runtime model is unavailable");
            }
            other => panic!("expected module-item processor error, got {other:?}"),
        }
        match editor_err {
            super::EngineError::Processor(err) => {
                assert_eq!(err.processor_id(), "asm");
                assert_eq!(err.kind(), super::ProcessorErrorKind::InvalidRequest);
                assert_eq!(err.code(), "processing.runtime_model.unavailable");
                assert_eq!(err.summary(), "VM tokenizer runtime model is unavailable");
            }
            other => panic!("expected processor error, got {other:?}"),
        }
    }

    #[test]
    fn route_module_item_line_with_model_parses_use_through_vm_stage() {
        let model = crate::editor_default_runtime_model().expect("default runtime model");
        let register_checker = register_checker_none();
        let (ast, trace) = route_module_item_line_with_model(
            model,
            crate::DEFAULT_TOKENIZER_CPU_ID,
            None,
            ".use math as m",
            1,
            &register_checker,
        )
        .expect("line should route");
        assert!(matches!(ast, Some(LineAst::Use(..))));
        assert_eq!(
            trace.requests(),
            &[ProcessingRequestKind::Opcore(OpcoreRequestKind::ModuleItem)]
        );
    }

    #[test]
    fn route_module_item_line_with_model_parses_module_through_vm_stage() {
        let model = crate::editor_default_runtime_model().expect("default runtime model");
        let register_checker = register_checker_none();
        let (ast, trace) = route_module_item_line_with_model(
            model,
            crate::DEFAULT_TOKENIZER_CPU_ID,
            None,
            ".module demo",
            1,
            &register_checker,
        )
        .expect("line should route");
        assert!(matches!(ast, Some(LineAst::Statement(..))));
        assert_eq!(
            trace.requests(),
            &[ProcessingRequestKind::Opcore(OpcoreRequestKind::ModuleItem)]
        );
    }

    #[test]
    fn finish_module_item_route_maps_unsupported_returns_to_invalid_request() {
        let err = finish_module_item_route(
            ProcessingOutcome::Return(ProcessingReturn::Request {
                request: ProcessingRequestKind::Processor {
                    processor: "asm".to_string(),
                    kind: "statement".to_string(),
                },
            }),
            LineProcessingTrace::default(),
            1,
        )
        .expect_err("unsupported module-item return should surface invalid request");

        match err {
            EngineError::Processor(err) => {
                assert_eq!(err.processor_id(), "engine");
                assert_eq!(err.kind(), ProcessorErrorKind::InvalidRequest);
                assert_eq!(err.code(), "processing.request.unsupported");
                assert_eq!(
                    err.summary(),
                    "Unsupported processor return for module-item routing"
                );
                assert_eq!(err.details().len(), 1);
            }
            other => panic!("expected processor error, got {other:?}"),
        }
    }
}
