// SPDX-License-Identifier: GPL-3.0-or-later

//! Lower-level assembler processor services.

use std::cell::RefCell;
use std::rc::Rc;

use opcore::parser::{Expr, LineAst, ParseError, Parser};
use opcore::tokenizer::{Span, Token};
use registry::syntax::{
    register_checker_none, tokenize_statement_line_with_registers, RegisterChecker,
};
use types::lockstep::{
    ContinuationHead, ExecutionMode, LockstepCheckpoint, LockstepComparisonCategory,
    LockstepDivergence, LockstepMatch, LockstepReport, LockstepStage,
};
use types::processing::{LineProcessingTrace, ProcessingOutcome, ProcessingRequestKind};
use vm::portable_contract::PortableLineAst;
use vm::vm_opasm::{
    parse_statement_line_with_model_and_expr_handler_with_rollout_overrides,
    parse_statement_line_with_model_with_rollout_overrides,
    parse_statement_tokens_with_model_and_expr_handler_with_rollout_overrides,
    HierarchyExecutionModel,
};

#[derive(Debug, Clone)]
pub struct TokenizedStatement {
    pub tokens: Vec<Token>,
    pub end_span: Span,
    pub end_token_text: Option<String>,
}

#[derive(Clone, Copy)]
pub struct StatementRequest<'a> {
    pub execution_mode: ExecutionMode,
    pub model: Option<&'a HierarchyExecutionModel>,
    pub cpu_id: &'a str,
    pub dialect_override: Option<&'a str>,
    pub expr_parser_opt_in_families: &'a [String],
    pub expr_parser_force_host_families: &'a [String],
    pub line: &'a str,
    pub line_num: u32,
    pub register_checker: &'a RegisterChecker,
    pub collect_processing_trace: bool,
    pub pretokenized: Option<&'a TokenizedStatement>,
}

impl<'a> StatementRequest<'a> {
    pub fn new(line: &'a str, line_num: u32) -> Self {
        Self {
            execution_mode: ExecutionMode::Rust,
            model: None,
            cpu_id: "",
            dialect_override: None,
            expr_parser_opt_in_families: &[],
            expr_parser_force_host_families: &[],
            line,
            line_num,
            register_checker: default_register_checker(),
            collect_processing_trace: true,
            pretokenized: None,
        }
    }

    pub fn with_execution_mode(mut self, execution_mode: ExecutionMode) -> Self {
        self.execution_mode = execution_mode;
        self
    }

    pub fn with_model(
        mut self,
        model: &'a HierarchyExecutionModel,
        cpu_id: &'a str,
        dialect_override: Option<&'a str>,
    ) -> Self {
        self.model = Some(model);
        self.cpu_id = cpu_id;
        self.dialect_override = dialect_override;
        self
    }

    pub fn with_expr_parser_rollout_overrides(
        mut self,
        expr_parser_opt_in_families: &'a [String],
        expr_parser_force_host_families: &'a [String],
    ) -> Self {
        self.expr_parser_opt_in_families = expr_parser_opt_in_families;
        self.expr_parser_force_host_families = expr_parser_force_host_families;
        self
    }

    pub fn with_register_checker(mut self, register_checker: &'a RegisterChecker) -> Self {
        self.register_checker = register_checker;
        self
    }

    pub fn with_processing_trace(mut self, collect_processing_trace: bool) -> Self {
        self.collect_processing_trace = collect_processing_trace;
        self
    }

    pub fn with_pretokenized(mut self, pretokenized: Option<&'a TokenizedStatement>) -> Self {
        self.pretokenized = pretokenized;
        self
    }
}

#[derive(Debug, Clone)]
pub struct StatementParseResult {
    pub ast: LineAst,
    pub end_span: Span,
    pub end_token_text: Option<String>,
}

#[derive(Debug, Clone)]
pub struct StatementProcessResult {
    pub parsed: StatementParseResult,
    pub trace: LineProcessingTrace,
    pub lockstep_report: LockstepReport,
}

pub trait StatementExprProcessor {
    fn process_expr_request(
        &mut self,
        request: ProcessingRequestKind,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> (ProcessingOutcome<Expr, ParseError>, LockstepReport);
}

struct VmExprProcessorAdapter<'a, 'b> {
    processor: &'a mut dyn StatementExprProcessor,
    trace: &'b mut LineProcessingTrace,
    lockstep_report: &'b mut LockstepReport,
    collect_processing_trace: bool,
}

impl vm::vm_opasm::ExprProcessingHandler for VmExprProcessorAdapter<'_, '_> {
    fn process_expr_request(
        &mut self,
        request: ProcessingRequestKind,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> ProcessingOutcome<Expr, ParseError> {
        if self.collect_processing_trace {
            self.trace.push(request.clone());
        }
        let (outcome, report) =
            self.processor
                .process_expr_request(request, tokens, end_span, end_token_text);
        self.lockstep_report.extend(report);
        outcome
    }
}

pub fn default_register_checker() -> &'static RegisterChecker {
    static REGISTER_CHECKER: std::sync::OnceLock<RegisterChecker> = std::sync::OnceLock::new();
    REGISTER_CHECKER.get_or_init(register_checker_none)
}

pub fn tokenize_statement(request: StatementRequest<'_>) -> Result<TokenizedStatement, ParseError> {
    let (tokens, end_span, end_token_text) = tokenize_statement_line_with_registers(
        request.line,
        request.line_num,
        request.register_checker.clone(),
    )?;
    Ok(TokenizedStatement {
        tokens,
        end_span,
        end_token_text,
    })
}

pub fn parse_statement(request: StatementRequest<'_>) -> Result<StatementParseResult, ParseError> {
    Ok(process_statement(request, None)?.parsed)
}

pub fn process_statement(
    request: StatementRequest<'_>,
    expr_processor: Option<&mut dyn StatementExprProcessor>,
) -> Result<StatementProcessResult, ParseError> {
    let mut trace = LineProcessingTrace::default();
    if request.collect_processing_trace {
        trace.push(ProcessingRequestKind::Processor {
            processor: "asm".to_string(),
            kind: "statement".to_string(),
        });
    }
    let mut lockstep_report = LockstepReport::default();
    let parsed = match request.execution_mode {
        ExecutionMode::Rust => parse_statement_rust(request)?,
        ExecutionMode::Vm => {
            parse_statement_vm(request, expr_processor, &mut trace, &mut lockstep_report)?
        }
        ExecutionMode::Lockstep { continuation_head } => {
            let rust_parsed = parse_statement_rust(request);
            let vm_parsed =
                parse_statement_vm(request, expr_processor, &mut trace, &mut lockstep_report);
            lockstep_report.extend(record_lockstep_result(
                continuation_head,
                request,
                &rust_parsed,
                &vm_parsed,
            ));
            match continuation_head {
                ContinuationHead::Rust => rust_parsed?,
                ContinuationHead::Vm => vm_parsed?,
            }
        }
    };

    Ok(StatementProcessResult {
        parsed,
        trace,
        lockstep_report,
    })
}

fn parse_statement_rust(request: StatementRequest<'_>) -> Result<StatementParseResult, ParseError> {
    let tokenized = tokenize_statement(request)?;
    let mut parser = Parser::from_tokens(
        tokenized.tokens,
        tokenized.end_span,
        tokenized.end_token_text.clone(),
    );
    let ast = parser.parse_compat_mixed_line()?;
    Ok(StatementParseResult {
        ast,
        end_span: tokenized.end_span,
        end_token_text: tokenized.end_token_text,
    })
}

fn parse_statement_vm(
    request: StatementRequest<'_>,
    expr_processor: Option<&mut dyn StatementExprProcessor>,
    trace: &mut LineProcessingTrace,
    lockstep_report: &mut LockstepReport,
) -> Result<StatementParseResult, ParseError> {
    let model = request.model.ok_or_else(|| ParseError {
        message: "VM-backed opasm statement processing requires a runtime model".to_string(),
        span: Span {
            line: request.line_num,
            col_start: 1,
            col_end: 1,
        },
    })?;

    let parsed = if let Some(processor) = expr_processor {
        let expr_handler = Rc::new(RefCell::new(Box::new(VmExprProcessorAdapter {
            processor,
            trace,
            lockstep_report,
            collect_processing_trace: request.collect_processing_trace,
        })
            as Box<dyn vm::vm_opasm::ExprProcessingHandler + '_>));
        if let Some(tokenized) = request.pretokenized {
            parse_statement_tokens_with_model_and_expr_handler_with_rollout_overrides(
                model,
                request.cpu_id,
                request.dialect_override,
                request.expr_parser_opt_in_families,
                request.expr_parser_force_host_families,
                request.line,
                request.line_num,
                tokenized.tokens.clone(),
                tokenized.end_span,
                tokenized.end_token_text.clone(),
                Some(expr_handler),
            )
        } else {
            parse_statement_line_with_model_and_expr_handler_with_rollout_overrides(
                model,
                request.cpu_id,
                request.dialect_override,
                request.expr_parser_opt_in_families,
                request.expr_parser_force_host_families,
                request.line,
                request.line_num,
                request.register_checker,
                Some(expr_handler),
            )
        }
    } else if let Some(tokenized) = request.pretokenized {
        parse_statement_tokens_with_model_and_expr_handler_with_rollout_overrides(
            model,
            request.cpu_id,
            request.dialect_override,
            request.expr_parser_opt_in_families,
            request.expr_parser_force_host_families,
            request.line,
            request.line_num,
            tokenized.tokens.clone(),
            tokenized.end_span,
            tokenized.end_token_text.clone(),
            None,
        )
    } else {
        parse_statement_line_with_model_with_rollout_overrides(
            model,
            request.cpu_id,
            request.dialect_override,
            request.expr_parser_opt_in_families,
            request.expr_parser_force_host_families,
            request.line,
            request.line_num,
            request.register_checker,
        )
    }?;

    Ok(StatementParseResult {
        ast: parsed.0,
        end_span: parsed.1,
        end_token_text: parsed.2,
    })
}

fn record_lockstep_result(
    continuation_head: ContinuationHead,
    request: StatementRequest<'_>,
    rust_outcome: &Result<StatementParseResult, ParseError>,
    vm_outcome: &Result<StatementParseResult, ParseError>,
) -> LockstepReport {
    let mut report = LockstepReport::default();
    let (left, right, category, reason_code) = build_lockstep_comparison(rust_outcome, vm_outcome);

    if left == right {
        report.push_match(LockstepMatch {
            stage: LockstepStage::OpasmStatementParse,
            request: ProcessingRequestKind::Processor {
                processor: "asm".to_string(),
                kind: "statement".to_string(),
            },
            category,
        });
    } else {
        report.push_divergence(LockstepDivergence {
            stage: LockstepStage::OpasmStatementParse,
            processor_domain: "asm".to_string(),
            request: ProcessingRequestKind::Processor {
                processor: "asm".to_string(),
                kind: "statement".to_string(),
            },
            continuation_head,
            source_line: Some(request.line_num),
            active_cpu: if request.cpu_id.is_empty() {
                None
            } else {
                Some(request.cpu_id.to_string())
            },
            active_dialect: request.dialect_override.map(str::to_string),
            left,
            right,
            category,
            reason_code: reason_code.to_string(),
        });
    }

    report
}

fn build_lockstep_comparison(
    rust_outcome: &Result<StatementParseResult, ParseError>,
    vm_outcome: &Result<StatementParseResult, ParseError>,
) -> (
    LockstepCheckpoint,
    LockstepCheckpoint,
    LockstepComparisonCategory,
    &'static str,
) {
    match (rust_outcome, vm_outcome) {
        (Ok(left), Ok(right)) => (
            LockstepCheckpoint::PortableLineAst {
                normalized: normalize_portable_line_checkpoint(&left.ast),
            },
            LockstepCheckpoint::PortableLineAst {
                normalized: normalize_portable_line_checkpoint(&right.ast),
            },
            LockstepComparisonCategory::Ast,
            "opasm-statement-ast-mismatch",
        ),
        (Err(left), Err(right)) => (
            LockstepCheckpoint::Diagnostic {
                normalized: normalize_parse_error_checkpoint(left),
            },
            LockstepCheckpoint::Diagnostic {
                normalized: normalize_parse_error_checkpoint(right),
            },
            LockstepComparisonCategory::Diagnostics,
            "opasm-statement-diagnostic-mismatch",
        ),
        (Ok(left), Err(right)) => (
            LockstepCheckpoint::PortableLineAst {
                normalized: normalize_portable_line_checkpoint(&left.ast),
            },
            LockstepCheckpoint::Diagnostic {
                normalized: normalize_parse_error_checkpoint(right),
            },
            LockstepComparisonCategory::Ast,
            "opasm-statement-success-vs-error",
        ),
        (Err(left), Ok(right)) => (
            LockstepCheckpoint::Diagnostic {
                normalized: normalize_parse_error_checkpoint(left),
            },
            LockstepCheckpoint::PortableLineAst {
                normalized: normalize_portable_line_checkpoint(&right.ast),
            },
            LockstepComparisonCategory::Ast,
            "opasm-statement-error-vs-success",
        ),
    }
}

fn normalize_parse_error_checkpoint(err: &ParseError) -> String {
    format!(
        "{}@{}:{}-{}",
        err.message, err.span.line, err.span.col_start, err.span.col_end
    )
}

fn normalize_portable_line_checkpoint(ast: &LineAst) -> String {
    format!("{:?}", PortableLineAst::from_core_line_ast(ast))
}

#[cfg(test)]
mod tests {
    use super::{process_statement, tokenize_statement, StatementRequest};
    use opcore::parser::LineAst;
    use types::lockstep::{ContinuationHead, ExecutionMode, LockstepStage};

    #[test]
    fn tokenize_statement_exposes_lower_level_opasm_service() {
        let tokenized = tokenize_statement(StatementRequest::new("lda #$42", 1))
            .expect("tokenization should succeed");
        assert!(!tokenized.tokens.is_empty());
    }

    #[test]
    fn process_statement_rust_returns_trace() {
        let result = process_statement(StatementRequest::new(".module demo", 1), None)
            .expect("parse should succeed");
        assert!(matches!(result.parsed.ast, LineAst::Statement(..)));
        assert_eq!(result.trace.requests().len(), 1);
    }

    #[test]
    fn process_statement_lockstep_records_statement_match() {
        let model = vm::vm_opasm::load_model_from_registry(&engine::build_default_asm_registry())
            .expect("vm model");
        let request = StatementRequest::new("    LDA #$42", 1)
            .with_execution_mode(ExecutionMode::Lockstep {
                continuation_head: ContinuationHead::Vm,
            })
            .with_model(&model, "m6502", None);
        let result = process_statement(request, None).expect("lockstep parse");
        assert!(result
            .lockstep_report
            .matches()
            .iter()
            .any(|entry| entry.stage == LockstepStage::OpasmStatementParse));
    }
}
