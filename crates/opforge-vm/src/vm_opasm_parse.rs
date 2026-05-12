use std::cell::RefCell;
use std::rc::Rc;

use opcore::parser::{Expr, LineAst, ParseError};
use opcore::tokenizer::{Span, Token};
use registry::syntax::{register_checker_none, RegisterChecker};
use types::processing::{ProcessingOutcome, ProcessingRequestKind};

use crate::portable_contract::PortableLineAst;
use crate::runtime_diagnostics::RuntimeBridgeDiagnostic;
use crate::runtime_error::RuntimeBridgeError;
use crate::runtime_parse_utils::{parse_span_at_end, runtime_bridge_error_to_parse_error};
use crate::tokenizer_runtime_utils;
use crate::vm_opcore::HierarchyExecutionModel;

use crate::execution_model::parser_vm::parse_line_with_parser_vm;

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
    pub(crate) expr_parser_opt_in_families: &'a [String],
    pub(crate) expr_parser_force_host_families: &'a [String],
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
    parse_line_with_model_with_rollout_overrides(
        model,
        cpu_id,
        dialect_override,
        &[],
        &[],
        line,
        line_num,
        register_checker,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn parse_line_with_model_with_rollout_overrides(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    expr_parser_opt_in_families: &[String],
    expr_parser_force_host_families: &[String],
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> Result<(LineAst, Span, Option<String>), ParseError> {
    parse_line_with_model_with_expr_handler_and_rollout_overrides(
        model,
        cpu_id,
        dialect_override,
        expr_parser_opt_in_families,
        expr_parser_force_host_families,
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
    parse_line_with_model_with_expr_handler_and_rollout_overrides(
        model,
        cpu_id,
        dialect_override,
        &[],
        &[],
        line,
        line_num,
        register_checker,
        expr_handler,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn parse_line_with_model_with_expr_handler_and_rollout_overrides<'a>(
    model: &'a HierarchyExecutionModel,
    cpu_id: &'a str,
    dialect_override: Option<&'a str>,
    expr_parser_opt_in_families: &'a [String],
    expr_parser_force_host_families: &'a [String],
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
                expr_parser_opt_in_families,
                expr_parser_force_host_families,
                expr_handler,
            },
        },
    )?;
    Ok((line_ast, end_span, end_token_text))
}
