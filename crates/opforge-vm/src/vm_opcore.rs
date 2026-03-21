// SPDX-License-Identifier: GPL-3.0-or-later

//! `.opcore` VM surface.
//!
//! This groups VM-side functionality that primarily supports the language/core
//! processor domain rather than assembler instruction selection or encoding.

use opcore::expr_vm::compile_core_expr_to_portable_program;
use opcore::expr_vm::{
    eval_portable_expr_program, expr_program_has_unstable_symbols, PortableExprBudgets,
    PortableExprEvalContext, PortableExprEvaluation, PortableExprProgram, PortableExprRef,
};
use opcore::parser::{Expr, ParseError, Parser};
use opcore::tokenizer::{Span, Token};
use registry::family::AssemblerContext;
use registry::syntax::RegisterChecker;
use types::processing::{
    OpcoreRequestKind, ProcessingOutcome, ProcessingRequestKind, ProcessingReturn,
};

#[cfg(test)]
use crate::execution_model::CORE_EXPR_PARSER_FAILPOINT;
pub use crate::expr_vm_compat;
use crate::rollout::portable_expr_parser_runtime_enabled_for_family;
use crate::runtime_diagnostics::RuntimeBridgeDiagnostic;
use crate::runtime_error::RuntimeBridgeError;
use crate::runtime_parse_utils::runtime_bridge_error_to_parse_error;
pub use crate::vm_core::HierarchyExecutionModel;
use crate::vm_opasm_parse::VmExprParseContext;

struct RuntimePortableExprEvalContext<'a> {
    assembler_ctx: &'a dyn AssemblerContext,
}

impl PortableExprEvalContext for RuntimePortableExprEvalContext<'_> {
    fn lookup_symbol(&self, name: &str) -> Option<i64> {
        if !self.assembler_ctx.has_symbol(name) {
            return None;
        }
        self.assembler_ctx
            .eval_expr(&Expr::Identifier(name.to_string(), Span::default()))
            .ok()
    }

    fn current_address(&self) -> Option<i64> {
        Some(self.assembler_ctx.current_address() as i64)
    }

    fn pass(&self) -> u8 {
        self.assembler_ctx.pass()
    }

    fn symbol_is_finalized(&self, name: &str) -> Option<bool> {
        self.assembler_ctx.symbol_is_finalized(name)
    }

    fn eval_string_literal(&self, bytes: &[u8]) -> Result<i64, String> {
        self.assembler_ctx
            .eval_expr(&Expr::String(bytes.to_vec(), Span::default()))
    }
}

/// Runnable `.opcore` VM stage: parse an expression from tokenized input using
/// the VM-side runtime expression parser.
pub fn parse_expression_tokens(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Expr, ParseError> {
    crate::runtime_expr_parser::RuntimeExpressionParser::new(tokens, end_span, end_token_text)
        .parse_expr_from_tokens()
}

/// Runnable `.opcore` VM stage: evaluate an expression for assembler use
/// through the VM-backed portable expression runtime and resolved budgets.
pub fn evaluate_expression_for_assembler(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    expr: &Expr,
    ctx: &dyn AssemblerContext,
) -> Result<i64, String> {
    let program = compile_core_expr_to_portable_program(expr).map_err(|err| err.to_string())?;
    model
        .evaluate_portable_expression_program_with_contract_for_assembler(
            cpu_id,
            dialect_override,
            &program,
            ctx,
        )
        .map(|evaluation| evaluation.value)
        .map_err(|err| err.to_string())
}

/// Runnable `.opcore` VM stage: determine whether an expression still depends
/// on unstable symbols through the VM-backed portable expression runtime.
pub fn expression_has_unstable_symbols_for_assembler(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    expr: &Expr,
    ctx: &dyn AssemblerContext,
) -> Result<bool, String> {
    let program = compile_core_expr_to_portable_program(expr).map_err(|err| err.to_string())?;
    model
        .portable_expression_has_unstable_symbols_with_contract_for_assembler(
            cpu_id,
            dialect_override,
            &program,
            ctx,
        )
        .map_err(|err| err.to_string())
}

/// Runnable `.opcore` VM stage: parse a core-language module/import line
/// through the VM-backed line parser and keep only core-owned module-item
/// forms.
pub fn process_module_item_request_with_model(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> ProcessingOutcome<opcore::parser::LineAst, ParseError> {
    match crate::vm_opasm::parse_statement_line_with_model(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
    ) {
        Ok((ast, _, _)) => match ast {
            opcore::parser::LineAst::Use(..) => ProcessingOutcome::Done(ast),
            ref line_ast @ opcore::parser::LineAst::Statement(ref statement) => {
                let Some(mnemonic) = statement.mnemonic.as_deref() else {
                    return ProcessingOutcome::Return(ProcessingReturn::Unknown);
                };
                if mnemonic.eq_ignore_ascii_case(".module")
                    || mnemonic.eq_ignore_ascii_case(".endmodule")
                {
                    ProcessingOutcome::Done(line_ast.clone())
                } else {
                    ProcessingOutcome::Return(ProcessingReturn::Unknown)
                }
            }
            _ => ProcessingOutcome::Return(ProcessingReturn::Unknown),
        },
        Err(err) => ProcessingOutcome::Error(err),
    }
}

pub(crate) fn enforce_expr_token_budget(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
) -> Result<(), ParseError> {
    let token_budget = expr_parse_ctx
        .model
        .runtime_budget_limits()
        .max_parser_tokens_per_line;
    if tokens.len() > token_budget {
        let fallback_message = format!(
            "parser token budget exceeded ({} > {})",
            tokens.len(),
            token_budget
        );
        if let Some(contract) = expr_parse_ctx
            .model
            .resolve_parser_contract(expr_parse_ctx.cpu_id, expr_parse_ctx.dialect_override)
            .ok()
            .flatten()
        {
            return Err(runtime_bridge_error_to_parse_error(
                RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic::new(
                    contract.diagnostics.invalid_statement,
                    fallback_message,
                    Some(end_span),
                )),
                end_span,
            ));
        }
        return Err(ParseError {
            message: fallback_message,
            span: end_span,
        });
    }
    Ok(())
}

#[allow(dead_code)]
pub(crate) fn parse_expr_program_ref_with_vm_contract(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    parser_vm_opcode_version: Option<u16>,
) -> Result<(PortableExprRef, PortableExprProgram), ParseError> {
    enforce_expr_token_budget(expr_parse_ctx, tokens, end_span)?;
    let mut owned_tokens = Vec::with_capacity(tokens.len());
    owned_tokens.extend_from_slice(tokens);
    let program = expr_parse_ctx
        .model
        .compile_expression_program_with_parser_vm_opt_in_for_assembler(
            expr_parse_ctx.cpu_id,
            expr_parse_ctx.dialect_override,
            owned_tokens,
            end_span,
            end_token_text,
            parser_vm_opcode_version,
        )?;
    Ok((PortableExprRef { index: 0 }, program))
}

pub(crate) fn parse_expr_with_vm_contract(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Expr, ParseError> {
    if let Some(expr) =
        try_process_expr_request(expr_parse_ctx, tokens, end_span, end_token_text.clone())?
    {
        return Ok(expr);
    }
    enforce_expr_token_budget(expr_parse_ctx, tokens, end_span)?;
    expr_parse_ctx
        .model
        .validate_expression_parser_contract_for_assembler(
            expr_parse_ctx.cpu_id,
            expr_parse_ctx.dialect_override,
        )
        .map_err(|err| runtime_bridge_error_to_parse_error(err, end_span))?;

    let mut owned_tokens = Vec::with_capacity(tokens.len());
    owned_tokens.extend_from_slice(tokens);
    expr_parse_ctx.model.parse_expression_for_assembler(
        expr_parse_ctx.cpu_id,
        expr_parse_ctx.dialect_override,
        owned_tokens,
        end_span,
        end_token_text,
    )
}

fn try_process_expr_request(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Option<Expr>, ParseError> {
    let Some(ref handler_cell) = expr_parse_ctx.expr_handler else {
        return Ok(None);
    };
    let mut handler = handler_cell.borrow_mut();
    match handler.process_expr_request(
        ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr),
        tokens.to_vec(),
        end_span,
        end_token_text,
    ) {
        ProcessingOutcome::Done(expr) => Ok(Some(expr)),
        ProcessingOutcome::Error(err) => Err(err),
        ProcessingOutcome::Return(ProcessingReturn::Unknown) => Ok(None),
        ProcessingOutcome::Return(ProcessingReturn::Request { request }) => Err(ParseError {
            message: format!("Unsupported returned expression request: {request:?}"),
            span: end_span,
        }),
    }
}

pub(crate) fn parse_expr_with_vm_contract_and_boundary(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    boundary_token: Option<&Token>,
) -> Result<Expr, ParseError> {
    match parse_expr_with_vm_contract(expr_parse_ctx, tokens, end_span, end_token_text) {
        Ok(expr) => Ok(expr),
        Err(err)
            if err.message == crate::execution_model::HOST_PARSER_UNEXPECTED_END_OF_EXPRESSION
                && boundary_token.is_some() =>
        {
            let boundary_span = boundary_token.map(|token| token.span).unwrap_or(err.span);
            Err(ParseError {
                message: "Unexpected token in expression".to_string(),
                span: boundary_span,
            })
        }
        Err(err) => Err(err),
    }
}

pub fn load_model_from_registry(
    registry: &registry::registry::ModuleRegistry,
) -> Result<HierarchyExecutionModel, crate::vm_core::RuntimeModelLoadError> {
    crate::vm_core::load_execution_model_from_registry(registry)
}

pub fn load_model_from_chunks(
    chunks: package::HierarchyChunks,
) -> Result<HierarchyExecutionModel, crate::vm_core::RuntimeModelLoadError> {
    crate::vm_core::load_execution_model_from_chunks(chunks)
}

pub fn load_model_from_package_bytes(
    bytes: &[u8],
) -> Result<HierarchyExecutionModel, crate::vm_core::RuntimeModelLoadError> {
    crate::vm_core::load_execution_model_from_package_bytes(bytes)
}

impl HierarchyExecutionModel {
    pub fn parse_expression_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<Expr, ParseError> {
        let use_vm_parser = self.resolve_expr_parser_vm_rollout_for_assembler(
            cpu_id,
            dialect_override,
            false,
            end_span,
        )?;

        self.parse_expression_with_mode_for_assembler(
            cpu_id,
            dialect_override,
            tokens,
            end_span,
            end_token_text,
            use_vm_parser,
        )
    }

    fn parse_expression_with_mode_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
        use_vm_parser: bool,
    ) -> Result<Expr, ParseError> {
        self.validate_parser_contract_for_assembler(cpu_id, dialect_override, tokens.len())
            .map_err(|err| ParseError {
                message: err.to_string(),
                span: end_span,
            })?;

        if use_vm_parser {
            return parse_expression_tokens(tokens, end_span, end_token_text);
        }

        #[cfg(test)]
        if CORE_EXPR_PARSER_FAILPOINT.with(|flag| flag.get()) {
            return Err(ParseError {
                message: "core expression parser failpoint".to_string(),
                span: end_span,
            });
        }

        Parser::parse_expr_from_tokens(tokens, end_span, end_token_text)
    }

    fn resolve_expr_parser_vm_rollout_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        force_vm_parser: bool,
        end_span: Span,
    ) -> Result<bool, ParseError> {
        if force_vm_parser {
            return Ok(true);
        }

        let resolved = self
            .resolve_pipeline(cpu_id, dialect_override)
            .map_err(|err| ParseError {
                message: err.to_string(),
                span: end_span,
            })?;

        Ok(portable_expr_parser_runtime_enabled_for_family(
            resolved.family_id.as_str(),
            &[],
            &[],
        ))
    }

    fn compile_parsed_expression_for_assembler(
        expr: &Expr,
        end_span: Span,
    ) -> Result<PortableExprProgram, ParseError> {
        compile_core_expr_to_portable_program(expr).map_err(|err| ParseError {
            message: err.to_string(),
            span: err.span.unwrap_or(end_span),
        })
    }

    pub fn compile_expression_program_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<PortableExprProgram, ParseError> {
        let expr = self.parse_expression_for_assembler(
            cpu_id,
            dialect_override,
            tokens,
            end_span,
            end_token_text,
        )?;
        Self::compile_parsed_expression_for_assembler(&expr, end_span)
    }

    pub fn parse_expression_program_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<PortableExprProgram, ParseError> {
        self.compile_expression_program_with_parser_vm_opt_in_for_assembler(
            cpu_id,
            dialect_override,
            tokens,
            end_span,
            end_token_text,
            None,
        )
    }

    pub fn validate_expression_parser_contract_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<(), RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        let use_expr_parser_vm =
            portable_expr_parser_runtime_enabled_for_family(resolved.family_id.as_str(), &[], &[]);
        if !use_expr_parser_vm {
            return Ok(());
        }

        let contract = self.resolve_expr_parser_contract(cpu_id, dialect_override)?;
        if let Some(contract) = contract.as_ref() {
            self.ensure_expr_parser_contract_compatible_for_assembler(contract)?;
        }
        Ok(())
    }

    pub fn compile_expression_program_with_parser_vm_opt_in_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
        parser_vm_opcode_version: Option<u16>,
    ) -> Result<PortableExprProgram, ParseError> {
        let use_expr_parser_vm = self.resolve_expr_parser_vm_rollout_for_assembler(
            cpu_id,
            dialect_override,
            parser_vm_opcode_version.is_some(),
            end_span,
        )?;
        if !use_expr_parser_vm {
            let expr = self.parse_expression_with_mode_for_assembler(
                cpu_id,
                dialect_override,
                tokens,
                end_span,
                end_token_text,
                false,
            );
            return expr
                .and_then(|expr| Self::compile_parsed_expression_for_assembler(&expr, end_span));
        }

        let contract = self
            .resolve_expr_parser_contract(cpu_id, dialect_override)
            .map_err(|err| ParseError {
                message: err.to_string(),
                span: end_span,
            })?;

        if let Some(contract) = contract.as_ref() {
            self.ensure_expr_parser_contract_compatible_for_assembler(contract)
                .map_err(|err| ParseError {
                    message: err.to_string(),
                    span: end_span,
                })?;
        }

        let opcode_version = parser_vm_opcode_version
            .or_else(|| contract.as_ref().map(|entry| entry.opcode_version))
            .unwrap_or(package::EXPR_PARSER_VM_OPCODE_VERSION_V1);
        if opcode_version != package::EXPR_PARSER_VM_OPCODE_VERSION_V1 {
            return Err(ParseError {
                message: format!(
                    "unsupported VM expression parser VM opcode version {}",
                    opcode_version
                ),
                span: end_span,
            });
        }

        let expr = self.parse_expression_with_mode_for_assembler(
            cpu_id,
            dialect_override,
            tokens,
            end_span,
            end_token_text,
            true,
        )?;
        Self::compile_parsed_expression_for_assembler(&expr, end_span)
    }

    pub fn evaluate_portable_expression_program_for_assembler(
        &self,
        program: &PortableExprProgram,
        budgets: PortableExprBudgets,
        ctx: &dyn AssemblerContext,
    ) -> Result<PortableExprEvaluation, RuntimeBridgeError> {
        let adapter = RuntimePortableExprEvalContext { assembler_ctx: ctx };
        eval_portable_expr_program(program, &adapter, budgets)
            .map_err(|err| RuntimeBridgeError::Resolve(err.to_string()))
    }

    pub fn evaluate_portable_expression_program_with_contract_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        program: &PortableExprProgram,
        ctx: &dyn AssemblerContext,
    ) -> Result<PortableExprEvaluation, RuntimeBridgeError> {
        let budgets = self.resolve_expr_budgets(cpu_id, dialect_override)?;
        self.evaluate_portable_expression_program_for_assembler(program, budgets, ctx)
    }

    pub fn portable_expression_has_unstable_symbols_for_assembler(
        &self,
        program: &PortableExprProgram,
        budgets: PortableExprBudgets,
        ctx: &dyn AssemblerContext,
    ) -> Result<bool, RuntimeBridgeError> {
        let adapter = RuntimePortableExprEvalContext { assembler_ctx: ctx };
        expr_program_has_unstable_symbols(program, &adapter, budgets)
            .map_err(|err| RuntimeBridgeError::Resolve(err.to_string()))
    }

    pub fn portable_expression_has_unstable_symbols_with_contract_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        program: &PortableExprProgram,
        ctx: &dyn AssemblerContext,
    ) -> Result<bool, RuntimeBridgeError> {
        let budgets = self.resolve_expr_budgets(cpu_id, dialect_override)?;
        self.portable_expression_has_unstable_symbols_for_assembler(program, budgets, ctx)
    }
}
