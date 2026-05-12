//! Host-side bridge helpers for the native parser VM ABI.

use std::collections::HashMap;

use opcore::expression::expr_span;
use opcore::parser::{Expr, ParseError};
use opcore::tokenizer::Span;
use registry::family::AssemblerContext;
use registry::syntax::RegisterChecker;

use crate::execution_model::HierarchyExecutionModel;
use crate::vm_opasm::{parse_operand_expr_range, OperandExprBoundary, OperandExprParseHints};
use crate::vm_opasm_parse::{tokenize_parser_tokens_with_model, VmExprParseContext};
use crate::vm_opcore::{
    evaluate_expression_for_assembler, expression_has_unstable_symbols_for_assembler,
};

pub const NATIVE_PRVM_EXPR_REQUEST_RECORD_SIZE: usize = 32;
pub const NATIVE_PRVM_EXPR_RESULT_SLOT_SIZE: usize = 32;

pub const NATIVE_PRVM_EXPR_REQUEST_VERSION_V1: u16 = 1;

pub const NATIVE_PRVM_EXPR_SLOT_EMPTY: u16 = 0;
pub const NATIVE_PRVM_EXPR_SLOT_READY: u16 = 1;
pub const NATIVE_PRVM_EXPR_SLOT_READY_ERROR: u16 = 2;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativePrvmExprRequest {
    pub operand_index: u32,
    pub expr_slot_index: u32,
    pub start_token: u32,
    pub end_token: u32,
    pub boundary_span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativePrvmExprSlotState {
    Empty,
    ReadyExpression,
    ReadyExpressionError,
}

impl NativePrvmExprSlotState {
    pub fn as_u16(self) -> u16 {
        match self {
            Self::Empty => NATIVE_PRVM_EXPR_SLOT_EMPTY,
            Self::ReadyExpression => NATIVE_PRVM_EXPR_SLOT_READY,
            Self::ReadyExpressionError => NATIVE_PRVM_EXPR_SLOT_READY_ERROR,
        }
    }
}

#[derive(Clone, Debug)]
pub struct NativePrvmHostExpressionResult {
    pub request: NativePrvmExprRequest,
    pub slot_state: NativePrvmExprSlotState,
    pub host_expr_handle: u32,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NativePrvmHostExpressionEvaluation {
    Concrete { value: i64 },
    DeferredUnresolved { message: String },
}

#[derive(Clone, Debug)]
pub struct NativePrvmHostExpressionEvaluationResult {
    pub parsed: NativePrvmHostExpressionResult,
    pub evaluation: NativePrvmHostExpressionEvaluation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NativePrvmBridgeError {
    InvalidExpressionRequestSize {
        actual: usize,
    },
    InvalidExpressionRequestHeader,
    InvalidExpressionResultSlotSize {
        actual: usize,
    },
    InvalidTokenRange {
        start: u32,
        end: u32,
        token_count: usize,
    },
    ExpressionParser {
        message: String,
        span: Span,
    },
    ExpressionEvaluation {
        message: String,
        span: Span,
    },
    ExpressionParserReturnedNoOperand,
    TooManyHostExpressions,
}

pub struct NativePrvmHostExpressionBridge<'a> {
    model: &'a HierarchyExecutionModel,
    cpu_id: &'a str,
    dialect_override: Option<&'a str>,
    tokens: Vec<opcore::tokenizer::Token>,
    end_span: Span,
    end_token_text: Option<String>,
    mnemonic: Option<String>,
    expressions_by_handle: Vec<Expr>,
    expressions_by_native_slot: HashMap<u32, Expr>,
}

impl<'a> NativePrvmHostExpressionBridge<'a> {
    pub fn from_source_line(
        model: &'a HierarchyExecutionModel,
        cpu_id: &'a str,
        dialect_override: Option<&'a str>,
        source_line: &str,
        line_num: u32,
        register_checker: &RegisterChecker,
        mnemonic: Option<&str>,
    ) -> Result<Self, ParseError> {
        let (tokens, end_span, end_token_text) = tokenize_parser_tokens_with_model(
            model,
            cpu_id,
            dialect_override,
            source_line,
            line_num,
            register_checker,
        )?;
        Ok(Self {
            model,
            cpu_id,
            dialect_override,
            tokens,
            end_span,
            end_token_text,
            mnemonic: mnemonic.map(ToOwned::to_owned),
            expressions_by_handle: Vec::new(),
            expressions_by_native_slot: HashMap::new(),
        })
    }

    pub fn handle_expression_request_record(
        &mut self,
        request_record: &[u8],
        result_slot: &mut [u8],
    ) -> Result<NativePrvmHostExpressionResult, NativePrvmBridgeError> {
        let request = decode_expr_request_record(request_record)?;
        self.handle_expression_request(request, result_slot)
    }

    pub fn handle_and_evaluate_expression_request_record(
        &mut self,
        request_record: &[u8],
        result_slot: &mut [u8],
        assembler_ctx: &dyn AssemblerContext,
    ) -> Result<NativePrvmHostExpressionEvaluationResult, NativePrvmBridgeError> {
        let request = decode_expr_request_record(request_record)?;
        self.handle_and_evaluate_expression_request(request, result_slot, assembler_ctx)
    }

    pub fn handle_and_evaluate_expression_request(
        &mut self,
        request: NativePrvmExprRequest,
        result_slot: &mut [u8],
        assembler_ctx: &dyn AssemblerContext,
    ) -> Result<NativePrvmHostExpressionEvaluationResult, NativePrvmBridgeError> {
        let parsed = self.handle_expression_request(request, result_slot)?;
        let evaluation = self.evaluate_expression(&parsed.expr, assembler_ctx)?;
        Ok(NativePrvmHostExpressionEvaluationResult { parsed, evaluation })
    }

    pub fn handle_expression_request(
        &mut self,
        request: NativePrvmExprRequest,
        result_slot: &mut [u8],
    ) -> Result<NativePrvmHostExpressionResult, NativePrvmBridgeError> {
        if result_slot.len() != NATIVE_PRVM_EXPR_RESULT_SLOT_SIZE {
            return Err(NativePrvmBridgeError::InvalidExpressionResultSlotSize {
                actual: result_slot.len(),
            });
        }

        let start = request.start_token as usize;
        let end = request.end_token as usize;
        if start > end || end > self.tokens.len() {
            return Err(NativePrvmBridgeError::InvalidTokenRange {
                start: request.start_token,
                end: request.end_token,
                token_count: self.tokens.len(),
            });
        }

        self.model
            .ensure_parser_vm_v2_expr_subcall_contract_for_assembler(
                self.cpu_id,
                self.dialect_override,
            )
            .map_err(|err| NativePrvmBridgeError::ExpressionParser {
                message: err.to_string(),
                span: request.boundary_span,
            })?;

        let mut operands = Vec::new();
        parse_operand_expr_range(
            self.tokens.as_slice(),
            start,
            end,
            OperandExprBoundary {
                end_span: request.boundary_span,
                end_token_text: self.boundary_token_text(end),
            },
            OperandExprParseHints {
                mnemonic: self.mnemonic.as_deref(),
                operand_index: request.operand_index as usize,
            },
            &VmExprParseContext {
                model: self.model,
                cpu_id: self.cpu_id,
                dialect_override: self.dialect_override,
                expr_parser_opt_in_families: &[],
                expr_parser_force_host_families: &[],
                expr_handler: None,
            },
            &mut operands,
        )
        .map_err(|err| NativePrvmBridgeError::ExpressionParser {
            message: err.message,
            span: err.span,
        })?;

        let expr = operands
            .pop()
            .ok_or(NativePrvmBridgeError::ExpressionParserReturnedNoOperand)?;
        let slot_state = if matches!(expr, Expr::Error(_, _)) {
            NativePrvmExprSlotState::ReadyExpressionError
        } else {
            NativePrvmExprSlotState::ReadyExpression
        };
        let host_expr_handle = u32::try_from(self.expressions_by_handle.len())
            .map_err(|_| NativePrvmBridgeError::TooManyHostExpressions)?;
        self.expressions_by_handle.push(expr.clone());
        self.expressions_by_native_slot
            .insert(request.expr_slot_index, expr.clone());

        write_expr_result_slot(
            result_slot,
            slot_state,
            request.expr_slot_index,
            expr_span(&expr),
            host_expr_handle,
        )?;

        Ok(NativePrvmHostExpressionResult {
            request,
            slot_state,
            host_expr_handle,
            expr,
        })
    }

    pub fn expression_for_handle(&self, handle: u32) -> Option<&Expr> {
        self.expressions_by_handle.get(handle as usize)
    }

    pub fn expression_for_native_slot(&self, slot_index: u32) -> Option<&Expr> {
        self.expressions_by_native_slot.get(&slot_index)
    }

    pub fn evaluate_expression_for_handle(
        &self,
        handle: u32,
        assembler_ctx: &dyn AssemblerContext,
    ) -> Result<NativePrvmHostExpressionEvaluation, NativePrvmBridgeError> {
        let expr = self.expression_for_handle(handle).ok_or_else(|| {
            NativePrvmBridgeError::ExpressionEvaluation {
                message: format!("native PRVM expression handle {handle} does not exist"),
                span: self.end_span,
            }
        })?;
        self.evaluate_expression(expr, assembler_ctx)
    }

    pub fn evaluate_expression_for_native_slot(
        &self,
        slot_index: u32,
        assembler_ctx: &dyn AssemblerContext,
    ) -> Result<NativePrvmHostExpressionEvaluation, NativePrvmBridgeError> {
        let expr = self.expression_for_native_slot(slot_index).ok_or_else(|| {
            NativePrvmBridgeError::ExpressionEvaluation {
                message: format!("native PRVM expression slot {slot_index} does not exist"),
                span: self.end_span,
            }
        })?;
        self.evaluate_expression(expr, assembler_ctx)
    }

    pub fn expression_slots(&self) -> &HashMap<u32, Expr> {
        &self.expressions_by_native_slot
    }

    fn boundary_token_text(&self, end: usize) -> Option<String> {
        self.tokens
            .get(end)
            .map(|token| token.to_source_text())
            .or_else(|| self.end_token_text.clone())
    }

    fn evaluate_expression(
        &self,
        expr: &Expr,
        assembler_ctx: &dyn AssemblerContext,
    ) -> Result<NativePrvmHostExpressionEvaluation, NativePrvmBridgeError> {
        let eval_expr = match expr {
            Expr::Immediate(inner, _) => inner.as_ref(),
            _ => expr,
        };
        let span = expr_span(eval_expr);
        let has_unstable_symbols = expression_has_unstable_symbols_for_assembler(
            self.model,
            self.cpu_id,
            self.dialect_override,
            eval_expr,
            assembler_ctx,
        )
        .map_err(|message| NativePrvmBridgeError::ExpressionEvaluation { message, span })?;
        if has_unstable_symbols && assembler_ctx.pass() == 1 {
            return Ok(NativePrvmHostExpressionEvaluation::DeferredUnresolved {
                message: "expression depends on unresolved symbols".to_string(),
            });
        }

        evaluate_expression_for_assembler(
            self.model,
            self.cpu_id,
            self.dialect_override,
            eval_expr,
            assembler_ctx,
        )
        .map(|value| NativePrvmHostExpressionEvaluation::Concrete { value })
        .map_err(|message| NativePrvmBridgeError::ExpressionEvaluation { message, span })
    }

    #[allow(dead_code)]
    pub fn end_span(&self) -> Span {
        self.end_span
    }
}

pub fn decode_expr_request_record(
    bytes: &[u8],
) -> Result<NativePrvmExprRequest, NativePrvmBridgeError> {
    if bytes.len() != NATIVE_PRVM_EXPR_REQUEST_RECORD_SIZE {
        return Err(NativePrvmBridgeError::InvalidExpressionRequestSize {
            actual: bytes.len(),
        });
    }
    if read_u16(bytes, 0) != NATIVE_PRVM_EXPR_REQUEST_VERSION_V1 || read_u16(bytes, 2) != 0 {
        return Err(NativePrvmBridgeError::InvalidExpressionRequestHeader);
    }
    Ok(NativePrvmExprRequest {
        operand_index: read_u32(bytes, 4),
        expr_slot_index: read_u32(bytes, 8),
        start_token: read_u32(bytes, 12),
        end_token: read_u32(bytes, 16),
        boundary_span: Span {
            line: read_u32(bytes, 20),
            col_start: read_u32(bytes, 24) as usize,
            col_end: read_u32(bytes, 28) as usize,
        },
    })
}

fn write_expr_result_slot(
    bytes: &mut [u8],
    state: NativePrvmExprSlotState,
    expr_slot_index: u32,
    span: Span,
    host_expr_handle: u32,
) -> Result<(), NativePrvmBridgeError> {
    if bytes.len() != NATIVE_PRVM_EXPR_RESULT_SLOT_SIZE {
        return Err(NativePrvmBridgeError::InvalidExpressionResultSlotSize {
            actual: bytes.len(),
        });
    }
    bytes.fill(0);
    write_u16(bytes, 0, state.as_u16());
    write_u16(bytes, 2, 0);
    write_u32(bytes, 4, expr_slot_index);
    write_u32(bytes, 8, span.line);
    write_u32(bytes, 12, span.col_start as u32);
    write_u32(bytes, 16, span.col_end as u32);
    write_u32(bytes, 20, host_expr_handle);
    write_u32(bytes, 24, u32::MAX);
    write_u32(bytes, 28, 0);
    Ok(())
}

fn read_u16(bytes: &[u8], offset: usize) -> u16 {
    u16::from_be_bytes(bytes[offset..offset + 2].try_into().expect("u16 field"))
}

fn read_u32(bytes: &[u8], offset: usize) -> u32 {
    u32::from_be_bytes(bytes[offset..offset + 4].try_into().expect("u32 field"))
}

fn write_u16(bytes: &mut [u8], offset: usize, value: u16) {
    bytes[offset..offset + 2].copy_from_slice(&value.to_be_bytes());
}

fn write_u32(bytes: &mut [u8], offset: usize, value: u32) {
    bytes[offset..offset + 4].copy_from_slice(&value.to_be_bytes());
}
