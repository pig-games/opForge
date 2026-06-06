use crate::selector_encoding_utils::{
    encode_fixed_width_value, encode_relative_offset, mode_key_operand_size,
};
use families::m65816::encode_runtime_operand_plan as encode_m65816_runtime_operand_plan;
use opcore::parser::Expr;
use package::ModeSelectorDescriptor;
use registry::registry::VmEncodeCandidate;

use super::selector_bridge::{SelectorExprContext, SelectorInput};

pub(super) fn selector_to_candidate(
    selector: &ModeSelectorDescriptor,
    input: &SelectorInput<'_>,
    upper_mnemonic: &str,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Option<VmEncodeCandidate>, String> {
    let mode_key = selector.mode_key.to_ascii_lowercase();
    let Some(mode_operand_size) = mode_key_operand_size(mode_key.as_str()) else {
        return Ok(None);
    };
    let operand_bytes = match selector.operand_plan.as_str() {
        "none" => Vec::new(),
        "u8" => {
            let Some(expr0) = input.expr0 else {
                return Ok(None);
            };
            vec![encode_expr_u8(expr0, expr_ctx)?]
        }
        "u16" => {
            let Some(expr0) = input.expr0 else {
                return Ok(None);
            };
            vec![encode_expr_u16(expr0, expr_ctx)?]
        }
        "u24" => {
            let Some(expr0) = input.expr0 else {
                return Ok(None);
            };
            vec![encode_expr_u24(expr0, expr_ctx)?]
        }
        "rel8" => {
            let Some(expr0) = input.expr0 else {
                return Ok(None);
            };
            vec![encode_expr_rel8(expr0, expr_ctx, 2)?]
        }
        "rel16" => {
            let Some(expr0) = input.expr0 else {
                return Ok(None);
            };
            vec![encode_expr_rel16(expr0, expr_ctx, 3)?]
        }
        "pair_u8_rel8" => vec![
            encode_expr_u8(
                input
                    .expr0
                    .ok_or_else(|| "missing first operand".to_string())?,
                expr_ctx,
            )?,
            encode_expr_rel8(
                input
                    .expr1
                    .ok_or_else(|| "missing second operand".to_string())?,
                expr_ctx,
                3,
            )?,
        ],
        "u8u8_packed" => vec![{
            let mut packed = encode_expr_u8(
                input
                    .expr0
                    .ok_or_else(|| "missing first operand".to_string())?,
                expr_ctx,
            )?;
            packed.extend(encode_expr_u8(
                input
                    .expr1
                    .ok_or_else(|| "missing second operand".to_string())?,
                expr_ctx,
            )?);
            packed
        }],
        _ => match encode_m65816_runtime_operand_plan(
            selector.operand_plan.as_str(),
            input.expr0,
            input.expr1,
            upper_mnemonic,
            expr_ctx.assembler_ctx,
            |expr| expr_ctx.eval_expr(expr),
            |expr| expr_ctx.has_unstable_symbols(expr),
        )? {
            Some(operand_bytes) => operand_bytes,
            None => return Ok(None),
        },
    };

    if mode_operand_size == 0 && !operand_bytes.is_empty() {
        return Ok(None);
    }
    Ok(Some(VmEncodeCandidate {
        mode_key,
        operand_bytes,
    }))
}

fn encode_expr_u8(expr: &Expr, expr_ctx: &SelectorExprContext<'_>) -> Result<Vec<u8>, String> {
    encode_expr_fixed_width(expr, expr_ctx, 1, 0xFF, "invalid u8 operand")
}

fn encode_expr_u16(expr: &Expr, expr_ctx: &SelectorExprContext<'_>) -> Result<Vec<u8>, String> {
    encode_expr_fixed_width(expr, expr_ctx, 2, 0xFFFF, "invalid u16 operand")
}

fn encode_expr_u24(expr: &Expr, expr_ctx: &SelectorExprContext<'_>) -> Result<Vec<u8>, String> {
    encode_expr_fixed_width(expr, expr_ctx, 3, 0xFF_FFFF, "invalid u24 operand")
}

fn encode_expr_fixed_width(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
    byte_count: usize,
    max_value: i64,
    error_message: &str,
) -> Result<Vec<u8>, String> {
    let value = expr_ctx.eval_expr(expr)?;
    encode_fixed_width_value(value, byte_count, max_value, error_message)
}

fn encode_expr_rel8(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
    instr_len: i64,
) -> Result<Vec<u8>, String> {
    encode_expr_relative(
        expr,
        expr_ctx,
        instr_len,
        -128,
        127,
        1,
        "Branch target out of range",
    )
}

fn encode_expr_rel16(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
    instr_len: i64,
) -> Result<Vec<u8>, String> {
    encode_expr_relative(
        expr,
        expr_ctx,
        instr_len,
        -32768,
        32767,
        2,
        "Long branch target out of range",
    )
}

fn encode_expr_relative(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
    instr_len: i64,
    min_offset: i64,
    max_offset: i64,
    byte_count: usize,
    error_label: &str,
) -> Result<Vec<u8>, String> {
    let value = expr_ctx.eval_expr(expr)?;
    let current = expr_ctx.assembler_ctx.current_address() as i64 + instr_len;
    let offset = value - current;
    encode_relative_offset(
        offset,
        min_offset,
        max_offset,
        byte_count,
        error_label,
        expr_ctx.assembler_ctx.pass(),
    )
}
