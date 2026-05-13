use crate::selector_encoding_utils::{
    encode_abs16_bank_fold_value, encode_fixed_width_value, encode_force_abs16_value,
    encode_force_d_value, encode_force_u24_value, encode_m65816_immediate_value,
    encode_relative_offset, expr_has_symbol_references, mode_key_operand_size,
    prefer_long_decision, should_defer_abs16_decision,
};
use families::m65816::state;
use opcore::parser::Expr;
use package::ModeSelectorDescriptor;
use registry::family::AssemblerContext;
use registry::registry::VmEncodeCandidate;

use super::selector_bridge::{SelectorExprContext, SelectorInput};
use super::{force_suffix, SelectorOperandForce};

pub(super) use crate::selector_encoding_utils::input_shape_requires_m65816;

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
        "force_l_u24" => vec![encode_expr_force_u24(
            input
                .expr0
                .ok_or_else(|| "missing force-l operand".to_string())?,
            expr_ctx,
        )?],
        "m65816_long_pref_u24" => {
            let expr0 = input
                .expr0
                .ok_or_else(|| "missing unresolved-long operand".to_string())?;
            if !prefer_long_for_expr(expr0, upper_mnemonic, expr_ctx)? {
                return Ok(None);
            }
            vec![encode_expr_force_u24(expr0, expr_ctx)?]
        }
        "m65816_abs16_bank_fold_dbr" => {
            let expr0 = input
                .expr0
                .ok_or_else(|| "missing bank-fold operand".to_string())?;
            if should_defer_abs16_to_other_candidates(expr0, upper_mnemonic, expr_ctx)? {
                return Ok(None);
            }
            vec![encode_expr_abs16_bank_fold(
                expr0,
                upper_mnemonic,
                expr_ctx,
            )?]
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
        "force_d_u8" => vec![encode_expr_force_d_u8(
            input
                .expr0
                .ok_or_else(|| "missing force-d operand".to_string())?,
            expr_ctx,
        )?],
        "force_b_abs16_dbr" => {
            if matches!(upper_mnemonic, "JMP" | "JSR") {
                return Ok(None);
            }
            vec![encode_expr_force_abs16(
                input
                    .expr0
                    .ok_or_else(|| "missing force-b operand".to_string())?,
                false,
                SelectorOperandForce::DataBank,
                upper_mnemonic,
                expr_ctx,
            )?]
        }
        "force_k_abs16_pbr" => {
            if !matches!(upper_mnemonic, "JMP" | "JSR") {
                return Ok(None);
            }
            vec![encode_expr_force_abs16(
                input
                    .expr0
                    .ok_or_else(|| "missing force-k operand".to_string())?,
                true,
                SelectorOperandForce::ProgramBank,
                upper_mnemonic,
                expr_ctx,
            )?]
        }
        "imm_mx" => vec![encode_expr_m65816_immediate(
            input
                .expr0
                .ok_or_else(|| "missing immediate operand".to_string())?,
            upper_mnemonic,
            expr_ctx,
        )?],
        _ => return Ok(None),
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

fn encode_expr_force_d_u8(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Vec<u8>, String> {
    if expr_ctx.assembler_ctx.pass() == 1 && expr_ctx.has_unstable_symbols(expr)? {
        return Ok(vec![0]);
    }
    let value = expr_ctx.eval_expr(expr)?;
    encode_force_d_value(
        value,
        state::direct_page_known(expr_ctx.assembler_ctx),
        state::direct_page(expr_ctx.assembler_ctx),
    )
}

fn encode_expr_force_u24(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Vec<u8>, String> {
    if expr_ctx.assembler_ctx.pass() == 1 && expr_ctx.has_unstable_symbols(expr)? {
        return Ok(vec![0, 0, 0]);
    }
    let value = expr_ctx.eval_expr(expr)?;
    encode_force_u24_value(value)
}

fn prefer_long_for_expr(
    expr: &Expr,
    upper_mnemonic: &str,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<bool, String> {
    let (assumed_bank, assumed_known) = assumed_bank_state(upper_mnemonic, expr_ctx.assembler_ctx);
    let symbol_based = expr_has_symbol_references(expr);
    let pass = expr_ctx.assembler_ctx.pass();
    let current_address = expr_ctx.assembler_ctx.current_address();

    let value = expr_ctx.eval_expr(expr)?;
    let has_unstable_symbols = expr_ctx.has_unstable_symbols(expr)?;
    Ok(prefer_long_decision(
        value,
        symbol_based,
        assumed_known,
        assumed_bank,
        current_address,
        pass,
        has_unstable_symbols,
    ))
}

fn should_defer_abs16_to_other_candidates(
    expr: &Expr,
    upper_mnemonic: &str,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<bool, String> {
    let pass = expr_ctx.assembler_ctx.pass();
    let value = expr_ctx.eval_expr(expr)?;
    let has_unstable_symbols = expr_ctx.has_unstable_symbols(expr)?;
    let (assumed_bank, assumed_known) = assumed_bank_state(upper_mnemonic, expr_ctx.assembler_ctx);
    Ok(should_defer_abs16_decision(
        value,
        assumed_known,
        assumed_bank,
        pass,
        has_unstable_symbols,
    ))
}

fn encode_expr_abs16_bank_fold(
    expr: &Expr,
    upper_mnemonic: &str,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Vec<u8>, String> {
    let value = expr_ctx.eval_expr(expr)?;
    let (assumed_bank, assumed_known) = assumed_bank_state(upper_mnemonic, expr_ctx.assembler_ctx);
    let assumed_key = if matches!(upper_mnemonic, "JMP" | "JSR") {
        "pbr"
    } else {
        "dbr"
    };
    encode_abs16_bank_fold_value(
        value,
        upper_mnemonic,
        assumed_known,
        assumed_bank,
        assumed_key,
    )
}

fn assumed_bank_state(upper_mnemonic: &str, ctx: &dyn AssemblerContext) -> (u8, bool) {
    if matches!(upper_mnemonic, "JMP" | "JSR") {
        (state::program_bank(ctx), state::program_bank_known(ctx))
    } else {
        (state::data_bank(ctx), state::data_bank_known(ctx))
    }
}

fn encode_expr_force_abs16(
    expr: &Expr,
    use_program_bank: bool,
    force: SelectorOperandForce,
    upper_mnemonic: &str,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Vec<u8>, String> {
    if expr_ctx.assembler_ctx.pass() == 1 && expr_ctx.has_unstable_symbols(expr)? {
        return Ok(vec![0, 0]);
    }
    let value = expr_ctx.eval_expr(expr)?;
    let assumed_bank_key = if use_program_bank { "pbr" } else { "dbr" };
    let assumed_known = if use_program_bank {
        state::program_bank_known(expr_ctx.assembler_ctx)
    } else {
        state::data_bank_known(expr_ctx.assembler_ctx)
    };
    let assumed_bank = if use_program_bank {
        state::program_bank(expr_ctx.assembler_ctx)
    } else {
        state::data_bank(expr_ctx.assembler_ctx)
    };
    encode_force_abs16_value(
        value,
        upper_mnemonic,
        force_suffix(force),
        assumed_known,
        assumed_bank,
        assumed_bank_key,
    )
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

fn encode_expr_m65816_immediate(
    expr: &Expr,
    upper_mnemonic: &str,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Vec<u8>, String> {
    let value = expr_ctx.eval_expr(expr)?;
    encode_m65816_immediate_value(
        value,
        upper_mnemonic,
        state::accumulator_is_8bit(expr_ctx.assembler_ctx),
        state::index_is_8bit(expr_ctx.assembler_ctx),
    )
}
