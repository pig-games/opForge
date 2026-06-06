// SPDX-License-Identifier: GPL-3.0-or-later

use crate::m65816::state;
use opcore::parser::Expr;
use registry::family::AssemblerContext;

#[derive(Clone, Copy, Debug)]
pub struct VmSelectorAdapter {
    accumulator_is_8bit: bool,
    index_is_8bit: bool,
    data_bank: u8,
    data_bank_known: bool,
    program_bank: u8,
    program_bank_known: bool,
    direct_page: u16,
    direct_page_known: bool,
}

impl VmSelectorAdapter {
    pub fn from_assembler_ctx(ctx: &dyn AssemblerContext) -> Self {
        Self {
            accumulator_is_8bit: state::accumulator_is_8bit(ctx),
            index_is_8bit: state::index_is_8bit(ctx),
            data_bank: state::data_bank(ctx),
            data_bank_known: state::data_bank_known(ctx),
            program_bank: state::program_bank(ctx),
            program_bank_known: state::program_bank_known(ctx),
            direct_page: state::direct_page(ctx),
            direct_page_known: state::direct_page_known(ctx),
        }
    }

    pub fn encode_abs16_bank_fold(
        self,
        value: i64,
        upper_mnemonic: &str,
    ) -> Result<Vec<u8>, String> {
        let (assumed_bank, assumed_known, assumed_bank_key) =
            self.assumed_bank_state_for_mnemonic(upper_mnemonic);
        if !(0..=0xFF_FFFF).contains(&value) {
            return Err(format!("Address {} out of 24-bit range", value));
        }
        if value <= 0xFFFF {
            return Ok(encode_le_bytes(value as u32, 2));
        }

        if !assumed_known {
            return Err(bank_unknown_error(assumed_bank_key, upper_mnemonic));
        }
        let absolute_bank = ((value as u32) >> 16) as u8;
        if absolute_bank != assumed_bank {
            return Err(bank_mismatch_error(
                value as u32,
                absolute_bank,
                assumed_bank,
                assumed_bank_key,
            ));
        }
        Ok(encode_le_bytes(value as u32, 2))
    }

    pub fn encode_force_abs16(
        self,
        value: i64,
        upper_mnemonic: &str,
        use_program_bank: bool,
        force_suffix: &str,
    ) -> Result<Vec<u8>, String> {
        if (0..=0xFFFF).contains(&value) {
            return Ok(encode_le_bytes(value as u32, 2));
        }
        if !(0..=0xFF_FFFF).contains(&value) {
            return Err(format!(
                "Address {} out of 24-bit range for explicit ',{}'",
                value, force_suffix
            ));
        }

        let (assumed_bank, assumed_known, assumed_bank_key) = if use_program_bank {
            (self.program_bank, self.program_bank_known, "pbr")
        } else {
            (self.data_bank, self.data_bank_known, "dbr")
        };

        if !assumed_known {
            return Err(bank_unknown_error(assumed_bank_key, upper_mnemonic));
        }
        let absolute_bank = ((value as u32) >> 16) as u8;
        if absolute_bank != assumed_bank {
            return Err(bank_mismatch_error(
                value as u32,
                absolute_bank,
                assumed_bank,
                assumed_bank_key,
            ));
        }
        Ok(encode_le_bytes(value as u32, 2))
    }

    pub fn encode_immediate(self, value: i64, upper_mnemonic: &str) -> Result<Vec<u8>, String> {
        let acc_imm = matches!(
            upper_mnemonic,
            "ADC" | "AND" | "BIT" | "CMP" | "EOR" | "LDA" | "ORA" | "SBC"
        );
        let idx_imm = matches!(upper_mnemonic, "CPX" | "CPY" | "LDX" | "LDY");

        if acc_imm {
            if self.accumulator_is_8bit {
                if !(0..=0xFF).contains(&value) {
                    return Err(format!(
                        "Accumulator immediate value {} out of range (0-255) in 8-bit mode",
                        value
                    ));
                }
                return Ok(vec![value as u8]);
            }
            if !(0..=0xFFFF).contains(&value) {
                return Err(format!(
                    "Accumulator immediate value {} out of range (0-65535) in 16-bit mode",
                    value
                ));
            }
            return Ok(encode_le_bytes(value as u32, 2));
        }

        if idx_imm {
            if self.index_is_8bit {
                if !(0..=0xFF).contains(&value) {
                    return Err(format!(
                        "Index immediate value {} out of range (0-255) in 8-bit mode",
                        value
                    ));
                }
                return Ok(vec![value as u8]);
            }
            if !(0..=0xFFFF).contains(&value) {
                return Err(format!(
                    "Index immediate value {} out of range (0-65535) in 16-bit mode",
                    value
                ));
            }
            return Ok(encode_le_bytes(value as u32, 2));
        }

        if !(0..=0xFF).contains(&value) {
            return Err(format!("Immediate value {} out of range (0-255)", value));
        }
        Ok(vec![value as u8])
    }

    pub fn encode_force_d(self, value: i64) -> Result<Vec<u8>, String> {
        if (0..=0xFF).contains(&value) {
            return Ok(vec![value as u8]);
        }
        if !(0..=0xFFFF).contains(&value) {
            return Err(format!(
                "Address {} out of 16-bit range for explicit ',d'",
                value
            ));
        }
        let absolute_value = value as u16;
        let Some(dp_offset) = self.direct_page_offset_for_absolute_address(absolute_value) else {
            return Err(format!(
                "Address ${absolute_value:04X} is outside the direct-page window for explicit ',d'"
            ));
        };
        Ok(vec![dp_offset])
    }

    pub fn encode_force_u24(self, value: i64) -> Result<Vec<u8>, String> {
        if !(0..=0xFF_FFFF).contains(&value) {
            return Err(format!(
                "Address {} out of 24-bit range for explicit ',l'",
                value
            ));
        }
        Ok(encode_le_bytes(value as u32, 3))
    }

    pub fn prefer_long(
        self,
        value: i64,
        upper_mnemonic: &str,
        symbol_based: bool,
        current_address: u32,
        pass: u8,
        has_unstable_symbols: bool,
    ) -> bool {
        let (assumed_bank, assumed_known, _) = self.assumed_bank_state_for_mnemonic(upper_mnemonic);

        if pass == 1 && has_unstable_symbols {
            return current_address > 0xFFFF || !assumed_known || assumed_bank != 0;
        }

        if symbol_based && (0..=0xFFFF).contains(&value) && (!assumed_known || assumed_bank != 0) {
            return true;
        }

        if (0x1_0000..=0xFF_FFFF).contains(&value) {
            let absolute_bank = ((value as u32) >> 16) as u8;
            if !assumed_known || absolute_bank != assumed_bank {
                return true;
            }
        }

        false
    }

    pub fn should_defer_abs16(
        self,
        value: i64,
        upper_mnemonic: &str,
        pass: u8,
        has_unstable_symbols: bool,
    ) -> bool {
        let (assumed_bank, assumed_known, _) = self.assumed_bank_state_for_mnemonic(upper_mnemonic);
        if pass == 1 && has_unstable_symbols {
            return true;
        }
        if value <= 0xFFFF {
            return true;
        }
        if value > 0xFF_FFFF {
            return false;
        }
        let absolute_bank = ((value as u32) >> 16) as u8;
        !assumed_known || absolute_bank != assumed_bank
    }

    fn assumed_bank_state_for_mnemonic(self, upper_mnemonic: &str) -> (u8, bool, &'static str) {
        if matches!(upper_mnemonic, "JMP" | "JSR") {
            (self.program_bank, self.program_bank_known, "pbr")
        } else {
            (self.data_bank, self.data_bank_known, "dbr")
        }
    }

    fn direct_page_offset_for_absolute_address(self, address: u16) -> Option<u8> {
        if !self.direct_page_known || address <= 0x00FF {
            return None;
        }
        let offset = address.wrapping_sub(self.direct_page);
        (offset <= 0x00FF).then_some(offset as u8)
    }
}

pub fn input_shape_requires_runtime_family_support(shape_key: &str) -> bool {
    shape_key.eq_ignore_ascii_case("stack_relative")
        || shape_key.eq_ignore_ascii_case("stack_relative_indirect_y")
        || shape_key.eq_ignore_ascii_case("indirect_long")
        || shape_key.eq_ignore_ascii_case("indirect_long_y")
}

pub fn encode_runtime_operand_plan<E, U>(
    operand_plan: &str,
    expr0: Option<&Expr>,
    expr1: Option<&Expr>,
    upper_mnemonic: &str,
    ctx: &dyn AssemblerContext,
    mut eval_expr: E,
    mut has_unstable_symbols: U,
) -> Result<Option<Vec<Vec<u8>>>, String>
where
    E: FnMut(&Expr) -> Result<i64, String>,
    U: FnMut(&Expr) -> Result<bool, String>,
{
    let adapter = VmSelectorAdapter::from_assembler_ctx(ctx);
    let operand_bytes = match operand_plan {
        "force_d_u8" => vec![encode_expr_force_d_u8(
            expr0,
            ctx,
            &mut eval_expr,
            &mut has_unstable_symbols,
            adapter,
        )?],
        "force_l_u24" => vec![encode_expr_force_u24(
            expr0,
            ctx,
            &mut eval_expr,
            &mut has_unstable_symbols,
            adapter,
        )?],
        "m65816_long_pref_u24" => {
            let expr0 = expr0.ok_or_else(|| "missing unresolved-long operand".to_string())?;
            let symbol_based = expr_has_symbol_references(expr0);
            let value = eval_expr(expr0)?;
            if !adapter.prefer_long(
                value,
                upper_mnemonic,
                symbol_based,
                ctx.current_address(),
                ctx.pass(),
                has_unstable_symbols(expr0)?,
            ) {
                return Ok(None);
            }
            vec![encode_expr_force_u24(
                Some(expr0),
                ctx,
                &mut eval_expr,
                &mut has_unstable_symbols,
                adapter,
            )?]
        }
        "m65816_abs16_bank_fold_dbr" => {
            let expr0 = expr0.ok_or_else(|| "missing bank-fold operand".to_string())?;
            let value = eval_expr(expr0)?;
            if adapter.should_defer_abs16(
                value,
                upper_mnemonic,
                ctx.pass(),
                has_unstable_symbols(expr0)?,
            ) {
                return Ok(None);
            }
            vec![adapter.encode_abs16_bank_fold(value, upper_mnemonic)?]
        }
        "force_b_abs16_dbr" => {
            if matches!(upper_mnemonic, "JMP" | "JSR") {
                return Ok(None);
            }
            vec![encode_expr_force_abs16(
                expr0,
                ctx,
                &mut eval_expr,
                &mut has_unstable_symbols,
                adapter,
                upper_mnemonic,
                false,
            )?]
        }
        "force_k_abs16_pbr" => {
            if !matches!(upper_mnemonic, "JMP" | "JSR") {
                return Ok(None);
            }
            vec![encode_expr_force_abs16(
                expr0,
                ctx,
                &mut eval_expr,
                &mut has_unstable_symbols,
                adapter,
                upper_mnemonic,
                true,
            )?]
        }
        "imm_mx" => {
            let expr0 = expr0.ok_or_else(|| "missing immediate operand".to_string())?;
            vec![adapter.encode_immediate(eval_expr(expr0)?, upper_mnemonic)?]
        }
        _ => return Ok(None),
    };

    let _ = expr1;
    Ok(Some(operand_bytes))
}

fn encode_expr_force_d_u8<E, U>(
    expr0: Option<&Expr>,
    ctx: &dyn AssemblerContext,
    eval_expr: &mut E,
    has_unstable_symbols: &mut U,
    adapter: VmSelectorAdapter,
) -> Result<Vec<u8>, String>
where
    E: FnMut(&Expr) -> Result<i64, String>,
    U: FnMut(&Expr) -> Result<bool, String>,
{
    let expr0 = expr0.ok_or_else(|| "missing force-d operand".to_string())?;
    if ctx.pass() == 1 && has_unstable_symbols(expr0)? {
        return Ok(vec![0]);
    }
    adapter.encode_force_d(eval_expr(expr0)?)
}

fn encode_expr_force_u24<E, U>(
    expr0: Option<&Expr>,
    ctx: &dyn AssemblerContext,
    eval_expr: &mut E,
    has_unstable_symbols: &mut U,
    adapter: VmSelectorAdapter,
) -> Result<Vec<u8>, String>
where
    E: FnMut(&Expr) -> Result<i64, String>,
    U: FnMut(&Expr) -> Result<bool, String>,
{
    let expr0 = expr0.ok_or_else(|| "missing force-l operand".to_string())?;
    if ctx.pass() == 1 && has_unstable_symbols(expr0)? {
        return Ok(vec![0, 0, 0]);
    }
    adapter.encode_force_u24(eval_expr(expr0)?)
}

fn encode_expr_force_abs16<E, U>(
    expr0: Option<&Expr>,
    ctx: &dyn AssemblerContext,
    eval_expr: &mut E,
    has_unstable_symbols: &mut U,
    adapter: VmSelectorAdapter,
    upper_mnemonic: &str,
    use_program_bank: bool,
) -> Result<Vec<u8>, String>
where
    E: FnMut(&Expr) -> Result<i64, String>,
    U: FnMut(&Expr) -> Result<bool, String>,
{
    let force_suffix = if use_program_bank { "k" } else { "b" };
    let missing_error = if use_program_bank {
        "missing force-k operand"
    } else {
        "missing force-b operand"
    };
    let expr0 = expr0.ok_or_else(|| missing_error.to_string())?;
    if ctx.pass() == 1 && has_unstable_symbols(expr0)? {
        return Ok(vec![0, 0]);
    }
    adapter.encode_force_abs16(
        eval_expr(expr0)?,
        upper_mnemonic,
        use_program_bank,
        force_suffix,
    )
}

fn expr_has_symbol_references(expr: &Expr) -> bool {
    match expr {
        Expr::Identifier(_, _) | Expr::Register(_, _) => true,
        Expr::Indirect(inner, _) | Expr::Immediate(inner, _) | Expr::IndirectLong(inner, _) => {
            expr_has_symbol_references(inner)
        }
        Expr::List(items, _) | Expr::Tuple(items, _) => {
            items.iter().any(expr_has_symbol_references)
        }
        Expr::Index { base, index, .. } => {
            expr_has_symbol_references(base) || expr_has_symbol_references(index)
        }
        Expr::Member { base, .. } => expr_has_symbol_references(base),
        Expr::StructLiteral { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_has_symbol_references(value)),
        Expr::Call { args, .. } => args.iter().any(expr_has_symbol_references),
        Expr::Placeholder(_) => false,
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => {
            expr_has_symbol_references(cond)
                || expr_has_symbol_references(then_expr)
                || expr_has_symbol_references(else_expr)
        }
        Expr::Unary { expr, .. } => expr_has_symbol_references(expr),
        Expr::Binary { left, right, .. } => {
            expr_has_symbol_references(left) || expr_has_symbol_references(right)
        }
        Expr::Range {
            start, end, step, ..
        } => {
            expr_has_symbol_references(start)
                || expr_has_symbol_references(end)
                || step
                    .as_ref()
                    .is_some_and(|step_expr| expr_has_symbol_references(step_expr))
        }
        Expr::Number(_, _) | Expr::Dollar(_) | Expr::String(_, _) | Expr::Error(_, _) => false,
    }
}

fn bank_mismatch_error(
    address: u32,
    actual_bank: u8,
    assumed_bank: u8,
    assumed_bank_key: &str,
) -> String {
    format!(
        "Address ${address:06X} is in bank ${actual_bank:02X}, but .assume {assumed_bank_key}=${assumed_bank:02X}"
    )
}

fn bank_unknown_error(assumed_bank_key: &str, upper_mnemonic: &str) -> String {
    let mut message = format!(
        "Unable to resolve 24-bit bank because .assume {assumed_bank_key}=... is unknown; set .assume {assumed_bank_key}=$00..$FF or {assumed_bank_key}=auto"
    );
    message.push_str(
        ". If this source relied on removed stack-sequence inference, update .assume near this site",
    );
    let has_long = matches!(
        upper_mnemonic,
        "ORA" | "AND" | "EOR" | "ADC" | "STA" | "LDA" | "CMP" | "SBC" | "JML" | "JSL"
    );
    if has_long {
        message.push_str("; long-capable operands can be forced with ',l'");
    }
    message.push('.');
    message
}

fn encode_le_bytes(value: u32, byte_count: usize) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(byte_count);
    let mut remaining = value;
    for _ in 0..byte_count {
        bytes.push((remaining & 0xFF) as u8);
        remaining >>= 8;
    }
    bytes
}

#[cfg(test)]
mod tests {
    use super::*;

    fn default_adapter() -> VmSelectorAdapter {
        VmSelectorAdapter {
            accumulator_is_8bit: false,
            index_is_8bit: false,
            data_bank: 0x12,
            data_bank_known: true,
            program_bank: 0x34,
            program_bank_known: true,
            direct_page: 0x1200,
            direct_page_known: true,
        }
    }

    #[test]
    fn encode_abs16_bank_fold_uses_assumed_bank_for_long_values() {
        let bytes = default_adapter()
            .encode_abs16_bank_fold(0x12_3456, "LDA")
            .expect("bytes");
        assert_eq!(bytes, vec![0x56, 0x34]);
    }

    #[test]
    fn encode_force_abs16_reports_unknown_assumed_bank() {
        let mut adapter = default_adapter();
        adapter.data_bank_known = false;
        let err = adapter
            .encode_force_abs16(0x12_3456, "LDA", false, "b")
            .expect_err("missing assume should error");
        assert!(err.contains("Unable to resolve 24-bit bank"));
    }

    #[test]
    fn encode_immediate_honors_accumulator_width() {
        let bytes = default_adapter()
            .encode_immediate(0x1234, "LDA")
            .expect("16-bit bytes");
        assert_eq!(bytes, vec![0x34, 0x12]);
    }

    #[test]
    fn encode_force_d_resolves_direct_page_offset() {
        let bytes = default_adapter()
            .encode_force_d(0x1234)
            .expect("direct-page bytes");
        assert_eq!(bytes, vec![0x34]);
    }

    #[test]
    fn encode_force_u24_rejects_out_of_range() {
        let err = default_adapter()
            .encode_force_u24(0x1_000000)
            .expect_err("range error");
        assert!(err.contains("out of 24-bit range"));
    }

    #[test]
    fn prefer_long_uses_pass1_unstable_path() {
        let mut adapter = default_adapter();
        adapter.data_bank = 0;
        assert!(adapter.prefer_long(0x10, "LDA", false, 0x1_0000, 1, true));
        assert!(!adapter.prefer_long(0x10, "LDA", false, 0x1000, 1, true));
    }

    #[test]
    fn should_defer_abs16_rejects_large_non_24bit_values() {
        assert!(!default_adapter().should_defer_abs16(0x1_000000, "LDA", 2, false));
    }

    #[test]
    fn runtime_family_support_shape_gate_matches_m65816_only_shapes() {
        assert!(input_shape_requires_runtime_family_support(
            "stack_relative"
        ));
        assert!(input_shape_requires_runtime_family_support(
            "indirect_long_y"
        ));
        assert!(!input_shape_requires_runtime_family_support("direct"));
    }
}
