// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared selector-encoding utility helpers used by runtime bridge adapters.

use opcore::parser::Expr;

pub fn input_shape_requires_m65816(shape_key: &str) -> bool {
    shape_key.eq_ignore_ascii_case("stack_relative")
        || shape_key.eq_ignore_ascii_case("stack_relative_indirect_y")
        || shape_key.eq_ignore_ascii_case("indirect_long")
        || shape_key.eq_ignore_ascii_case("indirect_long_y")
}

pub fn bank_mismatch_error(
    address: u32,
    actual_bank: u8,
    assumed_bank: u8,
    assumed_bank_key: &str,
) -> String {
    format!(
        "Address ${address:06X} is in bank ${actual_bank:02X}, but .assume {assumed_bank_key}=${assumed_bank:02X}"
    )
}

pub fn bank_unknown_error(assumed_bank_key: &str, upper_mnemonic: &str) -> String {
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

pub fn encode_le_bytes(value: u32, byte_count: usize) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(byte_count);
    let mut remaining = value;
    for _ in 0..byte_count {
        bytes.push((remaining & 0xFF) as u8);
        remaining >>= 8;
    }
    bytes
}

pub fn encode_fixed_width_value(
    value: i64,
    byte_count: usize,
    max_value: i64,
    error_message: &str,
) -> Result<Vec<u8>, String> {
    if !(0..=max_value).contains(&value) {
        return Err(error_message.to_string());
    }
    Ok(encode_le_bytes(value as u32, byte_count))
}

pub fn encode_relative_offset(
    offset: i64,
    min_offset: i64,
    max_offset: i64,
    byte_count: usize,
    error_label: &str,
    pass: u8,
) -> Result<Vec<u8>, String> {
    if !(min_offset..=max_offset).contains(&offset) {
        if pass > 1 {
            return Err(format!("{}: offset {}", error_label, offset));
        }
        return Ok(vec![0; byte_count]);
    }
    let mut bytes = Vec::with_capacity(byte_count);
    let mut remaining = offset as i32 as u32;
    for _ in 0..byte_count {
        bytes.push((remaining & 0xFF) as u8);
        remaining >>= 8;
    }
    Ok(bytes)
}

pub fn direct_page_offset_for_absolute_address(
    address: u16,
    direct_page_known: bool,
    direct_page: u16,
) -> Option<u8> {
    if !direct_page_known || address <= 0x00FF {
        return None;
    }
    let offset = address.wrapping_sub(direct_page);
    (offset <= 0x00FF).then_some(offset as u8)
}

pub fn mode_key_operand_size(mode_key_lower: &str) -> Option<u8> {
    match mode_key_lower {
        "implied" | "accumulator" => Some(0),
        "immediate"
        | "zeropage"
        | "zeropagex"
        | "zeropagey"
        | "indexedindirectx"
        | "indirectindexedy"
        | "relative"
        | "zeropageindirect"
        | "stackrelative"
        | "stackrelativeindirectindexedy"
        | "directpageindirectlongy"
        | "directpageindirectlong"
        | "indirectindexedz"
        | "directpageindirectlongz" => Some(1),
        "absolute"
        | "absolutex"
        | "absolutey"
        | "indirect"
        | "absoluteindexedindirect"
        | "relativelong"
        | "indirectlong"
        | "blockmove" => Some(2),
        "absolutelong" | "absolutelongx" => Some(3),
        _ => None,
    }
}

pub fn encode_abs16_bank_fold_value(
    value: i64,
    upper_mnemonic: &str,
    assumed_known: bool,
    assumed_bank: u8,
    assumed_bank_key: &str,
) -> Result<Vec<u8>, String> {
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

pub fn encode_force_abs16_value(
    value: i64,
    upper_mnemonic: &str,
    force_suffix: &str,
    assumed_known: bool,
    assumed_bank: u8,
    assumed_bank_key: &str,
) -> Result<Vec<u8>, String> {
    if (0..=65535).contains(&value) {
        return Ok(encode_le_bytes(value as u32, 2));
    }
    if !(0..=0xFF_FFFF).contains(&value) {
        return Err(format!(
            "Address {} out of 24-bit range for explicit ',{}'",
            value, force_suffix
        ));
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

pub fn encode_m65816_immediate_value(
    value: i64,
    upper_mnemonic: &str,
    accumulator_is_8bit: bool,
    index_is_8bit: bool,
) -> Result<Vec<u8>, String> {
    let acc_imm = matches!(
        upper_mnemonic,
        "ADC" | "AND" | "BIT" | "CMP" | "EOR" | "LDA" | "ORA" | "SBC"
    );
    let idx_imm = matches!(upper_mnemonic, "CPX" | "CPY" | "LDX" | "LDY");

    if acc_imm {
        if accumulator_is_8bit {
            if !(0..=255).contains(&value) {
                return Err(format!(
                    "Accumulator immediate value {} out of range (0-255) in 8-bit mode",
                    value
                ));
            }
            return Ok(vec![value as u8]);
        }
        if !(0..=65535).contains(&value) {
            return Err(format!(
                "Accumulator immediate value {} out of range (0-65535) in 16-bit mode",
                value
            ));
        }
        return Ok(encode_le_bytes(value as u32, 2));
    }

    if idx_imm {
        if index_is_8bit {
            if !(0..=255).contains(&value) {
                return Err(format!(
                    "Index immediate value {} out of range (0-255) in 8-bit mode",
                    value
                ));
            }
            return Ok(vec![value as u8]);
        }
        if !(0..=65535).contains(&value) {
            return Err(format!(
                "Index immediate value {} out of range (0-65535) in 16-bit mode",
                value
            ));
        }
        return Ok(encode_le_bytes(value as u32, 2));
    }

    if !(0..=255).contains(&value) {
        return Err(format!("Immediate value {} out of range (0-255)", value));
    }
    Ok(vec![value as u8])
}

pub fn encode_force_d_value(
    value: i64,
    direct_page_known: bool,
    direct_page: u16,
) -> Result<Vec<u8>, String> {
    if (0..=255).contains(&value) {
        return Ok(vec![value as u8]);
    }
    if !(0..=0xFFFF).contains(&value) {
        return Err(format!(
            "Address {} out of 16-bit range for explicit ',d'",
            value
        ));
    }
    let absolute_value = value as u16;
    let Some(dp_offset) =
        direct_page_offset_for_absolute_address(absolute_value, direct_page_known, direct_page)
    else {
        return Err(format!(
            "Address ${absolute_value:04X} is outside the direct-page window for explicit ',d'"
        ));
    };
    Ok(vec![dp_offset])
}

pub fn encode_force_u24_value(value: i64) -> Result<Vec<u8>, String> {
    if !(0..=0xFF_FFFF).contains(&value) {
        return Err(format!(
            "Address {} out of 24-bit range for explicit ',l'",
            value
        ));
    }
    Ok(encode_le_bytes(value as u32, 3))
}

pub fn prefer_long_decision(
    value: i64,
    symbol_based: bool,
    assumed_known: bool,
    assumed_bank: u8,
    current_address: u32,
    pass: u8,
    has_unstable_symbols: bool,
) -> bool {
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

pub fn should_defer_abs16_decision(
    value: i64,
    assumed_known: bool,
    assumed_bank: u8,
    pass: u8,
    has_unstable_symbols: bool,
) -> bool {
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

pub fn expr_has_symbol_references(expr: &Expr) -> bool {
    match expr {
        Expr::Identifier(_, _) | Expr::Register(_, _) => true,
        Expr::Indirect(inner, _) | Expr::Immediate(inner, _) | Expr::IndirectLong(inner, _) => {
            expr_has_symbol_references(inner)
        }
        Expr::List(items, _) => items.iter().any(expr_has_symbol_references),
        Expr::Index { base, index, .. } => {
            expr_has_symbol_references(base) || expr_has_symbol_references(index)
        }
        Expr::Member { base, .. } => expr_has_symbol_references(base),
        Expr::StructLiteral { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_has_symbol_references(value)),
        Expr::Call { args, .. } => args.iter().any(expr_has_symbol_references),
        Expr::Placeholder(_) => false,
        Expr::Tuple(items, _) => items.iter().any(expr_has_symbol_references),
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

#[cfg(test)]
mod tests {
    use super::*;
    use opcore::tokenizer::Span;

    #[test]
    fn expr_symbol_reference_detection_finds_identifiers() {
        let expr = Expr::Identifier("symbol_name".to_string(), Span::default());
        assert!(expr_has_symbol_references(&expr));
    }

    #[test]
    fn expr_symbol_reference_detection_ignores_literal_only_tree() {
        let expr = Expr::Binary {
            op: opcore::parser::BinaryOp::Add,
            left: Box::new(Expr::Number("1".to_string(), Span::default())),
            right: Box::new(Expr::Number("2".to_string(), Span::default())),
            span: Span::default(),
        };
        assert!(!expr_has_symbol_references(&expr));
    }

    #[test]
    fn encode_fixed_width_value_rejects_out_of_range() {
        let err = encode_fixed_width_value(0x1_0000, 2, 0xFFFF, "bad").expect_err("range error");
        assert_eq!(err, "bad");
    }

    #[test]
    fn encode_relative_offset_uses_zero_placeholder_on_pass1_overflow() {
        let bytes =
            encode_relative_offset(300, -128, 127, 1, "range", 1).expect("placeholder bytes");
        assert_eq!(bytes, vec![0]);
    }

    #[test]
    fn direct_page_offset_for_absolute_address_requires_known_direct_page() {
        assert_eq!(
            direct_page_offset_for_absolute_address(0x1234, false, 0x1200),
            None
        );
        assert_eq!(
            direct_page_offset_for_absolute_address(0x1234, true, 0x1200),
            Some(0x34)
        );
    }

    #[test]
    fn mode_key_operand_size_matches_known_values() {
        assert_eq!(mode_key_operand_size("implied"), Some(0));
        assert_eq!(mode_key_operand_size("relative"), Some(1));
        assert_eq!(mode_key_operand_size("absolute"), Some(2));
        assert_eq!(mode_key_operand_size("absolutelong"), Some(3));
        assert_eq!(mode_key_operand_size("unknown"), None);
    }

    #[test]
    fn encode_abs16_bank_fold_value_uses_assumed_bank_for_long_values() {
        let bytes =
            encode_abs16_bank_fold_value(0x12_3456, "LDA", true, 0x12, "dbr").expect("bytes");
        assert_eq!(bytes, vec![0x56, 0x34]);
    }

    #[test]
    fn encode_force_abs16_value_reports_unknown_assumed_bank() {
        let err = encode_force_abs16_value(0x12_3456, "LDA", "b", false, 0x12, "dbr")
            .expect_err("missing assume should error");
        assert!(err.contains("Unable to resolve 24-bit bank"));
    }

    #[test]
    fn encode_m65816_immediate_value_honors_accumulator_width() {
        let bytes =
            encode_m65816_immediate_value(0x1234, "LDA", false, true).expect("16-bit bytes");
        assert_eq!(bytes, vec![0x34, 0x12]);
    }

    #[test]
    fn encode_force_d_value_resolves_direct_page_offset() {
        let bytes = encode_force_d_value(0x1234, true, 0x1200).expect("direct-page bytes");
        assert_eq!(bytes, vec![0x34]);
    }

    #[test]
    fn encode_force_u24_value_rejects_out_of_range() {
        let err = encode_force_u24_value(0x1_000000).expect_err("range error");
        assert!(err.contains("out of 24-bit range"));
    }

    #[test]
    fn prefer_long_decision_uses_pass1_unstable_path() {
        assert!(prefer_long_decision(
            0x10, false, true, 0, 0x1_0000, 1, true
        ));
        assert!(!prefer_long_decision(0x10, false, true, 0, 0x1000, 1, true));
    }

    #[test]
    fn should_defer_abs16_decision_rejects_large_non_24bit_values() {
        assert!(!should_defer_abs16_decision(
            0x1_000000, true, 0x12, 2, false
        ));
    }
}
