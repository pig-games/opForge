// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared selector-encoding utility helpers used by runtime bridge adapters.

use opcore::parser::Expr;

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

pub fn mode_key_operand_size(mode_key_lower: &str) -> Option<u8> {
    match mode_key_lower {
        "implied" | "accumulator" => Some(0),
        "semantic" => Some(u8::MAX),
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
    fn mode_key_operand_size_matches_known_values() {
        assert_eq!(mode_key_operand_size("implied"), Some(0));
        assert_eq!(mode_key_operand_size("relative"), Some(1));
        assert_eq!(mode_key_operand_size("absolute"), Some(2));
        assert_eq!(mode_key_operand_size("absolutelong"), Some(3));
        assert_eq!(mode_key_operand_size("unknown"), None);
    }
}
