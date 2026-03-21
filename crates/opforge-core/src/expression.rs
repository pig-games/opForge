// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Shared expression helpers and evaluation.

use crate::parser::{AssignOp, BinaryOp, Expr, UnaryOp};
use crate::tokenizer::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstEvalErrorKind {
    Expression,
    Directive,
    Symbol,
    Instruction,
}

#[derive(Debug, Clone)]
pub struct AstEvalErrorDetail {
    kind: AstEvalErrorKind,
    message: String,
}

impl AstEvalErrorDetail {
    pub fn new(kind: AstEvalErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
        }
    }

    pub fn kind(&self) -> AstEvalErrorKind {
        self.kind
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

/// Error from expression evaluation with source span.
#[derive(Debug, Clone)]
pub struct AstEvalError {
    pub error: AstEvalErrorDetail,
    pub span: Span,
}

impl AstEvalError {
    pub fn new(kind: AstEvalErrorKind, message: impl Into<String>, span: Span) -> Self {
        Self {
            error: AstEvalErrorDetail::new(kind, message),
            span,
        }
    }

    pub fn expression(message: impl Into<String>, span: Span) -> Self {
        Self::new(AstEvalErrorKind::Expression, message, span)
    }

    pub fn directive(message: impl Into<String>, span: Span) -> Self {
        Self::new(AstEvalErrorKind::Directive, message, span)
    }

    pub fn symbol(message: impl Into<String>, span: Span) -> Self {
        Self::new(AstEvalErrorKind::Symbol, message, span)
    }
}

/// Get the span of an expression.
pub fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Number(_, span)
        | Expr::Identifier(_, span)
        | Expr::Register(_, span)
        | Expr::List(_, span)
        | Expr::Index { span, .. }
        | Expr::Member { span, .. }
        | Expr::StructLiteral { span, .. }
        | Expr::Call { span, .. }
        | Expr::Placeholder(span)
        | Expr::Indirect(_, span)
        | Expr::IndirectLong(_, span)
        | Expr::Immediate(_, span)
        | Expr::Tuple(_, span)
        | Expr::Dollar(span)
        | Expr::String(_, span)
        | Expr::Error(_, span) => *span,
        Expr::Unary { span, .. }
        | Expr::Binary { span, .. }
        | Expr::Ternary { span, .. }
        | Expr::Range { span, .. } => *span,
    }
}

/// Get the text representation of an expression for diagnostics.
pub fn expr_text(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Number(text, _) => Some(text.clone()),
        Expr::Identifier(name, _) | Expr::Register(name, _) => Some(name.clone()),
        Expr::List(elements, _) => {
            let parts: Vec<_> = elements.iter().filter_map(expr_text).collect();
            if parts.len() == elements.len() {
                Some(format!("{{{}}}", parts.join(", ")))
            } else {
                None
            }
        }
        Expr::Index { base, index, .. } => {
            Some(format!("{}[{}]", expr_text(base)?, expr_text(index)?))
        }
        Expr::Member { base, field, .. } => Some(format!("{}.{}", expr_text(base)?, field)),
        Expr::StructLiteral {
            type_name, fields, ..
        } => {
            let mut parts = Vec::with_capacity(fields.len());
            for (name, value) in fields {
                parts.push(format!("{name}: {}", expr_text(value)?));
            }
            Some(format!("{type_name}{{{}}}", parts.join(", ")))
        }
        Expr::Call { name, args, .. } => {
            let parts: Vec<_> = args.iter().filter_map(expr_text).collect();
            if parts.len() == args.len() {
                Some(format!("{name}({})", parts.join(", ")))
            } else {
                None
            }
        }
        Expr::Placeholder(_) => Some("?".to_string()),
        Expr::Indirect(inner, _) => expr_text(inner).map(|text| format!("({text})")),
        Expr::IndirectLong(inner, _) => expr_text(inner).map(|text| format!("[{text}]")),
        Expr::Immediate(inner, _) => expr_text(inner).map(|text| format!("#{text}")),
        Expr::Tuple(elements, _) => {
            let parts: Vec<_> = elements.iter().filter_map(expr_text).collect();
            if parts.len() == elements.len() {
                Some(format!("({})", parts.join(", ")))
            } else {
                None
            }
        }
        Expr::Dollar(_) => Some("$".to_string()),
        Expr::String(_, _) => Some("<string>".to_string()),
        Expr::Error(_, _) => None,
        Expr::Range {
            start,
            end,
            step,
            inclusive,
            ..
        } => {
            let start_text = expr_text(start)?;
            let end_text = expr_text(end)?;
            if let Some(step_expr) = step {
                let step_text = expr_text(step_expr)?;
                Some(format!(
                    "{}{}{}:{}",
                    start_text,
                    if *inclusive { "..=" } else { ".." },
                    end_text,
                    step_text
                ))
            } else {
                Some(format!(
                    "{}{}{}",
                    start_text,
                    if *inclusive { "..=" } else { ".." },
                    end_text
                ))
            }
        }
        Expr::Unary { .. } | Expr::Binary { .. } | Expr::Ternary { .. } => None,
    }
}

/// Get the text representation of a binary operator.
pub fn binary_op_text(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Power => "**",
        BinaryOp::Divide => "/",
        BinaryOp::Mod => "%",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::Eq => "==",
        BinaryOp::Ne => "!=",
        BinaryOp::Ge => ">=",
        BinaryOp::Gt => ">",
        BinaryOp::Le => "<=",
        BinaryOp::Lt => "<",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::BitXor => "^",
        BinaryOp::LogicAnd => "&&",
        BinaryOp::LogicOr => "||",
        BinaryOp::LogicXor => "^^",
    }
}

/// Parse a number literal from text.
pub fn parse_number_text(text: &str, span: Span) -> Result<u32, AstEvalError> {
    let upper = text.to_ascii_uppercase();
    let cleaned = upper.replace('_', "");
    let (digits, base) = if let Some(rest) = cleaned.strip_prefix('$') {
        (rest.to_string(), 16)
    } else if let Some(rest) = cleaned.strip_prefix('%') {
        (rest.to_string(), 2)
    } else {
        match cleaned.chars().last() {
            Some('H') => (cleaned[..cleaned.len().saturating_sub(1)].to_string(), 16),
            Some('B') => {
                let inner = &cleaned[..cleaned.len().saturating_sub(1)];
                if inner.chars().all(|c| c == '0' || c == '1') {
                    (inner.to_string(), 2)
                } else {
                    (inner.to_string(), 16)
                }
            }
            Some('O') | Some('Q') => (cleaned[..cleaned.len().saturating_sub(1)].to_string(), 8),
            Some('D') => (cleaned[..cleaned.len().saturating_sub(1)].to_string(), 10),
            _ => (cleaned, 10),
        }
    };

    if digits.is_empty() {
        return Err(AstEvalError::expression(
            "Illegal character in constant",
            span,
        ));
    }

    let valid = match base {
        2 => digits.chars().all(|c| c == '0' || c == '1'),
        8 => digits.chars().all(|c| matches!(c, '0'..='7')),
        10 => digits.chars().all(|c| c.is_ascii_digit()),
        16 => digits.chars().all(|c| c.is_ascii_hexdigit()),
        _ => false,
    };

    if !valid {
        let msg = match base {
            10 => "Illegal character in decimal constant",
            2 => "Illegal character in binary constant",
            8 => "Illegal character in octal constant",
            16 => "Illegal character in hex constant",
            _ => "Illegal character in constant",
        };
        return Err(AstEvalError::expression(msg, span));
    }

    let value = u32::from_str_radix(&digits, base)
        .map_err(|_| AstEvalError::expression("Illegal character in constant", span))?;

    Ok(value)
}

/// Concatenate two values by shifting left to make room for right.
pub fn concat_values(left: u32, right: u32) -> u32 {
    let width = if right == 0 {
        1
    } else {
        (32 - right.leading_zeros()).div_ceil(8).min(4)
    };
    let shift = (width * 8).min(32);
    let mask = if shift >= 32 {
        u64::MAX
    } else {
        (1u64 << shift) - 1
    };
    let combined = ((left as u64) << shift) | ((right as u64) & mask);
    combined as u32
}

/// Repeat a byte value a number of times.
pub fn repeat_value(left: u32, right: u32) -> u32 {
    let count = right.min(4);
    let byte = left & 0xff;
    let mut result = 0u32;
    for _ in 0..count {
        result = (result << 8) | byte;
    }
    result
}

/// Apply an assignment operator to compute a new value.
pub fn apply_assignment_op(
    op: AssignOp,
    left: u32,
    right: u32,
    span: Span,
) -> Result<u32, AstEvalError> {
    let val = match op {
        AssignOp::Add => left.wrapping_add(right),
        AssignOp::Sub => left.wrapping_sub(right),
        AssignOp::Mul => left.wrapping_mul(right),
        AssignOp::Div => {
            if right == 0 {
                return Err(AstEvalError::expression("Divide by zero", span));
            }
            left / right
        }
        AssignOp::Mod => {
            if right == 0 {
                return Err(AstEvalError::expression("Divide by zero", span));
            }
            left % right
        }
        AssignOp::Pow => {
            if right > 63 {
                return Err(AstEvalError::expression(
                    "Exponent out of range for integer power",
                    span,
                ));
            }
            left.wrapping_pow(right)
        }
        AssignOp::BitOr => left | right,
        AssignOp::BitXor => left ^ right,
        AssignOp::BitAnd => left & right,
        AssignOp::LogicOr => u32::from(left != 0 || right != 0),
        AssignOp::LogicAnd => u32::from(left != 0 && right != 0),
        AssignOp::Shl => left.wrapping_shl(right & 0x1f),
        AssignOp::Shr => left >> (right & 0x1f),
        AssignOp::Concat => concat_values(left, right),
        AssignOp::Min => left.min(right),
        AssignOp::Max => left.max(right),
        AssignOp::Repeat => repeat_value(left, right),
        AssignOp::Member => right,
        AssignOp::Const | AssignOp::Var | AssignOp::VarIfUndef => right,
    };
    Ok(val)
}

/// Evaluate a unary operator.
pub fn eval_unary_op(op: UnaryOp, inner: u32) -> u32 {
    match op {
        UnaryOp::Plus => inner,
        UnaryOp::Minus => 0u32.wrapping_sub(inner),
        UnaryOp::BitNot => !inner,
        UnaryOp::LogicNot => u32::from(inner == 0),
        UnaryOp::High => (inner >> 8) & 0xff,
        UnaryOp::Low => inner & 0xff,
    }
}

/// Evaluate a binary operator.
pub fn eval_binary_op(
    op: BinaryOp,
    left_val: u32,
    right_val: u32,
    span: Span,
    line_end_span: Option<Span>,
) -> Result<u32, AstEvalError> {
    let val = match op {
        BinaryOp::Multiply => left_val.wrapping_mul(right_val),
        BinaryOp::Divide => {
            if right_val == 0 {
                let span = line_end_span.unwrap_or(span);
                return Err(AstEvalError::expression("Divide by zero", span));
            }
            left_val / right_val
        }
        BinaryOp::Mod => {
            if right_val == 0 {
                let span = line_end_span.unwrap_or(span);
                return Err(AstEvalError::expression("Divide by zero", span));
            }
            left_val % right_val
        }
        BinaryOp::Power => {
            if right_val > 63 {
                let span = line_end_span.unwrap_or(span);
                return Err(AstEvalError::expression(
                    "Exponent out of range for integer power",
                    span,
                ));
            }
            left_val.wrapping_pow(right_val)
        }
        BinaryOp::Add => left_val.wrapping_add(right_val),
        BinaryOp::Subtract => left_val.wrapping_sub(right_val),
        BinaryOp::Shl => left_val.wrapping_shl(right_val & 0x1f),
        BinaryOp::Shr => left_val >> (right_val & 0x1f),
        BinaryOp::Eq => u32::from(left_val == right_val),
        BinaryOp::Ne => u32::from(left_val != right_val),
        BinaryOp::Lt => u32::from((left_val as i32) < (right_val as i32)),
        BinaryOp::Le => u32::from((left_val as i32) <= (right_val as i32)),
        BinaryOp::Gt => u32::from((left_val as i32) > (right_val as i32)),
        BinaryOp::Ge => u32::from((left_val as i32) >= (right_val as i32)),
        BinaryOp::BitAnd => left_val & right_val,
        BinaryOp::BitOr => left_val | right_val,
        BinaryOp::BitXor => left_val ^ right_val,
        BinaryOp::LogicAnd => u32::from(left_val != 0 && right_val != 0),
        BinaryOp::LogicOr => u32::from(left_val != 0 || right_val != 0),
        BinaryOp::LogicXor => u32::from((left_val != 0) ^ (right_val != 0)),
    };
    Ok(val)
}

#[cfg(test)]
mod tests {
    use super::parse_number_text;
    use crate::tokenizer::{Span, TokenKind, Tokenizer};

    #[test]
    fn parse_number_text_matches_expected_ambiguous_suffix_literals() {
        let span = Span {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        for (text, expected) in [
            ("$BB", 0xBB),
            ("0B8H", 0x0B8),
            ("101B", 0b101),
            ("1_0_1B", 0b101),
        ] {
            let asm_val = match parse_number_text(text, span) {
                Ok(value) => value,
                Err(_) => panic!("expression parser should accept literal: {text}"),
            };
            assert_eq!(asm_val, expected, "literal {text}");
        }
    }

    #[test]
    fn tokenizer_number_literals_round_trip_through_parse_number_text() {
        let mut tok = Tokenizer::new("$BB 0B8H 101B", 1);
        let mut values = Vec::new();

        loop {
            let token = tok.next_token().expect("tokenization should succeed");
            match token.kind {
                TokenKind::Number(num) => {
                    let value = match parse_number_text(&num.text, token.span) {
                        Ok(value) => value,
                        Err(_) => panic!("number literal should parse: {}", num.text),
                    };
                    values.push(value);
                }
                TokenKind::End => break,
                _ => {}
            }
        }

        assert_eq!(values, vec![0xBB, 0x0B8, 0b101]);
    }
}
