// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared tokenizer-runtime utility helpers used by host bridge adapters.

use crate::portable_contract::{
    PortableOperatorKind, PortableSpan, PortableToken, PortableTokenKind,
};
use opcore::parser::ParseError;
use opcore::text_utils::is_ident_start;
use opcore::tokenizer::{Span, Token, TokenKind, Tokenizer};
use registry::syntax::RegisterChecker;

const IDENT_CLASS_ASCII_ALPHA: u32 = 1 << 0;
const IDENT_CLASS_ASCII_DIGIT: u32 = 1 << 1;
const IDENT_CLASS_UNDERSCORE: u32 = 1 << 2;
const IDENT_CLASS_DOLLAR: u32 = 1 << 3;
const IDENT_CLASS_AT_SIGN: u32 = 1 << 4;
const IDENT_CLASS_DOT: u32 = 1 << 5;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AsciiCaseRule {
    Preserve,
    AsciiLower,
    AsciiUpper,
}

pub struct TokenizerDiagCodes<'a> {
    pub invalid_char: &'a str,
    pub unterminated_string: &'a str,
    pub step_limit_exceeded: &'a str,
    pub token_limit_exceeded: &'a str,
    pub lexeme_limit_exceeded: &'a str,
    pub error_limit_exceeded: &'a str,
}

pub fn tokenizer_vm_error_code(invalid_char_code: &str) -> &str {
    let code = invalid_char_code.trim();
    if code.is_empty() {
        "vm-runtime"
    } else {
        code
    }
}

pub fn vm_diag_code_for_slot<'a>(diagnostics: &TokenizerDiagCodes<'a>, slot: u8) -> &'a str {
    match slot {
        0 => diagnostics.invalid_char,
        1 => diagnostics.unterminated_string,
        2 => diagnostics.step_limit_exceeded,
        3 => diagnostics.token_limit_exceeded,
        4 => diagnostics.lexeme_limit_exceeded,
        5 => diagnostics.error_limit_exceeded,
        _ => diagnostics.invalid_char,
    }
}

pub fn vm_read_u8(
    program: &[u8],
    pc: &mut usize,
    diag_code: &str,
    context: &str,
) -> Result<u8, String> {
    let Some(value) = program.get(*pc).copied() else {
        return Err(format!(
            "{}: tokenizer VM truncated while reading {}",
            diag_code, context
        ));
    };
    *pc += 1;
    Ok(value)
}

pub fn vm_read_u16(
    program: &[u8],
    pc: &mut usize,
    diag_code: &str,
    context: &str,
) -> Result<u16, String> {
    let lo = vm_read_u8(program, pc, diag_code, context)?;
    let hi = vm_read_u8(program, pc, diag_code, context)?;
    Ok(u16::from_le_bytes([lo, hi]))
}

pub fn vm_read_u32(
    program: &[u8],
    pc: &mut usize,
    diag_code: &str,
    context: &str,
) -> Result<u32, String> {
    let b0 = vm_read_u8(program, pc, diag_code, context)?;
    let b1 = vm_read_u8(program, pc, diag_code, context)?;
    let b2 = vm_read_u8(program, pc, diag_code, context)?;
    let b3 = vm_read_u8(program, pc, diag_code, context)?;
    Ok(u32::from_le_bytes([b0, b1, b2, b3]))
}

pub fn vm_offset_to_pc(
    program: &[u8],
    offset: u32,
    diag_code: &str,
    context: &str,
) -> Result<usize, String> {
    let offset = usize::try_from(offset).map_err(|_| {
        format!(
            "{}: tokenizer VM {} exceeds host address range",
            diag_code, context
        )
    })?;
    if offset > program.len() {
        return Err(format!(
            "{}: tokenizer VM {} {} exceeds program length {}",
            diag_code,
            context,
            offset,
            program.len()
        ));
    }
    Ok(offset)
}

pub fn vm_token_lexeme_len(token: &PortableToken) -> usize {
    match &token.kind {
        PortableTokenKind::Identifier(name) | PortableTokenKind::Register(name) => name.len(),
        PortableTokenKind::Number { text, .. } => text.len(),
        PortableTokenKind::String { bytes, .. } => bytes.len(),
        PortableTokenKind::Comma
        | PortableTokenKind::Colon
        | PortableTokenKind::Dollar
        | PortableTokenKind::Dot
        | PortableTokenKind::Hash
        | PortableTokenKind::Question
        | PortableTokenKind::OpenBracket
        | PortableTokenKind::CloseBracket
        | PortableTokenKind::OpenBrace
        | PortableTokenKind::CloseBrace
        | PortableTokenKind::OpenParen
        | PortableTokenKind::CloseParen => 1,
        PortableTokenKind::Operator(op) => match op {
            PortableOperatorKind::Power
            | PortableOperatorKind::Shl
            | PortableOperatorKind::Shr
            | PortableOperatorKind::LogicAnd
            | PortableOperatorKind::LogicOr
            | PortableOperatorKind::LogicXor
            | PortableOperatorKind::Eq
            | PortableOperatorKind::Ne
            | PortableOperatorKind::Ge
            | PortableOperatorKind::Le => 2,
            _ => 1,
        },
    }
}

pub fn source_line_can_tokenize_to_empty(source_line: &str, comment_prefix: &str) -> bool {
    let trimmed = source_line.trim_start();
    trimmed.is_empty() || (!comment_prefix.is_empty() && trimmed.starts_with(comment_prefix))
}

pub fn validate_line_column_one(line: &str, line_num: u32) -> Result<(), ParseError> {
    if let Some(first) = line.as_bytes().first().copied() {
        if !first.is_ascii_whitespace()
            && first != b';'
            && first != b'.'
            && first != b'*'
            && !is_ident_start(first)
        {
            return Err(ParseError {
                message: format!(
                    "Illegal character in column 1. Must be symbol, '.', '*', comment, or space. Found: {}",
                    line
                ),
                span: Span {
                    line: line_num,
                    col_start: 1,
                    col_end: 1,
                },
            });
        }
    }
    Ok(())
}

pub fn runtime_tokens_to_core_tokens(
    tokens: &[PortableToken],
    source_line: Option<&str>,
    register_checker: &RegisterChecker,
) -> Result<Vec<Token>, ParseError> {
    let mut core_tokens = Vec::with_capacity(tokens.len());
    for token in tokens {
        let span: Span = token.span.into();
        if span.col_start == 0 || span.col_end < span.col_start {
            return Err(ParseError {
                message: "runtime tokenizer produced invalid token span".to_string(),
                span,
            });
        }
        let mut core_token = token.to_core_token();
        if let Some(lexeme_text) = source_line
            .and_then(|line| source_slice_for_span(line, &span))
            .filter(|text| !text.is_empty())
        {
            match &mut core_token.kind {
                TokenKind::Identifier(name) | TokenKind::Register(name) => {
                    *name = lexeme_text.clone();
                }
                TokenKind::Number(number) => {
                    number.text = lexeme_text.clone();
                }
                TokenKind::String(string) => {
                    string.raw = lexeme_text;
                }
                _ => {}
            }
        }
        if let TokenKind::Identifier(name) = &core_token.kind {
            if register_checker(name.to_ascii_uppercase().as_str()) {
                core_token.kind = TokenKind::Register(name.clone());
            }
        }
        core_tokens.push(core_token);
    }
    Ok(core_tokens)
}

pub fn parser_end_metadata(line: &str, line_num: u32, tokens: &[Token]) -> (Span, Option<String>) {
    let mut end_col = line.len().saturating_add(1);
    let mut end_token_text = None;
    if let Some(comment_idx) = first_comment_semicolon_outside_quotes(line) {
        end_col = comment_idx.saturating_add(1);
        end_token_text = Some(";".to_string());
    }
    if let Some(last_token) = tokens.last() {
        if last_token.span.col_end >= end_col {
            end_col = last_token.span.col_end;
            end_token_text = None;
        }
    }
    (
        Span {
            line: line_num,
            col_start: end_col,
            col_end: end_col,
        },
        end_token_text,
    )
}

pub fn vm_char_class_matches(
    byte: Option<u8>,
    class: u8,
    identifier_start_class: u32,
    identifier_continue_class: u32,
    quote_chars: &str,
    punctuation_chars: &str,
    operator_chars: &str,
) -> bool {
    let Some(byte) = byte else {
        return false;
    };
    let ch = byte as char;
    match class {
        1 => ch.is_ascii_whitespace(),
        2 => vm_matches_identifier_start_class(byte, identifier_start_class),
        3 => vm_matches_identifier_continue_class(byte, identifier_continue_class),
        4 => ch.is_ascii_digit(),
        5 => quote_chars.as_bytes().contains(&byte),
        6 => punctuation_chars.as_bytes().contains(&byte),
        7 => operator_chars.as_bytes().contains(&byte),
        _ => false,
    }
}

pub fn vm_matches_identifier_start_class(byte: u8, class_mask: u32) -> bool {
    let is_alpha =
        (class_mask & IDENT_CLASS_ASCII_ALPHA) != 0 && (byte as char).is_ascii_alphabetic();
    let is_underscore = (class_mask & IDENT_CLASS_UNDERSCORE) != 0 && byte == b'_';
    let is_dot = (class_mask & IDENT_CLASS_DOT) != 0 && byte == b'.';
    is_alpha || is_underscore || is_dot
}

pub fn vm_matches_identifier_continue_class(byte: u8, class_mask: u32) -> bool {
    let ch = byte as char;
    let is_alpha = (class_mask & IDENT_CLASS_ASCII_ALPHA) != 0 && ch.is_ascii_alphabetic();
    let is_digit = (class_mask & IDENT_CLASS_ASCII_DIGIT) != 0 && ch.is_ascii_digit();
    let is_underscore = (class_mask & IDENT_CLASS_UNDERSCORE) != 0 && byte == b'_';
    let is_dollar = (class_mask & IDENT_CLASS_DOLLAR) != 0 && byte == b'$';
    let is_at = (class_mask & IDENT_CLASS_AT_SIGN) != 0 && byte == b'@';
    let is_dot = (class_mask & IDENT_CLASS_DOT) != 0 && byte == b'.';
    is_alpha || is_digit || is_underscore || is_dollar || is_at || is_dot
}

pub fn apply_identifier_case_rule(name: String, rule: AsciiCaseRule) -> String {
    match rule {
        AsciiCaseRule::Preserve => name,
        AsciiCaseRule::AsciiLower => name.to_ascii_lowercase(),
        AsciiCaseRule::AsciiUpper => name.to_ascii_uppercase(),
    }
}

pub fn apply_token_case_rule(token: PortableToken, rule: AsciiCaseRule) -> PortableToken {
    let kind = match token.kind {
        PortableTokenKind::Identifier(name) => {
            PortableTokenKind::Identifier(apply_identifier_case_rule(name, rule))
        }
        PortableTokenKind::Register(name) => {
            PortableTokenKind::Register(apply_identifier_case_rule(name, rule))
        }
        other => other,
    };
    PortableToken {
        kind,
        span: token.span,
    }
}

pub fn vm_build_token(
    kind_code: u8,
    lexeme: &[u8],
    line_num: u32,
    lexeme_start: usize,
    lexeme_end: usize,
    cursor: usize,
) -> Result<PortableToken, String> {
    let span_start = if lexeme_end > lexeme_start {
        lexeme_start
    } else {
        cursor
    };
    let span_end = if lexeme_end > lexeme_start {
        lexeme_end
    } else {
        cursor.saturating_add(1)
    };
    let span = PortableSpan {
        line: line_num,
        col_start: span_start.saturating_add(1),
        col_end: span_end.saturating_add(1),
    };
    let kind = match kind_code {
        0 => PortableTokenKind::Identifier(String::from_utf8_lossy(lexeme).to_string()),
        1 => PortableTokenKind::Register(String::from_utf8_lossy(lexeme).to_string()),
        2 => {
            let upper = String::from_utf8_lossy(lexeme).to_ascii_uppercase();
            let base = if upper.starts_with('$') {
                16
            } else if upper.starts_with('%') {
                2
            } else if upper.ends_with('H') {
                16
            } else if upper.ends_with('B') {
                2
            } else if upper.ends_with('O') || upper.ends_with('Q') {
                8
            } else {
                10
            };
            PortableTokenKind::Number { text: upper, base }
        }
        3 => PortableTokenKind::String {
            raw: String::from_utf8_lossy(lexeme).to_string(),
            bytes: lexeme.to_vec(),
        },
        4 => PortableTokenKind::Comma,
        5 => PortableTokenKind::Colon,
        6 => PortableTokenKind::Dollar,
        7 => PortableTokenKind::Dot,
        8 => PortableTokenKind::Hash,
        9 => PortableTokenKind::Question,
        10 => PortableTokenKind::OpenBracket,
        11 => PortableTokenKind::CloseBracket,
        12 => PortableTokenKind::OpenBrace,
        13 => PortableTokenKind::CloseBrace,
        14 => PortableTokenKind::OpenParen,
        15 => PortableTokenKind::CloseParen,
        _ => return Err(format!("unknown tokenizer VM token kind {}", kind_code)),
    };
    Ok(PortableToken { kind, span })
}

pub fn vm_scan_next_core_token<'a>(
    source_line: &'a str,
    line_num: u32,
    cursor: usize,
    tokenizer: &mut Option<Tokenizer<'a>>,
) -> Result<Option<(PortableToken, usize)>, String> {
    if cursor >= source_line.len() {
        return Ok(None);
    }

    if tokenizer.is_none() {
        *tokenizer = Some(Tokenizer::new(source_line, line_num));
    }
    let Some(tokenizer) = tokenizer.as_mut() else {
        return Ok(None);
    };
    loop {
        let token = tokenizer.next_token().map_err(|err| err.message)?;
        let token_end = token.span.col_end.saturating_sub(1);
        if token_end <= cursor {
            if matches!(token.kind, TokenKind::End) {
                return Ok(None);
            }
            continue;
        }
        if matches!(token.kind, TokenKind::End) {
            return Ok(None);
        }
        if let Some(portable) = PortableToken::from_core_token(token) {
            return Ok(Some((portable, token_end)));
        }
        return Ok(None);
    }
}

fn source_slice_for_span(line: &str, span: &Span) -> Option<String> {
    let start = span.col_start.checked_sub(1)?;
    let end = span.col_end.checked_sub(1)?;
    if start >= end {
        return None;
    }
    let bytes = line.as_bytes();
    if end > bytes.len() {
        return None;
    }
    Some(String::from_utf8_lossy(&bytes[start..end]).to_string())
}

fn first_comment_semicolon_outside_quotes(line: &str) -> Option<usize> {
    let bytes = line.as_bytes();
    let mut idx = 0usize;
    let mut quote: Option<u8> = None;
    while idx < bytes.len() {
        let byte = bytes[idx];
        if let Some(active_quote) = quote {
            if byte == b'\\' {
                idx = idx.saturating_add(2);
                continue;
            }
            if byte == active_quote {
                quote = None;
            }
            idx = idx.saturating_add(1);
            continue;
        }
        match byte {
            b'\'' | b'"' => quote = Some(byte),
            b';' => return Some(idx),
            _ => {}
        }
        idx = idx.saturating_add(1);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vm_read_u32_decodes_little_endian_bytes() {
        let mut pc = 0usize;
        let value =
            vm_read_u32(&[0x78, 0x56, 0x34, 0x12], &mut pc, "diag", "jump").expect("decoded value");
        assert_eq!(value, 0x1234_5678);
        assert_eq!(pc, 4);
    }

    #[test]
    fn vm_offset_to_pc_rejects_out_of_bounds_offset() {
        let err =
            vm_offset_to_pc(&[0x00, 0x01], 3, "diag", "jump target").expect_err("range error");
        assert!(err.contains("exceeds program length"));
    }

    #[test]
    fn vm_build_token_rejects_unknown_kind() {
        let err = vm_build_token(99, b"X", 1, 0, 1, 1).expect_err("unknown kind");
        assert!(err.contains("unknown tokenizer VM token kind"));
    }

    #[test]
    fn source_line_can_tokenize_to_empty_accepts_comments() {
        assert!(source_line_can_tokenize_to_empty("   ; comment", ";"));
        assert!(!source_line_can_tokenize_to_empty("lda #1", ";"));
    }

    #[test]
    fn vm_char_class_matches_uses_identifier_masks() {
        assert!(vm_char_class_matches(
            Some(b'a'),
            2,
            IDENT_CLASS_ASCII_ALPHA,
            IDENT_CLASS_ASCII_ALPHA,
            "\"",
            ",",
            "+-"
        ));
        assert!(!vm_char_class_matches(
            Some(b'1'),
            2,
            IDENT_CLASS_ASCII_ALPHA,
            IDENT_CLASS_ASCII_ALPHA,
            "\"",
            ",",
            "+-"
        ));
    }

    #[test]
    fn apply_identifier_case_rule_honors_ascii_modes() {
        assert_eq!(
            apply_identifier_case_rule("AbC".to_string(), AsciiCaseRule::AsciiLower),
            "abc"
        );
        assert_eq!(
            apply_identifier_case_rule("AbC".to_string(), AsciiCaseRule::AsciiUpper),
            "ABC"
        );
    }

    #[test]
    fn apply_token_case_rule_updates_identifier_and_register() {
        let ident = PortableToken {
            kind: PortableTokenKind::Identifier("AbC".to_string()),
            span: PortableSpan {
                line: 1,
                col_start: 1,
                col_end: 4,
            },
        };
        let reg = PortableToken {
            kind: PortableTokenKind::Register("xY".to_string()),
            span: PortableSpan {
                line: 1,
                col_start: 5,
                col_end: 7,
            },
        };

        let ident_folded = apply_token_case_rule(ident, AsciiCaseRule::AsciiLower);
        let reg_folded = apply_token_case_rule(reg, AsciiCaseRule::AsciiUpper);

        assert_eq!(
            ident_folded.kind,
            PortableTokenKind::Identifier("abc".to_string())
        );
        assert_eq!(
            reg_folded.kind,
            PortableTokenKind::Register("XY".to_string())
        );
    }

    #[test]
    fn vm_scan_next_core_token_reads_identifier() {
        let mut tokenizer = None;
        let result = vm_scan_next_core_token("label", 1, 0, &mut tokenizer).expect("scan");
        let (token, next_cursor) = result.expect("token expected");
        assert_eq!(
            token.kind,
            PortableTokenKind::Identifier("label".to_string())
        );
        assert_eq!(next_cursor, 5);
    }

    #[test]
    fn tokenizer_vm_error_code_defaults_when_blank() {
        assert_eq!(tokenizer_vm_error_code("   "), "vm-runtime");
        assert_eq!(tokenizer_vm_error_code("diag.invalid"), "diag.invalid");
    }

    #[test]
    fn vm_diag_code_for_slot_selects_expected_entry() {
        let diags = TokenizerDiagCodes {
            invalid_char: "d0",
            unterminated_string: "d1",
            step_limit_exceeded: "d2",
            token_limit_exceeded: "d3",
            lexeme_limit_exceeded: "d4",
            error_limit_exceeded: "d5",
        };
        assert_eq!(vm_diag_code_for_slot(&diags, 0), "d0");
        assert_eq!(vm_diag_code_for_slot(&diags, 4), "d4");
        assert_eq!(vm_diag_code_for_slot(&diags, 99), "d0");
    }
}
