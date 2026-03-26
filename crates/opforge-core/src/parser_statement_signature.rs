// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use crate::tokenizer::{Span, Token, TokenKind};

use super::{
    ParseError, SignatureAtom, StatementCapture, StatementCaptureText, StatementMatch,
    StatementSignature,
};

fn parse_number(text: &str) -> Option<i64> {
    let text = text.trim();
    if text.is_empty() {
        return None;
    }

    let (is_neg, text) = if let Some(rest) = text.strip_prefix('-') {
        (true, rest)
    } else {
        (false, text)
    };

    let text: String = text.chars().filter(|&c| c != '_').collect();
    let text = text.as_str();

    let val = if let Some(hex) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
        i64::from_str_radix(hex, 16).ok()?
    } else if let Some(oct) = text.strip_prefix("0o").or_else(|| text.strip_prefix("0O")) {
        i64::from_str_radix(oct, 8).ok()?
    } else if let Some(bin) = text.strip_prefix('%') {
        i64::from_str_radix(bin, 2).ok()?
    } else if let Some(hex) = text.strip_prefix('$') {
        i64::from_str_radix(hex, 16).ok()?
    } else if text.ends_with('h') || text.ends_with('H') {
        i64::from_str_radix(&text[..text.len() - 1], 16).ok()?
    } else if let Some(bin) = text.strip_prefix("0b").or_else(|| text.strip_prefix("0B")) {
        if !bin.is_empty() && bin.chars().all(|c| c == '0' || c == '1') {
            i64::from_str_radix(bin, 2).ok()?
        } else {
            return None;
        }
    } else if text.ends_with('b') || text.ends_with('B') {
        let inner = &text[..text.len() - 1];
        if inner.chars().all(|c| c == '0' || c == '1') {
            i64::from_str_radix(inner, 2).ok()?
        } else {
            i64::from_str_radix(&text[..text.len() - 1], 16).ok()?
        }
    } else if text.ends_with('o')
        || text.ends_with('O')
        || text.ends_with('q')
        || text.ends_with('Q')
    {
        i64::from_str_radix(&text[..text.len() - 1], 8).ok()?
    } else if text.ends_with('d') || text.ends_with('D') {
        text[..text.len() - 1].parse::<i64>().ok()?
    } else {
        text.parse::<i64>().ok()?
    };

    Some(if is_neg { -val } else { val })
}

fn value_fits_byte(value: i64) -> bool {
    (-128..=0xff).contains(&value)
}

fn value_fits_word(value: i64) -> bool {
    (-32768..=0xffff).contains(&value)
}

fn value_fits_long(value: i64) -> bool {
    (-2147483648..=0xffff_ffff).contains(&value)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SignatureScore {
    literal_atoms: usize,
    atom_count: usize,
}

impl SignatureScore {
    fn better_than(self, other: Self) -> bool {
        if self.literal_atoms != other.literal_atoms {
            return self.literal_atoms > other.literal_atoms;
        }
        self.atom_count > other.atom_count
    }
}

fn signature_score(signature: &StatementSignature) -> SignatureScore {
    let mut literal_atoms = 0usize;
    for atom in &signature.atoms {
        if matches!(atom, SignatureAtom::Literal(_, _)) {
            literal_atoms += 1;
        }
    }
    SignatureScore {
        literal_atoms,
        atom_count: signature.atoms.len(),
    }
}

fn token_text(token: &Token) -> String {
    token.to_literal_text()
}

fn matches_literal(tokens: &[Token], start: usize, literal: &str) -> Option<(usize, Span)> {
    let mut acc = String::new();
    let mut idx = start;
    let mut last_span: Option<Span> = None;
    while idx < tokens.len() && acc.len() < literal.len() {
        let token = &tokens[idx];
        if let Some(prev) = last_span {
            if token.span.col_start != prev.col_end {
                return None;
            }
        }
        acc.push_str(&token_text(token));
        last_span = Some(token.span);
        if !literal.starts_with(&acc) {
            return None;
        }
        if acc == literal {
            return Some((idx + 1, token.span));
        }
        idx += 1;
    }
    None
}

fn match_signature_atoms(
    atoms: &[SignatureAtom],
    tokens: &[Token],
    start: usize,
    require_adjacent: bool,
    prev_span: Option<Span>,
    captures: &mut Vec<StatementCapture>,
) -> Option<(usize, Option<Span>)> {
    let mut idx = start;
    let mut last_span = prev_span;
    for atom in atoms {
        match atom {
            SignatureAtom::Literal(bytes, _) => {
                let literal = String::from_utf8_lossy(bytes).to_string();
                let (next_idx, span) = matches_literal(tokens, idx, &literal)?;
                if require_adjacent {
                    if let Some(prev) = last_span {
                        let first_span = tokens[idx].span;
                        if first_span.col_start != prev.col_end {
                            return None;
                        }
                    }
                }
                idx = next_idx;
                last_span = Some(span);
            }
            SignatureAtom::Capture {
                name, type_name, ..
            } => {
                let token = tokens.get(idx)?;
                if require_adjacent {
                    if let Some(prev) = last_span {
                        if token.span.col_start != prev.col_end {
                            return None;
                        }
                    }
                }
                if !token_matches_capture_type(type_name, token) {
                    return None;
                }
                captures.push(StatementCapture {
                    name: name.clone(),
                    tokens: vec![token.clone()],
                });
                last_span = Some(token.span);
                idx += 1;
            }
            SignatureAtom::Boundary { atoms: inner, .. } => {
                let (next_idx, inner_span) =
                    match_signature_atoms(inner, tokens, idx, true, last_span, captures)?;
                idx = next_idx;
                if let Some(span) = inner_span {
                    last_span = Some(span);
                }
            }
        }
    }
    Some((idx, last_span))
}

fn token_matches_capture_type(type_name: &str, token: &Token) -> bool {
    match type_name.to_ascii_lowercase().as_str() {
        "byte" => token_matches_byte(token),
        "word" => token_matches_word(token),
        "long" => token_matches_long(token),
        "char" => token_matches_char(token),
        "str" => token_matches_str(token),
        _ => matches_any_capture_token(token),
    }
}

pub(super) fn is_valid_capture_type(type_name: &str) -> bool {
    matches!(
        type_name.to_ascii_lowercase().as_str(),
        "byte" | "word" | "long" | "char" | "str"
    )
}

fn matches_any_capture_token(token: &Token) -> bool {
    matches!(
        token.kind,
        TokenKind::Identifier(_)
            | TokenKind::Register(_)
            | TokenKind::Number(_)
            | TokenKind::String(_)
    )
}

fn token_matches_byte(token: &Token) -> bool {
    match &token.kind {
        TokenKind::Number(lit) => parse_number(&lit.text).is_some_and(value_fits_byte),
        TokenKind::Identifier(_) | TokenKind::Register(_) => true,
        TokenKind::String(lit) => lit.bytes.len() == 1,
        _ => false,
    }
}

fn token_matches_word(token: &Token) -> bool {
    match &token.kind {
        TokenKind::Number(lit) => parse_number(&lit.text).is_some_and(value_fits_word),
        TokenKind::Identifier(_) | TokenKind::Register(_) => true,
        TokenKind::String(lit) => lit.bytes.len() == 1 || lit.bytes.len() == 2,
        _ => false,
    }
}

fn token_matches_long(token: &Token) -> bool {
    match &token.kind {
        TokenKind::Number(lit) => parse_number(&lit.text).is_some_and(value_fits_long),
        TokenKind::Identifier(_) | TokenKind::Register(_) => true,
        _ => false,
    }
}

fn token_matches_char(token: &Token) -> bool {
    match &token.kind {
        TokenKind::Identifier(text) | TokenKind::Register(text) => text.len() == 1,
        TokenKind::String(lit) => lit.bytes.len() == 1,
        _ => false,
    }
}

fn token_matches_str(token: &Token) -> bool {
    matches!(token.kind, TokenKind::String(_))
}

pub(crate) fn match_statement_signature(
    signature: &StatementSignature,
    tokens: &[Token],
) -> Option<StatementMatch> {
    let mut captures = Vec::new();
    let (next_idx, _) =
        match_signature_atoms(&signature.atoms, tokens, 0, false, None, &mut captures)?;
    if next_idx == tokens.len() {
        Some(StatementMatch { captures })
    } else {
        None
    }
}

pub(crate) fn select_statement_signature(
    signatures: &[StatementSignature],
    tokens: &[Token],
) -> Result<Option<usize>, ParseError> {
    let mut best_idx = None;
    let mut best_score = SignatureScore {
        literal_atoms: 0,
        atom_count: 0,
    };
    let mut tied = false;

    for (idx, signature) in signatures.iter().enumerate() {
        if match_statement_signature(signature, tokens).is_none() {
            continue;
        }
        let score = signature_score(signature);
        if best_idx.is_none() || score.better_than(best_score) {
            best_idx = Some(idx);
            best_score = score;
            tied = false;
        } else if score == best_score {
            tied = true;
        }
    }

    if tied {
        let span = tokens.first().map(|t| t.span).unwrap_or(Span {
            line: 0,
            col_start: 0,
            col_end: 0,
        });
        return Err(ParseError {
            message: "Ambiguous statement signature".to_string(),
            span,
        });
    }
    Ok(best_idx)
}

pub fn select_and_match_statement_signature_texts(
    signatures: &[StatementSignature],
    tokens: &[Token],
) -> Result<Option<(usize, Vec<StatementCaptureText>)>, ParseError> {
    let Some(idx) = select_statement_signature(signatures, tokens)? else {
        return Ok(None);
    };
    let statement_match =
        match_statement_signature(&signatures[idx], tokens).ok_or_else(|| ParseError {
            message: "Statement signature match failed".to_string(),
            span: tokens.first().map(|t| t.span).unwrap_or(Span {
                line: 0,
                col_start: 0,
                col_end: 0,
            }),
        })?;
    let captures = statement_match
        .captures
        .into_iter()
        .map(|capture| StatementCaptureText {
            name: capture.name,
            text: capture
                .tokens
                .iter()
                .map(Token::to_literal_text)
                .collect::<String>(),
        })
        .collect();
    Ok(Some((idx, captures)))
}
