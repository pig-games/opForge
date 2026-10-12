// SPDX-License-Identifier: GPL-3.0-or-later
use super::*;

pub fn validate_head_program(program: &[u8], span: Span) -> Result<(), ParseError> {
    if program.len() != 8
        || program[..2] != [statement_program::HEAD, 1]
        || program[6..] != [statement_program::PUBLISH, statement_program::END]
        || program[2..4] == [0, 0]
        || program[4..6] == [0, 0]
        || program[2..4] == program[4..6]
    {
        return Err(error("Invalid statement head program", span));
    }
    Ok(())
}

/// Resolve package-selected head identities over already prepared tokens. In
/// complete mode validate the definition keyword before publication; signature
/// validation belongs to entry 11. The capture owner translates token indices
/// to its owning packed-record offsets.
pub fn select_head(
    program: &[u8],
    mode: u32,
    tokens: &[wire::TokenView],
    names: &[u16],
    span: Span,
) -> Result<Option<wire::Head>, ParseError> {
    if !matches!(mode, wire::HEAD_CLASSIFY | wire::HEAD_COMPLETE) {
        return Err(error("Invalid statement head mode", span));
    }
    validate_head_program(program, span)?;
    if names.len() != tokens.len() {
        return Err(error("Invalid statement head identities", span));
    }
    let mut index = 0;
    let mut labelled = false;
    if tokens.first().is_some_and(|t| t.kind <= 1) {
        labelled = true;
        index += 1;
        if tokens.get(index).is_some_and(|t| t.kind == 5) {
            index += 1;
        }
    }
    if tokens.get(index).is_none_or(|t| t.kind != 7)
        || tokens.get(index + 1).is_none_or(|t| t.kind > 1)
    {
        return Ok(None);
    }
    let selected = names[index + 1];
    let definition = u16::from_be_bytes(program[2..4].try_into().unwrap());
    let end = u16::from_be_bytes(program[4..6].try_into().unwrap());
    let action = if selected == definition {
        1
    } else if selected == end {
        2
    } else {
        return Ok(None);
    };
    if mode == wire::HEAD_COMPLETE && labelled && action == 1 {
        return Err(error("Expected .statement definition", span));
    }
    if mode == wire::HEAD_COMPLETE
        && action == 1
        && tokens.get(index + 2).is_none_or(|t| t.kind > 1)
    {
        let keyword_span = tokens.get(index + 2).map_or(span, |t| Span {
            line: span.line,
            col_start: t.col_start as usize,
            col_end: t.col_end as usize,
        });
        return Err(error("Expected statement keyword", keyword_span));
    }
    Ok(Some(wire::Head {
        action,
        keyword_token: (index + 2) as u32,
        signature_token: (index + 3) as u32,
    }))
}

/// Hook classification only needs the source head; it must not reject arbitrary
/// ignored definition-body text by tokenizing the complete tail.
pub fn classify_directive(code: &str) -> Option<opcore::macro_processor::StatementDirectiveKind> {
    let mut tokenizer = Tokenizer::new(code, 0);
    let mut tokens = Vec::new();
    for _ in 0..4 {
        let token = tokenizer.next_token().ok()?;
        if matches!(token.kind, TokenKind::End) {
            break;
        }
        let head = matches!(
            token.kind,
            TokenKind::Identifier(_) | TokenKind::Register(_)
        ) && tokens
            .last()
            .is_some_and(|t: &Token| matches!(t.kind, TokenKind::Dot));
        tokens.push(token);
        if head {
            break;
        }
    }
    let names: Vec<_> = tokens
        .iter()
        .map(|token| {
            if let TokenKind::Identifier(name) | TokenKind::Register(name) = &token.kind {
                statement_program::HEAD_SPELLINGS
                    .iter()
                    .position(|head| head.eq_ignore_ascii_case(name))
                    .map_or(0, |i| i as u16 + 1)
            } else {
                0
            }
        })
        .collect();
    let (views, _) = prepare_tokens(&tokens);
    match select_head(
        &statement_program::head_program([1, 2]),
        wire::HEAD_CLASSIFY,
        &views,
        &names,
        tokens.first().map_or(
            Span {
                line: 0,
                col_start: 0,
                col_end: 0,
            },
            |t| t.span,
        ),
    ) {
        Ok(Some(head)) if head.action == 1 => {
            Some(opcore::macro_processor::StatementDirectiveKind::Def)
        }
        Ok(Some(_)) => Some(opcore::macro_processor::StatementDirectiveKind::End),
        _ => None,
    }
}
