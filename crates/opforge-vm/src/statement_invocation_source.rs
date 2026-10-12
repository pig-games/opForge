// SPDX-License-Identifier: GPL-3.0-or-later
//! Rust source-owner adapter for shared PRVM invocation services. Lex the source
//! once; only residual bytes of an owned keyword facet are tokenized again.
use crate::packed_statement_vm::{lower_signature, prepare_tokens, Plan};
use crate::statement_invocation::{
    decode_program, route, select_overloads, MatchInput, ScopedPlans,
};
use opcore::macro_processor::NativeStatementExport;
use opcore::parser::{ParseError, StatementCaptureText};
use opcore::tokenizer::{Span, Token, TokenKind, Tokenizer};
use types::packed_statement::NONE;
use types::statement_invocation::ScopeRow;

pub struct Invocation {
    pub definition: usize,
    pub label: Option<String>,
    pub captures: Vec<StatementCaptureText>,
}
fn token(lexer: &mut Tokenizer<'_>) -> Result<Token, ParseError> {
    lexer.next_token().map_err(|err| ParseError {
        message: err.message,
        span: err.span,
    })
}
pub fn select_source(
    program: &[u8],
    source: &str,
    line: u32,
    definitions: &[NativeStatementExport],
) -> Result<Option<Invocation>, ParseError> {
    if definitions.is_empty() {
        return Ok(None);
    }
    let mut lexer = Tokenizer::new(source, line);
    // Nonstatement prefixes remain the surrounding source owner's responsibility.
    let Ok(first) = token(&mut lexer) else {
        return Ok(None);
    };
    if !matches!(
        first.kind,
        TokenKind::Identifier(_) | TokenKind::Register(_)
    ) {
        return Ok(None);
    }
    let mut head_tokens = vec![first];
    if head_tokens[0].span.col_start == 1 {
        let next = token(&mut lexer)?;
        let colon = matches!(next.kind, TokenKind::Colon)
            && next.span.col_start == head_tokens[0].span.col_end;
        head_tokens.push(next);
        if colon {
            head_tokens.push(token(&mut lexer)?);
        }
        if !matches!(
            head_tokens.last().unwrap().kind,
            TokenKind::Identifier(_) | TokenKind::Register(_)
        ) {
            return Ok(None);
        }
    }
    let diagnostic = head_tokens[0].span;
    let policy = decode_program(program, diagnostic)?;
    let mut keywords = Vec::new();
    let mut rows = Vec::new();
    let mut plans = Vec::new();
    for (index, definition) in definitions.iter().enumerate() {
        rows.push(ScopeRow {
            keyword_offset: keywords.len() as u32,
            keyword_bytes: definition.keyword().len() as u32,
            plan_id: index as u32,
            template_id: index as u32,
        });
        keywords.extend(definition.keyword().as_bytes());
        plans.push(definition.signature().prepared_plan().map_or_else(
            || lower_signature(definition.signature()),
            |plan| Plan(plan.clone()),
        ));
    }
    let (heads, head_arena) = prepare_tokens(&head_tokens);
    let Some(routing) = route(&policy, &heads, &head_arena, &rows, &keywords, diagnostic)? else {
        return Ok(None);
    };
    let head = &heads[routing.head_token as usize];
    let spelling = &head_arena
        [head.literal_offset as usize..(head.literal_offset + head.literal_bytes) as usize];
    let residual = &spelling[routing.prefix_bytes as usize..];
    let mut suffix = Tokenizer::new(std::str::from_utf8(residual).unwrap(), line);
    let mut arguments = Vec::new();
    loop {
        let next = token(&mut suffix)?;
        if matches!(next.kind, TokenKind::End) {
            break;
        }
        arguments.push(next);
    }
    // Translate retained tail columns to the reference matcher coordinate space:
    // its input historically began at the residual keyword's first byte.
    let origin =
        head_tokens[routing.head_token as usize].span.col_start + routing.prefix_bytes as usize - 1;
    loop {
        let mut next = token(&mut lexer).map_err(|mut error| {
            error.span.col_start = error.span.col_start.saturating_sub(origin);
            error.span.col_end = error.span.col_end.saturating_sub(origin);
            error
        })?;
        if matches!(next.kind, TokenKind::End) {
            break;
        }
        next.span.col_start -= origin;
        next.span.col_end -= origin;
        arguments.push(next);
    }
    let (views, arena) = prepare_tokens(&arguments);
    let span = arguments.first().map_or(
        Span {
            line: 0,
            col_start: 0,
            col_end: 0,
        },
        |token| token.span,
    );
    let (selected, fallback) = select_overloads(
        &policy,
        program,
        &ScopedPlans {
            rows: &rows,
            keywords: &keywords,
            plans: &plans,
        },
        &MatchInput {
            route: routing,
            head: spelling,
            views: &views,
            arena: &arena,
            span,
        },
    )?;
    let Some(selected) = selected else {
        return Ok(None);
    };
    let selected_views = if selected.used_fallback {
        &fallback
    } else {
        &views
    };
    let plan = &plans[selected.plan_id as usize];
    let captures = selected
        .captures
        .iter()
        .map(|capture| {
            let view = &selected_views[capture.token_index as usize];
            StatementCaptureText {
                name: String::from_utf8_lossy(
                    &plan.0[capture.name_offset as usize
                        ..(capture.name_offset + capture.name_bytes) as usize],
                )
                .into_owned(),
                text: String::from_utf8_lossy(
                    &arena[view.literal_offset as usize
                        ..(view.literal_offset + view.literal_bytes) as usize],
                )
                .into_owned(),
            }
        })
        .collect();
    let label = (routing.label_token != NONE)
        .then(|| head_tokens[routing.label_token as usize].to_literal_text());
    Ok(Some(Invocation {
        definition: selected.template_id as usize,
        label,
        captures,
    }))
}
