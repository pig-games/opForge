// SPDX-License-Identifier: GPL-3.0-or-later
//! Shared PRVM invocation routing, fallback views, and scoped overload selection.
//! A9 follows A7's type rows before publication/end; no additional VM or BSP slot.
//! Scope rows are 16-byte BE {keyword offset, keyword bytes, plan ID, template ID}.
//! IDs resolve immutable owner storage; this service never owns source acquisition.
use crate::packed_statement_vm::{execute_match, Plan};
use opcore::parser::ParseError;
use opcore::tokenizer::Span;
use types::packed_statement::{TokenView, NONE};

pub const POLICY_BYTES: usize = 8;
const OP_INVOCATION: u8 = 0xa9;
const FOLD_ASCII: u8 = 1;
const COLUMN_ONE_LABEL: u8 = 2;
const DECIMAL_FALLBACK: u8 = 4;

pub use types::statement_invocation::{Route, ScopeRow, Selection};
#[derive(Debug, Clone, Copy)]
pub struct Policy {
    flags: u8,
    upper: (u8, u8),
    lower: (u8, u8),
    digits: (u8, u8),
}
fn error(message: &'static str, span: Span) -> ParseError {
    ParseError {
        message: message.into(),
        span,
    }
}
impl Policy {
    /// Receives only the A9 slice extracted by the current A7 program decoder.
    pub fn decode(bytes: &[u8], span: Span) -> Result<Self, ParseError> {
        if bytes.len() != POLICY_BYTES
            || bytes[0] != OP_INVOCATION
            || bytes[1] & !7 != 0
            || bytes[2] > bytes[3]
            || bytes[4] > bytes[5]
            || bytes[6] > bytes[7]
        {
            return Err(error("Invalid statement invocation policy", span));
        }
        Ok(Self {
            flags: bytes[1],
            upper: (bytes[2], bytes[3]),
            lower: (bytes[4], bytes[5]),
            digits: (bytes[6], bytes[7]),
        })
    }
    fn equal(&self, left: &[u8], right: &[u8]) -> bool {
        if self.flags & FOLD_ASCII != 0 {
            left.eq_ignore_ascii_case(right)
        } else {
            left == right
        }
    }
    fn letter(&self, byte: u8) -> bool {
        (self.upper.0..=self.upper.1).contains(&byte)
            || (self.lower.0..=self.lower.1).contains(&byte)
    }
}
pub(crate) fn text(arena: &[u8], offset: u32, bytes: u32, span: Span) -> Result<&[u8], ParseError> {
    let end = (offset as usize)
        .checked_add(bytes as usize)
        .ok_or_else(|| error("Invalid statement invocation extent", span))?;
    arena
        .get(offset as usize..end)
        .ok_or_else(|| error("Invalid statement invocation extent", span))
}

/// Select the longest registered keyword prefix. The caller tokenizes only the
/// residual owned keyword bytes using existing TKVM, then appends retained tail
/// views. No original source text, CPU parser, or shorter-keyword retry is used.
pub fn route(
    policy: &Policy,
    views: &[TokenView],
    arena: &[u8],
    scope: &[ScopeRow],
    keywords: &[u8],
    span: Span,
) -> Result<Option<Route>, ParseError> {
    for view in views {
        crate::packed_statement_vm::validate_view(view, arena, span)?;
    }
    validate_scope(scope, keywords, span)?;
    let Some(first) = views.first() else {
        return Ok(None);
    };
    if first.kind > 1 {
        return Ok(None);
    }
    let mut head = 0;
    let mut label = NONE;
    if policy.flags & COLUMN_ONE_LABEL != 0 && first.col_start == 1 {
        label = 0;
        head = 1;
        if views
            .get(head)
            .is_some_and(|v| v.kind == 5 && v.col_start == first.col_end)
        {
            head += 1;
        }
    }
    let Some(view) = views.get(head).filter(|v| v.kind <= 1) else {
        return Ok(None);
    };
    let spelling = text(arena, view.literal_offset, view.literal_bytes, span)?;
    let prefix = longest_prefix(policy, spelling, scope, keywords, span)?;
    Ok((prefix != 0).then_some(Route {
        label_token: label,
        head_token: head as u32,
        prefix_bytes: prefix,
    }))
}

/// Derived views borrow the same normalized arena. Decimal values are prepared
/// once here; a numeric overflow produces status zero, preserving lexical text.
pub fn fallback_views(
    policy: &Policy,
    views: &[TokenView],
    arena: &[u8],
    span: Span,
) -> Result<Vec<TokenView>, ParseError> {
    let mut output = Vec::with_capacity(views.len());
    for view in views {
        crate::packed_statement_vm::validate_view(view, arena, span)?;
    }
    for view in views {
        let bytes = text(arena, view.literal_offset, view.literal_bytes, span)?;
        if policy.flags & DECIMAL_FALLBACK == 0
            || view.kind > 1
            || bytes.len() < 2
            || !policy.letter(bytes[0])
            || !bytes[1..]
                .iter()
                .all(|b| (policy.digits.0..=policy.digits.1).contains(b))
        {
            output.push(*view);
            continue;
        }
        let letter_end = view
            .col_start
            .checked_add(1)
            .ok_or_else(|| error("Invalid statement invocation columns", span))?;
        let numeric_offset = view
            .literal_offset
            .checked_add(1)
            .ok_or_else(|| error("Invalid statement invocation extent", span))?;
        let radix = i64::from(policy.digits.1) - i64::from(policy.digits.0) + 1;
        let number = bytes[1..].iter().try_fold(0i64, |value, byte| {
            value
                .checked_mul(radix)?
                .checked_add(i64::from(*byte - policy.digits.0))
        });
        output.push(TokenView {
            literal_bytes: 1,
            col_end: letter_end,
            numeric_status: 0,
            numeric_value: 0,
            decoded_bytes: 0,
            ..*view
        });
        output.push(TokenView {
            kind: 2,
            literal_offset: numeric_offset,
            literal_bytes: view.literal_bytes - 1,
            col_start: letter_end,
            numeric_status: u8::from(number.is_some()),
            numeric_value: number.unwrap_or(0),
            decoded_bytes: 0,
            ..*view
        });
    }
    Ok(output)
}

/// Immutable owner storage. Plan IDs index the Arc-backed plan array.
pub struct ScopedPlans<'a> {
    pub rows: &'a [ScopeRow],
    pub keywords: &'a [u8],
    pub plans: &'a [Plan],
}
#[derive(Clone, Copy)]
pub struct MatchInput<'a> {
    pub route: Route,
    pub head: &'a [u8],
    pub views: &'a [TokenView],
    pub arena: &'a [u8],
    pub span: Span,
}
fn validate_scope(rows: &[ScopeRow], keywords: &[u8], span: Span) -> Result<(), ParseError> {
    for row in rows {
        let word = text(keywords, row.keyword_offset, row.keyword_bytes, span)?;
        if word.is_empty() || std::str::from_utf8(word).is_err() {
            return Err(error("Invalid statement scope keyword", span));
        }
    }
    Ok(())
}
pub fn longest_prefix(
    policy: &Policy,
    head: &[u8],
    rows: &[ScopeRow],
    keywords: &[u8],
    span: Span,
) -> Result<u32, ParseError> {
    validate_scope(rows, keywords, span)?;
    let mut longest = 0;
    for row in rows {
        let word = text(keywords, row.keyword_offset, row.keyword_bytes, span)?;
        if word.len() > longest
            && head
                .get(..word.len())
                .is_some_and(|prefix| policy.equal(prefix, word))
        {
            longest = word.len();
        }
    }
    Ok(longest as u32)
}
fn select_pass(
    policy: &Policy,
    program: &[u8],
    scope: &ScopedPlans<'_>,
    input: &MatchInput<'_>,
) -> Result<Option<Selection>, ParseError> {
    let prefix = &input.head[..input.route.prefix_bytes as usize];
    let mut best = None;
    let mut score = (0, 0);
    let mut tied = false;
    for (index, row) in scope.rows.iter().enumerate() {
        let word = text(
            scope.keywords,
            row.keyword_offset,
            row.keyword_bytes,
            input.span,
        )?;
        if !policy.equal(word, prefix) {
            continue;
        }
        let plan = &scope.plans[row.plan_id as usize];
        let Some(captures) = execute_match(program, plan, input.views, input.arena, input.span)?
        else {
            continue;
        };
        use types::packed_statement::{ATOM_SCORE, LITERAL_SCORE};
        let candidate = (
            u32::from_be_bytes(plan.0[LITERAL_SCORE..LITERAL_SCORE + 4].try_into().unwrap()),
            u32::from_be_bytes(plan.0[ATOM_SCORE..ATOM_SCORE + 4].try_into().unwrap()),
        );
        if best.is_none() || candidate > score {
            best = Some(Selection {
                scope_index: index as u32,
                plan_id: row.plan_id,
                template_id: row.template_id,
                captures,
                used_fallback: false,
            });
            score = candidate;
            tied = false;
        } else if candidate == score {
            tied = true;
        }
    }
    if tied {
        return Err(error("Ambiguous statement signature", input.span));
    }
    Ok(best)
}
/// Normal matching wins even when fallback could find a more specific signature.
/// Ambiguity never triggers fallback. Invalid scope IDs reject before matching.
pub fn select_overloads(
    policy: &Policy,
    program: &[u8],
    scope: &ScopedPlans<'_>,
    input: &MatchInput<'_>,
) -> Result<(Option<Selection>, Vec<TokenView>), ParseError> {
    crate::packed_statement_vm::validate_match_program(program, input.span)?;
    for view in input.views {
        crate::packed_statement_vm::validate_view(view, input.arena, input.span)?;
    }
    if scope
        .rows
        .iter()
        .any(|row| row.plan_id as usize >= scope.plans.len())
    {
        return Err(error("Invalid statement scope plan ID", input.span));
    }
    let prefix = longest_prefix(policy, input.head, scope.rows, scope.keywords, input.span)?;
    if prefix == 0 || prefix != input.route.prefix_bytes {
        return Err(error("Invalid statement keyword prefix", input.span));
    }
    if let Some(selected) = select_pass(policy, program, scope, input)? {
        return Ok((Some(selected), Vec::new()));
    }
    let fallback = fallback_views(policy, input.views, input.arena, input.span)?;
    let selected = select_pass(
        policy,
        program,
        scope,
        &MatchInput {
            views: &fallback,
            ..*input
        },
    )?
    .map(|mut selected| {
        selected.used_fallback = true;
        selected
    });
    Ok((selected, fallback))
}
pub fn decode_program(program: &[u8], span: Span) -> Result<Policy, ParseError> {
    crate::packed_statement_vm::validate_match_program(program, span)?;
    let offset =
        3 + package::statement_program::TYPE_COUNT * package::statement_program::TYPE_ROW_BYTES;
    Policy::decode(&program[offset..offset + POLICY_BYTES], span)
}

#[cfg(test)]
#[path = "statement_invocation/tests.rs"]
mod tests;
