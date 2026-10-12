// SPDX-License-Identifier: GPL-3.0-or-later
use super::*;
use opcore::parser::StatementCaptureText;
use types::packed_statement::{Capture, TokenView};

pub fn prepare_tokens(tokens: &[Token]) -> (Vec<TokenView>, Vec<u8>) {
    let mut arena = Vec::new();
    let views = tokens
        .iter()
        .map(|token| {
            let kind = compiler::kind(token);
            let text = statement_program::TOKEN_RENDER_SPELLINGS
                .iter()
                .find(|(id, _)| *id == kind)
                .map_or_else(|| token.to_literal_text(), |(_, text)| (*text).to_owned());
            let offset = arena.len() as u32;
            arena.extend(text.as_bytes());
            let (number, decoded) = match &token.kind {
                TokenKind::Identifier(_) => (None, 0),
                TokenKind::Register(_) => (None, 0),
                TokenKind::Number(n) => (opcore::expr::parse_number(&n.text), 0),
                TokenKind::String(s) => (None, s.bytes.len() as u32),
                _ => (None, 0),
            };
            TokenView {
                kind,
                numeric_status: u8::from(number.is_some()),
                literal_offset: offset,
                literal_bytes: text.len() as u32,
                decoded_bytes: decoded,
                col_start: token.span.col_start as u32,
                col_end: token.span.col_end as u32,
                numeric_value: number.unwrap_or(0),
            }
        })
        .collect();
    (views, arena)
}

struct Policy<'a> {
    rows: &'a [u8],
}
impl<'a> Policy<'a> {
    fn decode(program: &'a [u8], span: Span) -> Result<Self, ParseError> {
        let invalid = || error("Invalid statement matching program", span);
        if program.get(..3)
            != Some(&[
                statement_program::MATCH,
                1,
                statement_program::TYPE_COUNT as u8,
            ])
        {
            return Err(invalid());
        }
        let end = 3 + statement_program::TYPE_COUNT * statement_program::TYPE_ROW_BYTES;
        if program.get(end..) != Some(&[statement_program::PUBLISH, statement_program::END]) {
            return Err(invalid());
        }
        let rows = &program[3..end];
        for (index, row) in rows
            .chunks_exact(statement_program::TYPE_ROW_BYTES)
            .enumerate()
        {
            if row[0] != index as u8 + 1 || row[1] > 2 || row[20..] != [0; 4] {
                return Err(invalid());
            }
        }
        Ok(Self { rows })
    }
    fn accepts(&self, ty: u8, token: &TokenView) -> bool {
        let Some(row) = self
            .rows
            .chunks_exact(statement_program::TYPE_ROW_BYTES)
            .find(|r| r[0] == ty)
        else {
            return false;
        };
        match token.kind {
            0 | 1 => row[1] == 1 || (row[1] == 2 && token.literal_bytes == 1),
            2 => {
                let min = i64::from_be_bytes(row[4..12].try_into().unwrap());
                let max = i64::from_be_bytes(row[12..20].try_into().unwrap());
                token.numeric_status == 1 && (min..=max).contains(&token.numeric_value)
            }
            3 => {
                row[3] != 0
                    && token.decoded_bytes >= u32::from(row[2])
                    && (row[3] == 255 || token.decoded_bytes <= u32::from(row[3]))
            }
            _ => false,
        }
    }
}

struct ValidPlan<'a> {
    bytes: &'a [u8],
    count: usize,
}

pub fn validate_match_program(program: &[u8], span: Span) -> Result<(), ParseError> {
    Policy::decode(program, span).map(|_| ())
}
impl<'a> ValidPlan<'a> {
    fn decode(plan: &'a Plan, span: Span) -> Result<Self, ParseError> {
        let bytes = &plan.0;
        let invalid = || error("Invalid statement signature plan", span);
        if read_u32(bytes, wire::EXTENT).map(|n| n as usize) != Some(bytes.len()) {
            return Err(invalid());
        }
        let count = read_u32(bytes, wire::ROW_COUNT).ok_or_else(invalid)? as usize;
        let payload = wire::HEADER_BYTES
            .checked_add(count.checked_mul(wire::ROW_BYTES).ok_or_else(invalid)?)
            .ok_or_else(invalid)?;
        if payload > bytes.len() {
            return Err(invalid());
        }
        let mut stack = Vec::new();
        let mut literals = 0;
        let mut atoms = 0;
        for index in 0..count {
            let base = wire::HEADER_BYTES + index * wire::ROW_BYTES;
            let row = &bytes[base..base + wire::ROW_BYTES];
            let off = read_u32(row, wire::PAYLOAD_OFFSET).unwrap() as usize;
            let len = read_u32(row, wire::PAYLOAD_BYTES).unwrap() as usize;
            let pair = read_u32(row, wire::PAIRED_ROW).unwrap();
            if row[2..4] != [0; 2]
                || off < payload
                || off.checked_add(len).is_none_or(|end| end > bytes.len())
                || read_u32(row, wire::COL_START) > read_u32(row, wire::COL_END)
            {
                return Err(invalid());
            }
            if stack.is_empty() && row[0] != wire::BOUNDARY_CLOSE {
                atoms += 1;
                if row[0] == wire::LITERAL {
                    literals += 1;
                }
            }
            match row[0] {
                wire::LITERAL if row[1] == 0 && pair == wire::NONE => {}
                wire::CAPTURE
                    if (wire::BYTE..=wire::STR).contains(&row[1])
                        && pair == wire::NONE
                        && len > 0 => {}
                wire::BOUNDARY_OPEN
                    if row[1] == 0
                        && len == 0
                        && pair as usize > index
                        && (pair as usize) < count =>
                {
                    stack.push((index, pair as usize))
                }
                wire::BOUNDARY_CLOSE if row[1] == 0 && len == 0 => {
                    if stack.pop() != Some((pair as usize, index)) {
                        return Err(invalid());
                    }
                }
                _ => return Err(invalid()),
            }
        }
        if !stack.is_empty()
            || read_u32(bytes, wire::LITERAL_SCORE) != Some(literals)
            || read_u32(bytes, wire::ATOM_SCORE) != Some(atoms)
        {
            return Err(invalid());
        }
        Ok(Self { bytes, count })
    }
    fn row(&self, index: usize) -> &[u8] {
        let base = wire::HEADER_BYTES + index * wire::ROW_BYTES;
        &self.bytes[base..base + wire::ROW_BYTES]
    }
}

/// Entry 12 validates and matches a compiled plan using prepared lexical facets.
/// No expression evaluation or number-spelling parsing occurs in this service.
pub fn execute_match(
    program: &[u8],
    plan: &Plan,
    tokens: &[TokenView],
    arena: &[u8],
    span: Span,
) -> Result<Option<Vec<Capture>>, ParseError> {
    let policy = Policy::decode(program, span)?;
    let plan = ValidPlan::decode(plan, span)?;
    let mut captures = Vec::new();
    let mut token_index = 0;
    let mut last_end = None;
    let mut boundary_depth = 0;
    for token in tokens {
        validate_view(token, arena, span)?;
    }
    for index in 0..plan.count {
        let row = plan.row(index);
        match row[0] {
            wire::BOUNDARY_OPEN => {
                boundary_depth += 1;
                continue;
            }
            wire::BOUNDARY_CLOSE => {
                boundary_depth -= 1;
                continue;
            }
            _ => {}
        }
        let Some(token) = tokens.get(token_index) else {
            return Ok(None);
        };
        if boundary_depth > 0 && last_end.is_some_and(|end| end != token.col_start) {
            return Ok(None);
        }
        let offset = read_u32(row, wire::PAYLOAD_OFFSET).unwrap();
        let len = read_u32(row, wire::PAYLOAD_BYTES).unwrap();
        if row[0] == wire::CAPTURE {
            if !policy.accepts(row[1], token) {
                return Ok(None);
            }
            captures.push(Capture {
                name_offset: offset,
                name_bytes: len,
                token_index: token_index as u32,
            });
            token_index += 1;
            last_end = Some(token.col_end);
        } else {
            let literal = &plan.bytes[offset as usize..(offset + len) as usize];
            let mut consumed = 0;
            let mut previous = None;
            while consumed < literal.len() {
                let Some(token) = tokens.get(token_index) else {
                    return Ok(None);
                };
                if previous.is_some_and(|end| end != token.col_start) {
                    return Ok(None);
                }
                let text = &arena[token.literal_offset as usize
                    ..(token.literal_offset + token.literal_bytes) as usize];
                if !literal[consumed..].starts_with(text) {
                    return Ok(None);
                }
                consumed += text.len();
                previous = Some(token.col_end);
                token_index += 1;
            }
            // Rust's empty literal fails rather than consuming zero tokens.
            if consumed == 0 {
                return Ok(None);
            }
            last_end = previous;
        }
    }
    Ok((token_index == tokens.len()).then_some(captures))
}

pub fn select_and_match_statement_signature_texts(
    signatures: &[StatementSignature],
    tokens: &[Token],
) -> Result<Option<(usize, Vec<StatementCaptureText>)>, ParseError> {
    let plans: Vec<_> = signatures
        .iter()
        .map(|s| {
            s.prepared_plan()
                .map_or_else(|| lower_signature(s), |p| Plan(p.clone()))
        })
        .collect();
    select_plans(&plans, tokens)
}

pub fn select_plans(
    plans: &[Plan],
    tokens: &[Token],
) -> Result<Option<(usize, Vec<StatementCaptureText>)>, ParseError> {
    let span = tokens.first().map_or(
        Span {
            line: 0,
            col_start: 0,
            col_end: 0,
        },
        |t| t.span,
    );
    let (views, arena) = prepare_tokens(tokens);
    let program = statement_program::match_program();
    let mut best = None;
    let mut score = (0, 0);
    let mut tied = false;
    for (index, plan) in plans.iter().enumerate() {
        let Some(captures) = execute_match(&program, plan, &views, &arena, span)? else {
            continue;
        };
        let candidate = (
            read_u32(&plan.0, wire::LITERAL_SCORE).unwrap(),
            read_u32(&plan.0, wire::ATOM_SCORE).unwrap(),
        );
        if best.is_none() || candidate > score {
            best = Some((index, captures));
            score = candidate;
            tied = false;
        } else if candidate == score {
            tied = true;
        }
    }
    if tied {
        return Err(error("Ambiguous statement signature", span));
    }
    Ok(best.map(|(index, captures)| {
        let plan = &plans[index];
        let texts = captures
            .into_iter()
            .map(|capture| StatementCaptureText {
                name: String::from_utf8_lossy(
                    &plan.0[capture.name_offset as usize
                        ..(capture.name_offset + capture.name_bytes) as usize],
                )
                .into_owned(),
                text: tokens[capture.token_index as usize].to_literal_text(),
            })
            .collect();
        (index, texts)
    }))
}
