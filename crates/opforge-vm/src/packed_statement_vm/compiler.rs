// SPDX-License-Identifier: GPL-3.0-or-later
use super::*;

struct Grammar<'a> {
    kinds: &'a [u8],
    types: Vec<(u8, &'a str)>,
    renders: Vec<(u8, &'a str)>,
}
impl<'a> Grammar<'a> {
    fn decode(program: &'a [u8], span: Span) -> Result<Self, ParseError> {
        let invalid = || error("Invalid statement compilation program", span);
        if program.len() < 11 || program[0] != statement_program::COMPILE {
            return Err(invalid());
        }
        let kinds = &program[1..8];
        let mut cursor = 9;
        let mut types = Vec::new();
        for _ in 0..program[8] {
            let id = *program.get(cursor).ok_or_else(invalid)?;
            let len = usize::from(*program.get(cursor + 1).ok_or_else(invalid)?);
            cursor += 2;
            let name = std::str::from_utf8(program.get(cursor..cursor + len).ok_or_else(invalid)?)
                .map_err(|_| invalid())?;
            if !(wire::BYTE..=wire::STR).contains(&id)
                || name.is_empty()
                || types.iter().any(|(other, _)| *other == id)
            {
                return Err(invalid());
            }
            types.push((id, name));
            cursor += len;
        }
        let count = usize::from(*program.get(cursor).ok_or_else(invalid)?);
        cursor += 1;
        let mut renders = Vec::new();
        for _ in 0..count {
            let kind = *program.get(cursor).ok_or_else(invalid)?;
            let len = usize::from(*program.get(cursor + 1).ok_or_else(invalid)?);
            cursor += 2;
            let text = std::str::from_utf8(program.get(cursor..cursor + len).ok_or_else(invalid)?)
                .map_err(|_| invalid())?;
            if !(4..=40).contains(&kind)
                || text.is_empty()
                || renders.iter().any(|(prior, _)| *prior == kind)
            {
                return Err(invalid());
            }
            renders.push((kind, text));
            cursor += len;
        }
        if types.len() != statement_program::TYPE_COUNT
            || renders.len() != statement_program::TOKEN_RENDER_SPELLINGS.len()
        {
            return Err(invalid());
        }
        if program.get(cursor..) != Some(&[statement_program::PUBLISH, statement_program::END]) {
            return Err(invalid());
        }
        Ok(Self {
            kinds,
            types,
            renders,
        })
    }
}
pub(super) fn kind(token: &Token) -> u8 {
    use crate::packed_source_layout as source;
    match token.kind {
        TokenKind::Identifier(_) => source::TOKEN_IDENTIFIER,
        TokenKind::Register(_) => source::TOKEN_REGISTER,
        TokenKind::String(_) => source::TOKEN_STRING,
        TokenKind::Number(_) => source::TOKEN_NUMBER,
        TokenKind::Dot => source::TOKEN_DOT,
        TokenKind::Comma => source::TOKEN_COMMA,
        TokenKind::Colon => source::TOKEN_COLON,
        TokenKind::Dollar => 6,
        TokenKind::Hash => 8,
        TokenKind::Question => 9,
        TokenKind::At => 40,
        TokenKind::OpenParen => source::TOKEN_OPEN_PAREN,
        TokenKind::CloseParen => source::TOKEN_CLOSE_PAREN,
        TokenKind::Operator(op) => source::TOKEN_OPERATOR_FIRST + op as u8,
        TokenKind::OpenBracket => source::TOKEN_OPEN_BRACKET,
        TokenKind::CloseBracket => source::TOKEN_CLOSE_BRACKET,
        TokenKind::OpenBrace => source::TOKEN_OPEN_BRACE,
        TokenKind::CloseBrace => source::TOKEN_CLOSE_BRACE,
        _ => 255,
    }
}
fn parse(
    grammar: &Grammar<'_>,
    tokens: &[Token],
    cursor: &mut usize,
    boundary: bool,
    end: Span,
) -> Result<StatementSignature, ParseError> {
    let mut atoms = Vec::new();
    let mut closed = !boundary;
    let is = |index: usize, expected: u8| tokens.get(index).is_some_and(|t| kind(t) == expected);
    let k = grammar.kinds;
    while *cursor < tokens.len() {
        if boundary && is(*cursor, k[4]) && is(*cursor + 1, k[5]) {
            *cursor += 2;
            closed = true;
            break;
        }
        if boundary && is(*cursor, k[4]) {
            return Err(error("Missing closing }]", tokens[*cursor].span));
        }
        if is(*cursor, k[2]) && is(*cursor + 1, k[3]) {
            let start = tokens[*cursor].span;
            *cursor += 2;
            let inner = parse(grammar, tokens, cursor, true, end)?;
            let tail = tokens.get(cursor.saturating_sub(1)).map_or(end, |t| t.span);
            atoms.push(SignatureAtom::Boundary {
                atoms: inner.into_atoms(),
                span: Span {
                    line: start.line,
                    col_start: start.col_start,
                    col_end: tail.col_end,
                },
            });
            continue;
        }
        let token = &tokens[*cursor];
        *cursor += 1;
        if kind(token) == k[0] {
            let bytes = match &token.kind {
                TokenKind::String(lit) => lit.bytes.clone(),
                _ => token.to_literal_text().into_bytes(),
            };
            atoms.push(SignatureAtom::Literal(bytes, token.span));
        } else if kind(token) == k[1] {
            atoms.push(SignatureAtom::Literal(
                token.to_literal_text().into_bytes(),
                token.span,
            ));
        } else if matches!(token.kind, TokenKind::Comma) {
            return Err(error(
                "Commas must be quoted in statement signatures",
                token.span,
            ));
        } else if let TokenKind::Identifier(type_name) | TokenKind::Register(type_name) =
            &token.kind
        {
            if !grammar
                .types
                .iter()
                .any(|(_, name)| name.eq_ignore_ascii_case(type_name))
            {
                return Err(error(
                    format!("Unknown statement capture type: {type_name}"),
                    token.span,
                ));
            }
            let colon = tokens
                .get(*cursor)
                .ok_or_else(|| error("Expected ':' after capture type", end))?;
            if kind(colon) != k[6] {
                return Err(error("Expected ':' after capture type", colon.span));
            }
            *cursor += 1;
            let name = tokens
                .get(*cursor)
                .ok_or_else(|| error("Expected capture name after type", end))?;
            let (TokenKind::Identifier(text) | TokenKind::Register(text)) = &name.kind else {
                return Err(error("Expected capture name after type", name.span));
            };
            *cursor += 1;
            atoms.push(SignatureAtom::Capture {
                type_name: type_name.clone(),
                name: text.clone(),
                span: Span {
                    line: token.span.line,
                    col_start: token.span.col_start,
                    col_end: name.span.col_end,
                },
            });
        } else {
            return Err(error("Unexpected token in statement signature", token.span));
        }
    }
    if !closed {
        return Err(error("Missing closing }]", end));
    }
    Ok(StatementSignature::new(atoms))
}

pub(super) fn compile_ast(
    program: &[u8],
    tokens: &[Token],
    cursor: &mut usize,
    in_boundary: bool,
    end: Span,
) -> Result<StatementSignature, ParseError> {
    let grammar = Grammar::decode(program, end)?;
    let signature = parse(&grammar, tokens, cursor, in_boundary, end)?;
    let plan = lower_signature_with_types(&signature, &grammar.types);
    Ok(signature.with_prepared_plan(plan.0))
}

/// Entry 11 compiles signature tokens into the immutable numeric plan.
/// The source/template owner has already selected the definition keyword.
pub fn compile(program: &[u8], tokens: &[Token], end: Span) -> Result<Plan, ParseError> {
    let mut cursor = 0;
    let signature = compile_ast(program, tokens, &mut cursor, false, end)?;
    Ok(Plan(signature.prepared_plan().unwrap().clone()))
}

/// Adapter lowering uses the same package vocabulary as entry-11 compilation.
pub(super) fn capture_types(program: &[u8]) -> Vec<(u8, &str)> {
    Grammar::decode(
        program,
        Span {
            line: 0,
            col_start: 0,
            col_end: 0,
        },
    )
    .expect("generated statement program")
    .types
}

pub(super) fn render_types(program: &[u8], span: Span) -> Result<Vec<(u8, &str)>, ParseError> {
    Ok(Grammar::decode(program, span)?.renders)
}

pub fn validate_compile_program(program: &[u8], span: Span) -> Result<(), ParseError> {
    Grammar::decode(program, span).map(|_| ())
}
