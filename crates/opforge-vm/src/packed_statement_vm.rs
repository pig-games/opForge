// SPDX-License-Identifier: GPL-3.0-or-later
//! Shared PRVM statement compilation and core-AST adapters.
use opcore::parser::{ParseError, SignatureAtom, StatementSignature};
use opcore::tokenizer::{Span, Token, TokenKind, Tokenizer};
use package::statement_program;
use types::packed_statement as wire;

mod compiler;
mod head;
mod matcher;
pub use compiler::{compile, validate_compile_program};
pub use head::{classify_directive, select_head, validate_head_program};
pub use matcher::{
    execute_match, prepare_tokens, select_and_match_statement_signature_texts, select_plans,
    validate_match_program,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Plan(pub std::sync::Arc<[u8]>);

/// Numeric entry-11 bridge. Facets have already been normalized by the owning
/// tokenizer/capture preparation; this conversion never parses source text.
pub fn compile_views(
    program: &[u8],
    views: &[wire::TokenView],
    arena: &[u8],
    end: Span,
) -> Result<Plan, ParseError> {
    let renders = compiler::render_types(program, end)?;
    let mut tokens = Vec::with_capacity(views.len());
    for view in views {
        let span = Span {
            line: end.line,
            col_start: view.col_start as usize,
            col_end: view.col_end as usize,
        };
        validate_view(view, arena, span)?;
        let start = view.literal_offset as usize;
        let stop = start
            .checked_add(view.literal_bytes as usize)
            .ok_or_else(|| error("Invalid statement token view", span))?;
        let text = std::str::from_utf8(
            arena
                .get(start..stop)
                .ok_or_else(|| error("Invalid statement token view", span))?,
        )
        .map_err(|_| error("Invalid statement token view", span))?;
        if let Some((_, canonical)) = renders.iter().find(|(kind, _)| *kind == view.kind) {
            if text != *canonical {
                return Err(error("Invalid statement canonical token facet", span));
            }
        }
        let kind = match view.kind {
            0 => TokenKind::Identifier(text.into()),
            1 => TokenKind::Register(text.into()),
            2 => TokenKind::Number(opcore::tokenizer::NumberLiteral {
                text: text.into(),
                base: 10,
            }),
            3 => TokenKind::String(opcore::tokenizer::StringLiteral {
                raw: text.into(),
                bytes: text.as_bytes().to_vec(),
            }),
            4 => TokenKind::Comma,
            5 => TokenKind::Colon,
            6 => TokenKind::Dollar,
            7 => TokenKind::Dot,
            8 => TokenKind::Hash,
            9 => TokenKind::Question,
            10 => TokenKind::OpenBracket,
            11 => TokenKind::CloseBracket,
            12 => TokenKind::OpenBrace,
            13 => TokenKind::CloseBrace,
            14 => TokenKind::OpenParen,
            15 => TokenKind::CloseParen,
            16..=39 => {
                use opcore::tokenizer::OperatorKind::*;
                const OPERATORS: [opcore::tokenizer::OperatorKind; 24] = [
                    Range,
                    RangeInclusive,
                    Plus,
                    Minus,
                    Multiply,
                    Power,
                    Divide,
                    Mod,
                    Shl,
                    Shr,
                    BitNot,
                    LogicNot,
                    BitAnd,
                    BitOr,
                    BitXor,
                    LogicAnd,
                    LogicOr,
                    LogicXor,
                    Eq,
                    Ne,
                    Ge,
                    Gt,
                    Le,
                    Lt,
                ];
                TokenKind::Operator(OPERATORS[usize::from(view.kind - 16)])
            }
            40 => TokenKind::At,
            _ => TokenKind::End,
        };
        tokens.push(Token { kind, span });
    }
    compile(program, &tokens, end)
}

pub(crate) fn validate_view(view: &wire::TokenView, arena: &[u8], span: Span) -> Result<(), ParseError> {
    let invalid = || error("Invalid statement token view", span);
    let end = (view.literal_offset as usize)
        .checked_add(view.literal_bytes as usize)
        .ok_or_else(invalid)?;
    let text = arena
        .get(view.literal_offset as usize..end)
        .ok_or_else(invalid)?;
    if std::str::from_utf8(text).is_err()
        || view.kind > 40
        || view.col_start > view.col_end
        || view.numeric_status > 1
        || (view.kind != 2 && view.numeric_status != 0)
        || (view.numeric_status == 0 && view.numeric_value != 0)
        || (view.kind <= 2 && view.literal_bytes == 0)
    {
        return Err(invalid());
    }
    if view.kind == 3 {
        if view.decoded_bytes > view.literal_bytes
            || u64::from(view.literal_bytes) > u64::from(view.decoded_bytes) * 3
        {
            return Err(invalid());
        }
    } else if view.decoded_bytes != 0 {
        return Err(invalid());
    }
    Ok(())
}

fn error(message: impl Into<String>, span: Span) -> ParseError {
    ParseError {
        message: message.into(),
        span,
    }
}
fn read_u32(bytes: &[u8], offset: usize) -> Option<u32> {
    Some(u32::from_be_bytes(
        bytes.get(offset..offset.checked_add(4)?)?.try_into().ok()?,
    ))
}
fn put_u32(bytes: &mut [u8], offset: usize, value: u32) {
    bytes[offset..offset + 4].copy_from_slice(&value.to_be_bytes());
}

/// Lower an existing hook/export AST through the same immutable wire contract.
/// This adapter does not parse source or choose capture/matching policy.
pub fn lower_signature(signature: &StatementSignature) -> Plan {
    let program = statement_program::compile_program();
    let types = compiler::capture_types(&program);
    lower_signature_with_types(signature, &types)
}
fn lower_signature_with_types(signature: &StatementSignature, types: &[(u8, &str)]) -> Plan {
    struct Row {
        kind: u8,
        ty: u8,
        payload: Vec<u8>,
        pair: u32,
        span: Span,
    }
    fn rows(atoms: &[SignatureAtom], out: &mut Vec<Row>, types: &[(u8, &str)]) {
        for atom in atoms {
            match atom {
                SignatureAtom::Literal(bytes, span) => out.push(Row {
                    kind: wire::LITERAL,
                    ty: 0,
                    payload: String::from_utf8_lossy(bytes).into_owned().into_bytes(),
                    pair: wire::NONE,
                    span: *span,
                }),
                SignatureAtom::Capture {
                    type_name,
                    name,
                    span,
                } => out.push(Row {
                    kind: wire::CAPTURE,
                    ty: types
                        .iter()
                        .find(|(_, name)| name.eq_ignore_ascii_case(type_name))
                        .map_or(0, |(id, _)| *id),
                    payload: name.as_bytes().to_vec(),
                    pair: wire::NONE,
                    span: *span,
                }),
                SignatureAtom::Boundary { atoms, span } => {
                    let open = out.len();
                    out.push(Row {
                        kind: wire::BOUNDARY_OPEN,
                        ty: 0,
                        payload: Vec::new(),
                        pair: wire::NONE,
                        span: *span,
                    });
                    rows(atoms, out, types);
                    let close = out.len();
                    out[open].pair = close as u32;
                    out.push(Row {
                        kind: wire::BOUNDARY_CLOSE,
                        ty: 0,
                        payload: Vec::new(),
                        pair: open as u32,
                        span: *span,
                    });
                }
            }
        }
    }
    let mut entries = Vec::new();
    rows(signature.atoms(), &mut entries, types);
    let mut bytes = vec![0; wire::HEADER_BYTES + entries.len() * wire::ROW_BYTES];
    put_u32(&mut bytes, wire::ROW_COUNT, entries.len() as u32);
    put_u32(
        &mut bytes,
        wire::LITERAL_SCORE,
        signature
            .atoms()
            .iter()
            .filter(|a| matches!(a, SignatureAtom::Literal(..)))
            .count() as u32,
    );
    put_u32(&mut bytes, wire::ATOM_SCORE, signature.atoms().len() as u32);
    for (index, row) in entries.into_iter().enumerate() {
        let base = wire::HEADER_BYTES + index * wire::ROW_BYTES;
        bytes[base + wire::KIND] = row.kind;
        bytes[base + wire::CAPTURE_TYPE] = row.ty;
        let payload = bytes.len() as u32;
        put_u32(&mut bytes, base + wire::PAYLOAD_OFFSET, payload);
        put_u32(
            &mut bytes,
            base + wire::PAYLOAD_BYTES,
            row.payload.len() as u32,
        );
        put_u32(&mut bytes, base + wire::PAIRED_ROW, row.pair);
        put_u32(
            &mut bytes,
            base + wire::COL_START,
            row.span.col_start as u32,
        );
        put_u32(&mut bytes, base + wire::COL_END, row.span.col_end as u32);
        bytes.extend(row.payload);
    }
    let extent = bytes.len() as u32;
    put_u32(&mut bytes, wire::EXTENT, extent);
    Plan(bytes.into())
}

pub fn parse_statement_signature_from_tokens(
    tokens: &[Token],
    cursor: &mut usize,
    in_boundary: bool,
    end: Span,
) -> Result<StatementSignature, ParseError> {
    compiler::compile_ast(
        &statement_program::compile_program(),
        tokens,
        cursor,
        in_boundary,
        end,
    )
}

pub fn parse_statement_definition_from_line(
    line: &str,
    line_num: u32,
) -> Result<types::line_ast::StatementDefAst<StatementSignature, Span>, ParseError> {
    let mut tokenizer = Tokenizer::new(line, line_num);
    let mut tokens = Vec::new();
    let end = loop {
        let token = tokenizer
            .next_token()
            .map_err(|err| error(err.message, err.span))?;
        if matches!(token.kind, TokenKind::End) {
            break token.span;
        }
        tokens.push(token);
    };
    let Some(Token {
        kind: TokenKind::Dot,
        span: start,
    }) = tokens.first()
    else {
        return Err(error(
            "Expected .statement definition",
            tokens.first().map_or(end, |t| t.span),
        ));
    };
    let ident = |i: usize, message: &str| -> Result<String, ParseError> {
        match tokens.get(i) {
            Some(Token {
                kind: TokenKind::Identifier(s) | TokenKind::Register(s),
                ..
            }) => Ok(s.clone()),
            other => Err(error(message, other.map_or(end, |t| t.span))),
        }
    };
    let identities: Vec<_> = tokens
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
    let selected = select_head(
        &statement_program::head_program([1, 2]),
        wire::HEAD_COMPLETE,
        &views,
        &identities,
        end,
    )?;
    if selected.is_none_or(|head| head.action != 1) {
        return Err(error(
            "Expected .statement definition",
            tokens.get(1).map_or(end, |t| t.span),
        ));
    }
    let keyword = ident(2, "Expected statement keyword")?;
    let mut cursor = 3;
    let signature = parse_statement_signature_from_tokens(&tokens, &mut cursor, false, end)?;
    let tail = tokens.last().map_or(end, |t| t.span);
    Ok(types::line_ast::StatementDefAst {
        keyword,
        signature,
        span: Span {
            line: start.line,
            col_start: start.col_start,
            col_end: tail.col_end,
        },
    })
}

#[cfg(test)]
mod tests;
