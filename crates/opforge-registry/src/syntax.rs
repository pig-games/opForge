// SPDX-License-Identifier: GPL-3.0-or-later

//! Assembler-oriented syntax adapters built on top of `opcore`.

pub use opcore::tokenizer::RegisterChecker;

use opcore::parser::{Expr, LineAst, ParseError, Parser};
use opcore::text_utils::is_ident_start;
use opcore::tokenizer::{OperatorKind, Span, Token, TokenKind, TokenizeError, Tokenizer};
use types::line_ast::{PackAst, PlaceAst};

pub fn register_checker_none() -> RegisterChecker {
    opcore::tokenizer::register_checker_none()
}

pub fn register_checker_from_fn(func: fn(&str) -> bool) -> RegisterChecker {
    opcore::tokenizer::register_checker_from_fn(func)
}

pub fn parser_from_line_with_registers(
    line: &str,
    line_num: u32,
    is_register: RegisterChecker,
) -> Result<Parser, ParseError> {
    let (tokens, end_span, end_token_text) =
        tokenize_statement_line_with_registers(line, line_num, is_register)?;
    Ok(Parser::from_tokens(tokens, end_span, end_token_text))
}

pub fn tokenize_statement_line_with_registers(
    line: &str,
    line_num: u32,
    is_register: RegisterChecker,
) -> Result<(Vec<Token>, Span, Option<String>), ParseError> {
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

    let mut tokenizer = Tokenizer::with_register_checker(line, line_num, is_register);
    let mut tokens = Vec::new();
    let mut end_token_text = None;
    let end_span = loop {
        let token = tokenizer.next_token().map_err(map_tokenize_error)?;
        if matches!(token.kind, TokenKind::End) {
            let idx = token.span.col_start.saturating_sub(1);
            if idx < line.len() && line.as_bytes().get(idx) == Some(&b';') {
                end_token_text = Some(";".to_string());
            }
            break token.span;
        }
        tokens.push(token);
    };
    Ok((tokens, end_span, end_token_text))
}

pub fn parse_place_directive_from_tokens_with<F>(
    tokens: &[Token],
    cursor: &mut usize,
    start_span: Span,
    end_span: Span,
    parse_align_expr: F,
) -> Result<LineAst, ParseError>
where
    F: FnOnce(&[Token]) -> Result<Expr, ParseError>,
{
    let (section, section_span) =
        parse_ident_like_at(tokens, cursor, "Expected section name for .place", end_span)?;
    let (in_kw, in_span) = parse_ident_like_at(
        tokens,
        cursor,
        "Expected 'in' in .place directive",
        end_span,
    )?;
    if !in_kw.eq_ignore_ascii_case("in") {
        return Err(ParseError {
            message: "Expected 'in' in .place directive".to_string(),
            span: in_span,
        });
    }
    let (region, _) =
        parse_ident_like_at(tokens, cursor, "Expected region name for .place", end_span)?;

    let mut align = None;
    if consume_kind_at(tokens, cursor, TokenKind::Comma) {
        let (key, key_span) = parse_ident_like_at(
            tokens,
            cursor,
            "Expected option key after ',' in .place directive",
            end_span,
        )?;
        if !key.eq_ignore_ascii_case("align") {
            return Err(ParseError {
                message: "Unknown .place option key".to_string(),
                span: key_span,
            });
        }
        if !match_operator_at(tokens, cursor, OperatorKind::Eq) {
            return Err(ParseError {
                message: "Expected '=' after align in .place directive".to_string(),
                span: current_span_at(tokens, *cursor, end_span),
            });
        }
        align = Some(parse_align_expr(&tokens[*cursor..])?);
        *cursor = tokens.len();
    }

    if *cursor < tokens.len() {
        return Err(ParseError {
            message: "Unexpected trailing tokens".to_string(),
            span: tokens[*cursor].span,
        });
    }

    let tail_span = if *cursor == 0 {
        section_span
    } else {
        prev_span_at(tokens, *cursor, end_span)
    };
    Ok(LineAst::Place(PlaceAst {
        section,
        region,
        align,
        span: Span {
            line: start_span.line,
            col_start: start_span.col_start,
            col_end: tail_span.col_end,
        },
    }))
}

pub fn parse_pack_directive_from_tokens(
    tokens: &[Token],
    cursor: &mut usize,
    start_span: Span,
    end_span: Span,
) -> Result<LineAst, ParseError> {
    let (in_kw, in_span) =
        parse_ident_like_at(tokens, cursor, "Expected 'in' in .pack directive", end_span)?;
    if !in_kw.eq_ignore_ascii_case("in") {
        return Err(ParseError {
            message: "Expected 'in' in .pack directive".to_string(),
            span: in_span,
        });
    }
    let (region, _) =
        parse_ident_like_at(tokens, cursor, "Expected region name for .pack", end_span)?;
    if !consume_kind_at(tokens, cursor, TokenKind::Colon) {
        return Err(ParseError {
            message: "Expected ':' in .pack directive".to_string(),
            span: current_span_at(tokens, *cursor, end_span),
        });
    }

    let mut sections = Vec::new();
    let (first_section, _) = parse_ident_like_at(
        tokens,
        cursor,
        "Expected at least one section in .pack directive",
        end_span,
    )?;
    sections.push(first_section);
    while consume_kind_at(tokens, cursor, TokenKind::Comma) {
        let (name, _) = parse_ident_like_at(
            tokens,
            cursor,
            "Expected section name after ',' in .pack directive",
            end_span,
        )?;
        sections.push(name);
    }

    if *cursor < tokens.len() {
        return Err(ParseError {
            message: "Unexpected trailing tokens".to_string(),
            span: tokens[*cursor].span,
        });
    }
    let tail_span = prev_span_at(tokens, *cursor, start_span);
    Ok(LineAst::Pack(PackAst {
        region,
        sections,
        span: Span {
            line: start_span.line,
            col_start: start_span.col_start,
            col_end: tail_span.col_end,
        },
    }))
}

fn parse_ident_like_at(
    tokens: &[Token],
    cursor: &mut usize,
    message: &str,
    end_span: Span,
) -> Result<(String, Span), ParseError> {
    match tokens.get(*cursor) {
        Some(Token {
            kind: TokenKind::Identifier(name),
            span,
        })
        | Some(Token {
            kind: TokenKind::Register(name),
            span,
        }) => {
            *cursor = cursor.saturating_add(1);
            Ok((name.clone(), *span))
        }
        Some(token) => Err(ParseError {
            message: message.to_string(),
            span: token.span,
        }),
        None => Err(ParseError {
            message: message.to_string(),
            span: end_span,
        }),
    }
}

fn consume_kind_at(tokens: &[Token], cursor: &mut usize, kind: TokenKind) -> bool {
    if tokens.get(*cursor).map(|token| &token.kind) == Some(&kind) {
        *cursor = cursor.saturating_add(1);
        true
    } else {
        false
    }
}

fn match_operator_at(tokens: &[Token], cursor: &mut usize, expected: OperatorKind) -> bool {
    match tokens.get(*cursor) {
        Some(Token {
            kind: TokenKind::Operator(op),
            ..
        }) if *op == expected => {
            *cursor = cursor.saturating_add(1);
            true
        }
        _ => false,
    }
}

fn current_span_at(tokens: &[Token], cursor: usize, end_span: Span) -> Span {
    tokens
        .get(cursor)
        .map(|token| token.span)
        .unwrap_or(end_span)
}

fn prev_span_at(tokens: &[Token], cursor: usize, fallback: Span) -> Span {
    cursor
        .checked_sub(1)
        .and_then(|idx| tokens.get(idx))
        .map(|token| token.span)
        .unwrap_or(fallback)
}

fn map_tokenize_error(err: TokenizeError) -> ParseError {
    ParseError {
        message: err.message,
        span: err.span,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        parse_pack_directive_from_tokens, parse_place_directive_from_tokens_with,
        register_checker_from_fn, tokenize_statement_line_with_registers,
    };
    use opcore::parser::{Expr, LineAst};
    use opcore::tokenizer::{NumberLiteral, OperatorKind, Span, Token, TokenKind};
    use types::line_ast::{PackAst, PlaceAst};

    fn span(col_start: usize, col_end: usize) -> Span {
        Span {
            line: 1,
            col_start,
            col_end,
        }
    }

    #[test]
    fn parse_place_directive_from_tokens_with_parses_align_option() {
        let tokens = vec![
            Token {
                kind: TokenKind::Identifier("code".to_string()),
                span: span(1, 4),
            },
            Token {
                kind: TokenKind::Identifier("in".to_string()),
                span: span(6, 7),
            },
            Token {
                kind: TokenKind::Identifier("ram".to_string()),
                span: span(9, 11),
            },
            Token {
                kind: TokenKind::Comma,
                span: span(12, 12),
            },
            Token {
                kind: TokenKind::Identifier("align".to_string()),
                span: span(14, 18),
            },
            Token {
                kind: TokenKind::Operator(OperatorKind::Eq),
                span: span(19, 19),
            },
            Token {
                kind: TokenKind::Number(NumberLiteral {
                    text: "16".to_string(),
                    base: 10,
                }),
                span: span(20, 21),
            },
        ];
        let mut cursor = 0;
        let line = parse_place_directive_from_tokens_with(
            &tokens,
            &mut cursor,
            span(1, 5),
            span(21, 21),
            |tail| match tail {
                [Token {
                    kind: TokenKind::Number(value),
                    span,
                }] => Ok(Expr::Number(value.text.clone(), *span)),
                _ => panic!("unexpected align expression tokens"),
            },
        )
        .expect("place directive should parse");

        match line {
            LineAst::Place(PlaceAst {
                section,
                region,
                align: Some(Expr::Number(value, _)),
                ..
            }) => {
                assert_eq!(section, "code");
                assert_eq!(region, "ram");
                assert_eq!(value, "16");
            }
            other => panic!("expected .place AST, got {other:?}"),
        }
    }

    #[test]
    fn parse_pack_directive_from_tokens_parses_section_list() {
        let tokens = vec![
            Token {
                kind: TokenKind::Identifier("in".to_string()),
                span: span(1, 2),
            },
            Token {
                kind: TokenKind::Identifier("rom".to_string()),
                span: span(4, 6),
            },
            Token {
                kind: TokenKind::Colon,
                span: span(7, 7),
            },
            Token {
                kind: TokenKind::Identifier("code".to_string()),
                span: span(9, 12),
            },
            Token {
                kind: TokenKind::Comma,
                span: span(13, 13),
            },
            Token {
                kind: TokenKind::Identifier("data".to_string()),
                span: span(14, 17),
            },
        ];
        let mut cursor = 0;
        let line = parse_pack_directive_from_tokens(&tokens, &mut cursor, span(1, 5), span(17, 17))
            .expect("pack directive should parse");

        match line {
            LineAst::Pack(PackAst {
                region, sections, ..
            }) => {
                assert_eq!(region, "rom");
                assert_eq!(sections, vec!["code".to_string(), "data".to_string()]);
            }
            other => panic!("expected .pack AST, got {other:?}"),
        }
    }

    #[test]
    fn tokenize_statement_line_with_registers_marks_register_operands() {
        let (tokens, end_span, end_token_text) = tokenize_statement_line_with_registers(
            "lda a",
            1,
            register_checker_from_fn(|name| name.eq_ignore_ascii_case("a")),
        )
        .expect("statement tokenization should succeed");

        assert!(end_token_text.is_none());
        assert_eq!(end_span.line, 1);
        assert!(matches!(
            tokens.first().map(|t| &t.kind),
            Some(TokenKind::Identifier(_))
        ));
        assert!(matches!(
            tokens.get(1).map(|t| &t.kind),
            Some(TokenKind::Register(_))
        ));
    }
}
