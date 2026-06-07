// SPDX-License-Identifier: GPL-3.0-or-later

use opcore::parser::{BinaryOp, Expr, ParseError, UnaryOp};
use opcore::tokenizer::{OperatorKind, Span, Token, TokenKind};

type ExprSliceParser<'a> =
    dyn for<'tokens> FnMut(&'tokens [Token], Span, Option<String>) -> Result<Expr, ParseError> + 'a;

pub fn parse_runtime_operand_surface_expr(
    tokens: &[Token],
    mnemonic: Option<&str>,
    operand_index: usize,
    end_span: Span,
    end_token_text: Option<&str>,
    parse_expr: &mut ExprSliceParser<'_>,
    parse_wrapped_or_expr: &mut ExprSliceParser<'_>,
) -> Result<Option<Expr>, ParseError> {
    if let Some(expr) = parse_texture_operand(
        tokens,
        mnemonic,
        operand_index,
        end_span,
        end_token_text,
        parse_expr,
        parse_wrapped_or_expr,
    )? {
        return Ok(Some(expr));
    }
    if let Some(expr) =
        parse_postincrement_operand(tokens, end_span, end_token_text, parse_wrapped_or_expr)
    {
        return Ok(Some(expr));
    }
    if let Some(expr) =
        parse_predecrement_operand(tokens, end_span, end_token_text, parse_wrapped_or_expr)
    {
        return Ok(Some(expr));
    }
    if let Some(expr) = parse_postfix_tuple_operand(tokens, parse_expr, parse_wrapped_or_expr) {
        return Ok(Some(expr));
    }
    if let Some(expr) = parse_bitfield_suffix_operand(
        tokens,
        mnemonic,
        operand_index,
        end_span,
        end_token_text,
        parse_expr,
        parse_wrapped_or_expr,
    )? {
        return Ok(Some(expr));
    }
    parse_register_pair_operand(
        tokens,
        mnemonic,
        operand_index,
        end_span,
        end_token_text,
        parse_wrapped_or_expr,
    )
}

fn parse_postincrement_operand(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<&str>,
    parse_wrapped_or_expr: &mut ExprSliceParser<'_>,
) -> Option<Expr> {
    let plus = tokens.last()?;
    if !matches!(plus.kind, TokenKind::Operator(OperatorKind::Plus)) {
        return None;
    }
    let Expr::Indirect(inner, indirect_span) = parse_wrapped_or_expr(
        &tokens[..tokens.len().saturating_sub(1)],
        end_span,
        end_token_text.map(str::to_string),
    )
    .ok()?
    else {
        return None;
    };
    Some(Expr::Unary {
        op: UnaryOp::Plus,
        expr: Box::new(Expr::Indirect(inner, indirect_span)),
        span: Span {
            line: indirect_span.line,
            col_start: indirect_span.col_start,
            col_end: plus.span.col_end,
        },
    })
}

fn parse_predecrement_operand(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<&str>,
    parse_wrapped_or_expr: &mut ExprSliceParser<'_>,
) -> Option<Expr> {
    let minus = tokens.first()?;
    if !matches!(minus.kind, TokenKind::Operator(OperatorKind::Minus)) {
        return None;
    }
    let Expr::Indirect(inner, indirect_span) =
        parse_wrapped_or_expr(&tokens[1..], end_span, end_token_text.map(str::to_string)).ok()?
    else {
        return None;
    };
    Some(Expr::Unary {
        op: UnaryOp::Minus,
        expr: Box::new(Expr::Indirect(inner, indirect_span)),
        span: minus.span,
    })
}

fn parse_postfix_tuple_operand(
    tokens: &[Token],
    parse_expr: &mut ExprSliceParser<'_>,
    parse_wrapped_or_expr: &mut ExprSliceParser<'_>,
) -> Option<Expr> {
    if matches!(
        tokens.first().map(|token| &token.kind),
        Some(TokenKind::Dot)
    ) {
        return None;
    }
    let close = tokens.last()?;
    if !matches!(close.kind, TokenKind::CloseParen) {
        return None;
    }
    let open_index = find_adjacent_top_level_open_paren(tokens)?;
    if open_index == 0 || open_index + 1 >= tokens.len().saturating_sub(1) {
        return None;
    }
    if matches!(tokens[open_index - 1].kind, TokenKind::Colon) {
        return None;
    }
    if !top_level_group_closes_at_end(tokens, open_index) {
        return None;
    }

    let base = parse_inner_or_error(
        parse_expr,
        &tokens[..open_index],
        tokens[open_index].span,
        Some("(".to_string()),
    );
    let mut elements = vec![base];
    elements.extend(parse_tuple_elements(
        tokens,
        open_index + 1,
        tokens.len().saturating_sub(1),
        close.span,
        ")",
        parse_expr,
        parse_wrapped_or_expr,
    ));

    let start_span = opcore::expression::expr_span(&elements[0]);
    let span = Span {
        line: start_span.line,
        col_start: start_span.col_start,
        col_end: close.span.col_end,
    };
    Some(Expr::Indirect(Box::new(Expr::Tuple(elements, span)), span))
}

fn parse_texture_operand(
    tokens: &[Token],
    mnemonic: Option<&str>,
    operand_index: usize,
    end_span: Span,
    end_token_text: Option<&str>,
    parse_expr: &mut ExprSliceParser<'_>,
    parse_wrapped_or_expr: &mut ExprSliceParser<'_>,
) -> Result<Option<Expr>, ParseError> {
    if operand_index != 0 || !mnemonic.is_some_and(is_m68k_tex_mnemonic) {
        return Ok(None);
    }

    let Some(multiply_index) = find_top_level_multiply(tokens) else {
        return Ok(None);
    };
    if multiply_index == 0 || multiply_index + 1 >= tokens.len() {
        return Ok(None);
    }

    let left = parse_wrapped_or_expr(
        &tokens[..multiply_index],
        tokens[multiply_index].span,
        Some("*".to_string()),
    )?;
    let right = parse_expr(
        &tokens[multiply_index + 1..],
        end_span,
        end_token_text.map(str::to_string),
    )?;
    let left_span = opcore::expression::expr_span(&left);
    let right_span = opcore::expression::expr_span(&right);

    Ok(Some(Expr::Binary {
        op: BinaryOp::Multiply,
        left: Box::new(left),
        right: Box::new(right),
        span: Span {
            line: left_span.line,
            col_start: left_span.col_start,
            col_end: right_span.col_end,
        },
    }))
}

fn parse_register_pair_operand(
    tokens: &[Token],
    mnemonic: Option<&str>,
    operand_index: usize,
    end_span: Span,
    end_token_text: Option<&str>,
    parse_wrapped_or_expr: &mut ExprSliceParser<'_>,
) -> Result<Option<Expr>, ParseError> {
    let allow_pair = (mnemonic.is_some_and(is_m68k_cas2_mnemonic) && operand_index <= 2)
        || (mnemonic.is_some_and(is_m68k_long_divide_pair_mnemonic) && operand_index == 1);
    if !allow_pair {
        return Ok(None);
    }

    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;
    let mut colon_index = None;
    for (index, token) in tokens.iter().enumerate() {
        if depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
            && matches!(token.kind, TokenKind::Colon)
        {
            colon_index = Some(index);
            break;
        }
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
    }

    let Some(colon_index) = colon_index else {
        return Ok(None);
    };
    let left = parse_wrapped_or_expr(
        &tokens[..colon_index],
        tokens[colon_index].span,
        Some(":".to_string()),
    )?;
    let right = parse_wrapped_or_expr(
        &tokens[colon_index + 1..],
        end_span,
        end_token_text.map(str::to_string),
    )?;
    Ok(Some(build_call_expr(".pair", vec![left, right])))
}

fn parse_bitfield_suffix_operand(
    tokens: &[Token],
    mnemonic: Option<&str>,
    operand_index: usize,
    end_span: Span,
    end_token_text: Option<&str>,
    parse_expr: &mut ExprSliceParser<'_>,
    parse_wrapped_or_expr: &mut ExprSliceParser<'_>,
) -> Result<Option<Expr>, ParseError> {
    if !mnemonic.is_some_and(|name| is_m68k_bitfield_operand(name, operand_index)) {
        return Ok(None);
    }

    let Some(last) = tokens.last() else {
        return Ok(None);
    };
    if !matches!(last.kind, TokenKind::CloseBrace) {
        return Ok(None);
    }

    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;
    let mut open_brace_index = None;
    for (index, token) in tokens.iter().enumerate() {
        if depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
            && matches!(token.kind, TokenKind::OpenBrace)
        {
            open_brace_index = Some(index);
            break;
        }
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
    }

    let Some(open_brace_index) = open_brace_index else {
        return Ok(None);
    };
    if open_brace_index == 0 || open_brace_index + 1 >= tokens.len() {
        return Ok(None);
    }

    let mut inner_depth_paren = 0i32;
    let mut inner_depth_bracket = 0i32;
    let mut inner_depth_brace = 0i32;
    let mut colon_index = None;
    for (offset, token) in tokens[open_brace_index + 1..tokens.len() - 1]
        .iter()
        .enumerate()
    {
        if inner_depth_paren == 0
            && inner_depth_bracket == 0
            && inner_depth_brace == 0
            && matches!(token.kind, TokenKind::Colon)
        {
            colon_index = Some(open_brace_index + 1 + offset);
            break;
        }
        update_group_depths_for_token(
            &token.kind,
            &mut inner_depth_paren,
            &mut inner_depth_bracket,
            &mut inner_depth_brace,
        );
    }

    let Some(colon_index) = colon_index else {
        return Ok(Some(Expr::Error(
            "Expected ':' in bit-field selector".to_string(),
            last.span,
        )));
    };

    let base = parse_wrapped_or_expr(
        &tokens[..open_brace_index],
        tokens[open_brace_index].span,
        Some("{".to_string()),
    )?;
    let offset = parse_expr(
        &tokens[open_brace_index + 1..colon_index],
        tokens[colon_index].span,
        Some(":".to_string()),
    )?;
    let width = parse_expr(
        &tokens[colon_index + 1..tokens.len() - 1],
        end_span,
        end_token_text.map(str::to_string),
    )?;

    Ok(Some(build_call_expr(
        ".bitfield",
        vec![base, offset, width],
    )))
}

fn parse_tuple_elements(
    tokens: &[Token],
    start: usize,
    end: usize,
    close_span: Span,
    close_token_text: &str,
    parse_expr: &mut ExprSliceParser<'_>,
    parse_wrapped_or_expr: &mut ExprSliceParser<'_>,
) -> Vec<Expr> {
    split_top_level_comma_ranges(tokens, start, end)
        .into_iter()
        .map(|(start, end)| {
            let (element_end_span, element_end_token_text) = if let Some(comma) = tokens
                .get(end)
                .filter(|token| matches!(token.kind, TokenKind::Comma))
            {
                (comma.span, Some(",".to_string()))
            } else {
                (close_span, Some(close_token_text.to_string()))
            };
            if start == end {
                return Expr::Placeholder(element_end_span);
            }
            if let Ok(expr) = parse_wrapped_or_expr(
                &tokens[start..end],
                element_end_span,
                element_end_token_text.clone(),
            ) {
                return expr;
            }
            parse_inner_or_error(
                parse_expr,
                &tokens[start..end],
                element_end_span,
                element_end_token_text,
            )
        })
        .collect()
}

fn parse_inner_or_error(
    parse_inner: &mut ExprSliceParser<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Expr {
    match parse_inner(tokens, end_span, end_token_text) {
        Ok(expr) => expr,
        Err(err) => Expr::Error(err.message, err.span),
    }
}

fn build_call_expr(name: &str, args: Vec<Expr>) -> Expr {
    let start_span = opcore::expression::expr_span(args.first().expect("call requires args"));
    let end_span = opcore::expression::expr_span(args.last().expect("call requires args"));
    Expr::Call {
        name: name.to_string(),
        args,
        span: Span {
            line: start_span.line,
            col_start: start_span.col_start,
            col_end: end_span.col_end,
        },
    }
}

fn base_mnemonic_name(name: &str) -> &str {
    name.split('.').next().unwrap_or(name)
}

fn is_m68k_cas2_mnemonic(name: &str) -> bool {
    base_mnemonic_name(name).eq_ignore_ascii_case("CAS2")
}

fn is_m68k_long_divide_pair_mnemonic(name: &str) -> bool {
    matches!(
        base_mnemonic_name(name).to_ascii_uppercase().as_str(),
        "DIVS" | "DIVU" | "DIVSL" | "DIVUL"
    )
}

fn is_m68k_tex_mnemonic(name: &str) -> bool {
    matches!(
        base_mnemonic_name(name).to_ascii_uppercase().as_str(),
        "TEX8" | "TEX16" | "TEX24" | "TEX"
    )
}

fn is_m68k_bitfield_operand(name: &str, operand_index: usize) -> bool {
    match base_mnemonic_name(name).to_ascii_uppercase().as_str() {
        "BFINS" => operand_index == 1,
        "BFTST" | "BFEXTU" | "BFCHG" | "BFEXTS" | "BFCLR" | "BFFFO" | "BFSET" => operand_index == 0,
        _ => false,
    }
}

fn find_top_level_multiply(tokens: &[Token]) -> Option<usize> {
    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;
    for (index, token) in tokens.iter().enumerate() {
        if depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
            && matches!(token.kind, TokenKind::Operator(OperatorKind::Multiply))
        {
            return Some(index);
        }
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
    }
    None
}

fn find_adjacent_top_level_open_paren(tokens: &[Token]) -> Option<usize> {
    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;

    for (index, token) in tokens.iter().enumerate() {
        if depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
            && matches!(token.kind, TokenKind::OpenParen)
            && index > 0
            && token.span.col_start == tokens[index - 1].span.col_end
        {
            return Some(index);
        }
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
    }
    None
}

fn top_level_group_closes_at_end(tokens: &[Token], open_index: usize) -> bool {
    let mut depth = 0i32;
    for (index, token) in tokens.iter().enumerate().skip(open_index) {
        match token.kind {
            TokenKind::OpenParen => depth += 1,
            TokenKind::CloseParen => {
                depth -= 1;
                if depth == 0 {
                    return index == tokens.len().saturating_sub(1);
                }
                if depth < 0 {
                    return false;
                }
            }
            _ => {}
        }
    }
    false
}

fn update_group_depths_for_token(
    kind: &TokenKind,
    depth_paren: &mut i32,
    depth_bracket: &mut i32,
    depth_brace: &mut i32,
) {
    match kind {
        TokenKind::OpenParen => *depth_paren = depth_paren.saturating_add(1),
        TokenKind::CloseParen => *depth_paren = depth_paren.saturating_sub(1),
        TokenKind::OpenBracket => *depth_bracket = depth_bracket.saturating_add(1),
        TokenKind::CloseBracket => *depth_bracket = depth_bracket.saturating_sub(1),
        TokenKind::OpenBrace => *depth_brace = depth_brace.saturating_add(1),
        TokenKind::CloseBrace => *depth_brace = depth_brace.saturating_sub(1),
        _ => {}
    }
}

fn split_top_level_comma_ranges(tokens: &[Token], start: usize, end: usize) -> Vec<(usize, usize)> {
    let mut ranges = Vec::new();
    if start >= end {
        return ranges;
    }

    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;
    let mut current_start = start;

    for (cursor, token) in tokens.iter().enumerate().take(end).skip(start) {
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
        if matches!(token.kind, TokenKind::Comma)
            && depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
        {
            ranges.push((current_start, cursor));
            current_start = cursor.saturating_add(1);
        }
    }

    ranges.push((current_start, end));
    ranges
}

#[cfg(test)]
mod tests {
    use super::*;
    use opcore::tokenizer::NumberLiteral;

    fn span(col_start: usize, col_end: usize) -> Span {
        Span {
            line: 1,
            col_start,
            col_end,
        }
    }

    fn token(kind: TokenKind, col_start: usize, col_end: usize) -> Token {
        Token {
            kind,
            span: span(col_start, col_end),
        }
    }

    fn number(text: &str, col_start: usize, col_end: usize) -> Token {
        token(
            TokenKind::Number(NumberLiteral {
                text: text.to_string(),
                base: 10,
            }),
            col_start,
            col_end,
        )
    }

    fn parse_test_expr(
        tokens: &[Token],
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<Expr, ParseError> {
        match tokens {
            [Token {
                kind: TokenKind::Identifier(name),
                span,
            }] => Ok(Expr::Identifier(name.clone(), *span)),
            [Token {
                kind: TokenKind::Number(num),
                span,
            }] => Ok(Expr::Number(num.text.clone(), *span)),
            [Token {
                kind: TokenKind::Register(name),
                span,
            }] => Ok(Expr::Register(name.clone(), *span)),
            [] => Err(ParseError {
                message: match end_token_text {
                    Some(token) => format!("Expected label or numeric constant, found: {token}"),
                    None => "Unexpected end of expression".to_string(),
                },
                span: end_span,
            }),
            [token, ..] => Err(ParseError {
                message: "Unexpected token in expression".to_string(),
                span: token.span,
            }),
        }
    }

    fn parse_wrapped_or_expr(
        tokens: &[Token],
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<Expr, ParseError> {
        if let [Token {
            kind: TokenKind::OpenParen,
            span: open_span,
        }, middle @ .., Token {
            kind: TokenKind::CloseParen,
            span: close_span,
        }] = tokens
        {
            let inner = parse_test_expr(middle, close_span.to_owned(), Some(")".to_string()))?;
            return Ok(Expr::Indirect(
                Box::new(inner),
                Span {
                    line: open_span.line,
                    col_start: open_span.col_start,
                    col_end: close_span.col_end,
                },
            ));
        }
        parse_test_expr(tokens, end_span, end_token_text)
    }

    #[test]
    fn runtime_operand_surface_parses_postincrement_indirect() {
        let tokens = vec![
            token(TokenKind::OpenParen, 1, 2),
            token(TokenKind::Register("A0".to_string()), 2, 4),
            token(TokenKind::CloseParen, 4, 5),
            token(TokenKind::Operator(OperatorKind::Plus), 5, 6),
        ];

        let expr = parse_runtime_operand_surface_expr(
            &tokens,
            Some("MOVE"),
            0,
            span(99, 99),
            None,
            &mut parse_test_expr,
            &mut parse_wrapped_or_expr,
        )
        .expect("surface parse should succeed")
        .expect("expected family-owned postincrement parse");

        let Expr::Unary {
            op: UnaryOp::Plus,
            expr,
            span: wrapper_span,
        } = expr
        else {
            panic!("expected m68k postincrement operand");
        };

        assert_eq!(wrapper_span, span(1, 6));
        let Expr::Indirect(inner, indirect_span) = *expr else {
            panic!("expected indirect inside postincrement");
        };
        assert_eq!(indirect_span, span(1, 5));
        assert!(matches!(*inner, Expr::Register(ref name, _) if name == "A0"));
    }

    #[test]
    fn runtime_operand_surface_parses_predecrement_indirect() {
        let tokens = vec![
            token(TokenKind::Operator(OperatorKind::Minus), 1, 2),
            token(TokenKind::OpenParen, 2, 3),
            token(TokenKind::Register("A7".to_string()), 3, 5),
            token(TokenKind::CloseParen, 5, 6),
        ];

        let expr = parse_runtime_operand_surface_expr(
            &tokens,
            Some("MOVE"),
            0,
            span(99, 99),
            None,
            &mut parse_test_expr,
            &mut parse_wrapped_or_expr,
        )
        .expect("surface parse should succeed")
        .expect("expected family-owned predecrement parse");

        let Expr::Unary {
            op: UnaryOp::Minus,
            expr,
            span: wrapper_span,
        } = expr
        else {
            panic!("expected m68k predecrement operand");
        };

        assert_eq!(wrapper_span, span(1, 2));
        let Expr::Indirect(inner, indirect_span) = *expr else {
            panic!("expected indirect inside predecrement");
        };
        assert_eq!(indirect_span, span(2, 6));
        assert!(matches!(*inner, Expr::Register(ref name, _) if name == "A7"));
    }

    #[test]
    fn runtime_operand_surface_parses_postfix_tuple_indirect() {
        let tokens = vec![
            number("4", 1, 2),
            token(TokenKind::OpenParen, 2, 3),
            token(TokenKind::Register("A0".to_string()), 3, 5),
            token(TokenKind::Comma, 5, 6),
            token(TokenKind::Register("D1".to_string()), 6, 8),
            token(TokenKind::CloseParen, 8, 9),
        ];

        let expr = parse_runtime_operand_surface_expr(
            &tokens,
            Some("MOVE"),
            0,
            span(99, 99),
            None,
            &mut parse_test_expr,
            &mut parse_wrapped_or_expr,
        )
        .expect("surface parse should succeed")
        .expect("expected family-owned postfix tuple parse");

        let Expr::Indirect(inner, wrapper_span) = expr else {
            panic!("expected m68k postfix tuple indirect");
        };

        assert_eq!(wrapper_span, span(1, 9));
        let Expr::Tuple(elements, tuple_span) = *inner else {
            panic!("expected tuple inside postfix indirect");
        };
        assert_eq!(tuple_span, span(1, 9));
        assert_eq!(elements.len(), 3);
        assert!(matches!(elements[0], Expr::Number(ref text, _) if text == "4"));
        assert!(matches!(elements[1], Expr::Register(ref name, _) if name == "A0"));
        assert!(matches!(elements[2], Expr::Register(ref name, _) if name == "D1"));
    }

    #[test]
    fn runtime_operand_surface_preserves_syntactic_indirect_number_in_predecrement() {
        let tokens = vec![
            token(TokenKind::Operator(OperatorKind::Minus), 1, 2),
            token(TokenKind::OpenParen, 2, 3),
            number("1", 3, 4),
            token(TokenKind::CloseParen, 4, 5),
        ];

        let expr = parse_runtime_operand_surface_expr(
            &tokens,
            Some("MOVE"),
            0,
            span(99, 99),
            None,
            &mut parse_test_expr,
            &mut parse_wrapped_or_expr,
        )
        .expect("surface parse should succeed")
        .expect("expected family-owned predecrement parse");

        let Expr::Unary {
            op: UnaryOp::Minus,
            expr,
            span: wrapper_span,
        } = expr
        else {
            panic!("expected m68k predecrement operand");
        };

        assert_eq!(wrapper_span, span(1, 2));
        let Expr::Indirect(inner, indirect_span) = *expr else {
            panic!("expected indirect inside predecrement");
        };
        assert_eq!(indirect_span, span(2, 5));
        assert!(matches!(*inner, Expr::Number(ref text, _) if text == "1"));
    }
}
