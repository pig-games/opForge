// SPDX-License-Identifier: GPL-3.0-or-later

use super::*;

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

fn is_m68k_bitfield_mnemonic(name: &str) -> bool {
    matches!(
        base_mnemonic_name(name).to_ascii_uppercase().as_str(),
        "BFTST" | "BFEXTU" | "BFCHG" | "BFEXTS" | "BFCLR" | "BFFFO" | "BFSET" | "BFINS"
    )
}

fn is_m68k_bitfield_operand(name: &str, operand_index: usize) -> bool {
    match base_mnemonic_name(name).to_ascii_uppercase().as_str() {
        "BFINS" => operand_index == 1,
        "BFTST" | "BFEXTU" | "BFCHG" | "BFEXTS" | "BFCLR" | "BFFFO" | "BFSET" => operand_index == 0,
        _ => false,
    }
}

fn build_call_expr(name: &str, args: Vec<Expr>) -> Expr {
    let start = span_of_expr(
        args.first()
            .expect("call expressions require at least one arg"),
    );
    let end = span_of_expr(
        args.last()
            .expect("call expressions require at least one arg"),
    );
    Expr::Call {
        name: name.to_string(),
        args,
        span: Span {
            line: start.line,
            col_start: start.col_start,
            col_end: end.col_end,
        },
    }
}

fn parse_expression_slice(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Expr, ParseError> {
    Parser::parse_expr_from_tokens(tokens.to_vec(), end_span, end_token_text)
}

fn parse_expression_slice_or_error(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Expr {
    match parse_expression_slice(tokens, end_span, end_token_text) {
        Ok(expr) => expr,
        Err(err) => Expr::Error(err.message, err.span),
    }
}

fn token_depths_before(tokens: &[Token], index: usize) -> (usize, usize, usize) {
    let mut paren = 0usize;
    let mut bracket = 0usize;
    let mut brace = 0usize;
    for token in &tokens[..index] {
        match token.kind {
            TokenKind::OpenParen => paren = paren.saturating_add(1),
            TokenKind::CloseParen => paren = paren.saturating_sub(1),
            TokenKind::OpenBracket => bracket = bracket.saturating_add(1),
            TokenKind::CloseBracket => bracket = bracket.saturating_sub(1),
            TokenKind::OpenBrace => brace = brace.saturating_add(1),
            TokenKind::CloseBrace => brace = brace.saturating_sub(1),
            _ => {}
        }
    }
    (paren, bracket, brace)
}

fn is_top_level_at(tokens: &[Token], index: usize) -> bool {
    token_depths_before(tokens, index) == (0, 0, 0)
}

fn find_top_level_token(tokens: &[Token], kind: TokenKind) -> Option<usize> {
    tokens
        .iter()
        .enumerate()
        .find(|(index, token)| token.kind == kind && is_top_level_at(tokens, *index))
        .map(|(index, _)| index)
}

fn contains_top_level_comma(tokens: &[Token]) -> bool {
    find_top_level_token(tokens, TokenKind::Comma).is_some()
}

fn is_single_wrapped_operand(tokens: &[Token], open: TokenKind, close: TokenKind) -> bool {
    if tokens.len() < 2
        || tokens.first().map(|token| &token.kind) != Some(&open)
        || tokens.last().map(|token| &token.kind) != Some(&close)
    {
        return false;
    }

    let mut depth = 0usize;
    for (index, token) in tokens.iter().enumerate() {
        if token.kind == open {
            depth = depth.saturating_add(1);
        } else if token.kind == close {
            depth = depth.saturating_sub(1);
            if depth == 0 && index != tokens.len() - 1 {
                return false;
            }
        }
    }
    depth == 0
}

fn tuple_elements_from_tokens(
    tokens: &[Token],
    close_span: Span,
    close_token_text: &str,
) -> Vec<Expr> {
    let mut elements = Vec::new();
    let mut start = 0usize;
    for index in 0..=tokens.len() {
        let is_boundary = index == tokens.len()
            || (tokens[index].kind == TokenKind::Comma && is_top_level_at(tokens, index));
        if !is_boundary {
            continue;
        }

        if start == index {
            let span = tokens
                .get(index)
                .map(|token| token.span)
                .unwrap_or(close_span);
            elements.push(Expr::Placeholder(span));
        } else {
            let end_span = tokens
                .get(index)
                .map(|token| token.span)
                .unwrap_or(close_span);
            let end_text = tokens
                .get(index)
                .and_then(|token| (token.kind == TokenKind::Comma).then(|| ",".to_string()));
            elements.push(parse_statement_operand_base(
                &tokens[start..index],
                end_span,
                end_text.or_else(|| Some(close_token_text.to_string())),
            ));
        }
        start = index.saturating_add(1);
    }
    elements
}

fn parse_generic_operand_wrapper(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Option<Expr> {
    let first = tokens.first()?;
    if matches!(first.kind, TokenKind::Hash) {
        let inner = parse_expression_slice_or_error(&tokens[1..], end_span, end_token_text);
        if matches!(inner, Expr::Error(_, _)) {
            return Some(inner);
        }
        return Some(Expr::Immediate(
            Box::new(inner),
            Span {
                line: first.span.line,
                col_start: first.span.col_start,
                col_end: tokens
                    .last()
                    .map(|token| token.span.col_end)
                    .unwrap_or(first.span.col_end),
            },
        ));
    }

    if is_single_wrapped_operand(tokens, TokenKind::OpenParen, TokenKind::CloseParen) {
        let close_span = tokens[tokens.len() - 1].span;
        let span = Span {
            line: first.span.line,
            col_start: first.span.col_start,
            col_end: close_span.col_end,
        };
        let inner_tokens = &tokens[1..tokens.len() - 1];
        if contains_top_level_comma(inner_tokens) {
            let elements = tuple_elements_from_tokens(inner_tokens, close_span, ")");
            return Some(Expr::Indirect(Box::new(Expr::Tuple(elements, span)), span));
        }
        let inner =
            parse_expression_slice_or_error(inner_tokens, close_span, Some(")".to_string()));
        return Some(Expr::Indirect(Box::new(inner), span));
    }

    if is_single_wrapped_operand(tokens, TokenKind::OpenBracket, TokenKind::CloseBracket) {
        let close_span = tokens[tokens.len() - 1].span;
        let span = Span {
            line: first.span.line,
            col_start: first.span.col_start,
            col_end: close_span.col_end,
        };
        let inner_tokens = &tokens[1..tokens.len() - 1];
        if contains_top_level_comma(inner_tokens) {
            let elements = tuple_elements_from_tokens(inner_tokens, close_span, "]");
            return Some(Expr::IndirectLong(
                Box::new(Expr::Tuple(elements, span)),
                span,
            ));
        }
        let inner =
            parse_expression_slice_or_error(inner_tokens, close_span, Some("]".to_string()));
        return Some(Expr::IndirectLong(Box::new(inner), span));
    }

    None
}

fn top_level_group_closes_at_end(tokens: &[Token], open_index: usize) -> bool {
    let mut depth = 0usize;
    for (index, token) in tokens.iter().enumerate().skip(open_index) {
        match token.kind {
            TokenKind::OpenParen => depth = depth.saturating_add(1),
            TokenKind::CloseParen => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return index == tokens.len() - 1;
                }
            }
            _ => {}
        }
    }
    false
}

fn adjacent_top_level_open_paren_index(tokens: &[Token]) -> Option<usize> {
    tokens.iter().enumerate().find_map(|(index, token)| {
        if index == 0 || token.kind != TokenKind::OpenParen || !is_top_level_at(tokens, index) {
            return None;
        }
        let prev = &tokens[index - 1];
        (token.span.col_start == prev.span.col_end).then_some(index)
    })
}

fn parse_m68k_postfix_tuple_operand(tokens: &[Token]) -> Option<Expr> {
    let open_index = adjacent_top_level_open_paren_index(tokens)?;
    if matches!(tokens[open_index - 1].kind, TokenKind::Colon)
        || !top_level_group_closes_at_end(tokens, open_index)
    {
        return None;
    }

    let prefix_end_span = tokens[open_index].span;
    let prefix = parse_expression_slice(
        &tokens[..open_index],
        prefix_end_span,
        Some("(".to_string()),
    )
    .ok()?;
    let close_span = tokens[tokens.len() - 1].span;
    let mut elements = vec![prefix];
    elements.extend(tuple_elements_from_tokens(
        &tokens[open_index + 1..tokens.len() - 1],
        close_span,
        ")",
    ));
    let span = Span {
        line: tokens[0].span.line,
        col_start: tokens[0].span.col_start,
        col_end: close_span.col_end,
    };
    Some(Expr::Indirect(Box::new(Expr::Tuple(elements, span)), span))
}

fn parse_m68k_postincrement_operand(tokens: &[Token]) -> Option<Expr> {
    let plus = tokens.last()?;
    if plus.kind != TokenKind::Operator(OperatorKind::Plus) {
        return None;
    }
    let inner = parse_generic_operand_wrapper(
        &tokens[..tokens.len() - 1],
        plus.span,
        Some("+".to_string()),
    )?;
    if !matches!(inner, Expr::Indirect(_, _)) {
        return None;
    }
    let inner_span = span_of_expr(&inner);
    Some(Expr::Unary {
        op: UnaryOp::Plus,
        expr: Box::new(inner),
        span: Span {
            line: inner_span.line,
            col_start: inner_span.col_start,
            col_end: plus.span.col_end,
        },
    })
}

fn parse_m68k_predecrement_operand(tokens: &[Token]) -> Option<Expr> {
    let minus = tokens.first()?;
    if minus.kind != TokenKind::Operator(OperatorKind::Minus) || tokens.len() < 3 {
        return None;
    }
    let inner = parse_generic_operand_wrapper(&tokens[1..], minus.span, None)?;
    if !matches!(inner, Expr::Indirect(_, _)) {
        return None;
    }
    Some(Expr::Unary {
        op: UnaryOp::Minus,
        expr: Box::new(inner),
        span: minus.span,
    })
}

fn parse_statement_operand_base(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Expr {
    if tokens.is_empty() {
        return Expr::Error("Expected expression".to_string(), end_span);
    }
    if let Some(expr) = parse_generic_operand_wrapper(tokens, end_span, end_token_text.clone()) {
        return expr;
    }
    if let Some(expr) = parse_m68k_postincrement_operand(tokens) {
        return expr;
    }
    if let Some(expr) = parse_m68k_predecrement_operand(tokens) {
        return expr;
    }
    if let Some(expr) = parse_m68k_postfix_tuple_operand(tokens) {
        return expr;
    }
    parse_expression_slice_or_error(tokens, end_span, end_token_text)
}

fn matching_top_level_brace_suffix(tokens: &[Token]) -> Option<usize> {
    let open_index = find_top_level_token(tokens, TokenKind::OpenBrace)?;
    if !matches!(
        tokens.last(),
        Some(Token {
            kind: TokenKind::CloseBrace,
            ..
        })
    ) {
        return None;
    }
    let mut depth = 0usize;
    for (index, token) in tokens.iter().enumerate().skip(open_index) {
        match token.kind {
            TokenKind::OpenBrace => depth = depth.saturating_add(1),
            TokenKind::CloseBrace => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return (index == tokens.len() - 1).then_some(open_index);
                }
            }
            _ => {}
        }
    }
    None
}

fn parse_m68k_statement_operand_tokens(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    mnemonic: Option<&str>,
    operand_index: usize,
) -> Expr {
    if (mnemonic.is_some_and(is_m68k_cas2_mnemonic) && operand_index <= 2)
        || (mnemonic.is_some_and(is_m68k_long_divide_pair_mnemonic) && operand_index == 1)
    {
        if let Some(colon_index) = find_top_level_token(tokens, TokenKind::Colon) {
            let colon_span = tokens[colon_index].span;
            let left = parse_statement_operand_base(
                &tokens[..colon_index],
                colon_span,
                Some(":".to_string()),
            );
            let right =
                parse_statement_operand_base(&tokens[colon_index + 1..], end_span, end_token_text);
            return build_call_expr(".pair", vec![left, right]);
        }
    }

    if mnemonic.is_some_and(is_m68k_bitfield_mnemonic)
        && mnemonic.is_some_and(|name| is_m68k_bitfield_operand(name, operand_index))
    {
        if let Some(open_index) = matching_top_level_brace_suffix(tokens) {
            let close_span = tokens[tokens.len() - 1].span;
            if let Some(colon_index) =
                find_top_level_token(&tokens[open_index + 1..tokens.len() - 1], TokenKind::Colon)
            {
                let colon_index = open_index + 1 + colon_index;
                let base = parse_statement_operand_base(
                    &tokens[..open_index],
                    tokens[open_index].span,
                    Some("{".to_string()),
                );
                let offset = parse_expression_slice_or_error(
                    &tokens[open_index + 1..colon_index],
                    tokens[colon_index].span,
                    Some(":".to_string()),
                );
                let width = parse_expression_slice_or_error(
                    &tokens[colon_index + 1..tokens.len() - 1],
                    close_span,
                    Some("}".to_string()),
                );
                return build_call_expr(".bitfield", vec![base, offset, width]);
            }
            return Expr::Error("Expected ':' in bit-field selector".to_string(), close_span);
        }
    }

    parse_statement_operand_base(tokens, end_span, end_token_text)
}

fn take_statement_operand_tokens(parser: &mut Parser) -> (Vec<Token>, Span, Option<String>) {
    let start = parser.index;
    let mut end = parser.tokens.len();
    for index in start..parser.tokens.len() {
        if parser.tokens[index].kind == TokenKind::Comma
            && token_depths_before(&parser.tokens[start..=index], index - start) == (0, 0, 0)
        {
            end = index;
            break;
        }
    }
    let end_span = parser
        .tokens
        .get(end)
        .map(|token| token.span)
        .unwrap_or(parser.end_span);
    let end_token_text = parser
        .tokens
        .get(end)
        .and_then(|token| (token.kind == TokenKind::Comma).then(|| ",".to_string()));
    let tokens = parser.tokens[start..end].to_vec();
    parser.index = end;
    (tokens, end_span, end_token_text)
}

fn parse_m68k_statement_operand(
    parser: &mut Parser,
    mnemonic: Option<&str>,
    operand_index: usize,
) -> Result<Expr, ParseError> {
    let (tokens, end_span, end_token_text) = take_statement_operand_tokens(parser);
    Ok(parse_m68k_statement_operand_tokens(
        &tokens,
        end_span,
        end_token_text,
        mnemonic,
        operand_index,
    ))
}

pub(super) fn parse_compat_mixed_line(parser: &mut Parser) -> Result<LineAst, ParseError> {
    if parser.tokens.is_empty() {
        return Ok(LineAst::Empty);
    }

    let mut label = None;
    let mut idx = 0usize;
    if let Some(first) = parser.tokens.first() {
        let label_name = match &first.kind {
            TokenKind::Identifier(name) => Some(name.clone()),
            TokenKind::Register(name) => Some(name.clone()),
            _ => None,
        };
        if let Some(name) = label_name {
            if first.span.col_start == 1 {
                if let Some(colon) = parser.tokens.get(1) {
                    if matches!(colon.kind, TokenKind::Colon)
                        && colon.span.col_start == first.span.col_end
                    {
                        label = Some(Label {
                            name: name.clone(),
                            span: first.span,
                        });
                        idx = 2;
                    }
                    if label.is_none() {
                        label = Some(Label {
                            name,
                            span: first.span,
                        });
                        idx = 1;
                    }
                } else {
                    label = Some(Label {
                        name,
                        span: first.span,
                    });
                    idx = 1;
                }
            }
        }
    }

    parser.index = idx;
    if parser.index >= parser.tokens.len() {
        return Ok(LineAst::Statement(StatementAst {
            label,
            mnemonic: None,
            operands: Vec::new(),
        }));
    }

    if label.is_none() {
        if let Some(Token {
            kind: TokenKind::Operator(OperatorKind::Multiply),
            ..
        }) = parser.tokens.get(parser.index)
        {
            if matches!(
                parser.tokens.get(parser.index + 1),
                Some(Token {
                    kind: TokenKind::Operator(OperatorKind::Eq),
                    ..
                })
            ) {
                parser.index = parser.index.saturating_add(2);
                let expr = parser.parse_expr()?;
                if parser.index < parser.tokens.len() {
                    return Err(ParseError {
                        message: "Unexpected trailing tokens".to_string(),
                        span: parser.tokens[parser.index].span,
                    });
                }
                return Ok(LineAst::Statement(StatementAst {
                    label,
                    mnemonic: Some(".org".to_string()),
                    operands: vec![expr],
                }));
            }
        }
    }

    if let Some(label) = &label {
        if let Some((op, span, consumed)) = parser.match_assignment_op() {
            parser.index = parser.index.saturating_add(consumed);
            let expr = match parser.parse_expr() {
                Ok(expr) => expr,
                Err(err) => Expr::Error(err.message, err.span),
            };
            if parser.index < parser.tokens.len() {
                return Err(ParseError {
                    message: "Unexpected trailing tokens".to_string(),
                    span: parser.tokens[parser.index].span,
                });
            }
            return Ok(LineAst::Assignment(AssignmentAst {
                label: label.clone(),
                op,
                expr,
                span,
            }));
        }
    }

    if parser.consume_kind(TokenKind::Dot) {
        let (name, span) = match parser.next() {
            Some(Token {
                kind: TokenKind::Identifier(name),
                span,
            }) => (name, span),
            Some(Token {
                kind: TokenKind::Register(name),
                span,
            }) => (name, span),
            Some(token) => {
                return Err(ParseError {
                    message: "Expected conditional after '.'".to_string(),
                    span: token.span,
                });
            }
            None => {
                return Err(ParseError {
                    message: "Expected conditional after '.'".to_string(),
                    span: parser.end_span,
                });
            }
        };
        let upper = name.to_ascii_uppercase();
        if upper.as_str() == "STATEMENT" {
            let start_span = span;
            let keyword = match parser.next() {
                Some(Token {
                    kind: TokenKind::Identifier(name),
                    ..
                }) => name,
                Some(Token {
                    kind: TokenKind::Register(name),
                    ..
                }) => name,
                Some(token) => {
                    return Err(ParseError {
                        message: "Expected statement keyword".to_string(),
                        span: token.span,
                    });
                }
                None => {
                    return Err(ParseError {
                        message: "Expected statement keyword".to_string(),
                        span: parser.end_span,
                    });
                }
            };
            let signature = parse_statement_signature(parser, false)?;
            let end_span = if parser.index == 0 {
                parser.end_span
            } else {
                parser.prev_span()
            };
            let span = Span {
                line: start_span.line,
                col_start: start_span.col_start,
                col_end: end_span.col_end,
            };
            return Ok(LineAst::StatementDef(StatementDefAst {
                keyword,
                signature,
                span,
            }));
        }
        if upper.as_str() == "ENDSTATEMENT" {
            if parser.index < parser.tokens.len() {
                return Err(ParseError {
                    message: "Unexpected tokens after .endstatement".to_string(),
                    span: parser.tokens[parser.index].span,
                });
            }
            return Ok(LineAst::StatementEnd(StatementEndAst { span }));
        }
        if upper.as_str() == "USE" {
            return parser.parse_use_directive(span);
        }
        if upper.as_str() == "PLACE" {
            return parse_place_directive(parser, span);
        }
        if upper.as_str() == "PACK" {
            return parse_pack_directive(parser, span);
        }
        if matches!(upper.as_str(), "FOR" | "BFOR") {
            return parser.parse_for_like_directive(label, name);
        }
        if matches!(upper.as_str(), "WHILE" | "BWHILE") {
            return parser.parse_while_like_directive(label, name);
        }
        if matches!(
            upper.as_str(),
            "STRUCT" | "ENDSTRUCT" | "ENDFOR" | "ENDWHILE"
        ) {
            if parser.index < parser.tokens.len() {
                return Err(ParseError {
                    message: "Unexpected trailing tokens".to_string(),
                    span: parser.tokens[parser.index].span,
                });
            }
            return Ok(LineAst::Statement(StatementAst {
                label,
                mnemonic: Some(format!(".{name}")),
                operands: Vec::new(),
            }));
        }
        if matches!(
            upper.as_str(),
            "MACRO" | "SEGMENT" | "ENDMACRO" | "ENDSEGMENT" | "ENDM" | "ENDS"
        ) {
            parser.index = parser.tokens.len();
            return Ok(LineAst::Statement(StatementAst {
                label,
                mnemonic: Some(format!(".{name}")),
                operands: Vec::new(),
            }));
        }
        let (kind, needs_expr, list_exprs) = match upper.as_str() {
            "IF" => (ConditionalKind::If, true, false),
            "ELSEIF" => (ConditionalKind::ElseIf, true, false),
            "ELSE" => (ConditionalKind::Else, false, false),
            "ENDIF" => (ConditionalKind::EndIf, false, false),
            "MATCH" => (ConditionalKind::Switch, true, false),
            "CASE" => (ConditionalKind::Case, true, true),
            "DEFAULT" => (ConditionalKind::Default, false, false),
            "ENDMATCH" => (ConditionalKind::EndSwitch, false, false),
            _ => {
                let mut operands = Vec::new();
                let mnemonic = Some(format!(".{name}"));
                if parser.index < parser.tokens.len() {
                    match parse_m68k_statement_operand(parser, mnemonic.as_deref(), operands.len())
                    {
                        Ok(expr) => operands.push(expr),
                        Err(err) => {
                            operands.push(Expr::Error(err.message, err.span));
                            return Ok(LineAst::Statement(StatementAst {
                                label,
                                mnemonic,
                                operands,
                            }));
                        }
                    }
                    while parser.consume_comma() {
                        match parse_m68k_statement_operand(
                            parser,
                            mnemonic.as_deref(),
                            operands.len(),
                        ) {
                            Ok(expr) => operands.push(expr),
                            Err(err) => {
                                operands.push(Expr::Error(err.message, err.span));
                                return Ok(LineAst::Statement(StatementAst {
                                    label,
                                    mnemonic,
                                    operands,
                                }));
                            }
                        }
                    }
                }
                if parser.index < parser.tokens.len() {
                    return Err(ParseError {
                        message: "Unexpected trailing tokens".to_string(),
                        span: parser.tokens[parser.index].span,
                    });
                }
                return Ok(LineAst::Statement(StatementAst {
                    label,
                    mnemonic,
                    operands,
                }));
            }
        };
        let mut exprs = Vec::new();
        if needs_expr {
            match parser.parse_expr() {
                Ok(expr) => exprs.push(expr),
                Err(err) => exprs.push(Expr::Error(err.message, err.span)),
            }
            if list_exprs {
                while parser.consume_comma() {
                    match parser.parse_expr() {
                        Ok(expr) => exprs.push(expr),
                        Err(err) => {
                            exprs.push(Expr::Error(err.message, err.span));
                            break;
                        }
                    }
                }
            }
        }
        if parser.index < parser.tokens.len() {
            return Err(ParseError {
                message: "Unexpected tokens after conditional".to_string(),
                span: parser.tokens[parser.index].span,
            });
        }
        return Ok(LineAst::Conditional(ConditionalAst { kind, exprs, span }));
    }

    let mnemonic = match parser.next() {
        Some(Token {
            kind: TokenKind::Identifier(name),
            span,
        }) => {
            let mut name = name;
            if name.to_ascii_uppercase().ends_with(".S")
                && matches!(
                    parser.tokens.get(parser.index),
                    Some(Token {
                        kind: TokenKind::Operator(OperatorKind::Plus),
                        span: plus_span,
                    }) if plus_span.col_start == span.col_end
                )
            {
                parser.index += 1;
                name.push('+');
            }
            Some(name)
        }
        Some(token) => {
            return Err(ParseError {
                message: "Expected mnemonic identifier".to_string(),
                span: token.span,
            });
        }
        None => None,
    };

    let mut operands = Vec::new();
    if parser.index < parser.tokens.len() {
        if parser.consume_comma() {
            let comma_span = parser.prev_span();
            operands.push(Expr::Number("0".to_string(), comma_span));
            match parse_m68k_statement_operand(parser, mnemonic.as_deref(), operands.len()) {
                Ok(expr) => operands.push(expr),
                Err(err) => {
                    operands.push(Expr::Error(err.message, err.span));
                    return Ok(LineAst::Statement(StatementAst {
                        label,
                        mnemonic,
                        operands,
                    }));
                }
            }
        } else {
            match parse_m68k_statement_operand(parser, mnemonic.as_deref(), operands.len()) {
                Ok(expr) => operands.push(expr),
                Err(err) => {
                    operands.push(Expr::Error(err.message, err.span));
                    return Ok(LineAst::Statement(StatementAst {
                        label,
                        mnemonic,
                        operands,
                    }));
                }
            }
        }
        while parser.consume_comma() {
            match parse_m68k_statement_operand(parser, mnemonic.as_deref(), operands.len()) {
                Ok(expr) => operands.push(expr),
                Err(err) => {
                    operands.push(Expr::Error(err.message, err.span));
                    return Ok(LineAst::Statement(StatementAst {
                        label,
                        mnemonic,
                        operands,
                    }));
                }
            }
        }
    }

    if parser.index < parser.tokens.len() {
        return Err(ParseError {
            message: "Unexpected trailing tokens".to_string(),
            span: parser.tokens[parser.index].span,
        });
    }

    Ok(LineAst::Statement(StatementAst {
        label,
        mnemonic,
        operands,
    }))
}

fn parse_place_directive(parser: &mut Parser, start_span: Span) -> Result<LineAst, ParseError> {
    let (section, section_span) = parser.parse_ident_like("Expected section name for .place")?;
    let (in_kw, in_span) = parser.parse_ident_like("Expected 'in' in .place directive")?;
    if !in_kw.eq_ignore_ascii_case("in") {
        return Err(ParseError {
            message: "Expected 'in' in .place directive".to_string(),
            span: in_span,
        });
    }
    let (region, _) = parser.parse_ident_like("Expected region name for .place")?;

    let mut align = None;
    if parser.consume_comma() {
        let (key, key_span) =
            parser.parse_ident_like("Expected option key after ',' in .place directive")?;
        if !key.eq_ignore_ascii_case("align") {
            return Err(ParseError {
                message: "Unknown .place option key".to_string(),
                span: key_span,
            });
        }
        if !parser.match_operator(OperatorKind::Eq) {
            return Err(ParseError {
                message: "Expected '=' after align in .place directive".to_string(),
                span: parser.current_span(),
            });
        }
        align = Some(parser.parse_expr()?);
    }

    if parser.index < parser.tokens.len() {
        return Err(ParseError {
            message: "Unexpected trailing tokens".to_string(),
            span: parser.tokens[parser.index].span,
        });
    }

    let end_span = if parser.index == 0 {
        section_span
    } else {
        parser.prev_span()
    };
    Ok(LineAst::Place(PlaceAst {
        section,
        region,
        align,
        span: Span {
            line: start_span.line,
            col_start: start_span.col_start,
            col_end: end_span.col_end,
        },
    }))
}

fn parse_pack_directive(parser: &mut Parser, start_span: Span) -> Result<LineAst, ParseError> {
    let (in_kw, in_span) = parser.parse_ident_like("Expected 'in' in .pack directive")?;
    if !in_kw.eq_ignore_ascii_case("in") {
        return Err(ParseError {
            message: "Expected 'in' in .pack directive".to_string(),
            span: in_span,
        });
    }
    let (region, _) = parser.parse_ident_like("Expected region name for .pack")?;
    if !parser.consume_kind(TokenKind::Colon) {
        return Err(ParseError {
            message: "Expected ':' in .pack directive".to_string(),
            span: parser.current_span(),
        });
    }

    let mut sections = Vec::new();
    let (first_section, _) =
        parser.parse_ident_like("Expected at least one section in .pack directive")?;
    sections.push(first_section);
    while parser.consume_comma() {
        let (name, _) =
            parser.parse_ident_like("Expected section name after ',' in .pack directive")?;
        sections.push(name);
    }

    if parser.index < parser.tokens.len() {
        return Err(ParseError {
            message: "Unexpected trailing tokens".to_string(),
            span: parser.tokens[parser.index].span,
        });
    }

    let end_span = if parser.index == 0 {
        start_span
    } else {
        parser.prev_span()
    };
    Ok(LineAst::Pack(PackAst {
        region,
        sections,
        span: Span {
            line: start_span.line,
            col_start: start_span.col_start,
            col_end: end_span.col_end,
        },
    }))
}

fn parse_statement_signature(
    parser: &mut Parser,
    in_boundary: bool,
) -> Result<StatementSignature, ParseError> {
    let mut atoms = Vec::new();
    let mut closed = !in_boundary;
    while parser.index < parser.tokens.len() {
        if in_boundary
            && parser.peek_kind(TokenKind::CloseBrace)
            && peek_kind_next(parser, TokenKind::CloseBracket)
        {
            parser.index += 2;
            closed = true;
            break;
        }

        if in_boundary && parser.peek_kind(TokenKind::CloseBrace) {
            let token = expect_next(parser, || "Missing closing }]".to_string())?;
            return Err(ParseError {
                message: "Missing closing }]".to_string(),
                span: token.span,
            });
        }

        if parser.peek_kind(TokenKind::OpenBracket) && peek_kind_next(parser, TokenKind::OpenBrace)
        {
            let open_span = parser.tokens[parser.index].span;
            parser.index += 2;
            let inner = parse_statement_signature(parser, true)?;
            let close_span = parser.prev_span();
            let span = Span {
                line: open_span.line,
                col_start: open_span.col_start,
                col_end: close_span.col_end,
            };
            atoms.push(SignatureAtom::Boundary {
                atoms: inner.atoms,
                span,
            });
            continue;
        }

        let token = expect_next(parser, || {
            "Unexpected end of statement signature".to_string()
        })?;
        match token.kind {
            TokenKind::String(lit) => {
                atoms.push(SignatureAtom::Literal(lit.bytes, token.span));
            }
            TokenKind::Dot => {
                atoms.push(SignatureAtom::Literal(vec![b'.'], token.span));
            }
            TokenKind::Comma => {
                return Err(ParseError {
                    message: "Commas must be quoted in statement signatures".to_string(),
                    span: token.span,
                });
            }
            TokenKind::Identifier(type_name) | TokenKind::Register(type_name) => {
                if !parser_statement_signature::is_valid_capture_type(&type_name) {
                    return Err(ParseError {
                        message: format!("Unknown statement capture type: {type_name}"),
                        span: token.span,
                    });
                }
                let colon = expect_next(parser, || "Expected ':' after capture type".to_string())?;
                if !matches!(colon.kind, TokenKind::Colon) {
                    return Err(ParseError {
                        message: "Expected ':' after capture type".to_string(),
                        span: colon.span,
                    });
                }
                let next = expect_next(parser, || "Expected capture name after type".to_string())?;
                let name = match next.kind {
                    TokenKind::Identifier(name) | TokenKind::Register(name) => name,
                    _ => {
                        return Err(ParseError {
                            message: "Expected capture name after type".to_string(),
                            span: next.span,
                        });
                    }
                };
                let span = Span {
                    line: token.span.line,
                    col_start: token.span.col_start,
                    col_end: next.span.col_end,
                };
                atoms.push(SignatureAtom::Capture {
                    type_name,
                    name,
                    span,
                });
            }
            _ => {
                return Err(ParseError {
                    message: "Unexpected token in statement signature".to_string(),
                    span: token.span,
                });
            }
        }
    }
    if !closed {
        return Err(ParseError {
            message: "Missing closing }]".to_string(),
            span: parser.end_span,
        });
    }
    Ok(StatementSignature { atoms })
}

fn expect_next<F>(parser: &mut Parser, message: F) -> Result<Token, ParseError>
where
    F: FnOnce() -> String,
{
    parser.next().ok_or_else(|| ParseError {
        message: message(),
        span: parser.end_span,
    })
}

fn peek_kind_next(parser: &Parser, kind: TokenKind) -> bool {
    matches!(parser.tokens.get(parser.index + 1), Some(Token { kind: k, .. }) if *k == kind)
}
