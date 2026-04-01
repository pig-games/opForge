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

fn parse_m68k_statement_operand(
    parser: &mut Parser,
    mnemonic: Option<&str>,
    operand_index: usize,
) -> Result<Expr, ParseError> {
    let mut expr = parser.parse_expr()?;

    if ((mnemonic.is_some_and(is_m68k_cas2_mnemonic) && operand_index <= 2)
        || (mnemonic.is_some_and(is_m68k_long_divide_pair_mnemonic) && operand_index == 1))
        && parser.consume_kind(TokenKind::Colon)
    {
        let right = parser.parse_expr()?;
        expr = build_call_expr(".pair", vec![expr, right]);
    }

    if mnemonic.is_some_and(is_m68k_bitfield_mnemonic)
        && mnemonic.is_some_and(|name| is_m68k_bitfield_operand(name, operand_index))
        && parser.consume_kind(TokenKind::OpenBrace)
    {
        let offset = parser.parse_expr()?;
        if !parser.consume_kind(TokenKind::Colon) {
            return Err(ParseError {
                message: "Expected ':' in bit-field selector".to_string(),
                span: parser.current_span(),
            });
        }
        let width = parser.parse_expr()?;
        if !parser.consume_kind(TokenKind::CloseBrace) {
            return Err(ParseError {
                message: "Missing '}' in bit-field selector".to_string(),
                span: parser.current_span(),
            });
        }
        expr = build_call_expr(".bitfield", vec![expr, offset, width]);
    }

    Ok(expr)
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
                if parser.index < parser.tokens.len() {
                    match parser.parse_expr() {
                        Ok(expr) => operands.push(expr),
                        Err(err) => {
                            operands.push(Expr::Error(err.message, err.span));
                            return Ok(LineAst::Statement(StatementAst {
                                label,
                                mnemonic: Some(format!(".{name}")),
                                operands,
                            }));
                        }
                    }
                    while parser.consume_comma() {
                        match parser.parse_expr() {
                            Ok(expr) => operands.push(expr),
                            Err(err) => {
                                operands.push(Expr::Error(err.message, err.span));
                                return Ok(LineAst::Statement(StatementAst {
                                    label,
                                    mnemonic: Some(format!(".{name}")),
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
                    mnemonic: Some(format!(".{name}")),
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
            ..
        }) => Some(name),
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
