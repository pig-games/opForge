use opcore::parser::{Expr, Label, LineAst, ParseError};
use opcore::tokenizer::{ConditionalKind, OperatorKind, Span, Token, TokenKind};
use registry::{
    parse_pack_directive_from_tokens, parse_place_directive_from_tokens_with,
    parse_statement_signature_from_tokens,
};
use types::line_ast::{
    ConditionalAst, StatementAst, StatementDefAst, StatementEndAst, UseAst, UseItemAst, UseParamAst,
};

use crate::vm_opasm::{
    parse_operand_expr_range, split_top_level_comma_ranges, update_group_depths_for_token,
    OperandExprBoundary, OperandExprParseHints,
};
use crate::vm_opasm_parse::VmExprParseContext;
use crate::vm_opcore::{parse_expr_with_vm_contract, parse_expr_with_vm_contract_and_boundary};

pub(crate) fn parse_dot_directive_line_from_tokens(
    tokens: &[Token],
    dot_index: usize,
    label: Option<Label>,
    end_span: Span,
    end_token_text: Option<String>,
    expr_parse_ctx: &VmExprParseContext<'_>,
) -> Result<LineAst, ParseError> {
    let mut cursor = dot_index.saturating_add(1);
    let (name, name_span) = parse_ident_like_at(
        tokens,
        &mut cursor,
        "Expected conditional after '.'",
        end_span,
    )?;
    let upper = name.to_ascii_uppercase();

    if upper.as_str() == "STATEMENT" {
        let keyword = match tokens.get(cursor) {
            Some(Token {
                kind: TokenKind::Identifier(keyword),
                ..
            }) => {
                cursor = cursor.saturating_add(1);
                keyword.clone()
            }
            Some(Token {
                kind: TokenKind::Register(keyword),
                ..
            }) => {
                cursor = cursor.saturating_add(1);
                keyword.clone()
            }
            Some(token) => {
                return Err(ParseError {
                    message: "Expected statement keyword".to_string(),
                    span: token.span,
                })
            }
            None => {
                return Err(ParseError {
                    message: "Expected statement keyword".to_string(),
                    span: end_span,
                })
            }
        };
        let signature =
            parse_statement_signature_from_tokens(tokens, &mut cursor, false, end_span)?;
        let tail_span = prev_span_at(tokens, cursor, end_span);
        return Ok(LineAst::StatementDef(StatementDefAst {
            keyword,
            signature,
            span: Span {
                line: name_span.line,
                col_start: name_span.col_start,
                col_end: tail_span.col_end,
            },
        }));
    }

    if upper.as_str() == "ENDSTATEMENT" {
        if cursor < tokens.len() {
            return Err(ParseError {
                message: "Unexpected tokens after .endstatement".to_string(),
                span: tokens[cursor].span,
            });
        }
        return Ok(LineAst::StatementEnd(StatementEndAst { span: name_span }));
    }

    if upper.as_str() == "USE" {
        return parse_use_directive_from_tokens(
            tokens,
            &mut cursor,
            name_span,
            end_span,
            end_token_text,
            expr_parse_ctx,
        );
    }
    if upper.as_str() == "PLACE" {
        return parse_place_directive_from_tokens_with(
            tokens,
            &mut cursor,
            name_span,
            end_span,
            |tail| parse_expr_with_vm_contract(expr_parse_ctx, tail, end_span, end_token_text),
        );
    }
    if upper.as_str() == "PACK" {
        return parse_pack_directive_from_tokens(tokens, &mut cursor, name_span, end_span);
    }
    if matches!(upper.as_str(), "FOR" | "BFOR") {
        return parse_for_like_directive_from_tokens(
            tokens,
            &mut cursor,
            label,
            name,
            end_span,
            end_token_text,
            expr_parse_ctx,
        );
    }
    if matches!(upper.as_str(), "WHILE" | "BWHILE") {
        return parse_while_like_directive_from_tokens(
            tokens,
            &mut cursor,
            label,
            name,
            end_span,
            end_token_text,
            expr_parse_ctx,
        );
    }
    if upper.as_str() == "STRUCT" {
        let mut operands: Vec<Expr> = Vec::new();
        if cursor < tokens.len() {
            for (start, end) in split_top_level_comma_ranges(tokens, cursor, tokens.len()) {
                parse_operand_expr_range(
                    tokens,
                    start,
                    end,
                    OperandExprBoundary {
                        end_span,
                        end_token_text: end_token_text.clone(),
                    },
                    OperandExprParseHints {
                        mnemonic: None,
                        operand_index: operands.len(),
                    },
                    expr_parse_ctx,
                    &mut operands,
                )?;
                if matches!(operands.last(), Some(Expr::Error(_, _))) {
                    break;
                }
            }
        }
        return Ok(LineAst::Statement(StatementAst {
            label,
            mnemonic: Some(format!(".{name}")),
            operands,
        }));
    }

    if matches!(upper.as_str(), "ENDSTRUCT" | "ENDFOR" | "ENDWHILE") {
        if cursor < tokens.len() {
            return Err(ParseError {
                message: "Unexpected trailing tokens".to_string(),
                span: tokens[cursor].span,
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
        return Ok(LineAst::Statement(StatementAst {
            label,
            mnemonic: Some(format!(".{name}")),
            operands: Vec::new(),
        }));
    }

    if let Some((kind, needs_expr, list_exprs)) = dot_conditional_kind(&upper) {
        let mut exprs: Vec<Expr> = Vec::new();
        if needs_expr {
            if list_exprs {
                for (start, end) in split_top_level_comma_ranges(tokens, cursor, tokens.len()) {
                    parse_operand_expr_range(
                        tokens,
                        start,
                        end,
                        OperandExprBoundary {
                            end_span,
                            end_token_text: end_token_text.clone(),
                        },
                        OperandExprParseHints {
                            mnemonic: None,
                            operand_index: exprs.len(),
                        },
                        expr_parse_ctx,
                        &mut exprs,
                    )?;
                    if matches!(exprs.last(), Some(Expr::Error(_, _))) {
                        break;
                    }
                }
            } else {
                let expr = match parse_expr_with_vm_contract(
                    expr_parse_ctx,
                    &tokens[cursor..],
                    end_span,
                    end_token_text,
                ) {
                    Ok(expr) => expr,
                    Err(err) => Expr::Error(err.message, err.span),
                };
                exprs.push(expr);
            }
        }
        return Ok(LineAst::Conditional(ConditionalAst {
            kind,
            exprs,
            span: name_span,
        }));
    }

    let mut operands: Vec<Expr> = Vec::new();
    if cursor < tokens.len() {
        for (start, end) in split_top_level_comma_ranges(tokens, cursor, tokens.len()) {
            parse_operand_expr_range(
                tokens,
                start,
                end,
                OperandExprBoundary {
                    end_span,
                    end_token_text: end_token_text.clone(),
                },
                OperandExprParseHints {
                    mnemonic: None,
                    operand_index: operands.len(),
                },
                expr_parse_ctx,
                &mut operands,
            )?;
            if matches!(operands.last(), Some(Expr::Error(_, _))) {
                break;
            }
        }
    }

    Ok(LineAst::Statement(StatementAst {
        label,
        mnemonic: Some(format!(".{name}")),
        operands,
    }))
}

fn parse_for_like_directive_from_tokens(
    tokens: &[Token],
    cursor: &mut usize,
    label: Option<Label>,
    name: String,
    end_span: Span,
    end_token_text: Option<String>,
    expr_parse_ctx: &VmExprParseContext<'_>,
) -> Result<LineAst, ParseError> {
    let mut operands = Vec::new();
    let mnemonic = Some(format!(".{name}"));
    let start_cursor = *cursor;

    if let Some(Token {
        kind: TokenKind::Identifier(var_name),
        span: var_span,
    })
    | Some(Token {
        kind: TokenKind::Register(var_name),
        span: var_span,
    }) = tokens.get(*cursor)
    {
        *cursor = cursor.saturating_add(1);
        if match_keyword_at(tokens, cursor, "in") {
            operands.push(Expr::Identifier(var_name.clone(), *var_span));
            match parse_expr_with_vm_contract(
                expr_parse_ctx,
                &tokens[*cursor..],
                end_span,
                end_token_text,
            ) {
                Ok(expr) => operands.push(expr),
                Err(err) => operands.push(Expr::Error(err.message, err.span)),
            }
            return Ok(LineAst::Statement(StatementAst {
                label,
                mnemonic,
                operands,
            }));
        }
    }

    *cursor = start_cursor;
    match parse_expr_with_vm_contract(expr_parse_ctx, &tokens[*cursor..], end_span, end_token_text)
    {
        Ok(expr) => operands.push(expr),
        Err(err) => operands.push(Expr::Error(err.message, err.span)),
    }
    Ok(LineAst::Statement(StatementAst {
        label,
        mnemonic,
        operands,
    }))
}

fn parse_while_like_directive_from_tokens(
    tokens: &[Token],
    cursor: &mut usize,
    label: Option<Label>,
    name: String,
    end_span: Span,
    end_token_text: Option<String>,
    expr_parse_ctx: &VmExprParseContext<'_>,
) -> Result<LineAst, ParseError> {
    let mut operands = Vec::new();
    let mnemonic = Some(format!(".{name}"));

    match parse_expr_with_vm_contract(expr_parse_ctx, &tokens[*cursor..], end_span, end_token_text)
    {
        Ok(expr) => operands.push(expr),
        Err(err) => operands.push(Expr::Error(err.message, err.span)),
    }
    Ok(LineAst::Statement(StatementAst {
        label,
        mnemonic,
        operands,
    }))
}

pub(super) fn parse_use_directive_from_tokens(
    tokens: &[Token],
    cursor: &mut usize,
    start_span: Span,
    end_span: Span,
    end_token_text: Option<String>,
    expr_parse_ctx: &VmExprParseContext<'_>,
) -> Result<LineAst, ParseError> {
    let (module_id, _) =
        parse_ident_like_at(tokens, cursor, "Expected module id after .use", end_span)?;
    let mut alias = None;
    let mut items = Vec::new();
    let mut params = Vec::new();

    if match_keyword_at(tokens, cursor, "as") {
        let (name, _) = parse_ident_like_at(
            tokens,
            cursor,
            "Expected alias identifier after 'as'",
            end_span,
        )?;
        alias = Some(name);
    }

    if consume_kind_at(tokens, cursor, TokenKind::OpenParen) {
        if consume_kind_at(tokens, cursor, TokenKind::CloseParen) {
            return Err(ParseError {
                message: "Selective import list cannot be empty".to_string(),
                span: prev_span_at(tokens, *cursor, end_span),
            });
        }
        if match_operator_at(tokens, cursor, OperatorKind::Multiply) {
            let star_span = prev_span_at(tokens, *cursor, end_span);
            if match_keyword_at(tokens, cursor, "as") {
                return Err(ParseError {
                    message: "Wildcard import cannot have an alias".to_string(),
                    span: current_span_at(tokens, *cursor, end_span),
                });
            }
            if !consume_kind_at(tokens, cursor, TokenKind::CloseParen) {
                return Err(ParseError {
                    message: "Wildcard import must be the only selective item".to_string(),
                    span: current_span_at(tokens, *cursor, end_span),
                });
            }
            items.push(UseItemAst {
                name: "*".to_string(),
                alias: None,
                span: star_span,
            });
        } else {
            loop {
                let (name, span) = parse_ident_like_at(
                    tokens,
                    cursor,
                    "Expected identifier in selective import list",
                    end_span,
                )?;
                let mut item_alias = None;
                if match_keyword_at(tokens, cursor, "as") {
                    let (alias_name, _) = parse_ident_like_at(
                        tokens,
                        cursor,
                        "Expected alias in selective import list",
                        end_span,
                    )?;
                    item_alias = Some(alias_name);
                }
                items.push(UseItemAst {
                    name,
                    alias: item_alias,
                    span,
                });
                if consume_kind_at(tokens, cursor, TokenKind::CloseParen) {
                    break;
                }
                if !consume_kind_at(tokens, cursor, TokenKind::Comma) {
                    return Err(ParseError {
                        message: "Expected ',' or ')' in selective import list".to_string(),
                        span: current_span_at(tokens, *cursor, end_span),
                    });
                }
            }
        }
    }

    if match_keyword_at(tokens, cursor, "with") {
        if !consume_kind_at(tokens, cursor, TokenKind::OpenParen) {
            return Err(ParseError {
                message: "Expected '(' after 'with'".to_string(),
                span: current_span_at(tokens, *cursor, end_span),
            });
        }
        if consume_kind_at(tokens, cursor, TokenKind::CloseParen) {
            return Err(ParseError {
                message: "Parameter list cannot be empty".to_string(),
                span: prev_span_at(tokens, *cursor, end_span),
            });
        }
        loop {
            let (name, span) = parse_ident_like_at(
                tokens,
                cursor,
                "Expected parameter name in 'with' list",
                end_span,
            )?;
            if !match_operator_at(tokens, cursor, OperatorKind::Eq) {
                return Err(ParseError {
                    message: "Expected '=' in 'with' parameter".to_string(),
                    span: current_span_at(tokens, *cursor, end_span),
                });
            }
            let value_start = *cursor;
            let mut depth_paren = 0i32;
            let mut depth_bracket = 0i32;
            let mut depth_brace = 0i32;
            while *cursor < tokens.len() {
                let token = &tokens[*cursor];
                if matches!(token.kind, TokenKind::CloseParen)
                    && depth_paren == 0
                    && depth_bracket == 0
                    && depth_brace == 0
                {
                    break;
                }
                if matches!(token.kind, TokenKind::Comma)
                    && depth_paren == 0
                    && depth_bracket == 0
                    && depth_brace == 0
                {
                    break;
                }
                update_group_depths_for_token(
                    &token.kind,
                    &mut depth_paren,
                    &mut depth_bracket,
                    &mut depth_brace,
                );
                *cursor = cursor.saturating_add(1);
            }
            let expr_end_span = tokens
                .get(*cursor)
                .map(|token| token.span)
                .unwrap_or(end_span);
            let value = parse_expr_with_vm_contract_and_boundary(
                expr_parse_ctx,
                &tokens[value_start..*cursor],
                expr_end_span,
                end_token_text.clone(),
                tokens.get(*cursor),
            )?;
            params.push(UseParamAst { name, value, span });
            if consume_kind_at(tokens, cursor, TokenKind::CloseParen) {
                break;
            }
            if !consume_kind_at(tokens, cursor, TokenKind::Comma) {
                return Err(ParseError {
                    message: "Expected ',' or ')' in 'with' parameter list".to_string(),
                    span: current_span_at(tokens, *cursor, end_span),
                });
            }
        }
    }

    if *cursor < tokens.len() {
        return Err(ParseError {
            message: "Unexpected trailing tokens after .use".to_string(),
            span: tokens[*cursor].span,
        });
    }
    let tail_span = if *cursor == 0 {
        end_span
    } else {
        prev_span_at(tokens, *cursor, end_span)
    };
    Ok(LineAst::Use(UseAst {
        module_id,
        alias,
        items,
        params,
        span: Span {
            line: start_span.line,
            col_start: start_span.col_start,
            col_end: tail_span.col_end,
        },
    }))
}

pub(super) fn dot_conditional_kind(name_upper: &str) -> Option<(ConditionalKind, bool, bool)> {
    match name_upper {
        "IF" => Some((ConditionalKind::If, true, false)),
        "ELSEIF" => Some((ConditionalKind::ElseIf, true, false)),
        "ELSE" => Some((ConditionalKind::Else, false, false)),
        "ENDIF" => Some((ConditionalKind::EndIf, false, false)),
        "MATCH" => Some((ConditionalKind::Switch, true, false)),
        "CASE" => Some((ConditionalKind::Case, true, true)),
        "DEFAULT" => Some((ConditionalKind::Default, false, false)),
        "ENDMATCH" => Some((ConditionalKind::EndSwitch, false, false)),
        _ => None,
    }
}

pub(super) fn parse_ident_like_at(
    tokens: &[Token],
    cursor: &mut usize,
    message: &str,
    end_span: Span,
) -> Result<(String, Span), ParseError> {
    match tokens.get(*cursor) {
        Some(Token {
            kind: TokenKind::Identifier(name),
            span,
        }) => {
            *cursor = cursor.saturating_add(1);
            Ok((name.clone(), *span))
        }
        Some(Token {
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

pub(super) fn match_keyword_at(tokens: &[Token], cursor: &mut usize, keyword: &str) -> bool {
    match tokens.get(*cursor) {
        Some(Token {
            kind: TokenKind::Identifier(name),
            ..
        }) if name.eq_ignore_ascii_case(keyword) => {
            *cursor = cursor.saturating_add(1);
            true
        }
        _ => false,
    }
}

pub(super) fn consume_kind_at(tokens: &[Token], cursor: &mut usize, kind: TokenKind) -> bool {
    if matches!(tokens.get(*cursor), Some(Token { kind: value, .. }) if *value == kind) {
        *cursor = cursor.saturating_add(1);
        return true;
    }
    false
}

pub(super) fn match_operator_at(tokens: &[Token], cursor: &mut usize, op: OperatorKind) -> bool {
    if matches!(
        tokens.get(*cursor),
        Some(Token {
            kind: TokenKind::Operator(value),
            ..
        }) if *value == op
    ) {
        *cursor = cursor.saturating_add(1);
        return true;
    }
    false
}

pub(super) fn prev_span_at(tokens: &[Token], cursor: usize, fallback: Span) -> Span {
    if cursor == 0 {
        fallback
    } else {
        tokens
            .get(cursor.saturating_sub(1))
            .map(|token| token.span)
            .unwrap_or(fallback)
    }
}

pub(super) fn current_span_at(tokens: &[Token], cursor: usize, fallback: Span) -> Span {
    tokens
        .get(cursor)
        .map(|token| token.span)
        .unwrap_or(fallback)
}
