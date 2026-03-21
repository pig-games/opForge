// SPDX-License-Identifier: GPL-3.0-or-later

use super::*;

pub(super) fn process_opcore_statement_request(
    parser: &mut Parser,
) -> ProcessingOutcome<LineAst, ParseError> {
    if parser.tokens.is_empty() {
        return ProcessingOutcome::Done(LineAst::Empty);
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
        return asm_statement_request();
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
                return asm_statement_request();
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
                return ProcessingOutcome::Error(ParseError {
                    message: "Unexpected trailing tokens".to_string(),
                    span: parser.tokens[parser.index].span,
                });
            }
            return ProcessingOutcome::Done(LineAst::Assignment(AssignmentAst {
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
                return ProcessingOutcome::Error(ParseError {
                    message: "Expected conditional after '.'".to_string(),
                    span: token.span,
                });
            }
            None => {
                return ProcessingOutcome::Error(ParseError {
                    message: "Expected conditional after '.'".to_string(),
                    span: parser.end_span,
                });
            }
        };
        let upper = name.to_ascii_uppercase();
        if upper.as_str() == "USE" {
            return match parser.parse_use_directive(span) {
                Ok(ast) => ProcessingOutcome::Done(ast),
                Err(err) => ProcessingOutcome::Error(err),
            };
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
                if matches!(upper.as_str(), "FOR" | "BFOR") {
                    return match parser.parse_for_like_directive(label, name) {
                        Ok(ast) => ProcessingOutcome::Done(ast),
                        Err(err) => ProcessingOutcome::Error(err),
                    };
                }
                if matches!(upper.as_str(), "WHILE" | "BWHILE") {
                    return match parser.parse_while_like_directive(label, name) {
                        Ok(ast) => ProcessingOutcome::Done(ast),
                        Err(err) => ProcessingOutcome::Error(err),
                    };
                }
                if matches!(
                    upper.as_str(),
                    "MACRO" | "SEGMENT" | "ENDMACRO" | "ENDSEGMENT" | "ENDM" | "ENDS"
                ) {
                    parser.index = parser.tokens.len();
                    return ProcessingOutcome::Done(LineAst::Statement(StatementAst {
                        label,
                        mnemonic: Some(format!(".{name}")),
                        operands: Vec::new(),
                    }));
                }
                if !is_opcore_owned_dot_statement_name(&upper) {
                    return asm_statement_request();
                }
                let mut operands = Vec::new();
                if parser.index < parser.tokens.len() {
                    match parser.parse_expr() {
                        Ok(expr) => operands.push(expr),
                        Err(err) => {
                            operands.push(Expr::Error(err.message, err.span));
                            return ProcessingOutcome::Done(LineAst::Statement(StatementAst {
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
                                return ProcessingOutcome::Done(LineAst::Statement(StatementAst {
                                    label,
                                    mnemonic: Some(format!(".{name}")),
                                    operands,
                                }));
                            }
                        }
                    }
                }
                if parser.index < parser.tokens.len() {
                    return ProcessingOutcome::Error(ParseError {
                        message: "Unexpected trailing tokens".to_string(),
                        span: parser.tokens[parser.index].span,
                    });
                }
                return ProcessingOutcome::Done(LineAst::Statement(StatementAst {
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
            return ProcessingOutcome::Error(ParseError {
                message: "Unexpected tokens after conditional".to_string(),
                span: parser.tokens[parser.index].span,
            });
        }
        return ProcessingOutcome::Done(LineAst::Conditional(ConditionalAst { kind, exprs, span }));
    }

    asm_statement_request()
}

fn asm_statement_request() -> ProcessingOutcome<LineAst, ParseError> {
    ProcessingOutcome::Return(ProcessingReturn::Request {
        request: ProcessingRequestKind::Processor {
            processor: "asm".to_string(),
            kind: "statement".to_string(),
        },
    })
}

fn is_opcore_owned_dot_statement_name(upper: &str) -> bool {
    matches!(
        upper,
        "MODULE"
            | "ENDMODULE"
            | "END"
            | "FOR"
            | "BFOR"
            | "ENDFOR"
            | "WHILE"
            | "BWHILE"
            | "ENDWHILE"
            | "STRUCT"
            | "ENDSTRUCT"
            | "MACRO"
            | "ENDMACRO"
            | "ENDM"
            | "SEGMENT"
            | "ENDSEGMENT"
            | "ENDS"
            | "NAMESPACE"
            | "ENDN"
            | "ENDNAMESPACE"
            | "BLOCK"
            | "ENDBLOCK"
            | "PUB"
            | "PRIV"
    )
}
