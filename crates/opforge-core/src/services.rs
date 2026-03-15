// SPDX-License-Identifier: GPL-3.0-or-later

//! Stable lower-level opcore processor services.

use crate::modules;
use crate::parser::{Expr, LineAst, ParseError, Parser};
use crate::tokenizer::{Span, Token, TokenKind, TokenizeError, Tokenizer};
use types::processing::ProcessingOutcome;

#[derive(Debug, Clone)]
pub struct TokenizedLine {
    pub tokens: Vec<Token>,
    pub end_span: Span,
    pub end_token_text: Option<String>,
}

pub fn tokenize_line(line: &str, line_num: u32) -> Result<TokenizedLine, TokenizeError> {
    let mut tokenizer = Tokenizer::new(line, line_num);
    let mut tokens = Vec::new();
    let mut end_token_text = None;
    let end_span = loop {
        let token = tokenizer.next_token()?;
        if matches!(token.kind, TokenKind::End) {
            let idx = token.span.col_start.saturating_sub(1);
            if idx < line.len() && line.as_bytes().get(idx) == Some(&b';') {
                end_token_text = Some(line[idx..].to_string());
            }
            break token.span;
        }
        tokens.push(token);
    };

    Ok(TokenizedLine {
        tokens,
        end_span,
        end_token_text,
    })
}

pub fn parse_expression_tokens(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Expr, ParseError> {
    Parser::parse_expr_from_tokens(tokens, end_span, end_token_text)
}

pub fn parse_expression(tokenized: TokenizedLine) -> Result<Expr, ParseError> {
    parse_expression_tokens(
        tokenized.tokens,
        tokenized.end_span,
        tokenized.end_token_text,
    )
}

pub fn process_module_item(line: &str, line_num: u32) -> ProcessingOutcome<LineAst, ParseError> {
    modules::process_module_item_request(line, line_num)
}
