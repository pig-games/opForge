// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

// Parser for tokenized assembly source.

use crate::text_utils::is_ident_start;
use crate::tokenizer::{
    ConditionalKind, OperatorKind, Span, Token, TokenKind, TokenizeError, Tokenizer,
};
use types::line_ast::{
    AssignmentAst, ConditionalAst, PackAst, PlaceAst, StatementAst, StatementDefAst,
    StatementEndAst, UseAst, UseItemAst, UseParamAst, UseSectionMapAst,
};
use types::processing::{ProcessingOutcome, ProcessingRequestKind, ProcessingReturn};

#[path = "parser_compat_mixed.rs"]
mod parser_compat_mixed;
#[path = "parser_opcore_requests.rs"]
mod parser_opcore_requests;
#[path = "parser_statement_signature.rs"]
mod parser_statement_signature;
pub use parser_statement_signature::select_and_match_statement_signature_texts;
#[cfg(test)]
pub(crate) use parser_statement_signature::{
    match_statement_signature, select_statement_signature,
};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LineAst {
    Empty,
    Conditional(ConditionalAst<ConditionalKind, Expr, Span>),
    Place(PlaceAst<Expr, Span>),
    Pack(PackAst<Span>),
    Use(UseAst<UseItem, UseParam, Span>),
    StatementDef(StatementDefAst<StatementSignature, Span>),
    StatementEnd(StatementEndAst<Span>),
    Assignment(AssignmentAst<Label, AssignOp, Expr, Span>),
    Statement(StatementAst<Label, Expr>),
}

#[derive(Debug, Clone)]
pub struct Label {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(String, Span),
    Identifier(String, Span),
    Register(String, Span),
    List(Vec<Expr>, Span),
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    Member {
        base: Box<Expr>,
        field: String,
        span: Span,
    },
    StructLiteral {
        type_name: String,
        fields: Vec<(String, Expr)>,
        span: Span,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        span: Span,
    },
    Placeholder(Span),
    /// Indirect/memory reference via register: (HL), (BC), (IX+d), etc.
    /// For simple cases like (HL), the inner is Register.
    /// For indexed like (IX+5), the inner is Binary with base register.
    Indirect(Box<Expr>, Span),
    /// Immediate value: #expr
    Immediate(Box<Expr>, Span),
    /// Bracketed long-indirect expression: [expr]
    IndirectLong(Box<Expr>, Span),
    /// Tuple/List: (a, b) - used for complex indirects like ($nn, X)
    Tuple(Vec<Expr>, Span),
    Dollar(Span),
    String(Vec<u8>, Span),
    Error(String, Span),
    Ternary {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        step: Option<Box<Expr>>,
        inclusive: bool,
        span: Span,
    },
}

pub type UseItem = UseItemAst<Span>;
pub type UseParam = UseParamAst<Expr, Span>;

#[derive(Debug, Clone)]
pub struct StatementSignature {
    pub atoms: Vec<SignatureAtom>,
}

#[derive(Debug, Clone)]
pub enum SignatureAtom {
    Literal(Vec<u8>, Span),
    Capture {
        type_name: String,
        name: String,
        span: Span,
    },
    Boundary {
        atoms: Vec<SignatureAtom>,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct StatementCapture {
    pub(crate) name: String,
    pub(crate) tokens: Vec<Token>,
}

#[derive(Debug, Clone)]
pub(crate) struct StatementMatch {
    pub(crate) captures: Vec<StatementCapture>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StatementCaptureText {
    pub name: String,
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    BitNot,
    LogicNot,
    High,
    Low,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Const,
    Var,
    VarIfUndef,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    BitOr,
    BitXor,
    BitAnd,
    LogicOr,
    LogicAnd,
    Shl,
    Shr,
    Concat,
    Min,
    Max,
    Repeat,
    Member,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Multiply,
    Divide,
    Mod,
    Power,
    Shl,
    Shr,
    Add,
    Subtract,
    Eq,
    Ne,
    Ge,
    Gt,
    Le,
    Lt,
    BitAnd,
    BitOr,
    BitXor,
    LogicAnd,
    LogicOr,
    LogicXor,
}

/// Maximum nesting depth for recursive expression parsing (unary chains,
/// parenthesised sub-expressions). Prevents stack overflow on malicious or
/// pathological input.
const MAX_PARSE_DEPTH: usize = 256;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    end_span: Span,
    end_token_text: Option<String>,
    parse_depth: usize,
}

impl Parser {
    fn from_token_parts(
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Self {
        Self {
            tokens,
            index: 0,
            end_span,
            end_token_text,
            parse_depth: 0,
        }
    }
    pub fn from_line(line: &str, line_num: u32) -> Result<Self, ParseError> {
        Self::from_line_with_registers(line, line_num, crate::tokenizer::register_checker_none())
    }

    pub fn from_tokens(tokens: Vec<Token>, end_span: Span, end_token_text: Option<String>) -> Self {
        Self::from_token_parts(tokens, end_span, end_token_text)
    }

    pub fn parse_compat_mixed_line_from_tokens(
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<LineAst, ParseError> {
        let mut parser = Self::from_token_parts(tokens, end_span, end_token_text);
        parser.parse_compat_mixed_line()
    }

    pub fn parse_expr_from_tokens(
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<Expr, ParseError> {
        let mut parser = Self::from_token_parts(tokens, end_span, end_token_text);
        let expr = parser.parse_expr()?;
        if parser.index < parser.tokens.len() {
            return Err(ParseError {
                message: "Unexpected trailing tokens".to_string(),
                span: parser.tokens[parser.index].span,
            });
        }
        Ok(expr)
    }

    pub fn process_opcore_line_request(
        line: &str,
        line_num: u32,
    ) -> ProcessingOutcome<LineAst, ParseError> {
        let mut parser = match Self::from_line(line, line_num) {
            Ok(parser) => parser,
            Err(err) => return ProcessingOutcome::Error(err),
        };
        parser.process_opcore_statement_request()
    }

    fn from_line_with_registers(
        line: &str,
        line_num: u32,
        is_register: crate::tokenizer::RegisterChecker,
    ) -> Result<Self, ParseError> {
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
        Ok(Self::from_token_parts(tokens, end_span, end_token_text))
    }

    pub fn end_span(&self) -> Span {
        self.end_span
    }

    pub fn end_token_text(&self) -> Option<&str> {
        self.end_token_text.as_deref()
    }

    pub fn parse_compat_mixed_line(&mut self) -> Result<LineAst, ParseError> {
        parser_compat_mixed::parse_compat_mixed_line(self)
    }

    pub fn process_opcore_statement_request(&mut self) -> ProcessingOutcome<LineAst, ParseError> {
        parser_opcore_requests::process_opcore_statement_request(self)
    }

    fn parse_for_like_directive(
        &mut self,
        label: Option<Label>,
        name: String,
    ) -> Result<LineAst, ParseError> {
        let mut operands = Vec::new();
        let mnemonic = Some(format!(".{name}"));

        let start_index = self.index;
        if let Some(Token {
            kind: TokenKind::Identifier(var_name),
            span: var_span,
        })
        | Some(Token {
            kind: TokenKind::Register(var_name),
            span: var_span,
        }) = self.peek().cloned()
        {
            self.index = self.index.saturating_add(1);
            if self.match_keyword("in") {
                operands.push(Expr::Identifier(var_name, var_span));
                match self.parse_expr() {
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
                if self.index < self.tokens.len() {
                    return Err(ParseError {
                        message: "Unexpected trailing tokens".to_string(),
                        span: self.tokens[self.index].span,
                    });
                }
                return Ok(LineAst::Statement(StatementAst {
                    label,
                    mnemonic,
                    operands,
                }));
            }
        }

        self.index = start_index;
        match self.parse_expr() {
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
        if self.index < self.tokens.len() {
            return Err(ParseError {
                message: "Unexpected trailing tokens".to_string(),
                span: self.tokens[self.index].span,
            });
        }
        Ok(LineAst::Statement(StatementAst {
            label,
            mnemonic,
            operands,
        }))
    }

    fn parse_while_like_directive(
        &mut self,
        label: Option<Label>,
        name: String,
    ) -> Result<LineAst, ParseError> {
        let mut operands = Vec::new();
        let mnemonic = Some(format!(".{name}"));

        match self.parse_expr() {
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
        if self.index < self.tokens.len() {
            return Err(ParseError {
                message: "Unexpected trailing tokens".to_string(),
                span: self.tokens[self.index].span,
            });
        }
        Ok(LineAst::Statement(StatementAst {
            label,
            mnemonic,
            operands,
        }))
    }

    fn match_assignment_op(&self) -> Option<(AssignOp, Span, usize)> {
        let token = self.tokens.get(self.index)?;
        let next = self.tokens.get(self.index + 1);
        let next2 = self.tokens.get(self.index + 2);
        match &token.kind {
            TokenKind::Operator(OperatorKind::Eq) => Some((AssignOp::Const, token.span, 1)),
            TokenKind::Colon => {
                if matches!(
                    next,
                    Some(Token {
                        kind: TokenKind::Question,
                        ..
                    })
                ) && matches!(
                    next2,
                    Some(Token {
                        kind: TokenKind::Operator(OperatorKind::Eq),
                        ..
                    })
                ) {
                    Some((AssignOp::VarIfUndef, token.span, 3))
                } else if matches!(
                    next,
                    Some(Token {
                        kind: TokenKind::Operator(OperatorKind::Eq),
                        ..
                    })
                ) {
                    Some((AssignOp::Var, token.span, 2))
                } else {
                    None
                }
            }
            TokenKind::Operator(kind) => {
                if *kind == OperatorKind::RangeInclusive {
                    return Some((AssignOp::Concat, token.span, 1));
                }
                let op = match kind {
                    OperatorKind::Plus => AssignOp::Add,
                    OperatorKind::Minus => AssignOp::Sub,
                    OperatorKind::Multiply => AssignOp::Mul,
                    OperatorKind::Divide => AssignOp::Div,
                    OperatorKind::Mod => AssignOp::Mod,
                    OperatorKind::Power => AssignOp::Pow,
                    OperatorKind::BitOr => AssignOp::BitOr,
                    OperatorKind::BitXor => AssignOp::BitXor,
                    OperatorKind::BitAnd => AssignOp::BitAnd,
                    OperatorKind::LogicOr => AssignOp::LogicOr,
                    OperatorKind::LogicAnd => AssignOp::LogicAnd,
                    OperatorKind::Shl => AssignOp::Shl,
                    OperatorKind::Shr => AssignOp::Shr,
                    OperatorKind::Lt => {
                        if matches!(
                            next,
                            Some(Token {
                                kind: TokenKind::Question,
                                ..
                            })
                        ) && matches!(
                            next2,
                            Some(Token {
                                kind: TokenKind::Operator(OperatorKind::Eq),
                                ..
                            })
                        ) {
                            return Some((AssignOp::Min, token.span, 3));
                        }
                        return None;
                    }
                    OperatorKind::Gt => {
                        if matches!(
                            next,
                            Some(Token {
                                kind: TokenKind::Question,
                                ..
                            })
                        ) && matches!(
                            next2,
                            Some(Token {
                                kind: TokenKind::Operator(OperatorKind::Eq),
                                ..
                            })
                        ) {
                            return Some((AssignOp::Max, token.span, 3));
                        }
                        return None;
                    }
                    _ => return None,
                };
                if matches!(
                    next,
                    Some(Token {
                        kind: TokenKind::Operator(OperatorKind::Eq),
                        ..
                    })
                ) {
                    Some((op, token.span, 2))
                } else {
                    None
                }
            }
            TokenKind::Dot => {
                if matches!(
                    next,
                    Some(Token {
                        kind: TokenKind::Dot,
                        ..
                    })
                ) && matches!(
                    next2,
                    Some(Token {
                        kind: TokenKind::Operator(OperatorKind::Eq),
                        ..
                    })
                ) {
                    Some((AssignOp::Concat, token.span, 3))
                } else if matches!(
                    next,
                    Some(Token {
                        kind: TokenKind::Operator(OperatorKind::Eq),
                        ..
                    })
                ) {
                    Some((AssignOp::Member, token.span, 2))
                } else {
                    None
                }
            }
            TokenKind::Identifier(name) => {
                if name.eq_ignore_ascii_case("x")
                    && matches!(
                        next,
                        Some(Token {
                            kind: TokenKind::Operator(OperatorKind::Eq),
                            ..
                        })
                    )
                {
                    Some((AssignOp::Repeat, token.span, 2))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn peek_kind(&self, kind: TokenKind) -> bool {
        matches!(self.peek(), Some(Token { kind: k, .. }) if *k == kind)
    }

    fn match_keyword(&mut self, keyword: &str) -> bool {
        match self.peek() {
            Some(Token {
                kind: TokenKind::Identifier(name),
                ..
            }) if name.eq_ignore_ascii_case(keyword) => {
                self.index += 1;
                true
            }
            _ => false,
        }
    }

    fn parse_ident_like(&mut self, message: &str) -> Result<(String, Span), ParseError> {
        match self.next() {
            Some(Token {
                kind: TokenKind::Identifier(name),
                span,
            }) => Ok((name, span)),
            Some(Token {
                kind: TokenKind::Register(name),
                span,
            }) => Ok((name, span)),
            Some(token) => Err(ParseError {
                message: message.to_string(),
                span: token.span,
            }),
            None => Err(ParseError {
                message: message.to_string(),
                span: self.end_span,
            }),
        }
    }

    fn parse_use_directive(&mut self, start_span: Span) -> Result<LineAst, ParseError> {
        let (module_id, _module_span) = self.parse_ident_like("Expected module id after .use")?;
        let mut alias = None;
        let mut items = Vec::new();
        let mut params = Vec::new();
        let mut section_maps = Vec::new();

        if self.consume_kind(TokenKind::OpenParen) {
            if self.consume_kind(TokenKind::CloseParen) {
                return Err(ParseError {
                    message: "Selective import list cannot be empty".to_string(),
                    span: self.prev_span(),
                });
            }
            if self.match_operator(OperatorKind::Multiply) {
                let star_span = self.prev_span();
                if self.match_keyword("as") {
                    return Err(ParseError {
                        message: "Wildcard import cannot have an alias".to_string(),
                        span: self.current_span(),
                    });
                }
                if !self.consume_kind(TokenKind::CloseParen) {
                    return Err(ParseError {
                        message: "Wildcard import must be the only selective item".to_string(),
                        span: self.current_span(),
                    });
                }
                items.push(UseItemAst {
                    name: "*".to_string(),
                    alias: None,
                    span: star_span,
                });
            } else {
                loop {
                    let (name, span) =
                        self.parse_ident_like("Expected identifier in selective import list")?;
                    let mut item_alias = None;
                    if self.match_keyword("as") {
                        let (alias_name, _alias_span) =
                            self.parse_ident_like("Expected alias in selective import list")?;
                        item_alias = Some(alias_name);
                    }
                    items.push(UseItemAst {
                        name,
                        alias: item_alias,
                        span,
                    });
                    if self.consume_kind(TokenKind::CloseParen) {
                        break;
                    }
                    if !self.consume_comma() {
                        return Err(ParseError {
                            message: "Expected ',' or ')' in selective import list".to_string(),
                            span: self.current_span(),
                        });
                    }
                }
            }
        }

        if self.match_keyword("as") {
            let (name, _span) = self.parse_ident_like("Expected alias identifier after 'as'")?;
            alias = Some(name);
            if items
                .iter()
                .any(|item: &UseItemAst<Span>| item.alias.is_some() || item.name == "*")
            {
                return Err(ParseError {
                    message:
                        "Qualified selective imports cannot use per-item aliases or wildcard selections"
                            .to_string(),
                    span: self.prev_span(),
                });
            }
        }

        if self.match_keyword("with") {
            if !self.consume_kind(TokenKind::OpenParen) {
                return Err(ParseError {
                    message: "Expected '(' after 'with'".to_string(),
                    span: self.current_span(),
                });
            }
            if self.consume_kind(TokenKind::CloseParen) {
                return Err(ParseError {
                    message: "Parameter list cannot be empty".to_string(),
                    span: self.prev_span(),
                });
            }
            loop {
                let (name, span) =
                    self.parse_ident_like("Expected parameter name in 'with' list")?;
                if !self.match_operator(OperatorKind::Eq) {
                    return Err(ParseError {
                        message: "Expected '=' in 'with' parameter".to_string(),
                        span: self.current_span(),
                    });
                }
                let value = self.parse_expr()?;
                params.push(UseParamAst { name, value, span });
                if self.consume_kind(TokenKind::CloseParen) {
                    break;
                }
                if !self.consume_comma() {
                    return Err(ParseError {
                        message: "Expected ',' or ')' in 'with' parameter list".to_string(),
                        span: self.current_span(),
                    });
                }
            }
        }

        if self.match_keyword("map") {
            if !self.consume_kind(TokenKind::OpenBrace) {
                return Err(ParseError {
                    message: "Expected '{' after 'map'".to_string(),
                    span: self.current_span(),
                });
            }
            if self.consume_kind(TokenKind::CloseBrace) {
                return Err(ParseError {
                    message: "Section map cannot be empty".to_string(),
                    span: self.prev_span(),
                });
            }
            loop {
                let (logical, span) =
                    self.parse_ident_like("Expected logical section name in map")?;
                if !self.match_operator(OperatorKind::Minus)
                    || !self.match_operator(OperatorKind::Gt)
                {
                    return Err(ParseError {
                        message: "Expected '->' in section map entry".to_string(),
                        span: self.current_span(),
                    });
                }
                let (concrete, _) =
                    self.parse_ident_like("Expected concrete section name in map")?;
                section_maps.push(UseSectionMapAst {
                    logical,
                    concrete,
                    span,
                });
                if self.consume_kind(TokenKind::CloseBrace) {
                    break;
                }
                let _ = self.consume_comma();
            }
            if alias.is_none()
                && (!items.is_empty() || module_id.rsplit('.').all(|segment| segment.is_empty()))
            {
                return Err(ParseError {
                    message: "Section maps require a module namespace qualifier".to_string(),
                    span: self.prev_span(),
                });
            }
        }

        if self.index < self.tokens.len() {
            return Err(ParseError {
                message: "Unexpected trailing tokens after .use".to_string(),
                span: self.tokens[self.index].span,
            });
        }

        let end_span = if self.index == 0 {
            self.end_span
        } else {
            self.prev_span()
        };
        let span = Span {
            line: start_span.line,
            col_start: start_span.col_start,
            col_end: end_span.col_end,
        };

        Ok(LineAst::Use(UseAst {
            module_id,
            alias,
            items,
            params,
            section_maps,
            span,
        }))
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek_operator_kind() {
            Some(OperatorKind::Lt) => {
                self.index += 1;
                let span = self.prev_span();
                let expr = self.parse_expr()?;
                return Ok(Expr::Unary {
                    op: UnaryOp::Low,
                    expr: Box::new(expr),
                    span,
                });
            }
            Some(OperatorKind::Gt) => {
                self.index += 1;
                let span = self.prev_span();
                let expr = self.parse_expr()?;
                return Ok(Expr::Unary {
                    op: UnaryOp::High,
                    expr: Box::new(expr),
                    span,
                });
            }
            _ => {}
        }

        self.parse_ternary()
    }

    fn parse_ternary(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_logical_or()?;
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Question {
                let span = token.span;
                self.index += 1;
                let then_expr = self.parse_expr()?;
                if !self.consume_kind(TokenKind::Colon) {
                    return Err(ParseError {
                        message: "Missing ':' in conditional expression".to_string(),
                        span: self.current_span(),
                    });
                }
                let else_expr = self.parse_expr()?;
                node = Expr::Ternary {
                    cond: Box::new(node),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                    span,
                };
            }
        }
        Ok(node)
    }

    fn parse_logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_logical_and()?;
        loop {
            let op = match self.peek_operator_kind() {
                Some(OperatorKind::LogicOr) => BinaryOp::LogicOr,
                Some(OperatorKind::LogicXor) => BinaryOp::LogicXor,
                _ => break,
            };
            self.index += 1;
            let op_span = self.prev_span();
            let right = self.parse_logical_and()?;
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(right),
                span: op_span,
            };
        }
        Ok(node)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_bit_or()?;
        while self.match_operator(OperatorKind::LogicAnd) {
            let op_span = self.prev_span();
            let right = self.parse_bit_or()?;
            node = Expr::Binary {
                op: BinaryOp::LogicAnd,
                left: Box::new(node),
                right: Box::new(right),
                span: op_span,
            };
        }
        Ok(node)
    }

    fn parse_bit_or(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_bit_xor()?;
        while self.match_operator(OperatorKind::BitOr) {
            let op_span = self.prev_span();
            let right = self.parse_bit_xor()?;
            node = Expr::Binary {
                op: BinaryOp::BitOr,
                left: Box::new(node),
                right: Box::new(right),
                span: op_span,
            };
        }
        Ok(node)
    }

    fn parse_bit_xor(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_bit_and()?;
        while self.match_operator(OperatorKind::BitXor) {
            let op_span = self.prev_span();
            let right = self.parse_bit_and()?;
            node = Expr::Binary {
                op: BinaryOp::BitXor,
                left: Box::new(node),
                right: Box::new(right),
                span: op_span,
            };
        }
        Ok(node)
    }

    fn parse_bit_and(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_range()?;
        while self.match_operator(OperatorKind::BitAnd) {
            let op_span = self.prev_span();
            let right = self.parse_range()?;
            node = Expr::Binary {
                op: BinaryOp::BitAnd,
                left: Box::new(node),
                right: Box::new(right),
                span: op_span,
            };
        }
        Ok(node)
    }

    fn parse_range(&mut self) -> Result<Expr, ParseError> {
        let start = self.parse_compare()?;
        let (inclusive, op_span) = match self.peek_operator_kind() {
            Some(OperatorKind::Range) => {
                self.index += 1;
                (false, self.prev_span())
            }
            Some(OperatorKind::RangeInclusive) => {
                self.index += 1;
                (true, self.prev_span())
            }
            _ => return Ok(start),
        };

        let end = self.parse_compare()?;
        let step = if self.consume_kind(TokenKind::Colon) {
            Some(Box::new(self.parse_compare()?))
        } else {
            None
        };

        Ok(Expr::Range {
            start: Box::new(start),
            end: Box::new(end),
            step,
            inclusive,
            span: op_span,
        })
    }

    fn parse_compare(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_shift()?;
        loop {
            let op = match self.peek_operator_kind() {
                Some(OperatorKind::Eq) => BinaryOp::Eq,
                Some(OperatorKind::Ne) => BinaryOp::Ne,
                Some(OperatorKind::Ge) => BinaryOp::Ge,
                Some(OperatorKind::Gt) => BinaryOp::Gt,
                Some(OperatorKind::Le) => BinaryOp::Le,
                Some(OperatorKind::Lt) => BinaryOp::Lt,
                _ => break,
            };
            self.index += 1;
            let op_span = self.prev_span();
            let right = self.parse_shift()?;
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(right),
                span: op_span,
            };
        }
        Ok(node)
    }

    fn parse_shift(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_sum()?;
        loop {
            let op = match self.peek_operator_kind() {
                Some(OperatorKind::Shl) => BinaryOp::Shl,
                Some(OperatorKind::Shr) => BinaryOp::Shr,
                _ => break,
            };
            self.index += 1;
            let op_span = self.prev_span();
            let right = self.parse_sum()?;
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(right),
                span: op_span,
            };
        }
        Ok(node)
    }

    fn parse_sum(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_term()?;
        loop {
            let op = match self.peek_operator_kind() {
                Some(OperatorKind::Plus) => BinaryOp::Add,
                Some(OperatorKind::Minus) => BinaryOp::Subtract,
                _ => break,
            };
            self.index += 1;
            let op_span = self.prev_span();
            let right = self.parse_term()?;
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(right),
                span: op_span,
            };
        }
        Ok(node)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_power()?;
        loop {
            let op = match self.peek_operator_kind() {
                Some(OperatorKind::Multiply) => BinaryOp::Multiply,
                Some(OperatorKind::Divide) => BinaryOp::Divide,
                Some(OperatorKind::Mod) => BinaryOp::Mod,
                _ => break,
            };
            self.index += 1;
            let op_span = self.prev_span();
            let right = self.parse_power()?;
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(right),
                span: op_span,
            };
        }
        Ok(node)
    }

    fn parse_power(&mut self) -> Result<Expr, ParseError> {
        let node = self.parse_unary()?;
        if self.match_operator(OperatorKind::Power) {
            let op_span = self.prev_span();
            let right = self.parse_power()?;
            return Ok(Expr::Binary {
                op: BinaryOp::Power,
                left: Box::new(node),
                right: Box::new(right),
                span: op_span,
            });
        }
        Ok(node)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op) = match self.peek_operator_kind() {
            Some(OperatorKind::Plus) => Some(UnaryOp::Plus),
            Some(OperatorKind::Minus) => Some(UnaryOp::Minus),
            Some(OperatorKind::BitNot) => Some(UnaryOp::BitNot),
            Some(OperatorKind::LogicNot) => Some(UnaryOp::LogicNot),
            _ => None,
        } {
            self.parse_depth += 1;
            if self.parse_depth > MAX_PARSE_DEPTH {
                let span = self.current_span();
                return Err(ParseError {
                    message: format!(
                        "Expression nesting exceeds maximum depth ({})",
                        MAX_PARSE_DEPTH
                    ),
                    span,
                });
            }
            self.index += 1;
            let span = self.prev_span();
            let expr = self.parse_unary()?;
            self.parse_depth -= 1;
            return Ok(Expr::Unary {
                op,
                expr: Box::new(expr),
                span,
            });
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let base = match self.next() {
            Some(Token {
                kind: TokenKind::Hash,
                span,
            }) => Err(ParseError {
                message: "Unexpected token in expression".to_string(),
                span,
            }),
            Some(Token {
                kind: TokenKind::Number(num),
                span,
            }) => Ok(Expr::Number(num.text, span)),
            Some(Token {
                kind: TokenKind::Identifier(name),
                span,
            }) => Ok(Expr::Identifier(name, span)),
            Some(Token {
                kind: TokenKind::Register(name),
                span,
            }) => Ok(Expr::Register(name, span)),
            Some(Token {
                kind: TokenKind::Dollar,
                span,
            }) => Ok(Expr::Dollar(span)),
            Some(Token {
                kind: TokenKind::String(lit),
                span,
            }) => Ok(Expr::String(lit.bytes, span)),
            Some(Token {
                kind: TokenKind::Question,
                span,
            }) => Ok(Expr::Placeholder(span)),
            Some(Token {
                kind: TokenKind::Dot,
                span: dot_span,
            }) => {
                let name = match self.next() {
                    Some(Token {
                        kind: TokenKind::Identifier(name),
                        ..
                    })
                    | Some(Token {
                        kind: TokenKind::Register(name),
                        ..
                    }) => name,
                    Some(token) => {
                        return Err(ParseError {
                            message: "Expected function name after '.'".to_string(),
                            span: token.span,
                        })
                    }
                    None => {
                        return Err(ParseError {
                            message: "Expected function name after '.'".to_string(),
                            span: self.end_span,
                        })
                    }
                };
                if !self.consume_kind(TokenKind::OpenParen) {
                    return Err(ParseError {
                        message: "Expected '(' after function name".to_string(),
                        span: self.current_span(),
                    });
                }
                let mut args = Vec::new();
                if !self.consume_kind(TokenKind::CloseParen) {
                    args.push(self.parse_expr()?);
                    while self.consume_comma() {
                        args.push(self.parse_expr()?);
                    }
                    if !self.consume_kind(TokenKind::CloseParen) {
                        return Err(ParseError {
                            message: "Missing ')' in function call".to_string(),
                            span: self.current_span(),
                        });
                    }
                }
                let end_span = self.prev_span();
                Ok(Expr::Call {
                    name: format!(".{name}"),
                    args,
                    span: Span {
                        line: dot_span.line,
                        col_start: dot_span.col_start,
                        col_end: end_span.col_end,
                    },
                })
            }
            Some(Token {
                kind: TokenKind::OpenParen,
                span: _open_span,
            }) => {
                let expr = self.parse_expr()?;
                if self.consume_comma() {
                    return Err(ParseError {
                        message: "Unexpected token in expression".to_string(),
                        span: self.prev_span(),
                    });
                }
                if !self.consume_kind(TokenKind::CloseParen) {
                    return Err(ParseError {
                        message: "Missing ')'".to_string(),
                        span: self.current_span(),
                    });
                }
                Ok(expr)
            }
            Some(Token {
                kind: TokenKind::OpenBracket,
                span,
            }) => Err(ParseError {
                message: "Unexpected token in expression".to_string(),
                span,
            }),
            Some(Token {
                kind: TokenKind::OpenBrace,
                span: open_span,
            }) => {
                let mut elements = Vec::new();
                if !self.consume_kind(TokenKind::CloseBrace) {
                    elements.push(self.parse_expr()?);
                    while self.consume_comma() {
                        elements.push(self.parse_expr()?);
                    }
                    if !self.consume_kind(TokenKind::CloseBrace) {
                        return Err(ParseError {
                            message: "Missing '}' in list literal".to_string(),
                            span: self.current_span(),
                        });
                    }
                }
                let close_span = self.prev_span();
                Ok(Expr::List(
                    elements,
                    Span {
                        line: open_span.line,
                        col_start: open_span.col_start,
                        col_end: close_span.col_end,
                    },
                ))
            }
            Some(token) => Err(ParseError {
                message: "Unexpected token in expression".to_string(),
                span: token.span,
            }),
            None => Err(ParseError {
                message: match self.end_token_text.as_deref() {
                    Some(token) => format!("Expected label or numeric constant, found: {token}"),
                    None => "Unexpected end of expression".to_string(),
                },
                span: self.end_span,
            }),
        }?;

        let base = self.parse_struct_literal_if_present(base)?;
        self.parse_postfix_expr(base)
    }

    fn parse_struct_literal_if_present(&mut self, expr: Expr) -> Result<Expr, ParseError> {
        let (type_name, type_span) = match &expr {
            Expr::Identifier(name, span) | Expr::Register(name, span) => (name.clone(), *span),
            _ => return Ok(expr),
        };
        if !self.peek_kind(TokenKind::OpenBrace) {
            return Ok(expr);
        }
        self.index += 1; // '{'

        let mut fields = Vec::new();
        if !self.consume_kind(TokenKind::CloseBrace) {
            loop {
                let field_name = match self.next() {
                    Some(Token {
                        kind: TokenKind::Identifier(name),
                        ..
                    })
                    | Some(Token {
                        kind: TokenKind::Register(name),
                        ..
                    }) => name,
                    Some(token) => {
                        return Err(ParseError {
                            message: "Expected field name in struct literal".to_string(),
                            span: token.span,
                        })
                    }
                    None => {
                        return Err(ParseError {
                            message: "Expected field name in struct literal".to_string(),
                            span: self.end_span,
                        })
                    }
                };

                if !self.consume_kind(TokenKind::Colon) {
                    return Err(ParseError {
                        message: "Expected ':' after field name in struct literal".to_string(),
                        span: self.current_span(),
                    });
                }
                let field_expr = self.parse_expr()?;
                fields.push((field_name, field_expr));

                if self.consume_comma() {
                    continue;
                }
                if !self.consume_kind(TokenKind::CloseBrace) {
                    return Err(ParseError {
                        message: "Missing '}' in struct literal".to_string(),
                        span: self.current_span(),
                    });
                }
                break;
            }
        }

        let close_span = self.prev_span();
        Ok(Expr::StructLiteral {
            type_name,
            fields,
            span: Span {
                line: type_span.line,
                col_start: type_span.col_start,
                col_end: close_span.col_end,
            },
        })
    }

    fn parse_postfix_expr(&mut self, mut expr: Expr) -> Result<Expr, ParseError> {
        loop {
            if self.consume_kind(TokenKind::OpenBracket) {
                let index = self.parse_expr()?;
                let close_span = self.current_span();
                if !self.consume_kind(TokenKind::CloseBracket) {
                    return Err(ParseError {
                        message: "Missing ']' in index expression".to_string(),
                        span: self.current_span(),
                    });
                }
                let start_span = span_of_expr(&expr);
                expr = Expr::Index {
                    base: Box::new(expr),
                    index: Box::new(index),
                    span: Span {
                        line: start_span.line,
                        col_start: start_span.col_start,
                        col_end: close_span.col_end,
                    },
                };
                continue;
            }

            if self.consume_kind(TokenKind::Dot) {
                let (field, field_span) = match self.next() {
                    Some(Token {
                        kind: TokenKind::Identifier(name),
                        span,
                    })
                    | Some(Token {
                        kind: TokenKind::Register(name),
                        span,
                    }) => (name, span),
                    Some(token) => {
                        return Err(ParseError {
                            message: "Expected member name after '.'".to_string(),
                            span: token.span,
                        })
                    }
                    None => {
                        return Err(ParseError {
                            message: "Expected member name after '.'".to_string(),
                            span: self.end_span,
                        })
                    }
                };
                let start_span = span_of_expr(&expr);
                expr = Expr::Member {
                    base: Box::new(expr),
                    field,
                    span: Span {
                        line: start_span.line,
                        col_start: start_span.col_start,
                        col_end: field_span.col_end,
                    },
                };
                continue;
            }

            if let Some(Token {
                kind: TokenKind::Operator(OperatorKind::Plus),
                span: plus_span,
            }) = self.peek().cloned()
            {
                let start_span = span_of_expr(&expr);
                if matches!(expr, Expr::Indirect(_, _)) && plus_span.col_start == start_span.col_end
                {
                    self.index += 1;
                    expr = Expr::Unary {
                        op: UnaryOp::Plus,
                        expr: Box::new(expr),
                        span: Span {
                            line: start_span.line,
                            col_start: start_span.col_start,
                            col_end: plus_span.col_end,
                        },
                    };
                    continue;
                }
            }

            break;
        }
        Ok(expr)
    }

    fn consume_comma(&mut self) -> bool {
        self.consume_kind(TokenKind::Comma)
    }

    fn consume_kind(&mut self, kind: TokenKind) -> bool {
        if let Some(token) = self.peek() {
            if token.kind == kind {
                self.index += 1;
                return true;
            }
        }
        false
    }

    fn match_operator(&mut self, op: OperatorKind) -> bool {
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Operator(op) {
                self.index += 1;
                return true;
            }
        }
        false
    }

    fn peek_operator_kind(&self) -> Option<OperatorKind> {
        if let Some(token) = self.peek() {
            if let TokenKind::Operator(op) = token.kind {
                return Some(op);
            }
        }
        None
    }

    fn next(&mut self) -> Option<Token> {
        if self.index >= self.tokens.len() {
            None
        } else {
            let token = self.tokens[self.index].clone();
            self.index += 1;
            Some(token)
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn prev_span(&self) -> Span {
        if self.index == 0 {
            Span {
                line: 0,
                col_start: 0,
                col_end: 0,
            }
        } else {
            self.tokens[self.index - 1].span
        }
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.index)
            .map(|t| t.span)
            .unwrap_or(self.end_span)
    }
}

fn map_tokenize_error(err: TokenizeError) -> ParseError {
    ParseError {
        message: err.message,
        span: err.span,
    }
}

fn span_of_expr(expr: &Expr) -> Span {
    match expr {
        Expr::Number(_, span)
        | Expr::Identifier(_, span)
        | Expr::Register(_, span)
        | Expr::List(_, span)
        | Expr::Index { span, .. }
        | Expr::Member { span, .. }
        | Expr::StructLiteral { span, .. }
        | Expr::Call { span, .. }
        | Expr::Placeholder(span)
        | Expr::Indirect(_, span)
        | Expr::Immediate(_, span)
        | Expr::IndirectLong(_, span)
        | Expr::Tuple(_, span)
        | Expr::Dollar(span)
        | Expr::String(_, span)
        | Expr::Error(_, span)
        | Expr::Range { span, .. } => *span,
        Expr::Ternary { span, .. } | Expr::Unary { span, .. } | Expr::Binary { span, .. } => *span,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        match_statement_signature, select_statement_signature, AssignOp, BinaryOp, ConditionalKind,
        Expr, LineAst, Parser, SignatureAtom, UnaryOp,
    };
    use crate::expression::expr_text;
    use crate::tokenizer::{
        register_checker_from_fn, NumberLiteral, Span, Token, TokenKind, Tokenizer,
    };
    use types::line_ast::{ConditionalAst, PackAst, PlaceAst, UseAst};

    fn tokenize_line(line: &str) -> Vec<crate::tokenizer::Token> {
        let mut tokenizer = Tokenizer::new(line, 1);
        let mut tokens = Vec::new();
        loop {
            let token = tokenizer.next_token().unwrap();
            if matches!(token.kind, crate::tokenizer::TokenKind::End) {
                break;
            }
            tokens.push(token);
        }
        tokens
    }

    fn is_m68k_register(name: &str) -> bool {
        matches!(
            name.to_ascii_uppercase().as_str(),
            "D0" | "D1"
                | "D2"
                | "D3"
                | "D4"
                | "D5"
                | "D6"
                | "D7"
                | "A0"
                | "A1"
                | "A2"
                | "A3"
                | "A4"
                | "A5"
                | "A6"
                | "A7"
                | "SP"
                | "PC"
        )
    }

    #[test]
    fn parser_from_tokens_preserves_end_metadata() {
        let tokens = tokenize_line("LDA #$42");
        let end_span = Span {
            line: 1,
            col_start: 99,
            col_end: 99,
        };
        let parser = Parser::from_tokens(tokens, end_span, Some(";".to_string()));
        assert_eq!(parser.end_span(), end_span);
        assert_eq!(parser.end_token_text(), Some(";"));
    }

    #[test]
    fn parser_from_tokens_matches_line_parse_for_basic_statement() {
        let line = "LABEL: MOV A,B";
        let tokens = tokenize_line(line);
        let mut from_line = Parser::from_line(line, 1).unwrap();
        let expected = from_line.parse_compat_mixed_line().unwrap();
        let mut from_tokens = Parser::from_tokens(
            tokens,
            Span {
                line: 1,
                col_start: line.len() + 1,
                col_end: line.len() + 1,
            },
            None,
        );
        let actual = from_tokens.parse_compat_mixed_line().unwrap();
        assert_eq!(format!("{expected:?}"), format!("{actual:?}"));
    }

    #[test]
    fn parses_label_and_mnemonic() {
        let mut parser = Parser::from_line("LABEL: MOV A,B", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                let label = statement.label.expect("label");
                assert_eq!(label.name, "LABEL");
                assert_eq!(statement.mnemonic.as_deref(), Some("MOV"));
                assert_eq!(statement.operands.len(), 2);
            }
            _ => panic!("Expected statement"),
        }
    }

    #[test]
    fn parses_label_without_colon() {
        let mut parser = Parser::from_line("LABEL MOV A,B", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                let label = statement.label.expect("label");
                assert_eq!(label.name, "LABEL");
                assert_eq!(statement.mnemonic.as_deref(), Some("MOV"));
                assert_eq!(statement.operands.len(), 2);
            }
            _ => panic!("Expected statement"),
        }
    }

    #[test]
    fn parses_label_for_const() {
        let mut parser = Parser::from_line("NAME .const 3", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                let label = statement.label.expect("label");
                assert_eq!(label.name, "NAME");
                assert_eq!(statement.mnemonic.as_deref(), Some(".const"));
                assert_eq!(statement.operands.len(), 1);
            }
            _ => panic!("Expected statement"),
        }
    }

    #[test]
    fn parses_assignment_constant() {
        let mut parser = Parser::from_line("WIDTH = 40", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Assignment(assignment) => {
                assert_eq!(assignment.label.name, "WIDTH");
                assert_eq!(assignment.op, AssignOp::Const);
            }
            _ => panic!("Expected assignment"),
        }
    }

    #[test]
    fn parses_assignment_var() {
        let mut parser = Parser::from_line("var2 := 1", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Assignment(assignment) => {
                assert_eq!(assignment.label.name, "var2");
                assert_eq!(assignment.op, AssignOp::Var);
            }
            _ => panic!("Expected assignment"),
        }
    }

    #[test]
    fn parses_conditionals() {
        let mut parser = Parser::from_line(".if 1", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Conditional(ConditionalAst { kind, exprs, .. }) => {
                assert_eq!(kind, ConditionalKind::If);
                assert_eq!(exprs.len(), 1);
            }
            _ => panic!("Expected conditional"),
        }
    }

    #[test]
    fn parses_switch_case_list() {
        let mut parser = Parser::from_line(".case 1, 2, 3", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Conditional(ConditionalAst { kind, exprs, .. }) => {
                assert_eq!(kind, ConditionalKind::Case);
                assert_eq!(exprs.len(), 3);
            }
            _ => panic!("Expected conditional"),
        }
    }

    #[test]
    fn parses_operand_list() {
        let mut parser = Parser::from_line("    DB 1, 2, 3", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.operands.len(), 3);
            }
            _ => panic!("Expected statement"),
        }
    }

    #[test]
    fn parses_68k_postincrement_operand() {
        let mut parser = Parser::from_line_with_registers(
            "    MOVE (A0)+,D0",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some("MOVE"));
                match &statement.operands[0] {
                    Expr::Unary { op, expr, .. } => {
                        assert_eq!(*op, UnaryOp::Plus);
                        assert!(matches!(
                            expr.as_ref(),
                            Expr::Indirect(inner, _)
                                if matches!(inner.as_ref(), Expr::Register(name, _) if name == "A0")
                        ));
                    }
                    other => panic!("expected postincrement operand, got {other:?}"),
                }
                assert!(matches!(&statement.operands[1], Expr::Register(name, _) if name == "D0"));
            }
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_68k_extended_short_branch_mnemonic() {
        let mut parser = Parser::from_line_with_registers(
            "    BRA.S+ $0082",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some("BRA.S+"));
                assert_eq!(statement.operands.len(), 1);
            }
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_68k_displacement_operand() {
        let mut parser = Parser::from_line_with_registers(
            "    MOVE 4(A0),D0",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => match &statement.operands[0] {
                Expr::Indirect(inner, _) => match inner.as_ref() {
                    Expr::Tuple(elements, _) => {
                        assert_eq!(elements.len(), 2);
                        assert!(matches!(&elements[0], Expr::Number(text, _) if text == "4"));
                        assert!(matches!(&elements[1], Expr::Register(name, _) if name == "A0"));
                    }
                    other => panic!("expected postfix tuple, got {other:?}"),
                },
                other => panic!("expected indirect operand, got {other:?}"),
            },
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_68k_indexed_operand() {
        let mut parser = Parser::from_line_with_registers(
            "    MOVE 4(A0,D1.W),D0",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => match &statement.operands[0] {
                Expr::Indirect(inner, _) => match inner.as_ref() {
                    Expr::Tuple(elements, _) => {
                        assert_eq!(elements.len(), 3);
                        assert!(matches!(&elements[0], Expr::Number(text, _) if text == "4"));
                        assert!(matches!(&elements[1], Expr::Register(name, _) if name == "A0"));
                        assert!(matches!(
                            &elements[2],
                            Expr::Identifier(text, _) if text.eq_ignore_ascii_case("D1.W")
                        ));
                    }
                    other => panic!("expected indexed tuple, got {other:?}"),
                },
                other => panic!("expected indirect operand, got {other:?}"),
            },
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_68k_indexed_identity_scale_alias_operand() {
        let mut parser = Parser::from_line_with_registers(
            "    MOVE 4(A0,D1.W*1),D0",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => match &statement.operands[0] {
                Expr::Indirect(inner, _) => match inner.as_ref() {
                    Expr::Tuple(elements, _) => {
                        assert_eq!(elements.len(), 3);
                        assert!(matches!(&elements[0], Expr::Number(text, _) if text == "4"));
                        assert!(matches!(&elements[1], Expr::Register(name, _) if name == "A0"));
                        assert!(matches!(
                            &elements[2],
                            Expr::Binary {
                                op: BinaryOp::Multiply,
                                left,
                                right,
                                ..
                            }
                                if matches!(left.as_ref(), Expr::Identifier(text, _) if text.eq_ignore_ascii_case("D1.W"))
                                    && matches!(right.as_ref(), Expr::Number(text, _) if text == "1")
                        ));
                    }
                    other => panic!("expected indexed tuple, got {other:?}"),
                },
                other => panic!("expected indirect operand, got {other:?}"),
            },
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_68k_full_extension_tuple_with_omitted_base_displacement() {
        let mut parser = Parser::from_line_with_registers(
            "    MOVE (,A0,D1.L*4),D0",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => match &statement.operands[0] {
                Expr::Indirect(inner, _) => match inner.as_ref() {
                    Expr::Tuple(elements, _) => {
                        assert_eq!(elements.len(), 3);
                        assert!(matches!(&elements[0], Expr::Placeholder(_)));
                        assert!(matches!(&elements[1], Expr::Register(name, _) if name == "A0"));
                        assert!(matches!(
                            &elements[2],
                            Expr::Binary {
                                op: BinaryOp::Multiply,
                                left,
                                right,
                                ..
                            }
                                if matches!(left.as_ref(), Expr::Identifier(text, _) if text.eq_ignore_ascii_case("D1.L"))
                                    && matches!(right.as_ref(), Expr::Number(text, _) if text == "4")
                        ));
                    }
                    other => panic!("expected full-extension tuple, got {other:?}"),
                },
                other => panic!("expected indirect operand, got {other:?}"),
            },
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_68k_full_extension_tuple_with_base_and_index_suppression() {
        let mut parser = Parser::from_line_with_registers(
            "    MOVE (4.W,,),D0",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => match &statement.operands[0] {
                Expr::Indirect(inner, _) => match inner.as_ref() {
                    Expr::Tuple(elements, _) => {
                        assert_eq!(elements.len(), 3);
                        assert!(matches!(
                            &elements[0],
                            Expr::Member { field, .. } if field.eq_ignore_ascii_case("W")
                        ));
                        assert!(matches!(&elements[1], Expr::Placeholder(_)));
                        assert!(matches!(&elements[2], Expr::Placeholder(_)));
                    }
                    other => panic!("expected full-extension tuple, got {other:?}"),
                },
                other => panic!("expected indirect operand, got {other:?}"),
            },
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_68k_preindexed_memory_indirect_operand() {
        let mut parser = Parser::from_line_with_registers(
            "    MOVE ([disp.W,PC,D4.L*8],outer.L),D0",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => match &statement.operands[0] {
                Expr::Indirect(inner, _) => match inner.as_ref() {
                    Expr::Tuple(elements, _) => {
                        assert_eq!(elements.len(), 2);
                        match &elements[0] {
                            Expr::IndirectLong(bracketed, _) => match bracketed.as_ref() {
                                Expr::Tuple(inner_elements, _) => {
                                    assert_eq!(inner_elements.len(), 3);
                                    assert_eq!(
                                        expr_text(&inner_elements[0]).as_deref(),
                                        Some("disp.W")
                                    );
                                    assert!(matches!(
                                        &inner_elements[1],
                                        Expr::Register(name, _) if name == "PC"
                                    ));
                                    assert!(matches!(
                                        &inner_elements[2],
                                        Expr::Binary {
                                            op: BinaryOp::Multiply,
                                            left,
                                            right,
                                            ..
                                        }
                                            if matches!(left.as_ref(), Expr::Identifier(text, _) if text.eq_ignore_ascii_case("D4.L"))
                                                && matches!(right.as_ref(), Expr::Number(text, _) if text == "8")
                                    ));
                                }
                                other => panic!("expected bracketed tuple, got {other:?}"),
                            },
                            other => panic!("expected bracketed indirect-long, got {other:?}"),
                        }
                        assert_eq!(expr_text(&elements[1]).as_deref(), Some("outer.L"));
                    }
                    other => panic!("expected outer tuple, got {other:?}"),
                },
                other => panic!("expected indirect operand, got {other:?}"),
            },
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_68k_immediate_member_suffix_expression() {
        let mut parser = Parser::from_line_with_registers(
            "    FADD #1.d,FP0",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => match &statement.operands[0] {
                Expr::Immediate(inner, _) => match inner.as_ref() {
                    Expr::Member { base, field, .. } => {
                        assert_eq!(field, "d");
                        assert!(matches!(base.as_ref(), Expr::Number(text, _) if text == "1"));
                    }
                    other => panic!("expected immediate member suffix, got {other:?}"),
                },
                other => panic!("expected immediate operand, got {other:?}"),
            },
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_68k_postindexed_memory_indirect_operand_with_omissions() {
        let mut parser = Parser::from_line_with_registers(
            "    MOVE ([disp.L,A3],,outer.W),D0",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => match &statement.operands[0] {
                Expr::Indirect(inner, _) => match inner.as_ref() {
                    Expr::Tuple(elements, _) => {
                        assert_eq!(elements.len(), 3);
                        match &elements[0] {
                            Expr::IndirectLong(bracketed, _) => match bracketed.as_ref() {
                                Expr::Tuple(inner_elements, _) => {
                                    assert_eq!(inner_elements.len(), 2);
                                    assert_eq!(
                                        expr_text(&inner_elements[0]).as_deref(),
                                        Some("disp.L")
                                    );
                                    assert!(matches!(
                                        &inner_elements[1],
                                        Expr::Register(name, _) if name == "A3"
                                    ));
                                }
                                other => panic!("expected bracketed tuple, got {other:?}"),
                            },
                            other => panic!("expected bracketed indirect-long, got {other:?}"),
                        }
                        assert!(matches!(&elements[1], Expr::Placeholder(_)));
                        assert_eq!(expr_text(&elements[2]).as_deref(), Some("outer.W"));
                    }
                    other => panic!("expected outer tuple, got {other:?}"),
                },
                other => panic!("expected indirect operand, got {other:?}"),
            },
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_dot_directive_statement() {
        let mut parser = Parser::from_line("    .byte 1, 2", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some(".byte"));
                assert_eq!(statement.operands.len(), 2);
            }
            _ => panic!("Expected statement"),
        }
    }

    #[test]
    fn parses_place_directive() {
        let mut parser = Parser::from_line(".place code in ram, align=2", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Place(PlaceAst {
                section,
                region,
                align,
                ..
            }) => {
                assert_eq!(section, "code");
                assert_eq!(region, "ram");
                assert!(align.is_some());
            }
            _ => panic!("Expected place directive"),
        }
    }

    #[test]
    fn parses_pack_directive() {
        let mut parser = Parser::from_line(".pack in ram : code, data", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Pack(PackAst {
                region, sections, ..
            }) => {
                assert_eq!(region, "ram");
                assert_eq!(sections, vec!["code".to_string(), "data".to_string()]);
            }
            _ => panic!("Expected pack directive"),
        }
    }

    #[test]
    fn parses_use_basic() {
        let mut parser = Parser::from_line(".use std.math", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Use(UseAst {
                module_id,
                alias,
                items,
                params,
                ..
            }) => {
                assert_eq!(module_id, "std.math");
                assert!(alias.is_none());
                assert!(items.is_empty());
                assert!(params.is_empty());
            }
            _ => panic!("Expected use directive"),
        }
    }

    #[test]
    fn parses_use_with_alias_selective_params() {
        let mut parser =
            Parser::from_line(".use std.math (add16, sub16 as sub) with (FEATURE=1)", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Use(UseAst {
                module_id,
                alias,
                items,
                params,
                ..
            }) => {
                assert_eq!(module_id, "std.math");
                assert!(alias.is_none());
                assert_eq!(items.len(), 2);
                assert_eq!(items[0].name, "add16");
                assert_eq!(items[1].alias.as_deref(), Some("sub"));
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].name, "FEATURE");
            }
            _ => panic!("Expected use directive"),
        }
    }

    #[test]
    fn parses_use_with_selection_alias_and_section_map() {
        let mut parser =
            Parser::from_line(".use std.math (add16) as M map { code -> app_code }", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Use(UseAst {
                alias,
                items,
                section_maps,
                ..
            }) => {
                assert_eq!(alias.as_deref(), Some("M"));
                assert_eq!(items[0].name, "add16");
                assert_eq!(section_maps.len(), 1);
                assert_eq!(section_maps[0].logical, "code");
                assert_eq!(section_maps[0].concrete, "app_code");
            }
            _ => panic!("Expected use directive"),
        }
    }

    #[test]
    fn rejects_alias_before_selection_for_use() {
        let mut parser = Parser::from_line(".use std.math as M (add16)", 1).unwrap();
        assert!(parser.parse_compat_mixed_line().is_err());
    }

    #[test]
    fn rejects_qualified_selective_item_alias() {
        let mut parser = Parser::from_line(".use std.math (add16 as add) as M", 1).unwrap();
        assert!(parser.parse_compat_mixed_line().is_err());
    }

    #[test]
    fn rejects_section_map_without_namespace_binding() {
        let mut parser =
            Parser::from_line(".use std.math (add16) map { code -> app_code }", 1).unwrap();
        assert!(parser.parse_compat_mixed_line().is_err());
    }

    #[test]
    fn rejects_empty_selective_list() {
        let mut parser = Parser::from_line(".use std.math ()", 1).unwrap();
        assert!(parser.parse_compat_mixed_line().is_err());
    }

    #[test]
    fn parses_use_wildcard_selective_list() {
        let mut parser = Parser::from_line(".use std.math (*)", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Use(UseAst { items, .. }) => {
                assert_eq!(items.len(), 1);
                assert_eq!(items[0].name, "*");
                assert!(items[0].alias.is_none());
            }
            _ => panic!("Expected use directive"),
        }
    }

    #[test]
    fn rejects_wildcard_with_alias_in_selective_list() {
        let mut parser = Parser::from_line(".use std.math (* as all)", 1).unwrap();
        assert!(parser.parse_compat_mixed_line().is_err());
    }

    #[test]
    fn parses_macro_directive_line_without_error() {
        let mut parser = Parser::from_line(".macro COPY(src, dst)", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some(".macro"));
            }
            _ => panic!("Expected statement"),
        }
    }

    #[test]
    fn parses_name_first_macro_definition_without_error() {
        let mut parser = Parser::from_line("COPY .macro src, dst", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.label.map(|l| l.name), Some("COPY".to_string()));
                assert_eq!(statement.mnemonic.as_deref(), Some(".macro"));
            }
            _ => panic!("Expected statement"),
        }
    }

    #[test]
    fn parses_segment_directive_line_without_error() {
        let mut parser = Parser::from_line(".segment INLINE(val)", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some(".segment"));
            }
            _ => panic!("Expected statement"),
        }
    }

    #[test]
    fn parses_statement_definition_with_signature() {
        let mut parser = Parser::from_line(".statement move.b char:dst \",\" char:src", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::StatementDef(def) => {
                assert_eq!(def.keyword, "move.b");
                assert_eq!(def.signature.atoms.len(), 3);
                assert!(matches!(
                    def.signature.atoms[0],
                    SignatureAtom::Capture { .. }
                ));
                assert!(matches!(
                    def.signature.atoms[1],
                    SignatureAtom::Literal(_, _)
                ));
                assert!(matches!(
                    def.signature.atoms[2],
                    SignatureAtom::Capture { .. }
                ));
            }
            _ => panic!("Expected statement definition"),
        }
    }

    #[test]
    fn parses_statement_boundary_span() {
        let mut parser =
            Parser::from_line(".statement sta \"[\" byte:a \",\"[{char:reg}]", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::StatementDef(def) => {
                assert_eq!(def.signature.atoms.len(), 4);
                assert!(matches!(
                    def.signature.atoms[0],
                    SignatureAtom::Literal(_, _)
                ));
                assert!(matches!(
                    def.signature.atoms[1],
                    SignatureAtom::Capture { .. }
                ));
                assert!(matches!(
                    def.signature.atoms[2],
                    SignatureAtom::Literal(_, _)
                ));
                assert!(matches!(
                    def.signature.atoms[3],
                    SignatureAtom::Boundary { .. }
                ));
            }
            _ => panic!("Expected statement definition"),
        }
    }

    #[test]
    fn matches_statement_signature_literal_sequence() {
        let mut sig_parser = Parser::from_line(".statement sta \"],y\"", 1).unwrap();
        let signature = match sig_parser.parse_compat_mixed_line().unwrap() {
            LineAst::StatementDef(def) => def.signature,
            _ => panic!("Expected statement definition"),
        };
        assert_eq!(signature.atoms.len(), 1);
        match &signature.atoms[0] {
            SignatureAtom::Literal(bytes, _) => {
                assert_eq!(String::from_utf8_lossy(bytes), "],y");
            }
            _ => panic!("Expected literal atom"),
        }

        let mut tokenizer = Tokenizer::new("],y", 1);
        let mut tokens = Vec::new();
        loop {
            let token = tokenizer.next_token().unwrap();
            if matches!(token.kind, crate::tokenizer::TokenKind::End) {
                break;
            }
            tokens.push(token);
        }
        assert!(match_statement_signature(&signature, &tokens).is_some());
    }

    #[test]
    fn statement_signature_precedence_prefers_more_literals() {
        let mut parser1 = Parser::from_line(".statement foo \"x\" byte:a", 1).unwrap();
        let sig1 = match parser1.parse_compat_mixed_line().unwrap() {
            LineAst::StatementDef(def) => def.signature,
            _ => panic!("Expected statement definition"),
        };
        assert_eq!(sig1.atoms.len(), 2);
        assert!(matches!(sig1.atoms[0], SignatureAtom::Literal(_, _)));
        assert!(matches!(sig1.atoms[1], SignatureAtom::Capture { .. }));

        let mut parser2 = Parser::from_line(".statement foo byte:a", 1).unwrap();
        let sig2 = match parser2.parse_compat_mixed_line().unwrap() {
            LineAst::StatementDef(def) => def.signature,
            _ => panic!("Expected statement definition"),
        };
        assert_eq!(sig2.atoms.len(), 1);
        assert!(matches!(sig2.atoms[0], SignatureAtom::Capture { .. }));

        let mut tokenizer = Tokenizer::new("x 10", 1);
        let mut tokens = Vec::new();
        loop {
            let token = tokenizer.next_token().unwrap();
            if matches!(token.kind, crate::tokenizer::TokenKind::End) {
                break;
            }
            tokens.push(token);
        }

        let idx = select_statement_signature(&[sig1, sig2], &tokens)
            .expect("select")
            .expect("match");
        assert_eq!(idx, 0);
    }

    #[test]
    fn statement_signature_byte_capture_rejects_out_of_range() {
        let mut parser = Parser::from_line(".statement foo byte:a", 1).unwrap();
        let signature = match parser.parse_compat_mixed_line().unwrap() {
            LineAst::StatementDef(def) => def.signature,
            _ => panic!("Expected statement definition"),
        };

        let ok_tokens = tokenize_line("255");
        assert!(match_statement_signature(&signature, &ok_tokens).is_some());

        let bad_tokens = tokenize_line("256");
        assert!(match_statement_signature(&signature, &bad_tokens).is_none());

        let label_tokens = tokenize_line("LABEL");
        assert!(match_statement_signature(&signature, &label_tokens).is_some());
    }

    #[test]
    fn statement_signature_word_capture_rejects_out_of_range() {
        let mut parser = Parser::from_line(".statement foo word:a", 1).unwrap();
        let signature = match parser.parse_compat_mixed_line().unwrap() {
            LineAst::StatementDef(def) => def.signature,
            _ => panic!("Expected statement definition"),
        };

        let ok_tokens = tokenize_line("65535");
        assert!(match_statement_signature(&signature, &ok_tokens).is_some());

        let bad_tokens = tokenize_line("65536");
        assert!(match_statement_signature(&signature, &bad_tokens).is_none());

        let str_tokens = tokenize_line("\"AB\"");
        assert!(match_statement_signature(&signature, &str_tokens).is_some());
    }

    #[test]
    fn statement_signature_long_capture_rejects_out_of_range_and_strings() {
        let mut parser = Parser::from_line(".statement foo long:a", 1).unwrap();
        let signature = match parser.parse_compat_mixed_line().unwrap() {
            LineAst::StatementDef(def) => def.signature,
            _ => panic!("Expected statement definition"),
        };

        let ok_tokens = tokenize_line("4294967295");
        assert!(match_statement_signature(&signature, &ok_tokens).is_some());

        let bad_tokens = tokenize_line("4294967296");
        assert!(match_statement_signature(&signature, &bad_tokens).is_none());

        let ok_negative_tokens = vec![Token {
            kind: TokenKind::Number(NumberLiteral {
                text: "-2147483648".to_string(),
                base: 10,
            }),
            span: Span {
                line: 1,
                col_start: 1,
                col_end: 11,
            },
        }];
        assert!(match_statement_signature(&signature, &ok_negative_tokens).is_some());

        let bad_negative_tokens = vec![Token {
            kind: TokenKind::Number(NumberLiteral {
                text: "-2147483649".to_string(),
                base: 10,
            }),
            span: Span {
                line: 1,
                col_start: 1,
                col_end: 11,
            },
        }];
        assert!(match_statement_signature(&signature, &bad_negative_tokens).is_none());

        let label_tokens = tokenize_line("LABEL");
        assert!(match_statement_signature(&signature, &label_tokens).is_some());

        let str_tokens = tokenize_line("\"ABCD\"");
        assert!(match_statement_signature(&signature, &str_tokens).is_none());
    }

    #[test]
    fn statement_signature_char_capture_requires_single_char() {
        let mut parser = Parser::from_line(".statement foo char:c", 1).unwrap();
        let signature = match parser.parse_compat_mixed_line().unwrap() {
            LineAst::StatementDef(def) => def.signature,
            _ => panic!("Expected statement definition"),
        };

        let ok_tokens = tokenize_line("y");
        assert!(match_statement_signature(&signature, &ok_tokens).is_some());

        let bad_tokens = tokenize_line("yy");
        assert!(match_statement_signature(&signature, &bad_tokens).is_none());

        let str_tokens = tokenize_line("\"A\"");
        assert!(match_statement_signature(&signature, &str_tokens).is_some());

        let long_str_tokens = tokenize_line("\"AB\"");
        assert!(match_statement_signature(&signature, &long_str_tokens).is_none());
    }

    #[test]
    fn statement_signature_str_capture_requires_string_literal() {
        let mut parser = Parser::from_line(".statement foo str:s", 1).unwrap();
        let signature = match parser.parse_compat_mixed_line().unwrap() {
            LineAst::StatementDef(def) => def.signature,
            _ => panic!("Expected statement definition"),
        };

        let ok_tokens = tokenize_line("\"hello\"");
        assert!(match_statement_signature(&signature, &ok_tokens).is_some());

        let bad_tokens = tokenize_line("hello");
        assert!(match_statement_signature(&signature, &bad_tokens).is_none());
    }

    #[test]
    fn statement_signature_rejects_unknown_capture_type() {
        let mut parser = Parser::from_line(".statement move reg:dst", 1).unwrap();
        let err = parser
            .parse_compat_mixed_line()
            .expect_err("expected error");
        assert!(err.message.contains("Unknown statement capture type"));
    }

    #[test]
    fn statement_signature_rejects_unquoted_commas() {
        let mut parser = Parser::from_line(".statement move.b char:dst, char:src", 1).unwrap();
        let err = parser
            .parse_compat_mixed_line()
            .expect_err("expected error");
        assert!(err
            .message
            .contains("Commas must be quoted in statement signatures"));
    }

    #[test]
    fn statement_signature_selection_reports_ambiguity() {
        let mut parser1 = Parser::from_line(".statement foo byte:a", 1).unwrap();
        let sig1 = match parser1.parse_compat_mixed_line().unwrap() {
            LineAst::StatementDef(def) => def.signature,
            _ => panic!("Expected statement definition"),
        };

        let mut parser2 = Parser::from_line(".statement foo word:b", 1).unwrap();
        let sig2 = match parser2.parse_compat_mixed_line().unwrap() {
            LineAst::StatementDef(def) => def.signature,
            _ => panic!("Expected statement definition"),
        };

        let mut tokenizer = Tokenizer::new("10", 1);
        let mut tokens = Vec::new();
        loop {
            let token = tokenizer.next_token().unwrap();
            if matches!(token.kind, crate::tokenizer::TokenKind::End) {
                break;
            }
            tokens.push(token);
        }

        let err = select_statement_signature(&[sig1, sig2], &tokens)
            .expect_err("expected ambiguity error");
        assert_eq!(err.message, "Ambiguous statement signature");
        assert_eq!(err.span.line, 1);
        assert_eq!(err.span.col_start, 1);
    }

    #[test]
    fn parses_endstatement_line() {
        let mut parser = Parser::from_line(".endstatement", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::StatementEnd(..) => {}
            _ => panic!("Expected statement end"),
        }
    }

    #[test]
    fn parses_star_org_assignment() {
        let mut parser = Parser::from_line("* = $1000", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some(".org"));
                assert_eq!(statement.operands.len(), 1);
            }
            _ => panic!("Expected statement"),
        }
    }

    #[test]
    fn classic_parser_parentheses_group_scalar_expression() {
        let expr = Parser::parse_expr_from_tokens(
            tokenize_line("(1+2)"),
            Span {
                line: 1,
                col_start: 6,
                col_end: 6,
            },
            None,
        )
        .expect("grouped scalar expression should parse");

        match expr {
            Expr::Binary {
                op, left, right, ..
            } => {
                assert_eq!(op, BinaryOp::Add);
                assert!(matches!(*left, Expr::Number(ref text, _) if text == "1"));
                assert!(matches!(*right, Expr::Number(ref text, _) if text == "2"));
            }
            other => panic!("Expected grouped binary expression, got {other:?}"),
        }
    }

    #[test]
    fn classic_parser_rejects_operand_only_expression_prefixes() {
        let immediate = Parser::parse_expr_from_tokens(
            tokenize_line("#1"),
            Span {
                line: 1,
                col_start: 3,
                col_end: 3,
            },
            None,
        )
        .expect_err("standalone expression should reject immediate prefix");
        assert_eq!(immediate.message, "Unexpected token in expression");
        assert_eq!(immediate.span.col_start, 1);

        let bracket = Parser::parse_expr_from_tokens(
            tokenize_line("[1]"),
            Span {
                line: 1,
                col_start: 4,
                col_end: 4,
            },
            None,
        )
        .expect_err("standalone expression should reject indirect-long wrapper");
        assert_eq!(bracket.message, "Unexpected token in expression");
        assert_eq!(bracket.span.col_start, 1);
    }

    #[test]
    fn dot_statement_operand_parentheses_group_inside_expression() {
        let mut parser = Parser::from_line("    .byte (1) & $ff", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => match &statement.operands[0] {
                Expr::Binary {
                    op, left, right, ..
                } => {
                    assert_eq!(*op, BinaryOp::BitAnd);
                    assert!(matches!(left.as_ref(), Expr::Number(text, _) if text == "1"));
                    assert!(matches!(right.as_ref(), Expr::Number(text, _) if text == "$ff"));
                }
                other => panic!("Expected grouped binary operand, got {other:?}"),
            },
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn statement_operand_whole_parentheses_remains_indirect() {
        let mut parser = Parser::from_line_with_registers(
            "    MOVE (A0),D0",
            1,
            register_checker_from_fn(is_m68k_register),
        )
        .unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => match &statement.operands[0] {
                Expr::Indirect(inner, _) => {
                    assert!(matches!(inner.as_ref(), Expr::Register(name, _) if name == "A0"));
                }
                other => panic!("Expected indirect operand, got {other:?}"),
            },
            other => panic!("Expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_range_expression() {
        let expr = Parser::parse_expr_from_tokens(
            tokenize_line("0..8"),
            Span {
                line: 1,
                col_start: 5,
                col_end: 5,
            },
            None,
        )
        .expect("range expression should parse");

        match expr {
            Expr::Range {
                inclusive,
                step,
                start,
                end,
                ..
            } => {
                assert!(!inclusive);
                assert!(step.is_none());
                assert!(matches!(*start, Expr::Number(_, _)));
                assert!(matches!(*end, Expr::Number(_, _)));
            }
            other => panic!("Expected range expression, got {other:?}"),
        }
    }

    #[test]
    fn parses_inclusive_range_with_step_expression() {
        let expr = Parser::parse_expr_from_tokens(
            tokenize_line("10..=0:-1"),
            Span {
                line: 1,
                col_start: 10,
                col_end: 10,
            },
            None,
        )
        .expect("inclusive range with step should parse");

        match expr {
            Expr::Range {
                inclusive,
                step,
                start,
                end,
                ..
            } => {
                assert!(inclusive);
                assert!(step.is_some());
                assert!(matches!(*start, Expr::Number(_, _)));
                assert!(matches!(*end, Expr::Number(_, _)));
            }
            other => panic!("Expected range expression, got {other:?}"),
        }
    }

    #[test]
    fn parses_list_literal_expression() {
        let expr = Parser::parse_expr_from_tokens(
            tokenize_line("{1,2,3}"),
            Span {
                line: 1,
                col_start: 8,
                col_end: 8,
            },
            None,
        )
        .expect("list expression should parse");

        match expr {
            Expr::List(items, _) => {
                assert_eq!(items.len(), 3);
                assert!(items.iter().all(|item| matches!(item, Expr::Number(_, _))));
            }
            other => panic!("Expected list expression, got {other:?}"),
        }
    }

    #[test]
    fn parses_typed_struct_literal_expression() {
        let expr = Parser::parse_expr_from_tokens(
            tokenize_line("Point{x:1,y:2}"),
            Span {
                line: 1,
                col_start: 14,
                col_end: 14,
            },
            None,
        )
        .expect("struct literal expression should parse");

        match expr {
            Expr::StructLiteral {
                type_name, fields, ..
            } => {
                assert_eq!(type_name, "Point");
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0, "x");
                assert!(matches!(fields[0].1, Expr::Number(_, _)));
                assert_eq!(fields[1].0, "y");
                assert!(matches!(fields[1].1, Expr::Number(_, _)));
            }
            other => panic!("Expected struct literal expression, got {other:?}"),
        }
    }

    #[test]
    fn parses_struct_literal_followed_by_member_access() {
        let expr = Parser::parse_expr_from_tokens(
            tokenize_line("Point{x:1,y:2}.x"),
            Span {
                line: 1,
                col_start: 16,
                col_end: 16,
            },
            None,
        )
        .expect("struct literal member expression should parse");

        match expr {
            Expr::Member { base, field, .. } => {
                assert_eq!(field, "x");
                assert!(matches!(*base, Expr::StructLiteral { .. }));
            }
            other => panic!("Expected member expression, got {other:?}"),
        }
    }

    #[test]
    fn parses_index_then_member_expression() {
        let expr = Parser::parse_expr_from_tokens(
            tokenize_line("arr[2].len"),
            Span {
                line: 1,
                col_start: 11,
                col_end: 11,
            },
            None,
        )
        .expect("postfix expression should parse");

        match expr {
            Expr::Member { base, field, .. } => {
                assert_eq!(field, "len");
                assert!(matches!(*base, Expr::Index { .. }));
            }
            other => panic!("Expected member expression, got {other:?}"),
        }
    }

    #[test]
    fn parses_dot_call_with_placeholder_argument() {
        let expr = Parser::parse_expr_from_tokens(
            tokenize_line(".pick({1,2},?)"),
            Span {
                line: 1,
                col_start: 14,
                col_end: 14,
            },
            None,
        )
        .expect("call expression should parse");

        match expr {
            Expr::Call { name, args, .. } => {
                assert_eq!(name, ".pick");
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], Expr::List(_, _)));
                assert!(matches!(args[1], Expr::Placeholder(_)));
            }
            other => panic!("Expected call expression, got {other:?}"),
        }
    }

    #[test]
    fn parses_for_directive_var_in_head() {
        let mut parser = Parser::from_line(".for i in 0..8", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some(".for"));
                assert_eq!(statement.operands.len(), 2);
                assert!(
                    matches!(statement.operands[0], Expr::Identifier(ref name, _) if name == "i")
                );
                assert!(matches!(statement.operands[1], Expr::Range { .. }));
            }
            other => panic!("Expected .for statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_for_directive_count_head() {
        let mut parser = Parser::from_line(".for 4+1", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some(".for"));
                assert_eq!(statement.operands.len(), 1);
                assert!(matches!(statement.operands[0], Expr::Binary { .. }));
            }
            other => panic!("Expected .for statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_while_directive_head() {
        let mut parser = Parser::from_line(".while addr < $c100", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some(".while"));
                assert_eq!(statement.operands.len(), 1);
                assert!(matches!(
                    statement.operands[0],
                    Expr::Binary {
                        op: BinaryOp::Lt,
                        ..
                    }
                ));
            }
            other => panic!("Expected .while statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_endfor_without_operands() {
        let mut parser = Parser::from_line(".endfor", 1).unwrap();
        let line = parser.parse_compat_mixed_line().unwrap();
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some(".endfor"));
                assert!(statement.operands.is_empty());
            }
            other => panic!("Expected .endfor statement, got {other:?}"),
        }
    }

    #[test]
    fn rejects_trailing_tokens_after_endfor() {
        let mut parser = Parser::from_line(".endfor 1", 1).unwrap();
        let err = parser
            .parse_compat_mixed_line()
            .expect_err("trailing tokens after .endfor should fail");
        assert!(err.message.contains("Unexpected trailing tokens"));
    }
}
