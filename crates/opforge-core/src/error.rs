// SPDX-License-Identifier: GPL-3.0-or-later

use crate::expr::EvalError;
use crate::macro_processor::MacroError;
use crate::parser::ParseError;
use crate::preprocess::PreprocessError;
use crate::tokenizer::{Span, TokenizeError};

fn code_for_kind(kind: CoreErrorKind) -> &'static str {
    match kind {
        CoreErrorKind::Tokenize => "opcore.tokenize",
        CoreErrorKind::Parse => "opcore.parse",
        CoreErrorKind::Expr => "opcore.expr",
        CoreErrorKind::Statement => "opcore.statement",
        CoreErrorKind::Module => "opcore.module",
        CoreErrorKind::Use => "opcore.use",
        CoreErrorKind::Import => "opcore.import",
        CoreErrorKind::Macro => "opcore.macro",
        CoreErrorKind::Conditional => "opcore.conditional",
        CoreErrorKind::Repetition => "opcore.repetition",
        CoreErrorKind::Namespace => "opcore.namespace",
        CoreErrorKind::Scope => "opcore.scope",
        CoreErrorKind::Preprocess => "opcore.preprocess",
        CoreErrorKind::Struct => "opcore.struct",
        CoreErrorKind::Segment => "opcore.segment",
    }
}

fn classify_module_item_kind(line: &str, message: &str) -> CoreErrorKind {
    let trimmed = line.trim_start();
    if trimmed.starts_with(".use") {
        if message.contains("import list")
            || message.contains("Wildcard import")
            || message.contains("parameter")
            || message.contains("Expected '(' after 'with'")
        {
            CoreErrorKind::Import
        } else {
            CoreErrorKind::Use
        }
    } else {
        CoreErrorKind::Module
    }
}

fn classify_line_parse_kind(line: &str) -> CoreErrorKind {
    let trimmed = line.trim_start();
    if trimmed.starts_with(".if") || trimmed.starts_with(".endif") {
        CoreErrorKind::Conditional
    } else if trimmed.starts_with(".for")
        || trimmed.starts_with(".bfor")
        || trimmed.starts_with(".endfor")
        || trimmed.starts_with(".while")
        || trimmed.starts_with(".bwhile")
        || trimmed.starts_with(".endwhile")
    {
        CoreErrorKind::Repetition
    } else {
        CoreErrorKind::Statement
    }
}

fn classify_routed_line_parse_kind(line: &str, message: &str) -> CoreErrorKind {
    let trimmed = line.trim_start();
    let lower = trimmed.to_ascii_lowercase();
    let message_lower = message.to_ascii_lowercase();
    if lower.starts_with(".if") || lower.starts_with(".endif") {
        CoreErrorKind::Conditional
    } else if lower.starts_with(".for")
        || lower.starts_with(".bfor")
        || lower.starts_with(".endfor")
        || lower.starts_with(".while")
        || lower.starts_with(".bwhile")
        || lower.starts_with(".endwhile")
    {
        CoreErrorKind::Repetition
    } else if lower.starts_with(".endsegment") || lower.starts_with(".segment") {
        CoreErrorKind::Segment
    } else if lower.starts_with(".endn") || lower.starts_with(".namespace") {
        CoreErrorKind::Namespace
    } else if lower.starts_with(".endblock") || lower.starts_with(".endmodule") {
        CoreErrorKind::Scope
    } else if lower.starts_with(".endmacro") || lower.starts_with(".macro") {
        CoreErrorKind::Macro
    } else if message_lower.contains("struct literal") {
        CoreErrorKind::Struct
    } else {
        CoreErrorKind::Statement
    }
}

fn classify_macro_error_kind(message: &str) -> CoreErrorKind {
    let lower = message.to_ascii_lowercase();
    if lower.contains("segment") || lower.contains(".endsegment") {
        CoreErrorKind::Segment
    } else if lower.contains("namespace") || lower.contains(".endn") {
        CoreErrorKind::Namespace
    } else if lower.contains("scope") || lower.contains(".endblock") || lower.contains(".endmodule")
    {
        CoreErrorKind::Scope
    } else {
        CoreErrorKind::Macro
    }
}

fn classify_parse_error_kind(message: &str) -> CoreErrorKind {
    let lower = message.to_ascii_lowercase();
    if lower.contains("struct literal") {
        CoreErrorKind::Struct
    } else {
        CoreErrorKind::Parse
    }
}

fn classify_eval_error_kind(message: &str) -> CoreErrorKind {
    let lower = message.to_ascii_lowercase();
    if lower.contains("struct") {
        CoreErrorKind::Struct
    } else {
        CoreErrorKind::Expr
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreErrorKind {
    Tokenize,
    Parse,
    Expr,
    Statement,
    Module,
    Use,
    Import,
    Macro,
    Conditional,
    Repetition,
    Namespace,
    Scope,
    Preprocess,
    Struct,
    Segment,
}

#[derive(Debug, Clone)]
pub struct ModuleItemError {
    kind: CoreErrorKind,
    pub message: String,
    pub span: Span,
}

impl ModuleItemError {
    pub fn from_module_item_parse(line: &str, err: ParseError) -> Self {
        Self {
            kind: classify_module_item_kind(line, &err.message),
            message: err.message,
            span: err.span,
        }
    }

    pub fn kind(&self) -> CoreErrorKind {
        self.kind
    }

    pub fn summary(&self) -> &str {
        &self.message
    }

    pub fn code(&self) -> &str {
        code_for_kind(self.kind)
    }

    fn from_kind_and_parse_error(kind: CoreErrorKind, err: ParseError) -> Self {
        Self {
            kind,
            message: err.message,
            span: err.span,
        }
    }
}

impl std::fmt::Display for ModuleItemError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.summary())
    }
}

impl std::error::Error for ModuleItemError {}

#[derive(Debug, Clone)]
pub struct LineParseError {
    kind: CoreErrorKind,
    pub message: String,
    pub span: Span,
}

impl LineParseError {
    pub fn from_line_parse(line: &str, err: ParseError) -> Self {
        Self {
            kind: classify_line_parse_kind(line),
            message: err.message,
            span: err.span,
        }
    }

    pub fn kind(&self) -> CoreErrorKind {
        self.kind
    }

    pub fn summary(&self) -> &str {
        &self.message
    }

    pub fn code(&self) -> &str {
        code_for_kind(self.kind)
    }

    fn from_kind_and_parse_error(kind: CoreErrorKind, err: ParseError) -> Self {
        Self {
            kind,
            message: err.message,
            span: err.span,
        }
    }
}

impl std::fmt::Display for LineParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.summary())
    }
}

impl std::error::Error for LineParseError {}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum CoreError {
    Tokenize(TokenizeError),
    Parse(ParseError),
    Expr(EvalError),
    Macro(MacroError),
    Preprocess(PreprocessError),
    ModuleItem(ModuleItemError),
    LineParse(LineParseError),
}

impl CoreError {
    pub fn kind(&self) -> CoreErrorKind {
        match self {
            Self::Tokenize(_) => CoreErrorKind::Tokenize,
            Self::Parse(err) => classify_parse_error_kind(&err.message),
            Self::Expr(err) => classify_eval_error_kind(&err.message),
            Self::Macro(err) => classify_macro_error_kind(err.message()),
            Self::Preprocess(_) => CoreErrorKind::Preprocess,
            Self::ModuleItem(err) => err.kind(),
            Self::LineParse(err) => err.kind(),
        }
    }

    pub fn summary(&self) -> &str {
        match self {
            Self::Tokenize(err) => &err.message,
            Self::Parse(err) => &err.message,
            Self::Expr(err) => &err.message,
            Self::Macro(err) => err.message(),
            Self::Preprocess(err) => err.message(),
            Self::ModuleItem(err) => err.summary(),
            Self::LineParse(err) => err.summary(),
        }
    }

    pub fn code(&self) -> &str {
        code_for_kind(self.kind())
    }

    pub fn from_statement_parse(line: &str, err: ParseError) -> Self {
        Self::from_routed_parse(classify_routed_line_parse_kind(line, &err.message), err)
    }

    pub fn from_module_item_parse(line: &str, err: ParseError) -> Self {
        Self::from_routed_parse(classify_module_item_kind(line, &err.message), err)
    }

    pub fn from_routed_parse(kind: CoreErrorKind, err: ParseError) -> Self {
        match kind {
            CoreErrorKind::Module | CoreErrorKind::Use | CoreErrorKind::Import => {
                Self::ModuleItem(ModuleItemError::from_kind_and_parse_error(kind, err))
            }
            CoreErrorKind::Statement | CoreErrorKind::Conditional | CoreErrorKind::Repetition => {
                Self::LineParse(LineParseError::from_kind_and_parse_error(kind, err))
            }
            CoreErrorKind::Macro
            | CoreErrorKind::Namespace
            | CoreErrorKind::Scope
            | CoreErrorKind::Segment => Self::Macro(MacroError::new(
                err.message,
                Some(err.span.line),
                Some(err.span.col_start),
            )),
            _ => Self::Parse(err),
        }
    }
}

impl std::fmt::Display for CoreError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.summary())
    }
}

impl std::error::Error for CoreError {}

impl From<TokenizeError> for CoreError {
    fn from(err: TokenizeError) -> Self {
        Self::Tokenize(err)
    }
}

impl From<ParseError> for CoreError {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

impl From<EvalError> for CoreError {
    fn from(err: EvalError) -> Self {
        Self::Expr(err)
    }
}

impl From<MacroError> for CoreError {
    fn from(err: MacroError) -> Self {
        Self::Macro(err)
    }
}

impl From<PreprocessError> for CoreError {
    fn from(err: PreprocessError) -> Self {
        Self::Preprocess(err)
    }
}

impl From<ModuleItemError> for CoreError {
    fn from(err: ModuleItemError) -> Self {
        Self::ModuleItem(err)
    }
}

impl From<LineParseError> for CoreError {
    fn from(err: LineParseError) -> Self {
        Self::LineParse(err)
    }
}
