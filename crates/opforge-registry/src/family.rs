// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Assembler family abstraction for hierarchical parsing and encoding.

use opcore::parser::Expr;
use opcore::tokenizer::Span;
use types::symbol::SymbolTable;

use crate::symbol_stability::is_symbol_unstable;

/// Error returned when the family handler cannot parse an operand.
#[derive(Debug, Clone)]
pub struct FamilyParseError {
    pub message: String,
    pub span: Span,
}

impl FamilyParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl std::fmt::Display for FamilyParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for FamilyParseError {}

#[derive(Debug, Clone)]
pub struct EncodeError {
    pub message: String,
    pub span: Option<Span>,
}

impl EncodeError {
    pub fn new(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl std::fmt::Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for EncodeError {}

pub type EncodeOutcome<T> = Result<Option<T>, EncodeError>;

#[derive(Debug, Clone)]
pub enum EncodeResult<T> {
    Ok(T),
    NotFound,
    Error(String, Option<Span>),
}

impl<T> EncodeResult<T> {
    pub fn ok(value: T) -> Self {
        EncodeResult::Ok(value)
    }

    pub fn not_found() -> Self {
        EncodeResult::NotFound
    }

    pub fn error(message: impl Into<String>) -> Self {
        EncodeResult::Error(message.into(), None)
    }

    pub fn error_with_span(message: impl Into<String>, span: Span) -> Self {
        EncodeResult::Error(message.into(), Some(span))
    }

    pub fn into_outcome(self) -> EncodeOutcome<T> {
        self.into()
    }

    pub fn from_outcome(outcome: EncodeOutcome<T>) -> Self {
        outcome.into()
    }
}

impl<T> From<EncodeResult<T>> for EncodeOutcome<T> {
    fn from(value: EncodeResult<T>) -> Self {
        match value {
            EncodeResult::Ok(bytes) => Ok(Some(bytes)),
            EncodeResult::NotFound => Ok(None),
            EncodeResult::Error(message, span) => Err(EncodeError::new(message, span)),
        }
    }
}

impl<T> From<EncodeOutcome<T>> for EncodeResult<T> {
    fn from(value: EncodeOutcome<T>) -> Self {
        match value {
            Ok(Some(bytes)) => EncodeResult::Ok(bytes),
            Ok(None) => EncodeResult::NotFound,
            Err(err) => EncodeResult::Error(err.message, err.span),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FamilyEncodeResult<T> {
    Ok(T),
    NotFound,
    Error {
        bytes: T,
        message: String,
        span: Option<Span>,
        param: Option<String>,
    },
}

impl<T> FamilyEncodeResult<T> {
    pub fn ok(bytes: T) -> Self {
        FamilyEncodeResult::Ok(bytes)
    }

    pub fn not_found() -> Self {
        FamilyEncodeResult::NotFound
    }

    pub fn error(
        bytes: T,
        message: impl Into<String>,
        span: Option<Span>,
        param: Option<String>,
    ) -> Self {
        FamilyEncodeResult::Error {
            bytes,
            message: message.into(),
            span,
            param,
        }
    }
}

pub trait AssemblerContext {
    fn eval_expr(&self, expr: &Expr) -> Result<i64, String>;
    fn symbols(&self) -> &SymbolTable;
    fn has_symbol(&self, name: &str) -> bool;
    fn symbol_is_finalized(&self, name: &str) -> Option<bool>;
    fn current_address(&self) -> u32;
    fn pass(&self) -> u8;

    fn scalar_value_symbol(&self, _name: &str) -> Option<i64> {
        None
    }

    fn cpu_state_flag(&self, _key: &str) -> Option<u32> {
        None
    }
}

pub trait FamilyHandler: Send + Sync {
    type FamilyOperand: Clone + std::fmt::Debug;
    type Operand: Clone + std::fmt::Debug;

    fn parse_operands(
        &self,
        mnemonic: &str,
        exprs: &[Expr],
    ) -> Result<Vec<Self::FamilyOperand>, FamilyParseError>;

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[Self::Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>>;

    fn encode_family_operands(
        &self,
        _canonical_mnemonic: &str,
        _display_mnemonic: &str,
        _operands: &[Self::FamilyOperand],
        _ctx: &dyn AssemblerContext,
    ) -> FamilyEncodeResult<Vec<u8>> {
        FamilyEncodeResult::NotFound
    }

    fn is_register(&self, name: &str) -> bool;

    fn is_condition(&self, _name: &str) -> bool {
        false
    }
}

pub trait CpuHandler: Send + Sync {
    type Family: FamilyHandler;

    fn family(&self) -> &Self::Family;

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &[<Self::Family as FamilyHandler>::FamilyOperand],
        ctx: &dyn AssemblerContext,
    ) -> Result<Vec<<Self::Family as FamilyHandler>::Operand>, String>;

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[<Self::Family as FamilyHandler>::Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>>;

    fn supports_mnemonic(&self, mnemonic: &str) -> bool;
}

pub fn expr_has_unstable_symbols(expr: &Expr, ctx: &dyn AssemblerContext) -> bool {
    match expr {
        Expr::Identifier(name, _) | Expr::Register(name, _) => is_symbol_unstable(
            name,
            ctx.pass(),
            |symbol| ctx.has_symbol(symbol),
            |symbol| ctx.symbol_is_finalized(symbol),
        ),
        Expr::Indirect(inner, _) | Expr::Immediate(inner, _) | Expr::IndirectLong(inner, _) => {
            expr_has_unstable_symbols(inner, ctx)
        }
        Expr::List(items, _) => items
            .iter()
            .any(|item| expr_has_unstable_symbols(item, ctx)),
        Expr::Index { base, index, .. } => {
            expr_has_unstable_symbols(base, ctx) || expr_has_unstable_symbols(index, ctx)
        }
        Expr::Member { base, .. } => expr_has_unstable_symbols(base, ctx),
        Expr::StructLiteral { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_has_unstable_symbols(value, ctx)),
        Expr::Call { args, .. } => args.iter().any(|arg| expr_has_unstable_symbols(arg, ctx)),
        Expr::Placeholder(_) => false,
        Expr::Tuple(items, _) => items
            .iter()
            .any(|item| expr_has_unstable_symbols(item, ctx)),
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => {
            expr_has_unstable_symbols(cond, ctx)
                || expr_has_unstable_symbols(then_expr, ctx)
                || expr_has_unstable_symbols(else_expr, ctx)
        }
        Expr::Unary { expr, .. } => expr_has_unstable_symbols(expr, ctx),
        Expr::Binary { left, right, .. } => {
            expr_has_unstable_symbols(left, ctx) || expr_has_unstable_symbols(right, ctx)
        }
        Expr::Range {
            start, end, step, ..
        } => {
            expr_has_unstable_symbols(start, ctx)
                || expr_has_unstable_symbols(end, ctx)
                || step
                    .as_ref()
                    .is_some_and(|step_expr| expr_has_unstable_symbols(step_expr, ctx))
        }
        Expr::Number(_, _) | Expr::Dollar(_) | Expr::String(_, _) | Expr::Error(_, _) => false,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{expr_has_unstable_symbols, AssemblerContext};
    use opcore::parser::{BinaryOp, Expr};
    use opcore::tokenizer::Span;
    use types::symbol::SymbolTable;

    struct TestCtx {
        pass: u8,
        symbols: SymbolTable,
        finalized: HashMap<String, bool>,
    }

    impl TestCtx {
        fn new(pass: u8, finalized: HashMap<String, bool>) -> Self {
            Self {
                pass,
                symbols: SymbolTable::new(),
                finalized,
            }
        }
    }

    impl AssemblerContext for TestCtx {
        fn eval_expr(&self, _expr: &Expr) -> Result<i64, String> {
            Err("not needed in these tests".to_string())
        }

        fn symbols(&self) -> &SymbolTable {
            &self.symbols
        }

        fn has_symbol(&self, name: &str) -> bool {
            self.finalized.contains_key(name)
        }

        fn symbol_is_finalized(&self, name: &str) -> Option<bool> {
            self.finalized.get(name).copied()
        }

        fn current_address(&self) -> u32 {
            0
        }

        fn pass(&self) -> u8 {
            self.pass
        }
    }

    fn span() -> Span {
        Span {
            line: 1,
            col_start: 1,
            col_end: 1,
        }
    }

    #[test]
    fn unstable_symbol_detection_reports_unknown_identifier() {
        let expr = Expr::Identifier("missing".to_string(), span());
        let ctx = TestCtx::new(1, HashMap::new());
        assert!(expr_has_unstable_symbols(&expr, &ctx));
    }

    #[test]
    fn unstable_symbol_detection_accepts_finalized_identifier() {
        let expr = Expr::Identifier("label".to_string(), span());
        let ctx = TestCtx::new(2, HashMap::from([("label".to_string(), true)]));
        assert!(!expr_has_unstable_symbols(&expr, &ctx));
    }

    #[test]
    fn unstable_symbol_detection_traverses_expression_tree() {
        let expr = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Identifier("left".to_string(), span())),
            right: Box::new(Expr::Identifier("right".to_string(), span())),
            span: span(),
        };
        let ctx = TestCtx::new(
            2,
            HashMap::from([("left".to_string(), true), ("right".to_string(), false)]),
        );
        assert!(expr_has_unstable_symbols(&expr, &ctx));
    }
}
