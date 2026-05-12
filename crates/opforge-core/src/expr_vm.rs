// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use std::{cell::Cell, collections::HashMap};

use crate::expr::{apply_binary, apply_unary, parse_number};
use crate::parser::{BinaryOp, Expr, UnaryOp};
use crate::tokenizer::Span;
use types::symbol_stability::is_symbol_unstable;

pub const EXPR_VM_OPCODE_VERSION_V1: u16 = 0x0001;
pub const EXPR_VM_OPCODE_VERSION_V2: u16 = 0x0002;

/// Expression VM opcode table and compatibility notes.
///
/// Opcode map (`EXPR_VM_OPCODE_VERSION_V1`):
/// - `0x00`: `End`
/// - `0x01`: `PushLiteral`
/// - `0x02`: `PushCurrentAddress`
/// - `0x03`: `PushSymbol`
/// - `0x04`: `ApplyUnary`
/// - `0x05`: `ApplyBinary`
/// - `0x06`: `SelectTernary`
/// - `0x07`: `PushStringLiteral`
///
/// Compatibility matrix:
/// - Runtime evaluator supports: `v1`.
/// - Program decode policy: reject unknown opcode versions; do not silently
///   reinterpret payloads across versions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ExprVmOpcode {
    End = 0x00,
    PushLiteral = 0x01,
    PushCurrentAddress = 0x02,
    PushSymbol = 0x03,
    ApplyUnary = 0x04,
    ApplyBinary = 0x05,
    SelectTernary = 0x06,
    PushStringLiteral = 0x07,
}

pub type ExprVmOpcodeV1 = ExprVmOpcode;

impl ExprVmOpcode {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x00 => Some(Self::End),
            0x01 => Some(Self::PushLiteral),
            0x02 => Some(Self::PushCurrentAddress),
            0x03 => Some(Self::PushSymbol),
            0x04 => Some(Self::ApplyUnary),
            0x05 => Some(Self::ApplyBinary),
            0x06 => Some(Self::SelectTernary),
            0x07 => Some(Self::PushStringLiteral),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ExprVmOpcodeV2 {
    End = 0x00,
    EmitDiag = 0x01,
    Fail = 0x02,
    PushLiteral = 0x10,
    PushCurrentAddress = 0x11,
    PushSymbol = 0x12,
    PushStringLiteral = 0x13,
    ApplyUnary = 0x20,
    ApplyBinary = 0x21,
    SelectTernary = 0x22,
    PushRegisterRef = 0x30,
    PushPlaceholder = 0x31,
    WrapImmediate = 0x40,
    WrapIndirect = 0x41,
    WrapIndirectLong = 0x42,
    BuildTuple = 0x50,
    BuildList = 0x51,
    BuildRange = 0x52,
    BuildStructLiteral = 0x53,
    GetMember = 0x60,
    IndexValue = 0x61,
    CallBuiltin = 0x62,
    RequireScalar = 0x70,
}

impl ExprVmOpcodeV2 {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x00 => Some(Self::End),
            0x01 => Some(Self::EmitDiag),
            0x02 => Some(Self::Fail),
            0x10 => Some(Self::PushLiteral),
            0x11 => Some(Self::PushCurrentAddress),
            0x12 => Some(Self::PushSymbol),
            0x13 => Some(Self::PushStringLiteral),
            0x20 => Some(Self::ApplyUnary),
            0x21 => Some(Self::ApplyBinary),
            0x22 => Some(Self::SelectTernary),
            0x30 => Some(Self::PushRegisterRef),
            0x31 => Some(Self::PushPlaceholder),
            0x40 => Some(Self::WrapImmediate),
            0x41 => Some(Self::WrapIndirect),
            0x42 => Some(Self::WrapIndirectLong),
            0x50 => Some(Self::BuildTuple),
            0x51 => Some(Self::BuildList),
            0x52 => Some(Self::BuildRange),
            0x53 => Some(Self::BuildStructLiteral),
            0x60 => Some(Self::GetMember),
            0x61 => Some(Self::IndexValue),
            0x62 => Some(Self::CallBuiltin),
            0x70 => Some(Self::RequireScalar),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ExprVmUnary {
    Plus = 0,
    Minus = 1,
    BitNot = 2,
    LogicNot = 3,
    High = 4,
    Low = 5,
}

impl ExprVmUnary {
    fn from_core(op: UnaryOp) -> Self {
        match op {
            UnaryOp::Plus => Self::Plus,
            UnaryOp::Minus => Self::Minus,
            UnaryOp::BitNot => Self::BitNot,
            UnaryOp::LogicNot => Self::LogicNot,
            UnaryOp::High => Self::High,
            UnaryOp::Low => Self::Low,
        }
    }

    fn to_core(self) -> UnaryOp {
        match self {
            Self::Plus => UnaryOp::Plus,
            Self::Minus => UnaryOp::Minus,
            Self::BitNot => UnaryOp::BitNot,
            Self::LogicNot => UnaryOp::LogicNot,
            Self::High => UnaryOp::High,
            Self::Low => UnaryOp::Low,
        }
    }

    fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::Plus),
            1 => Some(Self::Minus),
            2 => Some(Self::BitNot),
            3 => Some(Self::LogicNot),
            4 => Some(Self::High),
            5 => Some(Self::Low),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ExprVmBinary {
    Multiply = 0,
    Divide = 1,
    Mod = 2,
    Power = 3,
    Shl = 4,
    Shr = 5,
    Add = 6,
    Subtract = 7,
    Eq = 8,
    Ne = 9,
    Ge = 10,
    Gt = 11,
    Le = 12,
    Lt = 13,
    BitAnd = 14,
    BitOr = 15,
    BitXor = 16,
    LogicAnd = 17,
    LogicOr = 18,
    LogicXor = 19,
}

impl ExprVmBinary {
    fn from_core(op: BinaryOp) -> Self {
        match op {
            BinaryOp::Multiply => Self::Multiply,
            BinaryOp::Divide => Self::Divide,
            BinaryOp::Mod => Self::Mod,
            BinaryOp::Power => Self::Power,
            BinaryOp::Shl => Self::Shl,
            BinaryOp::Shr => Self::Shr,
            BinaryOp::Add => Self::Add,
            BinaryOp::Subtract => Self::Subtract,
            BinaryOp::Eq => Self::Eq,
            BinaryOp::Ne => Self::Ne,
            BinaryOp::Ge => Self::Ge,
            BinaryOp::Gt => Self::Gt,
            BinaryOp::Le => Self::Le,
            BinaryOp::Lt => Self::Lt,
            BinaryOp::BitAnd => Self::BitAnd,
            BinaryOp::BitOr => Self::BitOr,
            BinaryOp::BitXor => Self::BitXor,
            BinaryOp::LogicAnd => Self::LogicAnd,
            BinaryOp::LogicOr => Self::LogicOr,
            BinaryOp::LogicXor => Self::LogicXor,
        }
    }

    fn to_core(self) -> BinaryOp {
        match self {
            Self::Multiply => BinaryOp::Multiply,
            Self::Divide => BinaryOp::Divide,
            Self::Mod => BinaryOp::Mod,
            Self::Power => BinaryOp::Power,
            Self::Shl => BinaryOp::Shl,
            Self::Shr => BinaryOp::Shr,
            Self::Add => BinaryOp::Add,
            Self::Subtract => BinaryOp::Subtract,
            Self::Eq => BinaryOp::Eq,
            Self::Ne => BinaryOp::Ne,
            Self::Ge => BinaryOp::Ge,
            Self::Gt => BinaryOp::Gt,
            Self::Le => BinaryOp::Le,
            Self::Lt => BinaryOp::Lt,
            Self::BitAnd => BinaryOp::BitAnd,
            Self::BitOr => BinaryOp::BitOr,
            Self::BitXor => BinaryOp::BitXor,
            Self::LogicAnd => BinaryOp::LogicAnd,
            Self::LogicOr => BinaryOp::LogicOr,
            Self::LogicXor => BinaryOp::LogicXor,
        }
    }

    fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::Multiply),
            1 => Some(Self::Divide),
            2 => Some(Self::Mod),
            3 => Some(Self::Power),
            4 => Some(Self::Shl),
            5 => Some(Self::Shr),
            6 => Some(Self::Add),
            7 => Some(Self::Subtract),
            8 => Some(Self::Eq),
            9 => Some(Self::Ne),
            10 => Some(Self::Ge),
            11 => Some(Self::Gt),
            12 => Some(Self::Le),
            13 => Some(Self::Lt),
            14 => Some(Self::BitAnd),
            15 => Some(Self::BitOr),
            16 => Some(Self::BitXor),
            17 => Some(Self::LogicAnd),
            18 => Some(Self::LogicOr),
            19 => Some(Self::LogicXor),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PortableExprRef {
    pub index: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableExprProgramV2 {
    pub opcode_version: u16,
    pub code: Vec<u8>,
    pub symbols: Vec<String>,
    pub declared_stack_depth: u16,
    pub result_mode: PortableExprResultModeV2,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PortableExprResultModeV2 {
    Scalar,
    ShapePreserving,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PortableExprValueV2 {
    Int(i64),
    String(Vec<u8>),
    SymbolRef(u32),
    RegisterRef(u32),
    Immediate(Box<PortableExprValueV2>),
    Indirect(Box<PortableExprValueV2>),
    IndirectLong(Box<PortableExprValueV2>),
    Tuple(Vec<PortableExprValueV2>),
    List(Vec<PortableExprValueV2>),
    Range(PortableExprRangeValueV2),
    StructType(PortableExprStructTypeValueV2),
    StructLiteral(PortableExprStructLiteralValueV2),
    Placeholder,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableExprRangeValueV2 {
    pub start: Box<PortableExprValueV2>,
    pub end: Box<PortableExprValueV2>,
    pub step: Option<Box<PortableExprValueV2>>,
    pub inclusive: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableExprStructFieldValueV2 {
    pub field_name: String,
    pub value: PortableExprValueV2,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableExprStructLiteralValueV2 {
    pub type_name: String,
    pub fields: Vec<PortableExprStructFieldValueV2>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableExprStructTypeFieldValueV2 {
    pub field_name: String,
    pub offset: u32,
    pub size: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableExprStructTypeValueV2 {
    pub type_name: String,
    pub fields: Vec<PortableExprStructTypeFieldValueV2>,
    pub size: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PortableExprBudgetsV2 {
    pub max_program_bytes: usize,
    pub max_stack_depth: usize,
    pub max_symbol_refs: usize,
    pub max_eval_steps: usize,
    pub max_shape_items: usize,
}

impl Default for PortableExprBudgetsV2 {
    fn default() -> Self {
        Self {
            max_program_bytes: 2048,
            max_stack_depth: 64,
            max_symbol_refs: 128,
            max_eval_steps: 2048,
            max_shape_items: 256,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableExprEvaluationV2 {
    pub value: PortableExprValueV2,
    pub has_symbol_refs: bool,
    pub has_unstable_symbols: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableExprProgram {
    pub opcode_version: u16,
    pub code: Vec<u8>,
    pub symbols: Vec<String>,
    pub declared_stack_depth: u16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PortableExprBudgets {
    pub max_program_bytes: usize,
    pub max_stack_depth: usize,
    pub max_symbol_refs: usize,
    pub max_eval_steps: usize,
}

impl Default for PortableExprBudgets {
    fn default() -> Self {
        Self {
            max_program_bytes: 2048,
            max_stack_depth: 64,
            max_symbol_refs: 128,
            max_eval_steps: 2048,
        }
    }
}

pub trait PortableExprEvalContext {
    fn lookup_symbol(&self, name: &str) -> Option<i64>;
    fn current_address(&self) -> Option<i64>;
    fn pass(&self) -> u8;
    fn symbol_is_finalized(&self, name: &str) -> Option<bool>;

    fn lookup_symbol_value(&self, name: &str) -> Option<PortableExprValueV2> {
        self.lookup_symbol(name).map(PortableExprValueV2::Int)
    }

    fn symbol_exists(&self, name: &str) -> bool {
        self.lookup_symbol_value(name).is_some() || self.lookup_symbol(name).is_some()
    }

    fn eval_string_literal(&self, _bytes: &[u8]) -> Result<i64, String> {
        Err("string expression is not supported by portable expression VM context".to_string())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableExprEvaluation {
    pub value: i64,
    pub has_symbol_refs: bool,
    pub has_unstable_symbols: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableExprError {
    pub code: &'static str,
    pub message: String,
    pub span: Option<Span>,
}

impl PortableExprError {
    fn new(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            span: None,
        }
    }

    fn with_span(code: &'static str, message: impl Into<String>, span: Span) -> Self {
        Self {
            code,
            message: message.into(),
            span: Some(span),
        }
    }
}

impl std::fmt::Display for PortableExprError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.code, self.message)
    }
}

impl std::error::Error for PortableExprError {}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PortableExprDiagnosticMapV2 {
    pub invalid_opcode: String,
    pub stack_underflow: String,
    pub stack_depth_exceeded: String,
    pub unknown_symbol: String,
    pub eval_failure: String,
    pub unsupported_feature: String,
    pub budget_exceeded: String,
    pub invalid_program: String,
    pub invalid_scalar_conversion: String,
}

pub const DIAG_EXPR_INVALID_OPCODE: &str = "ope001";
pub const DIAG_EXPR_STACK_UNDERFLOW: &str = "ope002";
pub const DIAG_EXPR_STACK_DEPTH_EXCEEDED: &str = "ope003";
pub const DIAG_EXPR_UNKNOWN_SYMBOL: &str = "ope004";
pub const DIAG_EXPR_EVAL_FAILURE: &str = "ope005";
pub const DIAG_EXPR_UNSUPPORTED_FEATURE: &str = "ope006";
pub const DIAG_EXPR_BUDGET_EXCEEDED: &str = "ope007";
pub const DIAG_EXPR_INVALID_PROGRAM: &str = "ope008";

thread_local! {
    static LEGACY_EXPR_COMPILER_FAILPOINT: Cell<bool> = const { Cell::new(false) };
}

pub fn set_legacy_expr_compiler_failpoint_for_tests(enabled: bool) {
    LEGACY_EXPR_COMPILER_FAILPOINT.with(|flag| flag.set(enabled));
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PortableExprDirectLeaf {
    NumberText(String),
    SymbolName(String),
    CurrentAddress,
    StringLiteral(Vec<u8>),
}

pub struct PortableExprProgramBuilder {
    compiler: ExprCompiler,
    require_scalar_on_finish: bool,
}

impl PortableExprProgramBuilder {
    pub fn for_scalar(opcode_version: u16) -> Result<Self, PortableExprError> {
        Ok(Self {
            compiler: ExprCompiler::for_opcode_version(opcode_version, ExprCompilerMode::Scalar)?,
            require_scalar_on_finish: opcode_version == EXPR_VM_OPCODE_VERSION_V2,
        })
    }

    pub fn emit_direct_leaf(
        &mut self,
        leaf: &PortableExprDirectLeaf,
    ) -> Result<(), PortableExprError> {
        self.compiler.compile_direct_leaf(leaf)
    }

    pub fn emit_unary(&mut self, op: UnaryOp) -> Result<(), PortableExprError> {
        self.compiler.emit_apply_unary(ExprVmUnary::from_core(op));
        Ok(())
    }

    pub fn emit_binary(&mut self, op: BinaryOp) -> Result<(), PortableExprError> {
        self.compiler.emit_apply_binary(ExprVmBinary::from_core(op));
        self.compiler.stack_pop()?;
        Ok(())
    }

    pub fn emit_ternary(&mut self) -> Result<(), PortableExprError> {
        self.compiler.emit_select_ternary();
        self.compiler.stack_pop()?;
        self.compiler.stack_pop()?;
        Ok(())
    }

    pub fn emit_list(&mut self, count: usize) -> Result<(), PortableExprError> {
        if self.compiler.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
            return Err(PortableExprError::new(
                DIAG_EXPR_UNSUPPORTED_FEATURE,
                "List cannot be evaluated as scalar expression",
            ));
        }
        let count = u16::try_from(count).map_err(|_| {
            PortableExprError::new(
                DIAG_EXPR_BUDGET_EXCEEDED,
                "expression list exceeds u16 VM item capacity",
            )
        })?;
        self.compiler.emit_build_list(count);
        self.compiler.stack_collapse(count as usize)?;
        Ok(())
    }

    pub fn emit_range(&mut self, has_step: bool, inclusive: bool) -> Result<(), PortableExprError> {
        if self.compiler.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
            return Err(PortableExprError::new(
                DIAG_EXPR_UNSUPPORTED_FEATURE,
                "Range cannot be evaluated as scalar expression",
            ));
        }
        self.compiler.emit_build_range(has_step, inclusive);
        self.compiler.stack_collapse(if has_step { 3 } else { 2 })?;
        Ok(())
    }

    pub fn emit_struct_literal(
        &mut self,
        type_name: &str,
        field_names: &[String],
    ) -> Result<(), PortableExprError> {
        if self.compiler.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
            return Err(PortableExprError::new(
                DIAG_EXPR_UNSUPPORTED_FEATURE,
                "Struct literal cannot be evaluated as scalar expression",
            ));
        }
        let field_count = u16::try_from(field_names.len()).map_err(|_| {
            PortableExprError::new(
                DIAG_EXPR_BUDGET_EXCEEDED,
                "struct literal exceeds u16 VM field capacity",
            )
        })?;
        let type_name_idx = self.compiler.intern_symbol(type_name)?;
        let mut field_indices = Vec::with_capacity(field_names.len());
        for field_name in field_names {
            field_indices.push(self.compiler.intern_symbol(field_name)?);
        }
        self.compiler
            .emit_build_struct_literal(type_name_idx, field_count, &field_indices);
        self.compiler.stack_collapse(field_names.len())?;
        Ok(())
    }

    pub fn emit_index(&mut self) -> Result<(), PortableExprError> {
        if self.compiler.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
            return Err(PortableExprError::new(
                DIAG_EXPR_UNSUPPORTED_FEATURE,
                "Index expression cannot be evaluated as scalar expression",
            ));
        }
        self.compiler.emit_index_value();
        self.compiler.stack_pop()?;
        Ok(())
    }

    pub fn emit_member(&mut self, field_name: &str) -> Result<(), PortableExprError> {
        if self.compiler.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
            return Err(PortableExprError::new(
                DIAG_EXPR_UNSUPPORTED_FEATURE,
                "Member expression cannot be evaluated as scalar expression",
            ));
        }
        let field_idx = self.compiler.intern_symbol(field_name)?;
        self.compiler.emit_get_member(field_idx);
        Ok(())
    }

    pub fn finish(mut self) -> PortableExprProgram {
        if self.require_scalar_on_finish {
            self.compiler.emit_require_scalar();
        }
        self.compiler.emit_end();
        PortableExprProgram {
            opcode_version: self.compiler.opcode_version,
            code: self.compiler.code,
            symbols: self.compiler.symbols,
            declared_stack_depth: self.compiler.stack_max as u16,
        }
    }
}

fn unsupported_expression_vm_opcode_version(opcode_version: u16) -> PortableExprError {
    PortableExprError::new(
        DIAG_EXPR_INVALID_PROGRAM,
        format!(
            "unsupported expression VM opcode version {}",
            opcode_version
        ),
    )
}

pub fn compile_core_expr_to_portable_program(
    expr: &Expr,
) -> Result<PortableExprProgram, PortableExprError> {
    compile_core_expr_to_portable_program_with_opcode_version(expr, EXPR_VM_OPCODE_VERSION_V1)
}

pub fn compile_core_expr_to_portable_program_with_opcode_version(
    expr: &Expr,
    opcode_version: u16,
) -> Result<PortableExprProgram, PortableExprError> {
    if LEGACY_EXPR_COMPILER_FAILPOINT.with(|flag| flag.get()) {
        return Err(PortableExprError::new(
            DIAG_EXPR_INVALID_PROGRAM,
            "legacy expression compiler failpoint",
        ));
    }

    let mut compiler = ExprCompiler::for_opcode_version(opcode_version, ExprCompilerMode::Scalar)?;
    compiler.compile(expr)?;
    if opcode_version == EXPR_VM_OPCODE_VERSION_V2 {
        compiler.emit_require_scalar();
    }
    compiler.emit_end();
    Ok(PortableExprProgram {
        opcode_version: compiler.opcode_version,
        code: compiler.code,
        symbols: compiler.symbols,
        declared_stack_depth: compiler.stack_max as u16,
    })
}

pub fn compile_portable_expr_direct_leaf_to_program_with_opcode_version(
    leaf: &PortableExprDirectLeaf,
    opcode_version: u16,
) -> Result<PortableExprProgram, PortableExprError> {
    let mut compiler = ExprCompiler::for_opcode_version(opcode_version, ExprCompilerMode::Scalar)?;

    compiler.compile_direct_leaf(leaf)?;

    if opcode_version == EXPR_VM_OPCODE_VERSION_V2 {
        compiler.emit_require_scalar();
    }
    compiler.emit_end();

    Ok(PortableExprProgram {
        opcode_version: compiler.opcode_version,
        code: compiler.code,
        symbols: compiler.symbols,
        declared_stack_depth: compiler.stack_max as u16,
    })
}

pub fn expr_is_supported_by_direct_scalar_lowering(expr: &Expr) -> bool {
    match expr {
        Expr::Number(_, _)
        | Expr::Identifier(_, _)
        | Expr::Register(_, _)
        | Expr::Dollar(_)
        | Expr::String(_, _) => true,
        Expr::Unary { expr, .. } => expr_is_supported_by_direct_scalar_lowering(expr),
        Expr::Binary { left, right, .. } => {
            expr_is_supported_by_direct_scalar_lowering(left)
                && expr_is_supported_by_direct_scalar_lowering(right)
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => {
            expr_is_supported_by_direct_scalar_lowering(cond)
                && expr_is_supported_by_direct_scalar_lowering(then_expr)
                && expr_is_supported_by_direct_scalar_lowering(else_expr)
        }
        Expr::List(_, _)
        | Expr::Index { .. }
        | Expr::Member { .. }
        | Expr::StructLiteral { .. }
        | Expr::Call { .. }
        | Expr::Placeholder(_)
        | Expr::Indirect(_, _)
        | Expr::IndirectLong(_, _)
        | Expr::Immediate(_, _)
        | Expr::Tuple(_, _)
        | Expr::Range { .. }
        | Expr::Error(_, _) => false,
    }
}

pub fn compile_portable_expr_direct_scalar_to_program_with_opcode_version(
    expr: &Expr,
    opcode_version: u16,
) -> Result<PortableExprProgram, PortableExprError> {
    let mut compiler = ExprCompiler::for_opcode_version(opcode_version, ExprCompilerMode::Scalar)?;

    compiler.compile_direct_scalar(expr)?;

    if opcode_version == EXPR_VM_OPCODE_VERSION_V2 {
        compiler.emit_require_scalar();
    }
    compiler.emit_end();

    Ok(PortableExprProgram {
        opcode_version: compiler.opcode_version,
        code: compiler.code,
        symbols: compiler.symbols,
        declared_stack_depth: compiler.stack_max as u16,
    })
}

pub fn expr_is_supported_by_direct_structural_lowering(expr: &Expr) -> bool {
    match expr {
        Expr::List(items, _) => items
            .iter()
            .all(expr_is_supported_by_direct_structural_lowering),
        Expr::Range {
            start, end, step, ..
        } => {
            expr_is_supported_by_direct_structural_lowering(start)
                && expr_is_supported_by_direct_structural_lowering(end)
                && step
                    .as_deref()
                    .map(expr_is_supported_by_direct_structural_lowering)
                    .unwrap_or(true)
        }
        Expr::StructLiteral { fields, .. } => fields
            .iter()
            .all(|(_, field_expr)| expr_is_supported_by_direct_structural_lowering(field_expr)),
        _ => expr_is_supported_by_direct_scalar_lowering(expr),
    }
}

pub fn compile_portable_expr_direct_structural_to_program_with_opcode_version(
    expr: &Expr,
    opcode_version: u16,
) -> Result<PortableExprProgram, PortableExprError> {
    let mut compiler = ExprCompiler::for_opcode_version(opcode_version, ExprCompilerMode::Scalar)?;

    compiler.compile_direct_structural(expr)?;

    if opcode_version == EXPR_VM_OPCODE_VERSION_V2 {
        compiler.emit_require_scalar();
    }
    compiler.emit_end();

    Ok(PortableExprProgram {
        opcode_version: compiler.opcode_version,
        code: compiler.code,
        symbols: compiler.symbols,
        declared_stack_depth: compiler.stack_max as u16,
    })
}

pub fn expr_is_supported_by_direct_member_index_lowering(expr: &Expr) -> bool {
    match expr {
        Expr::Index { base, index, .. } => {
            expr_is_supported_by_direct_member_index_lowering(base)
                && expr_is_supported_by_direct_member_index_lowering(index)
        }
        Expr::Member { base, .. } => expr_is_supported_by_direct_member_index_lowering(base),
        _ => expr_is_supported_by_direct_structural_lowering(expr),
    }
}

pub fn compile_portable_expr_direct_member_index_to_program_with_opcode_version(
    expr: &Expr,
    opcode_version: u16,
) -> Result<PortableExprProgram, PortableExprError> {
    let mut compiler = ExprCompiler::for_opcode_version(opcode_version, ExprCompilerMode::Scalar)?;

    compiler.compile_direct_member_index(expr)?;

    if opcode_version == EXPR_VM_OPCODE_VERSION_V2 {
        compiler.emit_require_scalar();
    }
    compiler.emit_end();

    Ok(PortableExprProgram {
        opcode_version: compiler.opcode_version,
        code: compiler.code,
        symbols: compiler.symbols,
        declared_stack_depth: compiler.stack_max as u16,
    })
}

pub fn compile_core_expr_to_portable_program_v2_shape_preserving(
    expr: &Expr,
) -> Result<PortableExprProgramV2, PortableExprError> {
    let mut compiler = ExprCompiler::for_opcode_version(
        EXPR_VM_OPCODE_VERSION_V2,
        ExprCompilerMode::ShapePreserving,
    )?;
    compiler.compile(expr)?;
    compiler.emit_end();
    Ok(PortableExprProgramV2 {
        opcode_version: compiler.opcode_version,
        code: compiler.code,
        symbols: compiler.symbols,
        declared_stack_depth: compiler.stack_max as u16,
        result_mode: PortableExprResultModeV2::ShapePreserving,
    })
}

pub fn eval_portable_expr_program(
    program: &PortableExprProgram,
    ctx: &dyn PortableExprEvalContext,
    budgets: PortableExprBudgets,
) -> Result<PortableExprEvaluation, PortableExprError> {
    match program.opcode_version {
        EXPR_VM_OPCODE_VERSION_V1 => eval_portable_expr_program_v1(program, ctx, budgets),
        EXPR_VM_OPCODE_VERSION_V2 => eval_portable_expr_program_v2_scalar(program, ctx, budgets),
        other => Err(unsupported_expression_vm_opcode_version(other)),
    }
}

pub fn eval_portable_expr_program_v2(
    program: &PortableExprProgramV2,
    ctx: &dyn PortableExprEvalContext,
    budgets: PortableExprBudgetsV2,
) -> Result<PortableExprEvaluationV2, PortableExprError> {
    validate_portable_expr_program_v2_skeleton(program, budgets)?;
    let mut evaluation = eval_portable_expr_program_v2_internal(
        program.opcode_version,
        &program.code,
        &program.symbols,
        program.declared_stack_depth,
        ctx,
        budgets.into(),
    )?;
    if program.result_mode == PortableExprResultModeV2::Scalar {
        evaluation.value =
            PortableExprValueV2::Int(portable_expr_value_to_scalar(&evaluation.value, ctx)?);
    }
    Ok(evaluation)
}

fn eval_portable_expr_program_v1(
    program: &PortableExprProgram,
    ctx: &dyn PortableExprEvalContext,
    budgets: PortableExprBudgets,
) -> Result<PortableExprEvaluation, PortableExprError> {
    enforce_program_budgets_v1(program, budgets)?;

    let mut stack: Vec<i64> = Vec::new();
    let mut ip = 0usize;
    let mut steps = 0usize;
    let mut has_symbol_refs = false;
    let mut has_unstable_symbols = false;

    while ip < program.code.len() {
        steps = steps.saturating_add(1);
        if steps > budgets.max_eval_steps {
            return Err(PortableExprError::new(
                DIAG_EXPR_BUDGET_EXCEEDED,
                format!(
                    "expression evaluation step budget exceeded ({} > {})",
                    steps, budgets.max_eval_steps
                ),
            ));
        }

        let opcode = read_opcode_v1(&program.code, &mut ip)?;
        match opcode {
            ExprVmOpcode::End => break,
            ExprVmOpcode::PushLiteral => {
                let value = read_i64_le(&program.code, &mut ip)?;
                stack.push(value);
                enforce_stack_budget(&stack, budgets)?;
            }
            ExprVmOpcode::PushCurrentAddress => {
                let value = ctx.current_address().ok_or_else(|| {
                    PortableExprError::new(
                        DIAG_EXPR_EVAL_FAILURE,
                        "current address ($) not available",
                    )
                })?;
                stack.push(value);
                enforce_stack_budget(&stack, budgets)?;
            }
            ExprVmOpcode::PushSymbol => {
                let symbol_idx = read_u16_le(&program.code, &mut ip)? as usize;
                let Some(symbol_name) = program.symbols.get(symbol_idx) else {
                    return Err(PortableExprError::new(
                        DIAG_EXPR_INVALID_PROGRAM,
                        format!("symbol index out of range: {}", symbol_idx),
                    ));
                };

                has_symbol_refs = true;
                if is_symbol_unstable(
                    symbol_name,
                    ctx.pass(),
                    |symbol| ctx.lookup_symbol(symbol).is_some(),
                    |symbol| ctx.symbol_is_finalized(symbol),
                ) {
                    has_unstable_symbols = true;
                }

                let value = ctx.lookup_symbol(symbol_name).ok_or_else(|| {
                    PortableExprError::new(
                        DIAG_EXPR_UNKNOWN_SYMBOL,
                        format!("undefined symbol: {}", symbol_name),
                    )
                })?;
                stack.push(value);
                enforce_stack_budget(&stack, budgets)?;
            }
            ExprVmOpcode::PushStringLiteral => {
                let len = read_u16_le(&program.code, &mut ip)? as usize;
                let bytes = read_bytes(&program.code, &mut ip, len)?;
                let value = ctx
                    .eval_string_literal(bytes)
                    .map_err(|message| PortableExprError::new(DIAG_EXPR_EVAL_FAILURE, message))?;
                stack.push(value);
                enforce_stack_budget(&stack, budgets)?;
            }
            ExprVmOpcode::ApplyUnary => {
                let unary =
                    ExprVmUnary::from_u8(read_u8(&program.code, &mut ip)?).ok_or_else(|| {
                        PortableExprError::new(DIAG_EXPR_INVALID_OPCODE, "invalid unary opcode")
                    })?;
                let value = pop_value(&mut stack)?;
                let result = apply_unary(unary.to_core(), value, Span::default())
                    .map_err(|err| PortableExprError::new(DIAG_EXPR_EVAL_FAILURE, err.message))?;
                stack.push(result);
            }
            ExprVmOpcode::ApplyBinary => {
                let binary =
                    ExprVmBinary::from_u8(read_u8(&program.code, &mut ip)?).ok_or_else(|| {
                        PortableExprError::new(DIAG_EXPR_INVALID_OPCODE, "invalid binary opcode")
                    })?;
                let right = pop_value(&mut stack)?;
                let left = pop_value(&mut stack)?;
                let result = apply_binary(binary.to_core(), left, right, Span::default())
                    .map_err(|err| PortableExprError::new(DIAG_EXPR_EVAL_FAILURE, err.message))?;
                stack.push(result);
            }
            ExprVmOpcode::SelectTernary => {
                let else_value = pop_value(&mut stack)?;
                let then_value = pop_value(&mut stack)?;
                let cond_value = pop_value(&mut stack)?;
                stack.push(if cond_value != 0 {
                    then_value
                } else {
                    else_value
                });
            }
        }
    }

    if stack.len() != 1 {
        return Err(PortableExprError::new(
            DIAG_EXPR_INVALID_PROGRAM,
            format!(
                "expression VM must finish with exactly one stack value; got {}",
                stack.len()
            ),
        ));
    }

    Ok(PortableExprEvaluation {
        value: stack[0],
        has_symbol_refs,
        has_unstable_symbols,
    })
}

fn eval_portable_expr_program_v2_scalar(
    program: &PortableExprProgram,
    ctx: &dyn PortableExprEvalContext,
    budgets: PortableExprBudgets,
) -> Result<PortableExprEvaluation, PortableExprError> {
    let evaluation = eval_portable_expr_program_v2_internal(
        program.opcode_version,
        &program.code,
        &program.symbols,
        program.declared_stack_depth,
        ctx,
        budgets.into(),
    )?;

    Ok(PortableExprEvaluation {
        value: portable_expr_value_to_scalar(&evaluation.value, ctx)?,
        has_symbol_refs: evaluation.has_symbol_refs,
        has_unstable_symbols: evaluation.has_unstable_symbols,
    })
}

pub fn expr_program_has_unstable_symbols(
    program: &PortableExprProgram,
    ctx: &dyn PortableExprEvalContext,
    budgets: PortableExprBudgets,
) -> Result<bool, PortableExprError> {
    match program.opcode_version {
        EXPR_VM_OPCODE_VERSION_V1 => expr_program_has_unstable_symbols_v1(program, ctx, budgets),
        EXPR_VM_OPCODE_VERSION_V2 => {
            expr_program_has_unstable_symbols_v2_scalar(program, ctx, budgets)
        }
        other => Err(unsupported_expression_vm_opcode_version(other)),
    }
}

fn expr_program_has_unstable_symbols_v1(
    program: &PortableExprProgram,
    ctx: &dyn PortableExprEvalContext,
    budgets: PortableExprBudgets,
) -> Result<bool, PortableExprError> {
    enforce_program_budgets_v1(program, budgets)?;
    let mut ip = 0usize;

    while ip < program.code.len() {
        let opcode = read_opcode_v1(&program.code, &mut ip)?;
        match opcode {
            ExprVmOpcode::End => return Ok(false),
            ExprVmOpcode::PushLiteral => {
                read_i64_le(&program.code, &mut ip)?;
            }
            ExprVmOpcode::PushCurrentAddress => {}
            ExprVmOpcode::PushSymbol => {
                let symbol_idx = read_u16_le(&program.code, &mut ip)? as usize;
                let Some(symbol_name) = program.symbols.get(symbol_idx) else {
                    return Err(PortableExprError::new(
                        DIAG_EXPR_INVALID_PROGRAM,
                        format!("symbol index out of range: {}", symbol_idx),
                    ));
                };
                if is_symbol_unstable(
                    symbol_name,
                    ctx.pass(),
                    |symbol| ctx.lookup_symbol(symbol).is_some(),
                    |symbol| ctx.symbol_is_finalized(symbol),
                ) {
                    return Ok(true);
                }
            }
            ExprVmOpcode::PushStringLiteral => {
                let len = read_u16_le(&program.code, &mut ip)? as usize;
                read_bytes(&program.code, &mut ip, len)?;
            }
            ExprVmOpcode::ApplyUnary => {
                read_u8(&program.code, &mut ip)?;
            }
            ExprVmOpcode::ApplyBinary => {
                read_u8(&program.code, &mut ip)?;
            }
            ExprVmOpcode::SelectTernary => {}
        }
    }

    Ok(false)
}

fn expr_program_has_unstable_symbols_v2_scalar(
    program: &PortableExprProgram,
    ctx: &dyn PortableExprEvalContext,
    budgets: PortableExprBudgets,
) -> Result<bool, PortableExprError> {
    enforce_program_budgets_v2_scalar(program, budgets.into())?;
    let mut ip = 0usize;

    while ip < program.code.len() {
        let opcode = read_opcode_v2(&program.code, &mut ip)?;
        match opcode {
            ExprVmOpcodeV2::End => return Ok(false),
            ExprVmOpcodeV2::PushLiteral => {
                read_i64_le(&program.code, &mut ip)?;
            }
            ExprVmOpcodeV2::PushCurrentAddress => {}
            ExprVmOpcodeV2::PushSymbol => {
                let symbol_idx = read_u16_le(&program.code, &mut ip)? as usize;
                let Some(symbol_name) = program.symbols.get(symbol_idx) else {
                    return Err(PortableExprError::new(
                        DIAG_EXPR_INVALID_PROGRAM,
                        format!("symbol index out of range: {}", symbol_idx),
                    ));
                };
                if is_symbol_unstable(
                    symbol_name,
                    ctx.pass(),
                    |symbol| ctx.symbol_exists(symbol),
                    |symbol| ctx.symbol_is_finalized(symbol),
                ) {
                    return Ok(true);
                }
            }
            ExprVmOpcodeV2::PushStringLiteral => {
                let len = read_u16_le(&program.code, &mut ip)? as usize;
                read_bytes(&program.code, &mut ip, len)?;
            }
            ExprVmOpcodeV2::ApplyUnary => {
                ExprVmUnary::from_u8(read_u8(&program.code, &mut ip)?).ok_or_else(|| {
                    PortableExprError::new(DIAG_EXPR_INVALID_OPCODE, "invalid unary opcode")
                })?;
            }
            ExprVmOpcodeV2::ApplyBinary => {
                ExprVmBinary::from_u8(read_u8(&program.code, &mut ip)?).ok_or_else(|| {
                    PortableExprError::new(DIAG_EXPR_INVALID_OPCODE, "invalid binary opcode")
                })?;
            }
            ExprVmOpcodeV2::SelectTernary => {}
            ExprVmOpcodeV2::WrapImmediate
            | ExprVmOpcodeV2::WrapIndirect
            | ExprVmOpcodeV2::WrapIndirectLong
            | ExprVmOpcodeV2::IndexValue
            | ExprVmOpcodeV2::RequireScalar
            | ExprVmOpcodeV2::PushPlaceholder => {}
            ExprVmOpcodeV2::BuildTuple | ExprVmOpcodeV2::BuildList => {
                read_u16_le(&program.code, &mut ip)?;
            }
            ExprVmOpcodeV2::BuildRange => {
                read_u8(&program.code, &mut ip)?;
            }
            ExprVmOpcodeV2::BuildStructLiteral => {
                let type_name_idx = read_u16_le(&program.code, &mut ip)? as usize;
                let Some(type_name) = program.symbols.get(type_name_idx) else {
                    return Err(PortableExprError::new(
                        DIAG_EXPR_INVALID_PROGRAM,
                        format!("symbol index out of range: {}", type_name_idx),
                    ));
                };
                if is_symbol_unstable(
                    type_name,
                    ctx.pass(),
                    |symbol| ctx.symbol_exists(symbol),
                    |symbol| ctx.symbol_is_finalized(symbol),
                ) {
                    return Ok(true);
                }
                let field_count = read_u16_le(&program.code, &mut ip)? as usize;
                for _ in 0..field_count {
                    read_u16_le(&program.code, &mut ip)?;
                }
            }
            ExprVmOpcodeV2::GetMember => {
                read_u16_le(&program.code, &mut ip)?;
            }
            ExprVmOpcodeV2::EmitDiag
            | ExprVmOpcodeV2::Fail
            | ExprVmOpcodeV2::PushRegisterRef
            | ExprVmOpcodeV2::CallBuiltin => {
                return Err(PortableExprError::new(
                    DIAG_EXPR_UNSUPPORTED_FEATURE,
                    format!(
                        "expression VM v2 scalar runtime does not yet support opcode 0x{:02X}",
                        opcode as u8
                    ),
                ));
            }
        }
    }

    Ok(false)
}

#[derive(Clone, Copy, Debug)]
struct PortableExprBudgetLimitsV2 {
    max_program_bytes: usize,
    max_stack_depth: usize,
    max_symbol_refs: usize,
    max_eval_steps: usize,
    max_shape_items: usize,
}

impl From<PortableExprBudgets> for PortableExprBudgetLimitsV2 {
    fn from(value: PortableExprBudgets) -> Self {
        Self {
            max_program_bytes: value.max_program_bytes,
            max_stack_depth: value.max_stack_depth,
            max_symbol_refs: value.max_symbol_refs,
            max_eval_steps: value.max_eval_steps,
            max_shape_items: value.max_stack_depth,
        }
    }
}

impl From<PortableExprBudgetsV2> for PortableExprBudgetLimitsV2 {
    fn from(value: PortableExprBudgetsV2) -> Self {
        Self {
            max_program_bytes: value.max_program_bytes,
            max_stack_depth: value.max_stack_depth,
            max_symbol_refs: value.max_symbol_refs,
            max_eval_steps: value.max_eval_steps,
            max_shape_items: value.max_shape_items,
        }
    }
}

fn enforce_program_budgets_v1(
    program: &PortableExprProgram,
    budgets: PortableExprBudgets,
) -> Result<(), PortableExprError> {
    if program.opcode_version != EXPR_VM_OPCODE_VERSION_V1 {
        return Err(unsupported_expression_vm_opcode_version(
            program.opcode_version,
        ));
    }
    if program.code.len() > budgets.max_program_bytes {
        return Err(PortableExprError::new(
            DIAG_EXPR_BUDGET_EXCEEDED,
            format!(
                "expression VM program byte budget exceeded ({} > {})",
                program.code.len(),
                budgets.max_program_bytes
            ),
        ));
    }
    if program.symbols.len() > budgets.max_symbol_refs {
        return Err(PortableExprError::new(
            DIAG_EXPR_BUDGET_EXCEEDED,
            format!(
                "expression VM symbol reference budget exceeded ({} > {})",
                program.symbols.len(),
                budgets.max_symbol_refs
            ),
        ));
    }
    if (program.declared_stack_depth as usize) > budgets.max_stack_depth {
        return Err(PortableExprError::new(
            DIAG_EXPR_BUDGET_EXCEEDED,
            format!(
                "expression VM stack depth budget exceeded ({} > {})",
                program.declared_stack_depth, budgets.max_stack_depth
            ),
        ));
    }
    Ok(())
}

fn enforce_program_budgets_v2_scalar(
    program: &PortableExprProgram,
    budgets: PortableExprBudgetLimitsV2,
) -> Result<(), PortableExprError> {
    enforce_program_budgets_v2_parts(
        program.opcode_version,
        program.code.len(),
        program.symbols.len(),
        program.declared_stack_depth,
        budgets,
    )
}

fn enforce_program_budgets_v2_parts(
    opcode_version: u16,
    code_len: usize,
    symbol_len: usize,
    declared_stack_depth: u16,
    budgets: PortableExprBudgetLimitsV2,
) -> Result<(), PortableExprError> {
    if opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
        return Err(unsupported_expression_vm_opcode_version(opcode_version));
    }
    if code_len > budgets.max_program_bytes {
        return Err(PortableExprError::new(
            DIAG_EXPR_BUDGET_EXCEEDED,
            format!(
                "expression VM program byte budget exceeded ({} > {})",
                code_len, budgets.max_program_bytes
            ),
        ));
    }
    if symbol_len > budgets.max_symbol_refs {
        return Err(PortableExprError::new(
            DIAG_EXPR_BUDGET_EXCEEDED,
            format!(
                "expression VM symbol reference budget exceeded ({} > {})",
                symbol_len, budgets.max_symbol_refs
            ),
        ));
    }
    if (declared_stack_depth as usize) > budgets.max_stack_depth {
        return Err(PortableExprError::new(
            DIAG_EXPR_BUDGET_EXCEEDED,
            format!(
                "expression VM stack depth budget exceeded ({} > {})",
                declared_stack_depth, budgets.max_stack_depth
            ),
        ));
    }
    Ok(())
}

fn eval_portable_expr_program_v2_internal(
    opcode_version: u16,
    code: &[u8],
    symbols: &[String],
    declared_stack_depth: u16,
    ctx: &dyn PortableExprEvalContext,
    budgets: PortableExprBudgetLimitsV2,
) -> Result<PortableExprEvaluationV2, PortableExprError> {
    enforce_program_budgets_v2_parts(
        opcode_version,
        code.len(),
        symbols.len(),
        declared_stack_depth,
        budgets,
    )?;

    let mut stack: Vec<PortableExprValueV2> = Vec::new();
    let mut ip = 0usize;
    let mut steps = 0usize;
    let mut has_symbol_refs = false;
    let mut has_unstable_symbols = false;

    while ip < code.len() {
        steps = steps.saturating_add(1);
        if steps > budgets.max_eval_steps {
            return Err(PortableExprError::new(
                DIAG_EXPR_BUDGET_EXCEEDED,
                format!(
                    "expression evaluation step budget exceeded ({} > {})",
                    steps, budgets.max_eval_steps
                ),
            ));
        }

        let opcode = read_opcode_v2(code, &mut ip)?;
        match opcode {
            ExprVmOpcodeV2::End => break,
            ExprVmOpcodeV2::PushLiteral => {
                stack.push(PortableExprValueV2::Int(read_i64_le(code, &mut ip)?));
                enforce_stack_budget_v2(&stack, budgets)?;
            }
            ExprVmOpcodeV2::PushCurrentAddress => {
                let value = ctx.current_address().ok_or_else(|| {
                    PortableExprError::new(
                        DIAG_EXPR_EVAL_FAILURE,
                        "current address ($) not available",
                    )
                })?;
                stack.push(PortableExprValueV2::Int(value));
                enforce_stack_budget_v2(&stack, budgets)?;
            }
            ExprVmOpcodeV2::PushSymbol => {
                let symbol_name = read_symbol_name(code, &mut ip, symbols)?;
                track_symbol_reference(
                    symbol_name.as_str(),
                    ctx,
                    &mut has_symbol_refs,
                    &mut has_unstable_symbols,
                );
                let value = ctx
                    .lookup_symbol_value(symbol_name.as_str())
                    .ok_or_else(|| {
                        PortableExprError::new(
                            DIAG_EXPR_UNKNOWN_SYMBOL,
                            format!("undefined symbol: {}", symbol_name),
                        )
                    })?;
                stack.push(value);
                enforce_stack_budget_v2(&stack, budgets)?;
            }
            ExprVmOpcodeV2::PushStringLiteral => {
                let len = read_u16_le(code, &mut ip)? as usize;
                let bytes = read_bytes(code, &mut ip, len)?;
                stack.push(PortableExprValueV2::String(bytes.to_vec()));
                enforce_stack_budget_v2(&stack, budgets)?;
            }
            ExprVmOpcodeV2::ApplyUnary => {
                let unary = ExprVmUnary::from_u8(read_u8(code, &mut ip)?).ok_or_else(|| {
                    PortableExprError::new(DIAG_EXPR_INVALID_OPCODE, "invalid unary opcode")
                })?;
                let value = pop_value_v2(&mut stack)?;
                let scalar = portable_expr_value_to_scalar(&value, ctx)?;
                let result = apply_unary(unary.to_core(), scalar, Span::default())
                    .map_err(|err| PortableExprError::new(DIAG_EXPR_EVAL_FAILURE, err.message))?;
                stack.push(PortableExprValueV2::Int(result));
            }
            ExprVmOpcodeV2::ApplyBinary => {
                let binary = ExprVmBinary::from_u8(read_u8(code, &mut ip)?).ok_or_else(|| {
                    PortableExprError::new(DIAG_EXPR_INVALID_OPCODE, "invalid binary opcode")
                })?;
                let right = portable_expr_value_to_scalar(&pop_value_v2(&mut stack)?, ctx)?;
                let left = portable_expr_value_to_scalar(&pop_value_v2(&mut stack)?, ctx)?;
                let result = apply_binary(binary.to_core(), left, right, Span::default())
                    .map_err(|err| PortableExprError::new(DIAG_EXPR_EVAL_FAILURE, err.message))?;
                stack.push(PortableExprValueV2::Int(result));
            }
            ExprVmOpcodeV2::SelectTernary => {
                let else_value = pop_value_v2(&mut stack)?;
                let then_value = pop_value_v2(&mut stack)?;
                let cond_value = portable_expr_value_to_scalar(&pop_value_v2(&mut stack)?, ctx)?;
                stack.push(if cond_value != 0 {
                    then_value
                } else {
                    else_value
                });
            }
            ExprVmOpcodeV2::WrapImmediate => {
                let value = pop_value_v2(&mut stack)?;
                stack.push(PortableExprValueV2::Immediate(Box::new(value)));
            }
            ExprVmOpcodeV2::WrapIndirect => {
                let value = pop_value_v2(&mut stack)?;
                stack.push(PortableExprValueV2::Indirect(Box::new(value)));
            }
            ExprVmOpcodeV2::WrapIndirectLong => {
                let value = pop_value_v2(&mut stack)?;
                stack.push(PortableExprValueV2::IndirectLong(Box::new(value)));
            }
            ExprVmOpcodeV2::PushPlaceholder => {
                stack.push(PortableExprValueV2::Placeholder);
                enforce_stack_budget_v2(&stack, budgets)?;
            }
            ExprVmOpcodeV2::BuildTuple => {
                let count = read_u16_le(code, &mut ip)? as usize;
                enforce_shape_item_budget(count, budgets)?;
                let items = pop_values_v2(&mut stack, count)?;
                stack.push(PortableExprValueV2::Tuple(items));
            }
            ExprVmOpcodeV2::BuildList => {
                let count = read_u16_le(code, &mut ip)? as usize;
                enforce_shape_item_budget(count, budgets)?;
                let items = pop_values_v2(&mut stack, count)?;
                stack.push(PortableExprValueV2::List(items));
            }
            ExprVmOpcodeV2::BuildRange => {
                let flags = read_u8(code, &mut ip)?;
                if flags & !0x03 != 0 {
                    return Err(PortableExprError::new(
                        DIAG_EXPR_INVALID_PROGRAM,
                        format!("invalid v2 range flags: 0x{flags:02X}"),
                    ));
                }
                let has_step = flags & 0x01 != 0;
                let inclusive = flags & 0x02 != 0;
                let step = if has_step {
                    Some(Box::new(PortableExprValueV2::Int(
                        portable_expr_value_to_scalar(&pop_value_v2(&mut stack)?, ctx)?,
                    )))
                } else {
                    None
                };
                let end = portable_expr_value_to_scalar(&pop_value_v2(&mut stack)?, ctx)?;
                let start = portable_expr_value_to_scalar(&pop_value_v2(&mut stack)?, ctx)?;
                stack.push(PortableExprValueV2::Range(PortableExprRangeValueV2 {
                    start: Box::new(PortableExprValueV2::Int(start)),
                    end: Box::new(PortableExprValueV2::Int(end)),
                    step,
                    inclusive,
                }));
            }
            ExprVmOpcodeV2::BuildStructLiteral => {
                let type_name = read_symbol_name(code, &mut ip, symbols)?;
                track_symbol_reference(
                    type_name.as_str(),
                    ctx,
                    &mut has_symbol_refs,
                    &mut has_unstable_symbols,
                );
                let field_count = read_u16_le(code, &mut ip)? as usize;
                enforce_shape_item_budget(field_count, budgets)?;
                let mut field_names = Vec::with_capacity(field_count);
                for _ in 0..field_count {
                    field_names.push(read_symbol_name(code, &mut ip, symbols)?);
                }
                let field_values = pop_values_v2(&mut stack, field_count)?;
                let struct_type = ctx.lookup_symbol_value(type_name.as_str()).ok_or_else(|| {
                    PortableExprError::new(
                        DIAG_EXPR_EVAL_FAILURE,
                        format!("unknown struct type '{}' for struct literal", type_name),
                    )
                })?;
                let PortableExprValueV2::StructType(struct_def) = struct_type else {
                    return Err(PortableExprError::new(
                        DIAG_EXPR_EVAL_FAILURE,
                        format!("unknown struct type '{}' for struct literal", type_name),
                    ));
                };

                let mut seen_fields: HashMap<String, ()> = HashMap::new();
                let mut normalized_fields = Vec::with_capacity(field_count);
                for (field_name, field_value) in field_names.into_iter().zip(field_values) {
                    let Some(def_field) = struct_def.fields.iter().find(|candidate| {
                        candidate
                            .field_name
                            .eq_ignore_ascii_case(field_name.as_str())
                    }) else {
                        return Err(PortableExprError::new(
                            DIAG_EXPR_EVAL_FAILURE,
                            format!(
                                "unknown field '{}' in struct literal for '{}'",
                                field_name, struct_def.type_name
                            ),
                        ));
                    };
                    let field_key = def_field.field_name.to_ascii_uppercase();
                    if seen_fields.insert(field_key, ()).is_some() {
                        return Err(PortableExprError::new(
                            DIAG_EXPR_EVAL_FAILURE,
                            format!(
                                "duplicate field '{}' in struct literal for '{}'",
                                def_field.field_name, struct_def.type_name
                            ),
                        ));
                    }
                    normalized_fields.push(PortableExprStructFieldValueV2 {
                        field_name: def_field.field_name.clone(),
                        value: PortableExprValueV2::Int(portable_expr_value_to_scalar(
                            &field_value,
                            ctx,
                        )?),
                    });
                }

                for required_field in &struct_def.fields {
                    if !seen_fields
                        .contains_key(required_field.field_name.to_ascii_uppercase().as_str())
                    {
                        return Err(PortableExprError::new(
                            DIAG_EXPR_EVAL_FAILURE,
                            format!(
                                "missing required field '{}' in struct literal for '{}'",
                                required_field.field_name, struct_def.type_name
                            ),
                        ));
                    }
                }

                stack.push(PortableExprValueV2::StructLiteral(
                    PortableExprStructLiteralValueV2 {
                        type_name: struct_def.type_name,
                        fields: normalized_fields,
                    },
                ));
            }
            ExprVmOpcodeV2::GetMember => {
                let field_name = read_symbol_name(code, &mut ip, symbols)?;
                let base = pop_value_v2(&mut stack)?;
                let value = match base {
                    PortableExprValueV2::StructType(struct_def) => {
                        let Some(field) = struct_def.fields.iter().find(|candidate| {
                            candidate
                                .field_name
                                .eq_ignore_ascii_case(field_name.as_str())
                        }) else {
                            return Err(PortableExprError::new(
                                DIAG_EXPR_EVAL_FAILURE,
                                format!(
                                    "struct '{}' has no field '{}'",
                                    struct_def.type_name, field_name
                                ),
                            ));
                        };
                        PortableExprValueV2::Int(i64::from(field.offset))
                    }
                    PortableExprValueV2::StructLiteral(instance) => {
                        let Some(field) = instance.fields.iter().find(|candidate| {
                            candidate
                                .field_name
                                .eq_ignore_ascii_case(field_name.as_str())
                        }) else {
                            return Err(PortableExprError::new(
                                DIAG_EXPR_EVAL_FAILURE,
                                format!(
                                    "struct '{}' has no field '{}'",
                                    instance.type_name, field_name
                                ),
                            ));
                        };
                        field.value.clone()
                    }
                    _ => {
                        return Err(PortableExprError::new(
                            DIAG_EXPR_EVAL_FAILURE,
                            "Member expression requires struct base value",
                        ));
                    }
                };
                stack.push(value);
            }
            ExprVmOpcodeV2::IndexValue => {
                let index = portable_expr_value_to_scalar(&pop_value_v2(&mut stack)?, ctx)?;
                if index < 0 {
                    return Err(PortableExprError::new(
                        DIAG_EXPR_EVAL_FAILURE,
                        "Index cannot be negative",
                    ));
                }
                let index = usize::try_from(index).map_err(|_| {
                    PortableExprError::new(DIAG_EXPR_EVAL_FAILURE, "Index out of range")
                })?;
                let base = pop_value_v2(&mut stack)?;
                let value = match base {
                    PortableExprValueV2::List(items) => items.get(index).cloned(),
                    PortableExprValueV2::Range(range) => {
                        range_value_get(&range, index).map(PortableExprValueV2::Int)
                    }
                    _ => None,
                }
                .ok_or_else(|| {
                    PortableExprError::new(DIAG_EXPR_EVAL_FAILURE, "Index out of bounds")
                })?;
                stack.push(value);
            }
            ExprVmOpcodeV2::RequireScalar => {
                let value = pop_value_v2(&mut stack)?;
                stack.push(PortableExprValueV2::Int(portable_expr_value_to_scalar(
                    &value, ctx,
                )?));
            }
            ExprVmOpcodeV2::EmitDiag
            | ExprVmOpcodeV2::Fail
            | ExprVmOpcodeV2::PushRegisterRef
            | ExprVmOpcodeV2::CallBuiltin => {
                return Err(PortableExprError::new(
                    DIAG_EXPR_UNSUPPORTED_FEATURE,
                    format!(
                        "expression VM v2 scalar runtime does not yet support opcode 0x{:02X}",
                        opcode as u8
                    ),
                ));
            }
        }

        enforce_stack_budget_v2(&stack, budgets)?;
    }

    if stack.len() != 1 {
        return Err(PortableExprError::new(
            DIAG_EXPR_INVALID_PROGRAM,
            format!(
                "expression VM must finish with exactly one stack value; got {}",
                stack.len()
            ),
        ));
    }

    Ok(PortableExprEvaluationV2 {
        value: stack.pop().expect("v2 eval should end with one value"),
        has_symbol_refs,
        has_unstable_symbols,
    })
}

fn read_symbol_name(
    code: &[u8],
    ip: &mut usize,
    symbols: &[String],
) -> Result<String, PortableExprError> {
    let symbol_idx = read_u16_le(code, ip)? as usize;
    let Some(symbol_name) = symbols.get(symbol_idx) else {
        return Err(PortableExprError::new(
            DIAG_EXPR_INVALID_PROGRAM,
            format!("symbol index out of range: {}", symbol_idx),
        ));
    };
    Ok(symbol_name.clone())
}

fn track_symbol_reference(
    symbol_name: &str,
    ctx: &dyn PortableExprEvalContext,
    has_symbol_refs: &mut bool,
    has_unstable_symbols: &mut bool,
) {
    *has_symbol_refs = true;
    if is_symbol_unstable(
        symbol_name,
        ctx.pass(),
        |symbol| ctx.symbol_exists(symbol),
        |symbol| ctx.symbol_is_finalized(symbol),
    ) {
        *has_unstable_symbols = true;
    }
}

fn portable_expr_value_to_scalar(
    value: &PortableExprValueV2,
    ctx: &dyn PortableExprEvalContext,
) -> Result<i64, PortableExprError> {
    match value {
        PortableExprValueV2::Int(value) => Ok(*value),
        PortableExprValueV2::String(bytes) => ctx
            .eval_string_literal(bytes)
            .map_err(|message| PortableExprError::new(DIAG_EXPR_EVAL_FAILURE, message)),
        PortableExprValueV2::Immediate(inner)
        | PortableExprValueV2::Indirect(inner)
        | PortableExprValueV2::IndirectLong(inner) => portable_expr_value_to_scalar(inner, ctx),
        PortableExprValueV2::List(_) => Err(PortableExprError::new(
            DIAG_EXPR_EVAL_FAILURE,
            "List cannot be evaluated as scalar expression",
        )),
        PortableExprValueV2::Range(_) => Err(PortableExprError::new(
            DIAG_EXPR_EVAL_FAILURE,
            "Range cannot be evaluated as scalar expression",
        )),
        PortableExprValueV2::StructType(_) => Err(PortableExprError::new(
            DIAG_EXPR_EVAL_FAILURE,
            "Struct cannot be evaluated as scalar expression",
        )),
        PortableExprValueV2::StructLiteral(_) => Err(PortableExprError::new(
            DIAG_EXPR_EVAL_FAILURE,
            "Struct instance cannot be evaluated as scalar expression",
        )),
        PortableExprValueV2::Tuple(_) => Err(PortableExprError::new(
            DIAG_EXPR_EVAL_FAILURE,
            "Tuple cannot be evaluated as expression",
        )),
        PortableExprValueV2::Placeholder => Err(PortableExprError::new(
            DIAG_EXPR_EVAL_FAILURE,
            "Placeholder cannot be evaluated as scalar expression",
        )),
        PortableExprValueV2::SymbolRef(_) | PortableExprValueV2::RegisterRef(_) => {
            Err(PortableExprError::new(
                DIAG_EXPR_EVAL_FAILURE,
                "expression value cannot be reduced to scalar expression",
            ))
        }
    }
}

fn enforce_shape_item_budget(
    item_count: usize,
    budgets: PortableExprBudgetLimitsV2,
) -> Result<(), PortableExprError> {
    if item_count > budgets.max_shape_items {
        return Err(PortableExprError::new(
            DIAG_EXPR_BUDGET_EXCEEDED,
            format!(
                "expression VM structural item budget exceeded ({} > {})",
                item_count, budgets.max_shape_items
            ),
        ));
    }
    Ok(())
}

fn range_value_get(range: &PortableExprRangeValueV2, index: usize) -> Option<i64> {
    let PortableExprValueV2::Int(start) = range.start.as_ref() else {
        return None;
    };
    let PortableExprValueV2::Int(end) = range.end.as_ref() else {
        return None;
    };
    let step = match range.step.as_deref() {
        Some(PortableExprValueV2::Int(step)) => *step,
        Some(_) => return None,
        None => {
            if *start <= *end {
                1
            } else {
                -1
            }
        }
    };
    let end_exclusive = if range.inclusive {
        end.checked_add(step.signum())?
    } else {
        *end
    };
    if step == 0 {
        return None;
    }
    let index_i64 = i64::try_from(index).ok()?;
    let candidate = start.checked_add(step.checked_mul(index_i64)?)?;
    let in_bounds = if step > 0 {
        candidate < end_exclusive
    } else {
        candidate > end_exclusive
    };
    in_bounds.then_some(candidate)
}

pub fn validate_portable_expr_program_v2_skeleton(
    program: &PortableExprProgramV2,
    budgets: PortableExprBudgetsV2,
) -> Result<(), PortableExprError> {
    if program.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
        return Err(PortableExprError::new(
            DIAG_EXPR_INVALID_PROGRAM,
            format!(
                "unsupported expression VM opcode version {}",
                program.opcode_version
            ),
        ));
    }
    if program.code.len() > budgets.max_program_bytes {
        return Err(PortableExprError::new(
            DIAG_EXPR_BUDGET_EXCEEDED,
            format!(
                "expression VM program byte budget exceeded ({} > {})",
                program.code.len(),
                budgets.max_program_bytes
            ),
        ));
    }
    if program.symbols.len() > budgets.max_symbol_refs {
        return Err(PortableExprError::new(
            DIAG_EXPR_BUDGET_EXCEEDED,
            format!(
                "expression VM symbol reference budget exceeded ({} > {})",
                program.symbols.len(),
                budgets.max_symbol_refs
            ),
        ));
    }
    if (program.declared_stack_depth as usize) > budgets.max_stack_depth {
        return Err(PortableExprError::new(
            DIAG_EXPR_BUDGET_EXCEEDED,
            format!(
                "expression VM stack depth budget exceeded ({} > {})",
                program.declared_stack_depth, budgets.max_stack_depth
            ),
        ));
    }

    Ok(())
}

fn enforce_stack_budget(
    stack: &[i64],
    budgets: PortableExprBudgets,
) -> Result<(), PortableExprError> {
    if stack.len() > budgets.max_stack_depth {
        return Err(PortableExprError::new(
            DIAG_EXPR_STACK_DEPTH_EXCEEDED,
            format!(
                "expression VM stack depth exceeded ({} > {})",
                stack.len(),
                budgets.max_stack_depth
            ),
        ));
    }
    Ok(())
}

fn enforce_stack_budget_v2(
    stack: &[PortableExprValueV2],
    budgets: PortableExprBudgetLimitsV2,
) -> Result<(), PortableExprError> {
    if stack.len() > budgets.max_stack_depth {
        return Err(PortableExprError::new(
            DIAG_EXPR_STACK_DEPTH_EXCEEDED,
            format!(
                "expression VM stack depth exceeded ({} > {})",
                stack.len(),
                budgets.max_stack_depth
            ),
        ));
    }
    Ok(())
}

fn pop_value(stack: &mut Vec<i64>) -> Result<i64, PortableExprError> {
    stack.pop().ok_or_else(|| {
        PortableExprError::new(DIAG_EXPR_STACK_UNDERFLOW, "expression VM stack underflow")
    })
}

fn pop_value_v2(
    stack: &mut Vec<PortableExprValueV2>,
) -> Result<PortableExprValueV2, PortableExprError> {
    stack.pop().ok_or_else(|| {
        PortableExprError::new(DIAG_EXPR_STACK_UNDERFLOW, "expression VM stack underflow")
    })
}

fn pop_values_v2(
    stack: &mut Vec<PortableExprValueV2>,
    count: usize,
) -> Result<Vec<PortableExprValueV2>, PortableExprError> {
    if count > stack.len() {
        return Err(PortableExprError::new(
            DIAG_EXPR_STACK_UNDERFLOW,
            "expression VM stack underflow",
        ));
    }
    let start = stack.len() - count;
    Ok(stack.drain(start..).collect())
}

fn read_opcode_v1(code: &[u8], ip: &mut usize) -> Result<ExprVmOpcode, PortableExprError> {
    let opcode = read_u8(code, ip)?;
    ExprVmOpcode::from_u8(opcode).ok_or_else(|| {
        PortableExprError::new(
            DIAG_EXPR_INVALID_OPCODE,
            format!("invalid expression VM opcode: 0x{opcode:02X}"),
        )
    })
}

fn read_opcode_v2(code: &[u8], ip: &mut usize) -> Result<ExprVmOpcodeV2, PortableExprError> {
    let opcode = read_u8(code, ip)?;
    ExprVmOpcodeV2::from_u8(opcode).ok_or_else(|| {
        PortableExprError::new(
            DIAG_EXPR_INVALID_OPCODE,
            format!("invalid expression VM opcode: 0x{opcode:02X}"),
        )
    })
}

fn read_u8(code: &[u8], ip: &mut usize) -> Result<u8, PortableExprError> {
    if *ip >= code.len() {
        return Err(PortableExprError::new(
            DIAG_EXPR_INVALID_PROGRAM,
            "unexpected end of expression VM program",
        ));
    }
    let value = code[*ip];
    *ip += 1;
    Ok(value)
}

fn read_u16_le(code: &[u8], ip: &mut usize) -> Result<u16, PortableExprError> {
    if code.len().saturating_sub(*ip) < 2 {
        return Err(PortableExprError::new(
            DIAG_EXPR_INVALID_PROGRAM,
            "unexpected end of expression VM program (u16)",
        ));
    }
    let value = u16::from_le_bytes([code[*ip], code[*ip + 1]]);
    *ip += 2;
    Ok(value)
}

fn read_i64_le(code: &[u8], ip: &mut usize) -> Result<i64, PortableExprError> {
    if code.len().saturating_sub(*ip) < 8 {
        return Err(PortableExprError::new(
            DIAG_EXPR_INVALID_PROGRAM,
            "unexpected end of expression VM program (i64)",
        ));
    }
    let mut bytes = [0u8; 8];
    bytes.copy_from_slice(&code[*ip..*ip + 8]);
    *ip += 8;
    Ok(i64::from_le_bytes(bytes))
}

fn read_bytes<'a>(
    code: &'a [u8],
    ip: &mut usize,
    len: usize,
) -> Result<&'a [u8], PortableExprError> {
    if code.len().saturating_sub(*ip) < len {
        return Err(PortableExprError::new(
            DIAG_EXPR_INVALID_PROGRAM,
            "unexpected end of expression VM program (bytes)",
        ));
    }
    let start = *ip;
    *ip += len;
    Ok(&code[start..start + len])
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum ExprCompilerMode {
    #[default]
    Scalar,
    ShapePreserving,
}

#[derive(Default)]
struct ExprCompiler {
    opcode_version: u16,
    code: Vec<u8>,
    symbols: Vec<String>,
    symbol_index: HashMap<String, u16>,
    stack_cur: usize,
    stack_max: usize,
}

impl ExprCompiler {
    fn for_opcode_version(
        opcode_version: u16,
        _mode: ExprCompilerMode,
    ) -> Result<Self, PortableExprError> {
        if opcode_version != EXPR_VM_OPCODE_VERSION_V1
            && opcode_version != EXPR_VM_OPCODE_VERSION_V2
        {
            return Err(unsupported_expression_vm_opcode_version(opcode_version));
        }
        Ok(Self {
            opcode_version,
            ..Self::default()
        })
    }

    fn compile(&mut self, expr: &Expr) -> Result<(), PortableExprError> {
        match expr {
            Expr::Number(text, span) => {
                let value = parse_number(text).ok_or_else(|| {
                    PortableExprError::with_span(
                        DIAG_EXPR_EVAL_FAILURE,
                        format!("invalid number: {}", text),
                        *span,
                    )
                })?;
                self.emit_push_literal();
                self.emit_i64(value);
                self.stack_push();
                Ok(())
            }
            Expr::Identifier(name, _) | Expr::Register(name, _) => {
                let symbol_idx = self.intern_symbol(name)?;
                self.emit_push_symbol();
                self.emit_u16(symbol_idx);
                self.stack_push();
                Ok(())
            }
            Expr::List(items, span) => {
                if self.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
                    return Err(PortableExprError::with_span(
                        DIAG_EXPR_UNSUPPORTED_FEATURE,
                        "List cannot be evaluated as scalar expression",
                        *span,
                    ));
                }
                let item_count = u16::try_from(items.len()).map_err(|_| {
                    PortableExprError::new(
                        DIAG_EXPR_BUDGET_EXCEEDED,
                        "expression list exceeds u16 VM item capacity",
                    )
                })?;
                for item in items {
                    self.compile(item)?;
                }
                self.emit_build_list(item_count);
                self.stack_collapse(items.len())?;
                Ok(())
            }
            Expr::Index { base, index, span } => {
                if self.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
                    return Err(PortableExprError::with_span(
                        DIAG_EXPR_UNSUPPORTED_FEATURE,
                        "Index expression cannot be evaluated as scalar expression",
                        *span,
                    ));
                }
                self.compile(base)?;
                self.compile(index)?;
                self.emit_index_value();
                self.stack_pop()?;
                Ok(())
            }
            Expr::Member { base, field, span } => {
                if self.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
                    return Err(PortableExprError::with_span(
                        DIAG_EXPR_UNSUPPORTED_FEATURE,
                        "Member expression cannot be evaluated as scalar expression",
                        *span,
                    ));
                }
                self.compile(base)?;
                let field_idx = self.intern_symbol(field)?;
                self.emit_get_member(field_idx);
                Ok(())
            }
            Expr::StructLiteral {
                type_name,
                fields,
                span,
            } => {
                if self.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
                    return Err(PortableExprError::with_span(
                        DIAG_EXPR_UNSUPPORTED_FEATURE,
                        "Struct literal cannot be evaluated as scalar expression",
                        *span,
                    ));
                }
                let field_count = u16::try_from(fields.len()).map_err(|_| {
                    PortableExprError::new(
                        DIAG_EXPR_BUDGET_EXCEEDED,
                        "struct literal exceeds u16 VM field capacity",
                    )
                })?;
                let type_name_idx = self.intern_symbol(type_name)?;
                let mut field_indices = Vec::with_capacity(fields.len());
                for (field_name, field_expr) in fields {
                    self.compile(field_expr)?;
                    field_indices.push(self.intern_symbol(field_name)?);
                }
                self.emit_build_struct_literal(type_name_idx, field_count, &field_indices);
                self.stack_collapse(fields.len())?;
                Ok(())
            }
            Expr::Call { span, .. } => Err(PortableExprError::with_span(
                DIAG_EXPR_UNSUPPORTED_FEATURE,
                "Call expression cannot be evaluated as scalar expression",
                *span,
            )),
            Expr::Placeholder(span) => Err(PortableExprError::with_span(
                DIAG_EXPR_UNSUPPORTED_FEATURE,
                "Placeholder cannot be evaluated as scalar expression",
                *span,
            )),
            Expr::Dollar(_) => {
                self.emit_push_current_address();
                self.stack_push();
                Ok(())
            }
            Expr::Unary { op, expr, .. } => {
                self.compile(expr)?;
                self.emit_apply_unary(ExprVmUnary::from_core(*op));
                Ok(())
            }
            Expr::Binary {
                op, left, right, ..
            } => {
                self.compile(left)?;
                self.compile(right)?;
                self.emit_apply_binary(ExprVmBinary::from_core(*op));
                self.stack_pop()?;
                Ok(())
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => {
                self.compile(cond)?;
                self.compile(then_expr)?;
                self.compile(else_expr)?;
                self.emit_select_ternary();
                self.stack_pop()?;
                self.stack_pop()?;
                Ok(())
            }
            Expr::Indirect(inner, _) | Expr::IndirectLong(inner, _) | Expr::Immediate(inner, _) => {
                self.compile(inner)
            }
            Expr::Tuple(_, span) => Err(PortableExprError::with_span(
                DIAG_EXPR_UNSUPPORTED_FEATURE,
                "tuple expression is not supported by portable expression VM",
                *span,
            )),
            Expr::Range {
                start,
                end,
                step,
                inclusive,
                span,
            } => {
                if self.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
                    return Err(PortableExprError::with_span(
                        DIAG_EXPR_UNSUPPORTED_FEATURE,
                        "Range cannot be evaluated as scalar expression",
                        *span,
                    ));
                }
                self.compile(start)?;
                self.compile(end)?;
                let has_step = step.is_some();
                if let Some(step_expr) = step {
                    self.compile(step_expr)?;
                }
                self.emit_build_range(has_step, *inclusive);
                self.stack_collapse(if has_step { 3 } else { 2 })?;
                Ok(())
            }
            Expr::String(bytes, _span) => {
                let len = u16::try_from(bytes.len()).map_err(|_| {
                    PortableExprError::new(
                        DIAG_EXPR_BUDGET_EXCEEDED,
                        "string expression literal exceeds u16 VM payload capacity",
                    )
                })?;
                self.emit_push_string_literal();
                self.emit_u16(len);
                self.emit_bytes(bytes);
                self.stack_push();
                Ok(())
            }
            Expr::Error(message, span) => Err(PortableExprError::with_span(
                DIAG_EXPR_UNSUPPORTED_FEATURE,
                message.clone(),
                *span,
            )),
        }
    }

    fn compile_direct_leaf(
        &mut self,
        leaf: &PortableExprDirectLeaf,
    ) -> Result<(), PortableExprError> {
        match leaf {
            PortableExprDirectLeaf::NumberText(text) => {
                let value = parse_number(text).ok_or_else(|| {
                    PortableExprError::new(
                        DIAG_EXPR_EVAL_FAILURE,
                        format!("invalid number: {text}"),
                    )
                })?;
                self.emit_push_literal();
                self.emit_i64(value);
                self.stack_push();
                Ok(())
            }
            PortableExprDirectLeaf::SymbolName(name) => {
                let symbol_idx = self.intern_symbol(name)?;
                self.emit_push_symbol();
                self.emit_u16(symbol_idx);
                self.stack_push();
                Ok(())
            }
            PortableExprDirectLeaf::CurrentAddress => {
                self.emit_push_current_address();
                self.stack_push();
                Ok(())
            }
            PortableExprDirectLeaf::StringLiteral(bytes) => {
                let len = u16::try_from(bytes.len()).map_err(|_| {
                    PortableExprError::new(
                        DIAG_EXPR_BUDGET_EXCEEDED,
                        "string expression literal exceeds u16 VM payload capacity",
                    )
                })?;
                self.emit_push_string_literal();
                self.emit_u16(len);
                self.emit_bytes(bytes);
                self.stack_push();
                Ok(())
            }
        }
    }

    fn compile_direct_scalar(&mut self, expr: &Expr) -> Result<(), PortableExprError> {
        match expr {
            Expr::Number(text, _) => {
                self.compile_direct_leaf(&PortableExprDirectLeaf::NumberText(text.clone()))
            }
            Expr::Identifier(name, _) | Expr::Register(name, _) => {
                self.compile_direct_leaf(&PortableExprDirectLeaf::SymbolName(name.clone()))
            }
            Expr::Dollar(_) => self.compile_direct_leaf(&PortableExprDirectLeaf::CurrentAddress),
            Expr::String(bytes, _) => {
                self.compile_direct_leaf(&PortableExprDirectLeaf::StringLiteral(bytes.clone()))
            }
            Expr::Unary { op, expr, .. } => {
                self.compile_direct_scalar(expr)?;
                self.emit_apply_unary(ExprVmUnary::from_core(*op));
                Ok(())
            }
            Expr::Binary {
                op, left, right, ..
            } => {
                self.compile_direct_scalar(left)?;
                self.compile_direct_scalar(right)?;
                self.emit_apply_binary(ExprVmBinary::from_core(*op));
                self.stack_pop()?;
                Ok(())
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => {
                self.compile_direct_scalar(cond)?;
                self.compile_direct_scalar(then_expr)?;
                self.compile_direct_scalar(else_expr)?;
                self.emit_select_ternary();
                self.stack_pop()?;
                self.stack_pop()?;
                Ok(())
            }
            _ => Err(PortableExprError::new(
                DIAG_EXPR_UNSUPPORTED_FEATURE,
                "expression is outside direct scalar lowering coverage",
            )),
        }
    }

    fn compile_direct_structural(&mut self, expr: &Expr) -> Result<(), PortableExprError> {
        match expr {
            Expr::List(items, span) => {
                if self.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
                    return Err(PortableExprError::with_span(
                        DIAG_EXPR_UNSUPPORTED_FEATURE,
                        "List cannot be evaluated as scalar expression",
                        *span,
                    ));
                }
                let item_count = u16::try_from(items.len()).map_err(|_| {
                    PortableExprError::new(
                        DIAG_EXPR_BUDGET_EXCEEDED,
                        "expression list exceeds u16 VM item capacity",
                    )
                })?;
                for item in items {
                    self.compile_direct_structural(item)?;
                }
                self.emit_build_list(item_count);
                self.stack_collapse(items.len())?;
                Ok(())
            }
            Expr::Range {
                start,
                end,
                step,
                inclusive,
                span,
            } => {
                if self.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
                    return Err(PortableExprError::with_span(
                        DIAG_EXPR_UNSUPPORTED_FEATURE,
                        "Range cannot be evaluated as scalar expression",
                        *span,
                    ));
                }
                self.compile_direct_structural(start)?;
                self.compile_direct_structural(end)?;
                let has_step = step.is_some();
                if let Some(step_expr) = step {
                    self.compile_direct_structural(step_expr)?;
                }
                self.emit_build_range(has_step, *inclusive);
                self.stack_collapse(if has_step { 3 } else { 2 })?;
                Ok(())
            }
            Expr::StructLiteral {
                type_name,
                fields,
                span,
            } => {
                if self.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
                    return Err(PortableExprError::with_span(
                        DIAG_EXPR_UNSUPPORTED_FEATURE,
                        "Struct literal cannot be evaluated as scalar expression",
                        *span,
                    ));
                }
                let field_count = u16::try_from(fields.len()).map_err(|_| {
                    PortableExprError::new(
                        DIAG_EXPR_BUDGET_EXCEEDED,
                        "struct literal exceeds u16 VM field capacity",
                    )
                })?;
                let type_name_idx = self.intern_symbol(type_name)?;
                let mut field_indices = Vec::with_capacity(fields.len());
                for (field_name, field_expr) in fields {
                    self.compile_direct_structural(field_expr)?;
                    field_indices.push(self.intern_symbol(field_name)?);
                }
                self.emit_build_struct_literal(type_name_idx, field_count, &field_indices);
                self.stack_collapse(fields.len())?;
                Ok(())
            }
            _ => self.compile_direct_scalar(expr),
        }
    }

    fn compile_direct_member_index(&mut self, expr: &Expr) -> Result<(), PortableExprError> {
        match expr {
            Expr::Index { base, index, span } => {
                if self.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
                    return Err(PortableExprError::with_span(
                        DIAG_EXPR_UNSUPPORTED_FEATURE,
                        "Index expression cannot be evaluated as scalar expression",
                        *span,
                    ));
                }
                self.compile_direct_member_index(base)?;
                self.compile_direct_member_index(index)?;
                self.emit_index_value();
                self.stack_pop()?;
                Ok(())
            }
            Expr::Member { base, field, span } => {
                if self.opcode_version != EXPR_VM_OPCODE_VERSION_V2 {
                    return Err(PortableExprError::with_span(
                        DIAG_EXPR_UNSUPPORTED_FEATURE,
                        "Member expression cannot be evaluated as scalar expression",
                        *span,
                    ));
                }
                self.compile_direct_member_index(base)?;
                let field_idx = self.intern_symbol(field)?;
                self.emit_get_member(field_idx);
                Ok(())
            }
            _ => self.compile_direct_structural(expr),
        }
    }

    fn intern_symbol(&mut self, name: &str) -> Result<u16, PortableExprError> {
        if let Some(index) = self.symbol_index.get(name) {
            return Ok(*index);
        }
        let next = u16::try_from(self.symbols.len()).map_err(|_| {
            PortableExprError::new(
                DIAG_EXPR_BUDGET_EXCEEDED,
                "expression symbol table exceeds u16 capacity",
            )
        })?;
        self.symbols.push(name.to_string());
        self.symbol_index.insert(name.to_string(), next);
        Ok(next)
    }

    fn stack_push(&mut self) {
        self.stack_cur += 1;
        if self.stack_cur > self.stack_max {
            self.stack_max = self.stack_cur;
        }
    }

    fn stack_pop(&mut self) -> Result<(), PortableExprError> {
        if self.stack_cur == 0 {
            return Err(PortableExprError::new(
                DIAG_EXPR_INVALID_PROGRAM,
                "compiler stack tracking underflow",
            ));
        }
        self.stack_cur -= 1;
        Ok(())
    }

    fn stack_collapse(&mut self, input_count: usize) -> Result<(), PortableExprError> {
        if input_count == 0 {
            self.stack_push();
            return Ok(());
        }
        for _ in 1..input_count {
            self.stack_pop()?;
        }
        Ok(())
    }

    fn emit_end(&mut self) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V1 => ExprVmOpcode::End as u8,
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::End as u8,
            _ => unreachable!("unsupported expression VM opcode version"),
        });
    }

    fn emit_push_literal(&mut self) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V1 => ExprVmOpcode::PushLiteral as u8,
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::PushLiteral as u8,
            _ => unreachable!("unsupported expression VM opcode version"),
        });
    }

    fn emit_push_current_address(&mut self) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V1 => ExprVmOpcode::PushCurrentAddress as u8,
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::PushCurrentAddress as u8,
            _ => unreachable!("unsupported expression VM opcode version"),
        });
    }

    fn emit_push_symbol(&mut self) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V1 => ExprVmOpcode::PushSymbol as u8,
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::PushSymbol as u8,
            _ => unreachable!("unsupported expression VM opcode version"),
        });
    }

    fn emit_push_string_literal(&mut self) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V1 => ExprVmOpcode::PushStringLiteral as u8,
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::PushStringLiteral as u8,
            _ => unreachable!("unsupported expression VM opcode version"),
        });
    }

    fn emit_apply_unary(&mut self, unary: ExprVmUnary) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V1 => ExprVmOpcode::ApplyUnary as u8,
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::ApplyUnary as u8,
            _ => unreachable!("unsupported expression VM opcode version"),
        });
        self.emit_u8(unary as u8);
    }

    fn emit_apply_binary(&mut self, binary: ExprVmBinary) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V1 => ExprVmOpcode::ApplyBinary as u8,
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::ApplyBinary as u8,
            _ => unreachable!("unsupported expression VM opcode version"),
        });
        self.emit_u8(binary as u8);
    }

    fn emit_select_ternary(&mut self) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V1 => ExprVmOpcode::SelectTernary as u8,
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::SelectTernary as u8,
            _ => unreachable!("unsupported expression VM opcode version"),
        });
    }

    fn emit_build_list(&mut self, count: u16) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::BuildList as u8,
            _ => unreachable!("build-list emission requires expression VM opcode version v2"),
        });
        self.emit_u16(count);
    }

    fn emit_build_range(&mut self, has_step: bool, inclusive: bool) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::BuildRange as u8,
            _ => unreachable!("build-range emission requires expression VM opcode version v2"),
        });
        let mut flags = 0u8;
        if has_step {
            flags |= 0x01;
        }
        if inclusive {
            flags |= 0x02;
        }
        self.emit_u8(flags);
    }

    fn emit_build_struct_literal(
        &mut self,
        type_name_idx: u16,
        field_count: u16,
        field_indices: &[u16],
    ) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::BuildStructLiteral as u8,
            _ => {
                unreachable!(
                    "build-struct-literal emission requires expression VM opcode version v2"
                )
            }
        });
        self.emit_u16(type_name_idx);
        self.emit_u16(field_count);
        for field_idx in field_indices {
            self.emit_u16(*field_idx);
        }
    }

    fn emit_get_member(&mut self, field_idx: u16) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::GetMember as u8,
            _ => unreachable!("member emission requires expression VM opcode version v2"),
        });
        self.emit_u16(field_idx);
    }

    fn emit_index_value(&mut self) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::IndexValue as u8,
            _ => unreachable!("index emission requires expression VM opcode version v2"),
        });
    }

    fn emit_require_scalar(&mut self) {
        self.emit_u8(match self.opcode_version {
            EXPR_VM_OPCODE_VERSION_V2 => ExprVmOpcodeV2::RequireScalar as u8,
            _ => unreachable!("require-scalar emission requires expression VM opcode version v2"),
        });
    }

    fn emit_u8(&mut self, value: u8) {
        self.code.push(value);
    }

    fn emit_u16(&mut self, value: u16) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }

    fn emit_i64(&mut self, value: i64) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }

    fn emit_bytes(&mut self, bytes: &[u8]) {
        self.code.extend_from_slice(bytes);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct TestCtx {
        symbols: HashMap<String, i64>,
        values: HashMap<String, PortableExprValueV2>,
        finalized: HashMap<String, bool>,
        addr: Option<i64>,
        pass: u8,
    }

    impl PortableExprEvalContext for TestCtx {
        fn lookup_symbol(&self, name: &str) -> Option<i64> {
            self.symbols.get(name).copied()
        }

        fn lookup_symbol_value(&self, name: &str) -> Option<PortableExprValueV2> {
            self.values.get(name).cloned().or_else(|| {
                self.symbols
                    .get(name)
                    .copied()
                    .map(PortableExprValueV2::Int)
            })
        }

        fn current_address(&self) -> Option<i64> {
            self.addr
        }

        fn pass(&self) -> u8 {
            self.pass
        }

        fn symbol_is_finalized(&self, name: &str) -> Option<bool> {
            self.finalized.get(name).copied()
        }

        fn eval_string_literal(&self, bytes: &[u8]) -> Result<i64, String> {
            match bytes {
                [single] => Ok(*single as i64),
                [hi, lo] => Ok(((*hi as i64) << 8) | (*lo as i64)),
                _ => Err("Multi-character string not allowed in expression.".to_string()),
            }
        }
    }

    fn span() -> Span {
        Span {
            line: 1,
            col_start: 1,
            col_end: 1,
        }
    }

    fn point_struct_type() -> PortableExprValueV2 {
        PortableExprValueV2::StructType(PortableExprStructTypeValueV2 {
            type_name: "Point".to_string(),
            fields: vec![
                PortableExprStructTypeFieldValueV2 {
                    field_name: "x".to_string(),
                    offset: 0,
                    size: 1,
                },
                PortableExprStructTypeFieldValueV2 {
                    field_name: "y".to_string(),
                    offset: 1,
                    size: 1,
                },
            ],
            size: 2,
        })
    }

    #[test]
    fn eval_arithmetic_program() {
        let expr = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Number("2".to_string(), span())),
            right: Box::new(Expr::Binary {
                op: BinaryOp::Multiply,
                left: Box::new(Expr::Number("3".to_string(), span())),
                right: Box::new(Expr::Number("4".to_string(), span())),
                span: span(),
            }),
            span: span(),
        };
        let program = compile_core_expr_to_portable_program(&expr).expect("compile should work");
        let result = eval_portable_expr_program(&program, &TestCtx::default(), Default::default())
            .expect("eval should work");
        assert_eq!(result.value, 14);
    }

    #[test]
    fn eval_dollar_uses_current_address() {
        let expr = Expr::Dollar(span());
        let program = compile_core_expr_to_portable_program(&expr).expect("compile should work");
        let ctx = TestCtx {
            addr: Some(0x1234),
            ..Default::default()
        };
        let result = eval_portable_expr_program(&program, &ctx, Default::default())
            .expect("eval should work");
        assert_eq!(result.value, 0x1234);
    }

    #[test]
    fn unstable_symbol_detection_matches_pass_rules() {
        let expr = Expr::Identifier("label".to_string(), span());
        let program = compile_core_expr_to_portable_program(&expr).expect("compile should work");

        let pass1_unknown = TestCtx {
            pass: 1,
            ..Default::default()
        };
        assert!(
            expr_program_has_unstable_symbols(&program, &pass1_unknown, Default::default())
                .expect("scan should work")
        );

        let pass2_unfinalized = TestCtx {
            symbols: HashMap::from([("label".to_string(), 10)]),
            finalized: HashMap::from([("label".to_string(), false)]),
            pass: 2,
            ..Default::default()
        };
        assert!(expr_program_has_unstable_symbols(
            &program,
            &pass2_unfinalized,
            Default::default()
        )
        .expect("scan should work"));

        let pass2_finalized = TestCtx {
            symbols: HashMap::from([("label".to_string(), 10)]),
            finalized: HashMap::from([("label".to_string(), true)]),
            pass: 2,
            ..Default::default()
        };
        assert!(
            !expr_program_has_unstable_symbols(&program, &pass2_finalized, Default::default())
                .expect("scan should work")
        );
    }

    #[test]
    fn budget_exceeded_reports_stable_code() {
        let expr = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Number("1".to_string(), span())),
            right: Box::new(Expr::Number("2".to_string(), span())),
            span: span(),
        };
        let program = compile_core_expr_to_portable_program(&expr).expect("compile should work");
        let budgets = PortableExprBudgets {
            max_program_bytes: 1,
            ..Default::default()
        };
        let err = eval_portable_expr_program(&program, &TestCtx::default(), budgets)
            .expect_err("budget should fail");
        assert_eq!(err.code, DIAG_EXPR_BUDGET_EXCEEDED);
    }

    #[test]
    fn invalid_opcode_reports_stable_code() {
        let program = PortableExprProgram {
            opcode_version: EXPR_VM_OPCODE_VERSION_V1,
            code: vec![0xFE, ExprVmOpcode::End as u8],
            symbols: Vec::new(),
            declared_stack_depth: 1,
        };
        let err = eval_portable_expr_program(&program, &TestCtx::default(), Default::default())
            .expect_err("invalid opcode should fail");
        assert_eq!(err.code, DIAG_EXPR_INVALID_OPCODE);
    }

    #[test]
    fn expr_vm_v2_skeleton_types_are_constructible() {
        let program = PortableExprProgramV2 {
            opcode_version: EXPR_VM_OPCODE_VERSION_V2,
            code: vec![ExprVmOpcodeV2::End as u8],
            symbols: vec!["label".to_string()],
            declared_stack_depth: 1,
            result_mode: PortableExprResultModeV2::ShapePreserving,
        };
        let budgets = PortableExprBudgetsV2::default();
        validate_portable_expr_program_v2_skeleton(&program, budgets)
            .expect("v2 skeleton validation should accept matching version");

        let value = PortableExprValueV2::StructLiteral(PortableExprStructLiteralValueV2 {
            type_name: "Point".to_string(),
            fields: vec![PortableExprStructFieldValueV2 {
                field_name: "items".to_string(),
                value: PortableExprValueV2::Range(PortableExprRangeValueV2 {
                    start: Box::new(PortableExprValueV2::Int(1)),
                    end: Box::new(PortableExprValueV2::Int(4)),
                    step: Some(Box::new(PortableExprValueV2::Int(2))),
                    inclusive: true,
                }),
            }],
        });

        let evaluation = PortableExprEvaluationV2 {
            value,
            has_symbol_refs: true,
            has_unstable_symbols: false,
        };

        match evaluation.value {
            PortableExprValueV2::StructLiteral(struct_value) => {
                assert_eq!(struct_value.type_name, "Point");
                assert_eq!(struct_value.fields.len(), 1);
            }
            other => panic!("unexpected v2 value scaffold: {other:?}"),
        }
    }

    #[test]
    fn expr_vm_v2_scalar_programs_execute_through_runtime() {
        let expr = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Identifier("value".to_string(), span())),
            right: Box::new(Expr::Unary {
                op: UnaryOp::Minus,
                expr: Box::new(Expr::Number("5".to_string(), span())),
                span: span(),
            }),
            span: span(),
        };
        let program = compile_core_expr_to_portable_program_with_opcode_version(
            &expr,
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect("v2 scalar compile should work");
        assert_eq!(program.opcode_version, EXPR_VM_OPCODE_VERSION_V2);

        let ctx = TestCtx {
            symbols: HashMap::from([("value".to_string(), 12)]),
            ..Default::default()
        };
        let evaluation = eval_portable_expr_program(&program, &ctx, Default::default())
            .expect("v2 scalar eval should work");
        assert_eq!(evaluation.value, 7);
        assert!(evaluation.has_symbol_refs);
    }

    #[test]
    fn expr_vm_v2_scalar_programs_preserve_unstable_symbol_detection() {
        let expr = Expr::Identifier("label".to_string(), span());
        let program = compile_core_expr_to_portable_program_with_opcode_version(
            &expr,
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect("v2 scalar compile should work");

        let pass1_unknown = TestCtx {
            pass: 1,
            ..Default::default()
        };
        assert!(
            expr_program_has_unstable_symbols(&program, &pass1_unknown, Default::default())
                .expect("v2 unstable-symbol scan should work")
        );

        let pass2_finalized = TestCtx {
            symbols: HashMap::from([("label".to_string(), 10)]),
            finalized: HashMap::from([("label".to_string(), true)]),
            pass: 2,
            ..Default::default()
        };
        assert!(
            !expr_program_has_unstable_symbols(&program, &pass2_finalized, Default::default())
                .expect("v2 unstable-symbol scan should work")
        );
    }

    #[test]
    fn expr_vm_v2_scalar_compiler_supports_full_scalar_grammar() {
        let cases = [
            (
                Expr::Binary {
                    op: BinaryOp::LogicOr,
                    left: Box::new(Expr::Binary {
                        op: BinaryOp::Eq,
                        left: Box::new(Expr::Binary {
                            op: BinaryOp::Shl,
                            left: Box::new(Expr::Number("1".to_string(), span())),
                            right: Box::new(Expr::Number("2".to_string(), span())),
                            span: span(),
                        }),
                        right: Box::new(Expr::Number("4".to_string(), span())),
                        span: span(),
                    }),
                    right: Box::new(Expr::Number("0".to_string(), span())),
                    span: span(),
                },
                HashMap::new(),
                1,
            ),
            (
                Expr::Binary {
                    op: BinaryOp::BitOr,
                    left: Box::new(Expr::Binary {
                        op: BinaryOp::BitAnd,
                        left: Box::new(Expr::Identifier("mask".to_string(), span())),
                        right: Box::new(Expr::Identifier("flag".to_string(), span())),
                        span: span(),
                    }),
                    right: Box::new(Expr::Binary {
                        op: BinaryOp::BitXor,
                        left: Box::new(Expr::Identifier("extra".to_string(), span())),
                        right: Box::new(Expr::Identifier("invert".to_string(), span())),
                        span: span(),
                    }),
                    span: span(),
                },
                HashMap::from([
                    ("mask".to_string(), 6),
                    ("flag".to_string(), 3),
                    ("extra".to_string(), 1),
                    ("invert".to_string(), 2),
                ]),
                3,
            ),
            (
                Expr::Ternary {
                    cond: Box::new(Expr::Identifier("flag".to_string(), span())),
                    then_expr: Box::new(Expr::Ternary {
                        cond: Box::new(Expr::Identifier("zero".to_string(), span())),
                        then_expr: Box::new(Expr::Number("8".to_string(), span())),
                        else_expr: Box::new(Expr::Number("9".to_string(), span())),
                        span: span(),
                    }),
                    else_expr: Box::new(Expr::Number("10".to_string(), span())),
                    span: span(),
                },
                HashMap::from([("flag".to_string(), 1), ("zero".to_string(), 0)]),
                9,
            ),
        ];

        for (expr, symbols, expected_value) in cases {
            let program = compile_core_expr_to_portable_program_with_opcode_version(
                &expr,
                EXPR_VM_OPCODE_VERSION_V2,
            )
            .expect("v2 scalar compile should support full scalar grammar");
            assert_eq!(program.opcode_version, EXPR_VM_OPCODE_VERSION_V2);

            let ctx = TestCtx {
                symbols,
                ..Default::default()
            };
            let evaluation = eval_portable_expr_program(&program, &ctx, Default::default())
                .expect("v2 scalar eval should support full scalar grammar");
            assert_eq!(evaluation.value, expected_value);
        }
    }

    #[test]
    fn expr_vm_v2_shape_preserving_programs_evaluate_structural_values() {
        let list_program = compile_core_expr_to_portable_program_v2_shape_preserving(&Expr::List(
            vec![
                Expr::Number("1".to_string(), span()),
                Expr::Number("2".to_string(), span()),
            ],
            span(),
        ))
        .expect("shape-preserving list compile");

        let list_eval = eval_portable_expr_program_v2(
            &list_program,
            &TestCtx::default(),
            PortableExprBudgetsV2::default(),
        )
        .expect("shape-preserving list eval");
        assert_eq!(
            list_eval.value,
            PortableExprValueV2::List(vec![
                PortableExprValueV2::Int(1),
                PortableExprValueV2::Int(2),
            ])
        );

        let struct_program =
            compile_core_expr_to_portable_program_v2_shape_preserving(&Expr::StructLiteral {
                type_name: "Point".to_string(),
                fields: vec![
                    ("x".to_string(), Expr::Number("4".to_string(), span())),
                    ("y".to_string(), Expr::Number("7".to_string(), span())),
                ],
                span: span(),
            })
            .expect("shape-preserving struct compile");
        let struct_ctx = TestCtx {
            values: HashMap::from([("Point".to_string(), point_struct_type())]),
            ..Default::default()
        };
        let struct_eval = eval_portable_expr_program_v2(
            &struct_program,
            &struct_ctx,
            PortableExprBudgetsV2::default(),
        )
        .expect("shape-preserving struct eval");
        match struct_eval.value {
            PortableExprValueV2::StructLiteral(instance) => {
                assert_eq!(instance.type_name, "Point");
                assert_eq!(instance.fields.len(), 2);
            }
            other => panic!("unexpected struct value: {other:?}"),
        }
    }

    #[test]
    fn expr_vm_v2_opcode_from_u8_round_trip_and_unknown_rejection() {
        let opcodes = [
            (0x00, ExprVmOpcodeV2::End),
            (0x01, ExprVmOpcodeV2::EmitDiag),
            (0x02, ExprVmOpcodeV2::Fail),
            (0x10, ExprVmOpcodeV2::PushLiteral),
            (0x70, ExprVmOpcodeV2::RequireScalar),
        ];

        for (byte, opcode) in opcodes {
            assert_eq!(ExprVmOpcodeV2::from_u8(byte), Some(opcode));
            assert_eq!(opcode as u8, byte);
        }
        assert_eq!(ExprVmOpcodeV2::from_u8(0x03), None);
        assert_eq!(ExprVmOpcodeV2::from_u8(0xFF), None);
    }

    #[test]
    fn stack_underflow_reports_stable_code() {
        let program = PortableExprProgram {
            opcode_version: EXPR_VM_OPCODE_VERSION_V1,
            code: vec![ExprVmOpcode::ApplyUnary as u8, ExprVmUnary::Plus as u8],
            symbols: Vec::new(),
            declared_stack_depth: 1,
        };

        let err_a = eval_portable_expr_program(&program, &TestCtx::default(), Default::default())
            .expect_err("stack underflow should fail");
        let err_b = eval_portable_expr_program(&program, &TestCtx::default(), Default::default())
            .expect_err("stack underflow should fail deterministically");

        assert_eq!(err_a.code, DIAG_EXPR_STACK_UNDERFLOW);
        assert_eq!(err_b.code, DIAG_EXPR_STACK_UNDERFLOW);
    }

    #[test]
    fn unknown_symbol_reports_stable_code() {
        let expr = Expr::Identifier("missing_symbol".to_string(), span());
        let program = compile_core_expr_to_portable_program(&expr).expect("compile should work");

        let err_a = eval_portable_expr_program(&program, &TestCtx::default(), Default::default())
            .expect_err("unknown symbol should fail");
        let err_b = eval_portable_expr_program(&program, &TestCtx::default(), Default::default())
            .expect_err("unknown symbol should fail deterministically");

        assert_eq!(err_a.code, DIAG_EXPR_UNKNOWN_SYMBOL);
        assert_eq!(err_b.code, DIAG_EXPR_UNKNOWN_SYMBOL);
    }

    #[test]
    fn divide_by_zero_reports_stable_code() {
        let expr = Expr::Binary {
            op: BinaryOp::Divide,
            left: Box::new(Expr::Number("1".to_string(), span())),
            right: Box::new(Expr::Number("0".to_string(), span())),
            span: span(),
        };
        let program = compile_core_expr_to_portable_program(&expr).expect("compile should work");

        let err_a = eval_portable_expr_program(&program, &TestCtx::default(), Default::default())
            .expect_err("divide by zero should fail");
        let err_b = eval_portable_expr_program(&program, &TestCtx::default(), Default::default())
            .expect_err("divide by zero should fail deterministically");

        assert_eq!(err_a.code, DIAG_EXPR_EVAL_FAILURE);
        assert_eq!(err_b.code, DIAG_EXPR_EVAL_FAILURE);
    }

    #[test]
    fn ternary_opcode_selects_true_and_false_branches() {
        let expr_true = Expr::Ternary {
            cond: Box::new(Expr::Number("1".to_string(), span())),
            then_expr: Box::new(Expr::Number("10".to_string(), span())),
            else_expr: Box::new(Expr::Number("20".to_string(), span())),
            span: span(),
        };
        let expr_false = Expr::Ternary {
            cond: Box::new(Expr::Number("0".to_string(), span())),
            then_expr: Box::new(Expr::Number("10".to_string(), span())),
            else_expr: Box::new(Expr::Number("20".to_string(), span())),
            span: span(),
        };

        let program_true =
            compile_core_expr_to_portable_program(&expr_true).expect("compile true ternary");
        let program_false =
            compile_core_expr_to_portable_program(&expr_false).expect("compile false ternary");

        let result_true = eval_portable_expr_program(
            &program_true,
            &TestCtx::default(),
            PortableExprBudgets::default(),
        )
        .expect("eval true ternary");
        let result_false = eval_portable_expr_program(
            &program_false,
            &TestCtx::default(),
            PortableExprBudgets::default(),
        )
        .expect("eval false ternary");

        assert_eq!(result_true.value, 10);
        assert_eq!(result_false.value, 20);
    }

    #[test]
    fn wrapper_expressions_compile_to_inner_program_for_both_versions() {
        let inner = Expr::Identifier("label".to_string(), span());
        let expr_indirect = Expr::Indirect(Box::new(inner.clone()), span());
        let expr_indirect_long = Expr::IndirectLong(Box::new(inner.clone()), span());
        let expr_immediate = Expr::Immediate(Box::new(inner), span());

        for opcode_version in [EXPR_VM_OPCODE_VERSION_V1, EXPR_VM_OPCODE_VERSION_V2] {
            let direct_program = compile_core_expr_to_portable_program_with_opcode_version(
                &Expr::Identifier("label".to_string(), span()),
                opcode_version,
            )
            .expect("compile direct identifier");
            let indirect_program = compile_core_expr_to_portable_program_with_opcode_version(
                &expr_indirect,
                opcode_version,
            )
            .expect("compile indirect");
            let indirect_long_program = compile_core_expr_to_portable_program_with_opcode_version(
                &expr_indirect_long,
                opcode_version,
            )
            .expect("compile indirect long");
            let immediate_program = compile_core_expr_to_portable_program_with_opcode_version(
                &expr_immediate,
                opcode_version,
            )
            .expect("compile immediate");

            assert_eq!(indirect_program.code, direct_program.code);
            assert_eq!(indirect_program.symbols, direct_program.symbols);
            assert_eq!(indirect_long_program.code, direct_program.code);
            assert_eq!(indirect_long_program.symbols, direct_program.symbols);
            assert_eq!(immediate_program.code, direct_program.code);
            assert_eq!(immediate_program.symbols, direct_program.symbols);
        }
    }

    #[test]
    fn generic_value_nodes_report_scalar_eval_boundary() {
        let one = || Expr::Number("1".to_string(), span());
        let compile_reject_cases = vec![
            (
                Expr::List(vec![one()], span()),
                "List cannot be evaluated as scalar expression",
            ),
            (
                Expr::Index {
                    base: Box::new(Expr::Identifier("arr".to_string(), span())),
                    index: Box::new(one()),
                    span: span(),
                },
                "Index expression cannot be evaluated as scalar expression",
            ),
            (
                Expr::Member {
                    base: Box::new(Expr::Identifier("value".to_string(), span())),
                    field: "field".to_string(),
                    span: span(),
                },
                "Member expression cannot be evaluated as scalar expression",
            ),
            (
                Expr::StructLiteral {
                    type_name: "Point".to_string(),
                    fields: vec![("x".to_string(), one())],
                    span: span(),
                },
                "Struct literal cannot be evaluated as scalar expression",
            ),
            (
                Expr::Call {
                    name: ".pick".to_string(),
                    args: vec![one()],
                    span: span(),
                },
                "Call expression cannot be evaluated as scalar expression",
            ),
            (
                Expr::Placeholder(span()),
                "Placeholder cannot be evaluated as scalar expression",
            ),
            (
                Expr::Range {
                    start: Box::new(one()),
                    end: Box::new(Expr::Number("4".to_string(), span())),
                    step: None,
                    inclusive: false,
                    span: span(),
                },
                "Range cannot be evaluated as scalar expression",
            ),
        ];

        for (expr, message) in compile_reject_cases {
            let err = compile_core_expr_to_portable_program(&expr)
                .expect_err("generic value node should reject scalar compilation");
            assert_eq!(err.code, DIAG_EXPR_UNSUPPORTED_FEATURE);
            assert_eq!(err.message, message);
            assert_eq!(err.span, Some(span()));
        }

        let boundary_ctx = TestCtx {
            values: HashMap::from([("Point".to_string(), point_struct_type())]),
            ..Default::default()
        };
        let boundary_cases = vec![
            (
                Expr::List(vec![one()], span()),
                "List cannot be evaluated as scalar expression",
            ),
            (
                Expr::StructLiteral {
                    type_name: "Point".to_string(),
                    fields: vec![("x".to_string(), one()), ("y".to_string(), one())],
                    span: span(),
                },
                "Struct instance cannot be evaluated as scalar expression",
            ),
            (
                Expr::Range {
                    start: Box::new(one()),
                    end: Box::new(Expr::Number("4".to_string(), span())),
                    step: None,
                    inclusive: false,
                    span: span(),
                },
                "Range cannot be evaluated as scalar expression",
            ),
        ];

        for (expr, message) in boundary_cases {
            let program = compile_core_expr_to_portable_program_with_opcode_version(
                &expr,
                EXPR_VM_OPCODE_VERSION_V2,
            )
            .expect("v2 scalar compile should succeed for structural expression");
            let err = eval_portable_expr_program(&program, &boundary_ctx, Default::default())
                .expect_err("structural scalar boundary should reject irreducible value");
            assert_eq!(err.code, DIAG_EXPR_EVAL_FAILURE);
            assert_eq!(err.message, message);
        }

        let reduction_cases = vec![
            (
                Expr::Index {
                    base: Box::new(Expr::List(
                        vec![
                            Expr::Number("10".to_string(), span()),
                            Expr::Number("20".to_string(), span()),
                            Expr::Number("30".to_string(), span()),
                        ],
                        span(),
                    )),
                    index: Box::new(Expr::Number("1".to_string(), span())),
                    span: span(),
                },
                20,
            ),
            (
                Expr::Member {
                    base: Box::new(Expr::StructLiteral {
                        type_name: "Point".to_string(),
                        fields: vec![
                            ("x".to_string(), Expr::Number("4".to_string(), span())),
                            ("y".to_string(), Expr::Number("7".to_string(), span())),
                        ],
                        span: span(),
                    }),
                    field: "y".to_string(),
                    span: span(),
                },
                7,
            ),
            (
                Expr::Member {
                    base: Box::new(Expr::Identifier("Point".to_string(), span())),
                    field: "y".to_string(),
                    span: span(),
                },
                1,
            ),
        ];

        for (expr, expected_value) in reduction_cases {
            let program = compile_core_expr_to_portable_program_with_opcode_version(
                &expr,
                EXPR_VM_OPCODE_VERSION_V2,
            )
            .expect("v2 scalar compile should support structural reductions");
            let evaluation =
                eval_portable_expr_program(&program, &boundary_ctx, Default::default())
                    .expect("structural reduction should produce a scalar result");
            assert_eq!(evaluation.value, expected_value);
        }
    }

    #[test]
    fn string_literal_opcode_evaluates_via_context_for_both_versions() {
        let expr = Expr::String(vec![0x41], span());

        for opcode_version in [EXPR_VM_OPCODE_VERSION_V1, EXPR_VM_OPCODE_VERSION_V2] {
            let program =
                compile_core_expr_to_portable_program_with_opcode_version(&expr, opcode_version)
                    .expect("compile string");

            let result = eval_portable_expr_program(
                &program,
                &TestCtx::default(),
                PortableExprBudgets::default(),
            )
            .expect("eval string literal");

            assert_eq!(result.value, 0x41);
        }
    }

    #[test]
    fn direct_leaf_program_builder_emits_expected_v2_symbol_program() {
        let program = compile_portable_expr_direct_leaf_to_program_with_opcode_version(
            &PortableExprDirectLeaf::SymbolName("label".to_string()),
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect("compile direct symbol leaf");

        assert_eq!(program.opcode_version, EXPR_VM_OPCODE_VERSION_V2);
        assert_eq!(program.symbols, vec!["label".to_string()]);
        assert_eq!(program.declared_stack_depth, 1);
        assert_eq!(
            program.code,
            vec![
                ExprVmOpcodeV2::PushSymbol as u8,
                0,
                0,
                ExprVmOpcodeV2::RequireScalar as u8,
                ExprVmOpcodeV2::End as u8,
            ]
        );

        let ctx = TestCtx {
            symbols: HashMap::from([("label".to_string(), 99)]),
            ..Default::default()
        };
        let evaluation = eval_portable_expr_program(&program, &ctx, PortableExprBudgets::default())
            .expect("eval direct symbol leaf");
        assert_eq!(evaluation.value, 99);
    }

    #[test]
    fn legacy_expr_compiler_failpoint_does_not_block_direct_leaf_program_builder() {
        struct FailpointReset;

        impl Drop for FailpointReset {
            fn drop(&mut self) {
                set_legacy_expr_compiler_failpoint_for_tests(false);
            }
        }

        let _reset = FailpointReset;
        set_legacy_expr_compiler_failpoint_for_tests(true);

        let direct_program = compile_portable_expr_direct_leaf_to_program_with_opcode_version(
            &PortableExprDirectLeaf::NumberText("42".to_string()),
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect("direct leaf builder should bypass legacy compiler failpoint");
        let direct_eval = eval_portable_expr_program(
            &direct_program,
            &TestCtx::default(),
            PortableExprBudgets::default(),
        )
        .expect("direct leaf eval should succeed");
        assert_eq!(direct_eval.value, 42);

        let err = compile_core_expr_to_portable_program_with_opcode_version(
            &Expr::Number("42".to_string(), span()),
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect_err("legacy expr compiler failpoint should still trip the legacy compiler path");
        assert_eq!(err.code, DIAG_EXPR_INVALID_PROGRAM);
        assert_eq!(err.message, "legacy expression compiler failpoint");
    }

    #[test]
    fn direct_scalar_program_builder_matches_legacy_v2_program_for_mixed_scalar_expression() {
        let expr = Expr::Ternary {
            cond: Box::new(Expr::Binary {
                op: BinaryOp::LogicAnd,
                left: Box::new(Expr::Binary {
                    op: BinaryOp::Eq,
                    left: Box::new(Expr::Binary {
                        op: BinaryOp::Shl,
                        left: Box::new(Expr::Number("1".to_string(), span())),
                        right: Box::new(Expr::Number("2".to_string(), span())),
                        span: span(),
                    }),
                    right: Box::new(Expr::Number("4".to_string(), span())),
                    span: span(),
                }),
                right: Box::new(Expr::Unary {
                    op: UnaryOp::LogicNot,
                    expr: Box::new(Expr::Number("0".to_string(), span())),
                    span: span(),
                }),
                span: span(),
            }),
            then_expr: Box::new(Expr::Unary {
                op: UnaryOp::Minus,
                expr: Box::new(Expr::Identifier("target".to_string(), span())),
                span: span(),
            }),
            else_expr: Box::new(Expr::Dollar(span())),
            span: span(),
        };

        let direct_program = compile_portable_expr_direct_scalar_to_program_with_opcode_version(
            &expr,
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect("compile direct scalar expression");
        let legacy_program = compile_core_expr_to_portable_program_with_opcode_version(
            &expr,
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect("compile legacy scalar expression");

        assert_eq!(direct_program, legacy_program);
    }

    #[test]
    fn legacy_expr_compiler_failpoint_does_not_block_direct_scalar_program_builder() {
        struct FailpointReset;

        impl Drop for FailpointReset {
            fn drop(&mut self) {
                set_legacy_expr_compiler_failpoint_for_tests(false);
            }
        }

        let _reset = FailpointReset;
        set_legacy_expr_compiler_failpoint_for_tests(true);

        let expr = Expr::Ternary {
            cond: Box::new(Expr::Number("0".to_string(), span())),
            then_expr: Box::new(Expr::Number("1".to_string(), span())),
            else_expr: Box::new(Expr::Binary {
                op: BinaryOp::Add,
                left: Box::new(Expr::Unary {
                    op: UnaryOp::Minus,
                    expr: Box::new(Expr::Identifier("target".to_string(), span())),
                    span: span(),
                }),
                right: Box::new(Expr::Dollar(span())),
                span: span(),
            }),
            span: span(),
        };

        let direct_program = compile_portable_expr_direct_scalar_to_program_with_opcode_version(
            &expr,
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect("direct scalar builder should bypass legacy compiler failpoint");
        let direct_eval = eval_portable_expr_program(
            &direct_program,
            &TestCtx {
                symbols: HashMap::from([("target".to_string(), 40)]),
                addr: Some(2),
                pass: 2,
                ..Default::default()
            },
            PortableExprBudgets::default(),
        )
        .expect("direct scalar eval should succeed");
        assert_eq!(direct_eval.value, -38);

        let err = compile_core_expr_to_portable_program_with_opcode_version(
            &expr,
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect_err("legacy expr compiler failpoint should still trip the legacy compiler path");
        assert_eq!(err.code, DIAG_EXPR_INVALID_PROGRAM);
        assert_eq!(err.message, "legacy expression compiler failpoint");
    }

    #[test]
    fn direct_structural_program_builder_matches_legacy_v2_program_for_constructor_cases() {
        let cases = [
            Expr::List(
                vec![
                    Expr::Number("1".to_string(), span()),
                    Expr::Range {
                        start: Box::new(Expr::Number("2".to_string(), span())),
                        end: Box::new(Expr::Number("6".to_string(), span())),
                        step: Some(Box::new(Expr::Number("2".to_string(), span()))),
                        inclusive: true,
                        span: span(),
                    },
                ],
                span(),
            ),
            Expr::StructLiteral {
                type_name: "Point".to_string(),
                fields: vec![
                    ("x".to_string(), Expr::Number("1".to_string(), span())),
                    (
                        "y".to_string(),
                        Expr::List(
                            vec![
                                Expr::Number("2".to_string(), span()),
                                Expr::Number("3".to_string(), span()),
                            ],
                            span(),
                        ),
                    ),
                ],
                span: span(),
            },
        ];

        for expr in cases {
            let direct_program =
                compile_portable_expr_direct_structural_to_program_with_opcode_version(
                    &expr,
                    EXPR_VM_OPCODE_VERSION_V2,
                )
                .expect("compile direct structural expression");
            let legacy_program = compile_core_expr_to_portable_program_with_opcode_version(
                &expr,
                EXPR_VM_OPCODE_VERSION_V2,
            )
            .expect("compile legacy structural expression");

            assert_eq!(direct_program, legacy_program);
        }
    }

    #[test]
    fn legacy_expr_compiler_failpoint_does_not_block_direct_structural_program_builder() {
        struct FailpointReset;

        impl Drop for FailpointReset {
            fn drop(&mut self) {
                set_legacy_expr_compiler_failpoint_for_tests(false);
            }
        }

        let _reset = FailpointReset;
        set_legacy_expr_compiler_failpoint_for_tests(true);

        let expr = Expr::List(
            vec![
                Expr::Number("1".to_string(), span()),
                Expr::Number("2".to_string(), span()),
            ],
            span(),
        );

        let direct_program =
            compile_portable_expr_direct_structural_to_program_with_opcode_version(
                &expr,
                EXPR_VM_OPCODE_VERSION_V2,
            )
            .expect("direct structural builder should bypass legacy compiler failpoint");
        let err = eval_portable_expr_program(
            &direct_program,
            &TestCtx::default(),
            PortableExprBudgets::default(),
        )
        .expect_err("list root should still fail at the scalar boundary");
        assert_eq!(err.message, "List cannot be evaluated as scalar expression");

        let legacy_err = compile_core_expr_to_portable_program_with_opcode_version(
            &expr,
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect_err("legacy expr compiler failpoint should still trip the legacy compiler path");
        assert_eq!(legacy_err.code, DIAG_EXPR_INVALID_PROGRAM);
        assert_eq!(legacy_err.message, "legacy expression compiler failpoint");
    }

    #[test]
    fn direct_member_index_program_builder_matches_legacy_v2_program_for_access_cases() {
        let cases = [
            Expr::Index {
                base: Box::new(Expr::List(
                    vec![
                        Expr::Number("10".to_string(), span()),
                        Expr::Number("20".to_string(), span()),
                        Expr::Number("30".to_string(), span()),
                    ],
                    span(),
                )),
                index: Box::new(Expr::Number("1".to_string(), span())),
                span: span(),
            },
            Expr::Member {
                base: Box::new(Expr::StructLiteral {
                    type_name: "Point".to_string(),
                    fields: vec![
                        ("x".to_string(), Expr::Number("4".to_string(), span())),
                        ("y".to_string(), Expr::Number("7".to_string(), span())),
                    ],
                    span: span(),
                }),
                field: "x".to_string(),
                span: span(),
            },
        ];

        for expr in cases {
            let direct_program =
                compile_portable_expr_direct_member_index_to_program_with_opcode_version(
                    &expr,
                    EXPR_VM_OPCODE_VERSION_V2,
                )
                .expect("compile direct member/index expression");
            let legacy_program = compile_core_expr_to_portable_program_with_opcode_version(
                &expr,
                EXPR_VM_OPCODE_VERSION_V2,
            )
            .expect("compile legacy member/index expression");

            assert_eq!(direct_program, legacy_program);
        }
    }

    #[test]
    fn legacy_expr_compiler_failpoint_does_not_block_direct_member_index_program_builder() {
        struct FailpointReset;

        impl Drop for FailpointReset {
            fn drop(&mut self) {
                set_legacy_expr_compiler_failpoint_for_tests(false);
            }
        }

        let _reset = FailpointReset;
        set_legacy_expr_compiler_failpoint_for_tests(true);

        let expr = Expr::Member {
            base: Box::new(Expr::StructLiteral {
                type_name: "Point".to_string(),
                fields: vec![
                    ("x".to_string(), Expr::Number("4".to_string(), span())),
                    ("y".to_string(), Expr::Number("7".to_string(), span())),
                ],
                span: span(),
            }),
            field: "x".to_string(),
            span: span(),
        };

        let direct_program =
            compile_portable_expr_direct_member_index_to_program_with_opcode_version(
                &expr,
                EXPR_VM_OPCODE_VERSION_V2,
            )
            .expect("direct member/index builder should bypass legacy compiler failpoint");
        let direct_eval = eval_portable_expr_program(
            &direct_program,
            &TestCtx {
                values: HashMap::from([("Point".to_string(), point_struct_type())]),
                pass: 2,
                ..Default::default()
            },
            PortableExprBudgets::default(),
        )
        .expect("direct member/index eval should succeed");
        assert_eq!(direct_eval.value, 4);

        let legacy_err = compile_core_expr_to_portable_program_with_opcode_version(
            &expr,
            EXPR_VM_OPCODE_VERSION_V2,
        )
        .expect_err("legacy expr compiler failpoint should still trip the legacy compiler path");
        assert_eq!(legacy_err.code, DIAG_EXPR_INVALID_PROGRAM);
        assert_eq!(legacy_err.message, "legacy expression compiler failpoint");
    }

    #[test]
    fn hex_suffix_literal_with_0b_prefix_chars_compiles() {
        let expr = Expr::Number("0B8H".to_string(), span());
        let program = compile_core_expr_to_portable_program(&expr).expect("compile literal");
        let result = eval_portable_expr_program(
            &program,
            &TestCtx::default(),
            PortableExprBudgets::default(),
        )
        .expect("eval literal");

        assert_eq!(result.value, 0x0B8);
    }
}
