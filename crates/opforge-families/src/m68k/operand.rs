// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68000 operand types for the baseline parser slice.

use opcore::expression::expr_span;
use opcore::parser::Expr;
use opcore::tokenizer::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndexSize {
    Word,
    Long,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AbsoluteSize {
    Word,
    Long,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndexScale {
    One,
    Two,
    Four,
    Eight,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpecialRegisterKind {
    Ccr,
    Sr,
    Usp,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ControlRegisterKind {
    Sfc,
    Dfc,
    Vbr,
    Cacr,
    Caar,
    Msp,
    Isp,
    Tc,
    Itt0,
    Itt1,
    Dtt0,
    Dtt1,
    Mmusr,
    Urp,
    Srp,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FpuControlRegisterKind {
    Fpcr,
    Fpsr,
    Fpiar,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RegisterListRegister {
    Data(u8),
    Address(u8),
    FpuData(u8),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FullExtensionBase {
    Address(String),
    Pc,
    Suppressed,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FullExtensionIndex {
    pub register: String,
    pub size: IndexSize,
    pub scale: IndexScale,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryIndirectionKind {
    Preindexed,
    Postindexed,
}

#[derive(Clone, Debug)]
pub enum BitFieldSelector {
    DataRegister { register: String, span: Span },
    Immediate { expr: Expr, span: Span },
}

#[derive(Clone, Debug)]
pub enum FamilyOperand {
    DataRegister {
        register: String,
        span: Span,
    },
    AddressRegister {
        register: String,
        span: Span,
    },
    SpecialRegister {
        register: SpecialRegisterKind,
        span: Span,
    },
    ControlRegister {
        register: ControlRegisterKind,
        span: Span,
    },
    FpuDataRegister {
        register: String,
        span: Span,
    },
    FpuControlRegister {
        register: FpuControlRegisterKind,
        span: Span,
    },
    AddressIndirect {
        register: String,
        span: Span,
    },
    AddressPostincrement {
        register: String,
        span: Span,
    },
    AddressPredecrement {
        register: String,
        span: Span,
    },
    AddressDisplacement {
        displacement: Expr,
        base: String,
        span: Span,
    },
    AddressIndexed {
        displacement: Expr,
        base: String,
        index: String,
        index_size: IndexSize,
        span: Span,
    },
    PcDisplacement {
        displacement: Expr,
        span: Span,
    },
    PcIndexed {
        displacement: Expr,
        index: String,
        index_size: IndexSize,
        span: Span,
    },
    FullExtension {
        base_displacement: Option<(Expr, AbsoluteSize)>,
        base: FullExtensionBase,
        index: Option<FullExtensionIndex>,
        memory_indirection: Option<MemoryIndirectionKind>,
        outer_displacement: Option<(Expr, AbsoluteSize)>,
        span: Span,
    },
    Absolute {
        expr: Expr,
        size: AbsoluteSize,
        span: Span,
    },
    RegisterPair {
        left: String,
        right: String,
        span: Span,
    },
    IndirectRegisterPair {
        left: String,
        right: String,
        span: Span,
    },
    BitField {
        base: Box<FamilyOperand>,
        offset: BitFieldSelector,
        width: BitFieldSelector,
        span: Span,
    },
    RegisterList {
        registers: Vec<RegisterListRegister>,
        span: Span,
    },
    BranchTarget {
        expr: Expr,
        span: Span,
    },
    Immediate {
        expr: Expr,
        span: Span,
    },
}

impl FamilyOperand {
    pub fn span(&self) -> Span {
        match self {
            Self::DataRegister { span, .. }
            | Self::AddressRegister { span, .. }
            | Self::SpecialRegister { span, .. }
            | Self::ControlRegister { span, .. }
            | Self::FpuDataRegister { span, .. }
            | Self::FpuControlRegister { span, .. }
            | Self::AddressIndirect { span, .. }
            | Self::AddressPostincrement { span, .. }
            | Self::AddressPredecrement { span, .. }
            | Self::AddressDisplacement { span, .. }
            | Self::AddressIndexed { span, .. }
            | Self::PcDisplacement { span, .. }
            | Self::PcIndexed { span, .. }
            | Self::FullExtension { span, .. }
            | Self::Absolute { span, .. }
            | Self::RegisterPair { span, .. }
            | Self::IndirectRegisterPair { span, .. }
            | Self::BitField { span, .. }
            | Self::RegisterList { span, .. }
            | Self::BranchTarget { span, .. }
            | Self::Immediate { span, .. } => *span,
        }
    }
}

pub type Operand = FamilyOperand;

pub fn span_from_exprs(start: Span, end: Span) -> Span {
    Span {
        line: start.line,
        col_start: start.col_start,
        col_end: end.col_end,
    }
}

pub fn span_from_expr(expr: &Expr) -> Span {
    expr_span(expr)
}
