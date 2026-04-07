// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68000 family module.

use std::any::Any;

use super::{FamilyOperand, M68KFamilyHandler, Operand};
use opcore::parser::Expr;
use registry::cpu::{CpuFamily, CpuType};
use registry::family::{AssemblerContext, EncodeResult, FamilyHandler, FamilyParseError};
use registry::registry::{
    DialectModule, FamilyHandlerDyn, FamilyModule, FamilyOperandSet, OperandSet,
};

pub const DIALECT_MOTOROLA68K: &str = "motorola68k";
pub const FAMILY_ID: CpuFamily = CpuFamily::new("motorola68000");
const FAMILY_CPU_NAME: &str = "68000";
const FAMILY_REGISTER_IDS: &[&str] = &[
    "D0", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7",
    "E0", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12", "E13", "E14",
    "E15", "E16", "E17", "E18", "E19", "E20", "E21", "E22", "E23", "B0", "B1", "B2", "B3", "B4",
    "B5", "B6", "B7", "SP", "PC", "SR", "CCR", "USP", "SSP",
];

pub struct Motorola68000FamilyModule;

impl FamilyModule for Motorola68000FamilyModule {
    fn family_id(&self) -> CpuFamily {
        FAMILY_ID
    }

    fn family_cpu_id(&self) -> Option<CpuType> {
        Some(crate::m68000::module::CPU_ID)
    }

    fn family_cpu_name(&self) -> Option<&'static str> {
        Some(FAMILY_CPU_NAME)
    }

    fn canonical_dialect(&self) -> &'static str {
        DIALECT_MOTOROLA68K
    }

    fn register_ids(&self) -> &'static [&'static str] {
        FAMILY_REGISTER_IDS
    }

    fn dialects(&self) -> Vec<Box<dyn DialectModule>> {
        vec![Box::new(CanonicalDialect)]
    }

    fn handler(&self) -> Box<dyn FamilyHandlerDyn> {
        Box::new(M68KFamilyHandler::new())
    }
}

#[derive(Clone)]
pub struct M68KFamilyOperands(pub Vec<FamilyOperand>);

impl FamilyOperandSet for M68KFamilyOperands {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn FamilyOperandSet> {
        Box::new(self.clone())
    }
}

#[derive(Clone)]
pub struct M68KOperands(pub Vec<Operand>);

impl OperandSet for M68KOperands {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_box(&self) -> Box<dyn OperandSet> {
        Box::new(self.clone())
    }
}

struct CanonicalDialect;

impl DialectModule for CanonicalDialect {
    fn dialect_id(&self) -> &'static str {
        DIALECT_MOTOROLA68K
    }

    fn family_id(&self) -> CpuFamily {
        FAMILY_ID
    }

    fn map_mnemonic(
        &self,
        mnemonic: &str,
        operands: &dyn FamilyOperandSet,
    ) -> Option<(String, Box<dyn FamilyOperandSet>)> {
        let m68k_operands = operands.as_any().downcast_ref::<M68KFamilyOperands>()?;
        Some((
            mnemonic.to_string(),
            Box::new(M68KFamilyOperands(m68k_operands.0.clone())),
        ))
    }
}

impl FamilyHandlerDyn for M68KFamilyHandler {
    fn family_id(&self) -> CpuFamily {
        FAMILY_ID
    }

    fn parse_operands(
        &self,
        mnemonic: &str,
        exprs: &[Expr],
    ) -> Result<Box<dyn FamilyOperandSet>, FamilyParseError> {
        <Self as FamilyHandler>::parse_operands(self, mnemonic, exprs)
            .map(|ops| Box::new(M68KFamilyOperands(ops)) as Box<dyn FamilyOperandSet>)
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &dyn OperandSet,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let m68k_operands = match operands.as_any().downcast_ref::<M68KOperands>() {
            Some(ops) => ops,
            None => return EncodeResult::error("expected Motorola 68000 operands"),
        };
        <Self as FamilyHandler>::encode_instruction(self, mnemonic, &m68k_operands.0, ctx)
    }

    fn is_register(&self, name: &str) -> bool {
        <Self as FamilyHandler>::is_register(self, name)
    }

    fn is_condition(&self, name: &str) -> bool {
        <Self as FamilyHandler>::is_condition(self, name)
    }
}
