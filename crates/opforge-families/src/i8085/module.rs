// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! 8085 CPU module.

use crate::families::intel8080::module::{
    Intel8080FamilyOperands, Intel8080Operands, DIALECT_INTEL8080, FAMILY_ID as INTEL8080_FAMILY_ID,
};
use registry::cpu::{CpuFamily, CpuType};
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};
use registry::registry::{CpuHandlerDyn, CpuModule, FamilyOperandSet, OperandSet};

use super::I8085CpuHandler;

pub struct I8085CpuModule;

pub const CPU_ID: CpuType = CpuType::new("8085");

fn cpu_form_mnemonics() -> Vec<String> {
    let mut mnemonics: Vec<String> = super::I8085_EXTENSION_TABLE
        .iter()
        .map(|entry| entry.mnemonic.to_ascii_lowercase())
        .collect();
    mnemonics.sort();
    mnemonics.dedup();
    mnemonics
}

impl CpuModule for I8085CpuModule {
    fn cpu_id(&self) -> CpuType {
        CPU_ID
    }

    fn family_id(&self) -> CpuFamily {
        INTEL8080_FAMILY_ID
    }

    fn cpu_name(&self) -> &'static str {
        CPU_ID.as_str()
    }

    fn default_dialect(&self) -> &'static str {
        DIALECT_INTEL8080
    }

    fn form_mnemonics(&self) -> Vec<String> {
        cpu_form_mnemonics()
    }

    fn handler(&self) -> Box<dyn CpuHandlerDyn> {
        Box::new(I8085CpuHandler::new())
    }
}

impl CpuHandlerDyn for I8085CpuHandler {
    fn cpu_id(&self) -> CpuType {
        CPU_ID
    }

    fn family_id(&self) -> CpuFamily {
        INTEL8080_FAMILY_ID
    }

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &dyn FamilyOperandSet,
        ctx: &dyn AssemblerContext,
    ) -> Result<Box<dyn OperandSet>, String> {
        let intel_operands = family_operands
            .as_any()
            .downcast_ref::<Intel8080FamilyOperands>()
            .ok_or_else(|| "expected Intel 8080 family operands".to_string())?;
        <Self as CpuHandler>::resolve_operands(self, mnemonic, &intel_operands.0, ctx)
            .map(|ops| Box::new(Intel8080Operands(ops)) as Box<dyn OperandSet>)
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &dyn OperandSet,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let intel_operands = match operands.as_any().downcast_ref::<Intel8080Operands>() {
            Some(ops) => ops,
            None => return EncodeResult::error("expected Intel 8080 operands"),
        };
        <Self as CpuHandler>::encode_instruction(self, mnemonic, &intel_operands.0, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        <Self as CpuHandler>::supports_mnemonic(self, mnemonic)
    }
}
