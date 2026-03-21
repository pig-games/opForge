// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Hitachi HD6309 CPU module.

use crate::families::m6800::module::{
    M6800FamilyOperands, M6800Operands, DIALECT_MOTOROLA680X, FAMILY_ID as M6800_FAMILY_ID,
};
use registry::cpu::{CpuFamily, CpuType};
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};
use registry::registry::{CpuHandlerDyn, CpuModule, FamilyOperandSet, OperandSet};

use super::HD6309CpuHandler;

pub struct HD6309CpuModule;

pub const CPU_ID: CpuType = CpuType::new("hd6309");
const CPU_ALIASES: &[&str] = &["6309", "m6309", "h6309", "hitachi6309"];

fn cpu_form_mnemonics() -> Vec<String> {
    let mut mnemonics: Vec<String> = super::instructions::CPU_INSTRUCTION_TABLE
        .iter()
        .map(|entry| entry.mnemonic.to_ascii_lowercase())
        .collect();
    mnemonics.sort();
    mnemonics.dedup();
    mnemonics
}

impl CpuModule for HD6309CpuModule {
    fn cpu_id(&self) -> CpuType {
        CPU_ID
    }

    fn family_id(&self) -> CpuFamily {
        M6800_FAMILY_ID
    }

    fn cpu_name(&self) -> &'static str {
        CPU_ID.as_str()
    }

    fn cpu_aliases(&self) -> &'static [&'static str] {
        CPU_ALIASES
    }

    fn default_dialect(&self) -> &'static str {
        DIALECT_MOTOROLA680X
    }

    fn form_mnemonics(&self) -> Vec<String> {
        cpu_form_mnemonics()
    }

    fn handler(&self) -> Box<dyn CpuHandlerDyn> {
        Box::new(HD6309CpuHandler::new())
    }
}

impl CpuHandlerDyn for HD6309CpuHandler {
    fn cpu_id(&self) -> CpuType {
        CPU_ID
    }

    fn family_id(&self) -> CpuFamily {
        M6800_FAMILY_ID
    }

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &dyn FamilyOperandSet,
        ctx: &dyn AssemblerContext,
    ) -> Result<Box<dyn OperandSet>, String> {
        let m6800_operands = family_operands
            .as_any()
            .downcast_ref::<M6800FamilyOperands>()
            .ok_or_else(|| "expected Motorola 6800 family operands".to_string())?;
        <Self as CpuHandler>::resolve_operands(self, mnemonic, &m6800_operands.0, ctx)
            .map(|ops| Box::new(M6800Operands(ops)) as Box<dyn OperandSet>)
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &dyn OperandSet,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let m6800_operands = match operands.as_any().downcast_ref::<M6800Operands>() {
            Some(ops) => ops,
            None => return EncodeResult::error("expected Motorola 6800 operands"),
        };
        <Self as CpuHandler>::encode_instruction(self, mnemonic, &m6800_operands.0, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        <Self as CpuHandler>::supports_mnemonic(self, mnemonic)
    }

    fn max_program_address(&self) -> u32 {
        0xFFFF
    }

    fn native_word_size_bytes(&self) -> u32 {
        2
    }

    fn is_little_endian(&self) -> bool {
        false
    }
}
