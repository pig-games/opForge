// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68020 CPU module.

use std::collections::HashMap;

use crate::families::m68k::module::{
    M68KFamilyOperands, M68KOperands, DIALECT_MOTOROLA68K, FAMILY_ID as M68K_FAMILY_ID,
};
use crate::families::m68k::state;
use opcore::parser::Expr;
use registry::cpu::{CpuFamily, CpuType};
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};
use registry::registry::{CpuHandlerDyn, CpuModule, FamilyOperandSet, OperandSet};

use super::M68020CpuHandler;

pub struct M68020CpuModule;

pub const CPU_ID: CpuType = CpuType::new("m68020");
const CPU_ALIASES: &[&str] = &["68020", "mc68020"];

impl CpuModule for M68020CpuModule {
    fn cpu_id(&self) -> CpuType {
        CPU_ID
    }

    fn family_id(&self) -> CpuFamily {
        M68K_FAMILY_ID
    }

    fn cpu_name(&self) -> &'static str {
        CPU_ID.as_str()
    }

    fn cpu_aliases(&self) -> &'static [&'static str] {
        CPU_ALIASES
    }

    fn default_dialect(&self) -> &'static str {
        DIALECT_MOTOROLA68K
    }

    fn runtime_directive_ids(&self) -> &'static [&'static str] {
        state::RUNTIME_DIRECTIVE_IDS
    }

    fn handler(&self) -> Box<dyn CpuHandlerDyn> {
        Box::new(M68020CpuHandler::new())
    }
}

impl CpuHandlerDyn for M68020CpuHandler {
    fn cpu_id(&self) -> CpuType {
        CPU_ID
    }

    fn family_id(&self) -> CpuFamily {
        M68K_FAMILY_ID
    }

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &dyn FamilyOperandSet,
        ctx: &dyn AssemblerContext,
    ) -> Result<Box<dyn OperandSet>, String> {
        let m68k_operands = family_operands
            .as_any()
            .downcast_ref::<M68KFamilyOperands>()
            .ok_or_else(|| "expected Motorola 68000 family operands".to_string())?;
        <Self as CpuHandler>::resolve_operands(self, mnemonic, &m68k_operands.0, ctx)
            .map(|ops| Box::new(M68KOperands(ops)) as Box<dyn OperandSet>)
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
        <Self as CpuHandler>::encode_instruction(self, mnemonic, &m68k_operands.0, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        <Self as CpuHandler>::supports_mnemonic(self, mnemonic)
    }

    fn max_program_address(&self) -> u32 {
        0xFFFF_FFFF
    }

    fn native_word_size_bytes(&self) -> u32 {
        2
    }

    fn is_little_endian(&self) -> bool {
        false
    }

    fn runtime_state_defaults(&self) -> HashMap<String, u32> {
        state::initial_runtime_state()
    }

    fn apply_runtime_directive(
        &self,
        directive: &str,
        operands: &[Expr],
        ctx: &dyn AssemblerContext,
        state_flags: &mut HashMap<String, u32>,
    ) -> Result<bool, String> {
        state::apply_runtime_directive(directive, operands, CPU_ID, ctx, state_flags)
    }
}
