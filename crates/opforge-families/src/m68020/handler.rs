// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68020 CPU handler implementation.

use crate::families::m68k::{has_mnemonic, FamilyOperand, M68KFamilyHandler, Operand};
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};

#[derive(Debug)]
pub struct M68020CpuHandler {
    family: M68KFamilyHandler,
}

impl Default for M68020CpuHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl M68020CpuHandler {
    pub fn new() -> Self {
        Self {
            family: M68KFamilyHandler::new(),
        }
    }
}

impl CpuHandler for M68020CpuHandler {
    type Family = M68KFamilyHandler;

    fn family(&self) -> &Self::Family {
        &self.family
    }

    fn resolve_operands(
        &self,
        _mnemonic: &str,
        family_operands: &[FamilyOperand],
        _ctx: &dyn AssemblerContext,
    ) -> Result<Vec<Operand>, String> {
        Ok(family_operands.to_vec())
    }

    fn encode_instruction(
        &self,
        _mnemonic: &str,
        _operands: &[Operand],
        _ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        EncodeResult::NotFound
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        has_mnemonic(mnemonic)
    }
}
