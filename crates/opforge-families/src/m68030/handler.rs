// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68030 CPU handler implementation.

use crate::families::m68k::{FamilyOperand, M68KFamilyHandler, Operand};
use crate::m68020::M68020CpuHandler;
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};

#[derive(Debug)]
pub struct M68030CpuHandler {
    base: M68020CpuHandler,
}

impl Default for M68030CpuHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl M68030CpuHandler {
    pub fn new() -> Self {
        Self {
            base: M68020CpuHandler::new(),
        }
    }
}

impl CpuHandler for M68030CpuHandler {
    type Family = M68KFamilyHandler;

    fn family(&self) -> &Self::Family {
        self.base.family()
    }

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &[FamilyOperand],
        ctx: &dyn AssemblerContext,
    ) -> Result<Vec<Operand>, String> {
        self.base.resolve_operands(mnemonic, family_operands, ctx)
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.base.encode_instruction(mnemonic, operands, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        self.base.supports_mnemonic(mnemonic)
    }
}
