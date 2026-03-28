// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68040 CPU handler implementation.

use crate::families::m68k::{
    has_mnemonic, parse_m68010_mnemonic, FamilyOperand, M68010MnemonicKind, M68KFamilyHandler,
    Operand,
};
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};

#[derive(Debug)]
pub struct M68040CpuHandler {
    family: M68KFamilyHandler,
}

impl Default for M68040CpuHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl M68040CpuHandler {
    pub fn new() -> Self {
        Self {
            family: M68KFamilyHandler::new(),
        }
    }
}

impl CpuHandler for M68040CpuHandler {
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
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(parsed) = parse_m68010_mnemonic(mnemonic) else {
            return EncodeResult::NotFound;
        };
        if parsed.has_unknown_size_suffix {
            return EncodeResult::error(format!(
                "unsupported size suffix for {}",
                parsed.display_name
            ));
        }

        match parsed.kind {
            M68010MnemonicKind::Moves => {
                self.family
                    .encode_moves_instruction(parsed.size, operands, ctx)
            }
            M68010MnemonicKind::Bkpt | M68010MnemonicKind::Movec | M68010MnemonicKind::Rtd => {
                EncodeResult::NotFound
            }
        }
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        has_mnemonic(mnemonic)
            || matches!(
                parse_m68010_mnemonic(mnemonic),
                Some(parsed) if matches!(parsed.kind, M68010MnemonicKind::Moves)
            )
    }
}
