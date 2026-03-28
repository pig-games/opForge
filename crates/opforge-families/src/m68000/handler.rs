// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68000 CPU handler implementation.

use crate::families::m68k::operand::SpecialRegisterKind;
use crate::families::m68k::{
    has_mnemonic, parse_mnemonic, FamilyOperand, M68KFamilyHandler, MnemonicKind, Operand,
};
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};

#[derive(Debug)]
pub struct M68000CpuHandler {
    family: M68KFamilyHandler,
}

impl Default for M68000CpuHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl M68000CpuHandler {
    pub fn new() -> Self {
        Self {
            family: M68KFamilyHandler::new(),
        }
    }
}

impl CpuHandler for M68000CpuHandler {
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
        if family_operands
            .iter()
            .any(|operand| matches!(operand, FamilyOperand::FullExtension { .. }))
        {
            return Err(
                "68020+ full-extension addressing is not supported on baseline 68000".to_string(),
            );
        }
        Ok(family_operands.to_vec())
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        _ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if parse_mnemonic(mnemonic).is_some()
            && operands
                .iter()
                .any(|operand| matches!(operand, Operand::FullExtension { .. }))
        {
            return EncodeResult::error(
                "68020+ full-extension addressing is not supported on baseline 68000",
            );
        }

        if matches!(
            parse_mnemonic(mnemonic).map(|parsed| parsed.kind),
            Some(MnemonicKind::Move)
        ) && matches!(
            operands,
            [
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Ccr,
                    ..
                },
                _
            ]
        ) {
            return EncodeResult::error_with_span(
                "MOVE from CCR is not supported on baseline 68000",
                operands[0].span(),
            );
        }

        EncodeResult::NotFound
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        has_mnemonic(mnemonic)
    }
}
