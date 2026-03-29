// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68030 CPU handler implementation.

use crate::families::m68k::{parse_m68020_mnemonic, M68020MnemonicKind, OperationSize};
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

    fn encode_pflush(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("PFLUSH does not support size suffixes");
        }

        let [fc_operand, mask_operand] = operands else {
            return EncodeResult::error("PFLUSH currently expects exactly two operands: #fc,#mask");
        };

        let Operand::Immediate { expr: fc_expr, .. } = fc_operand else {
            return EncodeResult::error_with_span(
                "PFLUSH FC operand must be an immediate value on m68030",
                fc_operand.span(),
            );
        };
        let Operand::Immediate {
            expr: mask_expr, ..
        } = mask_operand
        else {
            return EncodeResult::error_with_span(
                "PFLUSH mask operand must be an immediate value on m68030",
                mask_operand.span(),
            );
        };

        let fc_value = match M68KFamilyHandler::eval_expr(fc_expr, ctx) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, fc_operand.span()),
        };
        if !(0..=7).contains(&fc_value) {
            return EncodeResult::error_with_span(
                "PFLUSH FC immediate out of range (0-7)",
                fc_operand.span(),
            );
        }

        let mask_value = match M68KFamilyHandler::eval_expr(mask_expr, ctx) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, mask_operand.span()),
        };
        if !(0..=7).contains(&mask_value) {
            return EncodeResult::error_with_span(
                "PFLUSH mask immediate out of range (0-7)",
                mask_operand.span(),
            );
        }

        let fc_bits = 0b10_000_u16 | fc_value as u16;
        let second_word = 0x3000_u16 | ((mask_value as u16) << 5) | fc_bits;

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, 0xF000);
        M68KFamilyHandler::emit_word(&mut bytes, second_word);
        EncodeResult::ok(bytes)
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
        if let Some(parsed) = parse_m68020_mnemonic(mnemonic) {
            if matches!(parsed.kind, M68020MnemonicKind::Pflush) {
                if parsed.has_unknown_size_suffix {
                    return EncodeResult::error(format!(
                        "unsupported size suffix for {}",
                        parsed.display_name
                    ));
                }
                return self.encode_pflush(parsed.size, operands, ctx);
            }
        }

        self.base.encode_instruction(mnemonic, operands, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        self.base.supports_mnemonic(mnemonic)
    }
}
