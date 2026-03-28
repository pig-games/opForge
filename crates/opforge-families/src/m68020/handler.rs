// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68020 CPU handler implementation.

use crate::families::m68k::operand::ControlRegisterKind;
use crate::families::m68k::{
    has_m68020_mnemonic, has_mnemonic, parse_m68010_mnemonic, parse_m68020_mnemonic,
    parse_mnemonic, FamilyOperand, M68010MnemonicKind, M68020MnemonicKind, M68KFamilyHandler,
    MnemonicKind, Operand, OperationSize,
};
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

    fn movec_control_register_code(register: ControlRegisterKind) -> Option<u16> {
        match register {
            ControlRegisterKind::Sfc => Some(0x000),
            ControlRegisterKind::Dfc => Some(0x001),
            ControlRegisterKind::Vbr => Some(0x801),
            ControlRegisterKind::Cacr => Some(0x002),
            ControlRegisterKind::Caar => Some(0x802),
            ControlRegisterKind::Msp => Some(0x803),
            ControlRegisterKind::Isp => Some(0x804),
        }
    }

    fn encode_movec(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("MOVEC does not support size suffixes");
        }

        let [src, dst] = operands else {
            return EncodeResult::error("MOVEC expects two operands");
        };

        let (dr_bit, general_operand, control_operand) = match (src, dst) {
            (Operand::ControlRegister { .. }, _) => (0_u16, dst, src),
            (_, Operand::ControlRegister { .. }) => (1_u16, src, dst),
            _ => {
                return EncodeResult::error(
                    "MOVEC expects one control register and one data/address register operand",
                );
            }
        };

        let Some((ad_bit, register_bits)) =
            M68KFamilyHandler::general_register_descriptor(general_operand)
        else {
            return EncodeResult::error_with_span(
                "MOVEC general register operand must be a data or address register",
                general_operand.span(),
            );
        };

        let Operand::ControlRegister { register, .. } = control_operand else {
            unreachable!("MOVEC control operand should be a control register");
        };
        let Some(control_bits) = Self::movec_control_register_code(*register) else {
            return EncodeResult::error_with_span(
                "unsupported MOVEC control register for m68020",
                control_operand.span(),
            );
        };

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, 0x4E7A | dr_bit);
        M68KFamilyHandler::emit_word(
            &mut bytes,
            (ad_bit << 15) | (register_bits << 12) | control_bits,
        );
        EncodeResult::ok(bytes)
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
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Some(parsed) = parse_mnemonic(mnemonic) {
            if parsed.has_unknown_size_suffix {
                return EncodeResult::error(format!(
                    "unsupported size suffix for {}",
                    parsed.display_name
                ));
            }

            match parsed.kind {
                MnemonicKind::Bra if matches!(parsed.size, Some(OperationSize::Long)) => {
                    return self.family.encode_long_branch_instruction(
                        &parsed.display_name,
                        None,
                        operands,
                        ctx,
                    );
                }
                MnemonicKind::Bsr if matches!(parsed.size, Some(OperationSize::Long)) => {
                    return self.family.encode_long_branch_instruction(
                        &parsed.display_name,
                        None,
                        operands,
                        ctx,
                    );
                }
                MnemonicKind::Bcc(condition)
                    if matches!(parsed.size, Some(OperationSize::Long)) =>
                {
                    return self.family.encode_long_branch_instruction(
                        &parsed.display_name,
                        Some(condition),
                        operands,
                        ctx,
                    );
                }
                MnemonicKind::Link if matches!(parsed.size, Some(OperationSize::Long)) => {
                    return self.family.encode_link_long_instruction(operands, ctx);
                }
                MnemonicKind::Muls if matches!(parsed.size, Some(OperationSize::Long)) => {
                    return self.family.encode_long_data_register_multiply(
                        &parsed.display_name,
                        true,
                        operands,
                        ctx,
                    );
                }
                MnemonicKind::Mulu if matches!(parsed.size, Some(OperationSize::Long)) => {
                    return self.family.encode_long_data_register_multiply(
                        &parsed.display_name,
                        false,
                        operands,
                        ctx,
                    );
                }
                _ => {}
            }
        }

        let Some(parsed) = parse_m68010_mnemonic(mnemonic) else {
            let Some(parsed) = parse_m68020_mnemonic(mnemonic) else {
                return EncodeResult::NotFound;
            };
            if parsed.has_unknown_size_suffix {
                return EncodeResult::error(format!(
                    "unsupported size suffix for {}",
                    parsed.display_name
                ));
            }

            return match parsed.kind {
                M68020MnemonicKind::Extb => {
                    self.family.encode_extb_instruction(parsed.size, operands)
                }
            };
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
            M68010MnemonicKind::Movec => self.encode_movec(parsed.size, operands),
            M68010MnemonicKind::Bkpt | M68010MnemonicKind::Rtd => EncodeResult::NotFound,
        }
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        has_mnemonic(mnemonic)
            || matches!(
                parse_m68010_mnemonic(mnemonic),
                Some(parsed)
                    if matches!(
                        parsed.kind,
                        M68010MnemonicKind::Moves | M68010MnemonicKind::Movec
                    )
            )
            || has_m68020_mnemonic(mnemonic)
    }
}
