// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68010 CPU handler implementation.

use crate::families::m68k::operand::{ControlRegisterKind, SpecialRegisterKind};
use crate::families::m68k::{
    has_m68010_mnemonic, has_mnemonic, parse_m68010_mnemonic, parse_m68020_mnemonic,
    parse_mnemonic, FamilyOperand, M68010MnemonicKind, M68020MnemonicKind, M68KFamilyHandler,
    MnemonicKind, Operand, OperationSize,
};
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};

#[derive(Debug)]
pub struct M68010CpuHandler {
    family: M68KFamilyHandler,
}

impl Default for M68010CpuHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl M68010CpuHandler {
    pub fn new() -> Self {
        Self {
            family: M68KFamilyHandler::new(),
        }
    }

    fn reject_m68020_only_baseline_forms(&self, mnemonic: &str) -> Option<EncodeResult<Vec<u8>>> {
        let parsed = parse_mnemonic(mnemonic)?;
        match (parsed.kind, parsed.size) {
            (
                MnemonicKind::Bra | MnemonicKind::Bsr | MnemonicKind::Bcc(_),
                Some(OperationSize::Long),
            ) => Some(EncodeResult::error(format!(
                "{} does not support .L size on m68010",
                parsed.display_name
            ))),
            (MnemonicKind::Link, Some(OperationSize::Long)) => Some(EncodeResult::error(
                "LINK does not support .L size on m68010",
            )),
            (MnemonicKind::Muls | MnemonicKind::Mulu, Some(OperationSize::Long)) => {
                Some(EncodeResult::error(format!(
                    "{} does not support .L size on m68010",
                    parsed.display_name
                )))
            }
            _ => None,
        }
    }

    fn encode_move_from_ccr(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.family
            .encode_move_from_ccr_instruction(size, operands, ctx)
    }

    fn general_register_descriptor(operand: &Operand) -> Option<(u16, u16)> {
        match operand {
            Operand::DataRegister { register, .. } => {
                Some((0, M68KFamilyHandler::data_register_number(register)? as u16))
            }
            Operand::AddressRegister { register, .. } => Some((
                1,
                M68KFamilyHandler::address_register_number(register)? as u16,
            )),
            _ => None,
        }
    }

    fn movec_control_register_code(register: ControlRegisterKind) -> Option<u16> {
        match register {
            ControlRegisterKind::Sfc => Some(0x000),
            ControlRegisterKind::Dfc => Some(0x001),
            ControlRegisterKind::Vbr => Some(0x801),
            ControlRegisterKind::Cacr
            | ControlRegisterKind::Caar
            | ControlRegisterKind::Msp
            | ControlRegisterKind::Isp
            | ControlRegisterKind::Tc
            | ControlRegisterKind::Itt0
            | ControlRegisterKind::Itt1
            | ControlRegisterKind::Dtt0
            | ControlRegisterKind::Dtt1
            | ControlRegisterKind::Mmusr
            | ControlRegisterKind::Urp
            | ControlRegisterKind::Srp => None,
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
                )
            }
        };

        let Some((ad_bit, register_bits)) = Self::general_register_descriptor(general_operand)
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
                "unsupported MOVEC control register for m68010",
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

    fn encode_moves(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error("MOVES requires an explicit size suffix (.B, .W, or .L)");
        };

        let [src, dst] = operands else {
            return EncodeResult::error("MOVES expects two operands");
        };

        let (dr_bit, register_operand, ea_operand) = if Self::general_register_descriptor(src)
            .is_some()
        {
            (1_u16, src, dst)
        } else if Self::general_register_descriptor(dst).is_some() {
            (0_u16, dst, src)
        } else {
            return EncodeResult::error(
                    "MOVES expects one data/address register and one memory-alterable effective address",
                );
        };

        let Some((ad_bit, register_bits)) = Self::general_register_descriptor(register_operand)
        else {
            return EncodeResult::error_with_span(
                "MOVES register operand must be a data or address register",
                register_operand.span(),
            );
        };

        let ea = match self
            .family
            .encode_effective_address(ea_operand, Some(size), ctx)
        {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !M68KFamilyHandler::memory_alterable(ea.kind) {
            return EncodeResult::error_with_span(
                if dr_bit == 0 {
                    format!(
                        "invalid source effective address for MOVES{}",
                        size.suffix()
                    )
                } else {
                    format!(
                        "invalid destination effective address for MOVES{}",
                        size.suffix()
                    )
                },
                ea_operand.span(),
            );
        }

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(
            &mut bytes,
            0x0E00 | (M68KFamilyHandler::size_bits(size) << 6) | ea.bits,
        );
        M68KFamilyHandler::emit_word(
            &mut bytes,
            (ad_bit << 15) | (register_bits << 12) | (dr_bit << 11),
        );
        bytes.extend_from_slice(&ea.extension);
        EncodeResult::ok(bytes)
    }

}

impl CpuHandler for M68010CpuHandler {
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
            return Err("68020+ full-extension addressing is not supported on m68010".to_string());
        }
        Ok(family_operands.to_vec())
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Some(result) = self.reject_m68020_only_baseline_forms(mnemonic) {
            return result;
        }

        let has_later_full_extension = operands
            .iter()
            .any(|operand| matches!(operand, Operand::FullExtension { .. }));

        if let Some(parsed) = parse_mnemonic(mnemonic) {
            if parsed.has_unknown_size_suffix {
                return EncodeResult::error(format!(
                    "unsupported size suffix for {}",
                    parsed.display_name
                ));
            }

            if has_later_full_extension {
                return EncodeResult::error(
                    "68020+ full-extension addressing is not supported on m68010",
                );
            }

            if matches!(parsed.kind, MnemonicKind::Move)
                && matches!(
                    operands,
                    [
                        Operand::SpecialRegister {
                            register: SpecialRegisterKind::Ccr,
                            ..
                        },
                        _
                    ]
                )
            {
                return self.encode_move_from_ccr(parsed.size, operands, ctx);
            }
        }

        if let Some(parsed) = parse_m68020_mnemonic(mnemonic) {
            return match parsed.kind {
                M68020MnemonicKind::Extb => {
                    EncodeResult::error("EXTB is only supported on m68020 and later")
                }
                _ => EncodeResult::error(format!(
                    "{} is only supported on m68020 and later",
                    parsed.display_name
                )),
            };
        }

        let Some(parsed) = parse_m68010_mnemonic(mnemonic) else {
            return EncodeResult::NotFound;
        };
        if parsed.has_unknown_size_suffix {
            return EncodeResult::error(format!(
                "unsupported size suffix for {}",
                parsed.display_name
            ));
        }

        if has_later_full_extension {
            return EncodeResult::error(
                "68020+ full-extension addressing is not supported on m68010",
            );
        }

        match parsed.kind {
            M68010MnemonicKind::Bkpt => {
                self.family.encode_bkpt_instruction(parsed.size, operands, ctx)
            }
            M68010MnemonicKind::Movec => self.encode_movec(parsed.size, operands),
            M68010MnemonicKind::Moves => self.encode_moves(parsed.size, operands, ctx),
            M68010MnemonicKind::Rtd => {
                self.family.encode_rtd_instruction(parsed.size, operands, ctx)
            }
        }
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        has_mnemonic(mnemonic) || has_m68010_mnemonic(mnemonic)
    }
}
