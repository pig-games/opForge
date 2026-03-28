// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68010 CPU handler implementation.

use crate::families::m68k::operand::{ControlRegisterKind, SpecialRegisterKind};
use crate::families::m68k::{
    has_m68010_mnemonic, has_mnemonic, parse_m68010_mnemonic, parse_mnemonic, FamilyOperand,
    M68010MnemonicKind, M68KFamilyHandler, MnemonicKind, Operand, OperationSize,
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

    fn encode_move_from_ccr(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("MOVE expects two operands");
        };
        if !matches!(
            src,
            Operand::SpecialRegister {
                register: SpecialRegisterKind::Ccr,
                ..
            }
        ) {
            return EncodeResult::NotFound;
        }

        match size {
            None | Some(OperationSize::Word) => {}
            Some(OperationSize::Byte) => {
                return EncodeResult::error("MOVE from CCR does not support .B size");
            }
            Some(OperationSize::Long) => {
                return EncodeResult::error("MOVE from CCR does not support .L size");
            }
        }

        let dst_ea = match self
            .family
            .encode_effective_address(dst, Some(OperationSize::Word), ctx)
        {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !M68KFamilyHandler::data_alterable(dst_ea.kind) {
            return EncodeResult::error_with_span(
                "invalid destination effective address for MOVE from CCR",
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, 0x42C0 | dst_ea.bits);
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
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
            | ControlRegisterKind::Isp => None,
        }
    }

    fn encode_bkpt(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("BKPT does not support size suffixes");
        }

        let [vector] = operands else {
            return EncodeResult::error("BKPT expects one immediate vector operand");
        };
        let Operand::Immediate { expr, .. } = vector else {
            return EncodeResult::error_with_span(
                "BKPT operand must be an immediate vector",
                vector.span(),
            );
        };

        let value = match M68KFamilyHandler::eval_expr(expr, ctx) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, vector.span()),
        };
        if !(0..=7).contains(&value) {
            return EncodeResult::error_with_span("BKPT vector out of range (0-7)", vector.span());
        }

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, 0x4848 | value as u16);
        EncodeResult::ok(bytes)
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

    fn encode_rtd(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("RTD does not support size suffixes");
        }

        let [displacement] = operands else {
            return EncodeResult::error("RTD expects one immediate displacement operand");
        };
        let Operand::Immediate { expr, .. } = displacement else {
            return EncodeResult::error_with_span(
                "RTD operand must be an immediate displacement",
                displacement.span(),
            );
        };

        let value = match M68KFamilyHandler::eval_expr(expr, ctx) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, displacement.span()),
        };
        let Some(encoded) = M68KFamilyHandler::encode_signed_word(value) else {
            return EncodeResult::error_with_span(
                "RTD displacement out of 16-bit signed range",
                displacement.span(),
            );
        };

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, 0x4E74);
        M68KFamilyHandler::emit_word(&mut bytes, encoded);
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
            M68010MnemonicKind::Bkpt => self.encode_bkpt(parsed.size, operands, ctx),
            M68010MnemonicKind::Movec => self.encode_movec(parsed.size, operands),
            M68010MnemonicKind::Moves => self.encode_moves(parsed.size, operands, ctx),
            M68010MnemonicKind::Rtd => self.encode_rtd(parsed.size, operands, ctx),
        }
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        has_mnemonic(mnemonic) || has_m68010_mnemonic(mnemonic)
    }
}
