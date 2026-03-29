// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68040 CPU handler implementation.

use crate::families::m68k::operand::{AbsoluteSize, ControlRegisterKind};
use crate::families::m68k::{
    parse_m68010_mnemonic, parse_m68020_mnemonic, FamilyOperand, M68010MnemonicKind,
    M68020MnemonicKind, M68KFamilyHandler, Operand,
};
use crate::m68030::M68030CpuHandler;
use opcore::tokenizer::Span;
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};

#[derive(Debug)]
pub struct M68040CpuHandler {
    base: M68030CpuHandler,
}

#[derive(Debug)]
struct ParsedMove16Mnemonic {
    display_name: String,
    has_size_suffix: bool,
}

impl Default for M68040CpuHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl M68040CpuHandler {
    pub fn new() -> Self {
        Self {
            base: M68030CpuHandler::new(),
        }
    }

    fn parse_move16_mnemonic(mnemonic: &str) -> Option<ParsedMove16Mnemonic> {
        let display_name = mnemonic.to_ascii_uppercase();
        let (base, suffix) = match display_name.split_once('.') {
            Some((base, suffix)) => (base, Some(suffix)),
            None => (display_name.as_str(), None),
        };
        let has_size_suffix = suffix.is_some();
        (base == "MOVE16").then_some(ParsedMove16Mnemonic {
            display_name,
            has_size_suffix,
        })
    }

    fn movec_control_register_code(register: ControlRegisterKind) -> Option<u16> {
        match register {
            ControlRegisterKind::Sfc => Some(0x000),
            ControlRegisterKind::Dfc => Some(0x001),
            ControlRegisterKind::Vbr => Some(0x801),
            ControlRegisterKind::Cacr => Some(0x002),
            ControlRegisterKind::Msp => Some(0x803),
            ControlRegisterKind::Isp => Some(0x804),
            ControlRegisterKind::Tc => Some(0x003),
            ControlRegisterKind::Itt0 => Some(0x004),
            ControlRegisterKind::Itt1 => Some(0x005),
            ControlRegisterKind::Dtt0 => Some(0x006),
            ControlRegisterKind::Dtt1 => Some(0x007),
            ControlRegisterKind::Mmusr => Some(0x805),
            ControlRegisterKind::Urp => Some(0x806),
            ControlRegisterKind::Srp => Some(0x807),
            ControlRegisterKind::Caar => None,
        }
    }

    fn encode_movec(
        &self,
        operands: &[Operand],
        size_suffix_present: bool,
    ) -> EncodeResult<Vec<u8>> {
        if size_suffix_present {
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
        if matches!(register, ControlRegisterKind::Caar) {
            return EncodeResult::error_with_span(
                "MOVEC CAAR is not supported on m68040",
                control_operand.span(),
            );
        }
        let Some(control_bits) = Self::movec_control_register_code(*register) else {
            return EncodeResult::error_with_span(
                "unsupported MOVEC control register for m68040",
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

    fn encode_move16(
        &self,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("MOVE16 expects two operands");
        };

        match (src, dst) {
            (
                Operand::AddressPostincrement {
                    register: src_reg, ..
                },
                Operand::AddressPostincrement {
                    register: dst_reg, ..
                },
            ) => {
                let Some(src_bits) = M68KFamilyHandler::address_register_number(src_reg) else {
                    return EncodeResult::error_with_span(
                        "MOVE16 source operand must use an address register",
                        src.span(),
                    );
                };
                let Some(dst_bits) = M68KFamilyHandler::address_register_number(dst_reg) else {
                    return EncodeResult::error_with_span(
                        "MOVE16 destination operand must use an address register",
                        dst.span(),
                    );
                };
                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(&mut bytes, 0xF620 | src_bits as u16);
                M68KFamilyHandler::emit_word(&mut bytes, 0x8000 | ((dst_bits as u16) << 12));
                EncodeResult::ok(bytes)
            }
            (
                Operand::AddressIndirect { register, .. }
                | Operand::AddressPostincrement { register, .. },
                Operand::Absolute {
                    expr,
                    size: AbsoluteSize::Long,
                    ..
                },
            ) => self.encode_move16_absolute(
                register,
                expr,
                match src {
                    Operand::AddressIndirect { .. } => 0b10,
                    Operand::AddressPostincrement { .. } => 0b00,
                    _ => unreachable!(
                        "MOVE16 register->absolute match should be indirect or postincrement"
                    ),
                },
                src.span(),
                ctx,
            ),
            (
                Operand::Absolute {
                    expr,
                    size: AbsoluteSize::Long,
                    ..
                },
                Operand::AddressIndirect { register, .. }
                | Operand::AddressPostincrement { register, .. },
            ) => self.encode_move16_absolute(
                register,
                expr,
                match dst {
                    Operand::AddressIndirect { .. } => 0b11,
                    Operand::AddressPostincrement { .. } => 0b01,
                    _ => unreachable!(
                        "MOVE16 absolute->register match should be indirect or postincrement"
                    ),
                },
                dst.span(),
                ctx,
            ),
            (Operand::Absolute { .. }, _) | (_, Operand::Absolute { .. }) => {
                EncodeResult::error("MOVE16 absolute operand must use .L size")
            }
            _ => EncodeResult::error(
                "MOVE16 expects '(Ax)+,(Ay)+' or one absolute .L operand paired with (Ay) or (Ay)+",
            ),
        }
    }

    fn encode_move16_absolute(
        &self,
        register: &str,
        expr: &opcore::parser::Expr,
        opmode: u16,
        register_span: Span,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(register_bits) = M68KFamilyHandler::address_register_number(register) else {
            return EncodeResult::error_with_span(
                "MOVE16 register operand must use an address register",
                register_span,
            );
        };

        let value = match M68KFamilyHandler::eval_expr(expr, ctx) {
            Ok(value) => value,
            Err(message) => return EncodeResult::error(message),
        };
        if !(-2_147_483_648..=4_294_967_295).contains(&value) {
            return EncodeResult::error(format!(
                "MOVE16 absolute address {value} out of 32-bit range"
            ));
        }

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, 0xF600 | (opmode << 3) | register_bits as u16);
        bytes.extend_from_slice(&(value as u32).to_be_bytes());
        EncodeResult::ok(bytes)
    }
}

impl CpuHandler for M68040CpuHandler {
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
        if let Some(parsed) = Self::parse_move16_mnemonic(mnemonic) {
            if parsed.has_size_suffix {
                return EncodeResult::error(format!(
                    "unsupported size suffix for {}",
                    parsed.display_name
                ));
            }
            return self.encode_move16(operands, ctx);
        }

        if let Some(parsed) = parse_m68020_mnemonic(mnemonic) {
            if parsed.has_unknown_size_suffix {
                return EncodeResult::error(format!(
                    "unsupported size suffix for {}",
                    parsed.display_name
                ));
            }

            match parsed.kind {
                M68020MnemonicKind::Callm => {
                    return EncodeResult::error("CALLM is not supported on m68040");
                }
                M68020MnemonicKind::Rtm => {
                    return EncodeResult::error("RTM is not supported on m68040");
                }
                M68020MnemonicKind::Pflush => {
                    return EncodeResult::error("PFLUSH is not yet supported on m68040");
                }
                _ => {}
            }
        }

        if let Some(parsed) = parse_m68010_mnemonic(mnemonic) {
            if parsed.has_unknown_size_suffix {
                return EncodeResult::error(format!(
                    "unsupported size suffix for {}",
                    parsed.display_name
                ));
            }

            if matches!(parsed.kind, M68010MnemonicKind::Movec) {
                return self.encode_movec(operands, parsed.size.is_some());
            }
        }

        self.base.encode_instruction(mnemonic, operands, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        self.base.supports_mnemonic(mnemonic) || Self::parse_move16_mnemonic(mnemonic).is_some()
    }
}
