// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68020 CPU handler implementation.

use crate::families::m68k::operand::{
    ControlRegisterKind, FpuControlRegisterKind, RegisterListRegister, SpecialRegisterKind,
};
use crate::families::m68k::{
    has_fpu_mnemonic, has_m68020_mnemonic, has_mnemonic, parse_fpu_mnemonic, parse_m68010_mnemonic,
    parse_m68020_mnemonic, parse_mnemonic, FamilyOperand, FpuMnemonicKind, M68010MnemonicKind,
    M68020MnemonicKind, M68KFamilyHandler, MnemonicKind, Operand, OperationSize,
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
    const LEGAL_FPU_TARGETS: [u32; 2] = [1, 2];

    pub fn new() -> Self {
        Self {
            family: M68KFamilyHandler::new_with_max_absolute_address(u32::MAX as i64),
        }
    }

    fn fpu_target_name(state_value: u32) -> &'static str {
        match state_value {
            1 => "68881",
            2 => "68882",
            3 => "68040",
            _ => "none",
        }
    }

    fn validate_fpu_target(
        &self,
        display_name: &str,
        ctx: &dyn AssemblerContext,
    ) -> Result<&'static str, EncodeResult<Vec<u8>>> {
        let target = ctx
            .cpu_state_flag(crate::families::m68k::state::FPU_TARGET_KEY)
            .unwrap_or(0);

        if target == 0 {
            return Err(EncodeResult::error(format!(
                "{display_name} requires an active .fpu target on m68020; legal .fpu targets for m68020 FPU instructions: 68881, 68882"
            )));
        }

        if !Self::LEGAL_FPU_TARGETS.contains(&target) {
            return Err(EncodeResult::error(format!(
                "{display_name} is not available with .fpu {} on m68020; legal .fpu targets for m68020 FPU instructions: 68881, 68882",
                Self::fpu_target_name(target),
            )));
        }

        Ok(Self::fpu_target_name(target))
    }

    fn deferred_fpu_message(&self, display_name: &str, target_name: &str) -> EncodeResult<Vec<u8>> {
        EncodeResult::error(format!(
            "{display_name} is recognized for .fpu {} on m68020, but FPU encoding is not yet implemented",
            target_name,
        ))
    }

    fn fpu_control_register_field(register: FpuControlRegisterKind) -> u16 {
        match register {
            FpuControlRegisterKind::Fpcr => 0b100 << 10,
            FpuControlRegisterKind::Fpsr => 0b010 << 10,
            FpuControlRegisterKind::Fpiar => 0b001 << 10,
        }
    }

    fn effective_address_mode(bits: u16) -> u16 {
        (bits >> 3) & 0b111
    }

    fn effective_address_register(bits: u16) -> u16 {
        bits & 0b111
    }

    fn fpu_register_list_mask(
        registers: &[RegisterListRegister],
        reverse: bool,
    ) -> Result<u16, EncodeResult<Vec<u8>>> {
        let mut mask = 0_u16;
        for register in registers {
            let RegisterListRegister::FpuData(reg) = register else {
                return Err(EncodeResult::error(
                    "FMOVEM currently supports only FP register lists on m68020",
                ));
            };
            let bit = if reverse {
                7 - *reg as u16
            } else {
                *reg as u16
            };
            mask |= 1_u16 << bit;
        }
        Ok(mask)
    }

    fn fmovem_register_to_memory_destination(bits: u16) -> bool {
        matches!(
            (
                Self::effective_address_mode(bits),
                Self::effective_address_register(bits),
            ),
            (2 | 4 | 5 | 6, _) | (7, 0 | 1)
        )
    }

    fn fmovem_memory_to_register_source(bits: u16) -> bool {
        matches!(
            (
                Self::effective_address_mode(bits),
                Self::effective_address_register(bits),
            ),
            (2 | 3 | 5 | 6, _) | (7, 0 | 1)
        )
    }

    fn encode_fmove(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("FMOVE expects two operands");
        };

        match (size, src, dst) {
            (
                None,
                Operand::FpuDataRegister {
                    register: src_register,
                    ..
                },
                Operand::FpuDataRegister {
                    register: dst_register,
                    ..
                },
            ) => {
                let Some(src_reg) = M68KFamilyHandler::fpu_data_register_number(src_register) else {
                    return EncodeResult::error_with_span(
                        "invalid FMOVE source FP register",
                        src.span(),
                    );
                };
                let Some(dst_reg) = M68KFamilyHandler::fpu_data_register_number(dst_register) else {
                    return EncodeResult::error_with_span(
                        "invalid FMOVE destination FP register",
                        dst.span(),
                    );
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(&mut bytes, 0xF000);
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    ((src_reg as u16) << 10) | ((dst_reg as u16) << 7),
                );
                EncodeResult::ok(bytes)
            }
            (
                Some(OperationSize::Long),
                Operand::DataRegister { register, .. },
                Operand::FpuControlRegister {
                    register: control, ..
                },
            ) => {
                let Some(reg_bits) = M68KFamilyHandler::data_register_number(register) else {
                    return EncodeResult::error_with_span(
                        "invalid FMOVE.L source data register",
                        src.span(),
                    );
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(&mut bytes, 0xF000 | reg_bits as u16);
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0x8000 | Self::fpu_control_register_field(*control),
                );
                EncodeResult::ok(bytes)
            }
            (
                Some(OperationSize::Long),
                Operand::FpuControlRegister {
                    register: control, ..
                },
                Operand::DataRegister { register, .. },
            ) => {
                let Some(reg_bits) = M68KFamilyHandler::data_register_number(register) else {
                    return EncodeResult::error_with_span(
                        "invalid FMOVE.L destination data register",
                        dst.span(),
                    );
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(&mut bytes, 0xF000 | reg_bits as u16);
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0xA000 | Self::fpu_control_register_field(*control),
                );
                EncodeResult::ok(bytes)
            }
            (Some(OperationSize::Byte), _, _) | (Some(OperationSize::Word), _, _) => {
                EncodeResult::error(
                    "FMOVE currently supports only unsuffixed FP-register moves and .L data-register control transfers on m68020",
                )
            }
            (Some(OperationSize::Long), _, _) => EncodeResult::error(
                "FMOVE.L currently supports only data-register <-> FP control-register transfers on m68020",
            ),
            (None, _, _) => EncodeResult::error(
                "FMOVE currently supports only FPn-to-FPm register moves on m68020; scalar and memory forms remain for a later slice",
            ),
        }
    }

    fn encode_fmovem(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(
                "FMOVEM currently supports only unsuffixed FP register-list transfers on m68020",
            );
        }

        let [src, dst] = operands else {
            return EncodeResult::error("FMOVEM expects two operands");
        };

        match (src, dst) {
            (Operand::RegisterList { registers, .. }, dst) => {
                let dst_ea = match self.family.encode_effective_address(dst, None, ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                if !Self::fmovem_register_to_memory_destination(dst_ea.bits) {
                    return EncodeResult::error_with_span(
                        "invalid destination effective address for FMOVEM",
                        dst.span(),
                    );
                }

                let predecrement = Self::effective_address_mode(dst_ea.bits) == 4;
                let mask = match Self::fpu_register_list_mask(registers, predecrement) {
                    Ok(mask) => mask,
                    Err(err) => return err,
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(&mut bytes, 0xF000 | (dst_ea.bits & 0x003F));
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    if predecrement { 0xE000 } else { 0xF000 } | mask,
                );
                bytes.extend_from_slice(&dst_ea.extension);
                EncodeResult::ok(bytes)
            }
            (src, Operand::RegisterList { registers, .. }) => {
                let src_ea = match self.family.encode_effective_address(src, None, ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                if !Self::fmovem_memory_to_register_source(src_ea.bits) {
                    return EncodeResult::error_with_span(
                        "invalid source effective address for FMOVEM",
                        src.span(),
                    );
                }

                let mask = match Self::fpu_register_list_mask(registers, false) {
                    Ok(mask) => mask,
                    Err(err) => return err,
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(&mut bytes, 0xF000 | (src_ea.bits & 0x003F));
                M68KFamilyHandler::emit_word(&mut bytes, 0xD000 | mask);
                bytes.extend_from_slice(&src_ea.extension);
                EncodeResult::ok(bytes)
            }
            _ => EncodeResult::error("FMOVEM expects exactly one FP register-list operand"),
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
            ControlRegisterKind::Tc
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

    fn encode_chk_long(
        &self,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("CHK.L expects two operands");
        };

        let Operand::DataRegister { register, .. } = dst else {
            return EncodeResult::error_with_span(
                "CHK.L destination must be a data register",
                dst.span(),
            );
        };
        let Some(dst_reg) = M68KFamilyHandler::data_register_number(register) else {
            return EncodeResult::error_with_span("invalid CHK.L destination register", dst.span());
        };

        let src_ea = match self
            .family
            .encode_effective_address(src, Some(OperationSize::Long), ctx)
        {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !M68KFamilyHandler::logic_allows_source(src_ea.kind) {
            return EncodeResult::error_with_span(
                "invalid source effective address for CHK.L",
                src.span(),
            );
        }

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(
            &mut bytes,
            0x4100 | ((dst_reg as u16) << 9) | (src_ea.bits & 0x3F),
        );
        bytes.extend_from_slice(&src_ea.extension);
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
        if let Some(parsed) = parse_fpu_mnemonic(mnemonic) {
            if parsed.has_unknown_size_suffix {
                return EncodeResult::error(format!(
                    "unsupported size suffix for {}",
                    parsed.display_name
                ));
            }

            let target_name = match self.validate_fpu_target(&parsed.display_name, ctx) {
                Ok(target_name) => target_name,
                Err(err) => return err,
            };

            return match parsed.kind {
                FpuMnemonicKind::Fmove => self.encode_fmove(parsed.size, operands),
                FpuMnemonicKind::Fmovem => self.encode_fmovem(parsed.size, operands, ctx),
                _ => self.deferred_fpu_message(&parsed.display_name, target_name),
            };
        }

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
                return self
                    .family
                    .encode_move_from_ccr_instruction(parsed.size, operands, ctx);
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
                MnemonicKind::Chk if matches!(parsed.size, Some(OperationSize::Long)) => {
                    return self.encode_chk_long(operands, ctx);
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
                M68020MnemonicKind::Cas => {
                    self.family
                        .encode_cas_instruction(parsed.size, operands, ctx)
                }
                M68020MnemonicKind::Cas2 => {
                    self.family.encode_cas2_instruction(parsed.size, operands)
                }
                M68020MnemonicKind::Chk2 => self.family.encode_chk2_cmp2_instruction(
                    "CHK2",
                    true,
                    parsed.size,
                    operands,
                    ctx,
                ),
                M68020MnemonicKind::Cmp2 => self.family.encode_chk2_cmp2_instruction(
                    "CMP2",
                    false,
                    parsed.size,
                    operands,
                    ctx,
                ),
                M68020MnemonicKind::BitField(mnemonic) => {
                    self.family
                        .encode_bit_field_instruction(mnemonic, parsed.size, operands, ctx)
                }
                M68020MnemonicKind::Pack => self.family.encode_pack_unpk_instruction(
                    "PACK",
                    0x8140,
                    parsed.size,
                    operands,
                    ctx,
                ),
                M68020MnemonicKind::Unpk => self.family.encode_pack_unpk_instruction(
                    "UNPK",
                    0x8180,
                    parsed.size,
                    operands,
                    ctx,
                ),
                M68020MnemonicKind::Trapcc(condition) => {
                    self.family
                        .encode_trapcc_instruction(condition, parsed.size, operands, ctx)
                }
                M68020MnemonicKind::Callm => {
                    self.family
                        .encode_callm_instruction(parsed.size, operands, ctx)
                }
                M68020MnemonicKind::Rtm => {
                    self.family.encode_rtm_instruction(parsed.size, operands)
                }
                M68020MnemonicKind::Pflush => {
                    EncodeResult::error("PFLUSH is not supported on m68020")
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
            M68010MnemonicKind::Bkpt => {
                self.family
                    .encode_bkpt_instruction(parsed.size, operands, ctx)
            }
            M68010MnemonicKind::Rtd => {
                self.family
                    .encode_rtd_instruction(parsed.size, operands, ctx)
            }
        }
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        has_mnemonic(mnemonic)
            || matches!(
                parse_m68010_mnemonic(mnemonic),
                Some(parsed)
                    if matches!(
                        parsed.kind,
                        M68010MnemonicKind::Bkpt
                            | M68010MnemonicKind::Movec
                            | M68010MnemonicKind::Moves
                            | M68010MnemonicKind::Rtd
                    )
            )
            || has_m68020_mnemonic(mnemonic)
            || has_fpu_mnemonic(mnemonic)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use opcore::parser::Expr;
    use std::collections::HashMap;
    use types::symbol::SymbolTable;

    #[derive(Default)]
    struct TestContext {
        state_flags: HashMap<String, u32>,
        symbols: SymbolTable,
    }

    impl TestContext {
        fn with_cpu_state_flag(mut self, key: &str, value: u32) -> Self {
            self.state_flags.insert(key.to_string(), value);
            self
        }
    }

    impl AssemblerContext for TestContext {
        fn eval_expr(&self, _expr: &Expr) -> Result<i64, String> {
            Err("unexpected expression evaluation in test".to_string())
        }

        fn symbols(&self) -> &SymbolTable {
            &self.symbols
        }

        fn has_symbol(&self, _name: &str) -> bool {
            false
        }

        fn symbol_is_finalized(&self, _name: &str) -> Option<bool> {
            None
        }

        fn current_address(&self) -> u32 {
            0
        }

        fn pass(&self) -> u8 {
            2
        }

        fn scalar_value_symbol(&self, _name: &str) -> Option<i64> {
            None
        }

        fn cpu_state_flag(&self, key: &str) -> Option<u32> {
            self.state_flags.get(key).copied()
        }
    }

    #[test]
    fn fpu_mnemonics_report_incompatible_target_on_m68020() {
        let handler = M68020CpuHandler::new();
        let ctx = TestContext::default()
            .with_cpu_state_flag(crate::families::m68k::state::FPU_TARGET_KEY, 3);

        match handler.encode_instruction("FMOVE", &[], &ctx) {
            EncodeResult::Error(message, None) => {
                assert!(message.contains("FMOVE is not available with .fpu 68040 on m68020"));
                assert!(message.contains("68881, 68882"));
            }
            other => panic!("expected incompatible-target diagnostic, got {other:?}"),
        }
    }

    #[test]
    fn fmove_and_fmovem_encode_for_external_fpu_targets_on_m68020() {
        let handler = M68020CpuHandler::new();
        let ctx = TestContext::default()
            .with_cpu_state_flag(crate::families::m68k::state::FPU_TARGET_KEY, 1);

        match handler.encode_instruction(
            "FMOVE",
            &[
                Operand::FpuDataRegister {
                    register: "FP0".to_string(),
                    span: Default::default(),
                },
                Operand::FpuDataRegister {
                    register: "FP1".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF0, 0x00, 0x00, 0x80]),
            other => panic!("expected FMOVE encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FMOVEM",
            &[
                Operand::RegisterList {
                    registers: vec![
                        RegisterListRegister::FpuData(0),
                        RegisterListRegister::FpuData(2),
                    ],
                    span: Default::default(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF0, 0x10, 0xF0, 0x05]),
            other => panic!("expected FMOVEM encoding, got {other:?}"),
        }
    }
}
