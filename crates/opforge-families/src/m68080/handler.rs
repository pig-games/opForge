// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68080 CPU handler implementation.

use crate::families::m68k::state;
use crate::families::m68k::{
    operand::ControlRegisterKind, parse_fpu_mnemonic, parse_m68010_mnemonic, parse_m68080_mnemonic,
    parse_mnemonic, EffectiveAddressKind, FpuMnemonicKind, M68010MnemonicKind, M68080MnemonicKind,
    M68KFamilyHandler, MnemonicKind,
};
use crate::m68020::M68020CpuHandler;
use crate::m68040::M68040CpuHandler;
use opcore::parser::{BinaryOp, Expr};
use opcore::tokenizer::Span;
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};

use crate::families::m68k::Operand;

struct EncodedAmmxVea {
    a_bit: u16,
    ea_bits: u16,
    extension: Vec<u8>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TexSourceKind {
    Nested,
    ExternalScale,
    ScaledInside,
    Flat,
}

#[derive(Clone, Copy, Debug)]
struct ParsedTexSource {
    base: u8,
    v: u8,
    u: u8,
    modifier: Option<u8>,
    kind: TexSourceKind,
    span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TexVariant {
    Tex8x512,
    Tex16x256,
    Tex24x64,
    TexByte,
}

#[derive(Debug)]
pub struct M68080CpuHandler {
    base: M68040CpuHandler,
    fpu_core: M68020CpuHandler,
}

impl Default for M68080CpuHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl M68080CpuHandler {
    const APOLLO_AMMX_SELECTOR: u16 = 0b111;
    const LEGAL_FPU_TARGETS: [u32; 1] = [4];

    pub fn new() -> Self {
        Self {
            base: M68040CpuHandler::new(),
            fpu_core: M68020CpuHandler::new(),
        }
    }

    pub fn supports_m68k_mnemonic(&self, mnemonic: &str) -> bool {
        parse_m68080_mnemonic(mnemonic).is_some() || self.base.supports_mnemonic(mnemonic)
    }

    pub fn resolve_m68k_operands(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> Result<Vec<Operand>, String> {
        self.base.resolve_operands(mnemonic, operands, ctx)
    }

    pub fn encode_m68k_instruction(
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

            return self.encode_68080_fpu_instruction(
                mnemonic,
                operands,
                ctx,
                &parsed.display_name,
                target_name,
            );
        }

        if let Some(parsed) = parse_m68080_mnemonic(mnemonic) {
            match parsed.kind {
                M68080MnemonicKind::Addiw => {
                    if parsed.size != Some(crate::families::m68k::OperationSize::Long) {
                        return EncodeResult::error("ADDIW requires .L size on m68080");
                    }
                    return self.encode_addiw(operands, ctx);
                }
                M68080MnemonicKind::Cmpiw => {
                    if parsed.size != Some(crate::families::m68k::OperationSize::Long) {
                        return EncodeResult::error("CMPIW requires .L size on m68080");
                    }
                    return self.encode_cmpiw(operands, ctx);
                }
                M68080MnemonicKind::Clrq => {
                    return self.encode_clr_q(operands, ctx);
                }
                M68080MnemonicKind::Move2 => {
                    return self.encode_move2(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Movex => {
                    return self.encode_movex(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Moveh => {
                    return self.encode_moveh(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Moviw => {
                    if parsed.size != Some(crate::families::m68k::OperationSize::Long) {
                        return EncodeResult::error("MOVIW requires .L size on m68080");
                    }
                    if !self.apollo_mode_enabled(ctx) {
                        return EncodeResult::error(
                            "MOVIW is Apollo-gated on m68080; enable .apollo on",
                        );
                    }
                    return self.encode_moviw(operands, ctx);
                }
                M68080MnemonicKind::Mov3q => {
                    if !self.apollo_mode_enabled(ctx) {
                        return EncodeResult::error(
                            "MOV3Q is Apollo-gated on m68080; enable .apollo on",
                        );
                    }
                    return self.encode_mov3q(operands, ctx);
                }
                M68080MnemonicKind::Movs => {
                    return self.encode_movs(false, parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Movz => {
                    return self.encode_movs(true, parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Movz2 => {
                    return self.encode_movz2(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Touch => {
                    return self.encode_touch(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Extub => {
                    return self.encode_extu("EXTUB", 0x4BC0, parsed.size, operands);
                }
                M68080MnemonicKind::Extuw => {
                    return self.encode_extu("EXTUW", 0x4DC0, parsed.size, operands);
                }
                M68080MnemonicKind::Perm => {
                    return self.encode_perm(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Load => {
                    return self.encode_ammx_load(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Loadi => {
                    return self.encode_ammx_loadi(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Store => {
                    return self.encode_ammx_store(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Storei => {
                    return self.encode_ammx_storei(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Storec => {
                    return self.encode_ammx_storec(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Storeilm => {
                    return self.encode_ammx_storeilm(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Padd => {
                    return self.encode_ammx_padd(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Paddb => {
                    return self.encode_ammx_fixed_size_alias(
                        "PADDB",
                        parsed.size,
                        operands,
                        ctx,
                        crate::families::m68k::OperationSize::Byte,
                        0x10,
                    );
                }
                M68080MnemonicKind::Paddw => {
                    return self.encode_ammx_fixed_size_alias(
                        "PADDW",
                        parsed.size,
                        operands,
                        ctx,
                        crate::families::m68k::OperationSize::Word,
                        0x11,
                    );
                }
                M68080MnemonicKind::Paddusb => {
                    return self.encode_ammx_fixed_size_alias(
                        "PADDUSB",
                        parsed.size,
                        operands,
                        ctx,
                        crate::families::m68k::OperationSize::Byte,
                        0x14,
                    );
                }
                M68080MnemonicKind::Paddusw => {
                    return self.encode_ammx_fixed_size_alias(
                        "PADDUSW",
                        parsed.size,
                        operands,
                        ctx,
                        crate::families::m68k::OperationSize::Word,
                        0x15,
                    );
                }
                M68080MnemonicKind::Psub => {
                    return self.encode_ammx_psub(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Psubb => {
                    return self.encode_ammx_fixed_size_alias(
                        "PSUBB",
                        parsed.size,
                        operands,
                        ctx,
                        crate::families::m68k::OperationSize::Byte,
                        0x12,
                    );
                }
                M68080MnemonicKind::Psubw => {
                    return self.encode_ammx_fixed_size_alias(
                        "PSUBW",
                        parsed.size,
                        operands,
                        ctx,
                        crate::families::m68k::OperationSize::Word,
                        0x13,
                    );
                }
                M68080MnemonicKind::Psubusb => {
                    return self.encode_ammx_fixed_size_alias(
                        "PSUBUSB",
                        parsed.size,
                        operands,
                        ctx,
                        crate::families::m68k::OperationSize::Byte,
                        0x16,
                    );
                }
                M68080MnemonicKind::Psubusw => {
                    return self.encode_ammx_fixed_size_alias(
                        "PSUBUSW",
                        parsed.size,
                        operands,
                        ctx,
                        crate::families::m68k::OperationSize::Word,
                        0x17,
                    );
                }
                M68080MnemonicKind::Pavgb => {
                    return self.encode_ammx_dotless_fixed(
                        "PAVGB",
                        parsed.size,
                        operands,
                        ctx,
                        0x0C,
                    );
                }
                M68080MnemonicKind::Pmaxsb => {
                    return self.encode_ammx_vea_b_d_fixed("PMAXSB", None, operands, ctx, 0x34);
                }
                M68080MnemonicKind::Pmaxub => {
                    return self.encode_ammx_vea_b_d_fixed("PMAXUB", None, operands, ctx, 0x36);
                }
                M68080MnemonicKind::Pmaxsw => {
                    return self.encode_ammx_vea_b_d_fixed("PMAXSW", None, operands, ctx, 0x35);
                }
                M68080MnemonicKind::Pmaxuw => {
                    return self.encode_ammx_vea_b_d_fixed("PMAXUW", None, operands, ctx, 0x37);
                }
                M68080MnemonicKind::Pminsb => {
                    return self.encode_ammx_vea_b_d_fixed("PMINSB", None, operands, ctx, 0x30);
                }
                M68080MnemonicKind::Pminub => {
                    return self.encode_ammx_vea_b_d_fixed("PMINUB", None, operands, ctx, 0x32);
                }
                M68080MnemonicKind::Pminsw => {
                    return self.encode_ammx_vea_b_d_fixed("PMINSW", None, operands, ctx, 0x31);
                }
                M68080MnemonicKind::Pminuw => {
                    return self.encode_ammx_vea_b_d_fixed("PMINUW", None, operands, ctx, 0x33);
                }
                M68080MnemonicKind::Lslq => {
                    return self.encode_ammx_vea_b_d_fixed("LSLQ", None, operands, ctx, 0x38);
                }
                M68080MnemonicKind::Lsrq => {
                    return self.encode_ammx_vea_b_d_fixed("LSRQ", None, operands, ctx, 0x39);
                }
                M68080MnemonicKind::Bflyb => {
                    return self.encode_ammx_vea_b_pair_fixed("BFLYB", None, operands, ctx, 0x1C);
                }
                M68080MnemonicKind::Bflyw => {
                    return self.encode_ammx_vea_b_pair_fixed("BFLYW", None, operands, ctx, 0x1D);
                }
                M68080MnemonicKind::C2p => {
                    return self.encode_ammx_c2p(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Minterm => {
                    return self.encode_ammx_minterm(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Transhi => {
                    return self.encode_ammx_transpose(
                        "TRANSHI",
                        parsed.size,
                        operands,
                        ctx,
                        0x002,
                    );
                }
                M68080MnemonicKind::Translo => {
                    return self.encode_ammx_transpose(
                        "TRANSLO",
                        parsed.size,
                        operands,
                        ctx,
                        0x003,
                    );
                }
                M68080MnemonicKind::Storem => {
                    return self.encode_ammx_b_d_vea_fixed(
                        "STOREM",
                        parsed.size,
                        operands,
                        ctx,
                        0x25,
                    );
                }
                M68080MnemonicKind::Storem3 => {
                    return self.encode_ammx_storem3(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Tex => {
                    return self.encode_tex(mnemonic, parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Pmul88 => {
                    return self.encode_ammx_vea_b_d_fixed("PMUL88", None, operands, ctx, 0x18);
                }
                M68080MnemonicKind::Pmulh => {
                    return self.encode_ammx_vea_b_d_fixed("PMULH", None, operands, ctx, 0x1A);
                }
                M68080MnemonicKind::Pmull => {
                    return self.encode_ammx_vea_b_d_fixed("PMULL", None, operands, ctx, 0x1B);
                }
                M68080MnemonicKind::Pmula => {
                    return self.encode_ammx_vea_b_d_fixed("PMULA", None, operands, ctx, 0x19);
                }
                M68080MnemonicKind::Pand => {
                    return self.encode_ammx_vea_b_d_fixed("PAND", None, operands, ctx, 0x08);
                }
                M68080MnemonicKind::Pandn => {
                    return self.encode_ammx_vea_b_d_fixed("PANDN", None, operands, ctx, 0x0B);
                }
                M68080MnemonicKind::Por => {
                    return self.encode_ammx_vea_b_d_fixed("POR", None, operands, ctx, 0x09);
                }
                M68080MnemonicKind::Peor => {
                    return self.encode_ammx_vea_b_d_fixed("PEOR", None, operands, ctx, 0x0A);
                }
                M68080MnemonicKind::Bsel => {
                    return self.encode_ammx_vea_b_d_fixed("BSEL", None, operands, ctx, 0x29);
                }
                M68080MnemonicKind::Pcmpeqb => {
                    return self.encode_ammx_vea_b_d_fixed("PCMPEQB", None, operands, ctx, 0x20);
                }
                M68080MnemonicKind::Pcmphib => {
                    return self.encode_ammx_vea_b_d_fixed("PCMPHIB", None, operands, ctx, 0x22);
                }
                M68080MnemonicKind::Pcmpgeb => {
                    return self.encode_ammx_vea_b_d_fixed("PCMPGEB", None, operands, ctx, 0x2C);
                }
                M68080MnemonicKind::Pcmpgtb => {
                    return self.encode_ammx_vea_b_d_fixed("PCMPGTB", None, operands, ctx, 0x2E);
                }
                M68080MnemonicKind::Pcmpeqw => {
                    return self.encode_ammx_vea_b_d_fixed("PCMPEQW", None, operands, ctx, 0x21);
                }
                M68080MnemonicKind::Pcmphiw => {
                    return self.encode_ammx_vea_b_d_fixed("PCMPHIW", None, operands, ctx, 0x23);
                }
                M68080MnemonicKind::Pcmpgew => {
                    return self.encode_ammx_vea_b_d_fixed("PCMPGEW", None, operands, ctx, 0x2D);
                }
                M68080MnemonicKind::Pcmpgtw => {
                    return self.encode_ammx_vea_b_d_fixed("PCMPGTW", None, operands, ctx, 0x2F);
                }
                M68080MnemonicKind::Pack3216 => {
                    return self.encode_ammx_pack3216(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Packuswb => {
                    return self.encode_ammx_packuswb(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Unpack1632 => {
                    return self.encode_ammx_unpack1632(parsed.size, operands, ctx);
                }
                M68080MnemonicKind::Vperm => {
                    return self.encode_ammx_vperm(parsed.size, operands, ctx);
                }
            }
        }

        if let Some(result) = self.try_encode_extended_short_branch(mnemonic, operands, ctx) {
            return result;
        }

        if let Some(parsed) = parse_m68010_mnemonic(mnemonic) {
            if parsed.has_unknown_size_suffix {
                return EncodeResult::error(format!(
                    "unsupported size suffix for {}",
                    parsed.display_name
                ));
            }

            if matches!(parsed.kind, M68010MnemonicKind::Movec) {
                return self.encode_movec_68080(operands, parsed.size.is_some());
            }
        }

        self.base.encode_instruction(mnemonic, operands, ctx)
    }

    fn fpu_target_name(state_value: u32) -> &'static str {
        match state_value {
            1 => "68881",
            2 => "68882",
            3 => "68040",
            4 => "68080",
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
                "{display_name} requires an active .fpu target on m68080; legal .fpu targets for m68080 FPU instructions: 68080"
            )));
        }

        if !Self::LEGAL_FPU_TARGETS.contains(&target) {
            return Err(EncodeResult::error(format!(
                "{display_name} is not available with .fpu {} on m68080; legal .fpu targets for m68080 FPU instructions: 68080",
                Self::fpu_target_name(target),
            )));
        }

        Ok(Self::fpu_target_name(target))
    }

    fn deferred_fpu_message(&self, display_name: &str, target_name: &str) -> EncodeResult<Vec<u8>> {
        EncodeResult::error(format!(
            "{display_name} is recognized for .fpu {} on m68080, but FPU encoding is not yet implemented",
            target_name,
        ))
    }

    fn banked_fpu_register_descriptor(name: &str) -> Option<(u16, u16)> {
        if let Some(reg) = M68KFamilyHandler::fpu_data_register_number(name) {
            return Some((0, u16::from(reg)));
        }
        Self::e_register_descriptor(name)
    }

    fn banked_xor_bits(base: (u16, u16), destination: (u16, u16)) -> (u16, u16) {
        let base_bits = ((base.0 & 0x3) << 3) | (base.1 & 0x7);
        let destination_bits = ((destination.0 & 0x3) << 3) | (destination.1 & 0x7);
        let xor_bits = base_bits ^ destination_bits;
        (((xor_bits >> 3) & 0x3), xor_bits & 0x7)
    }

    fn remap_banked_fpu_source_operand(
        operand: &Operand,
        has_scalar_size: bool,
    ) -> Option<(u16, Operand)> {
        match operand {
            Operand::FpuDataRegister { register, span } => {
                if let Some((bank_bits, register_bits)) = Self::e_register_descriptor(register) {
                    if has_scalar_size {
                        return Some((
                            bank_bits,
                            Operand::DataRegister {
                                register: format!("D{register_bits}"),
                                span: *span,
                            },
                        ));
                    }

                    return Some((
                        bank_bits,
                        Operand::FpuDataRegister {
                            register: format!("FP{register_bits}"),
                            span: *span,
                        },
                    ));
                }

                if !has_scalar_size {
                    let register_bits = M68KFamilyHandler::fpu_data_register_number(register)?;
                    return Some((
                        0,
                        Operand::FpuDataRegister {
                            register: format!("FP{register_bits}"),
                            span: *span,
                        },
                    ));
                }

                None
            }
            _ => Some((0, operand.clone())),
        }
    }

    fn encode_68080_banked_fpu_three_operand(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        display_name: &str,
        target_name: &str,
    ) -> Option<EncodeResult<Vec<u8>>> {
        let parsed = parse_fpu_mnemonic(mnemonic)?;
        if !matches!(
            parsed.kind,
            FpuMnemonicKind::Fadd
                | FpuMnemonicKind::Fcmp
                | FpuMnemonicKind::Fdiv
                | FpuMnemonicKind::Fmul
                | FpuMnemonicKind::Frem
                | FpuMnemonicKind::Fscale
                | FpuMnemonicKind::Fsub
        ) {
            return None;
        }

        let [src, middle, dst] = operands else {
            return None;
        };

        let Operand::FpuDataRegister {
            register: middle_register,
            span: middle_span,
        } = middle
        else {
            return None;
        };
        let Operand::FpuDataRegister {
            register: dst_register,
            ..
        } = dst
        else {
            return None;
        };

        let source_is_banked = matches!(
            src,
            Operand::FpuDataRegister { register, .. }
                if M68KFamilyHandler::fpu_banked_data_register_number(register).is_some()
        );
        let middle_is_banked =
            M68KFamilyHandler::fpu_banked_data_register_number(middle_register).is_some();
        let dst_is_banked =
            M68KFamilyHandler::fpu_banked_data_register_number(dst_register).is_some();
        if !source_is_banked && !middle_is_banked && !dst_is_banked {
            return None;
        }

        let middle_descriptor = Self::banked_fpu_register_descriptor(middle_register)?;
        let dst_descriptor = Self::banked_fpu_register_descriptor(dst_register)?;
        let (aa_bits, remapped_src) =
            Self::remap_banked_fpu_source_operand(src, parsed.size.is_some())?;
        let remapped_middle = Operand::FpuDataRegister {
            register: format!("FP{}", middle_descriptor.1),
            span: *middle_span,
        };
        let base_operands = [remapped_src, remapped_middle];
        let (cc_bits, ccc_bits) = Self::banked_xor_bits(middle_descriptor, dst_descriptor);

        Some(
            match self
                .fpu_core
                .encode_supported_fpu_core_mnemonic(mnemonic, &base_operands, ctx)
            {
                Some(EncodeResult::Ok(bytes)) => {
                    Self::with_bank_prefix(bytes, aa_bits, middle_descriptor.0, cc_bits, ccc_bits)
                }
                Some(EncodeResult::NotFound) => {
                    self.deferred_fpu_message(display_name, target_name)
                }
                Some(EncodeResult::Error(message, span)) => EncodeResult::Error(
                    Self::normalize_fpu_error_message(display_name, message),
                    span,
                ),
                None => self.deferred_fpu_message(display_name, target_name),
            },
        )
    }

    fn encode_68080_fpu_instruction(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        display_name: &str,
        target_name: &str,
    ) -> EncodeResult<Vec<u8>> {
        if let Some(result) = self.encode_68080_banked_fpu_three_operand(
            mnemonic,
            operands,
            ctx,
            display_name,
            target_name,
        ) {
            return result;
        }

        if operands.iter().any(|operand| {
            matches!(operand, Operand::FpuDataRegister { register, .. }
                if M68KFamilyHandler::fpu_banked_data_register_number(register).is_some())
        }) {
            return self.deferred_fpu_message(display_name, target_name);
        }

        match self
            .fpu_core
            .encode_supported_fpu_core_mnemonic(mnemonic, operands, ctx)
        {
            Some(EncodeResult::Ok(bytes)) => EncodeResult::ok(bytes),
            Some(EncodeResult::NotFound) => self.deferred_fpu_message(display_name, target_name),
            Some(EncodeResult::Error(message, span)) => EncodeResult::Error(
                Self::normalize_fpu_error_message(display_name, message),
                span,
            ),
            None => self.deferred_fpu_message(display_name, target_name),
        }
    }

    fn normalize_fpu_error_message(display_name: &str, message: String) -> String {
        if message == "68000 immediate operands require an explicit instruction size" {
            return format!(
                "{display_name} immediate literal source forms are not yet implemented on m68080"
            );
        }

        message.replace("m68020", "m68080")
    }

    fn encode_clr_q(
        &self,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("CLR.Q", ctx) {
            return err;
        }

        let [dst] = operands else {
            return EncodeResult::error("CLR.Q expects one operand");
        };

        let dst_ea = match self.base.family().encode_effective_address(
            dst,
            Some(crate::families::m68k::OperationSize::Long),
            ctx,
        ) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !M68KFamilyHandler::data_alterable(dst_ea.kind) {
            return EncodeResult::error_with_span(
                "invalid destination effective address for CLR.Q",
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, 0xAE00 | (dst_ea.bits & 0x003F));
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_extu(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        match size {
            Some(crate::families::m68k::OperationSize::Long) => {}
            Some(crate::families::m68k::OperationSize::Byte) => {
                return EncodeResult::error(format!("{mnemonic} does not support .B size"));
            }
            Some(crate::families::m68k::OperationSize::Word) => {
                return EncodeResult::error(format!("{mnemonic} does not support .W size"));
            }
            None => return EncodeResult::error(format!("{mnemonic} requires an explicit .L size")),
        }

        let [reg] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one data register operand"));
        };
        let reg_name = match reg {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    format!("{mnemonic} operand must be a data register"),
                    reg.span(),
                );
            }
        };
        let Some((bank_bits, reg_bits)) = Self::banked_data_register_descriptor(reg_name) else {
            return EncodeResult::error_with_span(
                format!("invalid {mnemonic} register"),
                reg.span(),
            );
        };

        let mut body = Vec::new();
        M68KFamilyHandler::emit_word(&mut body, opcode_base | reg_bits);
        Self::with_bank_prefix(body, bank_bits, bank_bits, 0, 0)
    }

    fn encode_perm(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("PERM does not accept a size suffix on m68080");
        }

        let [selector, left, right] = operands else {
            return EncodeResult::error("PERM expects three operands: #imm,Ra,Rb");
        };

        let Operand::Immediate { expr, span } = selector else {
            return EncodeResult::error_with_span(
                "PERM first operand must be an immediate selector",
                selector.span(),
            );
        };
        let (selector_value, _) = match M68KFamilyHandler::eval_expr_or_placeholder(expr, ctx, 0) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, *span),
        };
        if !(0..=0x0FFF).contains(&selector_value) {
            return EncodeResult::error_with_span(
                format!("PERM selector {selector_value} out of range (0-4095)"),
                *span,
            );
        }

        let Some((left_bank_bits, left_code)) = Self::perm_register_descriptor(left) else {
            return EncodeResult::error_with_span(
                "PERM left register must be D0-D7 or A0-A7",
                left.span(),
            );
        };
        let Some((right_bank_bits, right_code)) = Self::perm_register_descriptor(right) else {
            return EncodeResult::error_with_span(
                "PERM right register must be D0-D7 or A0-A7",
                right.span(),
            );
        };

        let mut body = Vec::new();
        M68KFamilyHandler::emit_word(&mut body, 0x4CC0 | left_code);
        M68KFamilyHandler::emit_word(
            &mut body,
            (right_code << 12) | (selector_value as u16 & 0x0FFF),
        );
        Self::with_bank_prefix(body, left_bank_bits, right_bank_bits, 0, 0)
    }

    fn e_register_descriptor(name: &str) -> Option<(u16, u16)> {
        let upper = name.to_ascii_uppercase();
        let suffix = upper.strip_prefix('E')?;
        let reg = suffix.parse::<u8>().ok()?;
        if reg > 23 {
            return None;
        }
        Some((u16::from(reg / 8 + 1), u16::from(reg % 8)))
    }

    fn banked_data_register_descriptor(name: &str) -> Option<(u16, u16)> {
        if let Some(reg) = M68KFamilyHandler::data_register_number(name) {
            return Some((0, reg as u16));
        }
        Self::e_register_descriptor(name)
    }

    fn perm_register_descriptor(operand: &Operand) -> Option<(u16, u16)> {
        match operand {
            Operand::DataRegister { register, .. } => {
                Self::banked_data_register_descriptor(register)
            }
            Operand::AddressRegister { register, .. } => Some((
                0,
                8 + u16::from(M68KFamilyHandler::address_register_number(register)?),
            )),
            _ => None,
        }
    }

    fn bank_prefix_size_bits(body_len: usize) -> Option<u16> {
        match body_len {
            2 => Some(0),
            4 => Some(1),
            6 => Some(2),
            8 => Some(3),
            _ => None,
        }
    }

    fn with_bank_prefix(
        mut body: Vec<u8>,
        aa_bits: u16,
        bb_bits: u16,
        cc_bits: u16,
        ccc_bits: u16,
    ) -> EncodeResult<Vec<u8>> {
        if aa_bits == 0 && bb_bits == 0 && cc_bits == 0 && ccc_bits == 0 {
            return EncodeResult::ok(body);
        }

        let Some(size_bits) = Self::bank_prefix_size_bits(body.len()) else {
            return EncodeResult::error(
                "generated BANK prefix requires a 2, 4, 6, or 8 byte base instruction on m68080",
            );
        };

        let prefix_word = 0x7100
            | ((ccc_bits & 0x7) << 9)
            | ((size_bits & 0x3) << 6)
            | ((cc_bits & 0x3) << 4)
            | ((aa_bits & 0x3) << 2)
            | (bb_bits & 0x3);

        let mut bytes = Vec::with_capacity(body.len() + 2);
        M68KFamilyHandler::emit_word(&mut bytes, prefix_word);
        bytes.append(&mut body);
        EncodeResult::ok(bytes)
    }

    fn movec_control_register_code_68080(register: ControlRegisterKind) -> Option<u16> {
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
            ControlRegisterKind::Pcr => Some(0x808),
            ControlRegisterKind::Ccc => Some(0x809),
            ControlRegisterKind::Iep1 => Some(0x80A),
            ControlRegisterKind::Iep2 => Some(0x80B),
            ControlRegisterKind::Bpc => Some(0x80C),
            ControlRegisterKind::Bpw => Some(0x80D),
            ControlRegisterKind::Dch => Some(0x80E),
            ControlRegisterKind::Dcm => Some(0x80F),
            ControlRegisterKind::Str => Some(0x00A),
            ControlRegisterKind::Stc => Some(0x00B),
            ControlRegisterKind::Sth => Some(0x00C),
            ControlRegisterKind::Stb => Some(0x00D),
            ControlRegisterKind::Mwr => Some(0x00E),
            ControlRegisterKind::Caar => None,
        }
    }

    fn encode_movec_68080(
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
                "MOVEC CAAR is not supported on m68080",
                control_operand.span(),
            );
        }
        let Some(control_bits) = Self::movec_control_register_code_68080(*register) else {
            return EncodeResult::error_with_span(
                "unsupported MOVEC control register for m68080",
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

    fn try_encode_extended_short_branch(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> Option<EncodeResult<Vec<u8>>> {
        let parsed = parse_mnemonic(mnemonic)?;
        if !parsed.has_unknown_size_suffix || !mnemonic.to_ascii_uppercase().ends_with(".S+") {
            return None;
        }

        let condition_bits = match parsed.kind {
            MnemonicKind::Bra => 0x0,
            MnemonicKind::Bsr => 0x1,
            MnemonicKind::Bcc(condition) => condition.opcode_bits(),
            _ => return None,
        };

        Some(self.encode_extended_short_branch(mnemonic, condition_bits, operands, ctx))
    }

    fn encode_extended_short_branch(
        &self,
        mnemonic: &str,
        condition_bits: u16,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [target] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one branch target"));
        };
        let Operand::BranchTarget { expr, .. } = target else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} requires a branch target expression"),
                target.span(),
            );
        };

        let (offset, unresolved) = match M68KFamilyHandler::eval_expr_or_placeholder(expr, ctx, 0) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, target.span()),
        };

        if !unresolved && (offset & 1) != 0 {
            return EncodeResult::error_with_span(
                format!("{mnemonic} branch displacement must be even on m68080"),
                target.span(),
            );
        }

        let encoded_displacement = if unresolved {
            0x01_u8
        } else {
            match Self::encode_extended_short_displacement(offset) {
                Some(value) => value,
                None => {
                    return EncodeResult::error_with_span(
                        format!(
                            "{mnemonic} extended-short displacement out of range: offset {offset}"
                        ),
                        target.span(),
                    );
                }
            }
        };

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(
            &mut bytes,
            0x6000 | (condition_bits << 8) | encoded_displacement as u16,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_extended_short_displacement(offset: i64) -> Option<u8> {
        if (offset & 1) != 0 {
            return None;
        }

        let encoded = if (128..=254).contains(&offset) {
            offset - 127
        } else if (-256..=-132).contains(&offset) {
            offset + 129
        } else {
            return None;
        };

        (-128..=127)
            .contains(&encoded)
            .then_some((encoded as i8) as u8)
    }

    fn encode_ammx_load(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_ammx_load_family("LOAD", size, operands, ctx, 0x0)
    }

    fn encode_ammx_loadi(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("LOADI", ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error("LOADI does not accept a size suffix on m68080");
        }
        let [src, selector] = operands else {
            return EncodeResult::error(
                "AMMX LOADI expects two operands: <vea>,Dn/En selector register",
            );
        };
        let selector_code = match selector {
            Operand::DataRegister { register, .. } => {
                match Self::ammx_data_register_name_code(register) {
                    Some(value) => value,
                    None => {
                        return EncodeResult::error_with_span(
                        "AMMX LOADI selector register must be D0-D7 or E0-E23; selector values map modulo 64 to D/A/B/E banks",
                        selector.span(),
                    );
                    }
                }
            }
            _ => {
                return EncodeResult::error_with_span(
                    "AMMX LOADI selector register must be D0-D7 or E0-E23; selector values map modulo 64 to D/A/B/E banks",
                    selector.span(),
                );
            }
        };
        let vea = match self.encode_ammx_load_source("LOADI", size, src, ctx) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (selector_high, selector_low) = Self::ammx_register_fields(selector_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            0,
            selector_high,
            vea.ea_bits,
            (0x1 << 12) | (selector_low << 8) | 0x01,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_store(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_ammx_store_family("STORE", size, operands, ctx, 0x0, 0x04)
    }

    fn encode_ammx_storei(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("STOREI", ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error("STOREI does not accept a size suffix on m68080");
        }
        let [selector, vea_operand] = operands else {
            return EncodeResult::error(
                "AMMX STOREI expects two operands: Dn/En selector register,<vea>",
            );
        };
        let selector_code = match selector {
            Operand::DataRegister { register, .. } => {
                match Self::ammx_data_register_name_code(register) {
                    Some(value) => value,
                    None => {
                        return EncodeResult::error_with_span(
                        "AMMX STOREI selector register must be D0-D7 or E0-E23; selector values map modulo 64 to D/A/B/E banks",
                        selector.span(),
                    );
                    }
                }
            }
            _ => {
                return EncodeResult::error_with_span(
                    "AMMX STOREI selector register must be D0-D7 or E0-E23; selector values map modulo 64 to D/A/B/E banks",
                    selector.span(),
                );
            }
        };
        let vea = match self.encode_ammx_vea(vea_operand, "STOREI destination", ctx) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (selector_high, selector_low) = Self::ammx_register_fields(selector_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            selector_high,
            0,
            vea.ea_bits,
            (selector_low << 12) | (0x1 << 8) | 0x04,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_storec(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_ammx_b_d_vea_fixed("STOREC", size, operands, ctx, 0x24)
    }

    fn encode_ammx_storeilm(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_ammx_b_d_vea_fixed("STOREILM", size, operands, ctx, 0x05)
    }

    fn encode_ammx_load_family(
        &self,
        mnemonic: &str,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        src_low_nibble: u16,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo(mnemonic, ctx) {
            return err;
        }
        let [src, dst] = operands else {
            return EncodeResult::error(format!(
                "AMMX {mnemonic} expects two operands: <vea>,Dn/En"
            ));
        };
        let dst_code = match self.ammx_data_register_code(dst) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (dst_high, dst_low) = Self::ammx_register_fields(dst_code);

        let vea = match self.encode_ammx_load_source(mnemonic, size, src, ctx) {
            Ok(value) => value,
            Err(err) => return err,
        };

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            0,
            dst_high,
            vea.ea_bits,
            (src_low_nibble << 12) | (dst_low << 8) | 0x01,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_load_source(
        &self,
        mnemonic: &str,
        size: Option<crate::families::m68k::OperationSize>,
        operand: &Operand,
        ctx: &dyn AssemblerContext,
    ) -> Result<EncodedAmmxVea, EncodeResult<Vec<u8>>> {
        match operand {
            Operand::Immediate { expr, .. } if mnemonic.eq_ignore_ascii_case("LOAD") => {
                if size != Some(crate::families::m68k::OperationSize::Word) {
                    return Err(EncodeResult::error(
                        "AMMX LOAD immediate source requires .W size on m68080",
                    ));
                }
                let value = ctx
                    .eval_expr(expr)
                    .map_err(|err| EncodeResult::error_with_span(err, operand.span()))?;
                if !(-32768..=65535).contains(&value) {
                    return Err(EncodeResult::error_with_span(
                        "AMMX LOAD.W immediate source must fit in 16 bits",
                        operand.span(),
                    ));
                }

                Ok(EncodedAmmxVea {
                    a_bit: 0,
                    ea_bits: (0b111_u16 << 3) | 0b100,
                    extension: (value as u16).to_be_bytes().to_vec(),
                })
            }
            Operand::Immediate { .. } => Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} source must be a vector effective address"),
                operand.span(),
            )),
            _ => {
                if size.is_some() {
                    return Err(EncodeResult::error(format!(
                        "{mnemonic} does not accept a size suffix on m68080 unless the source is an immediate .W literal"
                    )));
                }
                self.encode_ammx_vea(operand, &format!("{mnemonic} source"), ctx)
            }
        }
    }

    fn encode_ammx_store_family(
        &self,
        mnemonic: &str,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        dst_low_nibble: u16,
        opcode: u16,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo(mnemonic, ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error(format!(
                "{mnemonic} does not accept a size suffix on m68080"
            ));
        }
        let [src, vea_operand] = operands else {
            return EncodeResult::error(format!(
                "AMMX {mnemonic} expects two operands: Dn/En,<vea>"
            ));
        };
        let vea = match self.encode_ammx_vea(vea_operand, &format!("{mnemonic} destination"), ctx) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let src_code = match self.ammx_data_register_code(src) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (src_high, src_low) = Self::ammx_register_fields(src_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            src_high,
            0,
            vea.ea_bits,
            (src_low << 12) | (dst_low_nibble << 8) | opcode,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_addiw(
        &self,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("ADDIW expects two operands: #<imm16>,<ea>");
        };
        let Operand::Immediate { expr, .. } = src else {
            return EncodeResult::error_with_span(
                "ADDIW source must be an immediate value",
                src.span(),
            );
        };
        let value = match M68KFamilyHandler::eval_expr(expr, ctx) {
            Ok(v) => v,
            Err(err) => return EncodeResult::error_with_span(err, src.span()),
        };
        let Some(imm16) = Self::encode_word_extended_immediate(value) else {
            return EncodeResult::error_with_span(
                format!(
                    "ADDIW immediate {value} out of range for 16-bit word pattern (-32768..65535)"
                ),
                src.span(),
            );
        };
        let dst_ea = match self.base.family().encode_effective_address(
            dst,
            Some(crate::families::m68k::OperationSize::Long),
            ctx,
        ) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        let mut bytes = Vec::new();
        // PRM: 0000 0110 11 <mode><reg>  (size field = 11 replaces CALLM)
        Self::emit_word(&mut bytes, 0x06C0 | (dst_ea.bits & 0x3F));
        Self::emit_word(&mut bytes, imm16);
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_cmpiw(
        &self,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("CMPIW expects two operands: #<imm16>,<ea>");
        };
        let Operand::Immediate { expr, .. } = src else {
            return EncodeResult::error_with_span(
                "CMPIW source must be an immediate value",
                src.span(),
            );
        };
        let value = match M68KFamilyHandler::eval_expr(expr, ctx) {
            Ok(v) => v,
            Err(err) => return EncodeResult::error_with_span(err, src.span()),
        };
        let Some(imm16) = Self::encode_word_extended_immediate(value) else {
            return EncodeResult::error_with_span(
                format!(
                    "CMPIW immediate {value} out of range for 16-bit word pattern (-32768..65535)"
                ),
                src.span(),
            );
        };
        let dst_ea = match self.base.family().encode_effective_address(
            dst,
            Some(crate::families::m68k::OperationSize::Long),
            ctx,
        ) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        let mut bytes = Vec::new();
        // PRM: 0100 1110 00 <mode><reg>
        Self::emit_word(&mut bytes, 0x4E00 | (dst_ea.bits & 0x3F));
        Self::emit_word(&mut bytes, imm16);
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_moviw(
        &self,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("MOVIW expects two operands: #<imm16>,<ea>");
        };
        let Operand::Immediate { expr, .. } = src else {
            return EncodeResult::error_with_span(
                "MOVIW source must be an immediate value",
                src.span(),
            );
        };
        let value = match M68KFamilyHandler::eval_expr(expr, ctx) {
            Ok(v) => v,
            Err(err) => return EncodeResult::error_with_span(err, src.span()),
        };
        let Some(imm16) = Self::encode_word_extended_immediate(value) else {
            return EncodeResult::error_with_span(
                format!(
                    "MOVIW immediate {value} out of range for 16-bit word pattern (-32768..65535)"
                ),
                src.span(),
            );
        };
        let dst_ea = match self.base.family().encode_effective_address(
            dst,
            Some(crate::families::m68k::OperationSize::Long),
            ctx,
        ) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        let mut bytes = Vec::new();
        // PRM: 1010 0010 00 <mode><reg>  (Line-A)
        Self::emit_word(&mut bytes, 0xA200 | (dst_ea.bits & 0x3F));
        Self::emit_word(&mut bytes, imm16);
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_mov3q(
        &self,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("MOV3Q expects two operands: #<quick>,<ea>");
        };
        let Operand::Immediate { expr, .. } = src else {
            return EncodeResult::error_with_span(
                "MOV3Q source must be an immediate value",
                src.span(),
            );
        };
        let value = match M68KFamilyHandler::eval_expr(expr, ctx) {
            Ok(v) => v,
            Err(err) => return EncodeResult::error_with_span(err, src.span()),
        };
        // PRM: valid immediates are -1 and 1..7; data field encodes -1 as 0
        let data_field: u16 = match value {
            -1 => 0,
            1..=7 => value as u16,
            _ => {
                return EncodeResult::error_with_span(
                    format!("MOV3Q immediate {value} out of range; valid values are -1 and 1..7"),
                    src.span(),
                );
            }
        };
        let dst_ea = match self.base.family().encode_effective_address(
            dst,
            Some(crate::families::m68k::OperationSize::Long),
            ctx,
        ) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        let mut bytes = Vec::new();
        // PRM: 1010 <data:3> 001 <mode><reg>  (Line-A)
        Self::emit_word(
            &mut bytes,
            0xA040 | (data_field << 9) | (dst_ea.bits & 0x3F),
        );
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_movs(
        &self,
        zero_extend: bool,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let mnemonic = if zero_extend { "MOVZ" } else { "MOVS" };
        if let Err(err) = self.require_apollo(mnemonic, ctx) {
            return err;
        }

        let Some(size) = size else {
            return EncodeResult::error(format!(
                "{mnemonic} requires an explicit .B or .W size suffix"
            ));
        };
        let size_bit = match size {
            crate::families::m68k::OperationSize::Byte => 0_u16,
            crate::families::m68k::OperationSize::Word => 1_u16,
            crate::families::m68k::OperationSize::Long => {
                return EncodeResult::error(format!("{mnemonic} requires .B or .W size on m68080"));
            }
        };

        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands: <ea>,Dn"));
        };
        let Operand::DataRegister { register, .. } = dst else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} destination must be a data register"),
                dst.span(),
            );
        };
        let Some(dst_bits) = M68KFamilyHandler::data_register_number(register) else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} destination must be D0-D7"),
                dst.span(),
            );
        };

        let src_ea = match self
            .base
            .family()
            .encode_effective_address(src, Some(size), ctx)
        {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !M68KFamilyHandler::move_allows_source(src_ea.kind, size) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid source effective address for {mnemonic}{}",
                    size.suffix()
                ),
                src.span(),
            );
        }

        let mut bytes = Vec::new();
        let opcode = 0xA100
            | ((dst_bits as u16) << 9)
            | ((zero_extend as u16) << 7)
            | (size_bit << 6)
            | (src_ea.bits & 0x3F);
        Self::emit_word(&mut bytes, opcode);
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_movex(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let (size, size_bits) = match Self::movex_size_bits(size) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let [left, right] = operands else {
            return EncodeResult::error("MOVEX expects two operands");
        };

        if let Ok(dst_reg) = Self::integer_register_field(right) {
            return self.encode_movex_direction(size, size_bits, left, dst_reg, false, ctx);
        }
        if let Ok(src_reg) = Self::integer_register_field(left) {
            return self.encode_movex_direction(size, size_bits, right, src_reg, true, ctx);
        }

        EncodeResult::error("MOVEX expects one Dn/An register operand and one effective address")
    }

    fn encode_movex_direction(
        &self,
        size: crate::families::m68k::OperationSize,
        size_bits: u16,
        ea_operand: &Operand,
        register_bits: u16,
        register_to_ea: bool,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let ea = match self
            .base
            .family()
            .encode_effective_address(ea_operand, Some(size), ctx)
        {
            Ok(ea) => ea,
            Err(err) => return err,
        };

        let valid = if register_to_ea {
            M68KFamilyHandler::move_allows_destination(ea.kind)
        } else {
            M68KFamilyHandler::move_allows_source(ea.kind, size)
        };
        if !valid {
            let role = if register_to_ea {
                "destination"
            } else {
                "source"
            };
            return EncodeResult::error_with_span(
                format!(
                    "invalid {role} effective address for MOVEX{}",
                    size.suffix()
                ),
                ea_operand.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x0E00 | (size_bits << 6) | (ea.bits & 0x3F));
        Self::emit_word(
            &mut bytes,
            (register_bits << 12) | ((register_to_ea as u16) << 11) | 0x0010,
        );
        bytes.extend_from_slice(&ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_moveh(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("MOVEH does not accept a size suffix on m68080");
        }
        let [left, right] = operands else {
            return EncodeResult::error("MOVEH expects two operands");
        };

        if let Ok(dst_reg) = Self::integer_register_field(right) {
            return self.encode_moveh_direction(left, dst_reg, false, ctx);
        }
        if let Ok(src_reg) = Self::integer_register_field(left) {
            return self.encode_moveh_direction(right, src_reg, true, ctx);
        }

        EncodeResult::error(
            "MOVEH expects one Dn/An register operand and one memory effective address",
        )
    }

    fn encode_moveh_direction(
        &self,
        ea_operand: &Operand,
        register_bits: u16,
        register_to_ea: bool,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let ea = match self.base.family().encode_effective_address(
            ea_operand,
            Some(crate::families::m68k::OperationSize::Word),
            ctx,
        ) {
            Ok(ea) => ea,
            Err(err) => return err,
        };

        if register_to_ea {
            if !Self::integer_memory_destination(ea.kind) {
                return EncodeResult::error_with_span(
                    "invalid destination effective address for MOVEH",
                    ea_operand.span(),
                );
            }
        } else if !Self::integer_memory_source(ea.kind) {
            return EncodeResult::error_with_span(
                "invalid source effective address for MOVEH",
                ea_operand.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x0E40 | (ea.bits & 0x3F));
        Self::emit_word(
            &mut bytes,
            (register_bits << 12) | ((register_to_ea as u16) << 11) | 0x0013,
        );
        bytes.extend_from_slice(&ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_move2(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let (size, _) = match Self::move2_size_bits("MOVE2", size, true) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let [left, right] = operands else {
            return EncodeResult::error("MOVE2 expects two operands");
        };

        match (left, right) {
            (ea, Operand::RegisterPair { .. }) => {
                self.encode_move2_direction("MOVE2", size, ea, right, false, ctx)
            }
            (Operand::RegisterPair { .. }, ea) => {
                self.encode_move2_direction("MOVE2", size, ea, left, true, ctx)
            }
            _ => EncodeResult::error(
                "MOVE2 expects one memory effective address operand and one register pair operand",
            ),
        }
    }

    fn encode_movz2(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let (size, _) = match Self::move2_size_bits("MOVZ2", size, false) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let [src, dst_pair] = operands else {
            return EncodeResult::error("MOVZ2 expects two operands: <ea>,Rn:Rn");
        };
        self.encode_move2_direction("MOVZ2", size, src, dst_pair, false, ctx)
    }

    fn encode_move2_direction(
        &self,
        mnemonic: &str,
        size: crate::families::m68k::OperationSize,
        ea_operand: &Operand,
        pair_operand: &Operand,
        pair_to_ea: bool,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let ea = match self
            .base
            .family()
            .encode_effective_address(ea_operand, Some(size), ctx)
        {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if pair_to_ea {
            if !Self::integer_memory_destination(ea.kind) {
                return EncodeResult::error_with_span(
                    format!(
                        "invalid destination effective address for {mnemonic}{}",
                        size.suffix()
                    ),
                    ea_operand.span(),
                );
            }
        } else if !Self::integer_memory_source(ea.kind) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid source effective address for {mnemonic}{}",
                    size.suffix()
                ),
                ea_operand.span(),
            );
        }

        let (left_bits, right_bits) = match Self::integer_register_pair_fields(pair_operand) {
            Ok(bits) => bits,
            Err(err) => return err,
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x0E00 | (M68KFamilyHandler::size_bits(size) << 6) | (ea.bits & 0x3F),
        );
        let second_word = if mnemonic == "MOVZ2" {
            (left_bits << 12) | (right_bits << 6) | 0x0012
        } else {
            (left_bits << 12) | ((pair_to_ea as u16) << 11) | (right_bits << 6) | 0x0021
        };
        Self::emit_word(&mut bytes, second_word);
        bytes.extend_from_slice(&ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_touch(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("TOUCH does not accept a size suffix on m68080");
        }
        let [operand] = operands else {
            return EncodeResult::error("TOUCH expects one operand");
        };

        let ea = match self
            .base
            .family()
            .encode_effective_address(operand, None, ctx)
        {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !matches!(
            operand,
            Operand::AddressIndirect { .. }
                | Operand::AddressIndexed { .. }
                | Operand::FullExtension { .. }
        ) {
            return EncodeResult::error_with_span(
                "TOUCH expects address-indirect or indexed memory syntax",
                operand.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0xF600 | (ea.bits & 0x3F));
        bytes.extend_from_slice(&ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_padd(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("PADD", ctx) {
            return err;
        }
        let opcode = match size {
            Some(crate::families::m68k::OperationSize::Byte) => 0x10,
            Some(crate::families::m68k::OperationSize::Word) => 0x11,
            Some(crate::families::m68k::OperationSize::Long) => {
                return EncodeResult::error("PADD requires .B or .W size on m68080");
            }
            None => return EncodeResult::error("PADD requires .B or .W size on m68080"),
        };
        self.encode_ammx_vea_b_d("PADD", operands, ctx, opcode)
    }

    fn encode_ammx_fixed_size_alias(
        &self,
        mnemonic: &str,
        parsed_size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        implied_size: crate::families::m68k::OperationSize,
        opcode: u16,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo(mnemonic, ctx) {
            return err;
        }
        if parsed_size.is_some() {
            return EncodeResult::error(format!(
                "{mnemonic} does not accept a size suffix on m68080"
            ));
        }
        debug_assert!(matches!(
            implied_size,
            crate::families::m68k::OperationSize::Byte | crate::families::m68k::OperationSize::Word
        ));
        self.encode_ammx_vea_b_d(mnemonic, operands, ctx, opcode)
    }

    fn encode_ammx_dotless_fixed(
        &self,
        mnemonic: &str,
        parsed_size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        opcode: u16,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo(mnemonic, ctx) {
            return err;
        }
        if parsed_size.is_some() {
            return EncodeResult::error(format!(
                "{mnemonic} does not accept a size suffix on m68080"
            ));
        }
        self.encode_ammx_vea_b_d(mnemonic, operands, ctx, opcode)
    }

    fn encode_ammx_psub(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("PSUB", ctx) {
            return err;
        }
        let opcode = match size {
            Some(crate::families::m68k::OperationSize::Byte) => 0x12,
            Some(crate::families::m68k::OperationSize::Word) => 0x13,
            Some(crate::families::m68k::OperationSize::Long) => {
                return EncodeResult::error("PSUB requires .B or .W size on m68080");
            }
            None => return EncodeResult::error("PSUB requires .B or .W size on m68080"),
        };
        self.encode_ammx_vea_b_d("PSUB", operands, ctx, opcode)
    }

    fn encode_ammx_packuswb(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_ammx_b_d_vea_fixed("PACKUSWB", size, operands, ctx, 0x06)
    }

    fn encode_ammx_pack3216(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("PACK3216", ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error("PACK3216 does not accept a size suffix on m68080");
        }
        let [src, dst, vea] = operands else {
            return EncodeResult::error("AMMX PACK3216 expects three operands: Dn/En,Dn/En,<vea>");
        };

        let src_code = match self.ammx_data_register_code(src) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let dst_code = match self.ammx_data_register_code(dst) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let vea = match self.encode_ammx_vea(vea, "PACK3216 destination", ctx) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (src_high, src_low) = Self::ammx_register_fields(src_code);
        let (dst_high, dst_low) = Self::ammx_register_fields(dst_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            src_high,
            dst_high,
            vea.ea_bits,
            (src_low << 12) | (dst_low << 8) | 0x07,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_unpack1632(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("UNPACK1632", ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error("UNPACK1632 does not accept a size suffix on m68080");
        }
        let [vea_operand, dst_pair] = operands else {
            return EncodeResult::error(
                "AMMX UNPACK1632 expects two operands: <vea>,.pair(Dn/En,Dn/En)",
            );
        };
        let vea = match self.encode_ammx_vea(vea_operand, "UNPACK1632 source", ctx) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (dst_code, dst_pair_code) = match self.ammx_even_pair_code(dst_pair, "UNPACK1632") {
            Ok(value) => value,
            Err(err) => return err,
        };
        let _ = dst_pair_code;
        let (dst_high, dst_low) = Self::ammx_register_fields(dst_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            0,
            dst_high,
            vea.ea_bits,
            (dst_low << 8) | 0x1E,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_vperm(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("VPERM", ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error("VPERM does not accept a size suffix on m68080");
        }
        let [immediate, src_a, src_b, dst] = operands else {
            return EncodeResult::error("AMMX VPERM expects four operands: #imm,Dn/En,Dn/En,Dn/En");
        };

        let Operand::Immediate { expr, .. } = immediate else {
            return EncodeResult::error_with_span(
                "AMMX VPERM first operand must be an immediate selector",
                immediate.span(),
            );
        };
        let immediate_value = match ctx.eval_expr(expr) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, immediate.span()),
        };
        if !(0..=0xFFFF_FFFF).contains(&immediate_value) {
            return EncodeResult::error_with_span(
                "AMMX VPERM selector must be in 32-bit unsigned range",
                immediate.span(),
            );
        }

        let src_a_code = match self.ammx_data_register_code(src_a) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let src_b_code = match self.ammx_data_register_code(src_b) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let dst_code = match self.ammx_data_register_code(dst) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (src_a_high, src_a_low) = Self::ammx_register_fields(src_a_code);
        let (src_b_high, src_b_low) = Self::ammx_register_fields(src_b_code);
        let (dst_high, dst_low) = Self::ammx_register_fields(dst_code);

        let mut bytes = Vec::new();
        let mut selector = Vec::new();
        Self::emit_long(&mut selector, immediate_value as u32);
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            src_a_high,
            src_b_high,
            dst_high,
            0x3F,
            (src_b_low << 12) | (dst_low << 8) | src_a_low,
            &selector,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_vea_b_d(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        opcode: u16,
    ) -> EncodeResult<Vec<u8>> {
        let [vea_operand, src_operand, dst_operand] = operands else {
            return EncodeResult::error(format!(
                "AMMX {mnemonic} expects three operands: <vea>,Dn/En,Dn/En"
            ));
        };

        let vea = match self.encode_ammx_vea(vea_operand, &format!("{mnemonic} first operand"), ctx)
        {
            Ok(value) => value,
            Err(err) => return err,
        };
        let src_code = match self.ammx_data_register_code(src_operand) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let dst_code = match self.ammx_data_register_code(dst_operand) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (src_high, src_low) = Self::ammx_register_fields(src_code);
        let (dst_high, dst_low) = Self::ammx_register_fields(dst_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            src_high,
            dst_high,
            vea.ea_bits,
            (src_low << 12) | (dst_low << 8) | opcode,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_vea_b_d_fixed(
        &self,
        mnemonic: &str,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        opcode: u16,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo(mnemonic, ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error(format!(
                "{mnemonic} does not accept a size suffix on m68080"
            ));
        }
        self.encode_ammx_vea_b_d(mnemonic, operands, ctx, opcode)
    }

    fn encode_ammx_vea_b_pair_fixed(
        &self,
        mnemonic: &str,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        opcode: u16,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo(mnemonic, ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error(format!(
                "{mnemonic} does not accept a size suffix on m68080"
            ));
        }
        let [vea_operand, src_operand, dst_pair] = operands else {
            return EncodeResult::error(format!(
                "AMMX {mnemonic} expects three operands: <vea>,Dn/En,.pair(Dn/En,Dn/En)"
            ));
        };
        let vea = match self.encode_ammx_vea(vea_operand, &format!("{mnemonic} first operand"), ctx)
        {
            Ok(value) => value,
            Err(err) => return err,
        };
        let src_code = match self.ammx_data_register_code(src_operand) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (dst_code, _dst_pair_code) = match self.ammx_even_pair_code(dst_pair, mnemonic) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (src_high, src_low) = Self::ammx_register_fields(src_code);
        let (dst_high, dst_low) = Self::ammx_register_fields(dst_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            src_high,
            dst_high,
            vea.ea_bits,
            (src_low << 12) | (dst_low << 8) | opcode,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_c2p(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("C2P", ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error("C2P does not accept a size suffix on m68080");
        }
        let [vea_operand, dst_operand] = operands else {
            return EncodeResult::error("AMMX C2P expects two operands: <vea>,Dn/En");
        };
        let vea = match self.encode_ammx_vea(vea_operand, "C2P source", ctx) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let dst_code = match self.ammx_data_register_code(dst_operand) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (dst_high, dst_low) = Self::ammx_register_fields(dst_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            0,
            dst_high,
            vea.ea_bits,
            (dst_low << 10) | 0x0A8,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_minterm(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("MINTERM", ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error("MINTERM does not accept a size suffix on m68080");
        }
        let [src_group, dst_operand] = operands else {
            return EncodeResult::error("AMMX MINTERM expects two operands: Dn-Dn,Dn/En");
        };
        let (group_a_bit, group_aa_bits) = match self.ammx_group_fields(src_group, "MINTERM") {
            Ok(value) => value,
            Err(err) => return err,
        };
        let dst_code = match self.ammx_data_register_code(dst_operand) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (dst_high, dst_low) = Self::ammx_register_fields(dst_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            group_a_bit,
            0,
            dst_high,
            group_aa_bits << 2,
            (dst_low << 10) | 0x0AA,
            &[],
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_transpose(
        &self,
        mnemonic: &str,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        opcode: u16,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo(mnemonic, ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error(format!(
                "{mnemonic} does not accept a size suffix on m68080"
            ));
        }
        let [src_group, dst_pair] = operands else {
            return EncodeResult::error(format!(
                "AMMX {mnemonic} expects two operands: Dn-Dn,.pair(Dn/En,Dn/En)"
            ));
        };
        let (group_a_bit, group_aa_bits) = match self.ammx_group_fields(src_group, mnemonic) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (dst_code, _dst_pair_code) = match self.ammx_even_pair_code(dst_pair, mnemonic) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (dst_high, dst_low) = Self::ammx_register_fields(dst_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            group_a_bit,
            0,
            dst_high,
            group_aa_bits << 2,
            (dst_low << 10) | opcode,
            &[],
        );
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_storem3(
        &self,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("STOREM3", ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error("STOREM3 does not accept a size suffix on m68080");
        }

        let [src_operand, mode_operand, vea_operand] = operands else {
            return EncodeResult::error("AMMX STOREM3 expects three operands: Dn/En,#mode,<vea>");
        };
        let src_code = match self.ammx_data_register_code(src_operand) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let mode_value = match Self::storem3_mode(mode_operand, ctx) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let vea = match self.encode_ammx_vea(vea_operand, "STOREM3 destination", ctx) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (src_high, src_low) = Self::ammx_register_fields(src_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            src_high,
            0,
            vea.ea_bits,
            (src_low << 12) | (u16::from(mode_value) << 8) | 0x25,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_tex(
        &self,
        mnemonic: &str,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo("TEX", ctx) {
            return err;
        }

        let [src_operand, dst_operand] = operands else {
            return EncodeResult::error(
                "TEX expects two operands: texture source and destination Dn",
            );
        };
        let variant = match Self::parse_tex_variant(mnemonic, size) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let src = match self.parse_tex_source_operand(src_operand) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let dst = match Self::plain_data_register_number(
            dst_operand,
            "TEX destination must be D0-D7 data register",
        ) {
            Ok(value) => value,
            Err(err) => return err,
        };

        let word2 = match variant {
            TexVariant::Tex8x512 => {
                if src.kind != TexSourceKind::Nested {
                    return EncodeResult::error_with_span(
                        "TEX8.512 source must use (An,(Av,Au)) syntax",
                        src.span,
                    );
                }
                Self::tex_standard_word2(src.v, 0, 0, 0, 0b110, 0, 0)
            }
            TexVariant::Tex16x256 => {
                if src.kind != TexSourceKind::Nested {
                    return EncodeResult::error_with_span(
                        "TEX16.256 source must use (An,(Av,Au)) syntax",
                        src.span,
                    );
                }
                Self::tex_standard_word2(src.v, 0, 1, 0, 0b101, 0, 1)
            }
            TexVariant::Tex24x64 => {
                if src.kind != TexSourceKind::ExternalScale || src.modifier != Some(0) {
                    return EncodeResult::error_with_span(
                        "TEX24.64 source must use (An,(Av,Au))*D0 syntax",
                        src.span,
                    );
                }
                Self::tex_standard_word2(src.v, 1, 1, 1, 0b000, 1, 0)
            }
            TexVariant::TexByte => {
                if src.kind != TexSourceKind::ScaledInside
                    && src.kind != TexSourceKind::ExternalScale
                {
                    return EncodeResult::error_with_span(
                        "TEX.B source must use (An,Av*Dm,Au) syntax",
                        src.span,
                    );
                }
                let Some(modifier) = src.modifier else {
                    return EncodeResult::error_with_span(
                        "TEX.B source must use D0-D7 as the texture step register",
                        src.span,
                    );
                };
                0x8000 | (u16::from(src.v) << 12) | (u16::from(modifier) << 4)
            }
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0xFE30 | u16::from(src.base));
        Self::emit_word(
            &mut bytes,
            (u16::from(src.u) << 12) | (u16::from(dst) << 8) | 0x003E,
        );
        Self::emit_word(&mut bytes, word2);
        EncodeResult::ok(bytes)
    }

    fn encode_ammx_b_d_vea_fixed(
        &self,
        mnemonic: &str,
        size: Option<crate::families::m68k::OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        opcode: u16,
    ) -> EncodeResult<Vec<u8>> {
        if let Err(err) = self.require_apollo(mnemonic, ctx) {
            return err;
        }
        if size.is_some() {
            return EncodeResult::error(format!(
                "{mnemonic} does not accept a size suffix on m68080"
            ));
        }
        let [src_operand, dst_operand, vea_operand] = operands else {
            return EncodeResult::error(format!(
                "AMMX {mnemonic} expects three operands: Dn/En,Dn/En,<vea>"
            ));
        };
        let src_code = match self.ammx_data_register_code(src_operand) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let dst_code = match self.ammx_data_register_code(dst_operand) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let vea = match self.encode_ammx_vea(vea_operand, &format!("{mnemonic} destination"), ctx) {
            Ok(value) => value,
            Err(err) => return err,
        };
        let (src_high, src_low) = Self::ammx_register_fields(src_code);
        let (dst_high, dst_low) = Self::ammx_register_fields(dst_code);

        let mut bytes = Vec::new();
        Self::emit_apollo_two_word_instruction(
            &mut bytes,
            Self::APOLLO_AMMX_SELECTOR,
            vea.a_bit,
            src_high,
            dst_high,
            vea.ea_bits,
            (src_low << 12) | (dst_low << 8) | opcode,
            &vea.extension,
        );
        EncodeResult::ok(bytes)
    }

    fn require_apollo(
        &self,
        mnemonic: &str,
        ctx: &dyn AssemblerContext,
    ) -> Result<(), EncodeResult<Vec<u8>>> {
        if self.apollo_mode_enabled(ctx) {
            Ok(())
        } else {
            Err(EncodeResult::error(format!(
                "{mnemonic} is Apollo-gated on m68080; enable .apollo on"
            )))
        }
    }

    fn ammx_data_register_code(&self, operand: &Operand) -> Result<u8, EncodeResult<Vec<u8>>> {
        let Operand::DataRegister { register, .. } = operand else {
            return Err(EncodeResult::error_with_span(
                "AMMX operands must be D0-D7 or E0-E23 data registers",
                operand.span(),
            ));
        };

        Self::ammx_data_register_name_code(register).ok_or_else(|| {
            EncodeResult::error_with_span(
                if register
                    .strip_prefix('E')
                    .or_else(|| register.strip_prefix('e'))
                    .and_then(|suffix| suffix.parse::<u8>().ok())
                    .is_some()
                {
                    "AMMX register must be in range E0-E23"
                } else {
                    "AMMX operands must be D0-D7 or E0-E23 data registers"
                },
                operand.span(),
            )
        })
    }

    fn ammx_data_register_name_code(register: &str) -> Option<u8> {
        if let Some(value) = register
            .strip_prefix('D')
            .or_else(|| register.strip_prefix('d'))
            .and_then(|suffix| suffix.parse::<u8>().ok())
        {
            if value <= 7 {
                return Some(value);
            }
        }

        let value = register
            .strip_prefix('E')
            .or_else(|| register.strip_prefix('e'))
            .and_then(|suffix| suffix.parse::<u8>().ok())?;

        if value > 23 {
            return None;
        }

        Some(8 + value)
    }

    fn ammx_even_pair_code(
        &self,
        operand: &Operand,
        mnemonic: &str,
    ) -> Result<(u8, u8), EncodeResult<Vec<u8>>> {
        let Operand::RegisterPair { left, right, span } = operand else {
            return Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} expects .pair(Dn/En,Dn/En) destination syntax"),
                operand.span(),
            ));
        };
        let Some(left_code) = Self::ammx_data_register_name_code(left) else {
            return Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} destination pair must use D0-D7 or E0-E23 registers"),
                *span,
            ));
        };
        let Some(right_code) = Self::ammx_data_register_name_code(right) else {
            return Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} destination pair must use D0-D7 or E0-E23 registers"),
                *span,
            ));
        };
        if left_code & 1 != 0 {
            return Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} destination pair must start at an even register"),
                *span,
            ));
        }
        if right_code != left_code + 1 {
            return Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} destination pair must be consecutive"),
                *span,
            ));
        }
        Ok((left_code, right_code))
    }

    fn ammx_group_fields(
        &self,
        operand: &Operand,
        mnemonic: &str,
    ) -> Result<(u16, u16), EncodeResult<Vec<u8>>> {
        let Operand::RegisterGroup { start, end, span } = operand else {
            return Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} expects Dn-Dn or En-En register-group syntax"),
                operand.span(),
            ));
        };
        let Some(start_code) = Self::ammx_data_register_name_code(start) else {
            return Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} source group must use D0-D7 or E0-E23 registers"),
                *span,
            ));
        };
        let Some(end_code) = Self::ammx_data_register_name_code(end) else {
            return Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} source group must use D0-D7 or E0-E23 registers"),
                *span,
            ));
        };
        if start_code & 0x3 != 0 {
            return Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} source group must start at a multiple-of-four register"),
                *span,
            ));
        }
        if end_code != start_code + 3 {
            return Err(EncodeResult::error_with_span(
                format!("AMMX {mnemonic} source group must cover four consecutive registers"),
                *span,
            ));
        }
        Ok((
            u16::from((start_code >> 4) & 0x1),
            u16::from((start_code >> 2) & 0x3),
        ))
    }

    fn storem3_mode(
        operand: &Operand,
        ctx: &dyn AssemblerContext,
    ) -> Result<u8, EncodeResult<Vec<u8>>> {
        match operand {
            Operand::Immediate { expr, .. } => {
                let value = match M68KFamilyHandler::eval_expr(expr, ctx) {
                    Ok(value) => value,
                    Err(err) => return Err(EncodeResult::error_with_span(err, operand.span())),
                };
                if !(0..=3).contains(&value) {
                    return Err(EncodeResult::error_with_span(
                        "STOREM3 mode must be in range 0-3",
                        operand.span(),
                    ));
                }
                Ok(value as u8)
            }
            Operand::DataRegister { register, .. } => {
                let upper = register.to_ascii_uppercase();
                let Some(value) = M68KFamilyHandler::data_register_number(&upper) else {
                    return Err(EncodeResult::error_with_span(
                        "STOREM3 mode must be an immediate 0-3 or D0-D3",
                        operand.span(),
                    ));
                };
                if value > 3 {
                    return Err(EncodeResult::error_with_span(
                        "STOREM3 mode must be in range 0-3",
                        operand.span(),
                    ));
                }
                Ok(value)
            }
            _ => Err(EncodeResult::error_with_span(
                "STOREM3 mode must be an immediate 0-3 or D0-D3",
                operand.span(),
            )),
        }
    }

    fn parse_tex_variant(
        mnemonic: &str,
        size: Option<crate::families::m68k::OperationSize>,
    ) -> Result<TexVariant, EncodeResult<Vec<u8>>> {
        let upper = mnemonic.to_ascii_uppercase();
        match upper.as_str() {
            "TEX8.512" => Ok(TexVariant::Tex8x512),
            "TEX16.256" => Ok(TexVariant::Tex16x256),
            "TEX24.64" => Ok(TexVariant::Tex24x64),
            _ if upper == "TEX.B"
                || (upper == "TEX" && size == Some(crate::families::m68k::OperationSize::Byte)) =>
            {
                Ok(TexVariant::TexByte)
            }
            _ => Err(EncodeResult::error(format!(
                "unsupported TEX form `{mnemonic}` on m68080"
            ))),
        }
    }

    fn parse_tex_source_operand(
        &self,
        operand: &Operand,
    ) -> Result<ParsedTexSource, EncodeResult<Vec<u8>>> {
        let Operand::TextureOperand { expr, span } = operand else {
            return Err(EncodeResult::error_with_span(
                "TEX source must use a texture addressing form",
                operand.span(),
            ));
        };

        match expr {
            Expr::Indirect(inner, _) => self.parse_tex_nested_inner(inner, *span),
            Expr::Binary {
                op: BinaryOp::Multiply,
                left,
                right,
                ..
            } => {
                let mut parsed = match left.as_ref() {
                    Expr::Indirect(inner, _) => {
                        let (base, v, u) = self.parse_tex_external_tuple(inner, *span)?;
                        ParsedTexSource {
                            base,
                            v,
                            u,
                            modifier: None,
                            kind: TexSourceKind::Nested,
                            span: *span,
                        }
                    }
                    _ => {
                        return Err(EncodeResult::error_with_span(
                            "TEX source must use (An,(Av,Au))*D0 syntax when scaled externally",
                            *span,
                        ))
                    }
                };
                parsed.modifier = Some(Self::plain_data_register_number_expr(
                    right,
                    "TEX external scale register must be D0-D7",
                    *span,
                )?);
                parsed.kind = TexSourceKind::ExternalScale;
                Ok(parsed)
            }
            _ => Err(EncodeResult::error_with_span(
                "TEX source must use (An,(Av,Au)), (An,(Av,Au))*D0, or (An,Av*Dm,Au) syntax",
                *span,
            )),
        }
    }

    fn parse_tex_nested_inner(
        &self,
        inner: &Expr,
        span: Span,
    ) -> Result<ParsedTexSource, EncodeResult<Vec<u8>>> {
        let Expr::Tuple(elements, _) = inner else {
            return Err(EncodeResult::error_with_span(
                "TEX source must use a parenthesized texture tuple",
                span,
            ));
        };

        match elements.as_slice() {
            [base, nested] => {
                let base_reg = Self::plain_address_register_number_expr(
                    base,
                    "TEX base register must be A0-A7 or SP",
                    span,
                )?;
                let Expr::Indirect(coords, _) = nested else {
                    return Err(EncodeResult::error_with_span(
                        "TEX source must use nested (Av,Au) coordinate syntax",
                        span,
                    ));
                };
                let Expr::Tuple(coord_items, _) = coords.as_ref() else {
                    return Err(EncodeResult::error_with_span(
                        "TEX source must use nested (Av,Au) coordinate syntax",
                        span,
                    ));
                };
                let [v, u] = coord_items.as_slice() else {
                    return Err(EncodeResult::error_with_span(
                        "TEX coordinate tuple must contain Av and Au",
                        span,
                    ));
                };
                Ok(ParsedTexSource {
                    base: base_reg,
                    v: Self::plain_address_register_number_expr(
                        v,
                        "TEX V coordinate must be A0-A7 or SP",
                        span,
                    )?,
                    u: Self::plain_address_register_number_expr(
                        u,
                        "TEX U coordinate must be A0-A7 or SP",
                        span,
                    )?,
                    modifier: None,
                    kind: TexSourceKind::Nested,
                    span,
                })
            }
            [base, scaled_v, u] => {
                let base_reg = Self::plain_address_register_number_expr(
                    base,
                    "TEX base register must be A0-A7 or SP",
                    span,
                )?;
                let (left, right) = match scaled_v {
                    Expr::Binary {
                        op: BinaryOp::Multiply,
                        left,
                        right,
                        ..
                    }
                    | Expr::Index {
                        base: left,
                        index: right,
                        ..
                    } => (left, right),
                    _ => {
                        if let (Ok(v), Ok(u_reg)) = (
                            Self::plain_address_register_number_expr(
                                scaled_v,
                                "TEX V coordinate must be A0-A7 or SP",
                                span,
                            ),
                            Self::plain_address_register_number_expr(
                                u,
                                "TEX U coordinate must be A0-A7 or SP",
                                span,
                            ),
                        ) {
                            return Ok(ParsedTexSource {
                                base: base_reg,
                                v,
                                u: u_reg,
                                modifier: None,
                                kind: TexSourceKind::Flat,
                                span,
                            });
                        }
                        return Err(EncodeResult::error_with_span(
                            "TEX source has an unsupported tuple shape",
                            span,
                        ));
                    }
                };
                Ok(ParsedTexSource {
                    base: base_reg,
                    v: Self::plain_address_register_number_expr(
                        left,
                        "TEX V coordinate must be A0-A7 or SP",
                        span,
                    )?,
                    u: Self::plain_address_register_number_expr(
                        u,
                        "TEX U coordinate must be A0-A7 or SP",
                        span,
                    )?,
                    modifier: Some(Self::plain_data_register_number_expr(
                        right,
                        "TEX.B texture step register must be D0-D7",
                        span,
                    )?),
                    kind: TexSourceKind::ScaledInside,
                    span,
                })
            }
            [base, v, dm, u] => Ok(ParsedTexSource {
                base: Self::plain_address_register_number_expr(
                    base,
                    "TEX base register must be A0-A7 or SP",
                    span,
                )?,
                v: Self::plain_address_register_number_expr(
                    v,
                    "TEX V coordinate must be A0-A7 or SP",
                    span,
                )?,
                u: Self::plain_address_register_number_expr(
                    u,
                    "TEX U coordinate must be A0-A7 or SP",
                    span,
                )?,
                modifier: Some(Self::plain_data_register_number_expr(
                    dm,
                    "TEX.B texture step register must be D0-D7",
                    span,
                )?),
                kind: TexSourceKind::ScaledInside,
                span,
            }),
            _ => Err(EncodeResult::error_with_span(
                "TEX source has an unsupported tuple shape",
                span,
            )),
        }
    }

    fn parse_tex_external_tuple(
        &self,
        inner: &Expr,
        span: Span,
    ) -> Result<(u8, u8, u8), EncodeResult<Vec<u8>>> {
        let Expr::Tuple(elements, _) = inner else {
            return Err(EncodeResult::error_with_span(
                "TEX source must use a parenthesized texture tuple",
                span,
            ));
        };

        match elements.as_slice() {
            [base, nested] => {
                let base_reg = Self::plain_address_register_number_expr(
                    base,
                    "TEX base register must be A0-A7 or SP",
                    span,
                )?;
                let Expr::Indirect(coords, _) = nested else {
                    return Err(EncodeResult::error_with_span(
                        "TEX source must use nested (Av,Au) coordinate syntax",
                        span,
                    ));
                };
                let Expr::Tuple(coord_items, _) = coords.as_ref() else {
                    return Err(EncodeResult::error_with_span(
                        "TEX source must use nested (Av,Au) coordinate syntax",
                        span,
                    ));
                };
                let [v, u] = coord_items.as_slice() else {
                    return Err(EncodeResult::error_with_span(
                        "TEX coordinate tuple must contain Av and Au",
                        span,
                    ));
                };
                Ok((
                    base_reg,
                    Self::plain_address_register_number_expr(
                        v,
                        "TEX V coordinate must be A0-A7 or SP",
                        span,
                    )?,
                    Self::plain_address_register_number_expr(
                        u,
                        "TEX U coordinate must be A0-A7 or SP",
                        span,
                    )?,
                ))
            }
            [base, v, u] => Ok((
                Self::plain_address_register_number_expr(
                    base,
                    "TEX base register must be A0-A7 or SP",
                    span,
                )?,
                Self::plain_address_register_number_expr(
                    v,
                    "TEX V coordinate must be A0-A7 or SP",
                    span,
                )?,
                Self::plain_address_register_number_expr(
                    u,
                    "TEX U coordinate must be A0-A7 or SP",
                    span,
                )?,
            )),
            _ => Err(EncodeResult::error_with_span(
                "TEX source has an unsupported tuple shape",
                span,
            )),
        }
    }

    fn plain_data_register_number(
        operand: &Operand,
        message: &str,
    ) -> Result<u8, EncodeResult<Vec<u8>>> {
        let Operand::DataRegister { register, .. } = operand else {
            return Err(EncodeResult::error_with_span(message, operand.span()));
        };
        let upper = register.to_ascii_uppercase();
        M68KFamilyHandler::data_register_number(&upper)
            .ok_or_else(|| EncodeResult::error_with_span(message, operand.span()))
    }

    fn plain_data_register_number_expr(
        expr: &Expr,
        message: &str,
        span: Span,
    ) -> Result<u8, EncodeResult<Vec<u8>>> {
        match expr {
            Expr::Register(name, _) | Expr::Identifier(name, _) => {
                let upper = name.to_ascii_uppercase();
                M68KFamilyHandler::data_register_number(&upper)
                    .ok_or_else(|| EncodeResult::error_with_span(message, span))
            }
            _ => Err(EncodeResult::error_with_span(message, span)),
        }
    }

    fn plain_address_register_number_expr(
        expr: &Expr,
        message: &str,
        span: Span,
    ) -> Result<u8, EncodeResult<Vec<u8>>> {
        match expr {
            Expr::Register(name, _) | Expr::Identifier(name, _) => {
                let upper = name.to_ascii_uppercase();
                M68KFamilyHandler::address_register_number(&upper)
                    .ok_or_else(|| EncodeResult::error_with_span(message, span))
            }
            _ => Err(EncodeResult::error_with_span(message, span)),
        }
    }

    fn tex_standard_word2(
        v: u8,
        size_10: u16,
        size_9: u16,
        size_7: u16,
        texture: u16,
        size_1: u16,
        size_0: u16,
    ) -> u16 {
        0x8800
            | (u16::from(v) << 12)
            | ((size_10 & 0x1) << 10)
            | ((size_9 & 0x1) << 9)
            | ((size_7 & 0x1) << 7)
            | ((texture & 0x7) << 4)
            | ((size_1 & 0x1) << 1)
            | (size_0 & 0x1)
    }

    fn movex_size_bits(
        size: Option<crate::families::m68k::OperationSize>,
    ) -> Result<(crate::families::m68k::OperationSize, u16), EncodeResult<Vec<u8>>> {
        let Some(size) = size else {
            return Err(EncodeResult::error(
                "MOVEX requires an explicit .W or .L size suffix",
            ));
        };
        let bits = match size {
            crate::families::m68k::OperationSize::Word => 0b01,
            crate::families::m68k::OperationSize::Long => 0b10,
            crate::families::m68k::OperationSize::Byte => {
                return Err(EncodeResult::error(
                    "MOVEX requires .W or .L size on m68080",
                ));
            }
        };
        Ok((size, bits))
    }

    fn move2_size_bits(
        mnemonic: &str,
        size: Option<crate::families::m68k::OperationSize>,
        allow_long: bool,
    ) -> Result<(crate::families::m68k::OperationSize, u16), EncodeResult<Vec<u8>>> {
        let Some(size) = size else {
            return Err(EncodeResult::error(format!(
                "{mnemonic} requires an explicit {} size suffix",
                if allow_long {
                    ".B, .W, or .L"
                } else {
                    ".B or .W"
                }
            )));
        };
        let bits = match size {
            crate::families::m68k::OperationSize::Byte => 0b00,
            crate::families::m68k::OperationSize::Word => 0b01,
            crate::families::m68k::OperationSize::Long if allow_long => 0b10,
            crate::families::m68k::OperationSize::Long => {
                return Err(EncodeResult::error(format!(
                    "{mnemonic} requires .B or .W size on m68080"
                )));
            }
        };
        Ok((size, bits))
    }

    fn integer_register_field(operand: &Operand) -> Result<u16, EncodeResult<Vec<u8>>> {
        match operand {
            Operand::DataRegister { register, .. } => {
                let Some(bits) = M68KFamilyHandler::data_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "68080 integer register must be D0-D7 or A0-A7/SP",
                        operand.span(),
                    ));
                };
                Ok(bits as u16)
            }
            Operand::AddressRegister { register, .. } => {
                let Some(bits) = M68KFamilyHandler::address_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "68080 integer register must be D0-D7 or A0-A7/SP",
                        operand.span(),
                    ));
                };
                Ok(8 + bits as u16)
            }
            _ => Err(EncodeResult::error_with_span(
                "68080 integer register must be D0-D7 or A0-A7/SP",
                operand.span(),
            )),
        }
    }

    fn integer_register_pair_fields(
        operand: &Operand,
    ) -> Result<(u16, u16), EncodeResult<Vec<u8>>> {
        let Operand::RegisterPair { left, right, span } = operand else {
            return Err(EncodeResult::error_with_span(
                "68080 register-pair operand must use .pair(Rn,Rn) syntax",
                operand.span(),
            ));
        };

        let left_bits = Self::integer_register_name_field(left).ok_or_else(|| {
            EncodeResult::error_with_span(
                "68080 register pairs must use D0-D7 or A0-A7/SP registers",
                *span,
            )
        })?;
        let right_bits = Self::integer_register_name_field(right).ok_or_else(|| {
            EncodeResult::error_with_span(
                "68080 register pairs must use D0-D7 or A0-A7/SP registers",
                *span,
            )
        })?;
        Ok((left_bits, right_bits))
    }

    fn integer_register_name_field(register: &str) -> Option<u16> {
        M68KFamilyHandler::data_register_number(register)
            .map(u16::from)
            .or_else(|| {
                M68KFamilyHandler::address_register_number(register)
                    .map(|value| 8 + u16::from(value))
            })
    }

    fn integer_memory_source(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::PcDisplacement
                | EffectiveAddressKind::PcIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    fn integer_memory_destination(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    fn encode_word_extended_immediate(value: i64) -> Option<u16> {
        ((i16::MIN as i64)..=(u16::MAX as i64))
            .contains(&value)
            .then_some(value as u16)
    }

    fn ammx_register_fields(code: u8) -> (u16, u16) {
        (u16::from((code >> 4) & 0x1), u16::from(code & 0x0F))
    }

    fn encode_ammx_vea(
        &self,
        operand: &Operand,
        context: &str,
        ctx: &dyn AssemblerContext,
    ) -> Result<EncodedAmmxVea, EncodeResult<Vec<u8>>> {
        match operand {
            Operand::DataRegister { .. } => {
                let code = self.ammx_data_register_code(operand)?;
                let (a_bit, ea_bits) = match code {
                    0..=7 => (0, u16::from(code)),
                    8..=15 => (0, 0x08 | u16::from(code - 8)),
                    16..=23 => (1, u16::from(code - 16)),
                    24..=31 => (1, 0x08 | u16::from(code - 24)),
                    _ => unreachable!("AMMX data register codes are 5-bit"),
                };
                Ok(EncodedAmmxVea {
                    a_bit,
                    ea_bits,
                    extension: Vec::new(),
                })
            }
            Operand::AddressIndirect { register, .. } => {
                let Some((a_bit, reg)) = Self::ammx_address_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "AMMX memory operands require A0-A7, B0-B7, or SP",
                        operand.span(),
                    ));
                };
                Ok(EncodedAmmxVea {
                    a_bit,
                    ea_bits: (0b010_u16 << 3) | u16::from(reg),
                    extension: Vec::new(),
                })
            }
            Operand::AddressPostincrement { register, .. } => {
                let Some((a_bit, reg)) = Self::ammx_address_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "AMMX memory operands require A0-A7, B0-B7, or SP",
                        operand.span(),
                    ));
                };
                Ok(EncodedAmmxVea {
                    a_bit,
                    ea_bits: (0b011_u16 << 3) | u16::from(reg),
                    extension: Vec::new(),
                })
            }
            Operand::AddressPredecrement { register, .. } => {
                let Some((a_bit, reg)) = Self::ammx_address_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "AMMX memory operands require A0-A7, B0-B7, or SP",
                        operand.span(),
                    ));
                };
                Ok(EncodedAmmxVea {
                    a_bit,
                    ea_bits: (0b100_u16 << 3) | u16::from(reg),
                    extension: Vec::new(),
                })
            }
            Operand::AddressDisplacement { base, .. } | Operand::AddressIndexed { base, .. } => {
                let Some((a_bit, _)) = Self::ammx_address_register_number(base) else {
                    return Err(EncodeResult::error_with_span(
                        "AMMX memory operands require A0-A7, B0-B7, or SP",
                        operand.span(),
                    ));
                };

                let canonical_operand = if a_bit == 0 {
                    operand.clone()
                } else {
                    Self::ammx_rewrite_banked_vea_operand(operand)?
                };

                self.encode_ammx_m68k_vea_operand(&canonical_operand, a_bit, context, ctx)
            }
            Operand::PcDisplacement { .. }
            | Operand::PcIndexed { .. }
            | Operand::Absolute { .. } => {
                self.encode_ammx_m68k_vea_operand(operand, 0, context, ctx)
            }
            _ => Err(EncodeResult::error_with_span(
                format!(
                    "AMMX {context} must be a vector effective address (D0-D7, E0-E23, (An)/(Bn), (An)/(Bn)+, -(An)/(Bn), d16(An)/(Bn), d8(An)/(Bn,Xn), d16(PC), d8(PC,Xn), Abs.W, or Abs.L)"
                ),
                operand.span(),
            )),
        }
    }

    fn encode_ammx_m68k_vea_operand(
        &self,
        operand: &Operand,
        a_bit: u16,
        context: &str,
        ctx: &dyn AssemblerContext,
    ) -> Result<EncodedAmmxVea, EncodeResult<Vec<u8>>> {
        let encoded = self
            .base
            .family()
            .encode_effective_address(operand, None, ctx)?;
        if !matches!(
            encoded.kind,
            EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::PcDisplacement
                | EffectiveAddressKind::PcIndexed
                | EffectiveAddressKind::Absolute
        ) {
            return Err(EncodeResult::error_with_span(
                format!(
                    "AMMX {context} must be a vector effective address (D0-D7, E0-E23, (An)/(Bn), (An)/(Bn)+, -(An)/(Bn), d16(An)/(Bn), d8(An)/(Bn,Xn), d16(PC), d8(PC,Xn), Abs.W, or Abs.L)"
                ),
                operand.span(),
            ));
        }

        Ok(EncodedAmmxVea {
            a_bit,
            ea_bits: encoded.bits,
            extension: encoded.extension,
        })
    }

    fn ammx_rewrite_banked_vea_operand(
        operand: &Operand,
    ) -> Result<Operand, EncodeResult<Vec<u8>>> {
        match operand {
            Operand::AddressDisplacement {
                displacement,
                base,
                span,
            } => {
                let Some(reg) = Self::ammx_banked_address_register_number(base) else {
                    return Err(EncodeResult::error_with_span(
                        "AMMX memory operands require A0-A7, B0-B7, or SP",
                        *span,
                    ));
                };
                Ok(Operand::AddressDisplacement {
                    displacement: displacement.clone(),
                    base: format!("A{reg}"),
                    span: *span,
                })
            }
            Operand::AddressIndexed {
                displacement,
                base,
                index,
                index_size,
                span,
            } => {
                let Some(reg) = Self::ammx_banked_address_register_number(base) else {
                    return Err(EncodeResult::error_with_span(
                        "AMMX memory operands require A0-A7, B0-B7, or SP",
                        *span,
                    ));
                };
                Ok(Operand::AddressIndexed {
                    displacement: displacement.clone(),
                    base: format!("A{reg}"),
                    index: index.clone(),
                    index_size: *index_size,
                    span: *span,
                })
            }
            _ => Err(EncodeResult::error_with_span(
                "AMMX memory operands require A0-A7, B0-B7, or SP",
                operand.span(),
            )),
        }
    }
    #[allow(clippy::too_many_arguments)]
    fn emit_apollo_two_word_instruction(
        bytes: &mut Vec<u8>,
        selector: u16,
        a_bit: u16,
        b_bit: u16,
        d_bit: u16,
        ea_bits: u16,
        second_word: u16,
        extension: &[u8],
    ) {
        let first_word = 0xF000
            | ((selector & 0x7) << 9)
            | ((a_bit & 0x1) << 8)
            | ((b_bit & 0x1) << 7)
            | ((d_bit & 0x1) << 6)
            | (ea_bits & 0x3F);
        Self::emit_word(bytes, first_word);
        Self::emit_word(bytes, second_word);
        bytes.extend_from_slice(extension);
    }

    fn ammx_address_register_number(register: &str) -> Option<(u16, u8)> {
        if register.eq_ignore_ascii_case("SP") {
            return Some((0, 7));
        }
        if let Some(suffix) = register
            .strip_prefix('A')
            .or_else(|| register.strip_prefix('a'))
        {
            let value = suffix.parse::<u8>().ok()?;
            return (value <= 7).then_some((0, value));
        }
        let suffix = register
            .strip_prefix('B')
            .or_else(|| register.strip_prefix('b'))?;
        let value = suffix.parse::<u8>().ok()?;
        (value <= 7).then_some((1, value))
    }

    fn ammx_banked_address_register_number(register: &str) -> Option<u8> {
        let suffix = register
            .strip_prefix('B')
            .or_else(|| register.strip_prefix('b'))?;
        let value = suffix.parse::<u8>().ok()?;
        (value <= 7).then_some(value)
    }

    fn emit_word(bytes: &mut Vec<u8>, value: u16) {
        bytes.extend_from_slice(&value.to_be_bytes());
    }

    fn emit_long(bytes: &mut Vec<u8>, value: u32) {
        bytes.extend_from_slice(&value.to_be_bytes());
    }

    fn apollo_mode_enabled(&self, ctx: &dyn AssemblerContext) -> bool {
        ctx.cpu_state_flag(state::APOLLO_MODE_KEY).unwrap_or(0) != 0
    }
}
