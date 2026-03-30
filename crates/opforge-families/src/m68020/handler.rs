// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68020 CPU handler implementation.

use crate::families::m68k::operand::{
    ControlRegisterKind, FpuControlRegisterKind, RegisterListRegister, SpecialRegisterKind,
};
use crate::families::m68k::{
    has_fpu_mnemonic, has_m68020_mnemonic, has_mnemonic, parse_fpu_mnemonic, parse_m68010_mnemonic,
    parse_m68020_mnemonic, parse_mnemonic, FamilyOperand, FpuFormat, FpuMnemonicKind,
    M68010MnemonicKind, M68020MnemonicKind, M68KFamilyHandler, MnemonicKind, Operand,
    OperationSize,
};
use opcore::parser::Expr;
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FmovemRegisterListEncoding {
    Data(u16),
    Control(u16),
}

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

    fn fpu_opcode_word(low_bits: u16) -> u16 {
        0xF200 | low_bits
    }

    fn fpu_effective_address_word(prefix_bits: u16, ea_bits: u16) -> u16 {
        Self::fpu_opcode_word(prefix_bits | (ea_bits & 0x003F))
    }

    fn effective_address_mode(bits: u16) -> u16 {
        (bits >> 3) & 0b111
    }

    fn effective_address_register(bits: u16) -> u16 {
        bits & 0b111
    }

    fn fpu_scalar_format_bits(size: OperationSize) -> u16 {
        match size {
            OperationSize::Byte => 0x1800,
            OperationSize::Word => 0x1000,
            OperationSize::Long => 0x0000,
        }
    }

    fn fpu_native_format_bits(format: FpuFormat) -> u16 {
        match format {
            FpuFormat::Single => 0x0400,
            FpuFormat::Double => 0x1400,
            FpuFormat::Extended => 0x0800,
            FpuFormat::Packed => 0x0C00,
        }
    }

    fn fpu_format_suffix(format: FpuFormat) -> &'static str {
        match format {
            FpuFormat::Single => ".S",
            FpuFormat::Double => ".D",
            FpuFormat::Extended => ".X",
            FpuFormat::Packed => ".P",
        }
    }

    fn fpu_scalar_source_operand(bits: u16) -> bool {
        matches!(
            (
                Self::effective_address_mode(bits),
                Self::effective_address_register(bits),
            ),
            (0 | 2..=6, _) | (7, 0..=4)
        )
    }

    fn fpu_scalar_destination_operand(bits: u16) -> bool {
        matches!(
            (
                Self::effective_address_mode(bits),
                Self::effective_address_register(bits),
            ),
            (0 | 2 | 3 | 4 | 5 | 6, _) | (7, 0 | 1)
        )
    }

    fn fpu_native_source_operand(bits: u16) -> bool {
        matches!(
            (
                Self::effective_address_mode(bits),
                Self::effective_address_register(bits),
            ),
            (2..=6, _) | (7, 0..=4)
        )
    }

    fn fpu_native_destination_operand(bits: u16) -> bool {
        matches!(
            (
                Self::effective_address_mode(bits),
                Self::effective_address_register(bits),
            ),
            (2..=6, _) | (7, 0 | 1)
        )
    }

    fn fpu_save_operand(bits: u16) -> bool {
        matches!(
            (
                Self::effective_address_mode(bits),
                Self::effective_address_register(bits),
            ),
            (2 | 4 | 5 | 6, _) | (7, 0 | 1)
        )
    }

    fn fpu_restore_operand(bits: u16) -> bool {
        matches!(
            (
                Self::effective_address_mode(bits),
                Self::effective_address_register(bits),
            ),
            (2 | 3 | 5 | 6, _) | (7, 0..=3)
        )
    }

    fn fpu_condition_code(suffix: &str) -> Option<u16> {
        match suffix {
            "F" => Some(0x0000),
            "EQ" => Some(0x0001),
            "OGT" => Some(0x0002),
            "OGE" => Some(0x0003),
            "OLT" => Some(0x0004),
            "OLE" => Some(0x0005),
            "OGL" => Some(0x0006),
            "OR" => Some(0x0007),
            "UN" => Some(0x0008),
            "UEQ" => Some(0x0009),
            "UGT" => Some(0x000A),
            "UGE" => Some(0x000B),
            "ULT" => Some(0x000C),
            "ULE" => Some(0x000D),
            "NE" => Some(0x000E),
            "T" => Some(0x000F),
            "SF" => Some(0x0010),
            "SEQ" => Some(0x0011),
            "GT" => Some(0x0012),
            "GE" => Some(0x0013),
            "LT" => Some(0x0014),
            "LE" => Some(0x0015),
            "GL" => Some(0x0016),
            "GLE" => Some(0x0017),
            "NGLE" => Some(0x0018),
            "NGL" => Some(0x0019),
            "NLE" => Some(0x001A),
            "NLT" => Some(0x001B),
            "NGE" => Some(0x001C),
            "NGT" => Some(0x001D),
            "SNE" => Some(0x001E),
            "ST" => Some(0x001F),
            _ => None,
        }
    }

    fn fpu_condition_from_mnemonic(
        mnemonic: &str,
        prefix: &str,
    ) -> Result<u16, EncodeResult<Vec<u8>>> {
        let suffix = mnemonic.strip_prefix(prefix).ok_or_else(|| {
            EncodeResult::error(format!("unsupported FPU conditional mnemonic {mnemonic}"))
        })?;
        let suffix = suffix.to_ascii_uppercase();
        if prefix == "FDB" && suffix == "RA" {
            return Ok(0x0000);
        }
        Self::fpu_condition_code(&suffix).ok_or_else(|| {
            EncodeResult::error(format!("unsupported FPU conditional mnemonic {mnemonic}"))
        })
    }

    fn branch_offset(
        expr: &Expr,
        ctx: &dyn AssemblerContext,
        base_bytes: i64,
    ) -> Result<(i64, bool), String> {
        let (target, unresolved) = M68KFamilyHandler::eval_expr_or_placeholder(expr, ctx, 0)?;
        if unresolved {
            Ok((0, true))
        } else {
            Ok((target - (ctx.current_address() as i64 + base_bytes), false))
        }
    }

    fn encode_auto_sized_branch(
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

        let (offset, unresolved) = match Self::branch_offset(expr, ctx, 2) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, target.span()),
        };

        if !unresolved && M68KFamilyHandler::encode_signed_word(offset).is_none() {
            if !((i32::MIN as i64)..=(i32::MAX as i64)).contains(&offset) {
                return EncodeResult::error_with_span(
                    format!("{mnemonic}.L branch displacement out of range: offset {offset}"),
                    target.span(),
                );
            }

            let mut bytes = Vec::new();
            M68KFamilyHandler::emit_word(&mut bytes, 0x6000 | (condition_bits << 8) | 0x00FF);
            bytes.extend_from_slice(&(offset as i32 as u32).to_be_bytes());
            return EncodeResult::ok(bytes);
        }
        let Some(encoded) = M68KFamilyHandler::encode_signed_word(offset) else {
            return EncodeResult::error_with_span(
                format!("{mnemonic}.W branch displacement out of range: offset {offset}"),
                target.span(),
            );
        };

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, 0x6000 | (condition_bits << 8));
        M68KFamilyHandler::emit_word(&mut bytes, encoded);
        EncodeResult::ok(bytes)
    }

    fn fpu_register_list_encoding(
        registers: &[RegisterListRegister],
        reverse: bool,
    ) -> Result<FmovemRegisterListEncoding, EncodeResult<Vec<u8>>> {
        let mut data_mask = 0_u16;
        let mut control_mask = 0_u16;
        let mut saw_data = false;
        let mut saw_control = false;

        for register in registers {
            match register {
                RegisterListRegister::FpuData(reg) => {
                    saw_data = true;
                    let bit = if reverse { 7 - *reg as u16 } else { *reg as u16 };
                    data_mask |= 1_u16 << bit;
                }
                RegisterListRegister::FpuControl(register) => {
                    saw_control = true;
                    control_mask |= Self::fpu_control_register_field(*register);
                }
                _ => {
                    return Err(EncodeResult::error(
                        "FMOVEM currently supports only FP data-register or FPU control-register lists on m68020",
                    ));
                }
            }
        }

        if saw_data && saw_control {
            return Err(EncodeResult::error(
                "FMOVEM register lists cannot mix FP data registers with FPU control registers on m68020",
            ));
        }

        if saw_control {
            Ok(FmovemRegisterListEncoding::Control(control_mask))
        } else {
            Ok(FmovemRegisterListEncoding::Data(data_mask))
        }
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

    fn encode_fmove_integer_conversion(
        &self,
        size: OperationSize,
        src: &Operand,
        dst: &Operand,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let format_bits = Self::fpu_scalar_format_bits(size);

        match (src, dst) {
            (
                Operand::FpuDataRegister {
                    register: src_register,
                    ..
                },
                dst,
            ) => {
                let Some(src_reg) = M68KFamilyHandler::fpu_data_register_number(src_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid FMOVE{} source FP register", size.suffix()),
                        src.span(),
                    );
                };

                let dst_ea = match self.family.encode_effective_address(dst, Some(size), ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                if !Self::fpu_scalar_destination_operand(dst_ea.bits) {
                    return EncodeResult::error_with_span(
                        format!("invalid destination effective address for FMOVE{}", size.suffix()),
                        dst.span(),
                    );
                }

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, dst_ea.bits),
                );
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0x6000 | format_bits | ((src_reg as u16) << 7),
                );
                bytes.extend_from_slice(&dst_ea.extension);
                EncodeResult::ok(bytes)
            }
            (
                src,
                Operand::FpuDataRegister {
                    register: dst_register,
                    ..
                },
            ) => {
                let Some(dst_reg) = M68KFamilyHandler::fpu_data_register_number(dst_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid FMOVE{} destination FP register", size.suffix()),
                        dst.span(),
                    );
                };

                let src_ea = match self.family.encode_effective_address(src, Some(size), ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                if !Self::fpu_scalar_source_operand(src_ea.bits) {
                    return EncodeResult::error_with_span(
                        format!("invalid source effective address for FMOVE{}", size.suffix()),
                        src.span(),
                    );
                }

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, src_ea.bits),
                );
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0x4000 | format_bits | ((dst_reg as u16) << 7),
                );
                bytes.extend_from_slice(&src_ea.extension);
                EncodeResult::ok(bytes)
            }
            _ => EncodeResult::error(format!(
                "FMOVE{} currently supports only FP-register <-> scalar register or memory conversion forms on m68020",
                size.suffix(),
            )),
        }
    }

    fn encode_fmove_native_format(
        &self,
        format: FpuFormat,
        src: &Operand,
        dst: &Operand,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let format_bits = Self::fpu_native_format_bits(format);
        let suffix = Self::fpu_format_suffix(format);

        match (src, dst) {
            (
                Operand::FpuDataRegister {
                    register: src_register,
                    ..
                },
                dst,
            ) => {
                let Some(src_reg) = M68KFamilyHandler::fpu_data_register_number(src_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid FMOVE{suffix} source FP register"),
                        src.span(),
                    );
                };

                let dst_ea = match self.family.encode_effective_address(dst, None, ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                if !Self::fpu_native_destination_operand(dst_ea.bits) {
                    return EncodeResult::error_with_span(
                        format!("invalid destination effective address for FMOVE{suffix}"),
                        dst.span(),
                    );
                }

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, dst_ea.bits),
                );
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0x6000 | format_bits | ((src_reg as u16) << 7),
                );
                bytes.extend_from_slice(&dst_ea.extension);
                EncodeResult::ok(bytes)
            }
            (
                src,
                Operand::FpuDataRegister {
                    register: dst_register,
                    ..
                },
            ) => {
                let Some(dst_reg) = M68KFamilyHandler::fpu_data_register_number(dst_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid FMOVE{suffix} destination FP register"),
                        dst.span(),
                    );
                };

                let src_ea = match self.family.encode_effective_address(src, None, ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                if !Self::fpu_native_source_operand(src_ea.bits) {
                    return EncodeResult::error_with_span(
                        format!("invalid source effective address for FMOVE{suffix}"),
                        src.span(),
                    );
                }

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, src_ea.bits),
                );
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0x4000 | format_bits | ((dst_reg as u16) << 7),
                );
                bytes.extend_from_slice(&src_ea.extension);
                EncodeResult::ok(bytes)
            }
            _ => EncodeResult::error(format!(
                "FMOVE{suffix} currently supports native floating-point transfers between FP registers and legal memory effective addresses on m68020",
            )),
        }
    }

    fn encode_fmove(
        &self,
        size: Option<OperationSize>,
        format: Option<FpuFormat>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("FMOVE expects two operands");
        };

        match (size, format, src, dst) {
            (
                None,
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
                M68KFamilyHandler::emit_word(&mut bytes, Self::fpu_opcode_word(0x0000));
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    ((src_reg as u16) << 10) | ((dst_reg as u16) << 7),
                );
                EncodeResult::ok(bytes)
            }
            (
                Some(OperationSize::Long),
                None,
                src,
                Operand::FpuControlRegister {
                    register: control, ..
                },
            ) => {
                let src_ea = match self
                    .family
                    .encode_effective_address(src, Some(OperationSize::Long), ctx)
                {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                if !Self::fpu_scalar_source_operand(src_ea.bits) {
                    return EncodeResult::error_with_span(
                        "invalid source effective address for FMOVE.L",
                        src.span(),
                    );
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, src_ea.bits),
                );
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0x8000 | Self::fpu_control_register_field(*control),
                );
                bytes.extend_from_slice(&src_ea.extension);
                EncodeResult::ok(bytes)
            }
            (
                Some(OperationSize::Long),
                None,
                Operand::FpuControlRegister {
                    register: control, ..
                },
                dst,
            ) => {
                let dst_ea = match self
                    .family
                    .encode_effective_address(dst, Some(OperationSize::Long), ctx)
                {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                if !Self::fpu_scalar_destination_operand(dst_ea.bits) {
                    return EncodeResult::error_with_span(
                        "invalid destination effective address for FMOVE.L",
                        dst.span(),
                    );
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, dst_ea.bits),
                );
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0xA000 | Self::fpu_control_register_field(*control),
                );
                bytes.extend_from_slice(&dst_ea.extension);
                EncodeResult::ok(bytes)
            }
            (
                Some(size @ (OperationSize::Byte | OperationSize::Word | OperationSize::Long)),
                None,
                _,
                _,
            ) => {
                self.encode_fmove_integer_conversion(size, src, dst, ctx)
            }
            (None, Some(format), _, _) => self.encode_fmove_native_format(format, src, dst, ctx),
            (None, None, _, _) => EncodeResult::error(
                "FMOVE currently supports only unsuffixed FP-register moves on m68020; use .B, .W, or .L for scalar conversion forms",
            ),
            _ => EncodeResult::error("unsupported FMOVE suffix combination on m68020"),
        }
    }

    fn encode_fpu_result_operation(
        &self,
        display_name: &str,
        opcode: u16,
        size: Option<OperationSize>,
        format: Option<FpuFormat>,
        operands: &[Operand],
        allow_single_register: bool,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        match operands {
            [
                Operand::FpuDataRegister {
                    register: src_register,
                    ..
                },
                Operand::FpuDataRegister {
                    register: dst_register,
                    ..
                },
            ] if size.is_none() => {
                let Some(src_reg) = M68KFamilyHandler::fpu_data_register_number(src_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {display_name} source FP register"),
                        operands[0].span(),
                    );
                };
                let Some(dst_reg) = M68KFamilyHandler::fpu_data_register_number(dst_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {display_name} destination FP register"),
                        operands[1].span(),
                    );
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(&mut bytes, Self::fpu_opcode_word(0x0000));
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    ((src_reg as u16) << 10) | ((dst_reg as u16) << 7) | opcode,
                );
                EncodeResult::ok(bytes)
            }
            [Operand::FpuDataRegister { register, .. }] if allow_single_register && size.is_none() => {
                let Some(reg) = M68KFamilyHandler::fpu_data_register_number(register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {display_name} FP register"),
                        operands[0].span(),
                    );
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(&mut bytes, Self::fpu_opcode_word(0x0000));
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    ((reg as u16) << 10) | ((reg as u16) << 7) | opcode,
                );
                EncodeResult::ok(bytes)
            }
            [
                src,
                Operand::FpuDataRegister {
                    register: dst_register,
                    ..
                },
            ] => {
                let (format_bits, suffix, ea_size, native_format) = match (size, format) {
                    (Some(size), None) => (
                        Self::fpu_scalar_format_bits(size),
                        size.suffix().to_string(),
                        Some(size),
                        false,
                    ),
                    (None, Some(format)) => (
                        Self::fpu_native_format_bits(format),
                        Self::fpu_format_suffix(format).to_string(),
                        None,
                        true,
                    ),
                    (None, None) => {
                        return EncodeResult::error(format!(
                            "{display_name} currently supports FP-register forms without a size suffix and .B, .W, .L, .S, .D, .X, or .P source forms on m68020",
                        ))
                    }
                    _ => {
                        return EncodeResult::error(format!(
                            "{display_name} received an unsupported suffix combination on m68020",
                        ))
                    }
                };
                let Some(dst_reg) = M68KFamilyHandler::fpu_data_register_number(dst_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {display_name} destination FP register"),
                        operands[1].span(),
                    );
                };

                let src_ea = match self.family.encode_effective_address(src, ea_size, ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                let source_ok = if native_format {
                    Self::fpu_native_source_operand(src_ea.bits)
                } else {
                    Self::fpu_scalar_source_operand(src_ea.bits)
                };
                if !source_ok {
                    return EncodeResult::error(format!(
                        "{display_name} currently supports FP-register forms and sized scalar or native floating-point source forms on m68020",
                    ));
                }
                if !source_ok {
                    return EncodeResult::error_with_span(
                        format!("invalid source effective address for {display_name}{suffix}"),
                        src.span(),
                    );
                }

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, src_ea.bits),
                );
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0x4000 | format_bits | ((dst_reg as u16) << 7) | opcode,
                );
                bytes.extend_from_slice(&src_ea.extension);
                EncodeResult::ok(bytes)
            }
            _ => EncodeResult::error(format!(
                "{display_name} currently supports FP-register forms and sized scalar-source forms on m68020",
            )),
        }
    }

    fn encode_fsincos(
        &self,
        display_name: &str,
        size: Option<OperationSize>,
        format: Option<FpuFormat>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        match operands {
            [
                Operand::FpuDataRegister {
                    register: src_register,
                    ..
                },
                Operand::RegisterPair {
                    left: dst_cos_register,
                    right: dst_sin_register,
                    ..
                },
            ] if size.is_none() => {
                let Some(src_reg) = M68KFamilyHandler::fpu_data_register_number(src_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {display_name} source FP register"),
                        operands[0].span(),
                    );
                };
                let Some(dst_cos_reg) =
                    M68KFamilyHandler::fpu_data_register_number(dst_cos_register)
                else {
                    return EncodeResult::error_with_span(
                        format!("invalid {display_name} destination FP register pair"),
                        operands[1].span(),
                    );
                };
                let Some(dst_sin_reg) =
                    M68KFamilyHandler::fpu_data_register_number(dst_sin_register)
                else {
                    return EncodeResult::error_with_span(
                        format!("invalid {display_name} destination FP register pair"),
                        operands[1].span(),
                    );
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(&mut bytes, Self::fpu_opcode_word(0x0000));
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    ((src_reg as u16) << 10) | ((dst_sin_reg as u16) << 7) | (dst_cos_reg as u16) | 0x0030,
                );
                EncodeResult::ok(bytes)
            }
            [src, Operand::RegisterPair { left: dst_cos_register, right: dst_sin_register, .. }] => {
                let (format_bits, suffix, ea_size, native_format) = match (size, format) {
                    (Some(size), None) => (
                        Self::fpu_scalar_format_bits(size),
                        size.suffix().to_string(),
                        Some(size),
                        false,
                    ),
                    (None, Some(format)) => (
                        Self::fpu_native_format_bits(format),
                        Self::fpu_format_suffix(format).to_string(),
                        None,
                        true,
                    ),
                    (None, None) => {
                        return EncodeResult::error(format!(
                            "{display_name} currently supports FP-register forms without a size suffix and .B, .W, .L, .S, .D, .X, or .P source forms on m68020",
                        ))
                    }
                    _ => {
                        return EncodeResult::error(format!(
                            "{display_name} received an unsupported suffix combination on m68020",
                        ))
                    }
                };
                let Some(dst_cos_reg) =
                    M68KFamilyHandler::fpu_data_register_number(dst_cos_register)
                else {
                    return EncodeResult::error_with_span(
                        format!("invalid {display_name} destination FP register pair"),
                        operands[1].span(),
                    );
                };
                let Some(dst_sin_reg) =
                    M68KFamilyHandler::fpu_data_register_number(dst_sin_register)
                else {
                    return EncodeResult::error_with_span(
                        format!("invalid {display_name} destination FP register pair"),
                        operands[1].span(),
                    );
                };

                let src_ea = match self.family.encode_effective_address(src, ea_size, ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                let source_ok = if native_format {
                    Self::fpu_native_source_operand(src_ea.bits)
                } else {
                    Self::fpu_scalar_source_operand(src_ea.bits)
                };
                if !source_ok {
                    return EncodeResult::error(format!(
                        "{display_name} currently supports FP-register source forms with FP-register-pair destinations and sized scalar or native floating-point source forms on m68020",
                    ));
                }
                if !source_ok {
                    return EncodeResult::error_with_span(
                        format!("invalid source effective address for {display_name}{suffix}"),
                        src.span(),
                    );
                }

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, src_ea.bits),
                );
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0x4000 | format_bits | ((dst_sin_reg as u16) << 7) | (dst_cos_reg as u16) | 0x0030,
                );
                bytes.extend_from_slice(&src_ea.extension);
                EncodeResult::ok(bytes)
            }
            _ => EncodeResult::error(format!(
                "{display_name} currently supports FP-register source forms with FP-register-pair destinations and sized scalar-source forms on m68020",
            )),
        }
    }

    fn encode_ftst(
        &self,
        size: Option<OperationSize>,
        format: Option<FpuFormat>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        match operands {
            [Operand::FpuDataRegister { register, .. }] if size.is_none() && format.is_none() => {
                let Some(reg) = M68KFamilyHandler::fpu_data_register_number(register) else {
                    return EncodeResult::error_with_span(
                        "invalid FTST FP register",
                        operands[0].span(),
                    );
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(&mut bytes, Self::fpu_opcode_word(0x0000));
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    ((reg as u16) << 10) | ((reg as u16) << 7) | 0x003A,
                );
                EncodeResult::ok(bytes)
            }
            [src] => {
                let (format_bits, suffix, ea_size, native_format) = match (size, format) {
                    (Some(size), None) => (
                        Self::fpu_scalar_format_bits(size),
                        size.suffix().to_string(),
                        Some(size),
                        false,
                    ),
                    (None, Some(format)) => (
                        Self::fpu_native_format_bits(format),
                        Self::fpu_format_suffix(format).to_string(),
                        None,
                        true,
                    ),
                    (None, None) => {
                        return EncodeResult::error(
                            "FTST currently supports FP-register forms without a size suffix and .B, .W, .L, .S, .D, .X, or .P source forms on m68020",
                        )
                    }
                    _ => {
                        return EncodeResult::error(
                            "unsupported FTST suffix combination on m68020",
                        )
                    }
                };

                let src_ea = match self.family.encode_effective_address(src, ea_size, ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                let source_ok = if native_format {
                    Self::fpu_native_source_operand(src_ea.bits)
                } else {
                    Self::fpu_scalar_source_operand(src_ea.bits)
                };
                if !source_ok {
                    return EncodeResult::error_with_span(
                        format!("invalid source effective address for FTST{suffix}"),
                        src.span(),
                    );
                }

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, src_ea.bits),
                );
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    0x4000 | format_bits | 0x003A,
                );
                bytes.extend_from_slice(&src_ea.extension);
                EncodeResult::ok(bytes)
            }
            _ => EncodeResult::error("FTST expects exactly one operand"),
        }
    }

    fn encode_fbcc(
        &self,
        mnemonic: &str,
        size: Option<OperationSize>,
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

        let condition = match Self::fpu_condition_from_mnemonic(mnemonic, "FB") {
            Ok(condition) => condition,
            Err(err) => return err,
        };
        let (opcode, offset) = match size {
            Some(OperationSize::Byte) => {
                return EncodeResult::error(format!("{mnemonic} does not support .B size"));
            }
            Some(OperationSize::Long) => {
                let (offset, _) = match Self::branch_offset(expr, ctx, 2) {
                    Ok(result) => result,
                    Err(err) => return EncodeResult::error_with_span(err, target.span()),
                };
                (0xF0C0 | condition, offset)
            }
            Some(OperationSize::Word) | None => {
                let (offset, _) = match Self::branch_offset(expr, ctx, 2) {
                    Ok(result) => result,
                    Err(err) => return EncodeResult::error_with_span(err, target.span()),
                };
                (Self::fpu_opcode_word(0x0080 | condition), offset)
            }
        };
        let opcode = if matches!(size, Some(OperationSize::Long)) {
            Self::fpu_opcode_word(0x00C0 | condition)
        } else {
            opcode
        };

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, opcode);
        match size {
            Some(OperationSize::Long) => {
                if !((i32::MIN as i64)..=(i32::MAX as i64)).contains(&offset) {
                    return EncodeResult::error_with_span(
                        format!("{mnemonic}.L branch displacement out of range: offset {offset}"),
                        target.span(),
                    );
                }
                bytes.extend_from_slice(&(offset as i32 as u32).to_be_bytes());
            }
            Some(OperationSize::Word) | None => {
                let Some(encoded) = M68KFamilyHandler::encode_signed_word(offset) else {
                    return EncodeResult::error_with_span(
                        format!("{mnemonic}.W branch displacement out of range: offset {offset}"),
                        target.span(),
                    );
                };
                M68KFamilyHandler::emit_word(&mut bytes, encoded);
            }
            Some(OperationSize::Byte) => unreachable!("handled above"),
        }
        EncodeResult::ok(bytes)
    }

    fn encode_fdbcc(
        &self,
        mnemonic: &str,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }

        let [counter, target] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects a data register and target"));
        };
        let counter_register = match counter {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    format!("{mnemonic} counter must be a data register"),
                    counter.span(),
                );
            }
        };
        let Some(counter_bits) = M68KFamilyHandler::data_register_number(counter_register) else {
            return EncodeResult::error_with_span(
                format!("invalid {mnemonic} counter register"),
                counter.span(),
            );
        };
        let Operand::BranchTarget { expr, .. } = target else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} requires a branch target expression"),
                target.span(),
            );
        };

        let condition = match Self::fpu_condition_from_mnemonic(mnemonic, "FDB") {
            Ok(condition) => condition,
            Err(err) => return err,
        };
        let (offset, _) = match Self::branch_offset(expr, ctx, 4) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, target.span()),
        };
        let Some(encoded_displacement) = M68KFamilyHandler::encode_signed_word(offset) else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} branch displacement out of range: offset {offset}"),
                target.span(),
            );
        };

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(
            &mut bytes,
            Self::fpu_opcode_word(0x0048 | counter_bits as u16),
        );
        M68KFamilyHandler::emit_word(&mut bytes, condition);
        M68KFamilyHandler::emit_word(&mut bytes, encoded_displacement);
        EncodeResult::ok(bytes)
    }

    fn encode_fscc(
        &self,
        mnemonic: &str,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }
        let [dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one operand"));
        };

        let dst_ea = match self
            .family
            .encode_effective_address(dst, Some(OperationSize::Byte), ctx)
        {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::fpu_scalar_destination_operand(dst_ea.bits) {
            return EncodeResult::error_with_span(
                format!("invalid destination effective address for {mnemonic}"),
                dst.span(),
            );
        }

        let condition = match Self::fpu_condition_from_mnemonic(mnemonic, "FS") {
            Ok(condition) => condition,
            Err(err) => return err,
        };

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(
            &mut bytes,
            Self::fpu_effective_address_word(0x0040, dst_ea.bits),
        );
        M68KFamilyHandler::emit_word(&mut bytes, condition);
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_ftrapcc(
        &self,
        mnemonic: &str,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let condition = match Self::fpu_condition_from_mnemonic(mnemonic, "FTRAP") {
            Ok(condition) => condition,
            Err(err) => return err,
        };

        let mut bytes = Vec::new();
        match size {
            None => {
                if !operands.is_empty() {
                    return EncodeResult::error(format!(
                        "unsized {mnemonic} does not take an immediate operand"
                    ));
                }
                M68KFamilyHandler::emit_word(&mut bytes, Self::fpu_opcode_word(0x007C));
                M68KFamilyHandler::emit_word(&mut bytes, condition);
            }
            Some(size @ (OperationSize::Word | OperationSize::Long)) => {
                let [immediate] = operands else {
                    return EncodeResult::error(format!(
                        "{mnemonic}{} expects one immediate operand",
                        size.suffix()
                    ));
                };
                let Operand::Immediate { expr, .. } = immediate else {
                    return EncodeResult::error_with_span(
                        format!("{mnemonic}{} operand must be immediate", size.suffix()),
                        immediate.span(),
                    );
                };
                let value = match M68KFamilyHandler::eval_expr(expr, ctx) {
                    Ok(value) => value,
                    Err(err) => return EncodeResult::error_with_span(err, immediate.span()),
                };

                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_opcode_word(0x0078)
                        | if size == OperationSize::Word {
                            0x0002
                        } else {
                            0x0003
                        },
                );
                M68KFamilyHandler::emit_word(&mut bytes, condition);
                match size {
                    OperationSize::Word => {
                        if !(-32768..=65535).contains(&value) {
                            return EncodeResult::error_with_span(
                                format!("{mnemonic}.W immediate {value} out of range"),
                                immediate.span(),
                            );
                        }
                        M68KFamilyHandler::emit_word(&mut bytes, value as u16);
                    }
                    OperationSize::Long => {
                        if !(-2_147_483_648..=4_294_967_295).contains(&value) {
                            return EncodeResult::error_with_span(
                                format!("{mnemonic}.L immediate {value} out of range"),
                                immediate.span(),
                            );
                        }
                        bytes.extend_from_slice(&(value as i32 as u32).to_be_bytes());
                    }
                    OperationSize::Byte => unreachable!("filtered above"),
                }
            }
            Some(OperationSize::Byte) => {
                return EncodeResult::error(format!("{mnemonic} does not support .B size"));
            }
        }

        EncodeResult::ok(bytes)
    }

    fn encode_fnop(
        &self,
        mnemonic: &str,
        size: Option<OperationSize>,
        format: Option<FpuFormat>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() || format.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }
        if !operands.is_empty() {
            return EncodeResult::error(format!("{mnemonic} does not take operands"));
        }

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, Self::fpu_opcode_word(0x0000));
        M68KFamilyHandler::emit_word(&mut bytes, 0x0000);
        EncodeResult::ok(bytes)
    }

    fn encode_fmovecr(
        &self,
        mnemonic: &str,
        size: Option<OperationSize>,
        format: Option<FpuFormat>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() || format.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }
        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands"));
        };

        let Operand::Immediate { expr, .. } = src else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} source must be an immediate ROM constant selector"),
                src.span(),
            );
        };
        let value = match M68KFamilyHandler::eval_expr(expr, ctx) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, src.span()),
        };
        if !(0..=127).contains(&value) {
            return EncodeResult::error_with_span(
                format!("{mnemonic} ROM constant selector {value} out of range"),
                src.span(),
            );
        }

        let Operand::FpuDataRegister {
            register: dst_register,
            ..
        } = dst
        else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} destination must be an FP data register"),
                dst.span(),
            );
        };
        let Some(dst_reg) = M68KFamilyHandler::fpu_data_register_number(dst_register) else {
            return EncodeResult::error_with_span(
                format!("invalid {mnemonic} destination FP register"),
                dst.span(),
            );
        };

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, Self::fpu_opcode_word(0x0000));
        M68KFamilyHandler::emit_word(
            &mut bytes,
            0x5C00 | ((dst_reg as u16) << 7) | (value as u16 & 0x007F),
        );
        EncodeResult::ok(bytes)
    }

    fn encode_fsave(
        &self,
        mnemonic: &str,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }
        let [dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one operand"));
        };

        let dst_ea = match self.family.encode_effective_address(dst, None, ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::fpu_save_operand(dst_ea.bits) {
            return EncodeResult::error_with_span(
                format!("invalid destination effective address for {mnemonic}"),
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(
            &mut bytes,
            Self::fpu_effective_address_word(0x0100, dst_ea.bits),
        );
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_frestore(
        &self,
        mnemonic: &str,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }
        let [src] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one operand"));
        };

        let src_ea = match self.family.encode_effective_address(src, None, ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::fpu_restore_operand(src_ea.bits) {
            return EncodeResult::error_with_span(
                format!("invalid source effective address for {mnemonic}"),
                src.span(),
            );
        }

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(
            &mut bytes,
            Self::fpu_effective_address_word(0x0140, src_ea.bits),
        );
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_supported_fpu_core_mnemonic(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> Option<EncodeResult<Vec<u8>>> {
        let parsed = parse_fpu_mnemonic(mnemonic)?;
        if parsed.has_unknown_size_suffix {
            return Some(EncodeResult::error(format!(
                "unsupported size suffix for {}",
                parsed.display_name
            )));
        }

        Some(match parsed.kind {
            FpuMnemonicKind::Fnop => {
                self.encode_fnop(&parsed.display_name, parsed.size, parsed.format, operands)
            }
            FpuMnemonicKind::Fmove => self.encode_fmove(parsed.size, parsed.format, operands, ctx),
            FpuMnemonicKind::Fmovecr => {
                self.encode_fmovecr(&parsed.display_name, parsed.size, parsed.format, operands, ctx)
            }
            FpuMnemonicKind::Fmovem => self.encode_fmovem(parsed.size, operands, ctx),
            FpuMnemonicKind::Fadd => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0022,
                parsed.size,
                parsed.format,
                operands,
                false,
                ctx,
            ),
            FpuMnemonicKind::Fsub => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0028,
                parsed.size,
                parsed.format,
                operands,
                false,
                ctx,
            ),
            FpuMnemonicKind::Fmul => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0023,
                parsed.size,
                parsed.format,
                operands,
                false,
                ctx,
            ),
            FpuMnemonicKind::Fdiv => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0020,
                parsed.size,
                parsed.format,
                operands,
                false,
                ctx,
            ),
            FpuMnemonicKind::Fsqrt => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0004,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fabs => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0018,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fneg => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x001A,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fcmp => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0038,
                parsed.size,
                parsed.format,
                operands,
                false,
                ctx,
            ),
            FpuMnemonicKind::Ftst => self.encode_ftst(parsed.size, parsed.format, operands, ctx),
            FpuMnemonicKind::Fint => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0001,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fintrz => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0003,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fsgldiv => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0024,
                parsed.size,
                parsed.format,
                operands,
                false,
                ctx,
            ),
            FpuMnemonicKind::Fsglmul => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0027,
                parsed.size,
                parsed.format,
                operands,
                false,
                ctx,
            ),
            FpuMnemonicKind::Fsin => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x000E,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fcos => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x001D,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fsincos => {
                self.encode_fsincos(&parsed.display_name, parsed.size, parsed.format, operands, ctx)
            }
            FpuMnemonicKind::Ftan => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x000F,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fasin => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x000C,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Facos => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x001C,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fatan => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x000A,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fsinh => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0002,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fcosh => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0019,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Ftanh => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0009,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fatanh => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x000D,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fetox => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0010,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fetoxm1 => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0008,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Ftentox => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0012,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Ftwotox => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0011,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Flogn => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0014,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Flognp1 => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0006,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Flog10 => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0015,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Flog2 => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0016,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fgetexp => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x001E,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fgetman => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x001F,
                parsed.size,
                parsed.format,
                operands,
                true,
                ctx,
            ),
            FpuMnemonicKind::Fscale => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0026,
                parsed.size,
                parsed.format,
                operands,
                false,
                ctx,
            ),
            FpuMnemonicKind::Fmod => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0021,
                parsed.size,
                parsed.format,
                operands,
                false,
                ctx,
            ),
            FpuMnemonicKind::Frem => self.encode_fpu_result_operation(
                &parsed.display_name,
                0x0025,
                parsed.size,
                parsed.format,
                operands,
                false,
                ctx,
            ),
            FpuMnemonicKind::Fbranch => {
                self.encode_fbcc(&parsed.display_name, parsed.size, operands, ctx)
            }
            FpuMnemonicKind::Fdbcc => {
                self.encode_fdbcc(&parsed.display_name, parsed.size, operands, ctx)
            }
            FpuMnemonicKind::Fscc => {
                self.encode_fscc(&parsed.display_name, parsed.size, operands, ctx)
            }
            FpuMnemonicKind::Ftrapcc => {
                self.encode_ftrapcc(&parsed.display_name, parsed.size, operands, ctx)
            }
            FpuMnemonicKind::Fsave => {
                self.encode_fsave(&parsed.display_name, parsed.size, operands, ctx)
            }
            FpuMnemonicKind::Frestore => {
                self.encode_frestore(&parsed.display_name, parsed.size, operands, ctx)
            }
        })
    }

    fn encode_fmovem(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("FMOVEM expects two operands");
        };

        match (src, dst) {
            (Operand::RegisterList { registers, .. }, dst) => {
                let dst_ea = match self.family.encode_effective_address(dst, size, ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                let predecrement = Self::effective_address_mode(dst_ea.bits) == 4;
                let encoding = match Self::fpu_register_list_encoding(registers, predecrement) {
                    Ok(encoding) => encoding,
                    Err(err) => return err,
                };

                let second_word = match encoding {
                    FmovemRegisterListEncoding::Data(mask) => {
                        if size.is_some() {
                            return EncodeResult::error(
                                "FMOVEM currently supports unsuffixed transfers for FP data-register lists on m68020",
                            );
                        }
                        if !Self::fmovem_register_to_memory_destination(dst_ea.bits) {
                            return EncodeResult::error_with_span(
                                "invalid destination effective address for FMOVEM",
                                dst.span(),
                            );
                        }
                        (if predecrement { 0xE000 } else { 0xF000 }) | mask
                    }
                    FmovemRegisterListEncoding::Control(field) => {
                        if size != Some(OperationSize::Long) {
                            return EncodeResult::error(
                                "FMOVEM control-register-list transfers require a .L suffix on m68020",
                            );
                        }
                        if !Self::fpu_native_destination_operand(dst_ea.bits) {
                            return EncodeResult::error_with_span(
                                "invalid destination effective address for FMOVEM.L",
                                dst.span(),
                            );
                        }
                        0xA000 | field
                    }
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, dst_ea.bits),
                );
                M68KFamilyHandler::emit_word(&mut bytes, second_word);
                bytes.extend_from_slice(&dst_ea.extension);
                EncodeResult::ok(bytes)
            }
            (src, Operand::RegisterList { registers, .. }) => {
                let src_ea = match self.family.encode_effective_address(src, size, ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                let encoding = match Self::fpu_register_list_encoding(registers, false) {
                    Ok(encoding) => encoding,
                    Err(err) => return err,
                };

                let second_word = match encoding {
                    FmovemRegisterListEncoding::Data(mask) => {
                        if size.is_some() {
                            return EncodeResult::error(
                                "FMOVEM currently supports unsuffixed transfers for FP data-register lists on m68020",
                            );
                        }
                        if !Self::fmovem_memory_to_register_source(src_ea.bits) {
                            return EncodeResult::error_with_span(
                                "invalid source effective address for FMOVEM",
                                src.span(),
                            );
                        }
                        0xD000 | mask
                    }
                    FmovemRegisterListEncoding::Control(field) => {
                        if size != Some(OperationSize::Long) {
                            return EncodeResult::error(
                                "FMOVEM control-register-list transfers require a .L suffix on m68020",
                            );
                        }
                        if !Self::fpu_native_source_operand(src_ea.bits) {
                            return EncodeResult::error_with_span(
                                "invalid source effective address for FMOVEM.L",
                                src.span(),
                            );
                        }
                        0x8000 | field
                    }
                };

                let mut bytes = Vec::new();
                M68KFamilyHandler::emit_word(
                    &mut bytes,
                    Self::fpu_effective_address_word(0x0000, src_ea.bits),
                );
                M68KFamilyHandler::emit_word(&mut bytes, second_word);
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

            return self
                .encode_supported_fpu_core_mnemonic(mnemonic, operands, ctx)
                .unwrap_or_else(|| self.deferred_fpu_message(&parsed.display_name, target_name));
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
                MnemonicKind::Bra if parsed.size.is_none() => {
                    return self.encode_auto_sized_branch(&parsed.display_name, 0x0, operands, ctx);
                }
                MnemonicKind::Bsr if parsed.size.is_none() => {
                    return self.encode_auto_sized_branch(&parsed.display_name, 0x1, operands, ctx);
                }
                MnemonicKind::Bcc(condition) if parsed.size.is_none() => {
                    return self.encode_auto_sized_branch(
                        &parsed.display_name,
                        condition.opcode_bits(),
                        operands,
                        ctx,
                    );
                }
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
                MnemonicKind::Bra => {
                    return self
                        .family
                        .encode_branch(&parsed.display_name, None, parsed.size, operands, ctx);
                }
                MnemonicKind::Bsr => {
                    return self
                        .family
                        .encode_branch(&parsed.display_name, None, parsed.size, operands, ctx);
                }
                MnemonicKind::Bcc(condition) => {
                    return self.family.encode_branch(
                        &parsed.display_name,
                        Some(condition),
                        parsed.size,
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
                MnemonicKind::Divs if matches!(parsed.size, Some(OperationSize::Long)) => {
                    return self.family.encode_long_data_register_divide(
                        &parsed.display_name,
                        true,
                        operands,
                        ctx,
                    );
                }
                MnemonicKind::Divu if matches!(parsed.size, Some(OperationSize::Long)) => {
                    return self.family.encode_long_data_register_divide(
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
                M68020MnemonicKind::Divsl => {
                    if !matches!(parsed.size, Some(OperationSize::Long)) {
                        return EncodeResult::error("DIVSL requires an explicit .L size suffix");
                    }
                    if !matches!(operands, [_, Operand::RegisterPair { .. }]) {
                        return EncodeResult::error("DIVSL destination must be a data-register pair");
                    }
                    self.family
                        .encode_long_data_register_divide(&parsed.display_name, true, operands, ctx)
                }
                M68020MnemonicKind::Divul => {
                    if !matches!(parsed.size, Some(OperationSize::Long)) {
                        return EncodeResult::error("DIVUL requires an explicit .L size suffix");
                    }
                    if !matches!(operands, [_, Operand::RegisterPair { .. }]) {
                        return EncodeResult::error("DIVUL destination must be a data-register pair");
                    }
                    self.family.encode_long_data_register_divide(
                        &parsed.display_name,
                        false,
                        operands,
                        ctx,
                    )
                }
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
        current_address: u32,
        symbols: SymbolTable,
    }

    impl TestContext {
        fn with_cpu_state_flag(mut self, key: &str, value: u32) -> Self {
            self.state_flags.insert(key.to_string(), value);
            self
        }

        fn with_current_address(mut self, value: u32) -> Self {
            self.current_address = value;
            self
        }
    }

    impl AssemblerContext for TestContext {
        fn eval_expr(&self, expr: &Expr) -> Result<i64, String> {
            match expr {
                Expr::Number(value, _) => value
                    .parse::<i64>()
                    .map_err(|err| format!("failed to parse test number `{value}`: {err}")),
                _ => Err("unexpected expression evaluation in test".to_string()),
            }
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
            self.current_address
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
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x00, 0x00, 0x80]),
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
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x10, 0xF0, 0x05]),
            other => panic!("expected FMOVEM encoding, got {other:?}"),
        }
    }

    #[test]
    fn arithmetic_and_conversion_fpu_core_encode_for_external_targets_on_m68020() {
        let handler = M68020CpuHandler::new();
        let ctx = TestContext::default()
            .with_cpu_state_flag(crate::families::m68k::state::FPU_TARGET_KEY, 2);

        match handler.encode_instruction(
            "FADD",
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
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x00, 0x00, 0xA2]),
            other => panic!("expected FADD encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FABS",
            &[Operand::FpuDataRegister {
                register: "FP1".to_string(),
                span: Default::default(),
            }],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x00, 0x04, 0x98]),
            other => panic!("expected FABS encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FMOVE.B",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: Default::default(),
                },
                Operand::FpuDataRegister {
                    register: "FP1".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x00, 0x58, 0x80]),
            other => panic!("expected FMOVE.B encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FTST.W",
            &[Operand::DataRegister {
                register: "D0".to_string(),
                span: Default::default(),
            }],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x00, 0x50, 0x3A]),
            other => panic!("expected FTST.W encoding, got {other:?}"),
        }
    }

    #[test]
    fn trig_and_hyperbolic_fpu_slice_encodes_for_external_targets_on_m68020() {
        let handler = M68020CpuHandler::new();
        let ctx = TestContext::default()
            .with_cpu_state_flag(crate::families::m68k::state::FPU_TARGET_KEY, 1)
            .with_current_address(0);

        match handler.encode_instruction(
            "FSIN",
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
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x00, 0x00, 0x8E]),
            other => panic!("expected FSIN encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FCOS.W",
            &[
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: Default::default(),
                },
                Operand::FpuDataRegister {
                    register: "FP1".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x10, 0x50, 0x9D]),
            other => panic!("expected FCOS.W encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FSINCOS",
            &[
                Operand::FpuDataRegister {
                    register: "FP0".to_string(),
                    span: Default::default(),
                },
                Operand::RegisterPair {
                    left: "FP1".to_string(),
                    right: "FP2".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x00, 0x01, 0x31]),
            other => panic!("expected FSINCOS encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FATANH.W",
            &[
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: Default::default(),
                },
                Operand::FpuDataRegister {
                    register: "FP1".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x10, 0x50, 0x8D]),
            other => panic!("expected FATANH encoding, got {other:?}"),
        }
    }

    #[test]
    fn extended_math_fpu_slice_encodes_for_external_targets_on_m68020() {
        let handler = M68020CpuHandler::new();
        let ctx = TestContext::default()
            .with_cpu_state_flag(crate::families::m68k::state::FPU_TARGET_KEY, 1)
            .with_current_address(0);

        match handler.encode_instruction(
            "FETOX",
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
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x00, 0x00, 0x90]),
            other => panic!("expected FETOX encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FLOGN.W",
            &[
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: Default::default(),
                },
                Operand::FpuDataRegister {
                    register: "FP1".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x10, 0x50, 0x94]),
            other => panic!("expected FLOGN.W encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FSCALE",
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
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x00, 0x00, 0xA6]),
            other => panic!("expected FSCALE encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FREM.W",
            &[
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: Default::default(),
                },
                Operand::FpuDataRegister {
                    register: "FP1".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x10, 0x50, 0xA5]),
            other => panic!("expected FREM.W encoding, got {other:?}"),
        }
    }

    #[test]
    fn conditional_and_state_fpu_slice_encodes_for_external_targets_on_m68020() {
        let handler = M68020CpuHandler::new();
        let branch_ctx = TestContext::default()
            .with_cpu_state_flag(crate::families::m68k::state::FPU_TARGET_KEY, 1)
            .with_current_address(0);

        match handler.encode_instruction(
            "FBEQ",
            &[Operand::BranchTarget {
                expr: Expr::Number("4".to_string(), Default::default()),
                span: Default::default(),
            }],
            &branch_ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x81, 0x00, 0x02]),
            other => panic!("expected FBEQ encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FDBNE",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: Default::default(),
                },
                Operand::BranchTarget {
                    expr: Expr::Number("6".to_string(), Default::default()),
                    span: Default::default(),
                },
            ],
            &branch_ctx,
        ) {
            EncodeResult::Ok(bytes) => {
                assert_eq!(bytes, vec![0xF2, 0x48, 0x00, 0x0E, 0x00, 0x02])
            }
            other => panic!("expected FDBNE encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FSNE",
            &[Operand::DataRegister {
                register: "D0".to_string(),
                span: Default::default(),
            }],
            &branch_ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x40, 0x00, 0x0E]),
            other => panic!("expected FSNE encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FTRAPGT.W",
            &[Operand::Immediate {
                expr: Expr::Number("1".to_string(), Default::default()),
                span: Default::default(),
            }],
            &branch_ctx,
        ) {
            EncodeResult::Ok(bytes) => {
                assert_eq!(bytes, vec![0xF2, 0x7A, 0x00, 0x12, 0x00, 0x01])
            }
            other => panic!("expected FTRAPGT.W encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FSAVE",
            &[Operand::AddressIndirect {
                register: "A0".to_string(),
                span: Default::default(),
            }],
            &branch_ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF3, 0x10]),
            other => panic!("expected FSAVE encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FRESTORE",
            &[Operand::AddressPostincrement {
                register: "A0".to_string(),
                span: Default::default(),
            }],
            &branch_ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF3, 0x58]),
            other => panic!("expected FRESTORE encoding, got {other:?}"),
        }
    }

    #[test]
    fn native_format_and_control_state_transfers_encode_on_m68020() {
        let handler = M68020CpuHandler::new();
        let ctx = TestContext::default()
            .with_cpu_state_flag(crate::families::m68k::state::FPU_TARGET_KEY, 1);

        match handler.encode_instruction(
            "FMOVE.S",
            &[
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: Default::default(),
                },
                Operand::FpuDataRegister {
                    register: "FP0".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x10, 0x44, 0x00]),
            other => panic!("expected FMOVE.S encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FADD.X",
            &[
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: Default::default(),
                },
                Operand::FpuDataRegister {
                    register: "FP1".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x10, 0x48, 0xA2]),
            other => panic!("expected FADD.X encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FMOVE.L",
            &[
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: Default::default(),
                },
                Operand::FpuControlRegister {
                    register: FpuControlRegisterKind::Fpcr,
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x10, 0x90, 0x00]),
            other => panic!("expected FMOVE.L memory->FPCR encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FMOVE.L",
            &[
                Operand::FpuControlRegister {
                    register: FpuControlRegisterKind::Fpcr,
                    span: Default::default(),
                },
                Operand::AddressIndirect {
                    register: "A1".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x11, 0xB0, 0x00]),
            other => panic!("expected FMOVE.L FPCR->memory encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FMOVEM.L",
            &[
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: Default::default(),
                },
                Operand::RegisterList {
                    registers: vec![
                        RegisterListRegister::FpuControl(FpuControlRegisterKind::Fpcr),
                        RegisterListRegister::FpuControl(FpuControlRegisterKind::Fpsr),
                        RegisterListRegister::FpuControl(FpuControlRegisterKind::Fpiar),
                    ],
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x10, 0x9C, 0x00]),
            other => panic!("expected FMOVEM.L memory->control-list encoding, got {other:?}"),
        }

        match handler.encode_instruction(
            "FMOVEM.L",
            &[
                Operand::RegisterList {
                    registers: vec![
                        RegisterListRegister::FpuControl(FpuControlRegisterKind::Fpcr),
                        RegisterListRegister::FpuControl(FpuControlRegisterKind::Fpsr),
                        RegisterListRegister::FpuControl(FpuControlRegisterKind::Fpiar),
                    ],
                    span: Default::default(),
                },
                Operand::AddressIndirect {
                    register: "A1".to_string(),
                    span: Default::default(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, vec![0xF2, 0x11, 0xBC, 0x00]),
            other => panic!("expected FMOVEM.L control-list->memory encoding, got {other:?}"),
        }
    }
}
