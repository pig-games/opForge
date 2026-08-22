// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68080 CPU module.

use std::collections::HashMap;

use crate::families::m68k::module::{
    M68KFamilyOperands, M68KOperands, DIALECT_MOTOROLA68K, FAMILY_ID as M68K_FAMILY_ID,
};
use crate::families::m68k::parse_m68080_mnemonic;
use crate::families::m68k::state;
use crate::families::m68k::Operand;
use crate::families::m68k::{M68080MnemonicKind, OperationSize};
use opcore::parser::Expr;
use registry::cpu::{CpuFamily, CpuType};
use registry::family::{AssemblerContext, EncodeResult};
use registry::registry::{CpuHandlerDyn, CpuModule, FamilyOperandSet, OperandSet};

use super::M68080CpuHandler;

pub struct M68080CpuModule;

pub const CPU_ID: CpuType = CpuType::new("m68080");
const CPU_ALIASES: &[&str] = &["68080", "mc68080"];

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

fn address_register_number(register: &str) -> Option<u8> {
    if register.eq_ignore_ascii_case("SP") {
        return Some(7);
    }

    register
        .strip_prefix('A')
        .or_else(|| register.strip_prefix('a'))
        .and_then(|suffix| suffix.parse::<u8>().ok())
        .filter(|value| *value <= 7)
}

fn e_register_descriptor(register: &str) -> Option<(u16, u16)> {
    let value = register
        .strip_prefix('E')
        .or_else(|| register.strip_prefix('e'))
        .and_then(|suffix| suffix.parse::<u8>().ok())?;
    (value <= 23).then_some((u16::from(value / 8 + 1), u16::from(value % 8)))
}

fn banked_data_register_descriptor(register: &str) -> Option<(u16, u16)> {
    if let Some(value) = register
        .strip_prefix('D')
        .or_else(|| register.strip_prefix('d'))
        .and_then(|suffix| suffix.parse::<u8>().ok())
        .filter(|value| *value <= 7)
    {
        return Some((0, u16::from(value)));
    }

    e_register_descriptor(register)
}

fn perm_register_descriptor(operand: &Operand) -> Option<(u16, u16)> {
    match operand {
        Operand::DataRegister { register, .. } => banked_data_register_descriptor(register),
        Operand::AddressRegister { register, .. } => {
            Some((0, 8 + u16::from(address_register_number(register)?)))
        }
        _ => None,
    }
}

fn ammx_register_fields(code: u8) -> (u16, u16) {
    (u16::from((code >> 4) & 0x1), u16::from(code & 0x0F))
}

fn emit_word(bytes: &mut Vec<u8>, value: u16) {
    bytes.extend_from_slice(&value.to_be_bytes());
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
    emit_word(bytes, first_word);
    emit_word(bytes, second_word);
    bytes.extend_from_slice(extension);
}

fn ammx_register_operand_code(
    operand: &Operand,
) -> Result<u8, (&'static str, opcore::tokenizer::Span)> {
    let Operand::DataRegister { register, .. } = operand else {
        return Err((
            "AMMX operands must be D0-D7 or E0-E23 data registers",
            operand.span(),
        ));
    };
    let Some(code) = ammx_data_register_name_code(register) else {
        return Err((
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
        ));
    };
    Ok(code)
}

fn ammx_immediate_vea_opcode(mnemonic: &str) -> Option<(u16, Option<OperationSize>)> {
    let parsed = parse_m68080_mnemonic(mnemonic)?;
    match parsed.kind {
        M68080MnemonicKind::Padd => match parsed.size {
            Some(OperationSize::Byte) => Some((0x10, Some(OperationSize::Byte))),
            Some(OperationSize::Word) => Some((0x11, Some(OperationSize::Word))),
            _ => None,
        },
        M68080MnemonicKind::Paddb => {
            (parsed.size.is_none()).then_some((0x10, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Paddw => {
            (parsed.size.is_none()).then_some((0x11, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Paddusb => {
            (parsed.size.is_none()).then_some((0x14, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Paddusw => {
            (parsed.size.is_none()).then_some((0x15, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Psub => match parsed.size {
            Some(OperationSize::Byte) => Some((0x12, Some(OperationSize::Byte))),
            Some(OperationSize::Word) => Some((0x13, Some(OperationSize::Word))),
            _ => None,
        },
        M68080MnemonicKind::Psubb => {
            (parsed.size.is_none()).then_some((0x12, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Psubw => {
            (parsed.size.is_none()).then_some((0x13, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Psubusb => {
            (parsed.size.is_none()).then_some((0x16, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Psubusw => {
            (parsed.size.is_none()).then_some((0x17, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Pavgb => {
            (parsed.size.is_none()).then_some((0x0C, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Pmaxsb => {
            (parsed.size.is_none()).then_some((0x34, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Pmaxub => {
            (parsed.size.is_none()).then_some((0x36, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Pmaxsw => {
            (parsed.size.is_none()).then_some((0x35, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Pmaxuw => {
            (parsed.size.is_none()).then_some((0x37, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Pminsb => {
            (parsed.size.is_none()).then_some((0x30, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Pminub => {
            (parsed.size.is_none()).then_some((0x32, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Pminsw => {
            (parsed.size.is_none()).then_some((0x31, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Pminuw => {
            (parsed.size.is_none()).then_some((0x33, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Pmul88 => (parsed.size.is_none()).then_some((0x18, None)),
        M68080MnemonicKind::Pmulh => (parsed.size.is_none()).then_some((0x1A, None)),
        M68080MnemonicKind::Pmull => (parsed.size.is_none()).then_some((0x1B, None)),
        M68080MnemonicKind::Pmula => (parsed.size.is_none()).then_some((0x19, None)),
        M68080MnemonicKind::Pand => (parsed.size.is_none()).then_some((0x08, None)),
        M68080MnemonicKind::Pandn => (parsed.size.is_none()).then_some((0x0B, None)),
        M68080MnemonicKind::Por => (parsed.size.is_none()).then_some((0x09, None)),
        M68080MnemonicKind::Peor => (parsed.size.is_none()).then_some((0x0A, None)),
        M68080MnemonicKind::Bsel => (parsed.size.is_none()).then_some((0x29, None)),
        M68080MnemonicKind::Pcmpeqb => {
            (parsed.size.is_none()).then_some((0x20, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Pcmphib => {
            (parsed.size.is_none()).then_some((0x22, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Pcmpgeb => {
            (parsed.size.is_none()).then_some((0x2C, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Pcmpgtb => {
            (parsed.size.is_none()).then_some((0x2E, Some(OperationSize::Byte)))
        }
        M68080MnemonicKind::Pcmpeqw => {
            (parsed.size.is_none()).then_some((0x21, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Pcmphiw => {
            (parsed.size.is_none()).then_some((0x23, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Pcmpgew => {
            (parsed.size.is_none()).then_some((0x2D, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Pcmpgtw => {
            (parsed.size.is_none()).then_some((0x2F, Some(OperationSize::Word)))
        }
        M68080MnemonicKind::Lslq => (parsed.size.is_none()).then_some((0x38, None)),
        M68080MnemonicKind::Lsrq => (parsed.size.is_none()).then_some((0x39, None)),
        _ => None,
    }
}

fn try_encode_ammx_immediate_vea(
    mnemonic: &str,
    operands: &[Operand],
    ctx: &dyn AssemblerContext,
) -> Option<EncodeResult<Vec<u8>>> {
    let (opcode, size_context) = ammx_immediate_vea_opcode(mnemonic)?;

    let [src, src_operand, dst_operand] = operands else {
        return None;
    };

    let Operand::Immediate { expr, span } = src else {
        return None;
    };

    let value = match ctx.eval_expr(expr) {
        Ok(value) => value,
        Err(err) => return Some(EncodeResult::error_with_span(err, *span)),
    };
    if !(-32768..=65535).contains(&value) {
        return Some(EncodeResult::error_with_span(
            format!(
                "AMMX {} first operand immediate must fit in 16 bits",
                mnemonic
            ),
            *span,
        ));
    }

    let src_code = match ammx_register_operand_code(src_operand) {
        Ok(code) => code,
        Err((message, span)) => return Some(EncodeResult::error_with_span(message, span)),
    };
    let dst_code = match ammx_register_operand_code(dst_operand) {
        Ok(code) => code,
        Err((message, span)) => return Some(EncodeResult::error_with_span(message, span)),
    };

    let (src_high, src_low) = ammx_register_fields(src_code);
    let (dst_high, dst_low) = ammx_register_fields(dst_code);
    let a_bit = if size_context == Some(OperationSize::Word) {
        1
    } else {
        0
    };

    let mut bytes = Vec::with_capacity(6);
    emit_apollo_two_word_instruction(
        &mut bytes,
        0b111,
        a_bit,
        src_high,
        dst_high,
        0x3C,
        (src_low << 12) | (dst_low << 8) | opcode,
        &(value as u16).to_be_bytes(),
    );
    Some(EncodeResult::ok(bytes))
}

fn try_encode_perm_zero_size_bits(
    mnemonic: &str,
    operands: &[Operand],
    ctx: &dyn AssemblerContext,
) -> Option<EncodeResult<Vec<u8>>> {
    if !mnemonic.eq_ignore_ascii_case("PERM") {
        return None;
    }

    let [selector, left, right] = operands else {
        return None;
    };

    let Operand::Immediate { expr, span } = selector else {
        return Some(EncodeResult::error_with_span(
            "PERM first operand must be an immediate selector",
            selector.span(),
        ));
    };

    let selector_value = match ctx.eval_expr(expr) {
        Ok(value) => value,
        Err(err) => return Some(EncodeResult::error_with_span(err, *span)),
    };
    if !(0..=0x0FFF).contains(&selector_value) {
        return Some(EncodeResult::error_with_span(
            format!("PERM selector {selector_value} out of range (0-4095)"),
            *span,
        ));
    }

    let Some((left_bank_bits, left_code)) = perm_register_descriptor(left) else {
        return Some(EncodeResult::error_with_span(
            "PERM left register must be D0-D7 or A0-A7",
            left.span(),
        ));
    };
    let Some((right_bank_bits, right_code)) = perm_register_descriptor(right) else {
        return Some(EncodeResult::error_with_span(
            "PERM right register must be D0-D7 or A0-A7",
            right.span(),
        ));
    };

    let mut bytes = Vec::with_capacity(if left_bank_bits == 0 && right_bank_bits == 0 {
        4
    } else {
        6
    });
    if left_bank_bits != 0 || right_bank_bits != 0 {
        emit_word(
            &mut bytes,
            0x7100 | ((left_bank_bits & 0x3) << 2) | (right_bank_bits & 0x3),
        );
    }
    emit_word(&mut bytes, 0x4CC0 | left_code);
    emit_word(
        &mut bytes,
        (right_code << 12) | (selector_value as u16 & 0x0FFF),
    );
    Some(EncodeResult::ok(bytes))
}

fn try_encode_load_word_immediate(
    mnemonic: &str,
    operands: &[Operand],
    ctx: &dyn AssemblerContext,
) -> Option<EncodeResult<Vec<u8>>> {
    if !mnemonic.eq_ignore_ascii_case("LOAD.W") {
        return None;
    }

    let [src, dst] = operands else {
        return None;
    };

    let Operand::Immediate { expr, span } = src else {
        return None;
    };

    let value = match ctx.eval_expr(expr) {
        Ok(value) => value,
        Err(err) => return Some(EncodeResult::error_with_span(err, *span)),
    };
    if !(-32768..=65535).contains(&value) {
        return Some(EncodeResult::error_with_span(
            "AMMX LOAD.W immediate source must fit in 16 bits",
            *span,
        ));
    }

    let Operand::DataRegister { register, .. } = dst else {
        return Some(EncodeResult::error_with_span(
            "AMMX operands must be D0-D7 or E0-E23 data registers",
            dst.span(),
        ));
    };
    let Some(dst_code) = ammx_data_register_name_code(register) else {
        return Some(EncodeResult::error_with_span(
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
            dst.span(),
        ));
    };

    let (dst_high, dst_low) = ammx_register_fields(dst_code);
    let first_word = 0xF000 | (0b111_u16 << 9) | (1 << 8) | (dst_high << 6) | 0x3C;
    let second_word = (dst_low << 8) | 0x01;

    let mut bytes = Vec::with_capacity(6);
    emit_word(&mut bytes, first_word);
    emit_word(&mut bytes, second_word);
    emit_word(&mut bytes, value as u16);
    Some(EncodeResult::ok(bytes))
}

impl CpuModule for M68080CpuModule {
    fn cpu_id(&self) -> CpuType {
        CPU_ID
    }

    fn family_id(&self) -> CpuFamily {
        M68K_FAMILY_ID
    }

    fn cpu_name(&self) -> &'static str {
        CPU_ID.as_str()
    }

    fn cpu_aliases(&self) -> &'static [&'static str] {
        CPU_ALIASES
    }

    fn default_dialect(&self) -> &'static str {
        DIALECT_MOTOROLA68K
    }

    fn register_encodings(&self) -> Vec<package::RegisterEncodingDescriptor> {
        super::package_programs::register_encodings()
    }

    fn register_ids(&self) -> &'static [&'static str] {
        super::package_programs::M68080_REGISTER_IDS
    }

    fn runtime_directive_ids(&self) -> &'static [&'static str] {
        state::RUNTIME_DIRECTIVE_IDS
    }

    fn form_mnemonics(&self) -> Vec<String> {
        let mut forms =
            crate::families::m68k::package_programs::m68010_instruction_form_mnemonics();
        forms.extend(
            crate::families::m68k::package_programs::m68020_instruction_form_mnemonics(false),
        );
        forms.extend(super::package_programs::instruction_form_mnemonics());
        forms
    }

    fn value_programs(
        &self,
    ) -> Result<Vec<package::ValueProgramDescriptor>, package::OpcpuCodecError> {
        let mut programs =
            crate::families::m68k::package_programs::m68020_value_programs(CPU_ID.as_str())?;
        programs.extend(super::package_programs::value_programs()?);
        Ok(programs)
    }

    fn instruction_programs(
        &self,
    ) -> Result<Vec<package::VmProgramDescriptor>, package::OpcpuCodecError> {
        let mut programs =
            crate::families::m68k::package_programs::m68010_instruction_programs(CPU_ID.as_str());
        programs.extend(
            crate::families::m68k::package_programs::m68020_instruction_programs(
                CPU_ID.as_str(),
                false,
            ),
        );
        programs.extend(super::package_programs::instruction_programs());
        Ok(programs)
    }

    fn mode_selectors(
        &self,
    ) -> Result<Vec<package::ModeSelectorDescriptor>, package::OpcpuCodecError> {
        let mut selectors =
            crate::families::m68k::package_programs::m68010_mode_selectors(CPU_ID.as_str());
        selectors.extend(
            crate::families::m68k::package_programs::m68020_mode_selectors(CPU_ID.as_str(), false),
        );
        selectors.extend(super::package_programs::mode_selectors());
        Ok(selectors)
    }

    fn diagnostics(&self) -> Vec<package::DiagnosticDescriptor> {
        super::package_programs::diagnostics()
    }

    fn operand_record_programs(
        &self,
    ) -> Result<Vec<package::OperandRecordProgramDescriptor>, package::OpcpuCodecError> {
        super::package_programs::operand_record_programs()
    }

    fn handler(&self) -> Box<dyn CpuHandlerDyn> {
        Box::new(M68080CpuHandler::new())
    }
}

impl CpuHandlerDyn for M68080CpuHandler {
    fn cpu_id(&self) -> CpuType {
        CPU_ID
    }

    fn family_id(&self) -> CpuFamily {
        M68K_FAMILY_ID
    }

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &dyn FamilyOperandSet,
        ctx: &dyn AssemblerContext,
    ) -> Result<Box<dyn OperandSet>, String> {
        let m68k_operands = family_operands
            .as_any()
            .downcast_ref::<M68KFamilyOperands>()
            .ok_or_else(|| "expected Motorola 68000 family operands".to_string())?;
        self.resolve_m68k_operands(mnemonic, &m68k_operands.0, ctx)
            .map(|ops| Box::new(M68KOperands(ops)) as Box<dyn OperandSet>)
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &dyn OperandSet,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let m68k_operands = match operands.as_any().downcast_ref::<M68KOperands>() {
            Some(ops) => ops,
            None => return EncodeResult::error("expected Motorola 68000 operands"),
        };
        if let Some(result) = try_encode_perm_zero_size_bits(mnemonic, &m68k_operands.0, ctx) {
            return result;
        }
        if let Some(result) = try_encode_ammx_immediate_vea(mnemonic, &m68k_operands.0, ctx) {
            return result;
        }
        if let Some(result) = try_encode_load_word_immediate(mnemonic, &m68k_operands.0, ctx) {
            return result;
        }
        M68080CpuHandler::encode_m68k_instruction(self, mnemonic, &m68k_operands.0, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        self.supports_m68k_mnemonic(mnemonic)
    }

    fn max_program_address(&self) -> u32 {
        0xFFFF_FFFF
    }

    fn native_word_size_bytes(&self) -> u32 {
        2
    }

    fn is_little_endian(&self) -> bool {
        false
    }

    fn runtime_state_defaults(&self) -> HashMap<String, u32> {
        state::initial_runtime_state(CPU_ID)
    }

    fn apply_runtime_directive(
        &self,
        directive: &str,
        operands: &[Expr],
        ctx: &dyn AssemblerContext,
        state_flags: &mut HashMap<String, u32>,
    ) -> Result<bool, String> {
        state::apply_runtime_directive(directive, operands, CPU_ID, ctx, state_flags)
    }
}
