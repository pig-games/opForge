// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68000 family handler implementation.

use super::operand::{
    span_from_expr, span_from_exprs, AbsoluteSize, BitFieldSelector, ControlRegisterKind,
    FamilyOperand, FpuControlRegisterKind, FullExtensionBase, FullExtensionIndex, IndexScale,
    IndexSize, MemoryIndirectionKind, Operand, RegisterListRegister, SpecialRegisterKind,
};
use super::table::{
    parse_fpu_mnemonic, parse_m68020_mnemonic, parse_m68080_mnemonic, parse_mnemonic,
    BitFieldMnemonic, BitMnemonic, ConditionCode, FpuMnemonicKind, M68020MnemonicKind,
    M68080MnemonicKind, MnemonicKind, OperationSize, ParsedMnemonic, ShiftMnemonic,
};
use super::{
    is_address_register, is_data_register, is_register, state,
    validate_68080_register_compatibility as validate_68080_register_compatibility_impl,
};
use opcore::expr::parse_number;
use opcore::expression::expr_span;
use opcore::parser::{BinaryOp, Expr, UnaryOp};
use registry::family::{
    expr_has_unstable_symbols, AssemblerContext, EncodeResult, FamilyHandler, FamilyParseError,
};
use std::collections::HashSet;

#[derive(Debug)]
pub struct M68KFamilyHandler {
    max_absolute_address: i64,
}

impl Default for M68KFamilyHandler {
    fn default() -> Self {
        Self::new()
    }
}

const MAX_M68000_ABSOLUTE_ADDRESS: i64 = 0x00FF_FFFF;
type FullExtensionBaseDisplacement = Option<(Expr, AbsoluteSize)>;
type PreindexedIndirectInner = (
    FullExtensionBaseDisplacement,
    FullExtensionBase,
    Option<FullExtensionIndex>,
);
type PostindexedIndirectInner = (FullExtensionBaseDisplacement, FullExtensionBase);

#[derive(Clone, Copy, Debug)]
enum MovemRegisterToken {
    Register(RegisterListRegister, opcore::tokenizer::Span),
    Separator(opcore::tokenizer::Span),
    Range(opcore::tokenizer::Span),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RegisterListFamily {
    Integer,
    Fpu,
}

mod alu_branch;
mod bitfield;
mod control_movem;
mod effective_address;
mod later_family;
mod move_control_flow;
mod operand_parsing;

pub(crate) use effective_address::EffectiveAddressKind;

impl M68KFamilyHandler {
    pub fn new() -> Self {
        Self::new_with_max_absolute_address(MAX_M68000_ABSOLUTE_ADDRESS)
    }

    pub fn new_with_max_absolute_address(max_absolute_address: i64) -> Self {
        Self {
            max_absolute_address,
        }
    }

    pub fn validate_68080_register_compatibility(
        family_operands: &[FamilyOperand],
        ctx: &dyn AssemblerContext,
        cpu_name: &str,
    ) -> Result<(), String> {
        validate_68080_register_compatibility_impl(family_operands, ctx, cpu_name)
    }

    fn is_branch_mnemonic(mnemonic: &str) -> bool {
        matches!(
            parse_mnemonic(mnemonic).map(|parsed| parsed.kind),
            Some(MnemonicKind::Bra | MnemonicKind::Bsr | MnemonicKind::Bcc(_))
        )
    }

    fn is_dbcc_mnemonic(mnemonic: &str) -> bool {
        matches!(
            parse_mnemonic(mnemonic).map(|parsed| parsed.kind),
            Some(MnemonicKind::Dbcc(_))
        )
    }

    fn is_long_divide_mnemonic(mnemonic: &str) -> bool {
        matches!(
            parse_mnemonic(mnemonic),
            Some(parsed)
                if matches!(parsed.kind, MnemonicKind::Divs | MnemonicKind::Divu)
                    && matches!(parsed.size, Some(OperationSize::Long))
        ) || matches!(
            parse_m68020_mnemonic(mnemonic),
            Some(parsed) if matches!(parsed.kind, M68020MnemonicKind::Divsl | M68020MnemonicKind::Divul)
        )
    }

    fn fixed_instruction_dispatch(kind: &MnemonicKind) -> Option<(&'static str, u16)> {
        match kind {
            MnemonicKind::Nop => Some(("NOP", 0x4E71)),
            MnemonicKind::Reset => Some(("RESET", 0x4E70)),
            MnemonicKind::Rte => Some(("RTE", 0x4E73)),
            MnemonicKind::Rtr => Some(("RTR", 0x4E77)),
            MnemonicKind::Trapv => Some(("TRAPV", 0x4E76)),
            MnemonicKind::Illegal => Some(("ILLEGAL", 0x4AFC)),
            _ => None,
        }
    }

    fn encode_instruction_impl(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(parsed) = parse_mnemonic(mnemonic) else {
            return EncodeResult::NotFound;
        };

        if let Some(result) =
            self.try_encode_m68080_extended_short_branch(&parsed, mnemonic, operands, ctx)
        {
            return result;
        }

        if parsed.has_unknown_size_suffix {
            return EncodeResult::error(format!(
                "unsupported size suffix for {}",
                parsed.display_name
            ));
        }

        match parsed.kind {
            MnemonicKind::Move => self.encode_move(parsed.size, operands, ctx),
            MnemonicKind::MoveA => self.encode_movea(parsed.size, operands, ctx),
            MnemonicKind::Movem => self.encode_movem(parsed.size, operands, ctx),
            MnemonicKind::Movep => self.encode_movep(parsed.size, operands, ctx),
            MnemonicKind::Lea => self.encode_lea(parsed.size, operands, ctx),
            MnemonicKind::Pea => self.encode_pea(parsed.size, operands, ctx),
            MnemonicKind::Jmp => self.encode_jmp(parsed.size, operands, ctx),
            MnemonicKind::Jsr => self.encode_jsr(parsed.size, operands, ctx),
            MnemonicKind::Link if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Link => self.encode_link(parsed.size, operands, ctx),
            MnemonicKind::Unlk => self.encode_unlk(parsed.size, operands),
            MnemonicKind::Exg => self.encode_exg(parsed.size, operands),
            MnemonicKind::Swap => self.encode_swap(parsed.size, operands),
            MnemonicKind::Ext => self.encode_ext(parsed.size, operands),
            MnemonicKind::Trap => self.encode_trap(parsed.size, operands, ctx),
            MnemonicKind::Stop => self.encode_stop(parsed.size, operands, ctx),
            kind @ (MnemonicKind::Nop
            | MnemonicKind::Reset
            | MnemonicKind::Rte
            | MnemonicKind::Rtr
            | MnemonicKind::Trapv
            | MnemonicKind::Illegal) => {
                let (display_name, opcode) = Self::fixed_instruction_dispatch(&kind)
                    .expect("fixed-instruction dispatch must cover explicit fixed opcodes");
                self.encode_fixed_instruction(display_name, opcode, parsed.size, operands)
            }
            MnemonicKind::Add => {
                self.encode_data_register_binary_op("ADD", 0xD000, parsed.size, operands, ctx, true)
            }
            MnemonicKind::AddA => {
                self.encode_address_register_binary_op("ADDA", 0xD000, parsed.size, operands, ctx)
            }
            MnemonicKind::Addi => self.encode_immediate_binary_op(
                "ADDI",
                0x0600,
                parsed.size,
                operands,
                ctx,
                Self::data_alterable,
            ),
            MnemonicKind::Addx => {
                self.encode_extend_binary_op("ADDX", 0xD100, parsed.size, operands)
            }
            MnemonicKind::Abcd => {
                self.encode_decimal_adjust_instruction("ABCD", 0xC100, parsed.size, operands)
            }
            MnemonicKind::Chk => self.encode_chk(parsed.size, operands, ctx),
            MnemonicKind::Sub => {
                self.encode_data_register_binary_op("SUB", 0x9000, parsed.size, operands, ctx, true)
            }
            MnemonicKind::SubA => {
                self.encode_address_register_binary_op("SUBA", 0x9000, parsed.size, operands, ctx)
            }
            MnemonicKind::Subi => self.encode_immediate_binary_op(
                "SUBI",
                0x0400,
                parsed.size,
                operands,
                ctx,
                Self::data_alterable,
            ),
            MnemonicKind::Subx => {
                self.encode_extend_binary_op("SUBX", 0x9100, parsed.size, operands)
            }
            MnemonicKind::Sbcd => {
                self.encode_decimal_adjust_instruction("SBCD", 0x8100, parsed.size, operands)
            }
            MnemonicKind::Cmp => self.encode_cmp(parsed.size, operands, ctx),
            MnemonicKind::CmpA => {
                self.encode_address_register_binary_op("CMPA", 0xB000, parsed.size, operands, ctx)
            }
            MnemonicKind::Cmpi => self.encode_immediate_binary_op(
                "CMPI",
                0x0C00,
                parsed.size,
                operands,
                ctx,
                Self::data_addressing,
            ),
            MnemonicKind::Cmpm => self.encode_cmpm(parsed.size, operands),
            MnemonicKind::Addiw
            | MnemonicKind::Cmpiw
            | MnemonicKind::Move2
            | MnemonicKind::Movex
            | MnemonicKind::Moveh
            | MnemonicKind::Moviw
            | MnemonicKind::Mov3q
            | MnemonicKind::Movs
            | MnemonicKind::Movz
            | MnemonicKind::Movz2
            | MnemonicKind::Touch
            | MnemonicKind::Load
            | MnemonicKind::Loadi
            | MnemonicKind::Store
            | MnemonicKind::Storei
            | MnemonicKind::Storec
            | MnemonicKind::Storeilm
            | MnemonicKind::Padd
            | MnemonicKind::Psub
            | MnemonicKind::Pmul88
            | MnemonicKind::Pmulh
            | MnemonicKind::Pmull
            | MnemonicKind::Pmula
            | MnemonicKind::Pand
            | MnemonicKind::Pandn
            | MnemonicKind::Por
            | MnemonicKind::Peor
            | MnemonicKind::Bsel
            | MnemonicKind::Pcmpeqb
            | MnemonicKind::Pcmphib
            | MnemonicKind::Pcmpgeb
            | MnemonicKind::Pcmpgtb
            | MnemonicKind::Pcmpeqw
            | MnemonicKind::Pcmphiw
            | MnemonicKind::Pcmpgew
            | MnemonicKind::Pcmpgtw
            | MnemonicKind::Pack3216
            | MnemonicKind::Packuswb
            | MnemonicKind::Unpack1632
            | MnemonicKind::Vperm => EncodeResult::NotFound,
            MnemonicKind::And => self.encode_data_register_binary_op(
                "AND",
                0xC000,
                parsed.size,
                operands,
                ctx,
                false,
            ),
            MnemonicKind::Andi => self.encode_immediate_binary_op(
                "ANDI",
                0x0200,
                parsed.size,
                operands,
                ctx,
                Self::data_alterable,
            ),
            MnemonicKind::Or => {
                self.encode_data_register_binary_op("OR", 0x8000, parsed.size, operands, ctx, false)
            }
            MnemonicKind::Ori => self.encode_immediate_binary_op(
                "ORI",
                0x0000,
                parsed.size,
                operands,
                ctx,
                Self::data_alterable,
            ),
            MnemonicKind::Eor => self.encode_eor(parsed.size, operands, ctx),
            MnemonicKind::Eori => self.encode_immediate_binary_op(
                "EORI",
                0x0A00,
                parsed.size,
                operands,
                ctx,
                Self::data_alterable,
            ),
            MnemonicKind::Divs if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Divs => {
                self.encode_word_data_register_math("DIVS", 0x81C0, parsed.size, operands, ctx)
            }
            MnemonicKind::Divu if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Divu => {
                self.encode_word_data_register_math("DIVU", 0x80C0, parsed.size, operands, ctx)
            }
            MnemonicKind::Bra | MnemonicKind::Bsr | MnemonicKind::Bcc(_) => EncodeResult::NotFound,
            MnemonicKind::Dbcc(condition) => {
                self.encode_dbcc(&parsed.display_name, condition, parsed.size, operands, ctx)
            }
            MnemonicKind::Rts => self.encode_rts(parsed.size, operands),
            MnemonicKind::Moveq => self.encode_moveq(parsed.size, operands, ctx),
            MnemonicKind::Muls if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Muls => {
                self.encode_word_data_register_math("MULS", 0xC1C0, parsed.size, operands, ctx)
            }
            MnemonicKind::Mulu if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Mulu => {
                self.encode_word_data_register_math("MULU", 0xC0C0, parsed.size, operands, ctx)
            }
            MnemonicKind::Addq => self.encode_quick("ADDQ", false, parsed.size, operands, ctx),
            MnemonicKind::Subq => self.encode_quick("SUBQ", true, parsed.size, operands, ctx),
            MnemonicKind::Bit(kind) => self.encode_bit_op(kind, parsed.size, operands, ctx),
            MnemonicKind::Scc(condition) => {
                self.encode_scc(&parsed.display_name, condition, parsed.size, operands, ctx)
            }
            MnemonicKind::Clr => {
                self.encode_unary_data_instruction("CLR", 0x4200, parsed.size, operands, ctx)
            }
            MnemonicKind::Negx => {
                self.encode_unary_data_instruction("NEGX", 0x4000, parsed.size, operands, ctx)
            }
            MnemonicKind::Neg => {
                self.encode_unary_data_instruction("NEG", 0x4400, parsed.size, operands, ctx)
            }
            MnemonicKind::Nbcd => {
                self.encode_unsized_data_ea_instruction("NBCD", 0x4800, parsed.size, operands, ctx)
            }
            MnemonicKind::Not => {
                self.encode_unary_data_instruction("NOT", 0x4600, parsed.size, operands, ctx)
            }
            MnemonicKind::Tas => {
                self.encode_unsized_data_ea_instruction("TAS", 0x4AC0, parsed.size, operands, ctx)
            }
            MnemonicKind::Tst => {
                self.encode_unary_data_instruction("TST", 0x4A00, parsed.size, operands, ctx)
            }
            MnemonicKind::Shift(kind) => self.encode_shift(kind, parsed.size, operands, ctx),
        }
    }

    fn alu_allows_source(kind: EffectiveAddressKind, size: OperationSize) -> bool {
        match kind {
            EffectiveAddressKind::DataRegister
            | EffectiveAddressKind::AddressIndirect
            | EffectiveAddressKind::AddressPostincrement
            | EffectiveAddressKind::AddressPredecrement
            | EffectiveAddressKind::AddressDisplacement
            | EffectiveAddressKind::AddressIndexed
            | EffectiveAddressKind::PcDisplacement
            | EffectiveAddressKind::PcIndexed
            | EffectiveAddressKind::Absolute
            | EffectiveAddressKind::Immediate => true,
            EffectiveAddressKind::AddressRegister => !matches!(size, OperationSize::Byte),
        }
    }

    pub(crate) fn logic_allows_source(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::DataRegister
                | EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::PcDisplacement
                | EffectiveAddressKind::PcIndexed
                | EffectiveAddressKind::Absolute
                | EffectiveAddressKind::Immediate
        )
    }

    pub(crate) fn data_alterable(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::DataRegister
                | EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    fn data_addressing(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::DataRegister
                | EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::PcDisplacement
                | EffectiveAddressKind::PcIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    fn control_alterable(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    pub(crate) fn memory_alterable(kind: EffectiveAddressKind) -> bool {
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

    fn eor_allows_destination(kind: EffectiveAddressKind) -> bool {
        Self::data_alterable(kind)
    }

    fn bit_field_read_mode(kind: EffectiveAddressKind) -> bool {
        matches!(kind, EffectiveAddressKind::DataRegister) || Self::single_ea_control_mode(kind)
    }

    fn bit_field_write_mode(kind: EffectiveAddressKind) -> bool {
        matches!(kind, EffectiveAddressKind::DataRegister) || Self::control_alterable(kind)
    }

    fn quick_allows_destination(kind: EffectiveAddressKind, size: OperationSize) -> bool {
        Self::data_alterable(kind)
            || (matches!(kind, EffectiveAddressKind::AddressRegister)
                && !matches!(size, OperationSize::Byte))
    }

    fn bit_test_allows_destination(kind: EffectiveAddressKind) -> bool {
        Self::data_addressing(kind)
    }

    fn bit_modify_allows_destination(kind: EffectiveAddressKind) -> bool {
        Self::data_alterable(kind)
    }

    pub(crate) fn size_bits(size: OperationSize) -> u16 {
        match size {
            OperationSize::Byte => 0b00,
            OperationSize::Word => 0b01,
            OperationSize::Long => 0b10,
        }
    }

    fn data_register_opmode(size: OperationSize) -> u16 {
        Self::size_bits(size)
    }

    fn address_register_opmode(size: OperationSize) -> u16 {
        match size {
            OperationSize::Word => 0b011,
            OperationSize::Long => 0b111,
            OperationSize::Byte => unreachable!("handled by caller"),
        }
    }

    fn eor_opmode(size: OperationSize) -> u16 {
        match size {
            OperationSize::Byte => 0b100,
            OperationSize::Word => 0b101,
            OperationSize::Long => 0b110,
        }
    }

    fn memory_destination_opmode(size: OperationSize) -> u16 {
        match size {
            OperationSize::Byte => 0b100,
            OperationSize::Word => 0b101,
            OperationSize::Long => 0b110,
        }
    }

    fn quick_data_bits(value: i64) -> Option<u16> {
        match value {
            1..=7 => Some(value as u16),
            8 => Some(0),
            _ => None,
        }
    }

    fn movem_size_bit(size: OperationSize) -> u16 {
        match size {
            OperationSize::Word => 0,
            OperationSize::Long => 0x0040,
            OperationSize::Byte => unreachable!("handled by caller"),
        }
    }

    fn movem_register_mask(registers: &[RegisterListRegister], predecrement: bool) -> u16 {
        registers.iter().fold(0_u16, |mask, register| {
            let bit = match register {
                RegisterListRegister::Data(reg) => *reg as u16,
                RegisterListRegister::Address(reg) => 8 + *reg as u16,
                RegisterListRegister::FpuData(_) => {
                    unreachable!("integer MOVEM should not receive FPU register lists")
                }
                RegisterListRegister::FpuControl(_) => {
                    unreachable!("integer MOVEM should not receive FPU control-register lists")
                }
            };
            let bit = if predecrement { 15 - bit } else { bit };
            mask | (1_u16 << bit)
        })
    }

    fn single_ea_control_mode(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::PcDisplacement
                | EffectiveAddressKind::PcIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    pub(crate) fn data_register_number(name: &str) -> Option<u8> {
        let suffix = name.strip_prefix('D')?;
        let reg = suffix.parse::<u8>().ok()?;
        (reg <= 7).then_some(reg)
    }

    fn b_register_number(name: &str) -> Option<u8> {
        let upper = name.to_ascii_uppercase();
        let suffix = upper.strip_prefix('B')?;
        let reg = suffix.parse::<u8>().ok()?;
        (reg <= 7).then_some(reg)
    }

    pub(crate) fn fpu_data_register_number(name: &str) -> Option<u8> {
        let suffix = name.strip_prefix("FP")?;
        let reg = suffix.parse::<u8>().ok()?;
        (reg <= 7).then_some(reg)
    }

    pub(crate) fn fpu_banked_data_register_number(name: &str) -> Option<u8> {
        let upper = name.to_ascii_uppercase();
        let suffix = upper.strip_prefix('E')?;
        let reg = suffix.parse::<u8>().ok()?;
        (reg <= 23).then_some(reg)
    }

    pub(crate) fn address_register_number(name: &str) -> Option<u8> {
        if name.eq_ignore_ascii_case("SP") {
            return Some(7);
        }
        let suffix = name.strip_prefix('A')?;
        let reg = suffix.parse::<u8>().ok()?;
        (reg <= 7).then_some(reg)
    }

    fn b_register_direct_operand(operand: &Operand) -> Option<u8> {
        match operand {
            Operand::AddressRegister { register, .. } => Self::b_register_number(register),
            _ => None,
        }
    }

    fn b_register_indirect_operand(operand: &Operand) -> Option<u8> {
        match operand {
            Operand::AddressIndirect { register, .. } => Self::b_register_number(register),
            _ => None,
        }
    }

    pub(crate) fn emit_word(bytes: &mut Vec<u8>, value: u16) {
        bytes.push((value >> 8) as u8);
        bytes.push(value as u8);
    }

    fn emit_long(bytes: &mut Vec<u8>, value: u32) {
        bytes.push((value >> 24) as u8);
        bytes.push((value >> 16) as u8);
        bytes.push((value >> 8) as u8);
        bytes.push(value as u8);
    }

    fn encode_signed_byte(value: i64) -> Option<u8> {
        (-128..=127).contains(&value).then_some((value as i8) as u8)
    }

    fn encode_unsigned_byte(value: i64) -> Option<u8> {
        (0..=u8::MAX as i64).contains(&value).then_some(value as u8)
    }

    pub(crate) fn encode_signed_word(value: i64) -> Option<u16> {
        (-32768..=32767)
            .contains(&value)
            .then_some((value as i16) as u16)
    }

    fn encode_absolute_word(&self, value: i64) -> Option<u16> {
        if !(0..=self.max_absolute_address).contains(&value) {
            return None;
        }

        let encoded = value as u16;
        let sign_extended = ((encoded as i16) as i32 as u32) & (self.max_absolute_address as u32);
        (i64::from(sign_extended) == value).then_some(encoded)
    }

    fn encode_absolute_long(&self, value: i64) -> Option<u32> {
        (0..=self.max_absolute_address)
            .contains(&value)
            .then_some(value as u32)
    }

    fn encode_immediate(size: OperationSize, value: i64) -> Option<Vec<u8>> {
        let mut bytes = Vec::new();
        match size {
            OperationSize::Byte => {
                if !(-128..=255).contains(&value) {
                    return None;
                }
                Self::emit_word(&mut bytes, value as u8 as u16);
            }
            OperationSize::Word => {
                if !(-32768..=65535).contains(&value) {
                    return None;
                }
                Self::emit_word(&mut bytes, value as u16);
            }
            OperationSize::Long => {
                if !(-2_147_483_648..=4_294_967_295).contains(&value) {
                    return None;
                }
                Self::emit_long(&mut bytes, value as u32);
            }
        }
        Some(bytes)
    }

    pub(crate) fn general_register_descriptor(operand: &Operand) -> Option<(u16, u16)> {
        match operand {
            Operand::DataRegister { register, .. } => {
                Some(Self::general_register_name_descriptor(register)?)
            }
            Operand::AddressRegister { register, .. } => {
                Some(Self::general_register_name_descriptor(register)?)
            }
            _ => None,
        }
    }

    fn general_register_name_descriptor(name: &str) -> Option<(u16, u16)> {
        if let Some(reg) = Self::data_register_number(name) {
            return Some((0, reg as u16));
        }
        if let Some(reg) = Self::address_register_number(name) {
            return Some((1, reg as u16));
        }
        None
    }

    fn index_extension_word(index: &str, index_size: IndexSize, displacement: u8) -> Option<u16> {
        let (register, address_bit) = if let Some(reg) = Self::data_register_number(index) {
            (reg, 0_u16)
        } else {
            (Self::address_register_number(index)?, 1_u16)
        };
        let size_bit = match index_size {
            IndexSize::Word => 0_u16,
            IndexSize::Long => 1_u16,
        };
        Some(
            (address_bit << 15)
                | ((register as u16) << 12)
                | (size_bit << 11)
                | displacement as u16,
        )
    }

    fn normalize_wrapped_i32(value: i64) -> i64 {
        if ((i32::MAX as i64) + 1..=u32::MAX as i64).contains(&value) {
            value as u32 as i32 as i64
        } else {
            value
        }
    }

    fn expr_is_unresolved(expr: &Expr, ctx: &dyn AssemblerContext) -> bool {
        ctx.pass() == 1 && expr_has_unstable_symbols(expr, ctx)
    }

    fn expr_has_pc_relative_target_reference(expr: &Expr, ctx: &dyn AssemblerContext) -> bool {
        match expr {
            Expr::Dollar(_) => true,
            Expr::Identifier(name, _) | Expr::Register(name, _) => {
                if ctx.scalar_value_symbol(name).is_some() {
                    return false;
                }
                let Some(entry) = ctx.symbols().entry(name) else {
                    return true;
                };
                !entry.rw
            }
            Expr::Indirect(inner, _) | Expr::Immediate(inner, _) | Expr::IndirectLong(inner, _) => {
                Self::expr_has_pc_relative_target_reference(inner, ctx)
            }
            Expr::List(items, _) | Expr::Tuple(items, _) => items
                .iter()
                .any(|item| Self::expr_has_pc_relative_target_reference(item, ctx)),
            Expr::Index { base, index, .. } => {
                Self::expr_has_pc_relative_target_reference(base, ctx)
                    || Self::expr_has_pc_relative_target_reference(index, ctx)
            }
            Expr::Member { base, .. } => Self::expr_has_pc_relative_target_reference(base, ctx),
            Expr::StructLiteral { fields, .. } => fields
                .iter()
                .any(|(_, value)| Self::expr_has_pc_relative_target_reference(value, ctx)),
            Expr::Call { args, .. } => args
                .iter()
                .any(|arg| Self::expr_has_pc_relative_target_reference(arg, ctx)),
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => {
                Self::expr_has_pc_relative_target_reference(cond, ctx)
                    || Self::expr_has_pc_relative_target_reference(then_expr, ctx)
                    || Self::expr_has_pc_relative_target_reference(else_expr, ctx)
            }
            Expr::Unary { expr, .. } => Self::expr_has_pc_relative_target_reference(expr, ctx),
            Expr::Binary { left, right, .. } => {
                Self::expr_has_pc_relative_target_reference(left, ctx)
                    || Self::expr_has_pc_relative_target_reference(right, ctx)
            }
            Expr::Range {
                start, end, step, ..
            } => {
                Self::expr_has_pc_relative_target_reference(start, ctx)
                    || Self::expr_has_pc_relative_target_reference(end, ctx)
                    || step
                        .as_ref()
                        .is_some_and(|expr| Self::expr_has_pc_relative_target_reference(expr, ctx))
            }
            Expr::Number(_, _) | Expr::String(_, _) | Expr::Placeholder(_) | Expr::Error(_, _) => {
                false
            }
        }
    }

    pub(crate) fn eval_expr_or_placeholder(
        expr: &Expr,
        ctx: &dyn AssemblerContext,
        placeholder: i64,
    ) -> Result<(i64, bool), String> {
        if Self::expr_is_unresolved(expr, ctx) {
            return Ok((placeholder, true));
        }
        ctx.eval_expr(expr)
            .map(|value| (Self::normalize_wrapped_i32(value), false))
    }

    pub(crate) fn eval_expr(expr: &Expr, ctx: &dyn AssemblerContext) -> Result<i64, String> {
        if Self::expr_is_unresolved(expr, ctx) {
            return Ok(0);
        }
        ctx.eval_expr(expr).map(Self::normalize_wrapped_i32)
    }

    fn eval_pc_relative_displacement(
        expr: &Expr,
        ctx: &dyn AssemblerContext,
    ) -> Result<i64, String> {
        if !Self::expr_has_pc_relative_target_reference(expr, ctx) {
            return Self::eval_expr(expr, ctx);
        }

        if Self::expr_is_unresolved(expr, ctx) {
            return Ok(0);
        }

        let target = ctx.eval_expr(expr).map(Self::normalize_wrapped_i32)?;
        Ok(target - (ctx.current_address() as i64 + 2))
    }
}

impl FamilyHandler for M68KFamilyHandler {
    type FamilyOperand = FamilyOperand;
    type Operand = Operand;

    fn parse_operands(
        &self,
        mnemonic: &str,
        exprs: &[Expr],
    ) -> Result<Vec<Self::FamilyOperand>, FamilyParseError> {
        if matches!(
            parse_m68080_mnemonic(mnemonic).map(|parsed| parsed.kind),
            Some(M68080MnemonicKind::Tex)
        ) {
            let [src, dst] = exprs else {
                return Err(FamilyParseError::new(
                    "TEX expects a texture source and a destination data register",
                    exprs.first().map(span_from_expr).unwrap_or_default(),
                ));
            };
            return Ok(vec![
                FamilyOperand::TextureOperand {
                    expr: src.clone(),
                    span: span_from_expr(src),
                },
                self.parse_single_operand(dst)?,
            ]);
        }

        if let Some(parsed) = self.parse_deferred_fpu_operands(mnemonic, exprs) {
            return parsed;
        }

        if matches!(
            parse_mnemonic(mnemonic).map(|parsed| parsed.kind),
            Some(MnemonicKind::Movem)
        ) {
            return self.parse_movem_operands(exprs);
        }

        if Self::is_branch_mnemonic(mnemonic) {
            let [expr] = exprs else {
                return Err(FamilyParseError::new(
                    "68000 branches expect one target operand",
                    exprs.first().map(span_from_expr).unwrap_or_default(),
                ));
            };
            return Ok(vec![FamilyOperand::BranchTarget {
                expr: expr.clone(),
                span: span_from_expr(expr),
            }]);
        }

        if Self::is_dbcc_mnemonic(mnemonic) {
            let [counter, expr] = exprs else {
                return Err(FamilyParseError::new(
                    "68000 DBcc instructions expect a data register and target operand",
                    exprs.first().map(span_from_expr).unwrap_or_default(),
                ));
            };
            let counter = self.parse_single_operand(counter)?;
            return Ok(vec![
                counter,
                FamilyOperand::BranchTarget {
                    expr: expr.clone(),
                    span: span_from_expr(expr),
                },
            ]);
        }

        if Self::is_long_divide_mnemonic(mnemonic) {
            let [src, dst] = exprs else {
                return Err(FamilyParseError::new(
                    "long divide instructions expect two operands",
                    exprs.first().map(span_from_expr).unwrap_or_default(),
                ));
            };

            let src = self.parse_single_operand(src)?;
            let dst = match dst {
                Expr::Binary {
                    op: BinaryOp::Divide,
                    left,
                    right,
                    span,
                } => self.parse_pair_operand(&[*left.clone(), *right.clone()], *span)?,
                _ => self.parse_single_operand(dst)?,
            };
            return Ok(vec![src, dst]);
        }

        exprs
            .iter()
            .map(|expr| self.parse_single_operand(expr))
            .collect()
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[Self::Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_instruction_impl(mnemonic, operands, ctx)
    }

    fn is_register(&self, name: &str) -> bool {
        is_register(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::m68000::M68000CpuHandler;
    use crate::m68020::M68020CpuHandler;
    use crate::m68030::M68030CpuHandler;
    use crate::m68040::M68040CpuHandler;
    use opcore::expression::expr_text;
    use opcore::parser::LineAst;
    use opcore::tokenizer::Span;
    use registry::family::{CpuHandler, FamilyHandler};
    use registry::syntax::{parser_from_line_with_registers, register_checker_from_fn};
    use std::collections::HashMap;
    use types::symbol::{SymbolTable, SymbolTableResult, SymbolVisibility};

    fn span() -> Span {
        Span {
            line: 1,
            col_start: 1,
            col_end: 1,
        }
    }

    fn parse_test_number(text: &str) -> Result<i64, String> {
        if let Some(hex) = text.strip_prefix('$') {
            i64::from_str_radix(hex, 16).map_err(|err| err.to_string())
        } else {
            text.parse::<i64>().map_err(|err| err.to_string())
        }
    }

    fn parse_operand_from_source(source: &str) -> FamilyOperand {
        let mut operands = parse_operands_from_source(source);
        assert_eq!(operands.len(), 2, "expected MOVE source and destination");
        operands.remove(0)
    }

    fn parse_operands_from_source(source: &str) -> Vec<FamilyOperand> {
        let mut parser = parser_from_line_with_registers(
            source,
            1,
            register_checker_from_fn(crate::m68k::is_register),
        )
        .expect("parser");
        let line = parser.parse_compat_mixed_line().expect("line parse");
        let LineAst::Statement(statement) = line else {
            panic!("expected statement, got {line:?}");
        };
        let mnemonic = statement.mnemonic.as_deref().expect("mnemonic");
        M68KFamilyHandler::new()
            .parse_operands(mnemonic, &statement.operands)
            .expect("operand parse")
    }

    fn parse_operand_error_from_source(source: &str) -> FamilyParseError {
        let mut parser = parser_from_line_with_registers(
            source,
            1,
            register_checker_from_fn(crate::m68k::is_register),
        )
        .expect("parser");
        let line = parser.parse_compat_mixed_line().expect("line parse");
        let LineAst::Statement(statement) = line else {
            panic!("expected statement, got {line:?}");
        };
        let mnemonic = statement.mnemonic.as_deref().expect("mnemonic");
        M68KFamilyHandler::new()
            .parse_operands(mnemonic, &statement.operands)
            .expect_err("expected operand parse failure")
    }

    fn assert_full_extension_operand(
        operand: &FamilyOperand,
        expected_base_displacement: Option<(&str, AbsoluteSize)>,
        expected_base: FullExtensionBase,
        expected_index: Option<(&str, IndexSize, IndexScale)>,
        expected_memory_indirection: Option<MemoryIndirectionKind>,
        expected_outer_displacement: Option<(&str, AbsoluteSize)>,
    ) {
        let FamilyOperand::FullExtension {
            base_displacement,
            base,
            index,
            memory_indirection,
            outer_displacement,
            ..
        } = operand
        else {
            panic!("expected full-extension operand, got {operand:?}");
        };

        match (base_displacement.as_ref(), expected_base_displacement) {
            (Some((expr, size)), Some((text, expected_size))) => {
                assert_eq!(expr_text(expr).as_deref(), Some(text));
                assert_eq!(*size, expected_size);
            }
            (None, None) => {}
            other => panic!("unexpected base displacement: {other:?}"),
        }
        assert_eq!(base, &expected_base);
        match (index.as_ref(), expected_index) {
            (Some(actual), Some((register, size, scale))) => {
                assert_eq!(actual.register, register);
                assert_eq!(actual.size, size);
                assert_eq!(actual.scale, scale);
            }
            (None, None) => {}
            other => panic!("unexpected index: {other:?}"),
        }
        assert_eq!(*memory_indirection, expected_memory_indirection);
        match (outer_displacement.as_ref(), expected_outer_displacement) {
            (Some((expr, size)), Some((text, expected_size))) => {
                assert_eq!(expr_text(expr).as_deref(), Some(text));
                assert_eq!(*size, expected_size);
            }
            (None, None) => {}
            other => panic!("unexpected outer displacement: {other:?}"),
        }
    }

    #[derive(Default)]
    struct TestContext {
        values: HashMap<String, i64>,
        scalar_symbols: HashMap<String, i64>,
        state_flags: HashMap<String, u32>,
        symbols: SymbolTable,
        current_address: u32,
        pass: u8,
    }

    impl TestContext {
        fn with_symbol(mut self, name: &str, value: i64) -> Self {
            self.values.insert(name.to_string(), value);
            self
        }

        fn with_symbol_entry(mut self, name: &str, value: i64, rw: bool, updated: bool) -> Self {
            self.values.insert(name.to_string(), value);
            assert_eq!(
                self.symbols
                    .add(name, value as u32, rw, SymbolVisibility::Private, None),
                SymbolTableResult::Ok
            );
            if updated {
                let entry = self.symbols.entry_mut(name).expect("symbol entry");
                entry.updated = true;
            }
            self
        }

        fn with_scalar_value_symbol(mut self, name: &str, value: i64) -> Self {
            self.values.insert(name.to_string(), value);
            self.scalar_symbols.insert(name.to_string(), value);
            self
        }
    }

    impl AssemblerContext for TestContext {
        fn eval_expr(&self, expr: &Expr) -> Result<i64, String> {
            match expr {
                Expr::Number(text, _) => parse_test_number(text),
                Expr::Identifier(name, _) => self
                    .values
                    .get(name)
                    .copied()
                    .ok_or_else(|| format!("unknown symbol '{name}'")),
                _ => Err(format!("unsupported test expression: {expr:?}")),
            }
        }

        fn symbols(&self) -> &SymbolTable {
            &self.symbols
        }

        fn has_symbol(&self, name: &str) -> bool {
            self.values.contains_key(name)
        }

        fn symbol_is_finalized(&self, name: &str) -> Option<bool> {
            self.values.contains_key(name).then_some(true)
        }

        fn current_address(&self) -> u32 {
            self.current_address
        }

        fn pass(&self) -> u8 {
            self.pass
        }

        fn scalar_value_symbol(&self, name: &str) -> Option<i64> {
            self.scalar_symbols.get(name).copied()
        }

        fn cpu_state_flag(&self, key: &str) -> Option<u32> {
            self.state_flags.get(key).copied()
        }
    }

    fn expect_encoded(result: EncodeResult<Vec<u8>>, expected: &[u8]) {
        match result {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, expected),
            other => panic!("expected encoded bytes, got {other:?}"),
        }
    }

    fn expect_effective_address(
        result: Result<super::effective_address::EncodedEffectiveAddress, EncodeResult<Vec<u8>>>,
        expected_bits: u16,
        expected_extension: &[u8],
        expected_kind: EffectiveAddressKind,
    ) {
        match result {
            Ok(encoded) => {
                assert_eq!(encoded.bits, expected_bits);
                assert_eq!(encoded.extension, expected_extension);
                assert_eq!(encoded.kind, expected_kind);
            }
            Err(err) => panic!("expected encoded effective address, got {err:?}"),
        }
    }

    #[test]
    fn effective_address_encodes_register_and_address_forms() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_effective_address(
            handler.encode_effective_address(
                &Operand::DataRegister {
                    register: "D3".to_string(),
                    span: span(),
                },
                Some(OperationSize::Word),
                &ctx,
            ),
            0x0003,
            &[],
            EffectiveAddressKind::DataRegister,
        );

        expect_effective_address(
            handler.encode_effective_address(
                &Operand::AddressRegister {
                    register: "A5".to_string(),
                    span: span(),
                },
                Some(OperationSize::Word),
                &ctx,
            ),
            0x000D,
            &[],
            EffectiveAddressKind::AddressRegister,
        );

        expect_effective_address(
            handler.encode_effective_address(
                &Operand::AddressDisplacement {
                    displacement: Expr::Number("16".to_string(), span()),
                    base: "A2".to_string(),
                    span: span(),
                },
                Some(OperationSize::Word),
                &ctx,
            ),
            0x002A,
            &[0x00, 0x10],
            EffectiveAddressKind::AddressDisplacement,
        );
    }

    #[test]
    fn effective_address_encodes_pc_absolute_and_immediate_forms() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_effective_address(
            handler.encode_effective_address(
                &Operand::PcDisplacement {
                    displacement: Expr::Number("6".to_string(), span()),
                    span: span(),
                },
                Some(OperationSize::Word),
                &ctx,
            ),
            0x003A,
            &[0x00, 0x06],
            EffectiveAddressKind::PcDisplacement,
        );

        expect_effective_address(
            handler.encode_effective_address(
                &Operand::Absolute {
                    expr: Expr::Number("$1234".to_string(), span()),
                    size: AbsoluteSize::Word,
                    span: span(),
                },
                Some(OperationSize::Word),
                &ctx,
            ),
            0x0038,
            &[0x12, 0x34],
            EffectiveAddressKind::Absolute,
        );

        expect_effective_address(
            handler.encode_effective_address(
                &Operand::Immediate {
                    expr: Expr::Number("42".to_string(), span()),
                    span: span(),
                },
                Some(OperationSize::Word),
                &ctx,
            ),
            0x003C,
            &[0x00, 0x2A],
            EffectiveAddressKind::Immediate,
        );
    }

    #[test]
    fn effective_address_encodes_full_extension_forms() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_effective_address(
            handler.encode_effective_address(
                &Operand::FullExtension {
                    base_displacement: Some((
                        Expr::Number("16".to_string(), span()),
                        AbsoluteSize::Word,
                    )),
                    base: FullExtensionBase::Address("A2".to_string()),
                    index: Some(FullExtensionIndex {
                        register: "D3".to_string(),
                        size: IndexSize::Long,
                        scale: IndexScale::Four,
                    }),
                    memory_indirection: Some(MemoryIndirectionKind::Preindexed),
                    outer_displacement: Some((
                        Expr::Number("32".to_string(), span()),
                        AbsoluteSize::Word,
                    )),
                    span: span(),
                },
                None,
                &ctx,
            ),
            0x0032,
            &[0x3D, 0x22, 0x00, 0x10, 0x00, 0x20],
            EffectiveAddressKind::AddressIndexed,
        );

        expect_effective_address(
            handler.encode_effective_address(
                &Operand::FullExtension {
                    base_displacement: None,
                    base: FullExtensionBase::Pc,
                    index: Some(FullExtensionIndex {
                        register: "A1".to_string(),
                        size: IndexSize::Word,
                        scale: IndexScale::Two,
                    }),
                    memory_indirection: None,
                    outer_displacement: None,
                    span: span(),
                },
                None,
                &ctx,
            ),
            0x003B,
            &[0x93, 0x10],
            EffectiveAddressKind::PcIndexed,
        );
    }

    #[test]
    fn parses_baseline_register_operands() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands(
                "MOVE",
                &[
                    Expr::Register("D0".to_string(), span()),
                    Expr::Register("A1".to_string(), span()),
                ],
            )
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::DataRegister { register, .. } if register == "D0"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::AddressRegister { register, .. } if register == "A1"
        ));
    }

    #[test]
    fn pc_relative_scalar_symbols_encode_literal_displacements() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext {
            current_address: 0x1000,
            pass: 2,
            ..Default::default()
        }
        .with_scalar_value_symbol("CONST_DISP", 4)
        .with_scalar_value_symbol("SET_DISP", 2)
        .with_symbol_entry("TARGET", 0x1008, false, true);

        expect_encoded(
            handler.encode_instruction(
                "MOVE.W",
                &[
                    Operand::PcDisplacement {
                        displacement: Expr::Identifier("CONST_DISP".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x30, 0x3A, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE.W",
                &[
                    Operand::PcIndexed {
                        displacement: Expr::Identifier("SET_DISP".to_string(), span()),
                        index: "D1".to_string(),
                        index_size: IndexSize::Word,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x34, 0x3B, 0x10, 0x02],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE.W",
                &[
                    Operand::PcDisplacement {
                        displacement: Expr::Identifier("TARGET".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x30, 0x3A, 0x00, 0x06],
        );
    }

    #[test]
    fn parses_indirect_predec_and_postinc_operands() {
        let handler = M68KFamilyHandler::new();
        let indirect = Expr::Indirect(Box::new(Expr::Register("A0".to_string(), span())), span());
        let operands = handler
            .parse_operands(
                "MOVE",
                &[
                    Expr::Unary {
                        op: UnaryOp::Minus,
                        expr: Box::new(indirect.clone()),
                        span: span(),
                    },
                    Expr::Unary {
                        op: UnaryOp::Plus,
                        expr: Box::new(indirect),
                        span: Span {
                            line: 1,
                            col_start: 1,
                            col_end: 4,
                        },
                    },
                ],
            )
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::AddressPredecrement { register, .. } if register == "A0"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::AddressPostincrement { register, .. } if register == "A0"
        ));
    }

    #[test]
    fn parses_displacement_and_indexed_operands() {
        let handler = M68KFamilyHandler::new();
        let displacement = Expr::Number("4".to_string(), span());
        let indexed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    displacement.clone(),
                    Expr::Register("A0".to_string(), span()),
                    Expr::Identifier("D1.W".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );
        let pc_relative = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    displacement.clone(),
                    Expr::Register("PC".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );

        let operands = handler
            .parse_operands("MOVE", &[indexed, pc_relative])
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::AddressIndexed {
                base,
                index,
                index_size,
                ..
            } if base == "A0" && index == "D1" && *index_size == IndexSize::Word
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::PcDisplacement { displacement: Expr::Number(text, _), .. } if text == "4"
        ));
    }

    #[test]
    fn parses_zero_displacement_indexed_aliases_with_word_defaults() {
        let handler = M68KFamilyHandler::new();
        let address_indexed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Register("A0".to_string(), span()),
                    Expr::Register("D1".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );
        let pc_indexed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Identifier("PC".to_string(), span()),
                    Expr::Identifier("D2".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );

        let operands = handler
            .parse_operands("MOVE", &[address_indexed, pc_indexed])
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::AddressIndexed {
                displacement: Expr::Number(text, _),
                base,
                index,
                index_size,
                ..
            } if text == "0" && base == "A0" && index == "D1" && *index_size == IndexSize::Word
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::PcIndexed {
                displacement: Expr::Number(text, _),
                index,
                index_size,
                ..
            } if text == "0" && index == "D2" && *index_size == IndexSize::Word
        ));
    }

    #[test]
    fn parses_identity_scale_and_pc_shorthand_aliases() {
        let handler = M68KFamilyHandler::new();
        let address_indexed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Number("4".to_string(), span()),
                    Expr::Register("A0".to_string(), span()),
                    Expr::Binary {
                        op: BinaryOp::Multiply,
                        left: Box::new(Expr::Identifier("D1.L".to_string(), span())),
                        right: Box::new(Expr::Number("1".to_string(), span())),
                        span: span(),
                    },
                ],
                span(),
            )),
            span(),
        );
        let pc_indexed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Identifier("PC".to_string(), span()),
                    Expr::Binary {
                        op: BinaryOp::Multiply,
                        left: Box::new(Expr::Identifier("D2".to_string(), span())),
                        right: Box::new(Expr::Number("1".to_string(), span())),
                        span: span(),
                    },
                ],
                span(),
            )),
            span(),
        );
        let pc_shorthand =
            Expr::Indirect(Box::new(Expr::Identifier("PC".to_string(), span())), span());

        let operands = handler
            .parse_operands("MOVE", &[address_indexed, pc_indexed, pc_shorthand])
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::AddressIndexed {
                displacement: Expr::Number(text, _),
                base,
                index,
                index_size,
                ..
            } if text == "4" && base == "A0" && index == "D1" && *index_size == IndexSize::Long
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::PcIndexed {
                displacement: Expr::Number(text, _),
                index,
                index_size,
                ..
            } if text == "0" && index == "D2" && *index_size == IndexSize::Word
        ));
        assert!(matches!(
            &operands[2],
            FamilyOperand::PcDisplacement {
                displacement: Expr::Number(text, _),
                ..
            } if text == "0"
        ));
    }

    #[test]
    fn parses_canonical_68020_full_extension_operands() {
        let handler = M68KFamilyHandler::new();
        let address_full_extension = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Member {
                        base: Box::new(Expr::Number("4".to_string(), span())),
                        field: "W".to_string(),
                        span: span(),
                    },
                    Expr::Register("A0".to_string(), span()),
                    Expr::Binary {
                        op: BinaryOp::Multiply,
                        left: Box::new(Expr::Identifier("D1.L".to_string(), span())),
                        right: Box::new(Expr::Number("4".to_string(), span())),
                        span: span(),
                    },
                ],
                span(),
            )),
            span(),
        );
        let pc_full_extension = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Member {
                        base: Box::new(Expr::Identifier("disp".to_string(), span())),
                        field: "L".to_string(),
                        span: span(),
                    },
                    Expr::Identifier("PC".to_string(), span()),
                    Expr::Identifier("D2".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );
        let base_suppressed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Member {
                        base: Box::new(Expr::Number("8".to_string(), span())),
                        field: "W".to_string(),
                        span: span(),
                    },
                    Expr::Placeholder(span()),
                    Expr::Identifier("D3.W".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );
        let index_suppressed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Placeholder(span()),
                    Expr::Register("A4".to_string(), span()),
                    Expr::Placeholder(span()),
                ],
                span(),
            )),
            span(),
        );

        let operands = handler
            .parse_operands(
                "MOVE",
                &[
                    address_full_extension,
                    pc_full_extension,
                    base_suppressed,
                    index_suppressed,
                ],
            )
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::FullExtension {
                base_displacement: Some((Expr::Number(text, _), AbsoluteSize::Word)),
                base: FullExtensionBase::Address(base),
                index: Some(FullExtensionIndex {
                    register,
                    size: IndexSize::Long,
                    scale: IndexScale::Four,
                }),
                ..
            } if text == "4" && base == "A0" && register == "D1"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::FullExtension {
                base_displacement: Some((Expr::Identifier(name, _), AbsoluteSize::Long)),
                base: FullExtensionBase::Pc,
                index: Some(FullExtensionIndex {
                    register,
                    size: IndexSize::Word,
                    scale: IndexScale::One,
                }),
                ..
            } if name == "disp" && register == "D2"
        ));
        assert!(matches!(
            &operands[2],
            FamilyOperand::FullExtension {
                base_displacement: Some((Expr::Number(text, _), AbsoluteSize::Word)),
                base: FullExtensionBase::Suppressed,
                index: Some(FullExtensionIndex {
                    register,
                    size: IndexSize::Word,
                    scale: IndexScale::One,
                }),
                ..
            } if text == "8" && register == "D3"
        ));
        assert!(matches!(
            &operands[3],
            FamilyOperand::FullExtension {
                base_displacement: None,
                base: FullExtensionBase::Address(base),
                index: None,
                ..
            } if base == "A4"
        ));
    }

    #[test]
    fn rejects_malformed_68020_full_extension_operands_deterministically() {
        let handler = M68KFamilyHandler::new();
        let err = handler
            .parse_operands(
                "MOVE",
                &[Expr::Indirect(
                    Box::new(Expr::Tuple(
                        vec![
                            Expr::Number("4".to_string(), span()),
                            Expr::Placeholder(span()),
                            Expr::Identifier("D1.W".to_string(), span()),
                        ],
                        span(),
                    )),
                    span(),
                )],
            )
            .expect_err("missing explicit full-extension displacement width should fail");
        assert!(err
            .message
            .contains("68020 full-extension base displacement requires explicit .W or .L"));

        let err = handler
            .parse_operands(
                "MOVE",
                &[Expr::Indirect(
                    Box::new(Expr::Tuple(
                        vec![
                            Expr::Member {
                                base: Box::new(Expr::Number("4".to_string(), span())),
                                field: "W".to_string(),
                                span: span(),
                            },
                            Expr::Placeholder(span()),
                            Expr::Placeholder(span()),
                        ],
                        span(),
                    )),
                    span(),
                )],
            )
            .expect_err("suppressing both base and index should fail");
        assert!(err.message.contains("cannot suppress both base and index"));
    }

    #[test]
    fn rejects_non_identity_scaled_index_aliases() {
        let handler = M68KFamilyHandler::new();
        let err = handler
            .parse_operands(
                "MOVE",
                &[Expr::Indirect(
                    Box::new(Expr::Tuple(
                        vec![
                            Expr::Number("4".to_string(), span()),
                            Expr::Register("A0".to_string(), span()),
                            Expr::Binary {
                                op: BinaryOp::Multiply,
                                left: Box::new(Expr::Identifier("D1.W".to_string(), span())),
                                right: Box::new(Expr::Number("2".to_string(), span())),
                                span: span(),
                            },
                        ],
                        span(),
                    )),
                    span(),
                )],
            )
            .expect_err("non-identity scales should stay rejected");

        assert!(err
            .message
            .contains("68020 full-extension base displacement requires explicit .W or .L"));
    }

    #[test]
    fn normalizes_68020_memory_indirect_aliases_to_canonical_operands() {
        let canonical_preindexed = parse_operand_from_source("    MOVE ([,A0,D1.L*4],8.W),D0");
        let alias_preindexed = parse_operand_from_source("    MOVE ([A0,D1.L*4],8.W),D0");
        let canonical_postindexed = parse_operand_from_source("    MOVE ([,A3],D2.W*2,outer.L),D0");
        let alias_postindexed = parse_operand_from_source("    MOVE ([A3],D2.W*2,outer.L),D0");

        assert_full_extension_operand(
            &canonical_preindexed,
            None,
            FullExtensionBase::Address("A0".to_string()),
            Some(("D1", IndexSize::Long, IndexScale::Four)),
            Some(MemoryIndirectionKind::Preindexed),
            Some(("8", AbsoluteSize::Word)),
        );
        assert_full_extension_operand(
            &alias_preindexed,
            None,
            FullExtensionBase::Address("A0".to_string()),
            Some(("D1", IndexSize::Long, IndexScale::Four)),
            Some(MemoryIndirectionKind::Preindexed),
            Some(("8", AbsoluteSize::Word)),
        );
        assert_full_extension_operand(
            &canonical_postindexed,
            None,
            FullExtensionBase::Address("A3".to_string()),
            Some(("D2", IndexSize::Word, IndexScale::Two)),
            Some(MemoryIndirectionKind::Postindexed),
            Some(("outer", AbsoluteSize::Long)),
        );
        assert_full_extension_operand(
            &alias_postindexed,
            None,
            FullExtensionBase::Address("A3".to_string()),
            Some(("D2", IndexSize::Word, IndexScale::Two)),
            Some(MemoryIndirectionKind::Postindexed),
            Some(("outer", AbsoluteSize::Long)),
        );
    }

    #[test]
    fn normalizes_68020_width_explicit_sugar_to_canonical_full_extension_operands() {
        let canonical = parse_operand_from_source("    MOVE (disp.W,A0,D1.L*4),D0");
        let alias = parse_operand_from_source("    MOVE disp.W(A0,D1.L*4),D0");

        assert_full_extension_operand(
            &canonical,
            Some(("disp", AbsoluteSize::Word)),
            FullExtensionBase::Address("A0".to_string()),
            Some(("D1", IndexSize::Long, IndexScale::Four)),
            None,
            None,
        );
        assert_full_extension_operand(
            &alias,
            Some(("disp", AbsoluteSize::Word)),
            FullExtensionBase::Address("A0".to_string()),
            Some(("D1", IndexSize::Long, IndexScale::Four)),
            None,
            None,
        );
    }

    #[test]
    fn rejects_widthless_68020_displacement_leading_sugar_deterministically() {
        let err = parse_operand_error_from_source("    MOVE disp(A0,D1.L*4),D0");
        assert!(err
            .message
            .contains("68020 full-extension base displacement requires explicit .W or .L"));
    }

    #[test]
    fn parses_absolute_and_immediate_operands() {
        let handler = M68KFamilyHandler::new();
        let absolute = Expr::Member {
            base: Box::new(Expr::Indirect(
                Box::new(Expr::Identifier("label".to_string(), span())),
                span(),
            )),
            field: "L".to_string(),
            span: span(),
        };
        let immediate = Expr::Immediate(Box::new(Expr::Number("1".to_string(), span())), span());

        let operands = handler
            .parse_operands("MOVE", &[absolute, immediate])
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::Absolute {
                size: AbsoluteSize::Long,
                expr: Expr::Identifier(name, _),
                ..
            } if name == "label"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::Immediate {
                expr: Expr::Number(text, _),
                ..
            } if text == "1"
        ));
    }

    #[test]
    fn parses_non_parenthesized_absolute_aliases() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands(
                "MOVE",
                &[
                    Expr::Member {
                        base: Box::new(Expr::Identifier("label".to_string(), span())),
                        field: "W".to_string(),
                        span: span(),
                    },
                    Expr::Member {
                        base: Box::new(Expr::Number("$123456".to_string(), span())),
                        field: "L".to_string(),
                        span: span(),
                    },
                ],
            )
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::Absolute {
                size: AbsoluteSize::Word,
                expr: Expr::Identifier(name, _),
                ..
            } if name == "label"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::Absolute {
                size: AbsoluteSize::Long,
                expr: Expr::Number(text, _),
                ..
            } if text == "$123456"
        ));
    }

    #[test]
    fn parses_bare_numeric_absolute_operands_with_inferred_size() {
        let operands = parse_operands_from_source("    MOVE.L $1234,$DFF000");

        assert!(matches!(
            &operands[0],
            FamilyOperand::Absolute {
                size: AbsoluteSize::Word,
                expr: Expr::Number(text, _),
                ..
            } if text == "$1234"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::Absolute {
                size: AbsoluteSize::Long,
                expr: Expr::Number(text, _),
                ..
            } if text == "$DFF000"
        ));
    }

    #[test]
    fn parses_identifier_absolute_suffix_aliases() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands(
                "MOVEA.L",
                &[
                    Expr::Identifier("SysBase.W".to_string(), span()),
                    Expr::Identifier("A6".to_string(), span()),
                ],
            )
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::Absolute {
                size: AbsoluteSize::Word,
                expr: Expr::Identifier(name, _),
                ..
            } if name == "SysBase"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::AddressRegister { register, .. } if register == "A6"
        ));
    }

    #[test]
    fn parses_branch_targets_as_expression_operands() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands("BRA", &[Expr::Identifier("target".to_string(), span())])
            .expect("branch target");

        assert!(matches!(
            &operands[0],
            FamilyOperand::BranchTarget {
                expr: Expr::Identifier(name, _),
                ..
            } if name == "target"
        ));
    }

    #[test]
    fn parses_dbcc_targets_as_expression_operands() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands(
                "DBRA",
                &[
                    Expr::Register("D1".to_string(), span()),
                    Expr::Identifier("loop".to_string(), span()),
                ],
            )
            .expect("dbcc operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::DataRegister { register, .. } if register == "D1"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::BranchTarget {
                expr: Expr::Identifier(name, _),
                ..
            } if name == "loop"
        ));
    }

    #[test]
    fn parses_special_register_operands() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands(
                "MOVE",
                &[
                    Expr::Identifier("SR".to_string(), span()),
                    Expr::Identifier("CCR".to_string(), span()),
                    Expr::Identifier("USP".to_string(), span()),
                ],
            )
            .expect("special register operands");

        assert!(matches!(
            operands[0],
            FamilyOperand::SpecialRegister {
                register: SpecialRegisterKind::Sr,
                ..
            }
        ));
        assert!(matches!(
            operands[1],
            FamilyOperand::SpecialRegister {
                register: SpecialRegisterKind::Ccr,
                ..
            }
        ));
        assert!(matches!(
            operands[2],
            FamilyOperand::SpecialRegister {
                register: SpecialRegisterKind::Usp,
                ..
            }
        ));
    }

    #[test]
    fn parses_extended_control_register_operands() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands(
                "MOVEC",
                &[
                    Expr::Identifier("TC".to_string(), span()),
                    Expr::Identifier("ITT0".to_string(), span()),
                    Expr::Identifier("MMUSR".to_string(), span()),
                    Expr::Identifier("URP".to_string(), span()),
                    Expr::Identifier("SRP".to_string(), span()),
                ],
            )
            .expect("control register operands");

        assert!(matches!(
            operands[0],
            FamilyOperand::ControlRegister {
                register: ControlRegisterKind::Tc,
                ..
            }
        ));
        assert!(matches!(
            operands[1],
            FamilyOperand::ControlRegister {
                register: ControlRegisterKind::Itt0,
                ..
            }
        ));
        assert!(matches!(
            operands[2],
            FamilyOperand::ControlRegister {
                register: ControlRegisterKind::Mmusr,
                ..
            }
        ));
        assert!(matches!(
            operands[3],
            FamilyOperand::ControlRegister {
                register: ControlRegisterKind::Urp,
                ..
            }
        ));
        assert!(matches!(
            operands[4],
            FamilyOperand::ControlRegister {
                register: ControlRegisterKind::Srp,
                ..
            }
        ));
    }

    #[test]
    fn m68040_encodes_extended_movec_registers() {
        let handler = M68040CpuHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "MOVEC",
                &[
                    Operand::ControlRegister {
                        register: ControlRegisterKind::Tc,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x7A, 0x00, 0x03],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEC",
                &[
                    Operand::ControlRegister {
                        register: ControlRegisterKind::Urp,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x7A, 0x88, 0x06],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEC",
                &[
                    Operand::ControlRegister {
                        register: ControlRegisterKind::Mmusr,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x7A, 0x18, 0x05],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEC",
                &[
                    Operand::ControlRegister {
                        register: ControlRegisterKind::Dtt1,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x7A, 0x90, 0x07],
        );
    }

    #[test]
    fn m68010_supports_only_genuine_m68010_extensions() {
        let handler = crate::m68010::M68010CpuHandler::new();

        assert!(!handler.supports_mnemonic("BFTST"));
        assert!(handler.supports_mnemonic("MOVES.W"));
    }

    #[test]
    fn m68010_moves_delegates_to_family_encoder() {
        let handler = crate::m68010::M68010CpuHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "MOVES.W",
                &[
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0E, 0x50, 0x18, 0x00],
        );
    }

    #[test]
    fn parses_fpu_data_register_operands_without_global_register_tokens() {
        let handler = M68KFamilyHandler::new();
        for register in ["FP0", "FP1", "FP2", "FP3", "FP4", "FP5", "FP6", "FP7"] {
            let operands = handler
                .parse_operands(
                    "FMOVE",
                    &[
                        Expr::Identifier(register.to_string(), span()),
                        Expr::Identifier("FP0".to_string(), span()),
                    ],
                )
                .expect("operand parse");
            match &operands[0] {
                FamilyOperand::FpuDataRegister {
                    register: parsed, ..
                } => assert_eq!(parsed, register),
                other => panic!("expected FPU data register operand, got {other:?}"),
            }
        }
    }

    #[test]
    fn parses_banked_68080_fpu_data_register_operands() {
        let handler = M68KFamilyHandler::new();
        for register in ["E0", "E4", "E23"] {
            let operands = handler
                .parse_operands(
                    "FMUL",
                    &[
                        Expr::Identifier(register.to_string(), span()),
                        Expr::Identifier("FP3".to_string(), span()),
                        Expr::Identifier("E5".to_string(), span()),
                    ],
                )
                .expect("operand parse");
            assert!(matches!(
                operands.as_slice(),
                [
                    FamilyOperand::FpuDataRegister { register: src, .. },
                    FamilyOperand::FpuDataRegister { register: mid, .. },
                    FamilyOperand::FpuDataRegister { register: dst, .. },
                ] if src == register && mid == "FP3" && dst == "E5"
            ));
        }
    }

    #[test]
    fn parses_fpu_control_register_operands_without_global_register_tokens() {
        let handler = M68KFamilyHandler::new();
        for (source, expected) in [
            ("FPCR", FpuControlRegisterKind::Fpcr),
            ("FPSR", FpuControlRegisterKind::Fpsr),
            ("FPIAR", FpuControlRegisterKind::Fpiar),
        ] {
            let operands = handler
                .parse_operands(
                    "FMOVE",
                    &[
                        Expr::Identifier(source.to_string(), span()),
                        Expr::Identifier("FP0".to_string(), span()),
                    ],
                )
                .expect("operand parse");
            match &operands[0] {
                FamilyOperand::FpuControlRegister { register, .. } => {
                    assert_eq!(*register, expected)
                }
                other => panic!("expected FPU control register operand, got {other:?}"),
            }
        }
    }

    #[test]
    fn parses_movem_register_list_operands() {
        let handler = M68KFamilyHandler::new();
        let register_list = Expr::Binary {
            op: BinaryOp::Divide,
            left: Box::new(Expr::Binary {
                op: BinaryOp::Subtract,
                left: Box::new(Expr::Register("D0".to_string(), span())),
                right: Box::new(Expr::Register("D2".to_string(), span())),
                span: span(),
            }),
            right: Box::new(Expr::Identifier("A6".to_string(), span())),
            span: span(),
        };
        let indirect = Expr::Indirect(Box::new(Expr::Register("A7".to_string(), span())), span());
        let operands = handler
            .parse_operands(
                "MOVEM.W",
                &[
                    register_list,
                    Expr::Unary {
                        op: UnaryOp::Minus,
                        expr: Box::new(indirect),
                        span: span(),
                    },
                ],
            )
            .expect("movem operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::RegisterList { registers, .. }
                if registers
                    == &[
                        RegisterListRegister::Data(0),
                        RegisterListRegister::Data(1),
                        RegisterListRegister::Data(2),
                        RegisterListRegister::Address(6),
                    ]
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::AddressPredecrement { register, .. } if register == "A7"
        ));
    }

    #[test]
    fn rejects_duplicate_movem_registers() {
        let handler = M68KFamilyHandler::new();
        let duplicate_list = Expr::Binary {
            op: BinaryOp::Divide,
            left: Box::new(Expr::Register("D0".to_string(), span())),
            right: Box::new(Expr::Identifier("D0".to_string(), span())),
            span: span(),
        };

        let err = handler
            .parse_operands(
                "MOVEM.W",
                &[
                    duplicate_list,
                    Expr::Indirect(Box::new(Expr::Register("A0".to_string(), span())), span()),
                ],
            )
            .expect_err("duplicate MOVEM list should fail");
        assert!(err.message.contains("duplicate register in MOVEM list: D0"));
    }

    #[test]
    fn rejects_invalid_index_register_suffix() {
        let handler = M68KFamilyHandler::new();
        let operand = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Number("4".to_string(), span()),
                    Expr::Register("A0".to_string(), span()),
                    Expr::Member {
                        base: Box::new(Expr::Register("D1".to_string(), span())),
                        field: "Q".to_string(),
                        span: span(),
                    },
                ],
                span(),
            )),
            span(),
        );

        let err = handler
            .parse_operands("MOVE", &[operand])
            .expect_err("invalid suffix");
        assert!(err.message.contains("index register"));
    }

    #[test]
    fn m68k_operand_parsing_handles_indexed_alias_source() {
        let operand = parse_operand_from_source("    MOVE (4,A0,D1),D0");

        assert!(matches!(
            operand,
            FamilyOperand::AddressIndexed {
                displacement: Expr::Number(text, _),
                base,
                index,
                index_size: IndexSize::Word,
                ..
            } if text == "4" && base == "A0" && index == "D1"
        ));
    }

    #[test]
    fn m68k_operand_parsing_handles_movem_register_list_source() {
        let operands = parse_operands_from_source("    MOVEM.W D0-D2/A4,(A0)");

        assert!(matches!(
            operands.as_slice(),
            [
                FamilyOperand::RegisterList { registers, .. },
                FamilyOperand::AddressIndirect { register, .. }
            ] if registers
                == &[
                    RegisterListRegister::Data(0),
                    RegisterListRegister::Data(1),
                    RegisterListRegister::Data(2),
                    RegisterListRegister::Address(4),
                ] && register == "A0"
        ));
    }

    #[test]
    fn m68k_operand_parsing_handles_pair_source() {
        let operands = parse_operands_from_source("    FSINCOS FP0,.pair(FP1,FP2)");

        assert!(matches!(
            operands.as_slice(),
            [
                FamilyOperand::FpuDataRegister { register: src, .. },
                FamilyOperand::RegisterPair { left, right, .. },
            ] if src == "FP0" && left == "FP1" && right == "FP2"
        ));
    }

    #[test]
    fn m68k_operand_parsing_handles_register_kind_dispatch() {
        let handler = M68KFamilyHandler::new();

        let data = handler
            .parse_single_operand(&Expr::Register("D0".to_string(), span()))
            .expect("data register");
        let address = handler
            .parse_single_operand(&Expr::Register("A1".to_string(), span()))
            .expect("address register");
        let special = handler
            .parse_single_operand(&Expr::Identifier("SR".to_string(), span()))
            .expect("special register");
        let control = handler
            .parse_single_operand(&Expr::Identifier("VBR".to_string(), span()))
            .expect("control register");
        let fpu_data = handler
            .parse_single_operand(&Expr::Identifier("FP2".to_string(), span()))
            .expect("fpu data register");
        let fpu_control = handler
            .parse_single_operand(&Expr::Identifier("FPCR".to_string(), span()))
            .expect("fpu control register");

        assert!(matches!(
            data,
            FamilyOperand::DataRegister { register, .. } if register == "D0"
        ));
        assert!(matches!(
            address,
            FamilyOperand::AddressRegister { register, .. } if register == "A1"
        ));
        assert!(matches!(
            special,
            FamilyOperand::SpecialRegister {
                register: SpecialRegisterKind::Sr,
                ..
            }
        ));
        assert!(matches!(
            control,
            FamilyOperand::ControlRegister {
                register: ControlRegisterKind::Vbr,
                ..
            }
        ));
        assert!(matches!(
            fpu_data,
            FamilyOperand::FpuDataRegister { register, .. } if register == "FP2"
        ));
        assert!(matches!(
            fpu_control,
            FamilyOperand::FpuControlRegister {
                register: FpuControlRegisterKind::Fpcr,
                ..
            }
        ));
    }

    #[test]
    fn m68k_operand_parsing_handles_full_extension_operand() {
        let handler = M68KFamilyHandler::new();
        let operand = handler
            .parse_single_operand(&Expr::Indirect(
                Box::new(Expr::Tuple(
                    vec![
                        Expr::Member {
                            base: Box::new(Expr::Number("4".to_string(), span())),
                            field: "W".to_string(),
                            span: span(),
                        },
                        Expr::Register("A0".to_string(), span()),
                        Expr::Binary {
                            op: BinaryOp::Multiply,
                            left: Box::new(Expr::Identifier("D1.L".to_string(), span())),
                            right: Box::new(Expr::Number("4".to_string(), span())),
                            span: span(),
                        },
                    ],
                    span(),
                )),
                span(),
            ))
            .expect("full extension operand");

        assert_full_extension_operand(
            &operand,
            Some(("4", AbsoluteSize::Word)),
            FullExtensionBase::Address("A0".to_string()),
            Some(("D1", IndexSize::Long, IndexScale::Four)),
            None,
            None,
        );
    }

    #[test]
    fn m68k_operand_parsing_handles_bitfield_source() {
        let operands = parse_operands_from_source("    BFEXTU ($1234).W{D1:8},D2");
        let FamilyOperand::BitField {
            base,
            offset,
            width,
            ..
        } = &operands[0]
        else {
            panic!("expected bit-field operand, got {:?}", operands[0]);
        };

        assert!(matches!(
            base.as_ref(),
            FamilyOperand::Absolute {
                expr: Expr::Number(text, _),
                size: AbsoluteSize::Word,
                ..
            } if text == "$1234"
        ));
        assert!(matches!(
            offset,
            BitFieldSelector::DataRegister { register, .. } if register == "D1"
        ));
        assert!(matches!(
            width,
            BitFieldSelector::Immediate {
                expr: Expr::Number(text, _),
                ..
            } if text == "8"
        ));
    }

    #[test]
    fn parses_cas2_register_pair_operands() {
        let operands = parse_operands_from_source("    CAS2.L D0:D1,D2:D3,(A0):(A1)");
        assert!(matches!(
            operands.as_slice(),
            [
                FamilyOperand::RegisterPair { left, right, .. },
                FamilyOperand::RegisterPair {
                    left: update_left,
                    right: update_right,
                    ..
                },
                FamilyOperand::IndirectRegisterPair {
                    left: memory_left,
                    right: memory_right,
                    ..
                }
            ] if left == "D0"
                && right == "D1"
                && update_left == "D2"
                && update_right == "D3"
                && memory_left == "A0"
                && memory_right == "A1"
        ));
    }

    #[test]
    fn parses_fpu_register_pair_operands() {
        let operands = parse_operands_from_source("    FSINCOS FP0,.pair(FP1,FP2)");
        assert!(matches!(
            operands.as_slice(),
            [
                FamilyOperand::FpuDataRegister { register: src, .. },
                FamilyOperand::RegisterPair { left, right, .. },
            ] if src == "FP0" && left == "FP1" && right == "FP2"
        ));
    }

    #[test]
    fn parses_bitfield_brace_operands() {
        let operands = parse_operands_from_source("    BFEXTU ($1234).W{D1:8},D2");
        let FamilyOperand::BitField {
            base,
            offset,
            width,
            ..
        } = &operands[0]
        else {
            panic!("expected bit-field operand, got {:?}", operands[0]);
        };
        assert!(matches!(
            base.as_ref(),
            FamilyOperand::Absolute {
                expr: Expr::Number(text, _),
                size: AbsoluteSize::Word,
                ..
            } if text == "$1234"
        ));
        assert!(matches!(
            offset,
            BitFieldSelector::DataRegister { register, .. } if register == "D1"
        ));
        assert!(matches!(
            width,
            BitFieldSelector::Immediate {
                expr: Expr::Number(text, _),
                ..
            } if text == "8"
        ));
    }

    #[test]
    fn rejects_absolute_suffix_on_register_indirect() {
        let handler = M68KFamilyHandler::new();
        let operand = Expr::Member {
            base: Box::new(Expr::Indirect(
                Box::new(Expr::Register("A0".to_string(), span())),
                span(),
            )),
            field: "W".to_string(),
            span: span(),
        };

        let err = handler
            .parse_operands("MOVE", &[operand])
            .expect_err("invalid absolute");
        assert!(err.message.contains("requires an expression"));
    }

    #[test]
    fn encodes_move_word_immediate_to_absolute_short() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();
        let operands = vec![
            Operand::Immediate {
                expr: Expr::Number("$1234".to_string(), span()),
                span: span(),
            },
            Operand::Absolute {
                expr: Expr::Number("$1234".to_string(), span()),
                size: AbsoluteSize::Word,
                span: span(),
            },
        ];

        expect_encoded(
            handler.encode_instruction("MOVE.W", &operands, &ctx),
            &[0x31, 0xFC, 0x12, 0x34, 0x12, 0x34],
        );
    }

    #[test]
    fn encodes_sign_extended_absolute_short_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "MOVE.W",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$FF8000".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x30, 0x38, 0x80, 0x00],
        );

        let invalid_absolute_short = handler.encode_instruction(
            "MOVE.W",
            &[
                Operand::Absolute {
                    expr: Expr::Number("$018000".to_string(), span()),
                    size: AbsoluteSize::Word,
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_absolute_short {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("68000 absolute .W address out of 16-bit range"));
            }
            other => panic!("expected absolute-short range error, got {other:?}"),
        }
    }

    #[test]
    fn encodes_movea_and_control_addressing_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default().with_symbol("target", 0x0012_3456);

        expect_encoded(
            handler.encode_instruction(
                "MOVEA.L",
                &[
                    Operand::Absolute {
                        expr: Expr::Identifier("target".to_string(), span()),
                        size: AbsoluteSize::Long,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x20, 0x79, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "LEA",
                &[
                    Operand::AddressIndexed {
                        displacement: Expr::Number("4".to_string(), span()),
                        base: "A0".to_string(),
                        index: "D1".to_string(),
                        index_size: IndexSize::Word,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x43, 0xF0, 0x10, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "PEA",
                &[Operand::AddressDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    base: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x68, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "JMP",
                &[Operand::Absolute {
                    expr: Expr::Identifier("target".to_string(), span()),
                    size: AbsoluteSize::Long,
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0xF9, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "JSR",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x90],
        );
    }

    #[test]
    fn m68k_move_control_move_basics_and_special_registers() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "MOVE.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$1234".to_string(), span()),
                        span: span(),
                    },
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x31, 0xFC, 0x12, 0x34, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Sr,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x40, 0xC0],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Ccr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x44, 0xF8, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$2700".to_string(), span()),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Sr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x46, 0xFC, 0x27, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Usp,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x69],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::AddressRegister {
                        register: "A2".to_string(),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Usp,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x62],
        );
    }

    #[test]
    fn m68k_move_control_movea_and_control_addressing() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default().with_symbol("target", 0x0012_3456);

        expect_encoded(
            handler.encode_instruction(
                "MOVEA.L",
                &[
                    Operand::Absolute {
                        expr: Expr::Identifier("target".to_string(), span()),
                        size: AbsoluteSize::Long,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x20, 0x79, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "LEA",
                &[
                    Operand::AddressIndexed {
                        displacement: Expr::Number("4".to_string(), span()),
                        base: "A0".to_string(),
                        index: "D1".to_string(),
                        index_size: IndexSize::Word,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x43, 0xF0, 0x10, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "PEA",
                &[Operand::AddressDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    base: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x68, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "JMP",
                &[Operand::Absolute {
                    expr: Expr::Identifier("target".to_string(), span()),
                    size: AbsoluteSize::Long,
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0xF9, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "JSR",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x90],
        );
    }

    #[test]
    fn m68k_move_control_movep_and_register_utilities() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "MOVEP.W",
                &[
                    Operand::DataRegister {
                        register: "D5".to_string(),
                        span: span(),
                    },
                    Operand::AddressDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        base: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0B, 0x89, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEP.L",
                &[
                    Operand::AddressDisplacement {
                        displacement: Expr::Number("6".to_string(), span()),
                        base: "A2".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D6".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0D, 0x4A, 0x00, 0x06],
        );

        expect_encoded(
            handler.encode_instruction(
                "LINK",
                &[
                    Operand::AddressRegister {
                        register: "A6".to_string(),
                        span: span(),
                    },
                    Operand::Immediate {
                        expr: Expr::Number("-8".to_string(), span()),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x56, 0xFF, 0xF8],
        );

        expect_encoded(
            handler.encode_instruction(
                "UNLK",
                &[Operand::AddressRegister {
                    register: "A6".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x5E],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXG",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC1, 0x41],
        );

        expect_encoded(
            handler.encode_instruction(
                "SWAP",
                &[Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x40],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXT.W",
                &[Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x81],
        );

        expect_encoded(
            handler.encode_instruction(
                "TRAP",
                &[Operand::Immediate {
                    expr: Expr::Number("15".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x4F],
        );

        expect_encoded(
            handler.encode_instruction(
                "STOP",
                &[Operand::Immediate {
                    expr: Expr::Number("$2700".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x72, 0x27, 0x00],
        );

        expect_encoded(handler.encode_instruction("RTS", &[], &ctx), &[0x4E, 0x75]);
        expect_encoded(handler.encode_instruction("NOP", &[], &ctx), &[0x4E, 0x71]);
        expect_encoded(
            handler.encode_instruction("ILLEGAL", &[], &ctx),
            &[0x4A, 0xFC],
        );
    }

    #[test]
    fn m68k_move_control_later_cpu_extensions() {
        let m68020 = M68020CpuHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            m68020.encode_instruction(
                "LINK.L",
                &[
                    Operand::AddressRegister {
                        register: "A6".to_string(),
                        span: span(),
                    },
                    Operand::Immediate {
                        expr: Expr::Number("-8".to_string(), span()),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x48, 0x0E, 0xFF, 0xFF, 0xFF, 0xF8],
        );

        expect_encoded(
            m68020.encode_instruction(
                "EXTB.L",
                &[Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x49, 0xC1],
        );

        expect_encoded(
            m68020.encode_instruction(
                "MOVE",
                &[
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Ccr,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x42, 0xC0],
        );
    }

    #[test]
    fn m68k_dispatch_fixed_instructions_encode_expected_opcodes() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        for (mnemonic, expected) in [
            ("NOP", vec![0x4E, 0x71]),
            ("RESET", vec![0x4E, 0x70]),
            ("RTE", vec![0x4E, 0x73]),
            ("RTR", vec![0x4E, 0x77]),
            ("TRAPV", vec![0x4E, 0x76]),
            ("ILLEGAL", vec![0x4A, 0xFC]),
        ] {
            expect_encoded(
                handler.encode_instruction(mnemonic, &[], &ctx),
                expected.as_slice(),
            );
        }
    }

    #[test]
    fn m68k_dispatch_fixed_placeholders_stay_not_found() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        for mnemonic in ["ADDIW.L", "CMPIW.L", "MOVE2.W", "LOADI", "VPERM"] {
            assert!(
                matches!(
                    handler.encode_instruction(mnemonic, &[], &ctx),
                    EncodeResult::NotFound
                ),
                "expected {mnemonic} to stay NotFound"
            );
        }
    }

    #[test]
    fn m68k_alu_branch_core_arithmetic_quick_shift_and_branch() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "ADD.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xD0, 0x7C, 0x00, 0x01],
        );

        expect_encoded(
            handler.encode_instruction(
                "ADDA.L",
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xD3, 0xD0],
        );

        expect_encoded(
            handler.encode_instruction(
                "AND.B",
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC0, 0x10],
        );

        expect_encoded(
            handler.encode_instruction(
                "SUB.L",
                &[
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x90, 0x81],
        );

        expect_encoded(
            handler.encode_instruction(
                "CMP.W",
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xB2, 0x50],
        );

        let invalid_cmp_memory_destination = handler.encode_instruction(
            "CMP.W",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_cmp_memory_destination {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for CMP.W"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        expect_encoded(
            handler.encode_instruction(
                "OR.L",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$12345678".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x84, 0xBC, 0x12, 0x34, 0x56, 0x78],
        );

        expect_encoded(
            handler.encode_instruction(
                "EOR.W",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xB1, 0x51],
        );

        expect_encoded(
            handler.encode_branch(
                "BRA",
                None,
                None,
                &[Operand::BranchTarget {
                    expr: Expr::Number("4".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x60, 0x00, 0x00, 0x02],
        );

        expect_encoded(
            handler.encode_branch(
                "BNE",
                Some(ConditionCode::Ne),
                Some(OperationSize::Word),
                &[Operand::BranchTarget {
                    expr: Expr::Number("8".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x66, 0x00, 0x00, 0x06],
        );

        expect_encoded(
            handler.encode_branch(
                "BSR",
                None,
                Some(OperationSize::Word),
                &[Operand::BranchTarget {
                    expr: Expr::Number("8".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x61, 0x00, 0x00, 0x06],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEQ",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("-1".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x70, 0xFF],
        );

        expect_encoded(
            handler.encode_instruction(
                "ADDQ.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("8".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x50, 0x40],
        );

        expect_encoded(
            handler.encode_instruction(
                "SUBQ.L",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x53, 0x90],
        );

        expect_encoded(
            handler.encode_instruction(
                "ASL.B",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xE3, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "ROR.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xE2, 0x5B],
        );
    }

    #[test]
    fn m68k_alu_branch_memory_destination_forms() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "ADD.W",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xD1, 0x50],
        );

        expect_encoded(
            handler.encode_instruction(
                "SUB.W",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x91, 0x50],
        );

        expect_encoded(
            handler.encode_instruction(
                "AND.W",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC1, 0x50],
        );

        expect_encoded(
            handler.encode_instruction(
                "OR.W",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x81, 0x50],
        );
    }

    #[test]
    fn m68k_alu_branch_immediate_and_unary_forms() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "ORI.B",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$12".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x00, 0x00, 0x00, 0x12],
        );

        expect_encoded(
            handler.encode_instruction(
                "ANDI.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$1234".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x02, 0x50, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "ADDI.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        base: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x06, 0x68, 0x00, 0x01, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "SUBI.B",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressPostincrement {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x04, 0x19, 0x00, 0x01],
        );

        expect_encoded(
            handler.encode_instruction(
                "EORI.L",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$12345678".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0A, 0x81, 0x12, 0x34, 0x56, 0x78],
        );

        expect_encoded(
            handler.encode_instruction(
                "CMPI.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$1234".to_string(), span()),
                        span: span(),
                    },
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0C, 0x78, 0x12, 0x34, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "CMPI.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::PcDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0C, 0x7A, 0x00, 0x01, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "CLR.W",
                &[Operand::DataRegister {
                    register: "D2".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x42, 0x42],
        );

        expect_encoded(
            handler.encode_instruction(
                "NEG.B",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x44, 0x10],
        );

        expect_encoded(
            handler.encode_instruction(
                "NOT.L",
                &[Operand::DataRegister {
                    register: "D3".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x46, 0x83],
        );

        expect_encoded(
            handler.encode_instruction(
                "TST.W",
                &[Operand::AddressDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    base: "A3".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4A, 0x6B, 0x00, 0x04],
        );
    }

    #[test]
    fn m68k_alu_branch_condition_codes_and_dbcc() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "SNE",
                &[Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x56, 0xC0],
        );

        expect_encoded(
            handler.encode_instruction(
                "ST",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x50, 0xD0],
        );

        expect_encoded(
            handler.encode_instruction(
                "SF",
                &[Operand::Absolute {
                    expr: Expr::Number("$1234".to_string(), span()),
                    size: AbsoluteSize::Word,
                    span: span(),
                }],
                &ctx,
            ),
            &[0x51, 0xF8, 0x12, 0x34],
        );

        let loop_ctx = TestContext {
            current_address: 0,
            ..Default::default()
        };
        expect_encoded(
            handler.encode_instruction(
                "DBRA",
                &[
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                    Operand::BranchTarget {
                        expr: Expr::Number("0".to_string(), span()),
                        span: span(),
                    },
                ],
                &loop_ctx,
            ),
            &[0x51, 0xC9, 0xFF, 0xFE],
        );

        expect_encoded(
            handler.encode_instruction(
                "DBNE",
                &[
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                    Operand::BranchTarget {
                        expr: Expr::Number("8".to_string(), span()),
                        span: span(),
                    },
                ],
                &loop_ctx,
            ),
            &[0x56, 0xCA, 0x00, 0x06],
        );
    }

    #[test]
    fn m68k_alu_branch_bit_operations() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "BTST",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("3".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0x00, 0x00, 0x03],
        );

        expect_encoded(
            handler.encode_instruction(
                "BTST",
                &[
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x03, 0x10],
        );

        expect_encoded(
            handler.encode_instruction(
                "BTST",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressDisplacement {
                        base: "A1".to_string(),
                        displacement: Expr::Number("4".to_string(), span()),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0x29, 0x00, 0x01, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "BTST",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::PcDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0x3A, 0x00, 0x01, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "BCHG",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("5".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0x42, 0x00, 0x05],
        );

        expect_encoded(
            handler.encode_instruction(
                "BCHG",
                &[
                    Operand::DataRegister {
                        register: "D3".to_string(),
                        span: span(),
                    },
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x07, 0x78, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "BCLR",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0x91, 0x00, 0x01],
        );

        expect_encoded(
            handler.encode_instruction(
                "BCLR",
                &[
                    Operand::DataRegister {
                        register: "D4".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D5".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x09, 0x85],
        );

        expect_encoded(
            handler.encode_instruction(
                "BSET",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("7".to_string(), span()),
                        span: span(),
                    },
                    Operand::Absolute {
                        expr: Expr::Number("$123456".to_string(), span()),
                        size: AbsoluteSize::Long,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0xF9, 0x00, 0x07, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "BSET",
                &[
                    Operand::DataRegister {
                        register: "D6".to_string(),
                        span: span(),
                    },
                    Operand::AddressDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        base: "A2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0D, 0xEA, 0x00, 0x04],
        );
    }

    #[test]
    fn encodes_multiply_divide_check_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "CHK",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x41, 0xB8, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "MULU.W",
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC2, 0xD0],
        );

        expect_encoded(
            handler.encode_instruction(
                "MULS",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$00FF".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC5, 0xFC, 0x00, 0xFF],
        );

        expect_encoded(
            handler.encode_instruction(
                "DIVU",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$123456".to_string(), span()),
                        size: AbsoluteSize::Long,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x86, 0xF9, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "DIVS.W",
                &[
                    Operand::PcDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D4".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x89, 0xFA, 0x00, 0x04],
        );
    }

    #[test]
    fn encodes_extend_bcd_and_cmpm_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "ADDX.B",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xD3, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "ADDX.W",
                &[
                    Operand::AddressPredecrement {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::AddressPredecrement {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xD3, 0x48],
        );

        expect_encoded(
            handler.encode_instruction(
                "SUBX.L",
                &[
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x97, 0x82],
        );

        expect_encoded(
            handler.encode_instruction(
                "ABCD",
                &[
                    Operand::DataRegister {
                        register: "D4".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D5".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xCB, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "SBCD",
                &[
                    Operand::AddressPredecrement {
                        register: "A2".to_string(),
                        span: span(),
                    },
                    Operand::AddressPredecrement {
                        register: "A3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x87, 0x0A],
        );

        expect_encoded(
            handler.encode_instruction(
                "CMPM.W",
                &[
                    Operand::AddressPostincrement {
                        register: "A4".to_string(),
                        span: span(),
                    },
                    Operand::AddressPostincrement {
                        register: "A5".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xBB, 0x4C],
        );
    }

    #[test]
    fn encodes_rotate_and_memory_shift_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "ROXL.B",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xE3, 0x10],
        );

        expect_encoded(
            handler.encode_instruction(
                "ROXR.W",
                &[
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xE2, 0x72],
        );

        expect_encoded(
            handler.encode_instruction(
                "ASL",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0xE1, 0xD0],
        );

        expect_encoded(
            handler.encode_instruction(
                "LSR.W",
                &[Operand::Absolute {
                    expr: Expr::Number("$1234".to_string(), span()),
                    size: AbsoluteSize::Word,
                    span: span(),
                }],
                &ctx,
            ),
            &[0xE2, 0xF8, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "ROXL",
                &[Operand::AddressDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    base: "A1".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0xE5, 0xE9, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "ROR.W",
                &[Operand::AddressPredecrement {
                    register: "A2".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0xE6, 0xE2],
        );
    }

    #[test]
    fn encodes_movem_and_movep_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "MOVEM.W",
                &[
                    Operand::RegisterList {
                        registers: vec![
                            RegisterListRegister::Data(0),
                            RegisterListRegister::Data(1),
                            RegisterListRegister::Data(2),
                            RegisterListRegister::Address(6),
                        ],
                        span: span(),
                    },
                    Operand::AddressPredecrement {
                        register: "A7".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x48, 0xA7, 0xE0, 0x02],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEM.L",
                &[
                    Operand::AddressPostincrement {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::RegisterList {
                        registers: vec![
                            RegisterListRegister::Data(1),
                            RegisterListRegister::Data(3),
                            RegisterListRegister::Address(2),
                            RegisterListRegister::Address(3),
                            RegisterListRegister::Address(4),
                        ],
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4C, 0xD8, 0x1C, 0x0A],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEP.W",
                &[
                    Operand::DataRegister {
                        register: "D5".to_string(),
                        span: span(),
                    },
                    Operand::AddressDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        base: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0B, 0x89, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEP.L",
                &[
                    Operand::AddressDisplacement {
                        displacement: Expr::Number("6".to_string(), span()),
                        base: "A2".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D6".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0D, 0x4A, 0x00, 0x06],
        );
    }

    #[test]
    fn encodes_system_and_register_utility_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "LINK",
                &[
                    Operand::AddressRegister {
                        register: "A6".to_string(),
                        span: span(),
                    },
                    Operand::Immediate {
                        expr: Expr::Number("-8".to_string(), span()),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x56, 0xFF, 0xF8],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Sr,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x40, 0xC0],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Ccr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x44, 0xF8, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$2700".to_string(), span()),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Sr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x46, 0xFC, 0x27, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Usp,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x69],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::AddressRegister {
                        register: "A2".to_string(),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Usp,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x62],
        );

        expect_encoded(
            handler.encode_instruction(
                "ANDI",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$1F".to_string(), span()),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Ccr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x02, 0x3C, 0x00, 0x1F],
        );

        expect_encoded(
            handler.encode_instruction(
                "ORI",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$2700".to_string(), span()),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Sr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x00, 0x7C, 0x27, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "EORI",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$0F".to_string(), span()),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Ccr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0A, 0x3C, 0x00, 0x0F],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXG",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC1, 0x41],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXG",
                &[
                    Operand::AddressRegister {
                        register: "A2".to_string(),
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC5, 0x4B],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXG",
                &[
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC5, 0x8B],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXG",
                &[
                    Operand::AddressRegister {
                        register: "A3".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC5, 0x8B],
        );

        expect_encoded(
            handler.encode_instruction(
                "SWAP",
                &[Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x40],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXT.W",
                &[Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x81],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXT.L",
                &[Operand::DataRegister {
                    register: "D2".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0xC2],
        );

        expect_encoded(
            handler.encode_instruction(
                "TRAP",
                &[Operand::Immediate {
                    expr: Expr::Number("15".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x4F],
        );

        expect_encoded(handler.encode_instruction("NOP", &[], &ctx), &[0x4E, 0x71]);

        expect_encoded(
            handler.encode_instruction(
                "STOP",
                &[Operand::Immediate {
                    expr: Expr::Number("$2700".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x72, 0x27, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "UNLK",
                &[Operand::AddressRegister {
                    register: "A6".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x5E],
        );

        expect_encoded(
            handler.encode_instruction("RESET", &[], &ctx),
            &[0x4E, 0x70],
        );
        expect_encoded(handler.encode_instruction("RTE", &[], &ctx), &[0x4E, 0x73]);
        expect_encoded(handler.encode_instruction("RTR", &[], &ctx), &[0x4E, 0x77]);
        expect_encoded(
            handler.encode_instruction("TRAPV", &[], &ctx),
            &[0x4E, 0x76],
        );
        expect_encoded(
            handler.encode_instruction("ILLEGAL", &[], &ctx),
            &[0x4A, 0xFC],
        );
    }

    #[test]
    fn encodes_compare_and_operand_state_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "CMPA.W",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xB0, 0xF8, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "CMPA.L",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$123456".to_string(), span()),
                        size: AbsoluteSize::Long,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xB3, 0xF9, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "NEGX.B",
                &[Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x40, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "NEGX.W",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x40, 0x50],
        );

        expect_encoded(
            handler.encode_instruction(
                "NBCD",
                &[Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x01],
        );

        expect_encoded(
            handler.encode_instruction(
                "NBCD",
                &[Operand::AddressPredecrement {
                    register: "A2".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x22],
        );

        expect_encoded(
            handler.encode_instruction(
                "TAS",
                &[Operand::DataRegister {
                    register: "D2".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4A, 0xC2],
        );

        expect_encoded(
            handler.encode_instruction(
                "TAS",
                &[Operand::AddressIndirect {
                    register: "A3".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4A, 0xD3],
        );
    }

    #[test]
    fn rejects_invalid_effective_address_combinations() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        let invalid_move = handler.encode_instruction(
            "MOVE.W",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_move {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for MOVE.W"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_lea = handler.encode_instruction(
            "LEA",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_lea {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid source effective address for LEA"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_addi = handler.encode_instruction(
            "ADDI.W",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_addi {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for ADDI.W"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_tst = handler.encode_instruction(
            "TST.W",
            &[Operand::AddressRegister {
                register: "A0".to_string(),
                span: span(),
            }],
            &ctx,
        );
        match invalid_tst {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for TST.W"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_swap = handler.encode_instruction(
            "SWAP",
            &[Operand::AddressRegister {
                register: "A0".to_string(),
                span: span(),
            }],
            &ctx,
        );
        match invalid_swap {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("SWAP operand must be a data register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_link = handler.encode_instruction(
            "LINK",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_link {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("LINK first operand must be an address register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_trap = handler.encode_instruction(
            "TRAP",
            &[Operand::Immediate {
                expr: Expr::Number("16".to_string(), span()),
                span: span(),
            }],
            &ctx,
        );
        match invalid_trap {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("TRAP vector 16 out of range"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_cmpa = handler.encode_instruction(
            "CMPA.B",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_cmpa {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("does not support .B size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_nbcd = handler.encode_instruction(
            "NBCD",
            &[Operand::AddressRegister {
                register: "A0".to_string(),
                span: span(),
            }],
            &ctx,
        );
        match invalid_nbcd {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for NBCD"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_exg = handler.encode_instruction(
            "EXG",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_exg {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("data/address register pairs"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_sne = handler.encode_instruction(
            "SNE",
            &[Operand::AddressRegister {
                register: "A0".to_string(),
                span: span(),
            }],
            &ctx,
        );
        match invalid_sne {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for SNE"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_dbra = handler.encode_instruction(
            "DBRA",
            &[
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
                Operand::BranchTarget {
                    expr: Expr::Number("0".to_string(), span()),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_dbra {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("counter must be a data register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_move_ccr = handler.encode_instruction(
            "MOVE",
            &[
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Ccr,
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_move_ccr {
            EncodeResult::NotFound => {}
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_move_usp = handler.encode_instruction(
            "MOVE",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Usp,
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_move_usp {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("MOVE USP source must be an address register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_andi_ccr = handler.encode_instruction(
            "ANDI.W",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Ccr,
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_andi_ccr {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("ANDI does not support .W size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_move_to_sr = handler.encode_instruction(
            "MOVE",
            &[
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Sr,
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_move_to_sr {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid source effective address for MOVE to SR"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_btst = handler.encode_instruction(
            "BTST",
            &[
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_btst {
            EncodeResult::Error(message, _) => {
                assert!(
                    message.contains("BTST bit number must be an immediate value or data register")
                );
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_bchg = handler.encode_instruction(
            "BCHG",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::PcDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_bchg {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for BCHG"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_bclr = handler.encode_instruction(
            "BCLR",
            &[
                Operand::Immediate {
                    expr: Expr::Number("-1".to_string(), span()),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_bclr {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("BCLR bit number -1 out of range (0-255)"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_chk = handler.encode_instruction(
            "CHK.L",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_chk {
            EncodeResult::NotFound => {}
            other => panic!("expected cpu-level deferral, got {other:?}"),
        }

        let invalid_mulu = handler.encode_instruction(
            "MULU",
            &[
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_mulu {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid source effective address for MULU"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_divu = handler.encode_instruction(
            "DIVU",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_divu {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("DIVU destination must be a data register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_addx = handler.encode_instruction(
            "ADDX.W",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressPredecrement {
                    register: "A1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_addx {
            EncodeResult::Error(message, _) => {
                assert!(message.contains(
                    "ADDX operands must both be data registers or both be predecrement address operands"
                ));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_abcd = handler.encode_instruction(
            "ABCD.B",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_abcd {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("ABCD does not accept a size suffix"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_cmpm = handler.encode_instruction(
            "CMPM.W",
            &[
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_cmpm {
            EncodeResult::Error(message, _) => {
                assert!(
                    message.contains("CMPM operands must both be postincrement address operands")
                );
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_roxl_memory_size = handler.encode_instruction(
            "ROXL.L",
            &[Operand::AddressIndirect {
                register: "A0".to_string(),
                span: span(),
            }],
            &ctx,
        );
        match invalid_roxl_memory_size {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("ROXL memory form does not support .L size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_roxr_register_shape = handler.encode_instruction(
            "ROXR.W",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_roxr_register_shape {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("ROXR destination must be a data register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_ror_memory_ea = handler.encode_instruction(
            "ROR",
            &[Operand::PcDisplacement {
                displacement: Expr::Number("4".to_string(), span()),
                span: span(),
            }],
            &ctx,
        );
        match invalid_ror_memory_ea {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for ROR"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_movem_size = handler.encode_instruction(
            "MOVEM.B",
            &[
                Operand::RegisterList {
                    registers: vec![RegisterListRegister::Data(0)],
                    span: span(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_movem_size {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("MOVEM does not support .B size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_movem_destination = handler.encode_instruction(
            "MOVEM.W",
            &[
                Operand::RegisterList {
                    registers: vec![RegisterListRegister::Data(0)],
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_movem_destination {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for MOVEM"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_movep_addressing = handler.encode_instruction(
            "MOVEP.W",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_movep_addressing {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("MOVEP memory operand must use d16(An) addressing"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_movep_size = handler.encode_instruction(
            "MOVEP.B",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    base: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_movep_size {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("MOVEP does not support .B size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }
    }

    #[test]
    fn m68k_alu_branch_error_paths() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        match handler.encode_instruction(
            "ADD",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("explicit size suffix"));
            }
            other => panic!("expected ADD size diagnostic, got {other:?}"),
        }

        let pass1_ctx = TestContext {
            pass: 1,
            ..Default::default()
        };
        match handler.encode_branch(
            "BRA",
            None,
            None,
            &[Operand::BranchTarget {
                expr: Expr::Identifier("later".to_string(), span()),
                span: span(),
            }],
            &pass1_ctx,
        ) {
            EncodeResult::Ok(bytes) => {
                assert_eq!(bytes, vec![0x60, 0x00, 0x00, 0x00]);
            }
            other => panic!("expected unresolved BRA word placeholder, got {other:?}"),
        }

        match handler.encode_branch(
            "BRA",
            None,
            Some(OperationSize::Byte),
            &[Operand::BranchTarget {
                expr: Expr::Number("2".to_string(), span()),
                span: span(),
            }],
            &ctx,
        ) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("zero displacement"));
            }
            other => panic!("expected BRA.B zero displacement diagnostic, got {other:?}"),
        }

        match handler.encode_instruction(
            "MOVEQ",
            &[
                Operand::Immediate {
                    expr: Expr::Number("128".to_string(), span()),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("signed 8-bit range"));
            }
            other => panic!("expected MOVEQ range diagnostic, got {other:?}"),
        }

        match handler.encode_instruction(
            "ADDQ.W",
            &[
                Operand::Immediate {
                    expr: Expr::Number("9".to_string(), span()),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("out of range (1-8)"));
            }
            other => panic!("expected ADDQ range diagnostic, got {other:?}"),
        }

        match handler.encode_instruction(
            "ASL.B",
            &[
                Operand::Immediate {
                    expr: Expr::Number("9".to_string(), span()),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("out of range (1-8)"));
            }
            other => panic!("expected ASL range diagnostic, got {other:?}"),
        }
    }

    #[test]
    fn m68k_alu_branch_word_displacements_use_pc_plus_two() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext {
            current_address: 0x1000,
            pass: 2,
            ..Default::default()
        }
        .with_symbol("target", 0x1008);

        expect_encoded(
            handler.encode_branch(
                "BRA",
                None,
                Some(OperationSize::Word),
                &[Operand::BranchTarget {
                    expr: Expr::Identifier("target".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x60, 0x00, 0x00, 0x06],
        );
    }

    #[test]
    fn m68k_alu_branch_long_displacements_use_pc_plus_two() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext {
            current_address: 0x1000,
            pass: 2,
            ..Default::default()
        }
        .with_symbol("target", 0x1008);

        expect_encoded(
            handler.encode_long_branch_instruction(
                "BRA",
                None,
                &[Operand::BranchTarget {
                    expr: Expr::Identifier("target".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x60, 0xFF, 0x00, 0x00, 0x00, 0x06],
        );
    }

    #[test]
    fn long_multiply_single_register_extension_uses_destination_register_bits() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        for dst_reg in 0_u8..=7 {
            expect_encoded(
                handler.encode_long_data_register_multiply(
                    "MULS.L",
                    true,
                    &[
                        Operand::AddressIndirect {
                            register: "A0".to_string(),
                            span: span(),
                        },
                        Operand::DataRegister {
                            register: format!("D{dst_reg}"),
                            span: span(),
                        },
                    ],
                    &ctx,
                ),
                &[0x4C, 0x10, (dst_reg << 4) | 0x08, dst_reg],
            );
        }

        expect_encoded(
            handler.encode_long_data_register_multiply(
                "MULU.L",
                false,
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D5".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4C, 0x10, 0x50, 0x05],
        );
    }

    #[test]
    fn long_divide_extensions_encode_expected_register_bits() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_long_data_register_divide(
                "DIVU.L",
                false,
                false,
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D5".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4C, 0x50, 0x50, 0x05],
        );

        expect_encoded(
            handler.encode_long_data_register_divide(
                "DIVS.L",
                true,
                true,
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::RegisterPair {
                        left: "D1".to_string(),
                        right: "D3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4C, 0x50, 0x3C, 0x01],
        );
    }

    #[test]
    fn absolute_long_addresses_above_24_bits_are_allowed_on_m68020_only() {
        let m68020 = M68020CpuHandler::new();
        let m68000 = M68000CpuHandler::new();
        let ctx = TestContext::default();
        let operands = [
            Operand::Absolute {
                expr: Expr::Number("$01000000".to_string(), span()),
                size: AbsoluteSize::Long,
                span: span(),
            },
            Operand::DataRegister {
                register: "D0".to_string(),
                span: span(),
            },
        ];

        expect_encoded(
            m68020
                .family()
                .encode_instruction("MOVE.L", &operands, &ctx),
            &[0x20, 0x39, 0x01, 0x00, 0x00, 0x00],
        );

        match m68000
            .family()
            .encode_instruction("MOVE.L", &operands, &ctx)
        {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("68000 absolute .L address out of 24-bit range"));
            }
            other => panic!("expected 68000 range error, got {other:?}"),
        }
    }

    #[test]
    fn later_cpus_encode_bkpt_and_rtd() {
        let ctx = TestContext::default();
        fn assert_bkpt_rtd<H: CpuHandler<Family = M68KFamilyHandler>>(
            handler: &H,
            ctx: &TestContext,
        ) {
            expect_encoded(
                handler.encode_instruction(
                    "BKPT",
                    &[Operand::Immediate {
                        expr: Expr::Number("3".to_string(), span()),
                        span: span(),
                    }],
                    ctx,
                ),
                &[0x48, 0x4B],
            );
            expect_encoded(
                handler.encode_instruction(
                    "RTD",
                    &[Operand::Immediate {
                        expr: Expr::Number("-4".to_string(), span()),
                        span: span(),
                    }],
                    ctx,
                ),
                &[0x4E, 0x74, 0xFF, 0xFC],
            );
        }

        assert_bkpt_rtd(&M68020CpuHandler::new(), &ctx);
        assert_bkpt_rtd(&M68030CpuHandler::new(), &ctx);
        assert_bkpt_rtd(&M68040CpuHandler::new(), &ctx);
    }

    #[test]
    fn later_cpus_encode_move_from_ccr() {
        let ctx = TestContext::default();

        fn assert_move_from_ccr<H: CpuHandler<Family = M68KFamilyHandler>>(
            handler: &H,
            ctx: &TestContext,
        ) {
            expect_encoded(
                handler.encode_instruction(
                    "MOVE",
                    &[
                        Operand::SpecialRegister {
                            register: SpecialRegisterKind::Ccr,
                            span: span(),
                        },
                        Operand::DataRegister {
                            register: "D0".to_string(),
                            span: span(),
                        },
                    ],
                    ctx,
                ),
                &[0x42, 0xC0],
            );
        }

        assert_move_from_ccr(&M68020CpuHandler::new(), &ctx);
        assert_move_from_ccr(&M68030CpuHandler::new(), &ctx);
        assert_move_from_ccr(&M68040CpuHandler::new(), &ctx);
    }

    #[test]
    fn chk_long_is_handled_by_m68020_and_deferred_by_baseline() {
        let m68020 = M68020CpuHandler::new();
        let baseline = M68KFamilyHandler::new();
        let m68000 = M68000CpuHandler::new();
        let ctx = TestContext::default();
        let operands = [
            Operand::AddressIndirect {
                register: "A0".to_string(),
                span: span(),
            },
            Operand::DataRegister {
                register: "D1".to_string(),
                span: span(),
            },
        ];

        expect_encoded(
            m68020.encode_instruction("CHK.L", &operands, &ctx),
            &[0x43, 0x10],
        );
        assert!(matches!(
            baseline.encode_instruction("CHK.L", &operands, &ctx),
            EncodeResult::NotFound
        ));
        match m68000.encode_instruction("CHK.L", &operands, &ctx) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("CHK does not support .L size on baseline 68000"));
            }
            other => panic!("expected m68000 CPU-level rejection, got {other:?}"),
        }
    }

    #[test]
    fn m68k_specialized_groups_control_movem_slice() {
        m68010_moves_delegates_to_family_encoder();
        encodes_movem_and_movep_slice();
    }

    #[test]
    fn m68k_specialized_groups_bitfield_slice() {
        parses_bitfield_brace_operands();
        m68k_operand_parsing_handles_bitfield_source();

        let handler = M68020CpuHandler::new();
        let ctx = TestContext::default();
        let bit_field = Operand::BitField {
            base: Box::new(Operand::Absolute {
                expr: Expr::Number("$1234".to_string(), span()),
                size: AbsoluteSize::Word,
                span: span(),
            }),
            offset: BitFieldSelector::DataRegister {
                register: "D1".to_string(),
                span: span(),
            },
            width: BitFieldSelector::Immediate {
                expr: Expr::Number("8".to_string(), span()),
                span: span(),
            },
            span: span(),
        };

        match handler.encode_instruction(
            "BFEXTU",
            &[
                bit_field,
                Operand::DataRegister {
                    register: "D2".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Ok(bytes) => {
                assert_eq!(bytes.len(), 6);
                assert_eq!(&bytes[4..], &[0x12, 0x34]);
            }
            other => panic!("expected BFEXTU encoding, got {other:?}"),
        }
    }

    #[test]
    fn m68k_specialized_groups_later_family_slice() {
        long_multiply_single_register_extension_uses_destination_register_bits();
        long_divide_extensions_encode_expected_register_bits();
        later_cpus_encode_bkpt_and_rtd();
    }

    #[test]
    fn m68k_specialized_groups_deferred_fpu_parse_glue() {
        let handler = M68KFamilyHandler::new();

        let generic = handler
            .parse_operands("FMOVE", &[Expr::Identifier("FP0".to_string(), span())])
            .expect("generic FPU operand");
        assert!(matches!(
            generic.as_slice(),
            [FamilyOperand::FpuDataRegister { register, .. }] if register == "FP0"
        ));

        let fmovem = handler
            .parse_operands(
                "FMOVEM",
                &[
                    Expr::Identifier("FP0".to_string(), span()),
                    Expr::Indirect(Box::new(Expr::Register("A0".to_string(), span())), span()),
                ],
            )
            .expect("FMOVEM operands");
        assert!(matches!(
            fmovem.as_slice(),
            [
                FamilyOperand::RegisterList { registers, .. },
                FamilyOperand::AddressIndirect {
                    register: address_register,
                    ..
                },
            ] if registers == &[RegisterListRegister::FpuData(0)] && address_register == "A0"
        ));

        let fbranch = handler
            .parse_operands("FBEQ", &[Expr::Identifier("target".to_string(), span())])
            .expect("FPU branch target");
        assert!(matches!(
            fbranch.as_slice(),
            [FamilyOperand::BranchTarget {
                expr: Expr::Identifier(name, _),
                ..
            }] if name == "target"
        ));

        let fdbcc = handler
            .parse_operands(
                "FDBEQ",
                &[
                    Expr::Register("D0".to_string(), span()),
                    Expr::Identifier("loop".to_string(), span()),
                ],
            )
            .expect("FPU DBcc operands");
        assert!(matches!(
            fdbcc.as_slice(),
            [
                FamilyOperand::DataRegister { register, .. },
                FamilyOperand::BranchTarget {
                    expr: Expr::Identifier(name, _),
                    ..
                },
            ] if register == "D0" && name == "loop"
        ));
    }
}
