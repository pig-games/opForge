// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Package compilation adapter for Motorola 68000 scalar semantics.

use package::{
    compile_operand_record_program, compile_value_program, OpcpuCodecError,
    OperandRecordBaseSource, OperandRecordProgram, OperandRecordProgramDescriptor,
    OperandRecordUpdate, ValueConstraint, ValueProgramDescriptor, ValueProgramSource,
    OPERAND_RECORD_VM_VERSION_V1, VALUE_VM_OPCODE_VERSION_V1,
};
use types::hierarchy::ScopedOwner;

use super::M68KFamilyHandler;

pub const VALUE_NORMALIZED_INPUT: &str = "scalar.normalized-input";
pub const VALUE_SIGNED_BYTE: &str = "scalar.signed-byte";
pub const VALUE_UNSIGNED_BYTE: &str = "scalar.unsigned-byte";
pub const VALUE_SIGNED_WORD: &str = "scalar.signed-word";
pub const VALUE_IMMEDIATE_BYTE: &str = "scalar.immediate-byte";
pub const VALUE_IMMEDIATE_WORD: &str = "scalar.immediate-word";
pub const VALUE_IMMEDIATE_LONG: &str = "scalar.immediate-long";
pub const VALUE_LITERAL_ZERO: &str = "scalar.literal-zero";
pub const SIGNED_BYTE_RANGE: (i64, i64) = (-128, 127);
pub const UNSIGNED_BYTE_RANGE: (i64, i64) = (0, 255);
pub const SIGNED_WORD_RANGE: (i64, i64) = (-32_768, 32_767);
pub const IMMEDIATE_BYTE_RANGE: (i64, i64) = (-128, 255);
pub const IMMEDIATE_WORD_RANGE: (i64, i64) = (-32_768, 65_535);
pub const IMMEDIATE_LONG_RANGE: (i64, i64) = (-2_147_483_648, 4_294_967_295);
pub const RECORD_DATA_REGISTER: &str = "operand.data-register";
pub const RECORD_ADDRESS_REGISTER: &str = "operand.address-register";
pub const RECORD_ADDRESS_INDIRECT: &str = "operand.address-indirect";
pub const RECORD_ADDRESS_POSTINCREMENT: &str = "operand.address-postincrement";
pub const RECORD_ADDRESS_PREDECREMENT: &str = "operand.address-predecrement";
pub const RECORD_ADDRESS_DISPLACEMENT: &str = "operand.address-displacement";
pub const RECORD_ADDRESS_INDEXED_WORD: &str = "operand.address-indexed-word";
pub const RECORD_ADDRESS_INDEXED_LONG: &str = "operand.address-indexed-long";
pub const RECORD_PC_DISPLACEMENT: &str = "operand.pc-displacement";
pub const RECORD_PC_INDEXED_WORD: &str = "operand.pc-indexed-word";
pub const RECORD_PC_INDEXED_LONG: &str = "operand.pc-indexed-long";
pub const RECORD_ABSOLUTE_WORD: &str = "operand.absolute-word";
pub const RECORD_ABSOLUTE_LONG: &str = "operand.absolute-long";
pub const RECORD_IMMEDIATE: &str = "operand.immediate";

/// Convert a family-owned register spelling to the opaque class/index pair
/// consumed by the neutral operand-record runtime.
pub fn compile_register_input(register: &str) -> Option<(u16, u16)> {
    let (prefix, suffix) = register.split_at_checked(1)?;
    let index = suffix.parse::<u16>().ok()?;
    if index > 7 {
        return None;
    }
    match prefix.to_ascii_uppercase().as_str() {
        "D" => Some((0, index)),
        "A" => Some((1, index)),
        _ => None,
    }
}

fn input_program(
    id: &str,
    constraints: &[ValueConstraint],
) -> Result<ValueProgramDescriptor, OpcpuCodecError> {
    Ok(ValueProgramDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        id: id.to_string(),
        opcode_version: VALUE_VM_OPCODE_VERSION_V1,
        program: compile_value_program(ValueProgramSource::Input(0), constraints)?,
    })
}

/// Compile the scalar rules currently owned by the Rust m68k family handler.
pub fn value_programs() -> Result<Vec<ValueProgramDescriptor>, OpcpuCodecError> {
    let normalize = ValueConstraint::NormalizeTwosComplement(32);
    Ok(vec![
        input_program(VALUE_NORMALIZED_INPUT, &[normalize])?,
        input_program(
            VALUE_SIGNED_BYTE,
            &[normalize, ValueConstraint::SignedBits(8)],
        )?,
        input_program(
            VALUE_UNSIGNED_BYTE,
            &[normalize, ValueConstraint::UnsignedBits(8)],
        )?,
        input_program(
            VALUE_SIGNED_WORD,
            &[normalize, ValueConstraint::SignedBits(16)],
        )?,
        input_program(
            VALUE_IMMEDIATE_BYTE,
            &[
                normalize,
                ValueConstraint::InclusiveRange {
                    min: IMMEDIATE_BYTE_RANGE.0,
                    max: IMMEDIATE_BYTE_RANGE.1,
                },
            ],
        )?,
        input_program(
            VALUE_IMMEDIATE_WORD,
            &[
                normalize,
                ValueConstraint::InclusiveRange {
                    min: IMMEDIATE_WORD_RANGE.0,
                    max: IMMEDIATE_WORD_RANGE.1,
                },
            ],
        )?,
        input_program(
            VALUE_IMMEDIATE_LONG,
            &[
                normalize,
                ValueConstraint::InclusiveRange {
                    min: IMMEDIATE_LONG_RANGE.0,
                    max: IMMEDIATE_LONG_RANGE.1,
                },
            ],
        )?,
        ValueProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            id: VALUE_LITERAL_ZERO.to_string(),
            opcode_version: VALUE_VM_OPCODE_VERSION_V1,
            program: compile_value_program(ValueProgramSource::Literal(0), &[])?,
        },
    ])
}

fn record(
    id: &str,
    program: OperandRecordProgram,
) -> Result<OperandRecordProgramDescriptor, OpcpuCodecError> {
    Ok(OperandRecordProgramDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        id: id.to_string(),
        schema_version: OPERAND_RECORD_VM_VERSION_V1,
        program: compile_operand_record_program(program)?,
    })
}

/// Compile base addressing shapes to neutral operand-record constructors.
pub fn operand_record_programs() -> Result<Vec<OperandRecordProgramDescriptor>, OpcpuCodecError> {
    let register = |register_input| OperandRecordBaseSource::Register(register_input);
    let indexed = |base, index_width_bits| OperandRecordProgram::Indexed {
        base,
        index_register_input: 1,
        index_width_bits,
        scale: 1,
        value_input: 0,
    };
    Ok(vec![
        record(
            RECORD_DATA_REGISTER,
            OperandRecordProgram::Register { register_input: 0 },
        )?,
        record(
            RECORD_ADDRESS_REGISTER,
            OperandRecordProgram::Register { register_input: 0 },
        )?,
        record(
            RECORD_ADDRESS_INDIRECT,
            OperandRecordProgram::Indirect {
                register_input: 0,
                update: OperandRecordUpdate::None,
            },
        )?,
        record(
            RECORD_ADDRESS_POSTINCREMENT,
            OperandRecordProgram::Indirect {
                register_input: 0,
                update: OperandRecordUpdate::Postincrement,
            },
        )?,
        record(
            RECORD_ADDRESS_PREDECREMENT,
            OperandRecordProgram::Indirect {
                register_input: 0,
                update: OperandRecordUpdate::Predecrement,
            },
        )?,
        record(
            RECORD_ADDRESS_DISPLACEMENT,
            OperandRecordProgram::Displacement {
                base: register(0),
                value_input: 0,
            },
        )?,
        record(RECORD_ADDRESS_INDEXED_WORD, indexed(register(0), 16))?,
        record(RECORD_ADDRESS_INDEXED_LONG, indexed(register(0), 32))?,
        record(
            RECORD_PC_DISPLACEMENT,
            OperandRecordProgram::Displacement {
                base: OperandRecordBaseSource::ProgramCounter,
                value_input: 0,
            },
        )?,
        record(
            RECORD_PC_INDEXED_WORD,
            indexed(OperandRecordBaseSource::ProgramCounter, 16),
        )?,
        record(
            RECORD_PC_INDEXED_LONG,
            indexed(OperandRecordBaseSource::ProgramCounter, 32),
        )?,
        record(
            RECORD_ABSOLUTE_WORD,
            OperandRecordProgram::Absolute {
                value_input: 0,
                width_bits: 16,
            },
        )?,
        record(
            RECORD_ABSOLUTE_LONG,
            OperandRecordProgram::Absolute {
                value_input: 0,
                width_bits: 32,
            },
        )?,
        record(
            RECORD_IMMEDIATE,
            OperandRecordProgram::Immediate { value_input: 0 },
        )?,
    ])
}

/// Existing family scalar normalization retained as the differential oracle
/// while package-first conversion is incomplete.
pub fn oracle_normalize_wrapped_i32(value: i64) -> i64 {
    M68KFamilyHandler::normalize_wrapped_i32(value)
}
