// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Package compilation adapter for MOS 65x02 scalar semantics.

use package::{
    compile_operand_record_program, compile_value_program, OpcpuCodecError, OperandRecordProgram,
    OperandRecordProgramDescriptor, ValueConstraint, ValueProgramDescriptor, ValueProgramSource,
    OPERAND_RECORD_VM_VERSION_V1, VALUE_VM_OPCODE_VERSION_V1,
};
use types::hierarchy::ScopedOwner;

pub const VALUE_UNSIGNED_BYTE: &str = "scalar.unsigned-byte";
pub const VALUE_UNSIGNED_WORD: &str = "scalar.unsigned-word";
pub const VALUE_LITERAL_ZERO: &str = "scalar.literal-zero";
pub const RECORD_ABSOLUTE_WORD: &str = "operand.absolute-word";
pub const RECORD_IMMEDIATE: &str = "operand.immediate";

fn input_program(id: &str, bits: u8) -> Result<ValueProgramDescriptor, OpcpuCodecError> {
    Ok(ValueProgramDescriptor {
        owner: ScopedOwner::Family("mos6502".to_string()),
        id: id.to_string(),
        opcode_version: VALUE_VM_OPCODE_VERSION_V1,
        program: compile_value_program(
            ValueProgramSource::Input(0),
            &[ValueConstraint::UnsignedBits(bits)],
        )?,
    })
}

/// Compile the reusable scalar ranges owned by the MOS family.
pub fn value_programs() -> Result<Vec<ValueProgramDescriptor>, OpcpuCodecError> {
    Ok(vec![
        input_program(VALUE_UNSIGNED_BYTE, 8)?,
        input_program(VALUE_UNSIGNED_WORD, 16)?,
        ValueProgramDescriptor {
            owner: ScopedOwner::Family("mos6502".to_string()),
            id: VALUE_LITERAL_ZERO.to_string(),
            opcode_version: VALUE_VM_OPCODE_VERSION_V1,
            program: compile_value_program(ValueProgramSource::Literal(0), &[])?,
        },
    ])
}

/// Compile the reusable MOS scalar-address and immediate record shapes.
pub fn operand_record_programs() -> Result<Vec<OperandRecordProgramDescriptor>, OpcpuCodecError> {
    let record = |id: &str, program| -> Result<_, OpcpuCodecError> {
        Ok(OperandRecordProgramDescriptor {
            owner: ScopedOwner::Family("mos6502".to_string()),
            id: id.to_string(),
            schema_version: OPERAND_RECORD_VM_VERSION_V1,
            program: compile_operand_record_program(program)?,
        })
    };
    Ok(vec![
        record(
            RECORD_ABSOLUTE_WORD,
            OperandRecordProgram::Absolute {
                value_input: 0,
                width_bits: 16,
            },
        )?,
        record(
            RECORD_IMMEDIATE,
            OperandRecordProgram::Immediate { value_input: 0 },
        )?,
    ])
}
