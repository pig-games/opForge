// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Package compilation adapter for MOS 65x02 scalar semantics.

use package::{
    compile_encoding_program, compile_fixup_program, compile_operand_record_program,
    compile_value_program, EncodingEndian, EncodingStep, FixupBase, FixupEncodingStep, FixupRange,
    FixupTransform, OpcpuCodecError, OperandRecordProgram, OperandRecordProgramDescriptor,
    PortableRelocationKind, SemanticProgramDescriptor, UnresolvedValuePolicy, ValueConstraint,
    ValueProgramDescriptor, ValueProgramSource, OPERAND_RECORD_VM_VERSION_V1,
    SEMANTIC_VM_OPCODE_VERSION_V2, SEMANTIC_VM_OPCODE_VERSION_V4, VALUE_VM_OPCODE_VERSION_V1,
};
use types::hierarchy::ScopedOwner;

pub const VALUE_UNSIGNED_BYTE: &str = "scalar.unsigned-byte";
pub const VALUE_UNSIGNED_WORD: &str = "scalar.unsigned-word";
pub const VALUE_LITERAL_ZERO: &str = "scalar.literal-zero";
pub const RECORD_ABSOLUTE_WORD: &str = "operand.absolute-word";
pub const RECORD_IMMEDIATE: &str = "operand.immediate";
pub const FIXUP_RELATIVE_BYTE: &str = "fix.rel8";
pub const ENCODING_UNSIGNED_BYTE: &str = "enc.u8";
pub const ENCODING_UNSIGNED_WORD: &str = "enc.u16le";

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

/// Compile the MOS relative-byte projection with the shared neutral fixup VM.
pub fn semantic_programs() -> Result<Vec<SemanticProgramDescriptor>, OpcpuCodecError> {
    let owner = ScopedOwner::Family("mos6502".to_string());
    let scalar = |id: &str, width: u8, max: i64| -> Result<_, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Scalar {
                input: 0,
                width,
                endian: EncodingEndian::Little,
                min: 0,
                max,
            }])?,
        })
    };
    Ok(vec![
        scalar(ENCODING_UNSIGNED_BYTE, 1, 0xff)?,
        scalar(ENCODING_UNSIGNED_WORD, 2, 0xffff)?,
        SemanticProgramDescriptor {
            owner,
            id: FIXUP_RELATIVE_BYTE.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V4,
            program: compile_fixup_program(&[FixupEncodingStep {
                input: 0,
                width: 1,
                endian: EncodingEndian::Little,
                base: FixupBase::Position {
                    adjustment: 2,
                    target_references_only: false,
                },
                range: FixupRange::Signed,
                unresolved: UnresolvedValuePolicy::Placeholder(0),
                relocation: PortableRelocationKind::None,
                transform: FixupTransform::Identity,
            }])?,
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
