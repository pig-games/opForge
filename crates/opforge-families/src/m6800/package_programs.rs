// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Package compilation adapter for Motorola 6800-family scalar, structured
//! register-pair, and deferred relative-value semantics.

use package::{
    compile_encoding_program, compile_fixup_program, compile_operand_record_program,
    compile_structured_encoding_program, compile_value_program, EncodingEndian, EncodingStep,
    FixupBase, FixupEncodingStep, FixupRange, OpcpuCodecError, OperandRecordProgram,
    OperandRecordProgramDescriptor, PortableRelocationKind, SemanticProgramDescriptor,
    StructuredEncodingStep, UnresolvedValuePolicy, ValueConstraint, ValueProgramDescriptor,
    ValueProgramSource, OPERAND_RECORD_VM_VERSION_V1, OPERAND_RECORD_VM_VERSION_V2,
    SEMANTIC_VM_OPCODE_VERSION_V2, SEMANTIC_VM_OPCODE_VERSION_V3, SEMANTIC_VM_OPCODE_VERSION_V4,
    VALUE_VM_OPCODE_VERSION_V1,
};
use types::hierarchy::ScopedOwner;

pub const VALUE_UNSIGNED_BYTE: &str = "scalar.u8";
pub const VALUE_UNSIGNED_WORD: &str = "scalar.u16";
pub const VALUE_SIGNED_BYTE: &str = "scalar.i8";
pub const VALUE_SIGNED_WORD: &str = "scalar.i16";
pub const RECORD_REGISTER: &str = "operand.register";
pub const RECORD_PAIR: &str = "operand.register-pair";
pub const RECORD_ABSOLUTE_WORD: &str = "operand.absolute-word";
pub const RECORD_IMMEDIATE: &str = "operand.immediate";
pub const ENCODING_UNSIGNED_BYTE: &str = "enc.u8";
pub const ENCODING_UNSIGNED_WORD: &str = "enc.u16be";
pub const ENCODING_REGISTER_PAIR: &str = "enc.register-pair";
pub const FIXUP_RELATIVE_BYTE: &str = "fix.rel8";
pub const FIXUP_RELATIVE_WORD: &str = "fix.rel16";

/// Compile a family register spelling to the opaque class/index used by the
/// portable record and structured-pair programs.
pub fn compile_register_input(register: &str) -> Option<(u16, u16)> {
    super::module::register_code(register).map(|index| (0, u16::from(index)))
}

fn value(id: &str, constraint: ValueConstraint) -> Result<ValueProgramDescriptor, OpcpuCodecError> {
    Ok(ValueProgramDescriptor {
        owner: ScopedOwner::Family("motorola6800".to_string()),
        id: id.to_string(),
        opcode_version: VALUE_VM_OPCODE_VERSION_V1,
        program: compile_value_program(ValueProgramSource::Input(0), &[constraint])?,
    })
}

pub fn value_programs() -> Result<Vec<ValueProgramDescriptor>, OpcpuCodecError> {
    Ok(vec![
        value(VALUE_UNSIGNED_BYTE, ValueConstraint::UnsignedBits(8))?,
        value(VALUE_UNSIGNED_WORD, ValueConstraint::UnsignedBits(16))?,
        value(VALUE_SIGNED_BYTE, ValueConstraint::SignedBits(8))?,
        value(VALUE_SIGNED_WORD, ValueConstraint::SignedBits(16))?,
    ])
}

fn record(
    id: &str,
    version: u16,
    program: OperandRecordProgram,
) -> Result<OperandRecordProgramDescriptor, OpcpuCodecError> {
    Ok(OperandRecordProgramDescriptor {
        owner: ScopedOwner::Family("motorola6800".to_string()),
        id: id.to_string(),
        schema_version: version,
        program: compile_operand_record_program(program)?,
    })
}

pub fn operand_record_programs() -> Result<Vec<OperandRecordProgramDescriptor>, OpcpuCodecError> {
    Ok(vec![
        record(
            RECORD_REGISTER,
            OPERAND_RECORD_VM_VERSION_V1,
            OperandRecordProgram::Register { register_input: 0 },
        )?,
        record(
            RECORD_PAIR,
            OPERAND_RECORD_VM_VERSION_V2,
            OperandRecordProgram::RegisterPair {
                left_register_input: 0,
                right_register_input: 1,
                indirect: false,
            },
        )?,
        record(
            RECORD_ABSOLUTE_WORD,
            OPERAND_RECORD_VM_VERSION_V1,
            OperandRecordProgram::Absolute {
                value_input: 0,
                width_bits: 16,
            },
        )?,
        record(
            RECORD_IMMEDIATE,
            OPERAND_RECORD_VM_VERSION_V1,
            OperandRecordProgram::Immediate { value_input: 0 },
        )?,
    ])
}

pub fn semantic_programs() -> Result<Vec<SemanticProgramDescriptor>, OpcpuCodecError> {
    let owner = ScopedOwner::Family("motorola6800".to_string());
    let scalar = |id: &str, width: u8, max: i64| -> Result<_, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Scalar {
                input: 0,
                width,
                endian: EncodingEndian::Big,
                min: 0,
                max,
            }])?,
        })
    };
    let fixup = |id: &str, width: u8, adjustment: i32| -> Result<_, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V4,
            program: compile_fixup_program(&[FixupEncodingStep {
                input: 0,
                width,
                endian: EncodingEndian::Big,
                base: FixupBase::Position {
                    adjustment,
                    target_references_only: false,
                },
                range: FixupRange::Signed,
                unresolved: UnresolvedValuePolicy::Placeholder(0),
                relocation: PortableRelocationKind::None,
            }])?,
        })
    };
    Ok(vec![
        scalar(ENCODING_UNSIGNED_BYTE, 1, 0xff)?,
        scalar(ENCODING_UNSIGNED_WORD, 2, 0xffff)?,
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_REGISTER_PAIR.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V3,
            program: compile_structured_encoding_program(&[
                StructuredEncodingStep::RegisterPair {
                    record: 0,
                    base: 0,
                    width: 1,
                    endian: EncodingEndian::Big,
                    left_shift: 4,
                    right_shift: 0,
                    bits: 4,
                    indirect: Some(false),
                },
            ])?,
        },
        fixup(FIXUP_RELATIVE_BYTE, 1, 2)?,
        fixup(FIXUP_RELATIVE_WORD, 2, 3)?,
    ])
}
