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
pub const FIXUP_ABSOLUTE_LONG: &str = "fix.abs32";
pub const ENCODING_NONE: &str = "enc.none";
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

/// Compile MOS scalar encodings and relocations with the neutral semantic VM.
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
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_NONE.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[])?,
        },
        scalar(ENCODING_UNSIGNED_BYTE, 1, 0xff)?,
        scalar(ENCODING_UNSIGNED_WORD, 2, 0xffff)?,
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: FIXUP_ABSOLUTE_LONG.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V4,
            program: compile_fixup_program(&[FixupEncodingStep {
                input: 0,
                width: 4,
                endian: EncodingEndian::Little,
                base: FixupBase::Value,
                range: FixupRange::BitPattern,
                unresolved: UnresolvedValuePolicy::Placeholder(0),
                relocation: PortableRelocationKind::Absolute,
                transform: FixupTransform::Identity,
            }])?,
        },
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

/// Describe raw base-family source structures with CPU-neutral projection plans.
/// Retain the descriptor's existing precedence, width and widening policy.
/// Legacy semantic shapes remain available to specialized CPU parsers.
pub fn structural_selector(
    mut selector: package::ModeSelectorDescriptor,
    mode: super::AddressMode,
) -> Option<package::ModeSelectorDescriptor> {
    use super::AddressMode;
    let (shape, plan) = match mode {
        AddressMode::ZeroPageX | AddressMode::ZeroPageY
        | AddressMode::AbsoluteX | AddressMode::AbsoluteY => {
            let register = if matches!(mode, AddressMode::ZeroPageX | AddressMode::AbsoluteX) { "X" } else { "Y" };
            let byte = matches!(mode, AddressMode::ZeroPageX | AddressMode::ZeroPageY);
            let encoding = if byte { ENCODING_UNSIGNED_BYTE } else { ENCODING_UNSIGNED_WORD };
            let value = if byte { VALUE_UNSIGNED_BYTE } else { VALUE_UNSIGNED_WORD };
            ("direct_register", format!("semv.inputs.v1:{encoding}@required_value_program:{value}:scalar_expr0,named_register1={register}"))
        }
        AddressMode::ZeroPage | AddressMode::Absolute => {
            let byte = mode == AddressMode::ZeroPage;
            let encoding = if byte { ENCODING_UNSIGNED_BYTE } else { ENCODING_UNSIGNED_WORD };
            let value = if byte { VALUE_UNSIGNED_BYTE } else { VALUE_UNSIGNED_WORD };
            ("direct", format!("semv.inputs.v1:{encoding}@required_value_program:{value}:scalar_expr0"))
        }
        AddressMode::Relative => ("direct", format!("semv.sequence.v1:match:_@scalar_expr0;encode:{ENCODING_NONE}@literal:0;fixup:{FIXUP_RELATIVE_BYTE}@expr0")),
        AddressMode::Accumulator => ("register", format!("semv.inputs.v1:{ENCODING_NONE}@named_register0=A")),
        AddressMode::Indirect => ("direct", format!("semv.inputs.v1:{ENCODING_UNSIGNED_WORD}@required_value_program:{VALUE_UNSIGNED_WORD}:indirect_value0")),
        AddressMode::IndexedIndirectX => ("direct", format!("semv.inputs.v1:{ENCODING_UNSIGNED_BYTE}@required_value_program:{VALUE_UNSIGNED_BYTE}:indirect_tuple_value0.item0,indirect_tuple_named_register0.item1=X,indirect_tuple_arity0.value2")),
        AddressMode::IndirectIndexedY => ("direct_register", format!("semv.inputs.v1:{ENCODING_UNSIGNED_BYTE}@required_value_program:{VALUE_UNSIGNED_BYTE}:indirect_value0,named_register1=Y")),
        _ => return None,
    };
    selector.shape_key = shape.to_string();
    selector.operand_plan = plan;
    Some(selector)
}
