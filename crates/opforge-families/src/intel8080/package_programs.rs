// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Package compilation adapter for Intel 8080-family scalar, operand, and
//! deferred relative-value semantics.

use package::{
    compile_encoding_program, compile_fixup_program, compile_operand_record_program,
    compile_selector_map_program, compile_value_program, EncodingEndian, EncodingFieldSpec,
    EncodingStep, FixupBase, FixupEncodingStep, FixupRange, OpcpuCodecError, OperandRecordProgram,
    OperandRecordProgramDescriptor, PortableRelocationKind, SelectorProgramDescriptor,
    SemanticProgramDescriptor, UnresolvedValuePolicy, ValueConstraint, ValueProgramDescriptor,
    ValueProgramSource, OPERAND_RECORD_VM_VERSION_V1, SELECTOR_VM_OPCODE_VERSION_V1,
    SEMANTIC_VM_OPCODE_VERSION_V2, SEMANTIC_VM_OPCODE_VERSION_V4, VALUE_VM_OPCODE_VERSION_V1,
};
use types::hierarchy::ScopedOwner;

pub const VALUE_UNSIGNED_BYTE: &str = "scalar.u8";
pub const VALUE_UNSIGNED_WORD: &str = "scalar.u16";
pub const RECORD_REGISTER: &str = "operand.register";
pub const RECORD_INDIRECT: &str = "operand.indirect";
pub const RECORD_ABSOLUTE_WORD: &str = "operand.absolute-word";
pub const RECORD_IMMEDIATE: &str = "operand.immediate";
pub const ENCODING_RESTART_VECTOR: &str = "enc.restart-vector";
pub const FIXUP_Z80_RELATIVE_BYTE: &str = "fix.rel8";
pub const SELECTOR_ZILOG_EXACT_ALIASES: &str = "aliases.exact";

fn value(id: &str, bits: u8) -> Result<ValueProgramDescriptor, OpcpuCodecError> {
    Ok(ValueProgramDescriptor {
        owner: ScopedOwner::Family("intel8080".to_string()),
        id: id.to_string(),
        opcode_version: VALUE_VM_OPCODE_VERSION_V1,
        program: compile_value_program(
            ValueProgramSource::Input(0),
            &[ValueConstraint::UnsignedBits(bits)],
        )?,
    })
}

pub fn value_programs() -> Result<Vec<ValueProgramDescriptor>, OpcpuCodecError> {
    Ok(vec![
        value(VALUE_UNSIGNED_BYTE, 8)?,
        value(VALUE_UNSIGNED_WORD, 16)?,
    ])
}

fn record(
    id: &str,
    program: OperandRecordProgram,
) -> Result<OperandRecordProgramDescriptor, OpcpuCodecError> {
    Ok(OperandRecordProgramDescriptor {
        owner: ScopedOwner::Family("intel8080".to_string()),
        id: id.to_string(),
        schema_version: OPERAND_RECORD_VM_VERSION_V1,
        program: compile_operand_record_program(program)?,
    })
}

pub fn operand_record_programs() -> Result<Vec<OperandRecordProgramDescriptor>, OpcpuCodecError> {
    Ok(vec![
        record(
            RECORD_REGISTER,
            OperandRecordProgram::Register { register_input: 0 },
        )?,
        record(
            RECORD_INDIRECT,
            OperandRecordProgram::Indirect {
                register_input: 0,
                update: package::OperandRecordUpdate::None,
            },
        )?,
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

/// Compile the operand-independent part of the authoritative Zilog dialect
/// table. Operand-transforming entries remain family-owned compiler behavior.
pub fn selector_programs() -> Result<Vec<SelectorProgramDescriptor>, OpcpuCodecError> {
    let aliases = super::dialect::ZILOG_DIALECT_MAP
        .iter()
        .filter(|entry| {
            entry.from_regs == 0
                && !entry.from_has_imm
                && entry.canonical_regs == 0
                && !entry.canonical_has_imm
                && entry.transform == super::dialect::OperandTransform::Identity
                && entry.from != entry.canonical
                // RLC/RRC are also valid one-operand Z80 mnemonics. SLCT v1
                // cannot preserve the source alias's zero-operand arity.
                && !super::dialect::is_z80_only_mnemonic(entry.canonical)
        })
        .map(|entry| (entry.from.to_string(), entry.canonical.to_string()))
        .collect::<Vec<_>>();
    Ok(vec![SelectorProgramDescriptor {
        owner: ScopedOwner::Dialect("zilog".to_string()),
        id: SELECTOR_ZILOG_EXACT_ALIASES.to_string(),
        opcode_version: SELECTOR_VM_OPCODE_VERSION_V1,
        priority: 0,
        cpu_allow_list: None,
        program: compile_selector_map_program(&aliases)?,
    }])
}

#[cfg(test)]
mod tests {
    #[test]
    fn overloaded_rotate_aliases_remain_on_the_arity_aware_dialect_path() {
        for mnemonic in ["RLCA", "RRCA"] {
            assert!(super::super::dialect::find_mapping(mnemonic, 0, false).is_some());
            assert!(super::super::dialect::find_mapping(mnemonic, 1, false).is_none());
        }
    }
}

pub fn semantic_programs() -> Result<Vec<SemanticProgramDescriptor>, OpcpuCodecError> {
    Ok(vec![
        SemanticProgramDescriptor {
            owner: ScopedOwner::Family("intel8080".to_string()),
            id: ENCODING_RESTART_VECTOR.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Fields {
                base: 0xc7,
                width: 1,
                endian: EncodingEndian::Little,
                fields: vec![EncodingFieldSpec {
                    input: 0,
                    shift: 3,
                    bits: 3,
                    min: 0,
                    max: 7,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: ScopedOwner::Cpu("z80".to_string()),
            id: FIXUP_Z80_RELATIVE_BYTE.to_string(),
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
            }])?,
        },
    ])
}
