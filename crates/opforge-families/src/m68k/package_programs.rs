// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Package compilation adapter for Motorola 68000 scalar semantics.

use package::{
    compile_value_program, OpcpuCodecError, ValueConstraint, ValueProgramDescriptor,
    ValueProgramSource, VALUE_VM_OPCODE_VERSION_V1,
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

/// Existing family scalar normalization retained as the differential oracle
/// while package-first conversion is incomplete.
pub fn oracle_normalize_wrapped_i32(value: i64) -> i64 {
    M68KFamilyHandler::normalize_wrapped_i32(value)
}
