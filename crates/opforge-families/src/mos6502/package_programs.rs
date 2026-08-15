// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Package compilation adapter for MOS 65x02 scalar semantics.

use package::{
    compile_value_program, OpcpuCodecError, ValueConstraint, ValueProgramDescriptor,
    ValueProgramSource, VALUE_VM_OPCODE_VERSION_V1,
};
use types::hierarchy::ScopedOwner;

pub const VALUE_UNSIGNED_BYTE: &str = "scalar.unsigned-byte";
pub const VALUE_UNSIGNED_WORD: &str = "scalar.unsigned-word";
pub const VALUE_LITERAL_ZERO: &str = "scalar.literal-zero";

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
