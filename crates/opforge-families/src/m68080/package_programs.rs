// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-owned portable operand records for the Motorola 68080 extension.

use package::{
    compile_operand_record_program, OpcpuCodecError, OperandRecordProgram,
    OperandRecordProgramDescriptor, OPERAND_RECORD_VM_VERSION_V1, OPERAND_RECORD_VM_VERSION_V3,
};
use types::hierarchy::ScopedOwner;

pub const RECORD_AMMX_DATA_REGISTER: &str = "operand.ammx-data-register";
pub const RECORD_AMMX_VEA: &str = "operand.ammx-vea";
pub const RECORD_AMMX_PAIR: &str = "operand.ammx-pair";
pub const RECORD_AMMX_GROUP: &str = "operand.ammx-group";
pub const RECORD_TEXTURE_NESTED: &str = "operand.texture-nested";
pub const RECORD_TEXTURE_EXTERNAL_SCALE: &str = "operand.texture-external-scale";
pub const RECORD_TEXTURE_SCALED_INSIDE: &str = "operand.texture-scaled-inside";
pub const RECORD_TEXTURE_FLAT: &str = "operand.texture-flat";

pub const FORMAT_AMMX_VEA: u16 = 0;
pub const FORMAT_AMMX_PAIR: u16 = 1;
pub const FORMAT_AMMX_GROUP: u16 = 2;
pub const FORMAT_TEXTURE_NESTED: u16 = 16;
pub const FORMAT_TEXTURE_EXTERNAL_SCALE: u16 = 17;
pub const FORMAT_TEXTURE_SCALED_INSIDE: u16 = 18;
pub const FORMAT_TEXTURE_FLAT: u16 = 19;

/// Convert CPU-owned AMMX register spelling to an opaque class/index pair.
pub fn compile_register_input(register: &str) -> Option<(u16, u16)> {
    if register.eq_ignore_ascii_case("SP") {
        return Some((1, 7));
    }
    let (prefix, suffix) = register.split_at_checked(1)?;
    let index = suffix.parse::<u16>().ok()?;
    match prefix.to_ascii_uppercase().as_str() {
        "D" if index <= 7 => Some((0, index)),
        "A" if index <= 7 => Some((1, index)),
        "E" if index <= 23 => Some((4, index)),
        "B" if index <= 7 => Some((5, index)),
        _ => None,
    }
}

fn record(
    id: &str,
    schema_version: u16,
    program: OperandRecordProgram,
) -> Result<OperandRecordProgramDescriptor, OpcpuCodecError> {
    Ok(OperandRecordProgramDescriptor {
        owner: ScopedOwner::Cpu("m68080".to_string()),
        id: id.to_string(),
        schema_version,
        program: compile_operand_record_program(program)?,
    })
}

fn composite(id: &str, format: u16) -> Result<OperandRecordProgramDescriptor, OpcpuCodecError> {
    record(
        id,
        OPERAND_RECORD_VM_VERSION_V3,
        OperandRecordProgram::Composite {
            format,
            first_record_input: Some(0),
        },
    )
}

/// Compile CPU-scoped AMMX wrappers from neutral nested records.
pub fn operand_record_programs() -> Result<Vec<OperandRecordProgramDescriptor>, OpcpuCodecError> {
    Ok(vec![
        record(
            RECORD_AMMX_DATA_REGISTER,
            OPERAND_RECORD_VM_VERSION_V1,
            OperandRecordProgram::Register { register_input: 0 },
        )?,
        composite(RECORD_AMMX_VEA, FORMAT_AMMX_VEA)?,
        composite(RECORD_AMMX_PAIR, FORMAT_AMMX_PAIR)?,
        composite(RECORD_AMMX_GROUP, FORMAT_AMMX_GROUP)?,
        composite(RECORD_TEXTURE_NESTED, FORMAT_TEXTURE_NESTED)?,
        composite(RECORD_TEXTURE_EXTERNAL_SCALE, FORMAT_TEXTURE_EXTERNAL_SCALE)?,
        composite(RECORD_TEXTURE_SCALED_INSIDE, FORMAT_TEXTURE_SCALED_INSIDE)?,
        composite(RECORD_TEXTURE_FLAT, FORMAT_TEXTURE_FLAT)?,
    ])
}
