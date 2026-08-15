// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral execution of fixed-field and scalar `SEMV` v2 programs.

use package::{
    decode_encoding_program, EncodingEndian, EncodingStep, OpcpuCodecError,
    SEMANTIC_VM_OPCODE_VERSION_V2,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EncodingVmError {
    Program(OpcpuCodecError),
    MissingInput {
        index: usize,
        len: usize,
    },
    ValueOutOfRange {
        index: usize,
        value: i64,
        min: i64,
        max: i64,
    },
}

impl std::fmt::Display for EncodingVmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program(err) => write!(f, "{err}"),
            Self::MissingInput { index, len } => write!(
                f,
                "encoding input index {index} is out of range for {len} input(s)"
            ),
            Self::ValueOutOfRange {
                index,
                value,
                min,
                max,
            } => write!(
                f,
                "encoding input {index} value {value} is outside {min}..={max}"
            ),
        }
    }
}

impl std::error::Error for EncodingVmError {}

fn input(inputs: &[i64], index: u8, min: i64, max: i64) -> Result<i64, EncodingVmError> {
    let value = *inputs
        .get(index as usize)
        .ok_or(EncodingVmError::MissingInput {
            index: index as usize,
            len: inputs.len(),
        })?;
    if value < min || value > max {
        return Err(EncodingVmError::ValueOutOfRange {
            index: index as usize,
            value,
            min,
            max,
        });
    }
    Ok(value)
}

fn emit(out: &mut Vec<u8>, value: u32, width: u8, endian: EncodingEndian) {
    let bytes = match endian {
        EncodingEndian::Big => value.to_be_bytes(),
        EncodingEndian::Little => value.to_le_bytes(),
    };
    match endian {
        EncodingEndian::Big => out.extend_from_slice(&bytes[4 - width as usize..]),
        EncodingEndian::Little => out.extend_from_slice(&bytes[..width as usize]),
    }
}

pub fn execute_encoding_program(
    program: &[u8],
    inputs: &[i64],
) -> Result<Vec<u8>, EncodingVmError> {
    let steps = decode_encoding_program(SEMANTIC_VM_OPCODE_VERSION_V2, program)
        .map_err(EncodingVmError::Program)?;
    let mut out = Vec::new();
    for step in steps {
        match step {
            EncodingStep::Literal {
                value,
                width,
                endian,
            } => emit(&mut out, value, width, endian),
            EncodingStep::Scalar {
                input: index,
                width,
                endian,
                min,
                max,
            } => emit(
                &mut out,
                input(inputs, index, min, max)? as u32,
                width,
                endian,
            ),
            EncodingStep::Fields {
                base,
                width,
                endian,
                fields,
            } => {
                let mut value = base;
                for field in fields {
                    let mask = ((1_u64 << field.bits) - 1) as u32;
                    let shifted_mask = mask << field.shift;
                    value = (value & !shifted_mask)
                        | (((input(inputs, field.input, field.min, field.max)? as u32) & mask)
                            << field.shift);
                }
                emit(&mut out, value, width, endian);
            }
        }
    }
    Ok(out)
}
