// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral execution of fixed-field and scalar `SEMV` v2/v6 programs.

use package::{decode_encoding_program, EncodingEndian, EncodingStep, OpcpuCodecError};

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
            } => {
                if *min >= 0 {
                    write!(
                        f,
                        "encoding input {index} value {value} is out of range ({min}-{max})"
                    )
                } else {
                    write!(
                        f,
                        "encoding input {index} value {value} is outside {min}..={max}"
                    )
                }
            }
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

fn emit_ieee754(out: &mut Vec<u8>, value: u64, width: u8, endian: EncodingEndian) {
    let bytes = match endian {
        EncodingEndian::Big => value.to_be_bytes(),
        EncodingEndian::Little => value.to_le_bytes(),
    };
    match endian {
        EncodingEndian::Big => out.extend_from_slice(&bytes[8 - width as usize..]),
        EncodingEndian::Little => out.extend_from_slice(&bytes[..width as usize]),
    }
}

pub fn execute_encoding_program(
    opcode_version: u16,
    program: &[u8],
    inputs: &[i64],
) -> Result<Vec<u8>, EncodingVmError> {
    let steps =
        decode_encoding_program(opcode_version, program).map_err(EncodingVmError::Program)?;
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
            EncodingStep::InputFields {
                base_input,
                width,
                endian,
                fields,
            } => {
                let max = match width {
                    1 => 0xff,
                    2 => 0xffff,
                    4 => u32::MAX as i64,
                    _ => unreachable!("validated encoding width"),
                };
                let mut value = input(inputs, base_input, 0, max)? as u32;
                for field in fields {
                    let mask = ((1_u64 << field.bits) - 1) as u32;
                    let shifted_mask = mask << field.shift;
                    value = (value & !shifted_mask)
                        | (((input(inputs, field.input, field.min, field.max)? as u32) & mask)
                            << field.shift);
                }
                emit(&mut out, value, width, endian);
            }
            EncodingStep::IntegerToIeee754 {
                input: index,
                width,
                endian,
            } => {
                let value = input(inputs, index, i64::MIN, i64::MAX)?;
                let bits = match width {
                    4 => u64::from((value as f32).to_bits()),
                    8 => (value as f64).to_bits(),
                    _ => unreachable!("validated IEEE-754 width"),
                };
                emit_ieee754(&mut out, bits, width, endian);
            }
        }
    }
    Ok(out)
}
