// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral scalar-value program interpreter.

use package::{
    VALUE_VM_OPCODE_VERSION_V1, VALUE_VM_OPCODE_VERSION_V2, VALUE_VM_OP_ENCODE_UPPER_BOUND_AS_ZERO,
    VALUE_VM_OP_END, VALUE_VM_OP_NORMALIZE_TWOS_COMPLEMENT, VALUE_VM_OP_PUSH_INPUT,
    VALUE_VM_OP_PUSH_LITERAL_I64, VALUE_VM_OP_REQUIRE_RANGE_I64, VALUE_VM_OP_REQUIRE_SIGNED_BITS,
    VALUE_VM_OP_REQUIRE_UNSIGNED_BITS,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueVmError {
    InvalidProgram(String),
    MissingInput { index: u8 },
    ConstraintViolation { value: i64, constraint: String },
}

impl std::fmt::Display for ValueVmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidProgram(detail) => write!(f, "invalid value VM program: {detail}"),
            Self::MissingInput { index } => write!(f, "value VM input {index} is missing"),
            Self::ConstraintViolation { value, constraint } => {
                write!(f, "value {value} violates {constraint}")
            }
        }
    }
}

impl std::error::Error for ValueVmError {}

fn read_i64(program: &[u8], pc: &mut usize, context: &str) -> Result<i64, ValueVmError> {
    let end = pc
        .checked_add(8)
        .filter(|end| *end <= program.len())
        .ok_or_else(|| ValueVmError::InvalidProgram(format!("{context} is truncated")))?;
    let value = i64::from_le_bytes(
        program[*pc..end]
            .try_into()
            .expect("checked scalar byte width"),
    );
    *pc = end;
    Ok(value)
}

fn read_bits(program: &[u8], pc: &mut usize) -> Result<u8, ValueVmError> {
    let bits = *program
        .get(*pc)
        .ok_or_else(|| ValueVmError::InvalidProgram("bit width is truncated".to_string()))?;
    *pc += 1;
    if !(1..=64).contains(&bits) {
        return Err(ValueVmError::InvalidProgram(format!(
            "bit width {bits} is outside 1..=64"
        )));
    }
    Ok(bits)
}

fn require_value(value: Option<i64>) -> Result<i64, ValueVmError> {
    value.ok_or_else(|| ValueVmError::InvalidProgram("constraint precedes its source".to_string()))
}

/// Execute one validated scalar program using already-evaluated scalar inputs.
pub fn execute_value_program(
    opcode_version: u16,
    program: &[u8],
    inputs: &[i64],
) -> Result<i64, ValueVmError> {
    if opcode_version != VALUE_VM_OPCODE_VERSION_V1 && opcode_version != VALUE_VM_OPCODE_VERSION_V2
    {
        return Err(ValueVmError::InvalidProgram(format!(
            "unsupported opcode version {opcode_version}"
        )));
    }

    let mut pc = 0usize;
    let mut value = None;
    loop {
        let opcode = *program
            .get(pc)
            .ok_or_else(|| ValueVmError::InvalidProgram("truncated before END".to_string()))?;
        pc += 1;
        match opcode {
            VALUE_VM_OP_PUSH_LITERAL_I64 => {
                if value.is_some() {
                    return Err(ValueVmError::InvalidProgram(
                        "program defines more than one source".to_string(),
                    ));
                }
                value = Some(read_i64(program, &mut pc, "literal")?);
            }
            VALUE_VM_OP_PUSH_INPUT => {
                if value.is_some() {
                    return Err(ValueVmError::InvalidProgram(
                        "program defines more than one source".to_string(),
                    ));
                }
                let index = *program.get(pc).ok_or_else(|| {
                    ValueVmError::InvalidProgram("input index is truncated".to_string())
                })?;
                pc += 1;
                value = Some(
                    *inputs
                        .get(index as usize)
                        .ok_or(ValueVmError::MissingInput { index })?,
                );
            }
            VALUE_VM_OP_NORMALIZE_TWOS_COMPLEMENT => {
                let bits = read_bits(program, &mut pc)?;
                let current = require_value(value)?;
                if bits < 64 {
                    let modulus = 1_i128 << bits;
                    let signed_max = (1_i128 << (bits - 1)) - 1;
                    let current_wide = i128::from(current);
                    if current_wide > signed_max && current_wide < modulus {
                        value = Some((current_wide - modulus) as i64);
                    }
                }
            }
            VALUE_VM_OP_REQUIRE_SIGNED_BITS => {
                let bits = read_bits(program, &mut pc)?;
                let current = require_value(value)?;
                let (min, max) = if bits == 64 {
                    (i128::from(i64::MIN), i128::from(i64::MAX))
                } else {
                    let magnitude = 1_i128 << (bits - 1);
                    (-magnitude, magnitude - 1)
                };
                if !(min..=max).contains(&i128::from(current)) {
                    return Err(ValueVmError::ConstraintViolation {
                        value: current,
                        constraint: format!("signed {bits}-bit range"),
                    });
                }
            }
            VALUE_VM_OP_REQUIRE_UNSIGNED_BITS => {
                let bits = read_bits(program, &mut pc)?;
                let current = require_value(value)?;
                let max = if bits == 64 {
                    i128::from(i64::MAX)
                } else {
                    (1_i128 << bits) - 1
                };
                if !(0..=max).contains(&i128::from(current)) {
                    return Err(ValueVmError::ConstraintViolation {
                        value: current,
                        constraint: format!("unsigned {bits}-bit range"),
                    });
                }
            }
            VALUE_VM_OP_REQUIRE_RANGE_I64 => {
                let current = require_value(value)?;
                let min = read_i64(program, &mut pc, "inclusive range minimum")?;
                let max = read_i64(program, &mut pc, "inclusive range maximum")?;
                if min > max {
                    return Err(ValueVmError::InvalidProgram(format!(
                        "inclusive range minimum {min} exceeds maximum {max}"
                    )));
                }
                if !(min..=max).contains(&current) {
                    return Err(ValueVmError::ConstraintViolation {
                        value: current,
                        constraint: format!("inclusive range {min}..={max}"),
                    });
                }
            }
            VALUE_VM_OP_ENCODE_UPPER_BOUND_AS_ZERO
                if opcode_version == VALUE_VM_OPCODE_VERSION_V2 =>
            {
                let bits = read_bits(program, &mut pc)?;
                if bits > 62 {
                    return Err(ValueVmError::InvalidProgram(format!(
                        "packed-field width {bits} is outside 1..=62"
                    )));
                }
                let current = require_value(value)?;
                let upper = 1_i64 << bits;
                if !(1..=upper).contains(&current) {
                    return Err(ValueVmError::ConstraintViolation {
                        value: current,
                        constraint: format!("packed {bits}-bit source range 1..={upper}"),
                    });
                }
                if current == upper {
                    value = Some(0);
                }
            }
            VALUE_VM_OP_END if value.is_none() => {
                return Err(ValueVmError::InvalidProgram(
                    "program ends without a source".to_string(),
                ));
            }
            VALUE_VM_OP_END if pc == program.len() => return Ok(value.expect("checked value")),
            VALUE_VM_OP_END => {
                return Err(ValueVmError::InvalidProgram(
                    "trailing bytes after END".to_string(),
                ));
            }
            _ => {
                return Err(ValueVmError::InvalidProgram(format!(
                    "unknown opcode 0x{opcode:02X} at pc={}",
                    pc - 1
                )));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use package::{
        compile_value_program, compile_value_program_v2, ValueConstraint, ValueProgramSource,
    };

    #[test]
    fn scalar_program_enforces_bounds_and_normalizes_twos_complement() {
        let unsigned = compile_value_program(
            ValueProgramSource::Input(0),
            &[ValueConstraint::UnsignedBits(8)],
        )
        .expect("compile unsigned program");
        assert_eq!(
            execute_value_program(VALUE_VM_OPCODE_VERSION_V1, &unsigned, &[255]),
            Ok(255)
        );
        assert!(matches!(
            execute_value_program(VALUE_VM_OPCODE_VERSION_V1, &unsigned, &[256]),
            Err(ValueVmError::ConstraintViolation { .. })
        ));

        let normalized = compile_value_program(
            ValueProgramSource::Input(0),
            &[
                ValueConstraint::NormalizeTwosComplement(32),
                ValueConstraint::SignedBits(32),
            ],
        )
        .expect("compile normalized program");
        assert_eq!(
            execute_value_program(VALUE_VM_OPCODE_VERSION_V1, &normalized, &[0xffff_ffff]),
            Ok(-1)
        );
    }

    #[test]
    fn v2_packed_field_projection_maps_only_the_inclusive_upper_bound_to_zero() {
        let program = compile_value_program_v2(
            ValueProgramSource::Input(0),
            &[ValueConstraint::EncodeUpperBoundAsZero(3)],
        )
        .expect("compile packed-field projection");
        for (input, expected) in [(1, 1), (7, 7), (8, 0)] {
            assert_eq!(
                execute_value_program(VALUE_VM_OPCODE_VERSION_V2, &program, &[input]),
                Ok(expected)
            );
        }
        for rejected in [0, 9] {
            assert!(matches!(
                execute_value_program(VALUE_VM_OPCODE_VERSION_V2, &program, &[rejected]),
                Err(ValueVmError::ConstraintViolation { .. })
            ));
        }
        assert!(matches!(
            execute_value_program(VALUE_VM_OPCODE_VERSION_V1, &program, &[8]),
            Err(ValueVmError::InvalidProgram(_))
        ));
    }
}
