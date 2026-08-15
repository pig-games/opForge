// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral reconstruction of serialized operand records.

use package::{
    validate_operand_record_program, OPERAND_RECORD_OP_ABSOLUTE, OPERAND_RECORD_OP_DISPLACEMENT,
    OPERAND_RECORD_OP_IMMEDIATE, OPERAND_RECORD_OP_INDEXED, OPERAND_RECORD_OP_INDIRECT,
    OPERAND_RECORD_OP_REGISTER,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PortableRegisterRef {
    pub class: u16,
    pub index: u16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PortableAddressBase {
    Register(PortableRegisterRef),
    ProgramCounter,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PortableAddressUpdate {
    None,
    Postincrement,
    Predecrement,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PortableOperandRecord {
    Register(PortableRegisterRef),
    Indirect {
        base: PortableRegisterRef,
        update: PortableAddressUpdate,
    },
    Displacement {
        base: PortableAddressBase,
        displacement: i64,
    },
    Indexed {
        base: PortableAddressBase,
        index: PortableRegisterRef,
        index_width_bits: u8,
        scale: u8,
        displacement: i64,
    },
    Absolute {
        value: i64,
        width_bits: u8,
    },
    Immediate {
        value: i64,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OperandRecordVmError {
    InvalidProgram(String),
    MissingRegisterInput { index: u8 },
    MissingValueInput { index: u8 },
}

impl std::fmt::Display for OperandRecordVmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidProgram(detail) => write!(f, "invalid operand-record program: {detail}"),
            Self::MissingRegisterInput { index } => {
                write!(f, "operand-record register input {index} is missing")
            }
            Self::MissingValueInput { index } => {
                write!(f, "operand-record value input {index} is missing")
            }
        }
    }
}

impl std::error::Error for OperandRecordVmError {}

fn register_input(
    inputs: &[PortableRegisterRef],
    index: u8,
) -> Result<PortableRegisterRef, OperandRecordVmError> {
    inputs
        .get(index as usize)
        .copied()
        .ok_or(OperandRecordVmError::MissingRegisterInput { index })
}

fn value_input(inputs: &[i64], index: u8) -> Result<i64, OperandRecordVmError> {
    inputs
        .get(index as usize)
        .copied()
        .ok_or(OperandRecordVmError::MissingValueInput { index })
}

fn address_base(
    kind: u8,
    register_index: u8,
    registers: &[PortableRegisterRef],
) -> Result<PortableAddressBase, OperandRecordVmError> {
    match kind {
        0 => Ok(PortableAddressBase::Register(register_input(
            registers,
            register_index,
        )?)),
        1 => Ok(PortableAddressBase::ProgramCounter),
        _ => Err(OperandRecordVmError::InvalidProgram(
            "invalid address base kind".to_string(),
        )),
    }
}

pub fn execute_operand_record_program(
    schema_version: u16,
    program: &[u8],
    registers: &[PortableRegisterRef],
    values: &[i64],
) -> Result<PortableOperandRecord, OperandRecordVmError> {
    validate_operand_record_program(schema_version, program)
        .map_err(|error| OperandRecordVmError::InvalidProgram(error.to_string()))?;
    match program[0] {
        OPERAND_RECORD_OP_REGISTER => Ok(PortableOperandRecord::Register(register_input(
            registers, program[1],
        )?)),
        OPERAND_RECORD_OP_INDIRECT => {
            let update = match program[2] {
                0 => PortableAddressUpdate::None,
                1 => PortableAddressUpdate::Postincrement,
                2 => PortableAddressUpdate::Predecrement,
                _ => unreachable!("validated update mode"),
            };
            Ok(PortableOperandRecord::Indirect {
                base: register_input(registers, program[1])?,
                update,
            })
        }
        OPERAND_RECORD_OP_DISPLACEMENT => Ok(PortableOperandRecord::Displacement {
            base: address_base(program[1], program[2], registers)?,
            displacement: value_input(values, program[3])?,
        }),
        OPERAND_RECORD_OP_INDEXED => Ok(PortableOperandRecord::Indexed {
            base: address_base(program[1], program[2], registers)?,
            index: register_input(registers, program[3])?,
            index_width_bits: program[4],
            scale: program[5],
            displacement: value_input(values, program[6])?,
        }),
        OPERAND_RECORD_OP_ABSOLUTE => Ok(PortableOperandRecord::Absolute {
            value: value_input(values, program[1])?,
            width_bits: program[2],
        }),
        OPERAND_RECORD_OP_IMMEDIATE => Ok(PortableOperandRecord::Immediate {
            value: value_input(values, program[1])?,
        }),
        _ => unreachable!("validated operand-record opcode"),
    }
}
