// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral reconstruction of serialized operand records.

use package::{
    validate_operand_record_program, OPERAND_RECORD_OP_ABSOLUTE, OPERAND_RECORD_OP_COMPOSITE,
    OPERAND_RECORD_OP_DISPLACEMENT, OPERAND_RECORD_OP_FIELD, OPERAND_RECORD_OP_IMMEDIATE,
    OPERAND_RECORD_OP_INDEXED, OPERAND_RECORD_OP_INDIRECT, OPERAND_RECORD_OP_NESTED_ADDRESS,
    OPERAND_RECORD_OP_REGISTER, OPERAND_RECORD_OP_REGISTER_LIST, OPERAND_RECORD_OP_REGISTER_PAIR,
    OPERAND_RECORD_OP_REGISTER_RANGE,
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
    Suppressed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PortableAddressUpdate {
    None,
    Postincrement,
    Predecrement,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PortableSizedValue {
    pub value: i64,
    pub width_bits: u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PortableAddressIndex {
    pub register: PortableRegisterRef,
    pub width_bits: u8,
    pub scale: u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PortableMemoryIndirection {
    None,
    Preindexed,
    Postindexed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PortableFieldSelector {
    Register(PortableRegisterRef),
    Value(i64),
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
    NestedAddress {
        base: PortableAddressBase,
        base_displacement: Option<PortableSizedValue>,
        index: Option<PortableAddressIndex>,
        indirection: PortableMemoryIndirection,
        outer_displacement: Option<PortableSizedValue>,
    },
    RegisterPair {
        left: PortableRegisterRef,
        right: PortableRegisterRef,
        indirect: bool,
    },
    RegisterRange {
        start: PortableRegisterRef,
        end: PortableRegisterRef,
    },
    RegisterList(Vec<PortableRegisterRef>),
    Field {
        base: Box<PortableOperandRecord>,
        offset: PortableFieldSelector,
        width: PortableFieldSelector,
    },
    Composite {
        format: u16,
        records: Vec<PortableOperandRecord>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OperandRecordVmError {
    InvalidProgram(String),
    MissingRegisterInput { index: u8 },
    MissingValueInput { index: u8 },
    MissingRecordInput { index: u8 },
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
            Self::MissingRecordInput { index } => {
                write!(f, "operand-record nested input {index} is missing")
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
        2 => Ok(PortableAddressBase::Suppressed),
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
    execute_operand_record_program_with_records(schema_version, program, registers, values, &[])
}

pub fn execute_operand_record_program_with_records(
    schema_version: u16,
    program: &[u8],
    registers: &[PortableRegisterRef],
    values: &[i64],
    records: &[PortableOperandRecord],
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
        OPERAND_RECORD_OP_NESTED_ADDRESS => {
            let optional_value = |index: u8, width_bits: u8| {
                if index == u8::MAX {
                    Ok(None)
                } else {
                    Ok(Some(PortableSizedValue {
                        value: value_input(values, index)?,
                        width_bits,
                    }))
                }
            };
            let index = if program[5] == u8::MAX {
                None
            } else {
                Some(PortableAddressIndex {
                    register: register_input(registers, program[5])?,
                    width_bits: program[6],
                    scale: program[7],
                })
            };
            let indirection = match program[8] {
                0 => PortableMemoryIndirection::None,
                1 => PortableMemoryIndirection::Preindexed,
                2 => PortableMemoryIndirection::Postindexed,
                _ => unreachable!("validated indirection kind"),
            };
            Ok(PortableOperandRecord::NestedAddress {
                base: address_base(program[1], program[2], registers)?,
                base_displacement: optional_value(program[3], program[4])?,
                index,
                indirection,
                outer_displacement: optional_value(program[9], program[10])?,
            })
        }
        OPERAND_RECORD_OP_REGISTER_PAIR => Ok(PortableOperandRecord::RegisterPair {
            left: register_input(registers, program[1])?,
            right: register_input(registers, program[2])?,
            indirect: program[3] != 0,
        }),
        OPERAND_RECORD_OP_REGISTER_RANGE => Ok(PortableOperandRecord::RegisterRange {
            start: register_input(registers, program[1])?,
            end: register_input(registers, program[2])?,
        }),
        OPERAND_RECORD_OP_REGISTER_LIST => Ok(PortableOperandRecord::RegisterList(
            registers
                .get(program[1] as usize..)
                .filter(|registers| !registers.is_empty())
                .ok_or(OperandRecordVmError::MissingRegisterInput { index: program[1] })?
                .to_vec(),
        )),
        OPERAND_RECORD_OP_FIELD => {
            let field = |kind: u8, index: u8| match kind {
                0 => Ok(PortableFieldSelector::Register(register_input(
                    registers, index,
                )?)),
                1 => Ok(PortableFieldSelector::Value(value_input(values, index)?)),
                _ => unreachable!("validated field source"),
            };
            Ok(PortableOperandRecord::Field {
                base: Box::new(
                    records
                        .get(program[1] as usize)
                        .cloned()
                        .ok_or(OperandRecordVmError::MissingRecordInput { index: program[1] })?,
                ),
                offset: field(program[2], program[3])?,
                width: field(program[4], program[5])?,
            })
        }
        OPERAND_RECORD_OP_COMPOSITE => {
            let first_record = program[3];
            let records = if first_record == u8::MAX {
                Vec::new()
            } else {
                records
                    .get(first_record as usize..)
                    .filter(|records| !records.is_empty())
                    .ok_or(OperandRecordVmError::MissingRecordInput {
                        index: first_record,
                    })?
                    .to_vec()
            };
            Ok(PortableOperandRecord::Composite {
                format: u16::from_le_bytes([program[1], program[2]]),
                records,
            })
        }
        _ => unreachable!("validated operand-record opcode"),
    }
}
