// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral projection of portable structured operand records into bytes.

use package::{
    decode_structured_encoding_program, EncodingEndian, OpcpuCodecError, RegisterClassProjection,
    StructuredEncodingStep, SEMANTIC_VM_OPCODE_VERSION_V3,
};

use crate::operand_record_vm::{PortableFieldSelector, PortableOperandRecord, PortableRegisterRef};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StructuredEncodingVmError {
    Program(OpcpuCodecError),
    MissingRecord {
        index: usize,
        len: usize,
    },
    ShapeMismatch {
        index: usize,
        expected: &'static str,
    },
    RegisterOutOfRange {
        index: u16,
        bits: u8,
    },
    RegisterClassMismatch {
        left: u16,
        right: u16,
    },
    UnknownRegisterClass {
        class: u16,
    },
    PairIndirectMismatch {
        expected: bool,
        actual: bool,
    },
    CompositeFormatMismatch {
        expected: u16,
        actual: u16,
    },
    CompositeTooLong {
        len: usize,
        max: u8,
    },
    ValueOutOfRange {
        value: i64,
        bits: u8,
    },
}

impl std::fmt::Display for StructuredEncodingVmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program(err) => write!(f, "{err}"),
            Self::MissingRecord { index, len } => {
                write!(
                    f,
                    "structured record index {index} is out of range for {len} record(s)"
                )
            }
            Self::ShapeMismatch { index, expected } => {
                write!(f, "structured record {index} is not a {expected}")
            }
            Self::RegisterOutOfRange { index, bits } => {
                write!(f, "register index {index} does not fit in {bits} bits")
            }
            Self::RegisterClassMismatch { left, right } => {
                write!(f, "register classes {left} and {right} do not match")
            }
            Self::UnknownRegisterClass { class } => {
                write!(f, "register class {class} has no mask projection")
            }
            Self::PairIndirectMismatch { expected, actual } => write!(
                f,
                "register-pair indirect flag is {actual}, expected {expected}"
            ),
            Self::CompositeFormatMismatch { expected, actual } => {
                write!(
                    f,
                    "composite format {actual} does not match expected format {expected}"
                )
            }
            Self::CompositeTooLong { len, max } => {
                write!(f, "composite has {len} values, exceeding maximum {max}")
            }
            Self::ValueOutOfRange { value, bits } => {
                write!(f, "structured value {value} does not fit in {bits} bits")
            }
        }
    }
}

impl std::error::Error for StructuredEncodingVmError {}

fn record(
    records: &[PortableOperandRecord],
    index: u8,
) -> Result<&PortableOperandRecord, StructuredEncodingVmError> {
    records
        .get(index as usize)
        .ok_or(StructuredEncodingVmError::MissingRecord {
            index: index as usize,
            len: records.len(),
        })
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

fn register_value(
    register: PortableRegisterRef,
    bits: u8,
) -> Result<u32, StructuredEncodingVmError> {
    if u32::from(register.index) >= (1_u32 << bits) {
        return Err(StructuredEncodingVmError::RegisterOutOfRange {
            index: register.index,
            bits,
        });
    }
    Ok(u32::from(register.index))
}

fn selector_value(
    selector: PortableFieldSelector,
    bits: u8,
    full_width_zero: bool,
) -> Result<u32, StructuredEncodingVmError> {
    match selector {
        PortableFieldSelector::Register(register) => {
            Ok((1_u32 << bits) | register_value(register, bits)?)
        }
        PortableFieldSelector::Value(value) => {
            let natural_limit = 1_i64 << bits;
            if full_width_zero && value == natural_limit {
                return Ok(0);
            }
            if value < 0 || value >= natural_limit {
                return Err(StructuredEncodingVmError::ValueOutOfRange { value, bits });
            }
            Ok(value as u32)
        }
    }
}

fn mask_registers(
    registers: impl IntoIterator<Item = PortableRegisterRef>,
    unit_bits: u8,
    classes: &[RegisterClassProjection],
) -> Result<u32, StructuredEncodingVmError> {
    let mut mask = 0_u32;
    for register in registers {
        let offset = classes
            .iter()
            .find(|entry| entry.class == register.class)
            .ok_or(StructuredEncodingVmError::UnknownRegisterClass {
                class: register.class,
            })?
            .offset;
        let projected = u32::from(offset) + u32::from(register.index);
        if projected >= u32::from(unit_bits) {
            return Err(StructuredEncodingVmError::RegisterOutOfRange {
                index: register.index,
                bits: unit_bits,
            });
        }
        mask |= 1_u32 << projected;
    }
    Ok(mask)
}

fn flatten_composite(
    record: &PortableOperandRecord,
    format: u16,
    out: &mut Vec<i64>,
) -> Result<(), StructuredEncodingVmError> {
    match record {
        PortableOperandRecord::Register(register) => out.push(i64::from(register.index)),
        PortableOperandRecord::RegisterPair { left, right, .. } => {
            out.push(i64::from(left.index));
            out.push(i64::from(right.index));
        }
        PortableOperandRecord::RegisterRange { start, end } => {
            if start.class != end.class {
                return Err(StructuredEncodingVmError::RegisterClassMismatch {
                    left: start.class,
                    right: end.class,
                });
            }
            if start.index > end.index {
                return Err(StructuredEncodingVmError::ValueOutOfRange {
                    value: i64::from(start.index),
                    bits: 16,
                });
            }
            out.extend((start.index..=end.index).map(i64::from));
        }
        PortableOperandRecord::RegisterList(registers) => {
            out.extend(registers.iter().map(|register| i64::from(register.index)));
        }
        PortableOperandRecord::Immediate { value }
        | PortableOperandRecord::Absolute { value, .. } => out.push(*value),
        PortableOperandRecord::Composite {
            format: actual,
            records,
        } => {
            if *actual != format {
                return Err(StructuredEncodingVmError::CompositeFormatMismatch {
                    expected: format,
                    actual: *actual,
                });
            }
            for record in records {
                flatten_composite(record, format, out)?;
            }
        }
        _ => {
            return Err(StructuredEncodingVmError::ShapeMismatch {
                index: 0,
                expected: "composite scalar",
            })
        }
    }
    Ok(())
}

pub fn execute_structured_encoding_program(
    program: &[u8],
    records: &[PortableOperandRecord],
) -> Result<Vec<u8>, StructuredEncodingVmError> {
    let steps = decode_structured_encoding_program(SEMANTIC_VM_OPCODE_VERSION_V3, program)
        .map_err(StructuredEncodingVmError::Program)?;
    let mut out = Vec::new();
    for step in steps {
        match step {
            StructuredEncodingStep::RegisterMask {
                record: index,
                width,
                endian,
                reverse_bits,
                classes,
            } => {
                let source = record(records, index)?;
                let unit_bits = width * 8;
                let mut value = match source {
                    PortableOperandRecord::RegisterList(registers) => {
                        mask_registers(registers.iter().copied(), unit_bits, &classes)?
                    }
                    PortableOperandRecord::RegisterRange { start, end } => {
                        if start.class != end.class {
                            return Err(StructuredEncodingVmError::RegisterClassMismatch {
                                left: start.class,
                                right: end.class,
                            });
                        }
                        if start.index > end.index {
                            return Err(StructuredEncodingVmError::ValueOutOfRange {
                                value: i64::from(start.index),
                                bits: unit_bits,
                            });
                        }
                        mask_registers(
                            (start.index..=end.index).map(|index| PortableRegisterRef {
                                class: start.class,
                                index,
                            }),
                            unit_bits,
                            &classes,
                        )?
                    }
                    _ => {
                        return Err(StructuredEncodingVmError::ShapeMismatch {
                            index: index as usize,
                            expected: "register list or range",
                        })
                    }
                };
                if reverse_bits {
                    value = value.reverse_bits() >> (32 - unit_bits);
                }
                emit(&mut out, value, width, endian);
            }
            StructuredEncodingStep::RegisterPair {
                record: index,
                base,
                width,
                endian,
                left_shift,
                right_shift,
                bits,
                indirect,
            } => {
                let PortableOperandRecord::RegisterPair {
                    left,
                    right,
                    indirect: actual,
                } = record(records, index)?
                else {
                    return Err(StructuredEncodingVmError::ShapeMismatch {
                        index: index as usize,
                        expected: "register pair",
                    });
                };
                if let Some(expected) = indirect {
                    if expected != *actual {
                        return Err(StructuredEncodingVmError::PairIndirectMismatch {
                            expected,
                            actual: *actual,
                        });
                    }
                }
                let value = base
                    | (register_value(*left, bits)? << left_shift)
                    | (register_value(*right, bits)? << right_shift);
                emit(&mut out, value, width, endian);
            }
            StructuredEncodingStep::FieldSelectors {
                record: index,
                base,
                width,
                endian,
                offset_shift,
                width_shift,
                bits,
                offset_full_width_zero,
                width_full_width_zero,
            } => {
                let PortableOperandRecord::Field {
                    offset,
                    width: size,
                    ..
                } = record(records, index)?
                else {
                    return Err(StructuredEncodingVmError::ShapeMismatch {
                        index: index as usize,
                        expected: "field selector",
                    });
                };
                let value = base
                    | (selector_value(*offset, bits, offset_full_width_zero)? << offset_shift)
                    | (selector_value(*size, bits, width_full_width_zero)? << width_shift);
                emit(&mut out, value, width, endian);
            }
            StructuredEncodingStep::CompositeValues {
                record: index,
                format,
                width,
                endian,
                item_bits,
                max_items,
            } => {
                let source = record(records, index)?;
                let mut values = Vec::new();
                flatten_composite(source, format, &mut values)?;
                if values.len() > usize::from(max_items) {
                    return Err(StructuredEncodingVmError::CompositeTooLong {
                        len: values.len(),
                        max: max_items,
                    });
                }
                let mut value = 0_u32;
                for item in values {
                    if item < 0 || i128::from(item) >= (1_i128 << item_bits) {
                        return Err(StructuredEncodingVmError::ValueOutOfRange {
                            value: item,
                            bits: item_bits,
                        });
                    }
                    value = (value << item_bits) | item as u32;
                }
                emit(&mut out, value, width, endian);
            }
        }
    }
    Ok(out)
}
