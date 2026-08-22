// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral fixed-field and scalar emission programs carried by `SEMV` v2/v6.

use super::{
    OpcpuCodecError, SEMANTIC_VM_OPCODE_VERSION_V2, SEMANTIC_VM_OPCODE_VERSION_V6,
    SEMANTIC_VM_OPCODE_VERSION_V8,
};

pub const ENCODING_VM_OP_LITERAL: u8 = 0x01;
pub const ENCODING_VM_OP_SCALAR: u8 = 0x02;
pub const ENCODING_VM_OP_FIELDS: u8 = 0x03;
pub const ENCODING_VM_OP_INPUT_FIELDS: u8 = 0x04;
pub const ENCODING_VM_OP_INTEGER_TO_IEEE754: u8 = 0x05;
pub const ENCODING_VM_OP_END: u8 = 0xff;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EncodingEndian {
    Big,
    Little,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EncodingFieldSpec {
    pub input: u8,
    pub shift: u8,
    pub bits: u8,
    pub min: i64,
    pub max: i64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EncodingStep {
    Literal {
        value: u32,
        width: u8,
        endian: EncodingEndian,
    },
    Scalar {
        input: u8,
        width: u8,
        endian: EncodingEndian,
        min: i64,
        max: i64,
    },
    Fields {
        base: u32,
        width: u8,
        endian: EncodingEndian,
        fields: Vec<EncodingFieldSpec>,
    },
    InputFields {
        base_input: u8,
        width: u8,
        endian: EncodingEndian,
        fields: Vec<EncodingFieldSpec>,
    },
    IntegerToIeee754 {
        input: u8,
        width: u8,
        endian: EncodingEndian,
    },
}

fn invalid(detail: impl Into<String>) -> OpcpuCodecError {
    OpcpuCodecError::InvalidChunkFormat {
        chunk: "SEMV".to_string(),
        detail: detail.into(),
    }
}

fn width_mask(width: u8) -> Result<u64, OpcpuCodecError> {
    match width {
        1 => Ok(0xff),
        2 => Ok(0xffff),
        4 => Ok(0xffff_ffff),
        _ => Err(invalid(format!(
            "encoding VM width {width} is not 1, 2, or 4"
        ))),
    }
}

fn check_range(width: u8, min: i64, max: i64, context: &str) -> Result<(), OpcpuCodecError> {
    let mask = width_mask(width)?;
    let signed_min = -(1_i64 << (u32::from(width) * 8 - 1));
    if min > max || min < signed_min || (max as i128) > i128::from(mask) {
        return Err(invalid(format!(
            "encoding VM {context} range {min}..={max} is not encodable in {width} byte(s)"
        )));
    }
    Ok(())
}

fn push_header(out: &mut Vec<u8>, width: u8, endian: EncodingEndian) {
    out.push(width);
    out.push(match endian {
        EncodingEndian::Big => 0,
        EncodingEndian::Little => 1,
    });
}

fn field_range_tag(field: &EncodingFieldSpec) -> Result<u8, OpcpuCodecError> {
    let unsigned_max = ((1_i128 << field.bits) - 1) as i64;
    let signed_min = -(1_i64 << (field.bits - 1));
    let signed_max = (1_i64 << (field.bits - 1)) - 1;
    if field.min == 0 && field.max == unsigned_max {
        Ok(0)
    } else if field.min == signed_min && field.max == signed_max {
        Ok(1)
    } else {
        Err(invalid("encoding VM field range must be the natural signed or unsigned range for its bit width"))
    }
}

pub fn compile_encoding_program(steps: &[EncodingStep]) -> Result<Vec<u8>, OpcpuCodecError> {
    compile_encoding_program_for_version(SEMANTIC_VM_OPCODE_VERSION_V2, steps)
}

pub fn compile_parameterized_encoding_program(
    steps: &[EncodingStep],
) -> Result<Vec<u8>, OpcpuCodecError> {
    compile_encoding_program_for_version(SEMANTIC_VM_OPCODE_VERSION_V6, steps)
}

/// Compile a v8 numeric encoding program. The operation is CPU-neutral: it
/// converts an evaluated integer scalar to canonical IEEE-754 binary32 or
/// binary64 bytes, while packages retain ownership of when that conversion is
/// applicable.
pub fn compile_numeric_encoding_program(
    steps: &[EncodingStep],
) -> Result<Vec<u8>, OpcpuCodecError> {
    compile_encoding_program_for_version(SEMANTIC_VM_OPCODE_VERSION_V8, steps)
}

fn validate_ieee754_width(width: u8) -> Result<(), OpcpuCodecError> {
    if matches!(width, 4 | 8) {
        Ok(())
    } else {
        Err(invalid("encoding VM IEEE-754 width must be 4 or 8 bytes"))
    }
}

fn compile_encoding_program_for_version(
    opcode_version: u16,
    steps: &[EncodingStep],
) -> Result<Vec<u8>, OpcpuCodecError> {
    if steps.is_empty() {
        return Err(invalid(
            "encoding VM program must contain at least one step",
        ));
    }
    let mut out = Vec::new();
    for step in steps {
        match step {
            EncodingStep::Literal {
                value,
                width,
                endian,
            } => {
                let mask = width_mask(*width)?;
                if u64::from(*value) > mask {
                    return Err(invalid("encoding VM literal exceeds its output width"));
                }
                out.push(ENCODING_VM_OP_LITERAL);
                push_header(&mut out, *width, *endian);
                out.extend_from_slice(&value.to_le_bytes());
            }
            EncodingStep::Scalar {
                input,
                width,
                endian,
                min,
                max,
            } => {
                check_range(*width, *min, *max, "scalar")?;
                out.push(ENCODING_VM_OP_SCALAR);
                out.push(*input);
                push_header(&mut out, *width, *endian);
                out.extend_from_slice(&min.to_le_bytes());
                out.extend_from_slice(&max.to_le_bytes());
            }
            EncodingStep::Fields {
                base,
                width,
                endian,
                fields,
            } => {
                let mask = width_mask(*width)?;
                if u64::from(*base) > mask || fields.is_empty() || fields.len() > u8::MAX as usize {
                    return Err(invalid(
                        "encoding VM field step has invalid base or field count",
                    ));
                }
                let mut occupied = 0_u64;
                for field in fields {
                    if field.bits == 0
                        || u16::from(field.shift) + u16::from(field.bits) > u16::from(*width) * 8
                    {
                        return Err(invalid("encoding VM field exceeds its output unit"));
                    }
                    let field_mask = ((1_u64 << field.bits) - 1) << field.shift;
                    if occupied & field_mask != 0 {
                        return Err(invalid("encoding VM fields overlap"));
                    }
                    occupied |= field_mask;
                    let field_width = field.bits.div_ceil(8);
                    check_range(field_width, field.min, field.max, "field")?;
                    if field.max as i128 > ((1_i128 << field.bits) - 1) {
                        return Err(invalid("encoding VM field maximum exceeds its bit width"));
                    }
                    let signed_min = -(1_i64 << (field.bits - 1));
                    if field.min < signed_min {
                        return Err(invalid("encoding VM field minimum exceeds its bit width"));
                    }
                    field_range_tag(field)?;
                }
                out.push(ENCODING_VM_OP_FIELDS);
                push_header(&mut out, *width, *endian);
                out.extend_from_slice(&base.to_le_bytes());
                out.push(fields.len() as u8);
                for field in fields {
                    out.extend_from_slice(&[field.input, field.shift, field.bits]);
                    out.push(field_range_tag(field)?);
                }
            }
            EncodingStep::InputFields {
                base_input,
                width,
                endian,
                fields,
            } => {
                if opcode_version != SEMANTIC_VM_OPCODE_VERSION_V6
                    && opcode_version != SEMANTIC_VM_OPCODE_VERSION_V8
                {
                    return Err(invalid(
                        "encoding VM input-field steps require opcode version 6",
                    ));
                }
                validate_fields(*width, fields)?;
                out.push(ENCODING_VM_OP_INPUT_FIELDS);
                out.push(*base_input);
                push_header(&mut out, *width, *endian);
                out.push(fields.len() as u8);
                encode_fields(&mut out, fields)?;
            }
            EncodingStep::IntegerToIeee754 {
                input,
                width,
                endian,
            } => {
                if opcode_version != SEMANTIC_VM_OPCODE_VERSION_V8 {
                    return Err(invalid(
                        "encoding VM IEEE-754 conversion requires opcode version 8",
                    ));
                }
                validate_ieee754_width(*width)?;
                out.extend_from_slice(&[
                    ENCODING_VM_OP_INTEGER_TO_IEEE754,
                    *input,
                    *width,
                    u8::from(*endian == EncodingEndian::Little),
                ]);
            }
        }
    }
    out.push(ENCODING_VM_OP_END);
    validate_encoding_program(opcode_version, &out)?;
    Ok(out)
}

pub fn decode_encoding_program(
    opcode_version: u16,
    bytes: &[u8],
) -> Result<Vec<EncodingStep>, OpcpuCodecError> {
    if opcode_version != SEMANTIC_VM_OPCODE_VERSION_V2
        && opcode_version != SEMANTIC_VM_OPCODE_VERSION_V6
        && opcode_version != SEMANTIC_VM_OPCODE_VERSION_V8
    {
        return Err(invalid(format!(
            "unsupported encoding VM opcode version {opcode_version}"
        )));
    }
    let mut pc = 0;
    let mut steps = Vec::new();
    let take_u8 = |pc: &mut usize| -> Result<u8, OpcpuCodecError> {
        let value = *bytes
            .get(*pc)
            .ok_or_else(|| invalid("encoding VM program is truncated"))?;
        *pc += 1;
        Ok(value)
    };
    let take_u32 = |pc: &mut usize| -> Result<u32, OpcpuCodecError> {
        let end = pc
            .checked_add(4)
            .filter(|end| *end <= bytes.len())
            .ok_or_else(|| invalid("encoding VM u32 is truncated"))?;
        let value = u32::from_le_bytes(bytes[*pc..end].try_into().expect("four bytes"));
        *pc = end;
        Ok(value)
    };
    let take_i64 = |pc: &mut usize| -> Result<i64, OpcpuCodecError> {
        let end = pc
            .checked_add(8)
            .filter(|end| *end <= bytes.len())
            .ok_or_else(|| invalid("encoding VM i64 is truncated"))?;
        let value = i64::from_le_bytes(bytes[*pc..end].try_into().expect("eight bytes"));
        *pc = end;
        Ok(value)
    };
    loop {
        let opcode = take_u8(&mut pc)?;
        if opcode == ENCODING_VM_OP_END {
            if pc != bytes.len() || steps.is_empty() {
                return Err(invalid("encoding VM has trailing bytes or no steps"));
            }
            return Ok(steps);
        }
        let read_header = |pc: &mut usize| -> Result<(u8, EncodingEndian), OpcpuCodecError> {
            let width = take_u8(pc)?;
            width_mask(width)?;
            let endian = match take_u8(pc)? {
                0 => EncodingEndian::Big,
                1 => EncodingEndian::Little,
                value => {
                    return Err(invalid(format!(
                        "encoding VM endian tag {value} is invalid"
                    )))
                }
            };
            Ok((width, endian))
        };
        match opcode {
            ENCODING_VM_OP_LITERAL => {
                let (width, endian) = read_header(&mut pc)?;
                steps.push(EncodingStep::Literal {
                    value: take_u32(&mut pc)?,
                    width,
                    endian,
                });
            }
            ENCODING_VM_OP_SCALAR => {
                let input = take_u8(&mut pc)?;
                let (width, endian) = read_header(&mut pc)?;
                steps.push(EncodingStep::Scalar {
                    input,
                    width,
                    endian,
                    min: take_i64(&mut pc)?,
                    max: take_i64(&mut pc)?,
                });
            }
            ENCODING_VM_OP_FIELDS => {
                let (width, endian) = read_header(&mut pc)?;
                let base = take_u32(&mut pc)?;
                let count = take_u8(&mut pc)?;
                let mut fields = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    let input = take_u8(&mut pc)?;
                    let shift = take_u8(&mut pc)?;
                    let bits = take_u8(&mut pc)?;
                    if bits == 0 || bits > 32 {
                        return Err(invalid("encoding VM field bit width is invalid"));
                    }
                    let (min, max) = match take_u8(&mut pc)? {
                        0 => (0, ((1_i128 << bits) - 1) as i64),
                        1 => (-(1_i64 << (bits - 1)), (1_i64 << (bits - 1)) - 1),
                        value => {
                            return Err(invalid(format!(
                                "encoding VM field range tag {value} is invalid"
                            )))
                        }
                    };
                    fields.push(EncodingFieldSpec {
                        input,
                        shift,
                        bits,
                        min,
                        max,
                    });
                }
                steps.push(EncodingStep::Fields {
                    base,
                    width,
                    endian,
                    fields,
                });
            }
            ENCODING_VM_OP_INPUT_FIELDS => {
                if opcode_version != SEMANTIC_VM_OPCODE_VERSION_V6
                    && opcode_version != SEMANTIC_VM_OPCODE_VERSION_V8
                {
                    return Err(invalid(
                        "encoding VM input-field opcode requires opcode version 6",
                    ));
                }
                let base_input = take_u8(&mut pc)?;
                let (width, endian) = read_header(&mut pc)?;
                let count = take_u8(&mut pc)?;
                let fields = decode_fields(&mut pc, count, &take_u8)?;
                steps.push(EncodingStep::InputFields {
                    base_input,
                    width,
                    endian,
                    fields,
                });
            }
            ENCODING_VM_OP_INTEGER_TO_IEEE754 => {
                if opcode_version != SEMANTIC_VM_OPCODE_VERSION_V8 {
                    return Err(invalid(
                        "encoding VM IEEE-754 opcode requires opcode version 8",
                    ));
                }
                let input = take_u8(&mut pc)?;
                let width = take_u8(&mut pc)?;
                validate_ieee754_width(width)?;
                let endian = match take_u8(&mut pc)? {
                    0 => EncodingEndian::Big,
                    1 => EncodingEndian::Little,
                    value => {
                        return Err(invalid(format!(
                            "encoding VM endian tag {value} is invalid"
                        )))
                    }
                };
                steps.push(EncodingStep::IntegerToIeee754 {
                    input,
                    width,
                    endian,
                });
            }
            _ => {
                return Err(invalid(format!(
                    "invalid encoding VM opcode 0x{opcode:02x}"
                )))
            }
        }
    }
}

pub fn validate_encoding_program(opcode_version: u16, bytes: &[u8]) -> Result<(), OpcpuCodecError> {
    let decoded = decode_encoding_program(opcode_version, bytes)?;
    compile_encoding_program_unchecked_validation(&decoded)
}

fn compile_encoding_program_unchecked_validation(
    steps: &[EncodingStep],
) -> Result<(), OpcpuCodecError> {
    for step in steps {
        match step {
            EncodingStep::Literal { value, width, .. } => {
                if u64::from(*value) > width_mask(*width)? {
                    return Err(invalid("encoding VM literal exceeds its output width"));
                }
            }
            EncodingStep::Scalar {
                width, min, max, ..
            } => check_range(*width, *min, *max, "scalar")?,
            EncodingStep::Fields {
                base,
                width,
                fields,
                ..
            } => {
                if u64::from(*base) > width_mask(*width)? || fields.is_empty() {
                    return Err(invalid(
                        "encoding VM field step has invalid base or field count",
                    ));
                }
                let mut occupied = 0_u64;
                for field in fields {
                    if field.bits == 0
                        || u16::from(field.shift) + u16::from(field.bits) > u16::from(*width) * 8
                    {
                        return Err(invalid("encoding VM field exceeds its output unit"));
                    }
                    let mask = ((1_u64 << field.bits) - 1) << field.shift;
                    if occupied & mask != 0 {
                        return Err(invalid("encoding VM fields overlap"));
                    }
                    occupied |= mask;
                    if field.min > field.max
                        || field.min < -(1_i64 << (field.bits - 1))
                        || field.max as i128 > (1_i128 << field.bits) - 1
                    {
                        return Err(invalid("encoding VM field range exceeds its bit width"));
                    }
                    field_range_tag(field)?;
                }
            }
            EncodingStep::InputFields { width, fields, .. } => {
                validate_fields(*width, fields)?;
            }
            EncodingStep::IntegerToIeee754 { width, .. } => {
                validate_ieee754_width(*width)?;
            }
        }
    }
    Ok(())
}

fn validate_fields(width: u8, fields: &[EncodingFieldSpec]) -> Result<(), OpcpuCodecError> {
    width_mask(width)?;
    if fields.is_empty() || fields.len() > u8::MAX as usize {
        return Err(invalid("encoding VM field step has invalid field count"));
    }
    let mut occupied = 0_u64;
    for field in fields {
        if field.bits == 0 || u16::from(field.shift) + u16::from(field.bits) > u16::from(width) * 8
        {
            return Err(invalid("encoding VM field exceeds its output unit"));
        }
        let mask = ((1_u64 << field.bits) - 1) << field.shift;
        if occupied & mask != 0 {
            return Err(invalid("encoding VM fields overlap"));
        }
        occupied |= mask;
        if field.min > field.max
            || field.min < -(1_i64 << (field.bits - 1))
            || field.max as i128 > (1_i128 << field.bits) - 1
        {
            return Err(invalid("encoding VM field range exceeds its bit width"));
        }
        field_range_tag(field)?;
    }
    Ok(())
}

fn encode_fields(out: &mut Vec<u8>, fields: &[EncodingFieldSpec]) -> Result<(), OpcpuCodecError> {
    for field in fields {
        out.extend_from_slice(&[field.input, field.shift, field.bits]);
        out.push(field_range_tag(field)?);
    }
    Ok(())
}

fn decode_fields(
    pc: &mut usize,
    count: u8,
    take_u8: &impl Fn(&mut usize) -> Result<u8, OpcpuCodecError>,
) -> Result<Vec<EncodingFieldSpec>, OpcpuCodecError> {
    let mut fields = Vec::with_capacity(count as usize);
    for _ in 0..count {
        let input = take_u8(pc)?;
        let shift = take_u8(pc)?;
        let bits = take_u8(pc)?;
        if bits == 0 || bits > 32 {
            return Err(invalid("encoding VM field bit width is invalid"));
        }
        let (min, max) = match take_u8(pc)? {
            0 => (0, ((1_i128 << bits) - 1) as i64),
            1 => (-(1_i64 << (bits - 1)), (1_i64 << (bits - 1)) - 1),
            value => {
                return Err(invalid(format!(
                    "encoding VM field range tag {value} is invalid"
                )))
            }
        };
        fields.push(EncodingFieldSpec {
            input,
            shift,
            bits,
            min,
            max,
        });
    }
    Ok(fields)
}
