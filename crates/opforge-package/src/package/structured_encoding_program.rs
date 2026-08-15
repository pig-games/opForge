// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral structured-record emission programs carried by `SEMV` v3.

use super::{EncodingEndian, OpcpuCodecError, SEMANTIC_VM_OPCODE_VERSION_V3};

const OP_MASK: u8 = 1;
const OP_PAIR: u8 = 2;
const OP_FIELD: u8 = 3;
const OP_COMPOSITE: u8 = 4;
const OP_END: u8 = 0xff;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RegisterClassProjection {
    pub class: u16,
    pub offset: u8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StructuredEncodingStep {
    RegisterMask {
        record: u8,
        width: u8,
        endian: EncodingEndian,
        reverse_bits: bool,
        classes: Vec<RegisterClassProjection>,
    },
    RegisterPair {
        record: u8,
        base: u32,
        width: u8,
        endian: EncodingEndian,
        left_shift: u8,
        right_shift: u8,
        bits: u8,
        indirect: Option<bool>,
    },
    FieldSelectors {
        record: u8,
        base: u32,
        width: u8,
        endian: EncodingEndian,
        offset_shift: u8,
        width_shift: u8,
        bits: u8,
        offset_full_width_zero: bool,
        width_full_width_zero: bool,
    },
    CompositeValues {
        record: u8,
        format: u16,
        width: u8,
        endian: EncodingEndian,
        item_bits: u8,
        max_items: u8,
    },
}

fn invalid(detail: impl Into<String>) -> OpcpuCodecError {
    OpcpuCodecError::InvalidChunkFormat {
        chunk: "SEMV".to_string(),
        detail: detail.into(),
    }
}

fn endian_tag(endian: EncodingEndian) -> u8 {
    if endian == EncodingEndian::Big {
        0
    } else {
        1
    }
}
fn decode_endian(tag: u8) -> Result<EncodingEndian, OpcpuCodecError> {
    match tag {
        0 => Ok(EncodingEndian::Big),
        1 => Ok(EncodingEndian::Little),
        _ => Err(invalid("structured encoding endian tag is invalid")),
    }
}
fn width_bits(width: u8) -> Result<u8, OpcpuCodecError> {
    match width {
        1 | 2 | 4 => Ok(width * 8),
        _ => Err(invalid("structured encoding width must be 1, 2, or 4")),
    }
}
fn check_fields(width: u8, shifts: &[u8], bits: u8) -> Result<(), OpcpuCodecError> {
    let unit_bits = width_bits(width)?;
    if bits == 0
        || bits > 16
        || shifts
            .iter()
            .any(|shift| shift.saturating_add(bits) > unit_bits)
    {
        return Err(invalid("structured encoding field exceeds its output unit"));
    }
    if shifts.len() == 2 {
        let left = ((1_u64 << bits) - 1) << shifts[0];
        let right = ((1_u64 << bits) - 1) << shifts[1];
        if left & right != 0 {
            return Err(invalid("structured encoding fields overlap"));
        }
    }
    Ok(())
}

fn check_selectors(width: u8, shifts: &[u8], bits: u8) -> Result<(), OpcpuCodecError> {
    let unit_bits = width_bits(width)?;
    if bits == 0
        || bits > 15
        || shifts
            .iter()
            .any(|shift| shift.saturating_add(bits) >= unit_bits)
    {
        return Err(invalid(
            "structured selector value and kind marker exceed the output unit",
        ));
    }
    let left = ((1_u64 << (bits + 1)) - 1) << shifts[0];
    let right = ((1_u64 << (bits + 1)) - 1) << shifts[1];
    if left & right != 0 {
        return Err(invalid("structured selector fields overlap"));
    }
    Ok(())
}

fn push_classes(
    out: &mut Vec<u8>,
    classes: &[RegisterClassProjection],
) -> Result<(), OpcpuCodecError> {
    if classes.is_empty() || classes.len() > u8::MAX as usize {
        return Err(invalid("structured mask class table is empty or too large"));
    }
    let mut seen = std::collections::HashSet::new();
    if classes.iter().any(|entry| !seen.insert(entry.class)) {
        return Err(invalid("structured mask class table has duplicate classes"));
    }
    out.push(classes.len() as u8);
    for entry in classes {
        out.extend_from_slice(&entry.class.to_le_bytes());
        out.push(entry.offset);
    }
    Ok(())
}

pub fn compile_structured_encoding_program(
    steps: &[StructuredEncodingStep],
) -> Result<Vec<u8>, OpcpuCodecError> {
    if steps.is_empty() {
        return Err(invalid("structured encoding program must not be empty"));
    }
    let mut out = Vec::new();
    for step in steps {
        match step {
            StructuredEncodingStep::RegisterMask {
                record,
                width,
                endian,
                reverse_bits,
                classes,
            } => {
                width_bits(*width)?;
                out.extend_from_slice(&[
                    OP_MASK,
                    *record,
                    *width,
                    endian_tag(*endian),
                    u8::from(*reverse_bits),
                ]);
                push_classes(&mut out, classes)?;
            }
            StructuredEncodingStep::RegisterPair {
                record,
                base,
                width,
                endian,
                left_shift,
                right_shift,
                bits,
                indirect,
            } => {
                check_fields(*width, &[*left_shift, *right_shift], *bits)?;
                if u64::from(*base) >= (1_u64 << width_bits(*width)?) {
                    return Err(invalid("structured pair base exceeds output width"));
                }
                out.extend_from_slice(&[OP_PAIR, *record, *width, endian_tag(*endian)]);
                out.extend_from_slice(&base.to_le_bytes());
                out.extend_from_slice(&[
                    *left_shift,
                    *right_shift,
                    *bits,
                    match indirect {
                        None => 0,
                        Some(false) => 1,
                        Some(true) => 2,
                    },
                ]);
            }
            StructuredEncodingStep::FieldSelectors {
                record,
                base,
                width,
                endian,
                offset_shift,
                width_shift,
                bits,
                offset_full_width_zero,
                width_full_width_zero,
            } => {
                check_selectors(*width, &[*offset_shift, *width_shift], *bits)?;
                if u64::from(*base) >= (1_u64 << width_bits(*width)?) {
                    return Err(invalid("structured field base exceeds output width"));
                }
                out.extend_from_slice(&[OP_FIELD, *record, *width, endian_tag(*endian)]);
                out.extend_from_slice(&base.to_le_bytes());
                out.extend_from_slice(&[
                    *offset_shift,
                    *width_shift,
                    *bits,
                    u8::from(*offset_full_width_zero) | (u8::from(*width_full_width_zero) << 1),
                ]);
            }
            StructuredEncodingStep::CompositeValues {
                record,
                format,
                width,
                endian,
                item_bits,
                max_items,
            } => {
                let unit_bits = width_bits(*width)?;
                if *item_bits == 0
                    || *max_items == 0
                    || u16::from(*item_bits) * u16::from(*max_items) > u16::from(unit_bits)
                {
                    return Err(invalid("structured composite values exceed output width"));
                }
                out.extend_from_slice(&[OP_COMPOSITE, *record, *width, endian_tag(*endian)]);
                out.extend_from_slice(&format.to_le_bytes());
                out.extend_from_slice(&[*item_bits, *max_items]);
            }
        }
    }
    out.push(OP_END);
    validate_structured_encoding_program(SEMANTIC_VM_OPCODE_VERSION_V3, &out)?;
    Ok(out)
}

pub fn decode_structured_encoding_program(
    version: u16,
    bytes: &[u8],
) -> Result<Vec<StructuredEncodingStep>, OpcpuCodecError> {
    if version != SEMANTIC_VM_OPCODE_VERSION_V3 {
        return Err(invalid(format!(
            "unsupported structured encoding VM version {version}"
        )));
    }
    let mut pc = 0usize;
    let mut steps = Vec::new();
    let take = |pc: &mut usize| -> Result<u8, OpcpuCodecError> {
        let v = *bytes
            .get(*pc)
            .ok_or_else(|| invalid("structured encoding program is truncated"))?;
        *pc += 1;
        Ok(v)
    };
    let take_u32 = |pc: &mut usize| -> Result<u32, OpcpuCodecError> {
        let end = pc
            .checked_add(4)
            .filter(|end| *end <= bytes.len())
            .ok_or_else(|| invalid("structured encoding base is truncated"))?;
        let value = u32::from_le_bytes(bytes[*pc..end].try_into().expect("four bytes"));
        *pc = end;
        Ok(value)
    };
    loop {
        let op = take(&mut pc)?;
        if op == OP_END {
            if steps.is_empty() || pc != bytes.len() {
                return Err(invalid(
                    "structured encoding program is empty or has trailing bytes",
                ));
            }
            return Ok(steps);
        }
        let record = take(&mut pc)?;
        let width = take(&mut pc)?;
        let endian = decode_endian(take(&mut pc)?)?;
        steps.push(match op {
            OP_MASK => StructuredEncodingStep::RegisterMask {
                record,
                width,
                endian,
                reverse_bits: match take(&mut pc)? {
                    0 => false,
                    1 => true,
                    _ => return Err(invalid("structured mask flag is invalid")),
                },
                classes: {
                    let count = take(&mut pc)?;
                    if count == 0 {
                        return Err(invalid("structured mask class table is empty"));
                    }
                    let mut classes = Vec::with_capacity(count as usize);
                    for _ in 0..count {
                        classes.push(RegisterClassProjection {
                            class: u16::from_le_bytes([take(&mut pc)?, take(&mut pc)?]),
                            offset: take(&mut pc)?,
                        });
                    }
                    classes
                },
            },
            OP_PAIR => {
                let base = take_u32(&mut pc)?;
                StructuredEncodingStep::RegisterPair {
                    record,
                    base,
                    width,
                    endian,
                    left_shift: take(&mut pc)?,
                    right_shift: take(&mut pc)?,
                    bits: take(&mut pc)?,
                    indirect: match take(&mut pc)? {
                        0 => None,
                        1 => Some(false),
                        2 => Some(true),
                        _ => return Err(invalid("structured pair indirect tag is invalid")),
                    },
                }
            }
            OP_FIELD => {
                let base = take_u32(&mut pc)?;
                let offset_shift = take(&mut pc)?;
                let width_shift = take(&mut pc)?;
                let bits = take(&mut pc)?;
                let flags = take(&mut pc)?;
                if flags & !0x03 != 0 {
                    return Err(invalid("structured selector flags are invalid"));
                }
                StructuredEncodingStep::FieldSelectors {
                    record,
                    base,
                    width,
                    endian,
                    offset_shift,
                    width_shift,
                    bits,
                    offset_full_width_zero: flags & 0x01 != 0,
                    width_full_width_zero: flags & 0x02 != 0,
                }
            }
            OP_COMPOSITE => StructuredEncodingStep::CompositeValues {
                record,
                format: u16::from_le_bytes([take(&mut pc)?, take(&mut pc)?]),
                width,
                endian,
                item_bits: take(&mut pc)?,
                max_items: take(&mut pc)?,
            },
            _ => {
                return Err(invalid(format!(
                    "invalid structured encoding opcode 0x{op:02x}"
                )))
            }
        });
    }
}

pub fn validate_structured_encoding_program(
    version: u16,
    bytes: &[u8],
) -> Result<(), OpcpuCodecError> {
    let decoded = decode_structured_encoding_program(version, bytes)?;
    // Recompilation performs all width, overlap, base, and cardinality checks.
    let rebuilt = compile_structured_encoding_program_unchecked(&decoded)?;
    if rebuilt != bytes {
        return Err(invalid("structured encoding program is noncanonical"));
    }
    Ok(())
}

fn compile_structured_encoding_program_unchecked(
    steps: &[StructuredEncodingStep],
) -> Result<Vec<u8>, OpcpuCodecError> {
    // Avoid recursive validation while retaining the compiler as the canonical validator.
    let mut out = Vec::new();
    for step in steps {
        match step {
            StructuredEncodingStep::RegisterMask {
                record,
                width,
                endian,
                reverse_bits,
                classes,
            } => {
                width_bits(*width)?;
                out.extend_from_slice(&[
                    OP_MASK,
                    *record,
                    *width,
                    endian_tag(*endian),
                    u8::from(*reverse_bits),
                ]);
                push_classes(&mut out, classes)?;
            }
            StructuredEncodingStep::RegisterPair {
                record,
                base,
                width,
                endian,
                left_shift,
                right_shift,
                bits,
                indirect,
            } => {
                check_fields(*width, &[*left_shift, *right_shift], *bits)?;
                if u64::from(*base) >= (1_u64 << width_bits(*width)?) {
                    return Err(invalid("structured pair base exceeds output width"));
                }
                out.extend_from_slice(&[OP_PAIR, *record, *width, endian_tag(*endian)]);
                out.extend_from_slice(&base.to_le_bytes());
                out.extend_from_slice(&[
                    *left_shift,
                    *right_shift,
                    *bits,
                    match indirect {
                        None => 0,
                        Some(false) => 1,
                        Some(true) => 2,
                    },
                ]);
            }
            StructuredEncodingStep::FieldSelectors {
                record,
                base,
                width,
                endian,
                offset_shift,
                width_shift,
                bits,
                offset_full_width_zero,
                width_full_width_zero,
            } => {
                check_selectors(*width, &[*offset_shift, *width_shift], *bits)?;
                if u64::from(*base) >= (1_u64 << width_bits(*width)?) {
                    return Err(invalid("structured field base exceeds output width"));
                }
                out.extend_from_slice(&[OP_FIELD, *record, *width, endian_tag(*endian)]);
                out.extend_from_slice(&base.to_le_bytes());
                out.extend_from_slice(&[
                    *offset_shift,
                    *width_shift,
                    *bits,
                    u8::from(*offset_full_width_zero) | (u8::from(*width_full_width_zero) << 1),
                ]);
            }
            StructuredEncodingStep::CompositeValues {
                record,
                format,
                width,
                endian,
                item_bits,
                max_items,
            } => {
                let unit = width_bits(*width)?;
                if *item_bits == 0
                    || *max_items == 0
                    || u16::from(*item_bits) * u16::from(*max_items) > u16::from(unit)
                {
                    return Err(invalid("structured composite values exceed output width"));
                }
                out.extend_from_slice(&[OP_COMPOSITE, *record, *width, endian_tag(*endian)]);
                out.extend_from_slice(&format.to_le_bytes());
                out.extend_from_slice(&[*item_bits, *max_items]);
            }
        }
    }
    out.push(OP_END);
    Ok(out)
}
