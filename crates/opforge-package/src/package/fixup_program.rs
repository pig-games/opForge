// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral runtime-position and deferred-value programs carried by `SEMV` v4.

use super::{
    EncodingEndian, OpcpuCodecError, SEMANTIC_VM_OPCODE_VERSION_V4, SEMANTIC_VM_OPCODE_VERSION_V7,
};

const OP_PROJECT: u8 = 0x01;
const OP_END: u8 = 0xff;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FixupBase {
    Value,
    Position {
        adjustment: i32,
        target_references_only: bool,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FixupRange {
    Signed,
    Unsigned,
    BitPattern,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnresolvedValuePolicy {
    Reject,
    Placeholder(i32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PortableRelocationKind {
    None,
    Absolute,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FixupEncodingStep {
    pub input: u8,
    pub width: u8,
    pub endian: EncodingEndian,
    pub base: FixupBase,
    pub range: FixupRange,
    pub unresolved: UnresolvedValuePolicy,
    pub relocation: PortableRelocationKind,
    pub transform: FixupTransform,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FixupTransform {
    Identity,
    AlignedBitOr {
        alignment: u32,
        mask: u32,
    },
    RangeMap {
        alignment: u32,
        mappings: Vec<FixupRangeMapping>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FixupRangeMapping {
    pub min: i64,
    pub max: i64,
    pub adjustment: i64,
}

fn invalid(detail: impl Into<String>) -> OpcpuCodecError {
    OpcpuCodecError::InvalidChunkFormat {
        chunk: "SEMV".to_string(),
        detail: detail.into(),
    }
}

fn width_bits(width: u8) -> Result<u32, OpcpuCodecError> {
    match width {
        1 | 2 | 4 => Ok(u32::from(width) * 8),
        _ => Err(invalid("fixup encoding width must be 1, 2, or 4")),
    }
}

fn range_bounds(width: u8, range: FixupRange) -> Result<(i64, i64), OpcpuCodecError> {
    let bits = width_bits(width)?;
    Ok(match range {
        FixupRange::Signed => (-(1_i64 << (bits - 1)), (1_i64 << (bits - 1)) - 1),
        FixupRange::Unsigned => (0, ((1_u64 << bits) - 1) as i64),
        FixupRange::BitPattern => (-(1_i64 << (bits - 1)), ((1_u64 << bits) - 1) as i64),
    })
}

fn range_tag(range: FixupRange) -> u8 {
    match range {
        FixupRange::Signed => 0,
        FixupRange::Unsigned => 1,
        FixupRange::BitPattern => 2,
    }
}

fn decode_range(tag: u8) -> Result<FixupRange, OpcpuCodecError> {
    match tag {
        0 => Ok(FixupRange::Signed),
        1 => Ok(FixupRange::Unsigned),
        2 => Ok(FixupRange::BitPattern),
        _ => Err(invalid("fixup encoding range tag is invalid")),
    }
}

pub fn compile_fixup_program(steps: &[FixupEncodingStep]) -> Result<Vec<u8>, OpcpuCodecError> {
    if steps
        .iter()
        .any(|step| step.transform != FixupTransform::Identity)
    {
        return Err(invalid("fixup transforms require fixup VM v7"));
    }
    compile_fixup_program_for_version(SEMANTIC_VM_OPCODE_VERSION_V4, steps)
}

pub fn compile_projected_fixup_program(
    steps: &[FixupEncodingStep],
) -> Result<Vec<u8>, OpcpuCodecError> {
    compile_fixup_program_for_version(SEMANTIC_VM_OPCODE_VERSION_V7, steps)
}

fn compile_fixup_program_for_version(
    version: u16,
    steps: &[FixupEncodingStep],
) -> Result<Vec<u8>, OpcpuCodecError> {
    if steps.is_empty() {
        return Err(invalid("fixup encoding program must not be empty"));
    }
    let mut out = Vec::new();
    for step in steps {
        let (min, max) = range_bounds(step.width, step.range)?;
        if let UnresolvedValuePolicy::Placeholder(value) = step.unresolved {
            let value = i64::from(value);
            if value < min || value > max {
                return Err(invalid("fixup placeholder is outside the declared range"));
            }
        }
        out.extend_from_slice(&[
            OP_PROJECT,
            step.input,
            step.width,
            match step.endian {
                EncodingEndian::Big => 0,
                EncodingEndian::Little => 1,
            },
        ]);
        match step.base {
            FixupBase::Value => {
                out.push(0);
                out.extend_from_slice(&0_i32.to_le_bytes());
            }
            FixupBase::Position {
                adjustment,
                target_references_only: false,
            } => {
                out.push(1);
                out.extend_from_slice(&adjustment.to_le_bytes());
            }
            FixupBase::Position {
                adjustment,
                target_references_only: true,
            } => {
                out.push(2);
                out.extend_from_slice(&adjustment.to_le_bytes());
            }
        }
        match step.unresolved {
            UnresolvedValuePolicy::Reject => {
                out.push(0);
                out.extend_from_slice(&0_i32.to_le_bytes());
            }
            UnresolvedValuePolicy::Placeholder(value) => {
                out.push(1);
                out.extend_from_slice(&value.to_le_bytes());
            }
        }
        out.extend_from_slice(&[
            range_tag(step.range),
            match step.relocation {
                PortableRelocationKind::None => 0,
                PortableRelocationKind::Absolute => 1,
            },
        ]);
        if version == SEMANTIC_VM_OPCODE_VERSION_V7 {
            encode_transform(&mut out, step.width, &step.transform)?;
        }
    }
    out.push(OP_END);
    validate_fixup_program(version, &out)?;
    Ok(out)
}

fn validate_alignment(alignment: u32) -> Result<(), OpcpuCodecError> {
    if alignment == 0 || !alignment.is_power_of_two() {
        return Err(invalid(
            "fixup transform alignment must be a nonzero power of two",
        ));
    }
    Ok(())
}

fn encode_transform(
    out: &mut Vec<u8>,
    width: u8,
    transform: &FixupTransform,
) -> Result<(), OpcpuCodecError> {
    match transform {
        FixupTransform::Identity => out.push(0),
        FixupTransform::AlignedBitOr { alignment, mask } => {
            validate_alignment(*alignment)?;
            let width_mask = match width {
                1 => 0xff,
                2 => 0xffff,
                4 => u32::MAX,
                _ => unreachable!("fixup width already validated"),
            };
            if mask & !width_mask != 0 {
                return Err(invalid("fixup transform bit mask exceeds output width"));
            }
            out.push(1);
            out.extend_from_slice(&alignment.to_le_bytes());
            out.extend_from_slice(&mask.to_le_bytes());
        }
        FixupTransform::RangeMap {
            alignment,
            mappings,
        } => {
            validate_alignment(*alignment)?;
            let count = u8::try_from(mappings.len())
                .map_err(|_| invalid("fixup range-map count exceeds u8"))?;
            if count == 0 {
                return Err(invalid("fixup range-map transform must not be empty"));
            }
            let mut previous_max = None;
            for mapping in mappings {
                if mapping.min > mapping.max || previous_max.is_some_and(|max| mapping.min <= max) {
                    return Err(invalid(
                        "fixup range-map entries must be ordered, nonempty, and disjoint",
                    ));
                }
                mapping
                    .min
                    .checked_add(mapping.adjustment)
                    .and_then(|_| mapping.max.checked_add(mapping.adjustment))
                    .ok_or_else(|| invalid("fixup range-map adjustment overflows i64"))?;
                previous_max = Some(mapping.max);
            }
            out.push(2);
            out.extend_from_slice(&alignment.to_le_bytes());
            out.push(count);
            for mapping in mappings {
                out.extend_from_slice(&mapping.min.to_le_bytes());
                out.extend_from_slice(&mapping.max.to_le_bytes());
                out.extend_from_slice(&mapping.adjustment.to_le_bytes());
            }
        }
    }
    Ok(())
}

pub fn decode_fixup_program(
    version: u16,
    bytes: &[u8],
) -> Result<Vec<FixupEncodingStep>, OpcpuCodecError> {
    if !matches!(
        version,
        SEMANTIC_VM_OPCODE_VERSION_V4 | SEMANTIC_VM_OPCODE_VERSION_V7
    ) {
        return Err(invalid(format!(
            "unsupported fixup encoding VM version {version}"
        )));
    }
    let mut pc = 0usize;
    let mut steps = Vec::new();
    let take_u8 = |pc: &mut usize| -> Result<u8, OpcpuCodecError> {
        let value = *bytes
            .get(*pc)
            .ok_or_else(|| invalid("fixup encoding program is truncated"))?;
        *pc += 1;
        Ok(value)
    };
    let take_i32 = |pc: &mut usize| -> Result<i32, OpcpuCodecError> {
        let end = pc
            .checked_add(4)
            .filter(|end| *end <= bytes.len())
            .ok_or_else(|| invalid("fixup encoding i32 is truncated"))?;
        let value = i32::from_le_bytes(bytes[*pc..end].try_into().expect("four bytes"));
        *pc = end;
        Ok(value)
    };
    let take_u32 = |pc: &mut usize| -> Result<u32, OpcpuCodecError> {
        let end = pc
            .checked_add(4)
            .filter(|end| *end <= bytes.len())
            .ok_or_else(|| invalid("fixup encoding u32 is truncated"))?;
        let value = u32::from_le_bytes(bytes[*pc..end].try_into().expect("four bytes"));
        *pc = end;
        Ok(value)
    };
    let take_i64 = |pc: &mut usize| -> Result<i64, OpcpuCodecError> {
        let end = pc
            .checked_add(8)
            .filter(|end| *end <= bytes.len())
            .ok_or_else(|| invalid("fixup encoding i64 is truncated"))?;
        let value = i64::from_le_bytes(bytes[*pc..end].try_into().expect("eight bytes"));
        *pc = end;
        Ok(value)
    };
    loop {
        match take_u8(&mut pc)? {
            OP_END if steps.is_empty() => {
                return Err(invalid("fixup encoding program has no steps"));
            }
            OP_END if pc != bytes.len() => {
                return Err(invalid("fixup encoding program has trailing bytes"));
            }
            OP_END => return Ok(steps),
            OP_PROJECT => {
                let input = take_u8(&mut pc)?;
                let width = take_u8(&mut pc)?;
                width_bits(width)?;
                let endian = match take_u8(&mut pc)? {
                    0 => EncodingEndian::Big,
                    1 => EncodingEndian::Little,
                    _ => return Err(invalid("fixup encoding endian tag is invalid")),
                };
                let base_tag = take_u8(&mut pc)?;
                let adjustment = take_i32(&mut pc)?;
                let base = match base_tag {
                    0 if adjustment == 0 => FixupBase::Value,
                    0 => return Err(invalid("value fixup base has a nonzero adjustment")),
                    1 => FixupBase::Position {
                        adjustment,
                        target_references_only: false,
                    },
                    2 => FixupBase::Position {
                        adjustment,
                        target_references_only: true,
                    },
                    _ => return Err(invalid("fixup encoding base tag is invalid")),
                };
                let unresolved_tag = take_u8(&mut pc)?;
                let placeholder = take_i32(&mut pc)?;
                let unresolved = match unresolved_tag {
                    0 if placeholder == 0 => UnresolvedValuePolicy::Reject,
                    0 => return Err(invalid("reject policy has a nonzero placeholder")),
                    1 => UnresolvedValuePolicy::Placeholder(placeholder),
                    _ => return Err(invalid("fixup unresolved-policy tag is invalid")),
                };
                let range = decode_range(take_u8(&mut pc)?)?;
                let relocation = match take_u8(&mut pc)? {
                    0 => PortableRelocationKind::None,
                    1 => PortableRelocationKind::Absolute,
                    _ => return Err(invalid("fixup relocation tag is invalid")),
                };
                let transform = if version == SEMANTIC_VM_OPCODE_VERSION_V7 {
                    match take_u8(&mut pc)? {
                        0 => FixupTransform::Identity,
                        1 => {
                            let alignment = take_u32(&mut pc)?;
                            let mask = take_u32(&mut pc)?;
                            let transform = FixupTransform::AlignedBitOr { alignment, mask };
                            let mut encoded = Vec::new();
                            encode_transform(&mut encoded, width, &transform)?;
                            transform
                        }
                        2 => {
                            let alignment = take_u32(&mut pc)?;
                            validate_alignment(alignment)?;
                            let count = take_u8(&mut pc)?;
                            if count == 0 {
                                return Err(invalid("fixup range-map transform must not be empty"));
                            }
                            let mut mappings = Vec::with_capacity(count as usize);
                            for _ in 0..count {
                                mappings.push(FixupRangeMapping {
                                    min: take_i64(&mut pc)?,
                                    max: take_i64(&mut pc)?,
                                    adjustment: take_i64(&mut pc)?,
                                });
                            }
                            let mut encoded = Vec::new();
                            encode_transform(
                                &mut encoded,
                                width,
                                &FixupTransform::RangeMap {
                                    alignment,
                                    mappings: mappings.clone(),
                                },
                            )?;
                            FixupTransform::RangeMap {
                                alignment,
                                mappings,
                            }
                        }
                        _ => return Err(invalid("fixup transform tag is invalid")),
                    }
                } else {
                    FixupTransform::Identity
                };
                let (min, max) = range_bounds(width, range)?;
                if let UnresolvedValuePolicy::Placeholder(value) = unresolved {
                    let value = i64::from(value);
                    if value < min || value > max {
                        return Err(invalid("fixup placeholder is outside the declared range"));
                    }
                }
                steps.push(FixupEncodingStep {
                    input,
                    width,
                    endian,
                    base,
                    range,
                    unresolved,
                    relocation,
                    transform,
                });
            }
            opcode => {
                return Err(invalid(format!(
                    "invalid fixup encoding opcode 0x{opcode:02X}"
                )))
            }
        }
    }
}

pub fn validate_fixup_program(version: u16, bytes: &[u8]) -> Result<(), OpcpuCodecError> {
    decode_fixup_program(version, bytes).map(|_| ())
}
