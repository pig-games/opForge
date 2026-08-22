// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral execution of runtime-position and deferred-value `SEMV` v4 programs.

use package::{
    decode_fixup_program, EncodingEndian, FixupBase, FixupRange, FixupTransform, OpcpuCodecError,
    PortableRelocationKind, UnresolvedValuePolicy, SEMANTIC_VM_OPCODE_VERSION_V4,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PortableDeferredValue {
    Resolved(i64),
    Unresolved,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableFixupInput {
    pub value: PortableDeferredValue,
    pub target_reference: bool,
    pub relocation_target: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PortableFixupContext {
    pub position: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PortableOutputFixupKind {
    Absolute,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableOutputFixup {
    pub offset: u32,
    pub width: u8,
    pub kind: PortableOutputFixupKind,
    pub target: String,
    pub encoded_addend: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableFixupResult {
    pub bytes: Vec<u8>,
    pub fixups: Vec<PortableOutputFixup>,
    pub deferred_inputs: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FixupVmError {
    Program(OpcpuCodecError),
    MissingInput {
        index: usize,
        len: usize,
    },
    UnresolvedInput {
        index: usize,
    },
    PositionOverflow {
        position: i64,
        adjustment: i32,
    },
    ProjectionOverflow {
        value: i64,
        base: i64,
    },
    AlignmentViolation {
        index: usize,
        value: i64,
        alignment: u32,
    },
    NoRangeMapping {
        index: usize,
        value: i64,
    },
    TransformOverflow {
        index: usize,
        value: i64,
        adjustment: i64,
    },
    ValueOutOfRange {
        index: usize,
        value: i64,
        min: i64,
        max: i64,
    },
    OutputOffsetOverflow,
}

impl std::fmt::Display for FixupVmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program(err) => write!(f, "{err}"),
            Self::MissingInput { index, len } => write!(
                f,
                "fixup input index {index} is out of range for {len} input(s)"
            ),
            Self::UnresolvedInput { index } => {
                write!(f, "fixup input {index} is unresolved")
            }
            Self::PositionOverflow {
                position,
                adjustment,
            } => write!(
                f,
                "fixup position {position} plus adjustment {adjustment} overflows"
            ),
            Self::ProjectionOverflow { value, base } => {
                write!(f, "fixup value {value} minus base {base} overflows")
            }
            Self::AlignmentViolation {
                index,
                value,
                alignment,
            } => write!(
                f,
                "fixup input {index} projected value {value} is not aligned to {alignment}"
            ),
            Self::NoRangeMapping { index, value } => write!(
                f,
                "fixup input {index} projected value {value} has no declared range mapping"
            ),
            Self::TransformOverflow {
                index,
                value,
                adjustment,
            } => write!(
                f,
                "fixup input {index} value {value} plus adjustment {adjustment} overflows"
            ),
            Self::ValueOutOfRange {
                index,
                value,
                min,
                max,
            } => write!(
                f,
                "fixup input {index} value {value} is outside {min}..={max}"
            ),
            Self::OutputOffsetOverflow => write!(f, "fixup output offset exceeds u32"),
        }
    }
}

impl std::error::Error for FixupVmError {}

fn range_bounds(width: u8, range: FixupRange) -> (i64, i64) {
    let bits = u32::from(width) * 8;
    match range {
        FixupRange::Signed => (-(1_i64 << (bits - 1)), (1_i64 << (bits - 1)) - 1),
        FixupRange::Unsigned => (0, ((1_u64 << bits) - 1) as i64),
        FixupRange::BitPattern => (-(1_i64 << (bits - 1)), ((1_u64 << bits) - 1) as i64),
    }
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

pub fn execute_fixup_program(
    program: &[u8],
    inputs: &[PortableFixupInput],
    context: PortableFixupContext,
) -> Result<PortableFixupResult, FixupVmError> {
    execute_fixup_program_for_version(SEMANTIC_VM_OPCODE_VERSION_V4, program, inputs, context)
}

pub fn execute_fixup_program_for_version(
    version: u16,
    program: &[u8],
    inputs: &[PortableFixupInput],
    context: PortableFixupContext,
) -> Result<PortableFixupResult, FixupVmError> {
    let steps = decode_fixup_program(version, program).map_err(FixupVmError::Program)?;
    let mut bytes = Vec::new();
    let mut fixups = Vec::new();
    let mut deferred_inputs = Vec::new();

    for step in steps {
        let input = inputs
            .get(step.input as usize)
            .ok_or(FixupVmError::MissingInput {
                index: step.input as usize,
                len: inputs.len(),
            })?;
        let unresolved = matches!(input.value, PortableDeferredValue::Unresolved);
        let mut value = match input.value {
            PortableDeferredValue::Resolved(value) => value,
            PortableDeferredValue::Unresolved => match step.unresolved {
                UnresolvedValuePolicy::Reject => {
                    return Err(FixupVmError::UnresolvedInput {
                        index: step.input as usize,
                    })
                }
                UnresolvedValuePolicy::Placeholder(value) => {
                    if !deferred_inputs.contains(&step.input) {
                        deferred_inputs.push(step.input);
                    }
                    i64::from(value)
                }
            },
        };
        let should_project = match step.base {
            FixupBase::Value => false,
            FixupBase::Position {
                target_references_only,
                ..
            } => !target_references_only || input.target_reference,
        };
        if should_project && !matches!(input.value, PortableDeferredValue::Unresolved) {
            let FixupBase::Position { adjustment, .. } = step.base else {
                unreachable!("projection requires a position base")
            };
            let base = context.position.checked_add(i64::from(adjustment)).ok_or(
                FixupVmError::PositionOverflow {
                    position: context.position,
                    adjustment,
                },
            )?;
            value = value
                .checked_sub(base)
                .ok_or(FixupVmError::ProjectionOverflow { value, base })?;
        }
        if !unresolved {
            value = apply_transform(step.input as usize, value, &step.transform)?;
        }
        let (min, max) = range_bounds(step.width, step.range);
        if value < min || value > max {
            return Err(FixupVmError::ValueOutOfRange {
                index: step.input as usize,
                value,
                min,
                max,
            });
        }
        let offset = u32::try_from(bytes.len()).map_err(|_| FixupVmError::OutputOffsetOverflow)?;
        emit(&mut bytes, value as u32, step.width, step.endian);
        if step.relocation == PortableRelocationKind::Absolute {
            if let Some(target) = input.relocation_target.as_ref() {
                fixups.push(PortableOutputFixup {
                    offset,
                    width: step.width,
                    kind: PortableOutputFixupKind::Absolute,
                    target: target.clone(),
                    encoded_addend: value as u32,
                });
            }
        }
    }

    Ok(PortableFixupResult {
        bytes,
        fixups,
        deferred_inputs,
    })
}

fn apply_transform(
    index: usize,
    value: i64,
    transform: &FixupTransform,
) -> Result<i64, FixupVmError> {
    let alignment = match transform {
        FixupTransform::Identity => return Ok(value),
        FixupTransform::AlignedBitOr { alignment, .. }
        | FixupTransform::RangeMap { alignment, .. } => *alignment,
    };
    if value.rem_euclid(i64::from(alignment)) != 0 {
        return Err(FixupVmError::AlignmentViolation {
            index,
            value,
            alignment,
        });
    }
    match transform {
        FixupTransform::Identity => Ok(value),
        FixupTransform::AlignedBitOr { mask, .. } => Ok(value | i64::from(*mask)),
        FixupTransform::RangeMap { mappings, .. } => {
            let mapping = mappings
                .iter()
                .find(|mapping| (mapping.min..=mapping.max).contains(&value))
                .ok_or(FixupVmError::NoRangeMapping { index, value })?;
            value
                .checked_add(mapping.adjustment)
                .ok_or(FixupVmError::TransformOverflow {
                    index,
                    value,
                    adjustment: mapping.adjustment,
                })
        }
    }
}
