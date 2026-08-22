// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral execution of candidate-width `SEMV` v5 programs.

use package::{
    decode_branch_program, BranchCandidateSpec, EncodingEndian, OpcpuCodecError,
    SEMANTIC_VM_OPCODE_VERSION_V5,
};

use crate::fixup_vm::PortableDeferredValue;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PortableBranchRequest {
    pub requested_candidate: Option<u8>,
    pub previous_output_size: Option<u8>,
    pub automatic_class: u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PortableBranchContext {
    pub position: i64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableBranchResult {
    pub candidate_id: u8,
    pub bytes: Vec<u8>,
    pub output_size: u8,
    pub layout_changed: bool,
    pub deferred: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BranchVmError {
    Program(OpcpuCodecError),
    MissingScalarInput {
        index: usize,
        len: usize,
    },
    MissingTargetInput {
        index: usize,
        len: usize,
    },
    OpcodeOutOfRange {
        index: usize,
        value: i64,
    },
    UnknownExplicitCandidate {
        id: u8,
    },
    InvalidAutomaticClass {
        class: u8,
    },
    PositionOverflow {
        position: i64,
        adjustment: i32,
    },
    ProjectionOverflow {
        value: i64,
        base: i64,
    },
    ValueOutOfRange {
        candidate: u8,
        value: i64,
        min: i64,
        max: i64,
    },
    ReservedValue {
        candidate: u8,
        value: i64,
    },
    NoAutomaticCandidate {
        target: i64,
    },
    OutputSizeOverflow,
}

impl std::fmt::Display for BranchVmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program(err) => write!(f, "{err}"),
            Self::MissingScalarInput { index, len } => write!(
                f,
                "branch scalar input index {index} is out of range for {len} input(s)"
            ),
            Self::MissingTargetInput { index, len } => write!(
                f,
                "branch target input index {index} is out of range for {len} input(s)"
            ),
            Self::OpcodeOutOfRange { index, value } => {
                write!(
                    f,
                    "branch opcode input {index} value {value} is outside 0..=255"
                )
            }
            Self::UnknownExplicitCandidate { id } => {
                write!(f, "branch candidate {id} is not defined")
            }
            Self::InvalidAutomaticClass { class } => {
                write!(f, "branch automatic class {class} is outside 0..=7")
            }
            Self::PositionOverflow {
                position,
                adjustment,
            } => write!(
                f,
                "branch position {position} plus adjustment {adjustment} overflows"
            ),
            Self::ProjectionOverflow { value, base } => {
                write!(f, "branch target {value} minus base {base} overflows")
            }
            Self::ValueOutOfRange {
                candidate,
                value,
                min,
                max,
            } => write!(
                f,
                "branch candidate {candidate} value {value} is outside {min}..={max}"
            ),
            Self::ReservedValue { candidate, value } => {
                if *value == 0 {
                    write!(
                        f,
                        "zero displacement is reserved for branch candidate {candidate}"
                    )
                } else {
                    write!(f, "branch candidate {candidate} reserves value {value}")
                }
            }
            Self::NoAutomaticCandidate { target } => {
                write!(
                    f,
                    "no automatic branch candidate can encode target {target}"
                )
            }
            Self::OutputSizeOverflow => write!(f, "branch output size exceeds u8"),
        }
    }
}

impl std::error::Error for BranchVmError {}

fn signed_bounds(width: u8) -> (i64, i64) {
    let bits = u32::from(width) * 8;
    (-(1_i64 << (bits - 1)), (1_i64 << (bits - 1)) - 1)
}

fn project(
    target: i64,
    candidate: &BranchCandidateSpec,
    context: PortableBranchContext,
) -> Result<i64, BranchVmError> {
    let base = context
        .position
        .checked_add(i64::from(candidate.position_adjustment))
        .ok_or(BranchVmError::PositionOverflow {
            position: context.position,
            adjustment: candidate.position_adjustment,
        })?;
    target
        .checked_sub(base)
        .ok_or(BranchVmError::ProjectionOverflow {
            value: target,
            base,
        })
}

fn candidate_fits(candidate: &BranchCandidateSpec, value: i64) -> bool {
    let (min, max) = signed_bounds(candidate.displacement_width);
    (min..=max).contains(&value)
        && !candidate
            .reserved_values
            .iter()
            .any(|reserved| i64::from(*reserved) == value)
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

pub fn execute_branch_program(
    program: &[u8],
    scalar_inputs: &[i64],
    target_inputs: &[PortableDeferredValue],
    request: PortableBranchRequest,
    context: PortableBranchContext,
) -> Result<PortableBranchResult, BranchVmError> {
    let spec = decode_branch_program(SEMANTIC_VM_OPCODE_VERSION_V5, program)
        .map_err(BranchVmError::Program)?;
    let opcode = *scalar_inputs.get(spec.opcode_input as usize).ok_or(
        BranchVmError::MissingScalarInput {
            index: spec.opcode_input as usize,
            len: scalar_inputs.len(),
        },
    )?;
    let opcode = u8::try_from(opcode).map_err(|_| BranchVmError::OpcodeOutOfRange {
        index: spec.opcode_input as usize,
        value: opcode,
    })?;
    let target = *target_inputs.get(spec.target_input as usize).ok_or(
        BranchVmError::MissingTargetInput {
            index: spec.target_input as usize,
            len: target_inputs.len(),
        },
    )?;
    let deferred = matches!(target, PortableDeferredValue::Unresolved);
    let (candidate, value) = if let Some(requested) = request.requested_candidate {
        let candidate = spec
            .candidates
            .iter()
            .find(|candidate| candidate.id == requested)
            .ok_or(BranchVmError::UnknownExplicitCandidate { id: requested })?;
        let value = match target {
            PortableDeferredValue::Unresolved => i64::from(candidate.unresolved_placeholder),
            PortableDeferredValue::Resolved(target) => project(target, candidate, context)?,
        };
        let (min, max) = signed_bounds(candidate.displacement_width);
        if !(min..=max).contains(&value) {
            return Err(BranchVmError::ValueOutOfRange {
                candidate: candidate.id,
                value,
                min,
                max,
            });
        }
        if candidate
            .reserved_values
            .iter()
            .any(|reserved| i64::from(*reserved) == value)
        {
            return Err(BranchVmError::ReservedValue {
                candidate: candidate.id,
                value,
            });
        }
        (candidate, value)
    } else {
        let automatic_mask = 1_u8.checked_shl(u32::from(request.automatic_class)).ok_or(
            BranchVmError::InvalidAutomaticClass {
                class: request.automatic_class,
            },
        )?;
        match target {
            PortableDeferredValue::Unresolved => {
                let candidate = spec
                    .candidates
                    .iter()
                    .find(|candidate| {
                        candidate.id == spec.unresolved_candidate
                            && candidate.automatic_classes & automatic_mask != 0
                    })
                    .ok_or(BranchVmError::NoAutomaticCandidate { target: 0 })?;
                (candidate, i64::from(candidate.unresolved_placeholder))
            }
            PortableDeferredValue::Resolved(target) => {
                let mut selected = None;
                for candidate in spec
                    .candidates
                    .iter()
                    .filter(|candidate| candidate.automatic_classes & automatic_mask != 0)
                {
                    let value = project(target, candidate, context)?;
                    if candidate_fits(candidate, value) {
                        selected = Some((candidate, value));
                        break;
                    }
                }
                selected.ok_or(BranchVmError::NoAutomaticCandidate { target })?
            }
        }
    };

    let mut bytes =
        Vec::with_capacity(1 + candidate.suffix.len() + candidate.displacement_width as usize);
    bytes.push(opcode);
    bytes.extend_from_slice(&candidate.suffix);
    emit(
        &mut bytes,
        value as u32,
        candidate.displacement_width,
        candidate.endian,
    );
    let output_size = u8::try_from(bytes.len()).map_err(|_| BranchVmError::OutputSizeOverflow)?;
    Ok(PortableBranchResult {
        candidate_id: candidate.id,
        bytes,
        output_size,
        layout_changed: request
            .previous_output_size
            .is_some_and(|previous| previous != output_size),
        deferred,
    })
}
