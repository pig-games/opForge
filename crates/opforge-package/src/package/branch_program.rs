// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-neutral candidate-width programs carried by `SEMV` v5.

use super::{EncodingEndian, OpcpuCodecError, SEMANTIC_VM_OPCODE_VERSION_V5};

const OP_CHOOSE: u8 = 0x01;
const OP_END: u8 = 0xff;
const MAX_CANDIDATES: usize = 16;
const MAX_SUFFIX_BYTES: usize = 8;
const MAX_RESERVED_VALUES: usize = 8;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BranchCandidateSpec {
    pub id: u8,
    pub automatic_classes: u8,
    pub suffix: Vec<u8>,
    pub displacement_width: u8,
    pub endian: EncodingEndian,
    pub position_adjustment: i32,
    pub unresolved_placeholder: i32,
    pub reserved_values: Vec<i32>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BranchProgramSpec {
    pub opcode_input: u8,
    pub target_input: u8,
    pub unresolved_candidate: u8,
    pub candidates: Vec<BranchCandidateSpec>,
}

fn invalid(detail: impl Into<String>) -> OpcpuCodecError {
    OpcpuCodecError::InvalidChunkFormat {
        chunk: "SEMV".to_string(),
        detail: detail.into(),
    }
}

fn signed_bounds(width: u8) -> Result<(i64, i64), OpcpuCodecError> {
    let bits = match width {
        1 | 2 | 4 => u32::from(width) * 8,
        _ => return Err(invalid("branch displacement width must be 1, 2, or 4")),
    };
    Ok((-(1_i64 << (bits - 1)), (1_i64 << (bits - 1)) - 1))
}

fn validate_spec(spec: &BranchProgramSpec) -> Result<(), OpcpuCodecError> {
    if spec.candidates.is_empty() || spec.candidates.len() > MAX_CANDIDATES {
        return Err(invalid("branch program candidate count must be 1..=16"));
    }
    let mut ids = Vec::with_capacity(spec.candidates.len());
    let mut automatic_classes = 0_u8;
    for candidate in &spec.candidates {
        if ids.contains(&candidate.id) {
            return Err(invalid("branch candidate ids must be unique"));
        }
        ids.push(candidate.id);
        automatic_classes |= candidate.automatic_classes;
        if candidate.suffix.len() > MAX_SUFFIX_BYTES {
            return Err(invalid("branch candidate suffix exceeds 8 bytes"));
        }
        if candidate.reserved_values.len() > MAX_RESERVED_VALUES {
            return Err(invalid("branch candidate has more than 8 reserved values"));
        }
        let (min, max) = signed_bounds(candidate.displacement_width)?;
        let placeholder = i64::from(candidate.unresolved_placeholder);
        if placeholder < min || placeholder > max {
            return Err(invalid(
                "branch unresolved placeholder is outside candidate range",
            ));
        }
        if candidate
            .reserved_values
            .contains(&candidate.unresolved_placeholder)
        {
            return Err(invalid("branch unresolved placeholder is reserved"));
        }
        let mut previous = None;
        for value in &candidate.reserved_values {
            let value_i64 = i64::from(*value);
            if value_i64 < min || value_i64 > max {
                return Err(invalid("branch reserved value is outside candidate range"));
            }
            if previous.is_some_and(|previous| previous >= *value) {
                return Err(invalid(
                    "branch reserved values must be strictly increasing",
                ));
            }
            previous = Some(*value);
        }
        let output_size = 1usize
            .checked_add(candidate.suffix.len())
            .and_then(|size| size.checked_add(candidate.displacement_width as usize))
            .ok_or_else(|| invalid("branch candidate output size overflows"))?;
        if output_size > u8::MAX as usize {
            return Err(invalid("branch candidate output size exceeds u8"));
        }
    }
    if automatic_classes == 0 {
        return Err(invalid("branch program must have an automatic candidate"));
    }
    if !spec.candidates.iter().any(|candidate| {
        candidate.id == spec.unresolved_candidate && candidate.automatic_classes != 0
    }) {
        return Err(invalid(
            "branch unresolved candidate must name an automatic candidate",
        ));
    }
    Ok(())
}

pub fn compile_branch_program(spec: &BranchProgramSpec) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut canonical = spec.clone();
    for candidate in &mut canonical.candidates {
        candidate.reserved_values.sort_unstable();
        candidate.reserved_values.dedup();
    }
    validate_spec(&canonical)?;
    let mut out = vec![
        OP_CHOOSE,
        canonical.opcode_input,
        canonical.target_input,
        canonical.unresolved_candidate,
        canonical.candidates.len() as u8,
    ];
    for candidate in &canonical.candidates {
        out.extend_from_slice(&[
            candidate.id,
            candidate.automatic_classes,
            candidate.suffix.len() as u8,
        ]);
        out.extend_from_slice(&candidate.suffix);
        out.extend_from_slice(&[
            candidate.displacement_width,
            match candidate.endian {
                EncodingEndian::Big => 0,
                EncodingEndian::Little => 1,
            },
        ]);
        out.extend_from_slice(&candidate.position_adjustment.to_le_bytes());
        out.extend_from_slice(&candidate.unresolved_placeholder.to_le_bytes());
        out.push(candidate.reserved_values.len() as u8);
        for value in &candidate.reserved_values {
            out.extend_from_slice(&value.to_le_bytes());
        }
    }
    out.push(OP_END);
    validate_branch_program(SEMANTIC_VM_OPCODE_VERSION_V5, &out)?;
    Ok(out)
}

pub fn decode_branch_program(
    version: u16,
    bytes: &[u8],
) -> Result<BranchProgramSpec, OpcpuCodecError> {
    if version != SEMANTIC_VM_OPCODE_VERSION_V5 {
        return Err(invalid(format!("unsupported branch VM version {version}")));
    }
    let mut pc = 0usize;
    let take_u8 = |pc: &mut usize| -> Result<u8, OpcpuCodecError> {
        let value = *bytes
            .get(*pc)
            .ok_or_else(|| invalid("branch program is truncated"))?;
        *pc += 1;
        Ok(value)
    };
    let take_i32 = |pc: &mut usize| -> Result<i32, OpcpuCodecError> {
        let end = pc
            .checked_add(4)
            .filter(|end| *end <= bytes.len())
            .ok_or_else(|| invalid("branch program i32 is truncated"))?;
        let value = i32::from_le_bytes(bytes[*pc..end].try_into().expect("four bytes"));
        *pc = end;
        Ok(value)
    };
    if take_u8(&mut pc)? != OP_CHOOSE {
        return Err(invalid("branch program must begin with CHOOSE"));
    }
    let opcode_input = take_u8(&mut pc)?;
    let target_input = take_u8(&mut pc)?;
    let unresolved_candidate = take_u8(&mut pc)?;
    let count = take_u8(&mut pc)? as usize;
    if count == 0 || count > MAX_CANDIDATES {
        return Err(invalid("branch program candidate count must be 1..=16"));
    }
    let mut candidates = Vec::with_capacity(count);
    for _ in 0..count {
        let id = take_u8(&mut pc)?;
        let automatic_classes = take_u8(&mut pc)?;
        let suffix_len = take_u8(&mut pc)? as usize;
        if suffix_len > MAX_SUFFIX_BYTES {
            return Err(invalid("branch candidate suffix exceeds 8 bytes"));
        }
        let suffix_end = pc
            .checked_add(suffix_len)
            .filter(|end| *end <= bytes.len())
            .ok_or_else(|| invalid("branch candidate suffix is truncated"))?;
        let suffix = bytes[pc..suffix_end].to_vec();
        pc = suffix_end;
        let displacement_width = take_u8(&mut pc)?;
        signed_bounds(displacement_width)?;
        let endian = match take_u8(&mut pc)? {
            0 => EncodingEndian::Big,
            1 => EncodingEndian::Little,
            _ => return Err(invalid("branch candidate endian tag is invalid")),
        };
        let position_adjustment = take_i32(&mut pc)?;
        let unresolved_placeholder = take_i32(&mut pc)?;
        let reserved_count = take_u8(&mut pc)? as usize;
        if reserved_count > MAX_RESERVED_VALUES {
            return Err(invalid("branch candidate has more than 8 reserved values"));
        }
        let mut reserved_values = Vec::with_capacity(reserved_count);
        for _ in 0..reserved_count {
            reserved_values.push(take_i32(&mut pc)?);
        }
        candidates.push(BranchCandidateSpec {
            id,
            automatic_classes,
            suffix,
            displacement_width,
            endian,
            position_adjustment,
            unresolved_placeholder,
            reserved_values,
        });
    }
    if take_u8(&mut pc)? != OP_END {
        return Err(invalid("branch program is missing END"));
    }
    if pc != bytes.len() {
        return Err(invalid("branch program has trailing bytes"));
    }
    let spec = BranchProgramSpec {
        opcode_input,
        target_input,
        unresolved_candidate,
        candidates,
    };
    validate_spec(&spec)?;
    Ok(spec)
}

pub fn validate_branch_program(version: u16, bytes: &[u8]) -> Result<(), OpcpuCodecError> {
    decode_branch_program(version, bytes).map(|_| ())
}
