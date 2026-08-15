// SPDX-License-Identifier: GPL-3.0-or-later

//! CPU-neutral execution for package-owned selector choice programs.

use package::{
    SELECTOR_VM_OPCODE_VERSION_V1, SELECTOR_VM_OP_END, SELECTOR_VM_OP_MAP_EXACT,
    SELECTOR_VM_OP_MATCH_EXACT, SELECTOR_VM_OP_MATCH_PREFIX, SELECTOR_VM_OP_REWRITE_SUFFIX,
    SELECTOR_VM_OP_SELECT_DIAGNOSTIC, SELECTOR_VM_OP_SELECT_TARGET,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PortableSelectorOutcome {
    Target(String),
    Diagnostic(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectorVmError {
    UnsupportedVersion(u16),
    MalformedProgram(String),
}

impl std::fmt::Display for SelectorVmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsupportedVersion(version) => {
                write!(f, "unsupported selector VM opcode version {version}")
            }
            Self::MalformedProgram(detail) => write!(f, "malformed selector VM program: {detail}"),
        }
    }
}

impl std::error::Error for SelectorVmError {}

fn read_one_string<'a>(program: &'a [u8], pc: &mut usize) -> Result<&'a str, SelectorVmError> {
    let len = *program
        .get(*pc)
        .ok_or_else(|| SelectorVmError::MalformedProgram("missing string length".to_string()))?
        as usize;
    *pc += 1;
    if len == 0 {
        return Err(SelectorVmError::MalformedProgram(
            "empty string".to_string(),
        ));
    }
    let end = pc
        .checked_add(len)
        .filter(|end| *end <= program.len())
        .ok_or_else(|| SelectorVmError::MalformedProgram("truncated string".to_string()))?;
    let value = std::str::from_utf8(&program[*pc..end])
        .map_err(|_| SelectorVmError::MalformedProgram("non-UTF-8 string".to_string()))?;
    *pc = end;
    Ok(value)
}

fn read_strings<'a>(program: &'a [u8], pc: &mut usize) -> Result<Vec<&'a str>, SelectorVmError> {
    let count = *program
        .get(*pc)
        .ok_or_else(|| SelectorVmError::MalformedProgram("missing string count".to_string()))?;
    *pc += 1;
    if count == 0 {
        return Err(SelectorVmError::MalformedProgram(
            "empty string list".to_string(),
        ));
    }
    let mut values = Vec::with_capacity(count as usize);
    for _ in 0..count {
        values.push(read_one_string(program, pc)?);
    }
    Ok(values)
}

pub fn execute_selector_program(
    opcode_version: u16,
    program: &[u8],
    input: &str,
) -> Result<Option<PortableSelectorOutcome>, SelectorVmError> {
    if opcode_version != SELECTOR_VM_OPCODE_VERSION_V1 {
        return Err(SelectorVmError::UnsupportedVersion(opcode_version));
    }
    let mut pc = 0usize;
    let matcher_opcode = *program
        .get(pc)
        .ok_or_else(|| SelectorVmError::MalformedProgram("empty program".to_string()))?;
    pc += 1;
    if matcher_opcode == SELECTOR_VM_OP_MAP_EXACT {
        let count = *program.get(pc).ok_or_else(|| {
            SelectorVmError::MalformedProgram("missing mapping count".to_string())
        })?;
        pc += 1;
        if count == 0 {
            return Err(SelectorVmError::MalformedProgram(
                "empty mapping table".to_string(),
            ));
        }
        let mut selected = None;
        for _ in 0..count {
            let candidate = read_one_string(program, &mut pc)?;
            let target = read_one_string(program, &mut pc)?;
            if candidate.eq_ignore_ascii_case(input) {
                selected = Some(target.to_string());
            }
        }
        if program.get(pc) != Some(&SELECTOR_VM_OP_END) || pc + 1 != program.len() {
            return Err(SelectorVmError::MalformedProgram(
                "invalid mapping END".to_string(),
            ));
        }
        return Ok(selected.map(PortableSelectorOutcome::Target));
    }
    if matcher_opcode == SELECTOR_VM_OP_REWRITE_SUFFIX {
        let count = *program.get(pc).ok_or_else(|| {
            SelectorVmError::MalformedProgram("missing suffix mapping count".to_string())
        })?;
        pc += 1;
        if count == 0 {
            return Err(SelectorVmError::MalformedProgram(
                "empty suffix mapping table".to_string(),
            ));
        }
        let mut mappings = Vec::with_capacity(count as usize);
        for _ in 0..count {
            mappings.push((
                read_one_string(program, &mut pc)?,
                read_one_string(program, &mut pc)?,
            ));
        }
        let qualifier_prefix = read_one_string(program, &mut pc)?;
        let from_suffix = read_one_string(program, &mut pc)?;
        let to_suffix = read_one_string(program, &mut pc)?;
        let diagnostic = read_one_string(program, &mut pc)?;
        if program.get(pc) != Some(&SELECTOR_VM_OP_END) || pc + 1 != program.len() {
            return Err(SelectorVmError::MalformedProgram(
                "invalid suffix mapping END".to_string(),
            ));
        }
        for (candidate, target) in mappings {
            let Some(remainder) = input.get(candidate.len()..) else {
                continue;
            };
            if !input[..candidate.len()].eq_ignore_ascii_case(candidate) {
                continue;
            }
            if remainder.eq_ignore_ascii_case(from_suffix) {
                return Ok(Some(PortableSelectorOutcome::Target(format!(
                    "{target}{to_suffix}"
                ))));
            }
            if remainder
                .get(..qualifier_prefix.len())
                .is_some_and(|prefix| prefix.eq_ignore_ascii_case(qualifier_prefix))
            {
                return Ok(Some(PortableSelectorOutcome::Diagnostic(
                    diagnostic.to_string(),
                )));
            }
        }
        return Ok(None);
    }
    let matchers = read_strings(program, &mut pc)?;
    let matched = match matcher_opcode {
        SELECTOR_VM_OP_MATCH_EXACT => matchers
            .iter()
            .any(|candidate| candidate.eq_ignore_ascii_case(input)),
        SELECTOR_VM_OP_MATCH_PREFIX => matchers.iter().any(|candidate| {
            input
                .get(..candidate.len())
                .is_some_and(|prefix| prefix.eq_ignore_ascii_case(candidate))
        }),
        other => {
            return Err(SelectorVmError::MalformedProgram(format!(
                "invalid matcher opcode {other:#04x}"
            )))
        }
    };

    let outcome_opcode = *program
        .get(pc)
        .ok_or_else(|| SelectorVmError::MalformedProgram("missing outcome".to_string()))?;
    pc += 1;
    let outcomes = read_strings(program, &mut pc)?;
    if outcomes.len() != 1
        || program.get(pc) != Some(&SELECTOR_VM_OP_END)
        || pc + 1 != program.len()
    {
        return Err(SelectorVmError::MalformedProgram(
            "invalid outcome or END".to_string(),
        ));
    }
    if !matched {
        return Ok(None);
    }
    match outcome_opcode {
        SELECTOR_VM_OP_SELECT_TARGET => Ok(Some(PortableSelectorOutcome::Target(
            outcomes[0].to_string(),
        ))),
        SELECTOR_VM_OP_SELECT_DIAGNOSTIC => Ok(Some(PortableSelectorOutcome::Diagnostic(
            outcomes[0].to_string(),
        ))),
        other => Err(SelectorVmError::MalformedProgram(format!(
            "invalid outcome opcode {other:#04x}"
        ))),
    }
}
