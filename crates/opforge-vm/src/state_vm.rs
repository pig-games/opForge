// SPDX-License-Identifier: GPL-3.0-or-later

//! CPU-neutral execution for package-owned state matrices.

use std::collections::HashMap;

use package::DecodedStateProgram;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PortableStateDirectiveOutcome {
    NotHandled,
    Applied,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StateVmError {
    UnknownProgram(String),
    UnknownProfile(String),
    InvalidOperandCount {
        directive: String,
    },
    InvalidArgument {
        directive: String,
        argument: String,
    },
    IllegalCombination {
        profile: String,
        directive: String,
        argument: String,
    },
}

impl std::fmt::Display for StateVmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownProgram(program) => write!(f, "unknown state program '{program}'"),
            Self::UnknownProfile(profile) => write!(f, "unknown state profile '{profile}'"),
            Self::InvalidOperandCount { directive } => {
                write!(f, "state directive '{directive}' requires exactly one argument")
            }
            Self::InvalidArgument {
                directive,
                argument,
            } => write!(
                f,
                "invalid argument '{argument}' for state directive '{directive}'"
            ),
            Self::IllegalCombination {
                profile,
                directive,
                argument,
            } => write!(
                f,
                "state directive '{directive}' argument '{argument}' is not legal for profile '{profile}'"
            ),
        }
    }
}

impl std::error::Error for StateVmError {}

fn profile_index(program: &DecodedStateProgram, profile: &str) -> Result<u8, StateVmError> {
    program
        .profiles
        .iter()
        .position(|candidate| candidate.eq_ignore_ascii_case(profile))
        .map(|index| index as u8)
        .ok_or_else(|| StateVmError::UnknownProfile(profile.to_string()))
}

fn profile_allowed(mask: &[u8], profile: u8) -> bool {
    mask.get(profile as usize / 8)
        .is_some_and(|byte| byte & (1 << (profile % 8)) != 0)
}

pub fn initial_state(
    program: &DecodedStateProgram,
    profile: &str,
) -> Result<HashMap<String, u32>, StateVmError> {
    let profile = profile_index(program, profile)?;
    Ok(program
        .keys
        .iter()
        .map(|key| {
            let value = key
                .overrides
                .iter()
                .find_map(|(candidate, value)| (*candidate == profile).then_some(*value))
                .unwrap_or(key.default);
            (key.id.clone(), value)
        })
        .collect())
}

pub fn apply_directive(
    program: &DecodedStateProgram,
    profile: &str,
    directive: &str,
    arguments: &[String],
    state: &mut HashMap<String, u32>,
) -> Result<PortableStateDirectiveOutcome, StateVmError> {
    let profile_index = profile_index(program, profile)?;
    let Some(rule) = program
        .directives
        .iter()
        .find(|candidate| candidate.id.eq_ignore_ascii_case(directive))
    else {
        return Ok(PortableStateDirectiveOutcome::NotHandled);
    };
    if arguments.len() != 1 {
        return Err(StateVmError::InvalidOperandCount {
            directive: directive.to_string(),
        });
    }
    let argument = &arguments[0];
    let selected = rule
        .arguments
        .iter()
        .find(|candidate| candidate.id.eq_ignore_ascii_case(argument))
        .ok_or_else(|| StateVmError::InvalidArgument {
            directive: directive.to_string(),
            argument: argument.clone(),
        })?;
    if !profile_allowed(&selected.profile_mask, profile_index) {
        return Err(StateVmError::IllegalCombination {
            profile: profile.to_string(),
            directive: directive.to_string(),
            argument: argument.clone(),
        });
    }
    let key = &program.keys[rule.key_index as usize].id;
    state.insert(key.clone(), selected.value);
    Ok(PortableStateDirectiveOutcome::Applied)
}

pub fn capability_allowed(
    program: &DecodedStateProgram,
    profile: &str,
    capability: &str,
    state: &HashMap<String, u32>,
) -> Result<Option<bool>, StateVmError> {
    let profile_index = profile_index(program, profile)?;
    let Some(capability) = program
        .capabilities
        .iter()
        .find(|candidate| candidate.id.eq_ignore_ascii_case(capability))
    else {
        return Ok(None);
    };
    let key = &program.keys[capability.key_index as usize];
    let value = state.get(&key.id).copied().unwrap_or(key.default);
    Ok(Some(capability.rules.iter().any(|rule| {
        profile_allowed(&rule.profile_mask, profile_index) && rule.allowed_values.contains(&value)
    })))
}
