// SPDX-License-Identifier: GPL-3.0-or-later

//! CPU-neutral package state matrices.

use std::collections::{HashMap, HashSet};

use types::hierarchy::ScopedOwner;

use super::OpcpuCodecError;

pub const STATE_VM_OPCODE_VERSION_V1: u16 = 0x0001;
const STATE_VM_END: u8 = 0xff;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateProgramDescriptor {
    pub owner: ScopedOwner,
    pub id: String,
    pub opcode_version: u16,
    pub program: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateProgramSpec {
    pub profiles: Vec<String>,
    pub keys: Vec<StateKeySpec>,
    pub directives: Vec<StateDirectiveSpec>,
    pub capabilities: Vec<StateCapabilitySpec>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateKeySpec {
    pub id: String,
    pub default: u32,
    pub overrides: Vec<(String, u32)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateDirectiveSpec {
    pub id: String,
    pub key: String,
    pub arguments: Vec<StateArgumentSpec>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateArgumentSpec {
    pub id: String,
    pub value: u32,
    pub allowed_profiles: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateCapabilitySpec {
    pub id: String,
    pub key: String,
    pub rules: Vec<StateCapabilityRuleSpec>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateCapabilityRuleSpec {
    pub allowed_profiles: Vec<String>,
    pub allowed_values: Vec<u32>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecodedStateProgram {
    pub profiles: Vec<String>,
    pub keys: Vec<DecodedStateKey>,
    pub directives: Vec<DecodedStateDirective>,
    pub capabilities: Vec<DecodedStateCapability>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecodedStateKey {
    pub id: String,
    pub default: u32,
    pub overrides: Vec<(u8, u32)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecodedStateDirective {
    pub id: String,
    pub key_index: u8,
    pub arguments: Vec<DecodedStateArgument>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecodedStateArgument {
    pub id: String,
    pub value: u32,
    pub profile_mask: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecodedStateCapability {
    pub id: String,
    pub key_index: u8,
    pub rules: Vec<DecodedStateCapabilityRule>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecodedStateCapabilityRule {
    pub profile_mask: Vec<u8>,
    pub allowed_values: Vec<u32>,
}

fn invalid(detail: impl Into<String>) -> OpcpuCodecError {
    OpcpuCodecError::InvalidChunkFormat {
        chunk: "STVM".to_string(),
        detail: detail.into(),
    }
}

fn push_count(out: &mut Vec<u8>, count: usize, label: &str) -> Result<(), OpcpuCodecError> {
    let count = u8::try_from(count).map_err(|_| OpcpuCodecError::CountOutOfRange {
        context: format!("state VM {label} exceeds u8"),
    })?;
    out.push(count);
    Ok(())
}

fn push_string(out: &mut Vec<u8>, value: &str, label: &str) -> Result<(), OpcpuCodecError> {
    if value.is_empty() {
        return Err(invalid(format!("state VM {label} must not be empty")));
    }
    let bytes = value.as_bytes();
    let len = u8::try_from(bytes.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
        context: format!("state VM {label} length exceeds u8"),
    })?;
    out.push(len);
    out.extend_from_slice(bytes);
    Ok(())
}

fn push_var_u32(out: &mut Vec<u8>, mut value: u32) {
    loop {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80;
        }
        out.push(byte);
        if value == 0 {
            break;
        }
    }
}

fn profile_mask(
    profile_indexes: &HashMap<String, u8>,
    profiles: &[String],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut mask = vec![0u8; profile_indexes.len().div_ceil(8)];
    for profile in profiles {
        let index = profile_indexes
            .get(&profile.to_ascii_lowercase())
            .copied()
            .ok_or_else(|| invalid(format!("unknown state VM profile '{profile}'")))?;
        mask[index as usize / 8] |= 1 << (index % 8);
    }
    if mask.iter().all(|byte| *byte == 0) {
        return Err(invalid("state VM profile mask must not be empty"));
    }
    Ok(mask)
}

pub fn compile_state_program(spec: &StateProgramSpec) -> Result<Vec<u8>, OpcpuCodecError> {
    if spec.profiles.is_empty() || spec.keys.is_empty() {
        return Err(invalid(
            "state VM requires at least one profile and one state key",
        ));
    }
    let mut profile_indexes = HashMap::new();
    for (index, profile) in spec.profiles.iter().enumerate() {
        let index = u8::try_from(index).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "state VM profile index exceeds u8".to_string(),
        })?;
        if profile.is_empty()
            || profile_indexes
                .insert(profile.to_ascii_lowercase(), index)
                .is_some()
        {
            return Err(invalid("state VM profiles must be nonempty and unique"));
        }
    }
    let mut key_indexes = HashMap::new();
    for (index, key) in spec.keys.iter().enumerate() {
        let index = u8::try_from(index).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "state VM key index exceeds u8".to_string(),
        })?;
        if key.id.is_empty()
            || key_indexes
                .insert(key.id.to_ascii_lowercase(), index)
                .is_some()
        {
            return Err(invalid("state VM keys must be nonempty and unique"));
        }
    }

    let mut out = Vec::new();
    push_count(&mut out, spec.profiles.len(), "profile count")?;
    for profile in &spec.profiles {
        push_string(&mut out, profile, "profile id")?;
    }
    push_count(&mut out, spec.keys.len(), "key count")?;
    for key in &spec.keys {
        push_string(&mut out, &key.id, "key id")?;
        push_var_u32(&mut out, key.default);
        push_count(&mut out, key.overrides.len(), "default override count")?;
        let mut seen = HashSet::new();
        for (profile, value) in &key.overrides {
            let index = profile_indexes
                .get(&profile.to_ascii_lowercase())
                .copied()
                .ok_or_else(|| invalid(format!("unknown state VM profile '{profile}'")))?;
            if !seen.insert(index) {
                return Err(invalid("duplicate state VM default override"));
            }
            out.push(index);
            push_var_u32(&mut out, *value);
        }
    }

    push_count(&mut out, spec.directives.len(), "directive count")?;
    let mut directive_ids = HashSet::new();
    for directive in &spec.directives {
        if !directive_ids.insert(directive.id.to_ascii_lowercase()) {
            return Err(invalid("duplicate state VM directive id"));
        }
        push_string(&mut out, &directive.id, "directive id")?;
        out.push(
            *key_indexes
                .get(&directive.key.to_ascii_lowercase())
                .ok_or_else(|| invalid(format!("unknown state VM key '{}'", directive.key)))?,
        );
        push_count(
            &mut out,
            directive.arguments.len(),
            "directive argument count",
        )?;
        let mut argument_ids = HashSet::new();
        for argument in &directive.arguments {
            if !argument_ids.insert(argument.id.to_ascii_lowercase()) {
                return Err(invalid("duplicate state VM directive argument"));
            }
            push_string(&mut out, &argument.id, "directive argument")?;
            push_var_u32(&mut out, argument.value);
            let mask = profile_mask(&profile_indexes, &argument.allowed_profiles)?;
            push_count(&mut out, mask.len(), "profile mask length")?;
            out.extend_from_slice(&mask);
        }
    }

    push_count(&mut out, spec.capabilities.len(), "capability count")?;
    let mut capability_ids = HashSet::new();
    for capability in &spec.capabilities {
        if !capability_ids.insert(capability.id.to_ascii_lowercase()) {
            return Err(invalid("duplicate state VM capability id"));
        }
        push_string(&mut out, &capability.id, "capability id")?;
        out.push(
            *key_indexes
                .get(&capability.key.to_ascii_lowercase())
                .ok_or_else(|| invalid(format!("unknown state VM key '{}'", capability.key)))?,
        );
        push_count(&mut out, capability.rules.len(), "capability rule count")?;
        for rule in &capability.rules {
            if rule.allowed_values.is_empty() {
                return Err(invalid("state VM capability values must not be empty"));
            }
            let mask = profile_mask(&profile_indexes, &rule.allowed_profiles)?;
            push_count(&mut out, mask.len(), "profile mask length")?;
            out.extend_from_slice(&mask);
            push_count(
                &mut out,
                rule.allowed_values.len(),
                "capability value count",
            )?;
            for value in &rule.allowed_values {
                push_var_u32(&mut out, *value);
            }
        }
    }
    out.push(STATE_VM_END);
    decode_state_program(STATE_VM_OPCODE_VERSION_V1, &out)?;
    Ok(out)
}

struct ProgramReader<'a> {
    bytes: &'a [u8],
    pc: usize,
}

impl<'a> ProgramReader<'a> {
    fn byte(&mut self, label: &str) -> Result<u8, OpcpuCodecError> {
        let value = *self
            .bytes
            .get(self.pc)
            .ok_or_else(|| invalid(format!("state VM {label} is truncated")))?;
        self.pc += 1;
        Ok(value)
    }

    fn string(&mut self, label: &str) -> Result<String, OpcpuCodecError> {
        let len = self.byte(&format!("{label} length"))? as usize;
        if len == 0 {
            return Err(invalid(format!("state VM {label} must not be empty")));
        }
        let end = self
            .pc
            .checked_add(len)
            .filter(|end| *end <= self.bytes.len())
            .ok_or_else(|| invalid(format!("state VM {label} is truncated")))?;
        let value = std::str::from_utf8(&self.bytes[self.pc..end])
            .map_err(|_| invalid(format!("state VM {label} is not UTF-8")))?
            .to_string();
        self.pc = end;
        Ok(value)
    }

    fn var_u32(&mut self, label: &str) -> Result<u32, OpcpuCodecError> {
        let mut value = 0u32;
        for shift in (0..=28).step_by(7) {
            let byte = self.byte(label)?;
            if shift == 28 && byte > 0x0f {
                return Err(invalid(format!("state VM {label} varint overflows u32")));
            }
            value |= u32::from(byte & 0x7f) << shift;
            if byte & 0x80 == 0 {
                if shift != 0 && byte == 0 {
                    return Err(invalid(format!("state VM {label} varint is not canonical")));
                }
                return Ok(value);
            }
        }
        Err(invalid(format!("state VM {label} varint is too long")))
    }

    fn mask(
        &mut self,
        expected_len: usize,
        profile_count: usize,
    ) -> Result<Vec<u8>, OpcpuCodecError> {
        let len = self.byte("profile mask length")? as usize;
        if len != expected_len {
            return Err(invalid("state VM profile mask has invalid length"));
        }
        let end = self
            .pc
            .checked_add(len)
            .filter(|end| *end <= self.bytes.len())
            .ok_or_else(|| invalid("state VM profile mask is truncated"))?;
        let mask = self.bytes[self.pc..end].to_vec();
        self.pc = end;
        if mask.iter().all(|byte| *byte == 0) {
            return Err(invalid("state VM profile mask must not be empty"));
        }
        let remainder = profile_count % 8;
        if remainder != 0
            && mask
                .last()
                .is_some_and(|byte| byte & !((1u8 << remainder) - 1) != 0)
        {
            return Err(invalid("state VM profile mask sets unknown profiles"));
        }
        Ok(mask)
    }
}

fn ensure_unique(values: &[String], label: &str) -> Result<(), OpcpuCodecError> {
    let mut seen = HashSet::new();
    if values
        .iter()
        .any(|value| !seen.insert(value.to_ascii_lowercase()))
    {
        return Err(invalid(format!("duplicate state VM {label}")));
    }
    Ok(())
}

pub fn decode_state_program(
    opcode_version: u16,
    program: &[u8],
) -> Result<DecodedStateProgram, OpcpuCodecError> {
    if opcode_version != STATE_VM_OPCODE_VERSION_V1 {
        return Err(invalid(format!(
            "unsupported state VM opcode version {opcode_version}"
        )));
    }
    let mut reader = ProgramReader {
        bytes: program,
        pc: 0,
    };
    let profile_count = reader.byte("profile count")? as usize;
    if profile_count == 0 {
        return Err(invalid("state VM profile list must not be empty"));
    }
    let mut profiles = Vec::with_capacity(profile_count);
    for _ in 0..profile_count {
        profiles.push(reader.string("profile id")?);
    }
    ensure_unique(&profiles, "profile id")?;
    let mask_len = profile_count.div_ceil(8);

    let key_count = reader.byte("key count")? as usize;
    if key_count == 0 {
        return Err(invalid("state VM key list must not be empty"));
    }
    let mut keys = Vec::with_capacity(key_count);
    for _ in 0..key_count {
        let id = reader.string("key id")?;
        let default = reader.var_u32("key default")?;
        let override_count = reader.byte("default override count")? as usize;
        let mut overrides = Vec::with_capacity(override_count);
        let mut seen = HashSet::new();
        for _ in 0..override_count {
            let profile = reader.byte("default override profile")?;
            if profile as usize >= profile_count || !seen.insert(profile) {
                return Err(invalid("state VM default override profile is invalid"));
            }
            overrides.push((profile, reader.var_u32("default override value")?));
        }
        keys.push(DecodedStateKey {
            id,
            default,
            overrides,
        });
    }
    ensure_unique(
        &keys.iter().map(|key| key.id.clone()).collect::<Vec<_>>(),
        "key id",
    )?;

    let directive_count = reader.byte("directive count")? as usize;
    let mut directives = Vec::with_capacity(directive_count);
    for _ in 0..directive_count {
        let id = reader.string("directive id")?;
        let key_index = reader.byte("directive key index")?;
        if key_index as usize >= key_count {
            return Err(invalid("state VM directive key index is invalid"));
        }
        let argument_count = reader.byte("directive argument count")? as usize;
        if argument_count == 0 {
            return Err(invalid("state VM directive arguments must not be empty"));
        }
        let mut arguments = Vec::with_capacity(argument_count);
        for _ in 0..argument_count {
            arguments.push(DecodedStateArgument {
                id: reader.string("directive argument")?,
                value: reader.var_u32("directive value")?,
                profile_mask: reader.mask(mask_len, profile_count)?,
            });
        }
        ensure_unique(
            &arguments
                .iter()
                .map(|argument| argument.id.clone())
                .collect::<Vec<_>>(),
            "directive argument",
        )?;
        directives.push(DecodedStateDirective {
            id,
            key_index,
            arguments,
        });
    }
    ensure_unique(
        &directives
            .iter()
            .map(|directive| directive.id.clone())
            .collect::<Vec<_>>(),
        "directive id",
    )?;

    let capability_count = reader.byte("capability count")? as usize;
    let mut capabilities = Vec::with_capacity(capability_count);
    for _ in 0..capability_count {
        let id = reader.string("capability id")?;
        let key_index = reader.byte("capability key index")?;
        if key_index as usize >= key_count {
            return Err(invalid("state VM capability key index is invalid"));
        }
        let rule_count = reader.byte("capability rule count")? as usize;
        if rule_count == 0 {
            return Err(invalid("state VM capability rules must not be empty"));
        }
        let mut rules = Vec::with_capacity(rule_count);
        for _ in 0..rule_count {
            let profile_mask = reader.mask(mask_len, profile_count)?;
            let value_count = reader.byte("capability value count")? as usize;
            if value_count == 0 {
                return Err(invalid("state VM capability values must not be empty"));
            }
            let mut allowed_values = Vec::with_capacity(value_count);
            for _ in 0..value_count {
                allowed_values.push(reader.var_u32("capability value")?);
            }
            allowed_values.sort_unstable();
            allowed_values.dedup();
            rules.push(DecodedStateCapabilityRule {
                profile_mask,
                allowed_values,
            });
        }
        capabilities.push(DecodedStateCapability {
            id,
            key_index,
            rules,
        });
    }
    ensure_unique(
        &capabilities
            .iter()
            .map(|capability| capability.id.clone())
            .collect::<Vec<_>>(),
        "capability id",
    )?;

    if reader.byte("END")? != STATE_VM_END || reader.pc != program.len() {
        return Err(invalid("state VM program has an invalid END"));
    }
    Ok(DecodedStateProgram {
        profiles,
        keys,
        directives,
        capabilities,
    })
}

pub fn validate_state_program(opcode_version: u16, program: &[u8]) -> Result<(), OpcpuCodecError> {
    decode_state_program(opcode_version, program).map(|_| ())
}
