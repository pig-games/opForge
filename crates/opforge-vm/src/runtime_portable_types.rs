// SPDX-License-Identifier: GPL-3.0-or-later

//! Portable runtime request and adapter contracts.

use registry::registry::VmEncodeCandidate;

use crate::runtime_model_types::RuntimeTokenPolicy;

/// Minimal host-to-runtime ABI for portable/native targets.
///
/// Hosts provide resolved VM candidates plus active hierarchy ids; runtime lookup
/// and bytecode execution stays generic and package-driven.
pub trait PortableInstructionAdapter: std::fmt::Debug {
    fn cpu_id(&self) -> &str;
    fn dialect_override(&self) -> Option<&str> {
        None
    }
    fn mnemonic(&self) -> &str;
    fn vm_encode_candidates(&self) -> &[VmEncodeCandidate];
}

/// Portable tokenization request envelope for runtime VM integration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PortableTokenizeRequest<'a> {
    pub family_id: &'a str,
    pub cpu_id: &'a str,
    pub dialect_id: &'a str,
    pub source_line: &'a str,
    pub line_num: u32,
    pub token_policy: RuntimeTokenPolicy,
}

/// Default portable request container for host adapter integration.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PortableInstructionRequest {
    pub cpu_id: String,
    pub dialect_override: Option<String>,
    pub mnemonic: String,
    pub candidates: Vec<VmEncodeCandidate>,
}

impl PortableInstructionAdapter for PortableInstructionRequest {
    fn cpu_id(&self) -> &str {
        self.cpu_id.as_str()
    }

    fn dialect_override(&self) -> Option<&str> {
        self.dialect_override.as_deref()
    }

    fn mnemonic(&self) -> &str {
        self.mnemonic.as_str()
    }

    fn vm_encode_candidates(&self) -> &[VmEncodeCandidate] {
        self.candidates.as_slice()
    }
}
