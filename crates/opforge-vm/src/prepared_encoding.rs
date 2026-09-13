// SPDX-License-Identifier: GPL-3.0-or-later

//! Experimental prepare-once boundary for package encoding programs.
//!
//! Names and hierarchy are inputs to preparation only. The frozen directory owns
//! just program bytes and offsets; the original model may be dropped. A reference
//! identifies a program in this directory, never a machine opcode. Choosing an
//! operand form and computing its values remain the caller's responsibility.

use std::collections::HashMap;

use types::hierarchy::ResolvedHierarchy;

use crate::bytecode::execute_program;
use crate::runtime_error::RuntimeBridgeError;
use crate::runtime_model_core::{RuntimeModelCore, VmProgramKey};

/// Allows measurement callers to distinguish disabled counters from zero work.
pub const TELEMETRY_ENABLED: bool = cfg!(feature = "prepared-telemetry");

#[cfg(feature = "prepared-telemetry")]
#[doc(hidden)]
pub use types::vm_work::event;

/// Experimental work counters, compiled away (including arguments) by default.
#[cfg(feature = "prepared-telemetry")]
#[macro_export]
macro_rules! prepared_event {
    ($name:expr, $amount:expr) => {
        $crate::prepared_encoding::event($name, $amount as u64)
    };
}

#[cfg(not(feature = "prepared-telemetry"))]
#[macro_export]
macro_rules! prepared_event {
    ($name:expr, $amount:expr) => {};
}

/// Local to the directory that produced it. Do not persist across preparations
/// or combine references from independently prepared packages.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ProgramRef(u32);

/// Temporary binding state. Consuming `finish` releases the lookup map and the
/// borrow of the canonical model. Programs are retained once per resolved key.
pub struct EncodingPreparation<'a> {
    model: &'a RuntimeModelCore,
    bound: HashMap<VmProgramKey, ProgramRef>,
    bytes: Vec<u8>,
    offsets: Vec<u32>,
}

impl<'a> EncodingPreparation<'a> {
    pub fn new(model: &'a RuntimeModelCore) -> Self {
        Self {
            model,
            bound: HashMap::new(),
            bytes: Vec::new(),
            offsets: vec![0],
        }
    }

    /// Bind one package-defined mnemonic/form pair using canonical owner
    /// precedence. Missing pairs remain missing, with no implicit fallback.
    pub fn bind(
        &mut self,
        resolved: &ResolvedHierarchy,
        mnemonic: &str,
        mode: &str,
    ) -> Result<Option<ProgramRef>, RuntimeBridgeError> {
        crate::prepared_event!("prepared.encoding.bind", 1);
        let Some(mnemonic) = self.model.interned_id(&mnemonic.to_ascii_lowercase()) else {
            return Ok(None);
        };
        let Some(mode) = self.model.interned_id(&mode.to_ascii_lowercase()) else {
            return Ok(None);
        };
        for (owner, id) in self.model.scoped_owner_lookup_order(resolved) {
            let Some(id) = id else { continue };
            let key = (owner, id, mnemonic, mode);
            crate::prepared_event!("prepared.encoding.key_lookup", 1);
            let Some(program) = self.model.vm_programs.get(&key) else {
                continue;
            };
            if let Some(reference) = self.bound.get(&key) {
                return Ok(Some(*reference));
            }
            self.model.enforce_vm_program_budget(program.len())?;
            let end = self
                .bytes
                .len()
                .checked_add(program.len())
                .and_then(|end| u32::try_from(end).ok())
                .ok_or_else(|| {
                    RuntimeBridgeError::Resolve(
                        "prepared encoding directory exceeds 32-bit offsets".into(),
                    )
                })?;
            let index = u32::try_from(self.offsets.len() - 1).map_err(|_| {
                RuntimeBridgeError::Resolve("too many prepared encoding programs".into())
            })?;
            let reference = ProgramRef(index);
            self.bytes.extend_from_slice(program);
            self.offsets.push(end);
            self.bound.insert(key, reference);
            return Ok(Some(reference));
        }
        Ok(None)
    }

    pub fn finish(self) -> PreparedEncodings {
        PreparedEncodings {
            bytes: self.bytes.into_boxed_slice(),
            offsets: self.offsets.into_boxed_slice(),
            max_operands: self.model.budget_limits.max_operand_count_per_candidate,
            max_operand_bytes: self.model.budget_limits.max_operand_bytes_per_operand,
        }
    }
}

/// An immutable, string-free execution directory. Operand values are supplied
/// anew on each call. Relocation/effect markers must be handled by the caller;
/// this boundary accepts only the actual operands consumed by the bytecode.
pub struct PreparedEncodings {
    bytes: Box<[u8]>,
    offsets: Box<[u32]>,
    max_operands: usize,
    max_operand_bytes: usize,
}

impl PreparedEncodings {
    pub fn encode(
        &self,
        reference: ProgramRef,
        operands: &[&[u8]],
    ) -> Result<Vec<u8>, RuntimeBridgeError> {
        crate::prepared_event!("prepared.encoding.execute", 1);
        if operands.len() > self.max_operands {
            return Err(RuntimeModelCore::budget_error(
                "operand_count_per_candidate",
                self.max_operands,
                operands.len(),
            ));
        }
        for operand in operands {
            if operand.len() > self.max_operand_bytes {
                return Err(RuntimeModelCore::budget_error(
                    "operand_bytes_per_operand",
                    self.max_operand_bytes,
                    operand.len(),
                ));
            }
        }
        let index = reference.0 as usize;
        let range = self
            .offsets
            .get(index..)
            .and_then(|tail| tail.get(..2))
            .ok_or_else(|| {
                RuntimeBridgeError::Resolve("invalid prepared program reference".into())
            })?;
        execute_program(&self.bytes[range[0] as usize..range[1] as usize], operands)
            .map_err(RuntimeBridgeError::Vm)
    }

    pub fn program_count(&self) -> usize {
        self.offsets.len() - 1
    }

    /// Owned payload plus directory object; excludes allocator overhead, caller
    /// references/operands/output, and the temporary preparation peak.
    pub fn retained_bytes(&self) -> usize {
        std::mem::size_of::<Self>() + self.bytes.len() + std::mem::size_of_val(&*self.offsets)
    }
}
