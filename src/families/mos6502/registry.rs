// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! MOS 6502 family registry module.

use crate::core::cpu::{CpuFamily, CpuType};
use crate::core::registry::{CpuHandlerDyn, CpuModule, DialectModule, FamilyHandlerDyn, FamilyModule, FamilyOperandAny};

use super::{M6502CpuHandler, MOS6502FamilyHandler};

pub const DIALECT_TRANSPARENT: &str = "transparent";

pub struct MOS6502FamilyModule;

impl FamilyModule for MOS6502FamilyModule {
    fn family_id(&self) -> CpuFamily {
        CpuFamily::MOS6502
    }

    fn canonical_dialect(&self) -> &'static str {
        DIALECT_TRANSPARENT
    }

    fn dialects(&self) -> Vec<Box<dyn DialectModule>> {
        vec![Box::new(TransparentDialect)]
    }

    fn handler(&self) -> Box<dyn FamilyHandlerDyn> {
        Box::new(MOS6502FamilyHandler::new())
    }
}

pub struct M6502CpuModule;

impl CpuModule for M6502CpuModule {
    fn cpu_id(&self) -> CpuType {
        CpuType::M6502
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::MOS6502
    }

    fn default_dialect(&self) -> &'static str {
        DIALECT_TRANSPARENT
    }

    fn handler(&self) -> Box<dyn CpuHandlerDyn> {
        Box::new(M6502CpuHandler::new())
    }
}

struct TransparentDialect;

impl DialectModule for TransparentDialect {
    fn dialect_id(&self) -> &'static str {
        DIALECT_TRANSPARENT
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::MOS6502
    }

    fn map_mnemonic(
        &self,
        mnemonic: &str,
        operands: &[FamilyOperandAny],
    ) -> Option<(String, Vec<FamilyOperandAny>)> {
        let mut mapped = Vec::with_capacity(operands.len());
        for operand in operands {
            match operand {
                FamilyOperandAny::MOS6502(inner) => {
                    mapped.push(FamilyOperandAny::MOS6502(inner.clone()))
                }
                _ => return None,
            }
        }
        Some((mnemonic.to_string(), mapped))
    }
}
