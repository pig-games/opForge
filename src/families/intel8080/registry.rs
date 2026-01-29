// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Intel 8080 family registry module.

use crate::core::cpu::CpuFamily;
use crate::core::registry::{DialectModule, FamilyHandlerDyn, FamilyModule, FamilyOperandAny};

use super::dialect::map_zilog_to_canonical;
use super::Intel8080FamilyHandler;

pub const DIALECT_INTEL8080: &str = "intel8080";
pub const DIALECT_ZILOG: &str = "zilog";

pub struct Intel8080FamilyModule;

impl FamilyModule for Intel8080FamilyModule {
    fn family_id(&self) -> CpuFamily {
        CpuFamily::Intel8080
    }

    fn canonical_dialect(&self) -> &'static str {
        DIALECT_INTEL8080
    }

    fn dialects(&self) -> Vec<Box<dyn DialectModule>> {
        vec![Box::new(Intel8080Dialect), Box::new(ZilogDialect)]
    }

    fn handler(&self) -> Box<dyn FamilyHandlerDyn> {
        Box::new(Intel8080FamilyHandler)
    }
}

struct Intel8080Dialect;

impl DialectModule for Intel8080Dialect {
    fn dialect_id(&self) -> &'static str {
        DIALECT_INTEL8080
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::Intel8080
    }

    fn map_mnemonic(
        &self,
        mnemonic: &str,
        operands: &[FamilyOperandAny],
    ) -> Option<(String, Vec<FamilyOperandAny>)> {
        let mut mapped = Vec::with_capacity(operands.len());
        for operand in operands {
            match operand {
                FamilyOperandAny::Intel8080(inner) => {
                    mapped.push(FamilyOperandAny::Intel8080(inner.clone()))
                }
                _ => return None,
            }
        }
        Some((mnemonic.to_string(), mapped))
    }
}

struct ZilogDialect;

impl DialectModule for ZilogDialect {
    fn dialect_id(&self) -> &'static str {
        DIALECT_ZILOG
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::Intel8080
    }

    fn map_mnemonic(
        &self,
        mnemonic: &str,
        operands: &[FamilyOperandAny],
    ) -> Option<(String, Vec<FamilyOperandAny>)> {
        let mut intel_operands = Vec::with_capacity(operands.len());
        for operand in operands {
            match operand {
                FamilyOperandAny::Intel8080(inner) => intel_operands.push(inner.clone()),
                _ => return None,
            }
        }

        let (mapped_mnemonic, mapped_operands) = map_zilog_to_canonical(mnemonic, &intel_operands)?;
        let wrapped = mapped_operands
            .into_iter()
            .map(FamilyOperandAny::Intel8080)
            .collect();
        Some((mapped_mnemonic, wrapped))
    }
}

