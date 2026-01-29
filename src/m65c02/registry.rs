// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! 65C02 CPU registry module.

use crate::core::cpu::{CpuFamily, CpuType};
use crate::core::registry::{CpuHandlerDyn, CpuModule};
use crate::families::mos6502::registry::DIALECT_TRANSPARENT;

use super::M65C02CpuHandler;

pub struct M65C02CpuModule;

impl CpuModule for M65C02CpuModule {
    fn cpu_id(&self) -> CpuType {
        CpuType::M65C02
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::MOS6502
    }

    fn default_dialect(&self) -> &'static str {
        DIALECT_TRANSPARENT
    }

    fn handler(&self) -> Box<dyn CpuHandlerDyn> {
        Box::new(M65C02CpuHandler::new())
    }
}
