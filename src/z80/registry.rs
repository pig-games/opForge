// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Z80 CPU registry module.

use crate::core::cpu::{CpuFamily, CpuType};
use crate::core::registry::{CpuHandlerDyn, CpuModule};
use crate::families::intel8080::registry::DIALECT_ZILOG;

use super::Z80CpuHandler;

pub struct Z80CpuModule;

impl CpuModule for Z80CpuModule {
    fn cpu_id(&self) -> CpuType {
        CpuType::Z80
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::Intel8080
    }

    fn default_dialect(&self) -> &'static str {
        DIALECT_ZILOG
    }

    fn handler(&self) -> Box<dyn CpuHandlerDyn> {
        Box::new(Z80CpuHandler::new())
    }
}
