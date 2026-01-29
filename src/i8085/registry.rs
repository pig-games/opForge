// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! 8085 CPU registry module.

use crate::core::cpu::{CpuFamily, CpuType};
use crate::core::registry::{CpuHandlerDyn, CpuModule};
use crate::families::intel8080::registry::DIALECT_INTEL8080;

use super::I8085CpuHandler;

pub struct I8085CpuModule;

impl CpuModule for I8085CpuModule {
    fn cpu_id(&self) -> CpuType {
        CpuType::I8085
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::Intel8080
    }

    fn default_dialect(&self) -> &'static str {
        DIALECT_INTEL8080
    }

    fn handler(&self) -> Box<dyn CpuHandlerDyn> {
        Box::new(I8085CpuHandler::new())
    }
}
