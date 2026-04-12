// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! opForge language server modules.

pub mod capabilities;
pub mod code_actions;
pub mod completion;
pub mod config;
pub mod cpu_context;
pub mod definition;
pub mod diagnostics;
pub mod document_state;
pub mod document_symbols;
pub mod framing;
pub mod hover;
pub mod member_context;
pub mod protocol;
pub mod session;
pub mod validation_runner;
pub mod workspace_index;

pub fn build_default_asm_registry() -> libopforge::registry::AsmRegistry {
    libopforge::registry::default_asm_registry()
}

pub mod lsp {
    pub use crate::capabilities;
    pub use crate::code_actions;
    pub use crate::completion;
    pub use crate::config;
    pub use crate::cpu_context;
    pub use crate::definition;
    pub use crate::diagnostics;
    pub use crate::document_state;
    pub use crate::document_symbols;
    pub use crate::framing;
    pub use crate::hover;
    pub use crate::member_context;
    pub use crate::protocol;
    pub use crate::session;
    pub use crate::validation_runner;
    pub use crate::workspace_index;
}
