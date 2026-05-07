// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Source formatter scaffolding.

mod builtin_hooks;
mod config;
mod diagnostics;
mod engine;
#[cfg(test)]
mod fixture_tests;
mod hook_registry;
mod hooks;
mod planner;
mod renderer;
mod state_tracker;
mod surface_parser;
mod surface_tokenizer;
mod symbol_context;

pub mod formatter {
    pub use crate::*;
}

pub mod families {
    pub mod intel8080 {
        pub mod module {
            pub use families::families::intel8080::module::*;
        }
    }

    pub mod m6800 {
        pub mod module {
            pub use families::families::m6800::module::*;
        }
    }

    pub mod m68k {
        pub mod module {
            pub use families::families::m68k::module::*;
        }
    }

    pub mod mos6502 {
        pub mod module {
            pub use families::families::mos6502::module::*;
        }
    }
}

pub mod i8085 {
    pub mod module {
        pub use families::i8085::module::*;
    }
}

pub mod z80 {
    pub mod module {
        pub use families::z80::module::*;
    }
}

pub mod m65c02 {
    pub mod module {
        pub use families::m65c02::module::*;
    }
}

pub mod m65816 {
    pub mod module {
        pub use families::m65816::module::*;
    }
}

pub mod m45gs02 {
    pub mod module {
        pub use families::m45gs02::module::*;
    }
}

pub mod m6809 {
    pub mod module {
        pub use families::m6809::module::*;
    }
}

pub mod hd6309 {
    pub mod module {
        pub use families::hd6309::module::*;
    }
}

pub mod m68000 {
    pub mod module {
        pub use families::m68000::module::*;
    }
}

pub use config::{CaseStyle, FormatterConfig, IndentChar, LabelCaseStyle, LabelColonStyle};
pub use diagnostics::{collect_fallback_diagnostics, FormatterDiagnostic};
pub use engine::{
    FormatMode, FormatterEngine, FormatterFileReport, FormatterOutput, FormatterRunReport,
    FormatterRunSummary,
};
pub use hook_registry::{FormatterHookRegistry, ResolvedFormatterHooks};
pub use hooks::{
    CpuFormatterHook, DialectFormatterHook, FamilyFormatterHook, FormatterHints,
    FormatterHookContext, GlobalFormatterHook, NoopGlobalFormatterHook,
};
pub use planner::{plan_document, FormatPlan, PlannedLine};
pub use renderer::render_plan;
pub use state_tracker::{
    ActivePipeline, LinePipelineState, StateTrackError, StateTrackWarning, StateTracker,
    StateTrackerResult,
};
pub use surface_parser::{
    parse_document, parse_line, SurfaceLineKind, SurfaceParsedDocument, SurfaceParsedLine,
};
pub use surface_tokenizer::{tokenize_source, LineEnding, SurfaceDocument, SurfaceLine};
