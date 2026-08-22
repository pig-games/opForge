// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use opcore::scope::ScopeStack;
use registry::cpu::CpuType;
use registry::registry::ModuleRegistry;
use registry::syntax::{register_checker_none, RegisterChecker};
use types::asm_value::StructField;
use types::symbol::SymbolVisibility;

use crate::error::{AsmError, Fixit};
use crate::output::{PlacementDirective, RegionState, RootMetadata, SectionState};
use opcore::parser::ParseError;

#[derive(Debug, Clone)]
pub struct EncodingScopeState {
    pub definition_name: String,
    pub previous_active_encoding: String,
}

#[derive(Debug, Default)]
pub struct AsmDiagnosticsState {
    pub last_error: Option<AsmError>,
    pub last_error_column: Option<usize>,
    pub last_error_help: Option<String>,
    pub last_error_fixits: Vec<Fixit>,
    pub last_parser_error: Option<ParseError>,
}

impl AsmDiagnosticsState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            last_error: None,
            last_error_column: None,
            last_error_help: None,
            last_error_fixits: Vec::new(),
            last_parser_error: None,
        }
    }
}

#[derive(Debug, Default)]
pub struct AsmLayoutState {
    pub sections: HashMap<String, SectionState>,
    pub regions: HashMap<String, RegionState>,
    pub placement_directives: Vec<PlacementDirective>,
    pub section_symbol_sections: HashMap<String, String>,
    pub concrete_section_declarations: HashSet<String>,
    pub absolute_constant_symbols: HashSet<String>,
    pub section_stack: Vec<Option<String>>,
    pub current_section: Option<String>,
}

impl AsmLayoutState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            sections: HashMap::new(),
            regions: HashMap::new(),
            placement_directives: Vec::new(),
            section_symbol_sections: HashMap::new(),
            concrete_section_declarations: HashSet::new(),
            absolute_constant_symbols: HashSet::new(),
            section_stack: Vec::new(),
            current_section: None,
        }
    }
}

#[derive(Default)]
pub struct AsmSymbolScopeState {
    pub scope_stack: ScopeStack,
    pub visibility_stack: Vec<SymbolVisibility>,
    pub module_active: Option<String>,
    pub module_scope_depth: usize,
    pub saw_explicit_module: bool,
    pub top_level_content_seen: bool,
}

impl AsmSymbolScopeState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            scope_stack: ScopeStack::new(),
            visibility_stack: vec![SymbolVisibility::Private],
            module_active: None,
            module_scope_depth: 0,
            saw_explicit_module: false,
            top_level_content_seen: false,
        }
    }
}

pub struct AsmOutputState {
    pub root_metadata: RootMetadata,
    pub in_meta_block: bool,
    pub in_output_block: bool,
    pub output_cpu_block: Option<String>,
}

impl AsmOutputState {
    #[must_use]
    pub fn new(root_metadata: RootMetadata) -> Self {
        Self {
            root_metadata,
            in_meta_block: false,
            in_output_block: false,
            output_cpu_block: None,
        }
    }
}

pub struct AsmCpuModeState {
    pub program_address_max: u32,
    pub word_size_bytes: u32,
    pub little_endian: bool,
    pub state_flags: HashMap<String, u32>,
}

impl AsmCpuModeState {
    #[must_use]
    pub fn new(registry: &ModuleRegistry, cpu: CpuType) -> Self {
        Self::new_with_model(registry, cpu, None)
    }

    #[must_use]
    pub fn new_with_model(
        registry: &ModuleRegistry,
        cpu: CpuType,
        model: Option<&vm::vm_opasm::HierarchyExecutionModel>,
    ) -> Self {
        let resolved = registry.resolve_pipeline(cpu, None).ok();
        let package_state = model.and_then(|model| {
            let resolved = model.resolve_pipeline(cpu.as_str(), None).ok()?;
            model
                .initial_unique_package_state(&resolved, cpu.as_str())
                .ok()
                .flatten()
        });
        Self {
            program_address_max: resolved
                .as_ref()
                .map(|pipeline| pipeline.cpu.max_program_address())
                .unwrap_or(0xFFFF),
            word_size_bytes: resolved
                .as_ref()
                .map(|pipeline| pipeline.cpu.native_word_size_bytes().max(1))
                .unwrap_or(2),
            little_endian: resolved
                .as_ref()
                .map(|pipeline| pipeline.cpu.is_little_endian())
                .unwrap_or(true),
            state_flags: package_state.unwrap_or_else(|| {
                resolved
                    .as_ref()
                    .map(|pipeline| pipeline.cpu.runtime_state_defaults())
                    .unwrap_or_default()
            }),
        }
    }
}

#[must_use]
pub fn build_register_checker(registry: &ModuleRegistry, cpu: CpuType) -> RegisterChecker {
    match registry.resolve_pipeline(cpu, None) {
        Ok(pipeline) => {
            let family = pipeline.family;
            Arc::new(move |ident: &str| family.is_register(ident) || family.is_condition(ident))
        }
        Err(_) => register_checker_none(),
    }
}

#[must_use]
pub fn build_package_register_checker(
    model: &vm::vm_opasm::HierarchyExecutionModel,
    cpu: CpuType,
) -> Option<RegisterChecker> {
    model
        .resolve_pipeline(cpu.as_str(), None)
        .ok()
        .map(|resolved| model.register_checker_for_resolved(&resolved))
}

pub struct ActiveStructDefinition {
    pub name: String,
    pub open_line: u32,
    pub fields: Vec<StructField>,
    pub size: u32,
}

impl ActiveStructDefinition {
    #[must_use]
    pub fn new(name: String, open_line: u32) -> Self {
        Self {
            name,
            open_line,
            fields: Vec::new(),
            size: 0,
        }
    }
}
