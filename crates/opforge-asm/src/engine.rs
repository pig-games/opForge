// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use crate as asm;
use crate::line::{repetition, AsmLine, RuntimeLineRouter};
use crate::repetition_driver::{
    execute_lines as execute_repetition_lines, RepetitionPass, UnscopedRepeatKind,
};
use asm::error::{AsmError, AsmErrorKind, Diagnostic, LineStatus, PassCounts, Severity};
use asm::listing::{ListingLine, ListingWriter};
use asm::output::{LinkerOutputDirective, RegionState, RootMetadata, SectionKind, SectionState};
use families::{
    register_intel8080_family_stack, register_mos6502_family_stack,
    register_motorola68000_family_stack, register_motorola6800_family_stack,
};
use registry::cpu::CpuType;
use registry::registry::ModuleRegistry;
use std::collections::{HashMap, HashSet};
use std::convert::Infallible;
use std::io::Write;
use std::rc::Rc;
use types::image::ImageStore;
use types::lockstep::LockstepReport;
use types::processing::LineProcessingTrace;
use types::symbol::{SymbolTable, SymbolVisibility};
use vm::output_model::{LinkerOutputFormat, IMPLICIT_HUNK_CODE_SECTION_NAME};

fn build_default_registry_for_tests() -> ModuleRegistry {
    let mut registry = ModuleRegistry::new();
    register_intel8080_family_stack(&mut registry);
    register_mos6502_family_stack(&mut registry);
    register_motorola6800_family_stack(&mut registry);
    register_motorola68000_family_stack(&mut registry);
    registry
}

pub struct Assembler {
    pub symbols: SymbolTable,
    pub image: ImageStore,
    pub sections: HashMap<String, SectionState>,
    pub regions: HashMap<String, RegionState>,
    pub section_symbol_sections: HashMap<String, String>,
    pub absolute_constant_symbols: HashSet<String>,
    pub diagnostics: Vec<Diagnostic>,
    pub cpu: CpuType,
    pub registry: ModuleRegistry,
    pub root_metadata: RootMetadata,
    pub module_macro_names: HashMap<String, HashMap<String, SymbolVisibility>>,
    pub loop_iteration_trace_pass1: Vec<(u32, u32)>,
    pub max_loop_iterations: u32,
    pub opasm_package_path: Option<std::path::PathBuf>,
    pub runtime_line_router: Option<Rc<dyn RuntimeLineRouter>>,
    pub runtime_processing_traces: Vec<(u8, u32, LineProcessingTrace)>,
    pub runtime_lockstep_report: LockstepReport,
    implicit_hunk_output_requested: bool,
}

const MAX_LAYOUT_STABILIZATION_PASSES: usize = 8;

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::type_complexity)]
struct LayoutStabilitySnapshot {
    symbols: Vec<(String, u32, bool, bool, Option<String>)>,
    sections: Vec<(
        String,
        u32,
        u32,
        u32,
        bool,
        u32,
        vm::output_model::SectionKind,
        Option<String>,
        Option<u32>,
    )>,
    regions: Vec<(String, u32, u32, u32, u32, Vec<String>)>,
}

impl Assembler {
    #[cfg(test)]
    fn assert_partitioned_runtime_traces_present(
        lines: &[String],
        traces: &[(u8, u32, LineProcessingTrace)],
    ) {
        let has_non_empty_line = lines.iter().any(|line| !line.trim().is_empty());
        if !has_non_empty_line {
            return;
        }
        assert!(
            !traces.is_empty(),
            "expected partitioned runtime processing traces for non-empty assembly input"
        );
        for (_, _, trace) in traces {
            assert!(
                !trace.requests().is_empty(),
                "expected non-empty partitioned processing trace"
            );
        }
    }

    fn cpu_requires_layout_stabilization(&self) -> bool {
        matches!(
            self.cpu.as_str(),
            "68020" | "68030" | "68040" | "m68020" | "m68030" | "m68040"
        )
    }

    fn capture_layout_snapshot(&self) -> LayoutStabilitySnapshot {
        let mut symbols = self
            .symbols
            .entries()
            .iter()
            .map(|entry| {
                (
                    entry.name.clone(),
                    entry.val,
                    entry.rw,
                    entry.updated,
                    entry.module_id.clone(),
                )
            })
            .collect::<Vec<_>>();
        symbols.sort_by(|left, right| left.0.cmp(&right.0));

        let mut sections = self
            .sections
            .iter()
            .map(|(name, section)| {
                (
                    name.clone(),
                    section.start_pc,
                    section.pc,
                    section.max_pc,
                    section.layout_placed,
                    section.align,
                    section.kind,
                    section.default_region.clone(),
                    section.base_addr,
                )
            })
            .collect::<Vec<_>>();
        sections.sort_by(|left, right| left.0.cmp(&right.0));

        let mut regions = self
            .regions
            .iter()
            .map(|(name, region)| {
                (
                    name.clone(),
                    region.start,
                    region.end,
                    region.cursor,
                    region.align,
                    region
                        .placed
                        .iter()
                        .map(|section| section.name.clone())
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<Vec<_>>();
        regions.sort_by(|left, right| left.0.cmp(&right.0));

        LayoutStabilitySnapshot {
            symbols,
            sections,
            regions,
        }
    }

    fn run_layout_pass(
        &mut self,
        lines: &[String],
        pass_num: u8,
        capture_runtime_trace: bool,
        finalize_section_symbols: bool,
        loop_trace: &mut Vec<(u32, u32)>,
    ) -> PassCounts {
        let seeded_sections = if pass_num > 1 {
            Some(self.sections.clone())
        } else {
            None
        };
        let seeded_regions = if pass_num > 1 {
            Some(self.regions.clone())
        } else {
            None
        };
        let uses_implicit_hunk_code_section =
            Self::uses_implicit_hunk_code_section(lines, self.implicit_hunk_output_requested);
        self.sections.clear();
        self.regions.clear();
        self.diagnostics.clear();
        let mut addr: u32 = 0;
        let line_num: u32 = u32::try_from(lines.len())
            .unwrap_or(u32::MAX.saturating_sub(1))
            .saturating_add(1);
        let mut counts = PassCounts::new();
        let diagnostics = &mut self.diagnostics;

        {
            let root_metadata = if pass_num == 1 {
                std::mem::take(&mut self.root_metadata)
            } else {
                RootMetadata::default()
            };
            let mut asm_line = AsmLine::with_cpu_and_metadata(
                &mut self.symbols,
                self.cpu,
                &self.registry,
                root_metadata,
            );
            asm_line.set_runtime_package_path(self.opasm_package_path.as_deref());
            asm_line.set_runtime_line_router(self.runtime_line_router.clone());
            asm_line.clear_conditionals();
            asm_line.clear_scopes();
            if pass_num > 1 {
                asm_line.layout.section_symbol_sections = self.section_symbol_sections.clone();
                asm_line.layout.absolute_constant_symbols = self.absolute_constant_symbols.clone();
            }
            if let (Some(sections), Some(regions)) = (seeded_sections, seeded_regions) {
                asm_line.layout.sections = sections;
                asm_line.layout.regions = regions;
                for section in asm_line.layout.sections.values_mut() {
                    section.pc = 0;
                    section.bytes.clear();
                    section.relocation_free_certified = true;
                    section.hunk_relocation_compatible = true;
                    section.hunk_fixup_error = None;
                    section.output_fixups.clear();
                    section.emitted = false;
                }
            }
            if uses_implicit_hunk_code_section {
                Self::seed_implicit_hunk_code_section(&mut asm_line);
            }

            Self::execute_pass1_lines(
                lines,
                0,
                lines.len(),
                &mut asm_line,
                &mut addr,
                &mut counts,
                diagnostics,
                loop_trace,
                None,
                self.max_loop_iterations,
                pass_num,
            );

            if capture_runtime_trace {
                self.runtime_processing_traces.extend(
                    asm_line
                        .take_runtime_processing_traces()
                        .into_iter()
                        .map(|(line_num, trace)| (1, line_num, trace)),
                );
                self.runtime_lockstep_report
                    .extend(asm_line.take_runtime_lockstep_report());
                #[cfg(test)]
                Self::assert_partitioned_runtime_traces_present(
                    lines,
                    &self.runtime_processing_traces,
                );
            }

            if !asm_line.cond_is_empty() {
                let err = AsmError::new(
                    AsmErrorKind::Conditional,
                    "Found .if without .endif in pass 1",
                    None,
                );
                diagnostics.push(
                    Diagnostic::new(line_num, Severity::Error, err)
                        .with_help("add a matching .endif to close the open conditional block")
                        .with_fixit(asm::error::Fixit {
                            file: None,
                            line: line_num,
                            col_start: Some(1),
                            col_end: Some(1),
                            replacement: ".endif".to_string(),
                            applicability: "machine-applicable".to_string(),
                        }),
                );
                asm_line.clear_conditionals();
                counts.errors += 1;
            }

            if asm_line.in_module() {
                let err = AsmError::new(
                    AsmErrorKind::Directive,
                    "Found .module without .endmodule",
                    None,
                );
                diagnostics.push(
                    Diagnostic::new(line_num, Severity::Error, err)
                        .with_help("add a matching .endmodule to close the open module block")
                        .with_fixit(asm::error::Fixit {
                            file: None,
                            line: line_num,
                            col_start: Some(1),
                            col_end: Some(1),
                            replacement: ".endmodule".to_string(),
                            applicability: "machine-applicable".to_string(),
                        }),
                );
                counts.errors += 1;
            }

            if asm_line.in_user_section() {
                let err = AsmError::new(
                    AsmErrorKind::Directive,
                    "Found .section without .endsection",
                    None,
                );
                diagnostics.push(
                    Diagnostic::new(line_num, Severity::Error, err)
                        .with_help("add a matching .endsection to close the open section block")
                        .with_fixit(asm::error::Fixit {
                            file: None,
                            line: line_num,
                            col_start: Some(1),
                            col_end: Some(1),
                            replacement: ".endsection".to_string(),
                            applicability: "machine-applicable".to_string(),
                        }),
                );
                counts.errors += 1;
            }

            if let Some(open_line) = asm_line.open_struct_line() {
                let err = AsmError::new(
                    AsmErrorKind::Directive,
                    &format!("unterminated .struct (opened at line {open_line})"),
                    None,
                );
                diagnostics.push(
                    Diagnostic::new(line_num, Severity::Error, err)
                        .with_help("add a matching .endstruct to close the open struct definition")
                        .with_fixit(asm::error::Fixit {
                            file: None,
                            line: line_num,
                            col_start: Some(1),
                            col_end: Some(1),
                            replacement: ".endstruct".to_string(),
                            applicability: "machine-applicable".to_string(),
                        }),
                );
                asm_line.clear_struct_definition();
                counts.errors += 1;
            }

            let placement_directives = asm_line.take_placement_directives();
            if !asm_line.in_section() {
                for directive in &placement_directives {
                    let status = asm_line.apply_placement_directive(directive);
                    if status == LineStatus::Error || status == LineStatus::Pass1Error {
                        if let Some(err) = asm_line.error() {
                            diagnostics.push(Self::diagnostic_from_asmline(
                                &asm_line,
                                directive.line(),
                                Severity::Error,
                                err.clone(),
                            ));
                        }
                        counts.errors += 1;
                    }
                }
            }

            if finalize_section_symbols {
                for err in asm_line.finalize_section_symbol_addresses() {
                    diagnostics.push(Diagnostic::new(line_num, Severity::Error, err));
                    counts.errors += 1;
                }
            }

            for (name, section) in &asm_line.layout.sections {
                if section.default_region.is_some()
                    && !section.layout_placed
                    && !Self::section_can_be_unplaced_for_hunk_output(
                        name,
                        &asm_line.output_state.root_metadata.linker_outputs,
                    )
                {
                    let err = AsmError::new(
                        AsmErrorKind::Directive,
                        "Section with region=... must be explicitly placed",
                        Some(name),
                    );
                    diagnostics.push(Diagnostic::new(line_num, Severity::Error, err));
                    counts.errors += 1;
                }
            }

            for output in &asm_line.output_state.root_metadata.linker_outputs {
                let Some(section_names) = output.option_text_list("sections") else {
                    continue;
                };
                let requires_explicit_placement =
                    output.format() != Some(vm::output_model::LinkerOutputFormat::Hunk);
                for section_name in section_names {
                    let is_placed = asm_line
                        .layout
                        .sections
                        .get(section_name)
                        .map(|section| section.layout_placed)
                        .unwrap_or(false);
                    if requires_explicit_placement && !is_placed {
                        let err = AsmError::new(
                            AsmErrorKind::Directive,
                            "Section referenced by .output must be explicitly placed",
                            Some(section_name),
                        );
                        diagnostics.push(Diagnostic::new(line_num, Severity::Error, err));
                        counts.errors += 1;
                    }
                }
            }

            self.cpu = asm_line.cpu;
            self.section_symbol_sections = asm_line.layout.section_symbol_sections.clone();
            self.absolute_constant_symbols = asm_line.layout.absolute_constant_symbols.clone();
            self.root_metadata = asm_line.take_root_metadata();
            self.sections = asm_line.take_sections();
            self.regions = asm_line.take_regions();
        }

        for module in self.symbols.modules() {
            for import in &module.imports {
                for map in &import.section_maps {
                    let Some(target) = self.sections.get(&map.concrete) else {
                        let err = AsmError::new(
                            AsmErrorKind::Directive,
                            &format!(
                                "Import section map target '{}' is not a declared concrete section",
                                map.concrete
                            ),
                            Some(&map.logical),
                        );
                        diagnostics.push(
                            Diagnostic::new(map.span.line, Severity::Error, err)
                                .with_column(Some(map.span.col_start)),
                        );
                        counts.errors += 1;
                        continue;
                    };
                    if target.logical {
                        let err = AsmError::new(
                            AsmErrorKind::Directive,
                            "Import section map target must be a concrete section",
                            Some(&map.concrete),
                        );
                        diagnostics.push(
                            Diagnostic::new(map.span.line, Severity::Error, err)
                                .with_column(Some(map.span.col_start)),
                        );
                        counts.errors += 1;
                    }
                    let source_kind = self
                        .symbols
                        .modules()
                        .iter()
                        .find(|dep| dep.name.eq_ignore_ascii_case(&import.module_id))
                        .and_then(|dep| {
                            dep.logical_sections
                                .iter()
                                .find(|section| section.name.eq_ignore_ascii_case(&map.logical))
                        })
                        .map(|section| section.kind);
                    if let Some(source_kind) = source_kind {
                        let compatible = matches!(
                            (source_kind, target.kind),
                            (types::symbol::LogicalSectionKind::Code, SectionKind::Code)
                                | (types::symbol::LogicalSectionKind::Data, SectionKind::Data)
                                | (types::symbol::LogicalSectionKind::Bss, SectionKind::Bss)
                        );
                        if !compatible {
                            let err = AsmError::new(
                                AsmErrorKind::Directive,
                                "Import section map kind is incompatible with target section kind",
                                Some(&map.logical),
                            );
                            diagnostics.push(
                                Diagnostic::new(map.span.line, Severity::Error, err)
                                    .with_column(Some(map.span.col_start)),
                            );
                            counts.errors += 1;
                        }
                    }
                }
            }
        }

        for issue in self.symbols.validate_imports(&self.module_macro_names) {
            let kind = match issue.kind {
                types::symbol::ImportIssueKind::Directive => AsmErrorKind::Directive,
                types::symbol::ImportIssueKind::Symbol => AsmErrorKind::Symbol,
            };
            let err = AsmError::new(kind, &issue.message, issue.param.as_deref());
            diagnostics
                .push(Diagnostic::new(issue.line, Severity::Error, err).with_column(issue.column));
            counts.errors += 1;
        }

        counts.lines = u32::try_from(lines.len()).unwrap_or(u32::MAX);
        counts
    }

    pub fn new() -> Self {
        Self::with_registry(build_default_registry_for_tests())
    }

    pub fn with_registry(registry: ModuleRegistry) -> Self {
        Self::with_cpu_and_registry(CpuType::new("8085"), registry)
    }

    pub fn with_cpu(cpu: CpuType) -> Self {
        Self::with_cpu_and_registry(cpu, build_default_registry_for_tests())
    }

    pub fn with_cpu_and_registry(cpu: CpuType, registry: ModuleRegistry) -> Self {
        Self {
            symbols: SymbolTable::new(),
            image: ImageStore::new(),
            sections: HashMap::new(),
            regions: HashMap::new(),
            section_symbol_sections: HashMap::new(),
            absolute_constant_symbols: HashSet::new(),
            diagnostics: Vec::new(),
            cpu,
            registry,
            root_metadata: RootMetadata::default(),
            module_macro_names: HashMap::new(),
            loop_iteration_trace_pass1: Vec::new(),
            max_loop_iterations: repetition::DEFAULT_MAX_LOOP_ITERATIONS,
            opasm_package_path: None,
            runtime_line_router: None,
            runtime_processing_traces: Vec::new(),
            runtime_lockstep_report: LockstepReport::default(),
            implicit_hunk_output_requested: false,
        }
    }

    pub fn cpu(&self) -> CpuType {
        self.cpu
    }

    pub fn symbols(&self) -> &SymbolTable {
        &self.symbols
    }

    pub fn image(&self) -> &ImageStore {
        &self.image
    }

    pub fn sections(&self) -> &HashMap<String, SectionState> {
        &self.sections
    }

    pub fn regions(&self) -> &HashMap<String, RegionState> {
        &self.regions
    }

    pub fn clear_diagnostics(&mut self) {
        self.diagnostics.clear();
    }

    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = std::mem::take(&mut self.diagnostics);
        Self::dedup_diagnostics_preserving_order(&mut diagnostics);
        diagnostics
    }

    pub fn set_runtime_line_router(
        &mut self,
        runtime_line_router: Option<Rc<dyn RuntimeLineRouter>>,
    ) {
        self.runtime_line_router = runtime_line_router;
    }

    pub fn set_implicit_hunk_output_requested(&mut self, requested: bool) {
        self.implicit_hunk_output_requested = requested;
    }

    pub fn runtime_processing_traces(&self) -> &[(u8, u32, LineProcessingTrace)] {
        &self.runtime_processing_traces
    }

    pub fn runtime_lockstep_report(&self) -> &LockstepReport {
        &self.runtime_lockstep_report
    }

    fn dedup_diagnostics_preserving_order(diagnostics: &mut Vec<Diagnostic>) {
        let mut unique = Vec::with_capacity(diagnostics.len());
        for diagnostic in diagnostics.drain(..) {
            if unique
                .iter()
                .any(|existing| Self::diagnostics_match(existing, &diagnostic))
            {
                continue;
            }
            unique.push(diagnostic);
        }
        *diagnostics = unique;
    }

    fn dedup_current_diagnostics(&mut self) {
        Self::dedup_diagnostics_preserving_order(&mut self.diagnostics);
    }

    fn diagnostics_match(left: &Diagnostic, right: &Diagnostic) -> bool {
        left.line == right.line
            && left.column == right.column
            && left.col_end == right.col_end
            && left.code == right.code
            && left.severity == right.severity
            && left.error.kind() == right.error.kind()
            && left.error.message() == right.error.message()
            && left.file == right.file
            && left.source == right.source
            && left.parser_error == right.parser_error
            && left.related_spans == right.related_spans
            && left.notes == right.notes
            && left.help == right.help
            && left.fixits == right.fixits
    }

    pub fn pass1(&mut self, lines: &[String]) -> PassCounts {
        self.loop_iteration_trace_pass1.clear();
        self.runtime_processing_traces.clear();
        self.runtime_lockstep_report = LockstepReport::default();
        let mut pass1_loop_trace = Vec::new();
        let mut counts = self.run_layout_pass(lines, 1, true, true, &mut pass1_loop_trace);
        self.loop_iteration_trace_pass1 = pass1_loop_trace;
        if counts.errors > 0 {
            self.dedup_current_diagnostics();
            return counts;
        }
        if !self.cpu_requires_layout_stabilization() {
            self.dedup_current_diagnostics();
            return counts;
        }

        let mut previous_snapshot = self.capture_layout_snapshot();
        let mut stabilized = false;
        for _ in 0..MAX_LAYOUT_STABILIZATION_PASSES {
            let mut loop_trace = Vec::new();
            counts = self.run_layout_pass(lines, 2, false, true, &mut loop_trace);
            if counts.errors > 0 {
                self.dedup_current_diagnostics();
                return counts;
            }

            let next_snapshot = self.capture_layout_snapshot();
            if next_snapshot == previous_snapshot {
                stabilized = true;
                break;
            }
            previous_snapshot = next_snapshot;
        }

        if !stabilized {
            self.diagnostics.push(Diagnostic::new(
                u32::try_from(lines.len())
                    .unwrap_or(u32::MAX.saturating_sub(1))
                    .saturating_add(1),
                Severity::Error,
                AsmError::new(
                    AsmErrorKind::Directive,
                    "layout did not stabilize after residual branch sizing retries",
                    None,
                ),
            ));
            counts.errors += 1;
        }

        self.dedup_current_diagnostics();
        counts
    }

    pub fn pass2<W: Write>(
        &mut self,
        lines: &[String],
        listing: &mut ListingWriter<W>,
    ) -> std::io::Result<PassCounts> {
        let pass1_loop_trace = self.loop_iteration_trace_pass1.clone();
        let uses_implicit_hunk_code_section =
            Self::uses_implicit_hunk_code_section(lines, self.implicit_hunk_output_requested);
        let mut asm_line = AsmLine::with_cpu(&mut self.symbols, self.cpu, &self.registry);
        asm_line.set_runtime_package_path(self.opasm_package_path.as_deref());
        asm_line.set_runtime_line_router(self.runtime_line_router.clone());
        asm_line.clear_conditionals();
        asm_line.clear_scopes();
        asm_line.layout.section_symbol_sections = self.section_symbol_sections.clone();
        asm_line.layout.absolute_constant_symbols = self.absolute_constant_symbols.clone();
        // Seed pass2 with pass1 placement/layout state so section-local encoding
        // (especially relative branches) uses rebased absolute addresses even if
        // .place/.pack directives appear later in source order.
        asm_line.layout.sections = self.sections.clone();
        asm_line.layout.regions = self.regions.clone();
        for section in asm_line.layout.sections.values_mut() {
            section.pc = 0;
            section.bytes.clear();
            section.relocation_free_certified = true;
            section.hunk_relocation_compatible = true;
            section.hunk_fixup_error = None;
            section.output_fixups.clear();
            section.emitted = false;
        }
        if uses_implicit_hunk_code_section {
            Self::seed_implicit_hunk_code_section(&mut asm_line);
        }
        self.image = ImageStore::new();

        let mut addr: u32 = 0;
        let line_num: u32 = u32::try_from(lines.len())
            .unwrap_or(u32::MAX.saturating_sub(1))
            .saturating_add(1);
        let mut counts = PassCounts::new();
        let diagnostics = &mut self.diagnostics;
        let image = &mut self.image;
        let mut pass2_loop_trace_cursor = 0usize;

        if let Some(err) = image.init_error() {
            let message = format!("failed to initialize image store: {err}");
            let diag = Diagnostic::new(
                line_num,
                Severity::Error,
                AsmError::new(AsmErrorKind::Io, &message, None),
            );
            diagnostics.push(diag.clone());
            listing.write_diagnostic_with_annotations(&diag, lines)?;
            counts.errors += 1;
            counts.lines = u32::try_from(lines.len()).unwrap_or(u32::MAX);
            return Ok(counts);
        }

        Self::execute_pass2_lines(
            lines,
            0,
            lines.len(),
            &mut asm_line,
            &mut addr,
            &mut counts,
            diagnostics,
            listing,
            image,
            &pass1_loop_trace,
            &mut pass2_loop_trace_cursor,
            None,
            self.max_loop_iterations,
        )?;

        self.runtime_processing_traces.extend(
            asm_line
                .take_runtime_processing_traces()
                .into_iter()
                .map(|(line_num, trace)| (2, line_num, trace)),
        );
        self.runtime_lockstep_report
            .extend(asm_line.take_runtime_lockstep_report());
        #[cfg(test)]
        Self::assert_partitioned_runtime_traces_present(lines, &self.runtime_processing_traces);

        if !asm_line.cond_is_empty() {
            let err = AsmError::new(AsmErrorKind::Conditional, "Found .if without .endif", None);
            let diag = Diagnostic::new(line_num, Severity::Error, err.clone())
                .with_help("add a matching .endif to close the open conditional block")
                .with_fixit(asm::error::Fixit {
                    file: None,
                    line: line_num,
                    col_start: Some(1),
                    col_end: Some(1),
                    replacement: ".endif".to_string(),
                    applicability: "machine-applicable".to_string(),
                });
            diagnostics.push(diag.clone());
            listing.write_diagnostic_with_annotations(&diag, lines)?;
            asm_line.clear_conditionals();
            counts.errors += 1;
        }

        if asm_line.in_module() {
            let err = AsmError::new(
                AsmErrorKind::Directive,
                "Found .module without .endmodule",
                None,
            );
            let diag = Diagnostic::new(line_num, Severity::Error, err.clone())
                .with_help("add a matching .endmodule to close the open module block")
                .with_fixit(asm::error::Fixit {
                    file: None,
                    line: line_num,
                    col_start: Some(1),
                    col_end: Some(1),
                    replacement: ".endmodule".to_string(),
                    applicability: "machine-applicable".to_string(),
                });
            diagnostics.push(diag.clone());
            listing.write_diagnostic_with_annotations(&diag, lines)?;
            counts.errors += 1;
        }

        if asm_line.in_user_section() {
            let err = AsmError::new(
                AsmErrorKind::Directive,
                "Found .section without .endsection",
                None,
            );
            let diag = Diagnostic::new(line_num, Severity::Error, err.clone())
                .with_help("add a matching .endsection to close the open section block")
                .with_fixit(asm::error::Fixit {
                    file: None,
                    line: line_num,
                    col_start: Some(1),
                    col_end: Some(1),
                    replacement: ".endsection".to_string(),
                    applicability: "machine-applicable".to_string(),
                });
            diagnostics.push(diag.clone());
            listing.write_diagnostic_with_annotations(&diag, lines)?;
            counts.errors += 1;
        }

        if let Some(open_line) = asm_line.open_struct_line() {
            let err = AsmError::new(
                AsmErrorKind::Directive,
                &format!("unterminated .struct (opened at line {open_line})"),
                None,
            );
            let diag = Diagnostic::new(line_num, Severity::Error, err.clone())
                .with_help("add a matching .endstruct to close the open struct definition")
                .with_fixit(asm::error::Fixit {
                    file: None,
                    line: line_num,
                    col_start: Some(1),
                    col_end: Some(1),
                    replacement: ".endstruct".to_string(),
                    applicability: "machine-applicable".to_string(),
                });
            diagnostics.push(diag.clone());
            listing.write_diagnostic_with_annotations(&diag, lines)?;
            asm_line.clear_struct_definition();
            counts.errors += 1;
        }

        let sections = asm_line.take_sections();
        let mut deferred_sections: Vec<_> = sections
            .iter()
            .filter_map(|(name, section)| {
                if section.is_bss() || section.bytes.is_empty() || section.emitted {
                    return None;
                }
                section
                    .base_addr
                    .map(|base_addr| (base_addr, name, section))
            })
            .collect();
        deferred_sections.sort_by_key(|(base_addr, name, _)| (*base_addr, *name));
        for (base_addr, _, section) in deferred_sections {
            image.store_slice(base_addr, &section.bytes);
        }

        if Self::cpu_warns_for_wide_output(asm_line.cpu) {
            if let Ok(Some((_min_addr, max_addr))) = image.output_range() {
                if max_addr > 0xFFFF {
                    let message = format!(
                        "assembled output exceeds 64 KB for CPU {} (max emitted address ${max_addr:08X})",
                        asm_line.cpu.as_str()
                    );
                    let diag = Diagnostic::new(
                        line_num.saturating_sub(1),
                        Severity::Warning,
                        AsmError::new(AsmErrorKind::Assembler, &message, None),
                    );
                    diagnostics.push(diag.clone());
                    listing.write_diagnostic_with_annotations(&diag, lines)?;
                    counts.warnings += 1;
                }
            }
        }

        self.cpu = asm_line.cpu;
        self.absolute_constant_symbols = asm_line.layout.absolute_constant_symbols.clone();
        self.sections = sections;
        self.refresh_hunk_output_relocation_dispositions();
        counts.lines = u32::try_from(lines.len()).unwrap_or(u32::MAX);
        self.dedup_current_diagnostics();
        Ok(counts)
    }

    fn uses_implicit_hunk_code_section(
        lines: &[String],
        implicit_hunk_output_requested: bool,
    ) -> bool {
        let mut has_explicit_section = false;
        let mut has_hunk_output_without_sections = false;
        for line in lines {
            let source = line.split(';').next().unwrap_or("").to_ascii_lowercase();
            let trimmed = source.trim_start();
            let compact: String = source.chars().filter(|c| !c.is_whitespace()).collect();
            if trimmed.starts_with(".section") {
                has_explicit_section = true;
            }
            if trimmed.starts_with(".output")
                && (compact.contains("format=hunk") || compact.contains("format=\"hunk\""))
                && !compact.contains("sections=")
            {
                has_hunk_output_without_sections = true;
            }
        }

        (implicit_hunk_output_requested || has_hunk_output_without_sections)
            && !has_explicit_section
    }

    fn seed_implicit_hunk_code_section(asm_line: &mut AsmLine<'_>) {
        asm_line
            .layout
            .sections
            .entry(IMPLICIT_HUNK_CODE_SECTION_NAME.to_string())
            .or_insert_with(|| SectionState {
                align: 1,
                kind: vm::output_model::SectionKind::Code,
                ..Default::default()
            });
        asm_line.layout.current_section = Some(IMPLICIT_HUNK_CODE_SECTION_NAME.to_string());
    }

    fn refresh_hunk_output_relocation_dispositions(&mut self) {
        for output in &mut self.root_metadata.linker_outputs {
            if output.format() != Some(LinkerOutputFormat::Hunk) {
                continue;
            }
            output.relocation_disposition =
                Self::hunk_output_relocation_disposition(output, &self.sections);
        }
    }

    pub fn hunk_output_relocation_disposition_for(
        &self,
        output: &LinkerOutputDirective,
    ) -> vm::output_model::LinkerOutputRelocationDisposition {
        Self::hunk_output_relocation_disposition(output, &self.sections)
    }

    fn hunk_output_relocation_disposition(
        output: &LinkerOutputDirective,
        sections: &HashMap<String, SectionState>,
    ) -> vm::output_model::LinkerOutputRelocationDisposition {
        let implicit_section_names;
        let section_names = if let Some(section_names) = output.option_text_list("sections") {
            section_names
        } else if sections.contains_key(IMPLICIT_HUNK_CODE_SECTION_NAME) {
            implicit_section_names = [IMPLICIT_HUNK_CODE_SECTION_NAME.to_string()];
            &implicit_section_names
        } else {
            return vm::output_model::LinkerOutputRelocationDisposition::Unknown;
        };
        if section_names.is_empty() {
            return vm::output_model::LinkerOutputRelocationDisposition::Unknown;
        }
        if section_names.iter().all(|section_name| {
            sections
                .get(section_name)
                .is_some_and(|section| section.relocation_free_certified)
        }) {
            vm::output_model::LinkerOutputRelocationDisposition::ProvenRelocationFree
        } else if section_names.iter().all(|section_name| {
            sections.get(section_name).is_some_and(|section| {
                section.hunk_relocation_compatible
                    && (section.relocation_free_certified || !section.output_fixups.is_empty())
            })
        }) {
            vm::output_model::LinkerOutputRelocationDisposition::RelocationRecordsPresent
        } else {
            vm::output_model::LinkerOutputRelocationDisposition::Unknown
        }
    }

    fn section_can_be_unplaced_for_hunk_output(
        section_name: &str,
        outputs: &[LinkerOutputDirective],
    ) -> bool {
        let mut referenced_by_hunk = false;
        for output in outputs {
            let Some(section_names) = output.option_text_list("sections") else {
                continue;
            };
            if !section_names
                .iter()
                .any(|candidate| candidate.eq_ignore_ascii_case(section_name))
            {
                continue;
            }
            if output.format() == Some(vm::output_model::LinkerOutputFormat::Hunk) {
                referenced_by_hunk = true;
            } else {
                return false;
            }
        }
        referenced_by_hunk
    }

    #[allow(clippy::too_many_arguments)]
    fn execute_pass1_lines(
        lines: &[String],
        start_idx: usize,
        end_idx_exclusive: usize,
        asm_line: &mut AsmLine<'_>,
        addr: &mut u32,
        counts: &mut PassCounts,
        diagnostics: &mut Vec<Diagnostic>,
        pass1_loop_trace: &mut Vec<(u32, u32)>,
        unscoped_repeat_kind: Option<UnscopedRepeatKind>,
        max_loop_iterations: u32,
        pass_num: u8,
    ) {
        let mut traversal = Pass1RepetitionTraversal {
            counts,
            diagnostics,
            pass1_loop_trace,
            pass_num,
        };
        match execute_repetition_lines(
            &mut traversal,
            lines,
            start_idx,
            end_idx_exclusive,
            asm_line,
            addr,
            unscoped_repeat_kind,
            max_loop_iterations,
        ) {
            Ok(()) => {}
            Err(err) => match err {},
        }
    }

    fn execute_regular_line_pass1(
        asm_line: &mut AsmLine<'_>,
        src: &str,
        line_num: u32,
        addr: &mut u32,
        counts: &mut PassCounts,
        diagnostics: &mut Vec<Diagnostic>,
        pass_num: u8,
    ) {
        let line_addr = match asm_line.current_addr(*addr) {
            Ok(line_addr) => line_addr,
            Err(()) => {
                if let Some(err) = asm_line.error() {
                    diagnostics.push(Self::diagnostic_from_asmline(
                        asm_line,
                        line_num,
                        Severity::Error,
                        err.clone(),
                    ));
                }
                counts.errors += 1;
                *addr
            }
        };

        let status = asm_line.process(src, line_num, line_addr, pass_num);
        let status_failed = status == LineStatus::Pass1Error || status == LineStatus::Error;
        let update_failed = !status_failed && asm_line.update_addresses(addr, status).is_err();
        if status_failed || update_failed {
            if let Some(err) = asm_line.error() {
                diagnostics.push(Self::diagnostic_from_asmline(
                    asm_line,
                    line_num,
                    Severity::Error,
                    err.clone(),
                ));
            }
            counts.errors += 1;
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn execute_pass2_lines<W: Write>(
        lines: &[String],
        start_idx: usize,
        end_idx_exclusive: usize,
        asm_line: &mut AsmLine<'_>,
        addr: &mut u32,
        counts: &mut PassCounts,
        diagnostics: &mut Vec<Diagnostic>,
        listing: &mut ListingWriter<W>,
        image: &mut ImageStore,
        pass1_loop_trace: &[(u32, u32)],
        pass2_loop_trace_cursor: &mut usize,
        unscoped_repeat_kind: Option<UnscopedRepeatKind>,
        max_loop_iterations: u32,
    ) -> std::io::Result<()> {
        let mut traversal = Pass2RepetitionTraversal {
            counts,
            diagnostics,
            listing,
            image,
            pass1_loop_trace,
            pass2_loop_trace_cursor,
        };
        execute_repetition_lines(
            &mut traversal,
            lines,
            start_idx,
            end_idx_exclusive,
            asm_line,
            addr,
            unscoped_repeat_kind,
            max_loop_iterations,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn execute_regular_line_pass2<W: Write>(
        asm_line: &mut AsmLine<'_>,
        src: &str,
        line_num: u32,
        addr: &mut u32,
        counts: &mut PassCounts,
        diagnostics: &mut Vec<Diagnostic>,
        listing: &mut ListingWriter<W>,
        image: &mut ImageStore,
        all_lines: &[String],
    ) -> std::io::Result<()> {
        let line_addr = match asm_line.current_addr(*addr) {
            Ok(line_addr) => line_addr,
            Err(()) => {
                if let Some(err) = asm_line.error() {
                    diagnostics.push(Self::diagnostic_from_asmline(
                        asm_line,
                        line_num,
                        Severity::Error,
                        err.clone(),
                    ));
                    listing.write_diagnostic(
                        "ERROR",
                        err.message(),
                        line_num,
                        asm_line.error_column(),
                        all_lines,
                        asm_line.parser_error_ref(),
                    )?;
                }
                counts.errors += 1;
                *addr
            }
        };
        let status = asm_line.process(src, line_num, line_addr, 2);
        let line_addr = asm_line.start_addr();
        let bytes = asm_line.bytes();
        if !bytes.is_empty() && !asm_line.in_section() {
            image.store_slice(line_addr, bytes);
        }

        listing.write_line(ListingLine {
            addr: line_addr,
            bytes,
            status,
            aux: asm_line.aux_value(),
            line_num,
            source: src,
            section: asm_line.current_section_name(),
            cond: asm_line.cond_last(),
        })?;

        match status {
            LineStatus::Error | LineStatus::Pass1Error => {
                if let Some(err) = asm_line.error() {
                    diagnostics.push(Self::diagnostic_from_asmline(
                        asm_line,
                        line_num,
                        Severity::Error,
                        err.clone(),
                    ));
                    listing.write_diagnostic(
                        "ERROR",
                        err.message(),
                        line_num,
                        asm_line.error_column(),
                        all_lines,
                        asm_line.parser_error_ref(),
                    )?;
                }
                counts.errors += 1;
            }
            LineStatus::Warning => {
                if let Some(err) = asm_line.error() {
                    diagnostics.push(Self::diagnostic_from_asmline(
                        asm_line,
                        line_num,
                        Severity::Warning,
                        err.clone(),
                    ));
                    listing.write_diagnostic(
                        "WARNING",
                        err.message(),
                        line_num,
                        asm_line.error_column(),
                        all_lines,
                        asm_line.parser_error_ref(),
                    )?;
                }
                counts.warnings += 1;
            }
            _ => {}
        }

        if asm_line.update_addresses(addr, status).is_err() {
            if let Some(err) = asm_line.error() {
                diagnostics.push(Self::diagnostic_from_asmline(
                    asm_line,
                    line_num,
                    Severity::Error,
                    err.clone(),
                ));
                listing.write_diagnostic(
                    "ERROR",
                    err.message(),
                    line_num,
                    asm_line.error_column(),
                    all_lines,
                    None,
                )?;
            }
            counts.errors += 1;
        }
        Ok(())
    }

    fn cpu_warns_for_wide_output(cpu: CpuType) -> bool {
        // `8080` is retained as a defensive alias for direct helper calls/tests,
        // even though registry-backed Intel-family resolution currently canonicalizes
        // to concrete CPU ids (`8085`/`z80`).
        matches!(
            cpu.as_str(),
            "m6502" | "65c02" | "8080" | "8085" | "z80" | "m6809" | "hd6309"
        )
    }

    fn diagnostic_from_asmline(
        asm_line: &AsmLine<'_>,
        line_num: u32,
        severity: Severity,
        err: AsmError,
    ) -> Diagnostic {
        let mut diagnostic = Diagnostic::new(line_num, severity, err)
            .with_column(asm_line.error_column())
            .with_parser_error(
                asm_line
                    .parser_error()
                    .map(asm::error::parse_error_to_diagnostic),
            );

        if let Some(help) = asm_line.error_help() {
            diagnostic = diagnostic.with_help(help.to_string());
        }
        for fixit in asm_line.error_fixits() {
            diagnostic = diagnostic.with_fixit(fixit.clone());
        }
        diagnostic
    }
}

struct Pass1RepetitionTraversal<'a> {
    counts: &'a mut PassCounts,
    diagnostics: &'a mut Vec<Diagnostic>,
    pass1_loop_trace: &'a mut Vec<(u32, u32)>,
    pass_num: u8,
}

impl RepetitionPass for Pass1RepetitionTraversal<'_> {
    type Error = Infallible;

    fn before_label_restriction_error(&mut self, asm_line: &mut AsmLine<'_>, line_num: u32) {
        asm_line.record_default_processing_trace(line_num);
    }

    fn before_unmatched_end_error(&mut self, asm_line: &mut AsmLine<'_>, line_num: u32) {
        asm_line.record_default_processing_trace(line_num);
    }

    fn emit_error(
        &mut self,
        diagnostic: Diagnostic,
        _all_lines: &[String],
    ) -> Result<(), Self::Error> {
        self.diagnostics.push(diagnostic);
        self.counts.errors += 1;
        Ok(())
    }

    fn observe_loop_iterations(
        &mut self,
        line_num: u32,
        iterations: u32,
        _all_lines: &[String],
    ) -> Result<(), Self::Error> {
        self.pass1_loop_trace.push((line_num, iterations));
        Ok(())
    }

    fn execute_regular_line(
        &mut self,
        asm_line: &mut AsmLine<'_>,
        src: &str,
        line_num: u32,
        addr: &mut u32,
        _all_lines: &[String],
    ) -> Result<(), Self::Error> {
        Assembler::execute_regular_line_pass1(
            asm_line,
            src,
            line_num,
            addr,
            self.counts,
            self.diagnostics,
            self.pass_num,
        );
        Ok(())
    }
}

struct Pass2RepetitionTraversal<'a, W: Write> {
    counts: &'a mut PassCounts,
    diagnostics: &'a mut Vec<Diagnostic>,
    listing: &'a mut ListingWriter<W>,
    image: &'a mut ImageStore,
    pass1_loop_trace: &'a [(u32, u32)],
    pass2_loop_trace_cursor: &'a mut usize,
}

impl<W: Write> RepetitionPass for Pass2RepetitionTraversal<'_, W> {
    type Error = std::io::Error;

    fn emit_error(
        &mut self,
        diagnostic: Diagnostic,
        all_lines: &[String],
    ) -> Result<(), Self::Error> {
        self.diagnostics.push(diagnostic.clone());
        self.listing
            .write_diagnostic_with_annotations(&diagnostic, all_lines)?;
        self.counts.errors += 1;
        Ok(())
    }

    fn observe_loop_iterations(
        &mut self,
        line_num: u32,
        iterations: u32,
        all_lines: &[String],
    ) -> Result<(), Self::Error> {
        let (pass1_line, pass1_count) = self
            .pass1_loop_trace
            .get(*self.pass2_loop_trace_cursor)
            .copied()
            .unwrap_or((line_num, 0));
        *self.pass2_loop_trace_cursor = self.pass2_loop_trace_cursor.saturating_add(1);
        if pass1_line != line_num || pass1_count != iterations {
            let message = format!(
                "loop iteration count changed between passes (pass1: {pass1_count}, pass2: {iterations})"
            );
            self.emit_error(
                Diagnostic::new(
                    line_num,
                    Severity::Error,
                    AsmError::new(AsmErrorKind::Directive, &message, None),
                ),
                all_lines,
            )?;
        }
        Ok(())
    }

    fn execute_regular_line(
        &mut self,
        asm_line: &mut AsmLine<'_>,
        src: &str,
        line_num: u32,
        addr: &mut u32,
        all_lines: &[String],
    ) -> Result<(), Self::Error> {
        Assembler::execute_regular_line_pass2(
            asm_line,
            src,
            line_num,
            addr,
            self.counts,
            self.diagnostics,
            self.listing,
            self.image,
            all_lines,
        )
    }
}

impl Default for Assembler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::Assembler;
    use crate::error::Severity;
    use crate::listing::ListingWriter;
    use registry::cpu::CpuType;

    fn run_wide_output_case(cpu: CpuType) -> (usize, Vec<String>, Vec<(u32, u8)>) {
        let mut assembler = Assembler::with_cpu(cpu);
        assembler.clear_diagnostics();

        let lines = vec![".org $10000".to_string(), ".byte $aa".to_string()];
        let pass1 = assembler.pass1(&lines);
        assert_eq!(
            pass1.errors,
            0,
            "pass1 should succeed for {:?}; diagnostics: {:?}",
            cpu,
            assembler
                .diagnostics
                .iter()
                .map(|diag| diag.error.message().to_string())
                .collect::<Vec<_>>()
        );

        let mut listing_out = Vec::new();
        let mut listing = ListingWriter::new(&mut listing_out, false);
        let pass2 = assembler
            .pass2(&lines, &mut listing)
            .expect("pass2 should run");
        assert_eq!(pass2.errors, 0, "pass2 should succeed for {:?}", cpu);

        let warning_messages: Vec<String> = assembler
            .diagnostics
            .iter()
            .filter(|diag| diag.severity() == Severity::Warning)
            .map(|diag| diag.error.message().to_string())
            .collect();
        let entries = assembler
            .image
            .entries()
            .expect("image entries should be readable");

        (pass2.warnings as usize, warning_messages, entries)
    }

    fn run_legacy_cross_boundary_case(cpu: CpuType) -> Vec<String> {
        let mut assembler = Assembler::with_cpu(cpu);
        assembler.clear_diagnostics();

        let lines = vec![".org $ffff".to_string(), ".byte $aa, $bb".to_string()];
        let _ = assembler.pass1(&lines);
        assembler
            .diagnostics
            .iter()
            .map(|diag| diag.error.message().to_string())
            .collect()
    }

    #[test]
    fn wide_output_warning_policy_matches_target_cpu() {
        assert!(Assembler::cpu_warns_for_wide_output(CpuType::new("m6502")));
        assert!(Assembler::cpu_warns_for_wide_output(CpuType::new("65c02")));
        assert!(Assembler::cpu_warns_for_wide_output(CpuType::new("8080")));
        assert!(Assembler::cpu_warns_for_wide_output(CpuType::new("8085")));
        assert!(Assembler::cpu_warns_for_wide_output(CpuType::new("z80")));
        assert!(Assembler::cpu_warns_for_wide_output(CpuType::new("m6809")));
        assert!(Assembler::cpu_warns_for_wide_output(CpuType::new("hd6309")));
        assert!(!Assembler::cpu_warns_for_wide_output(CpuType::new("65816")));
        assert!(!Assembler::cpu_warns_for_wide_output(CpuType::new(
            "45gs02"
        )));
    }

    #[test]
    fn wide_output_integration_suppresses_warning_for_65816() {
        let cpu = CpuType::new("65816");
        let (warnings, warning_messages, entries) = run_wide_output_case(cpu);
        assert!(
            entries
                .iter()
                .any(|(addr, val)| *addr == 0x010000 && *val == 0xaa),
            "wide-output byte should be emitted for {:?}",
            cpu
        );
        assert_eq!(warnings, 0, "unexpected wide-output warning for {:?}", cpu);
        assert!(
            !warning_messages
                .iter()
                .any(|message| message.contains("assembled output exceeds 64 KB")),
            "unexpected wide-output warning diagnostic for {:?}: {warning_messages:?}",
            cpu
        );
    }

    #[test]
    fn legacy_cross_boundary_output_is_rejected_before_warning_policy() {
        for cpu in [
            CpuType::new("m6502"),
            CpuType::new("65c02"),
            CpuType::new("8085"),
            CpuType::new("m6809"),
            CpuType::new("hd6309"),
        ] {
            let diagnostics = run_legacy_cross_boundary_case(cpu);
            assert!(
                diagnostics.iter().any(|message| {
                    message.contains("span")
                        && message.contains("exceeds max $FFFF")
                        && message.contains(cpu.as_str())
                }),
                "expected legacy span guard diagnostic for {:?}: {diagnostics:?}",
                cpu
            );
        }
    }

    #[test]
    fn pass2_reports_image_store_init_failure_as_diagnostic() {
        types::image::run_with_forced_open_failure_for_tests(|| {
            let mut assembler = Assembler::new();
            let lines = vec![".byte $01".to_string()];
            let pass1 = assembler.pass1(&lines);
            assert_eq!(pass1.errors, 0, "pass1 should succeed");

            let mut listing_out = Vec::new();
            let mut listing = ListingWriter::new(&mut listing_out, false);
            let pass2 = assembler
                .pass2(&lines, &mut listing)
                .expect("pass2 should return counts");
            assert_eq!(pass2.errors, 1);
            assert!(assembler.diagnostics.iter().any(|diag| {
                diag.severity() == Severity::Error
                    && diag
                        .error
                        .message()
                        .contains("failed to initialize image store")
            }));
        });
    }
}
