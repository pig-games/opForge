// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use crate::error::{AsmError, AsmErrorKind, Fixit, LineStatus};
use crate::opasm::{self, StatementRequest};
use crate::output::{
    format_addr, section_kind_name, BinOutputSpec, ExportSectionsDirective, ExportSectionsFormat,
    ExportSectionsInclude, HunkMemoryType, LinkerOutputDirective, MapFileDirective, MapSymbolsMode,
    PlacedSectionInfo, PlacementDirective, RegionState, RootMetadata, SectionKind, SectionOptions,
    SectionState,
};
use crate::runtime_config::{
    expr_eval_force_host_families_from_env, expr_eval_opt_in_families_from_env,
    expr_parser_force_host_families_from_env, expr_parser_opt_in_families_from_env,
    portable_expr_runtime_enabled_for_family,
};
use crate::runtime_model::{
    build_execution_model as build_opthread_execution_model,
    build_execution_model_for_request as build_opthread_execution_model_for_request,
};
use crate::state::{
    build_register_checker, ActiveStructDefinition, AsmCpuModeState, AsmDiagnosticsState,
    AsmLayoutState, AsmOutputState, AsmSymbolScopeState, EncodingScopeState,
};
#[cfg(not(feature = "vm-runtime-only"))]
use families::intel8080::module::Intel8080FamilyOperands;
#[cfg(not(feature = "vm-runtime-only"))]
use families::intel8080::FamilyOperand as IntelFamilyOperand;
#[cfg(not(feature = "vm-runtime-only"))]
use families::intel8080::{
    dialect::{canonical_suggestion_for_zilog_mnemonic, map_zilog_to_canonical},
    module::FAMILY_ID as INTEL8080_FAMILY_ID,
};
#[cfg(not(feature = "vm-runtime-only"))]
use families::m68k::module::{M68KFamilyOperands, M68KOperands, FAMILY_ID as M68K_FAMILY_ID};
#[cfg(not(feature = "vm-runtime-only"))]
use families::m68k::FamilyOperand as M68KFamilyOperand;
#[cfg(not(feature = "vm-runtime-only"))]
use families::mos6502::module::FAMILY_ID as MOS6502_FAMILY_ID;
use opcore::conditional::{ConditionalBlockKind, ConditionalSubType};
use opcore::conditional::{ConditionalContext, ConditionalStack};
use opcore::expression::{
    apply_assignment_op, eval_binary_op, eval_unary_op, expr_span, parse_number_text, AstEvalError,
    AstEvalErrorKind,
};
use opcore::imports::module_import_from_parser;
use opcore::parser as asm_parser;
use opcore::parser::{AssignOp, Expr, Label, LineAst, ParseError};
#[cfg(not(feature = "vm-runtime-only"))]
use opcore::parser::{BinaryOp, UnaryOp};
use opcore::struct_table::StructTable;
use opcore::tokenizer::{ConditionalKind, Span};
#[cfg(not(feature = "vm-runtime-only"))]
use registry::cpu::CpuFamily;
use registry::cpu::CpuType;
use registry::family::AssemblerContext;
#[cfg(not(feature = "vm-runtime-only"))]
use registry::registry::{FamilyOperandSet, OperandSet, ResolvedPipeline};
use registry::registry::{ModuleRegistry, RegistryError};
use registry::syntax::RegisterChecker;
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::rc::Rc;
use types::asm_value::{AsmValue, StructField};
use types::lockstep::{ExecutionMode, LockstepReport};
use types::processing::{LineProcessingTrace, ProcessingRequestKind};
use types::symbol::{
    ImportResult, ImportedSymbolResolution, SymbolTable, SymbolTableEntry, SymbolTableResult,
    SymbolVisibility,
};
use types::text_encoding::TextEncodingRegistry;
use vm::output_model::{OutputFixupRecord, IMPLICIT_HUNK_CODE_SECTION_NAME};
use vm::vm_opasm::HierarchyExecutionModel;

thread_local! {
    static HOST_EXPR_EVAL_FAILPOINT: Cell<bool> = const { Cell::new(false) };
}

fn ast_eval_error_kind_to_asm(kind: AstEvalErrorKind) -> AsmErrorKind {
    match kind {
        AstEvalErrorKind::Expression => AsmErrorKind::Expression,
        AstEvalErrorKind::Directive => AsmErrorKind::Directive,
        AstEvalErrorKind::Symbol => AsmErrorKind::Symbol,
        AstEvalErrorKind::Instruction => AsmErrorKind::Instruction,
    }
}

fn asm_error_kind_to_ast_eval(kind: AsmErrorKind) -> AstEvalErrorKind {
    match kind {
        AsmErrorKind::Directive => AstEvalErrorKind::Directive,
        AsmErrorKind::Symbol => AstEvalErrorKind::Symbol,
        AsmErrorKind::Instruction => AstEvalErrorKind::Instruction,
        _ => AstEvalErrorKind::Expression,
    }
}

fn ast_eval_from_asm_error(error: AsmError, span: Span) -> AstEvalError {
    AstEvalError::new(
        asm_error_kind_to_ast_eval(error.kind()),
        error.message(),
        span,
    )
}

fn ast_eval_error(kind: AsmErrorKind, message: &str, span: Span) -> AstEvalError {
    AstEvalError::new(asm_error_kind_to_ast_eval(kind), message, span)
}

pub trait RuntimeLineRouter {
    fn parse_line(
        &self,
        model: &HierarchyExecutionModel,
        cpu_id: &str,
        line: &str,
        line_num: u32,
        register_checker: &RegisterChecker,
    ) -> Result<RuntimeLineParseResult, ParseError>;
}

pub type RuntimeLineParseResult = (
    LineAst,
    Span,
    Option<String>,
    Option<LineProcessingTrace>,
    Option<LockstepReport>,
);

pub fn set_host_expr_eval_failpoint_for_tests(enabled: bool) {
    HOST_EXPR_EVAL_FAILPOINT.with(|flag| flag.set(enabled));
}

#[path = "asmline_conditionals.rs"]
mod asmline_conditionals;
#[path = "asmline_directives.rs"]
mod asmline_directives;
#[path = "asmline_directives_data.rs"]
mod asmline_directives_data;
#[path = "asmline_directives_layout.rs"]
mod asmline_directives_layout;
#[path = "asmline_directives_metadata.rs"]
mod asmline_directives_metadata;
#[path = "asmline_directives_scope.rs"]
mod asmline_directives_scope;
#[path = "asmline_directives_text.rs"]
mod asmline_directives_text;
#[path = "asmline_eval.rs"]
mod asmline_eval;
#[path = "asmline_instruction.rs"]
mod asmline_instruction;
#[path = "repetition.rs"]
pub mod repetition;

/// Per-line assembler state.
pub struct AsmLine<'a> {
    pub symbols: &'a mut SymbolTable,
    registry: &'a ModuleRegistry,
    pub cond_stack: ConditionalStack,
    pub symbol_scope: AsmSymbolScopeState,
    pub output_state: AsmOutputState,
    pub layout: AsmLayoutState,
    struct_table: StructTable,
    value_symbols: HashMap<String, AsmValue>,
    scalar_value_symbols: HashSet<String>,
    repeat_iteration_scopes: HashMap<String, Vec<String>>,
    active_struct: Option<ActiveStructDefinition>,
    diagnostics: AsmDiagnosticsState,
    current_line_num: u32,
    current_source_line: Option<String>,
    line_end_span: Option<Span>,
    line_end_token: Option<String>,
    pub bytes: Vec<u8>,
    pending_output_fixups: Vec<OutputFixupRecord>,
    start_addr: u32,
    aux_value: u32,
    pass: u8,
    label: Option<String>,
    mnemonic: Option<String>,
    pub cpu: CpuType,
    pub register_checker: RegisterChecker,
    runtime_line_router: Option<Rc<dyn RuntimeLineRouter>>,
    runtime_processing_traces: Vec<(u32, LineProcessingTrace)>,
    runtime_lockstep_report: LockstepReport,
    pub cpu_mode: AsmCpuModeState,
    pub opthread_expr_eval_opt_in_families: Vec<String>,
    pub opthread_expr_eval_force_host_families: Vec<String>,
    pub opthread_expr_parser_opt_in_families: Vec<String>,
    pub opthread_expr_parser_force_host_families: Vec<String>,
    pub opthread_execution_model: Option<HierarchyExecutionModel>,
    text_encoding_registry: TextEncodingRegistry,
    active_text_encoding: String,
    encoding_scope_stack: Vec<EncodingScopeState>,
    loop_vars: Vec<(String, u32)>,
    statement_depth: usize,
}

impl<'a> AsmLine<'a> {
    fn default_partitioned_processing_trace() -> LineProcessingTrace {
        let mut trace = LineProcessingTrace::default();
        trace.push(ProcessingRequestKind::Processor {
            processor: "asm".to_string(),
            kind: "statement".to_string(),
        });
        trace
    }

    pub(crate) fn record_default_processing_trace(&mut self, line_num: u32) {
        self.runtime_processing_traces
            .push((line_num, Self::default_partitioned_processing_trace()));
    }

    pub fn new(symbols: &'a mut SymbolTable, registry: &'a ModuleRegistry) -> Self {
        Self::with_cpu(symbols, CpuType::new("8085"), registry)
    }

    pub fn with_cpu(
        symbols: &'a mut SymbolTable,
        cpu: CpuType,
        registry: &'a ModuleRegistry,
    ) -> Self {
        Self::with_cpu_and_metadata(symbols, cpu, registry, RootMetadata::default())
    }

    pub fn with_cpu_and_metadata(
        symbols: &'a mut SymbolTable,
        cpu: CpuType,
        registry: &'a ModuleRegistry,
        root_metadata: RootMetadata,
    ) -> Self {
        let text_encoding_registry = TextEncodingRegistry::new();
        let active_text_encoding = text_encoding_registry.default_encoding_name().to_string();
        Self {
            symbols,
            registry,
            cond_stack: ConditionalStack::new(),
            symbol_scope: AsmSymbolScopeState::new(),
            output_state: AsmOutputState::new(root_metadata),
            layout: AsmLayoutState::new(),
            struct_table: StructTable::new(),
            value_symbols: HashMap::new(),
            scalar_value_symbols: HashSet::new(),
            repeat_iteration_scopes: HashMap::new(),
            active_struct: None,
            diagnostics: AsmDiagnosticsState::new(),
            current_line_num: 1,
            current_source_line: None,
            line_end_span: None,
            line_end_token: None,
            bytes: Vec::with_capacity(256),
            pending_output_fixups: Vec::new(),
            start_addr: 0,
            aux_value: 0,
            pass: 1,
            label: None,
            mnemonic: None,
            cpu,
            register_checker: build_register_checker(registry, cpu),
            runtime_line_router: None,
            runtime_processing_traces: Vec::new(),
            runtime_lockstep_report: LockstepReport::default(),
            cpu_mode: AsmCpuModeState::new(registry, cpu),
            opthread_expr_eval_opt_in_families: expr_eval_opt_in_families_from_env(),
            opthread_expr_eval_force_host_families: expr_eval_force_host_families_from_env(),
            opthread_expr_parser_opt_in_families: expr_parser_opt_in_families_from_env(),
            opthread_expr_parser_force_host_families: expr_parser_force_host_families_from_env(),
            opthread_execution_model: build_opthread_execution_model(registry, cpu),
            text_encoding_registry,
            active_text_encoding,
            encoding_scope_stack: Vec::new(),
            loop_vars: Vec::new(),
            statement_depth: 0,
        }
    }

    pub fn set_runtime_package_path(&mut self, opasm_package_path: Option<&Path>) {
        self.opthread_execution_model =
            build_opthread_execution_model_for_request(self.registry, self.cpu, opasm_package_path);
    }

    fn portable_expr_runtime_enabled_for_family(&self, family_id: &str) -> bool {
        portable_expr_runtime_enabled_for_family(
            family_id,
            &self.opthread_expr_eval_opt_in_families,
            &self.opthread_expr_eval_force_host_families,
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn portable_expr_runtime_force_host_for_family(&self, family_id: &str) -> bool {
        crate::runtime_config::portable_expr_runtime_force_host_for_family(
            family_id,
            &self.opthread_expr_eval_force_host_families,
        )
    }

    pub fn take_root_metadata(&mut self) -> RootMetadata {
        std::mem::take(&mut self.output_state.root_metadata)
    }

    pub fn set_runtime_line_router(
        &mut self,
        runtime_line_router: Option<Rc<dyn RuntimeLineRouter>>,
    ) {
        self.runtime_line_router = runtime_line_router;
    }

    pub fn take_runtime_processing_traces(&mut self) -> Vec<(u32, LineProcessingTrace)> {
        std::mem::take(&mut self.runtime_processing_traces)
    }

    pub fn take_runtime_lockstep_report(&mut self) -> LockstepReport {
        std::mem::take(&mut self.runtime_lockstep_report)
    }

    pub fn take_placement_directives(&mut self) -> Vec<PlacementDirective> {
        std::mem::take(&mut self.layout.placement_directives)
    }

    pub fn take_sections(&mut self) -> HashMap<String, SectionState> {
        std::mem::take(&mut self.layout.sections)
    }

    pub fn take_regions(&mut self) -> HashMap<String, RegionState> {
        std::mem::take(&mut self.layout.regions)
    }

    pub fn finalize_section_symbol_addresses(&mut self) -> Vec<AsmError> {
        let section_symbols = self.layout.section_symbol_sections.clone();
        let mut errors = Vec::new();
        let cpu_name = self.cpu.as_str().to_string();
        for (symbol_name, section_name) in section_symbols {
            let Some(base_addr) = self
                .layout
                .sections
                .get(&section_name)
                .and_then(|s| s.base_addr)
            else {
                continue;
            };
            if let Some(entry) = self.symbols.entry_mut(&symbol_name) {
                match entry.val.checked_add(base_addr) {
                    Some(value) => {
                        entry.val = value;
                        entry.updated = true;
                    }
                    None => {
                        let message = format!(
                            "Section symbol address overflows address arithmetic for CPU {cpu_name}"
                        );
                        errors.push(AsmError::new(
                            AsmErrorKind::Directive,
                            &message,
                            Some(&symbol_name),
                        ));
                    }
                }
            }
        }
        errors
    }

    pub fn error(&self) -> Option<&AsmError> {
        self.diagnostics.last_error.as_ref()
    }

    pub fn error_column(&self) -> Option<usize> {
        self.diagnostics.last_error_column
    }

    pub fn error_help(&self) -> Option<&str> {
        self.diagnostics.last_error_help.as_deref()
    }

    pub fn error_fixits(&self) -> &[Fixit] {
        &self.diagnostics.last_error_fixits
    }

    pub fn parser_error(&self) -> Option<ParseError> {
        self.diagnostics.last_parser_error.clone()
    }

    pub fn parser_error_ref(&self) -> Option<&ParseError> {
        self.diagnostics.last_parser_error.as_ref()
    }

    pub fn error_message(&self) -> &str {
        self.diagnostics
            .last_error
            .as_ref()
            .map(|err| err.message())
            .unwrap_or("")
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn num_bytes(&self) -> usize {
        self.bytes.len()
    }

    pub fn start_addr(&self) -> u32 {
        self.start_addr
    }

    pub fn aux_value(&self) -> u32 {
        self.aux_value
    }

    pub fn clear_conditionals(&mut self) {
        self.cond_stack.clear();
    }

    pub fn clear_scopes(&mut self) {
        self.symbol_scope.scope_stack.clear();
        self.symbol_scope.visibility_stack.clear();
        self.symbol_scope
            .visibility_stack
            .push(SymbolVisibility::Private);
        self.symbol_scope.module_active = None;
        self.symbol_scope.module_scope_depth = 0;
        self.output_state.in_meta_block = false;
        self.output_state.in_output_block = false;
        self.output_state.output_cpu_block = None;
        self.layout.sections.clear();
        self.layout.regions.clear();
        self.layout.placement_directives.clear();
        self.layout.section_symbol_sections.clear();
        self.layout.absolute_constant_symbols.clear();
        self.layout.section_stack.clear();
        self.layout.current_section = None;
        self.symbol_scope.saw_explicit_module = false;
        self.symbol_scope.top_level_content_seen = false;
        self.reset_cpu_runtime_profile();
        self.reset_text_encoding_profile();
    }

    fn reset_cpu_runtime_profile(&mut self) {
        self.cpu_mode = AsmCpuModeState::new(self.registry, self.cpu);
    }

    fn reset_text_encoding_profile(&mut self) {
        self.active_text_encoding = self
            .text_encoding_registry
            .default_encoding_name()
            .to_string();
        self.encoding_scope_stack.clear();
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn apply_cpu_runtime_state_after_encode(
        &mut self,
        cpu_handler: &dyn registry::registry::CpuHandlerDyn,
        mnemonic: &str,
        operands: &dyn registry::registry::OperandSet,
    ) {
        cpu_handler.update_runtime_state_after_encode(
            mnemonic,
            operands,
            &mut self.cpu_mode.state_flags,
        );
    }

    fn resolve_pipeline_for_cpu<'b>(
        registry: &'b ModuleRegistry,
        cpu: CpuType,
    ) -> Result<registry::registry::ResolvedPipeline<'b>, String> {
        registry
            .resolve_pipeline(cpu, None)
            .map_err(registry_error_message)
    }

    fn apply_cpu_runtime_directive(
        &mut self,
        directive: &str,
        operands: &[Expr],
    ) -> Result<bool, String> {
        let pipeline = Self::resolve_pipeline_for_cpu(self.registry, self.cpu)?;
        let mut state_flags = std::mem::take(&mut self.cpu_mode.state_flags);
        let result =
            pipeline
                .cpu
                .apply_runtime_directive(directive, operands, self, &mut state_flags);
        self.cpu_mode.state_flags = state_flags;
        result
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn opthread_form_allows_mnemonic(
        &self,
        pipeline: &registry::registry::ResolvedPipeline<'_>,
        mapped_mnemonic: &str,
    ) -> Result<bool, String> {
        if !vm::rollout::package_runtime_default_enabled_for_family(pipeline.family_id.as_str()) {
            return Ok(true);
        }
        let Some(model) = self.opthread_execution_model.as_ref() else {
            return Ok(true);
        };
        model
            .supports_mnemonic(self.cpu.as_str(), None, mapped_mnemonic)
            .map_err(|err| err.to_string())
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn opthread_runtime_expr_operands_from_mapped(
        mapped_operands: &dyn registry::registry::FamilyOperandSet,
    ) -> Option<Vec<Expr>> {
        let intel_operands = mapped_operands
            .as_any()
            .downcast_ref::<Intel8080FamilyOperands>()?;
        let mut exprs = Vec::with_capacity(intel_operands.0.len());
        for operand in &intel_operands.0 {
            let expr = match operand {
                IntelFamilyOperand::Register(name, span)
                | IntelFamilyOperand::Condition(name, span) => {
                    Expr::Identifier(name.clone(), *span)
                }
                IntelFamilyOperand::Indirect(name, span) => {
                    Expr::Indirect(Box::new(Expr::Identifier(name.clone(), *span)), *span)
                }
                IntelFamilyOperand::Immediate(expr)
                | IntelFamilyOperand::RstVector(expr)
                | IntelFamilyOperand::InterruptMode(expr)
                | IntelFamilyOperand::BitNumber(expr)
                | IntelFamilyOperand::Port(expr) => expr.clone(),
                IntelFamilyOperand::Indexed { base, offset, span } => Expr::Indirect(
                    Box::new(Expr::Binary {
                        op: asm_parser::BinaryOp::Add,
                        left: Box::new(Expr::Identifier(base.clone(), *span)),
                        right: Box::new(offset.clone()),
                        span: *span,
                    }),
                    *span,
                ),
            };
            exprs.push(expr);
        }
        Some(exprs)
    }

    pub fn cond_last(&self) -> Option<&ConditionalContext> {
        self.cond_stack.last()
    }

    pub fn cond_skipping(&self) -> bool {
        self.cond_stack.skipping()
    }

    pub fn cond_is_empty(&self) -> bool {
        self.cond_stack.is_empty()
    }

    pub fn in_module(&self) -> bool {
        self.symbol_scope.module_active.is_some()
    }

    pub fn in_section(&self) -> bool {
        self.layout.current_section.is_some()
    }

    pub fn in_user_section(&self) -> bool {
        self.layout
            .current_section
            .as_deref()
            .is_some_and(|name| name != IMPLICIT_HUNK_CODE_SECTION_NAME)
    }

    fn in_struct_definition(&self) -> bool {
        self.active_struct.is_some()
    }

    pub fn open_struct_line(&self) -> Option<u32> {
        self.active_struct.as_ref().map(|state| state.open_line)
    }

    pub fn clear_struct_definition(&mut self) {
        self.active_struct = None;
    }

    fn value_symbol_key(name: &str) -> String {
        name.to_ascii_uppercase()
    }

    pub fn set_value_symbol(&mut self, name: &str, value: AsmValue) {
        self.clear_repeat_iteration_scopes(name);
        self.scalar_value_symbols
            .remove(&Self::value_symbol_key(name));
        self.value_symbols
            .insert(Self::value_symbol_key(name), value);
    }

    fn lookup_value_symbol(&self, name: &str) -> Option<&AsmValue> {
        self.value_symbols.get(&Self::value_symbol_key(name))
    }

    fn set_scalar_value_symbol(&mut self, name: &str) {
        self.value_symbols.remove(&Self::value_symbol_key(name));
        self.scalar_value_symbols
            .insert(Self::value_symbol_key(name));
        self.clear_repeat_iteration_scopes(name);
    }

    fn has_scalar_value_symbol(&self, name: &str) -> bool {
        self.scalar_value_symbols
            .contains(&Self::value_symbol_key(name))
    }

    pub fn set_repeat_iteration_scopes(&mut self, name: &str, scopes: Vec<String>) {
        if scopes.is_empty() {
            self.clear_repeat_iteration_scopes(name);
            return;
        }
        self.repeat_iteration_scopes
            .insert(Self::value_symbol_key(name), scopes);
    }

    fn clear_repeat_iteration_scopes(&mut self, name: &str) {
        self.repeat_iteration_scopes
            .remove(&Self::value_symbol_key(name));
    }

    fn lookup_repeat_iteration_scopes(&self, name: &str) -> Option<&[String]> {
        self.repeat_iteration_scopes
            .get(&Self::value_symbol_key(name))
            .map(Vec::as_slice)
    }

    fn scalar_shadow_for_value_symbol(value: &AsmValue) -> u32 {
        match value {
            AsmValue::Scalar(value) => *value as u32,
            AsmValue::Struct(def) => def.size,
            AsmValue::List(_) | AsmValue::Range { .. } | AsmValue::StructInstance(_) => 0,
        }
    }

    fn sync_value_symbol(&mut self, name: &str, value: &AsmValue) {
        match value {
            AsmValue::Scalar(_) => self.set_scalar_value_symbol(name),
            _ => self.set_value_symbol(name, value.clone()),
        }
    }

    fn assign_op_text(op: AssignOp) -> &'static str {
        match op {
            AssignOp::Const => "=",
            AssignOp::Var => ":=",
            AssignOp::VarIfUndef => ":?=",
            AssignOp::Add => "+=",
            AssignOp::Sub => "-=",
            AssignOp::Mul => "*=",
            AssignOp::Div => "/=",
            AssignOp::Mod => "%=",
            AssignOp::Pow => "^=",
            AssignOp::BitOr => "|=",
            AssignOp::BitXor => "^^=",
            AssignOp::BitAnd => "&=",
            AssignOp::LogicOr => "||=",
            AssignOp::LogicAnd => "&&=",
            AssignOp::Shl => "<<=",
            AssignOp::Shr => ">>=",
            AssignOp::Concat => "..=",
            AssignOp::Min => "<?=",
            AssignOp::Max => ">?=",
            AssignOp::Repeat => "x=",
            AssignOp::Member => ".=",
        }
    }

    fn resolve_scoped_value_name(&self, name: &str) -> Option<String> {
        if name.contains('.') {
            let candidate = self
                .resolve_qualified_imported_name(name)
                .ok()
                .flatten()
                .unwrap_or_else(|| name.to_string());
            if self.lookup_value_symbol(&candidate).is_some() {
                return Some(candidate);
            }
            return None;
        }

        let mut depth = self.symbol_scope.scope_stack.depth();
        while depth > 0 {
            let prefix = self.symbol_scope.scope_stack.prefix(depth);
            let candidate = format!("{prefix}.{name}");
            if self.lookup_value_symbol(&candidate).is_some() {
                return Some(candidate);
            }
            depth = depth.saturating_sub(1);
        }

        if self.lookup_value_symbol(name).is_some() {
            return Some(name.to_string());
        }

        let imported = self.resolve_imported_name(name)?;
        if self.lookup_value_symbol(&imported).is_some() {
            Some(imported)
        } else {
            None
        }
    }

    fn resolve_scoped_scalar_value_name(&self, name: &str) -> Option<String> {
        if name.contains('.') {
            let candidate = self
                .resolve_qualified_imported_name(name)
                .ok()
                .flatten()
                .unwrap_or_else(|| name.to_string());
            if self.has_scalar_value_symbol(&candidate) {
                return Some(candidate);
            }
            return None;
        }

        let mut depth = self.symbol_scope.scope_stack.depth();
        while depth > 0 {
            let prefix = self.symbol_scope.scope_stack.prefix(depth);
            let candidate = format!("{prefix}.{name}");
            if self.has_scalar_value_symbol(&candidate) {
                return Some(candidate);
            }
            depth = depth.saturating_sub(1);
        }

        if self.has_scalar_value_symbol(name) {
            return Some(name.to_string());
        }

        let imported = self.resolve_imported_name(name)?;
        if self.has_scalar_value_symbol(&imported) {
            Some(imported)
        } else {
            None
        }
    }

    pub fn push_loop_var(&mut self, name: &str, value: u32) {
        self.loop_vars.push((name.to_string(), value));
    }

    pub fn pop_loop_var(&mut self) {
        let _ = self.loop_vars.pop();
    }

    fn lookup_loop_var(&self, name: &str) -> Option<u32> {
        self.loop_vars
            .iter()
            .rev()
            .find(|(candidate, _)| candidate.eq_ignore_ascii_case(name))
            .map(|(_, value)| *value)
    }

    pub fn current_section_name(&self) -> Option<&str> {
        self.layout.current_section.as_deref()
    }

    fn mark_current_section_not_relocation_free(&mut self) {
        let Some(section_name) = self.layout.current_section.clone() else {
            return;
        };
        let Some(section) = self.layout.sections.get_mut(&section_name) else {
            return;
        };
        section.relocation_free_certified = false;
        section.hunk_relocation_compatible = false;
    }

    fn mark_current_section_hunk_relocatable(&mut self) {
        let Some(section_name) = self.layout.current_section.clone() else {
            return;
        };
        let Some(section) = self.layout.sections.get_mut(&section_name) else {
            return;
        };
        section.relocation_free_certified = false;
    }

    fn mark_current_section_hunk_fixup_error(&mut self, message: &str) {
        let Some(section_name) = self.layout.current_section.clone() else {
            return;
        };
        let Some(section) = self.layout.sections.get_mut(&section_name) else {
            return;
        };
        section.relocation_free_certified = false;
        section.hunk_relocation_compatible = false;
        if section.hunk_fixup_error.is_none() {
            section.hunk_fixup_error = Some(message.to_string());
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn hunk_abs32_output_fixup(
        &self,
        offset: u32,
        encoded_addend: u32,
        target_section: String,
    ) -> Option<OutputFixupRecord> {
        let source_section = self.layout.current_section.clone()?;
        Some(OutputFixupRecord::hunk_abs32(
            source_section,
            offset,
            encoded_addend,
            target_section,
        ))
    }

    #[cfg(feature = "vm-runtime-only")]
    fn hunk_abs32_output_fixup(
        &self,
        _offset: u32,
        _encoded_addend: u32,
        _target_section: String,
    ) -> Option<OutputFixupRecord> {
        None
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn hunk_data_expression_requires_unsupported_fixup(&self, expr: &Expr) -> bool {
        !self.expr_is_relocation_free_symbolic_value(expr, false)
    }

    #[cfg(feature = "vm-runtime-only")]
    fn hunk_data_expression_requires_unsupported_fixup(&self, _expr: &Expr) -> bool {
        false
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    pub(crate) fn family_operands_keep_current_section_relocation_free(
        &self,
        family_id: CpuFamily,
        operands: &dyn FamilyOperandSet,
    ) -> bool {
        if family_id != M68K_FAMILY_ID {
            return false;
        }
        let Some(m68k_operands) = operands.as_any().downcast_ref::<M68KFamilyOperands>() else {
            return false;
        };
        m68k_operands
            .0
            .iter()
            .all(|operand| self.m68k_operand_keeps_current_section_relocation_free(operand))
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_operand_keeps_current_section_relocation_free(
        &self,
        operand: &M68KFamilyOperand,
    ) -> bool {
        match operand {
            M68KFamilyOperand::DataRegister { .. }
            | M68KFamilyOperand::AddressRegister { .. }
            | M68KFamilyOperand::SpecialRegister { .. }
            | M68KFamilyOperand::ControlRegister { .. }
            | M68KFamilyOperand::FpuDataRegister { .. }
            | M68KFamilyOperand::FpuControlRegister { .. }
            | M68KFamilyOperand::AddressIndirect { .. }
            | M68KFamilyOperand::AddressPostincrement { .. }
            | M68KFamilyOperand::AddressPredecrement { .. }
            | M68KFamilyOperand::RegisterPair { .. }
            | M68KFamilyOperand::RegisterGroup { .. }
            | M68KFamilyOperand::IndirectRegisterPair { .. }
            | M68KFamilyOperand::RegisterList { .. } => true,
            M68KFamilyOperand::AddressDisplacement { displacement, .. }
            | M68KFamilyOperand::AddressIndexed { displacement, .. }
            | M68KFamilyOperand::TextureOperand {
                expr: displacement, ..
            }
            | M68KFamilyOperand::Absolute {
                expr: displacement, ..
            }
            | M68KFamilyOperand::Immediate {
                expr: displacement, ..
            } => self.expr_is_relocation_free_symbolic_value(displacement, false),
            M68KFamilyOperand::PcDisplacement { displacement, .. }
            | M68KFamilyOperand::PcIndexed { displacement, .. }
            | M68KFamilyOperand::BranchTarget {
                expr: displacement, ..
            } => self.expr_is_relocation_free_symbolic_value(displacement, true),
            M68KFamilyOperand::FullExtension {
                base_displacement,
                outer_displacement,
                ..
            } => {
                let base_ok = base_displacement.as_ref().is_none_or(|(expr, _)| {
                    self.expr_is_relocation_free_symbolic_value(expr, false)
                });
                let outer_ok = outer_displacement.as_ref().is_none_or(|(expr, _)| {
                    self.expr_is_relocation_free_symbolic_value(expr, false)
                });
                base_ok && outer_ok
            }
            M68KFamilyOperand::BitField {
                base,
                offset,
                width,
                ..
            } => {
                let selector_ok =
                    |selector: &families::m68k::operand::BitFieldSelector| match selector {
                        families::m68k::operand::BitFieldSelector::DataRegister { .. } => true,
                        families::m68k::operand::BitFieldSelector::Immediate { expr, .. } => {
                            self.expr_is_relocation_free_symbolic_value(expr, false)
                        }
                    };
                self.m68k_operand_keeps_current_section_relocation_free(base)
                    && selector_ok(offset)
                    && selector_ok(width)
            }
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn expr_is_relocation_free_symbolic_value(
        &self,
        expr: &Expr,
        allow_current_section_symbols: bool,
    ) -> bool {
        match expr {
            Expr::Number(_, _) | Expr::String(_, _) => true,
            Expr::Indirect(inner, _)
            | Expr::IndirectLong(inner, _)
            | Expr::Immediate(inner, _)
            | Expr::Unary { expr: inner, .. } => {
                self.expr_is_relocation_free_symbolic_value(inner, allow_current_section_symbols)
            }
            Expr::List(items, _) | Expr::Tuple(items, _) => items.iter().all(|item| {
                self.expr_is_relocation_free_symbolic_value(item, allow_current_section_symbols)
            }),
            Expr::StructLiteral { fields, .. } => fields.iter().all(|(_, value)| {
                self.expr_is_relocation_free_symbolic_value(value, allow_current_section_symbols)
            }),
            Expr::Binary { left, right, .. } => {
                self.expr_is_relocation_free_symbolic_value(left, allow_current_section_symbols)
                    && self.expr_is_relocation_free_symbolic_value(
                        right,
                        allow_current_section_symbols,
                    )
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => {
                self.expr_is_relocation_free_symbolic_value(cond, allow_current_section_symbols)
                    && self.expr_is_relocation_free_symbolic_value(
                        then_expr,
                        allow_current_section_symbols,
                    )
                    && self.expr_is_relocation_free_symbolic_value(
                        else_expr,
                        allow_current_section_symbols,
                    )
            }
            Expr::Range {
                start, end, step, ..
            } => {
                self.expr_is_relocation_free_symbolic_value(start, allow_current_section_symbols)
                    && self
                        .expr_is_relocation_free_symbolic_value(end, allow_current_section_symbols)
                    && step.as_ref().is_none_or(|step_expr| {
                        self.expr_is_relocation_free_symbolic_value(
                            step_expr,
                            allow_current_section_symbols,
                        )
                    })
            }
            Expr::Identifier(name, _) => {
                let Some(resolved_name) = self.resolve_symbol_name_for_relocation(name) else {
                    return false;
                };
                match self.resolved_symbol_section_name(&resolved_name) {
                    Some(section_name) => {
                        allow_current_section_symbols
                            && self
                                .current_section_name()
                                .is_some_and(|current| current.eq_ignore_ascii_case(&section_name))
                    }
                    None => {
                        self.symbols.entry(&resolved_name).is_some()
                            || allow_current_section_symbols
                    }
                }
            }
            Expr::Error(_, _)
            | Expr::Placeholder(_)
            | Expr::Dollar(_)
            | Expr::Register(_, _)
            | Expr::Index { .. }
            | Expr::Member { .. }
            | Expr::Call { .. } => false,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn resolve_symbol_name_for_relocation(&self, name: &str) -> Option<String> {
        match self.resolve_scoped_name(name) {
            Ok(Some(resolved)) => Some(resolved),
            Ok(None) => self
                .symbols
                .entry(name)
                .map(|entry| entry.name.clone())
                .or_else(|| Some(name.to_string())),
            Err(_) => self.symbols.entry(name).map(|entry| entry.name.clone()),
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn resolved_symbol_section_name(&self, resolved_name: &str) -> Option<String> {
        if let Some(section_name) = self.layout.section_symbol_sections.get(resolved_name) {
            return Some(section_name.clone());
        }
        let entry = self.symbols.entry(resolved_name)?;
        for (section_name, section) in &self.layout.sections {
            let Some(base_addr) = section.base_addr else {
                continue;
            };
            let Some(end_addr) = base_addr.checked_add(section.max_pc) else {
                continue;
            };
            if entry.val >= base_addr && entry.val < end_addr {
                return Some(section_name.clone());
            }
        }
        None
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn hunk_abs32_target_section_for_expr(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(name, _) => {
                let resolved_name = self.resolve_symbol_name_for_relocation(name)?;
                if self
                    .layout
                    .absolute_constant_symbols
                    .contains(&resolved_name)
                {
                    return None;
                }
                self.resolved_symbol_section_name(&resolved_name)
            }
            Expr::Unary {
                op: UnaryOp::Plus,
                expr: inner,
                ..
            }
            | Expr::Immediate(inner, _)
            | Expr::Indirect(inner, _)
            | Expr::IndirectLong(inner, _) => self.hunk_abs32_target_section_for_expr(inner),
            Expr::Binary {
                op: BinaryOp::Add,
                left,
                right,
                ..
            } => {
                if Self::expr_is_relocation_free_literal(left) {
                    self.hunk_abs32_target_section_for_expr(right)
                } else if Self::expr_is_relocation_free_literal(right) {
                    self.hunk_abs32_target_section_for_expr(left)
                } else {
                    None
                }
            }
            Expr::Binary {
                op: BinaryOp::Subtract,
                left,
                right,
                ..
            } => {
                if Self::expr_is_relocation_free_literal(right) {
                    self.hunk_abs32_target_section_for_expr(left)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn eval_hunk_abs32_relocation_value(
        &self,
        expr: &Expr,
    ) -> Result<Option<(u32, String)>, AstEvalError> {
        let Some(target_section) = self.hunk_abs32_target_section_for_expr(expr) else {
            return Ok(None);
        };
        let value = self.eval_expr_for_data_directive(expr)?;
        let Some(section) = self.layout.sections.get(&target_section) else {
            return Ok(None);
        };
        let base_addr = section.base_addr.unwrap_or(0);
        let adjusted = value.checked_sub(base_addr).ok_or_else(|| {
            AstEvalError::directive(
                "section relocation value underflows the target section base",
                expr_span(expr),
            )
        })?;
        Ok(Some((adjusted, target_section)))
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn hunk_abs32_target_section_for_data_expr(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(name, _) => {
                let resolved_name = self.resolve_symbol_name_for_relocation(name)?;
                if self
                    .layout
                    .absolute_constant_symbols
                    .contains(&resolved_name)
                {
                    return None;
                }
                self.resolved_symbol_section_name(&resolved_name)
            }
            Expr::Unary {
                op: UnaryOp::Plus,
                expr: inner,
                ..
            }
            | Expr::Immediate(inner, _)
            | Expr::Indirect(inner, _)
            | Expr::IndirectLong(inner, _) => self.hunk_abs32_target_section_for_data_expr(inner),
            Expr::Binary {
                op: BinaryOp::Add,
                left,
                right,
                ..
            } => {
                if self.expr_is_absolute_constant_symbol_expr(left) {
                    self.hunk_abs32_target_section_for_data_expr(right)
                } else if self.expr_is_absolute_constant_symbol_expr(right) {
                    self.hunk_abs32_target_section_for_data_expr(left)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn eval_hunk_abs32_data_relocation_value(
        &self,
        expr: &Expr,
    ) -> Result<Option<(u32, String)>, AstEvalError> {
        let Some(target_section) = self.hunk_abs32_target_section_for_data_expr(expr) else {
            return Ok(None);
        };
        let value = self.eval_expr_for_data_directive(expr)?;
        let Some(section) = self.layout.sections.get(&target_section) else {
            return Ok(None);
        };
        let base_addr = section.base_addr.unwrap_or(0);
        let adjusted = value.checked_sub(base_addr).ok_or_else(|| {
            AstEvalError::directive(
                "section relocation value underflows the target section base",
                expr_span(expr),
            )
        })?;
        Ok(Some((adjusted, target_section)))
    }

    #[cfg(feature = "vm-runtime-only")]
    fn eval_hunk_abs32_data_relocation_value(
        &self,
        _expr: &Expr,
    ) -> Result<Option<(u32, String)>, AstEvalError> {
        Ok(None)
    }

    #[cfg(feature = "vm-runtime-only")]
    fn eval_hunk_abs32_relocation_value(
        &self,
        _expr: &Expr,
    ) -> Result<Option<(u32, String)>, AstEvalError> {
        Ok(None)
    }

    #[allow(clippy::result_unit_err)]
    pub fn current_addr(&mut self, main_addr: u32) -> Result<u32, ()> {
        match self.layout.current_section.as_deref() {
            Some(name) => {
                let Some(section) = self.layout.sections.get(name) else {
                    return Ok(main_addr);
                };
                let max = self.max_program_address();
                let cpu_name = self.cpu.as_str().to_string();
                let label = format!("section {name} absolute address");
                match Self::checked_add_address(
                    section.start_pc,
                    section.pc,
                    max,
                    cpu_name.as_str(),
                    label.as_str(),
                ) {
                    Ok(addr) => Ok(addr),
                    Err(message) => {
                        self.diagnostics.last_error =
                            Some(AsmError::new(AsmErrorKind::Directive, &message, None));
                        self.diagnostics.last_error_column = None;
                        Err(())
                    }
                }
            }
            None => Ok(main_addr),
        }
    }

    fn track_section_symbol(&mut self, full_name: &str) {
        if let Some(section_name) = self.layout.current_section.as_ref() {
            self.layout
                .section_symbol_sections
                .insert(full_name.to_string(), section_name.clone());
        }
    }

    #[allow(clippy::result_unit_err)]
    pub fn update_addresses(&mut self, main_addr: &mut u32, status: LineStatus) -> Result<(), ()> {
        let num_bytes = match u32::try_from(self.num_bytes()) {
            Ok(num_bytes) => num_bytes,
            Err(_) => {
                let message = format!(
                    "line byte count exceeds supported range for CPU {}",
                    self.cpu.as_str()
                );
                return self.fail_address_update(message);
            }
        };
        let max = self.max_program_address();
        let cpu_name = self.cpu.as_str().to_string();
        if let Some(section_name) = self.layout.current_section.clone() {
            let update_result: Result<(), String> = (|| {
                let Some(section) = self.layout.sections.get_mut(&section_name) else {
                    return Ok(());
                };
                let current_abs = Self::checked_add_address(
                    section.start_pc,
                    section.pc,
                    max,
                    cpu_name.as_str(),
                    &format!("section {section_name} absolute address"),
                )?;
                if self.pass == 2 {
                    if status == LineStatus::DirDs && self.aux_value > 0 && !section.is_bss() {
                        section
                            .bytes
                            .extend(std::iter::repeat_n(0, self.aux_value as usize));
                    } else if status == LineStatus::DirEqu
                        && self.start_addr > current_abs
                        && !section.is_bss()
                    {
                        let pad = self.start_addr - current_abs;
                        section.bytes.extend(std::iter::repeat_n(0, pad as usize));
                    } else if !self.bytes.is_empty() && !section.is_bss() {
                        let base_offset = u32::try_from(section.bytes.len()).map_err(|_| {
                            format!(
                                "section {section_name} relocation base exceeds supported range"
                            )
                        })?;
                        for fixup in &self.pending_output_fixups {
                            let Some(offset) = base_offset.checked_add(fixup.offset) else {
                                return Err(format!(
                                    "section {section_name} relocation offset exceeds supported range"
                                ));
                            };
                            let mut fixup = fixup.clone();
                            fixup.offset = offset;
                            section.output_fixups.push(fixup);
                        }
                        section.bytes.extend_from_slice(&self.bytes);
                    }
                }
                section.pc = if status == LineStatus::DirDs {
                    let current_abs = Self::checked_add_address(
                        section.pc,
                        self.aux_value,
                        max,
                        cpu_name.as_str(),
                        &format!("section {section_name} program counter"),
                    )?;
                    current_abs
                } else if status == LineStatus::DirEqu {
                    Self::checked_sub_address(
                        self.start_addr,
                        section.start_pc,
                        max,
                        cpu_name.as_str(),
                        &format!("section {section_name} program counter"),
                    )?
                } else {
                    Self::checked_add_address(
                        section.pc,
                        num_bytes,
                        max,
                        cpu_name.as_str(),
                        &format!("section {section_name} program counter"),
                    )?
                };
                let _ = Self::checked_add_address(
                    section.start_pc,
                    section.pc,
                    max,
                    cpu_name.as_str(),
                    &format!("section {section_name} absolute address"),
                )?;
                section.max_pc = section.max_pc.max(section.pc);
                Ok(())
            })();
            if let Err(message) = update_result {
                return self.fail_address_update(message);
            }
        } else if status == LineStatus::DirDs {
            match Self::checked_add_address(
                *main_addr,
                self.aux_value,
                max,
                cpu_name.as_str(),
                "program counter",
            ) {
                Ok(addr) => *main_addr = addr,
                Err(message) => return self.fail_address_update(message),
            }
        } else if status == LineStatus::DirEqu {
            if self.start_addr > max {
                let message = format!(
                    "program counter ${} exceeds max ${} for CPU {}",
                    format_addr(self.start_addr),
                    format_addr(max),
                    cpu_name
                );
                return self.fail_address_update(message);
            }
            *main_addr = self.start_addr;
        } else {
            match Self::checked_add_address(
                *main_addr,
                num_bytes,
                max,
                cpu_name.as_str(),
                "program counter",
            ) {
                Ok(addr) => *main_addr = addr,
                Err(message) => return self.fail_address_update(message),
            }
        }
        Ok(())
    }

    fn checked_add_address(
        start: u32,
        delta: u32,
        max: u32,
        cpu_name: &str,
        label: &str,
    ) -> Result<u32, String> {
        let value = start
            .checked_add(delta)
            .ok_or_else(|| format!("{label} overflows address arithmetic for CPU {cpu_name}"))?;
        if value > max {
            return Err(format!(
                "{label} ${} exceeds max ${} for CPU {}",
                format_addr(value),
                format_addr(max),
                cpu_name
            ));
        }
        Ok(value)
    }

    fn checked_sub_address(
        value: u32,
        subtrahend: u32,
        max: u32,
        cpu_name: &str,
        label: &str,
    ) -> Result<u32, String> {
        let result = value
            .checked_sub(subtrahend)
            .ok_or_else(|| format!("{label} underflows address arithmetic for CPU {cpu_name}"))?;
        if result > max {
            return Err(format!(
                "{label} ${} exceeds max ${} for CPU {}",
                format_addr(result),
                format_addr(max),
                cpu_name
            ));
        }
        Ok(result)
    }

    fn fail_address_update(&mut self, message: String) -> Result<(), ()> {
        self.diagnostics.last_error = Some(AsmError::new(AsmErrorKind::Directive, &message, None));
        self.diagnostics.last_error_column = self.line_end_span.map(|span| span.col_start);
        Err(())
    }

    fn is_allowed_meta_directive(&self, mnemonic: &str) -> bool {
        if self.output_state.in_output_block {
            return is_output_block_directive(mnemonic)
                || self.is_output_cpu_block_directive(mnemonic);
        }
        is_meta_block_directive(mnemonic)
    }

    fn is_output_cpu_block_directive(&self, mnemonic: &str) -> bool {
        let upper = mnemonic.to_ascii_uppercase();
        if let Some(name) = upper.strip_prefix(".END") {
            return self.registry.resolve_cpu_name(name).is_some();
        }
        if let Some(name) = upper.strip_prefix('.') {
            return self.registry.resolve_cpu_name(name).is_some();
        }
        false
    }

    fn current_visibility(&self) -> SymbolVisibility {
        self.symbol_scope
            .visibility_stack
            .last()
            .copied()
            .unwrap_or(SymbolVisibility::Private)
    }

    pub fn push_visibility(&mut self) {
        let current = self.current_visibility();
        self.symbol_scope.visibility_stack.push(current);
    }

    pub fn pop_visibility(&mut self) -> bool {
        if self.symbol_scope.visibility_stack.len() > 1 {
            self.symbol_scope.visibility_stack.pop();
            true
        } else {
            false
        }
    }

    fn set_visibility(&mut self, visibility: SymbolVisibility) {
        if let Some(current) = self.symbol_scope.visibility_stack.last_mut() {
            *current = visibility;
        } else {
            self.symbol_scope.visibility_stack.push(visibility);
        }
    }

    fn ast_is_toplevel_directive(ast: &LineAst) -> bool {
        match ast {
            LineAst::Statement(statement) => match statement.mnemonic.as_deref() {
                Some(mnemonic) => is_toplevel_directive(mnemonic),
                None => false,
            },
            _ => false,
        }
    }

    pub fn symbols(&self) -> &SymbolTable {
        &*self.symbols
    }

    pub fn scoped_define_name(&self, name: &str) -> String {
        if name.contains('.') {
            name.to_string()
        } else {
            self.symbol_scope.scope_stack.qualify(name)
        }
    }

    fn resolve_imported_name(&self, name: &str) -> Option<String> {
        let module_id = self.symbol_scope.module_active.as_deref()?;
        match self.symbols.resolve_imported_symbol(module_id, name) {
            ImportedSymbolResolution::Resolved { full_name, .. } => Some(full_name),
            ImportedSymbolResolution::Unresolved | ImportedSymbolResolution::Ambiguous => None,
        }
    }

    fn resolve_qualified_imported_name(&self, name: &str) -> Result<Option<String>, AsmError> {
        let Some(module_id) = self.symbol_scope.module_active.as_deref() else {
            return Ok(None);
        };
        match self.symbols.resolve_imported_symbol(module_id, name) {
            ImportedSymbolResolution::Resolved { full_name, .. } => Ok(Some(full_name)),
            ImportedSymbolResolution::Unresolved => Ok(None),
            ImportedSymbolResolution::Ambiguous => Err(AsmError::new(
                AsmErrorKind::Symbol,
                "Ambiguous imported module path",
                Some(name),
            )),
        }
    }

    fn selective_import_conflict(&self, name: &str) -> bool {
        if name.contains('.') {
            return false;
        }
        let module_id = match self.symbol_scope.module_active.as_deref() {
            Some(module_id) => module_id,
            None => return false,
        };
        if self.symbol_scope.scope_stack.depth() != self.symbol_scope.module_scope_depth {
            return false;
        }
        self.symbols
            .resolve_selective_import(module_id, name)
            .is_some()
    }

    fn defer_outer_lookup_for_active_block_pass1(&self) -> bool {
        self.pass == 1
            && self
                .symbol_scope
                .scope_stack
                .has_block_deeper_than(self.symbol_scope.module_scope_depth)
    }

    fn resolve_scoped_name(&self, name: &str) -> Result<Option<String>, AsmError> {
        if name.contains('.') {
            let candidate = self
                .resolve_qualified_imported_name(name)?
                .unwrap_or_else(|| name.to_string());
            if let Some(entry) = self.symbols.entry(&candidate) {
                if !self.entry_is_visible(entry) {
                    return Err(self.visibility_error(name));
                }
                return Ok(Some(entry.name.clone()));
            }
            return Ok(None);
        }
        let block_local_only = self.defer_outer_lookup_for_active_block_pass1();
        let stop_depth = if block_local_only {
            self.symbol_scope.module_scope_depth
        } else {
            0
        };
        let mut depth = self.symbol_scope.scope_stack.depth();
        while depth > stop_depth {
            let prefix = self.symbol_scope.scope_stack.prefix(depth);
            let candidate = format!("{prefix}.{name}");
            if let Some(entry) = self.symbols.entry(&candidate) {
                if !self.entry_is_visible(entry) {
                    return Err(self.visibility_error(name));
                }
                return Ok(Some(entry.name.clone()));
            }
            depth = depth.saturating_sub(1);
        }
        if block_local_only {
            return Ok(None);
        }
        if let Some(entry) = self.symbols.entry(name) {
            if !self.entry_is_visible(entry) {
                return Err(self.visibility_error(name));
            }
            Ok(Some(entry.name.clone()))
        } else if let Some(imported) = self.resolve_imported_name(name) {
            if let Some(entry) = self.symbols.entry(&imported) {
                if !self.entry_is_visible(entry) {
                    return Err(self.visibility_error(name));
                }
                Ok(Some(entry.name.clone()))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn lookup_scoped_entry(&self, name: &str) -> Option<&SymbolTableEntry> {
        if name.contains('.') {
            let candidate = self
                .resolve_qualified_imported_name(name)
                .ok()
                .flatten()
                .unwrap_or_else(|| name.to_string());
            return self.symbols.entry(&candidate);
        }
        let block_local_only = self.defer_outer_lookup_for_active_block_pass1();
        let stop_depth = if block_local_only {
            self.symbol_scope.module_scope_depth
        } else {
            0
        };
        let mut depth = self.symbol_scope.scope_stack.depth();
        while depth > stop_depth {
            let prefix = self.symbol_scope.scope_stack.prefix(depth);
            let candidate = format!("{prefix}.{name}");
            if let Some(entry) = self.symbols.entry(&candidate) {
                return Some(entry);
            }
            depth = depth.saturating_sub(1);
        }
        if block_local_only {
            return None;
        }
        if let Some(entry) = self.symbols.entry(name) {
            return Some(entry);
        }
        if let Some(imported) = self.resolve_imported_name(name) {
            return self.symbols.entry(&imported);
        }
        None
    }

    fn entry_is_visible(&self, entry: &SymbolTableEntry) -> bool {
        match entry.visibility {
            SymbolVisibility::Public => true,
            SymbolVisibility::Private => match (&entry.module_id, &self.symbol_scope.module_active)
            {
                (Some(entry_module), Some(current_module)) => {
                    entry_module.eq_ignore_ascii_case(current_module)
                }
                (Some(_), None) => false,
                (None, _) => true,
            },
        }
    }

    fn visibility_error(&self, name: &str) -> AsmError {
        AsmError::new(AsmErrorKind::Symbol, "Symbol is private", Some(name))
    }
    fn process_with_runtime_tokenizer(&mut self, line: &str, line_num: u32) -> LineStatus {
        let model = match self.opthread_execution_model.as_ref() {
            Some(model) => model,
            None => {
                let family_id = Self::resolve_pipeline_for_cpu(self.registry, self.cpu)
                    .map(|pipeline| pipeline.family_id.as_str().to_string())
                    .unwrap_or_else(|_| self.cpu.as_str().to_string());
                let err = ParseError {
                    message: format!(
                        "VM runtime tokenizer model unavailable for family '{}'",
                        family_id
                    ),
                    span: Span {
                        line: line_num,
                        col_start: 1,
                        col_end: 1,
                    },
                };
                self.diagnostics.last_error =
                    Some(AsmError::new(AsmErrorKind::Parser, &err.message, None));
                self.diagnostics.last_error_column = Some(err.span.col_start);
                self.diagnostics.last_parser_error = Some(err);
                self.record_default_processing_trace(line_num);
                return LineStatus::Error;
            }
        };

        let parsed_line = if let Some(router) = &self.runtime_line_router {
            router.parse_line(
                model,
                self.cpu.as_str(),
                line,
                line_num,
                &self.register_checker,
            )
        } else {
            opasm::process_statement(
                StatementRequest::new(line, line_num)
                    .with_execution_mode(ExecutionMode::Vm)
                    .with_model(model, self.cpu.as_str(), None)
                    .with_expr_parser_rollout_overrides(
                        &self.opthread_expr_parser_opt_in_families,
                        &self.opthread_expr_parser_force_host_families,
                    )
                    .with_register_checker(&self.register_checker),
                None,
            )
            .map(|result| {
                (
                    result.parsed.ast,
                    result.parsed.end_span,
                    result.parsed.end_token_text,
                    Some(result.trace),
                    Some(result.lockstep_report),
                )
            })
        };

        let (ast, end_span, end_token_text, processing_trace, lockstep_report) = match parsed_line {
            Ok(parsed) => parsed,
            Err(err) => {
                self.line_end_span = Some(err.span);
                self.diagnostics.last_error =
                    Some(AsmError::new(AsmErrorKind::Parser, &err.message, None));
                self.diagnostics.last_error_column = Some(err.span.col_start);
                self.diagnostics.last_parser_error = Some(err);
                self.record_default_processing_trace(line_num);
                #[cfg(not(feature = "vm-runtime-only"))]
                self.attach_dialect_fixit_hint_from_source_line();
                return LineStatus::Error;
            }
        };

        if let Some(trace) = processing_trace {
            self.runtime_processing_traces.push((line_num, trace));
        }
        if let Some(report) = lockstep_report {
            self.runtime_lockstep_report.extend(report);
        }
        self.line_end_span = Some(end_span);
        self.line_end_token = end_token_text;
        self.process_ast(ast)
    }

    pub fn process(&mut self, line: &str, line_num: u32, addr: u32, pass: u8) -> LineStatus {
        self.diagnostics.last_error = None;
        self.diagnostics.last_error_column = None;
        self.diagnostics.last_error_help = None;
        self.diagnostics.last_error_fixits.clear();
        self.diagnostics.last_parser_error = None;
        self.current_line_num = line_num;
        self.current_source_line = Some(line.to_string());
        self.line_end_span = None;
        self.line_end_token = None;
        self.start_addr = addr;
        self.pass = pass;
        self.bytes.clear();
        self.pending_output_fixups.clear();
        self.aux_value = 0;

        self.label = None;
        self.mnemonic = None;

        self.process_with_runtime_tokenizer(line, line_num)
    }
    fn process_ast(&mut self, ast: LineAst) -> LineStatus {
        if self.statement_depth > 0 {
            return match ast {
                LineAst::StatementEnd(..) => {
                    self.statement_depth = self.statement_depth.saturating_sub(1);
                    LineStatus::Skip
                }
                LineAst::StatementDef(def) => self.failure_at_span(
                    LineStatus::Error,
                    AsmErrorKind::Parser,
                    "Nested .statement definitions are not supported",
                    None,
                    def.span,
                ),
                _ => LineStatus::Skip,
            };
        }

        if !self.in_module() {
            if self.symbol_scope.saw_explicit_module {
                if !matches!(ast, LineAst::Empty) && !Self::ast_is_toplevel_directive(&ast) {
                    return self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Directive,
                        "Top-level content must be inside a .module block",
                        None,
                    );
                }
            } else if !matches!(ast, LineAst::Empty) && !Self::ast_is_toplevel_directive(&ast) {
                self.symbol_scope.top_level_content_seen = true;
            }
        }

        if self.output_state.in_meta_block && !self.cond_stack.skipping() {
            match &ast {
                LineAst::Empty | LineAst::Conditional(..) => {}
                LineAst::Statement(statement) => {
                    let label = &statement.label;
                    let mnemonic = &statement.mnemonic;
                    if label.is_some() {
                        return self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Directive,
                            "Labels are not allowed inside a .meta block",
                            None,
                        );
                    }
                    match mnemonic.as_deref() {
                        Some(name) if self.is_allowed_meta_directive(name) => {}
                        Some(_) | None => {
                            return self.failure(
                                LineStatus::Error,
                                AsmErrorKind::Directive,
                                "Only metadata directives are allowed inside a .meta block",
                                None,
                            );
                        }
                    }
                }
                _ => {
                    return self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Directive,
                        "Only metadata directives are allowed inside a .meta block",
                        None,
                    );
                }
            }
        }
        if self.in_struct_definition() {
            return match ast {
                LineAst::Empty => LineStatus::NothingDone,
                LineAst::Statement(statement) => {
                    let label = statement.label;
                    let mnemonic = statement.mnemonic;
                    let operands = statement.operands;
                    self.label = label.as_ref().map(|l| l.name.clone());
                    self.mnemonic = mnemonic.clone();
                    self.process_struct_mode_statement_ast(
                        label.as_ref(),
                        mnemonic.as_deref(),
                        &operands,
                    )
                }
                _ => self.failure(
                    LineStatus::Error,
                    AsmErrorKind::Directive,
                    "invalid field directive in struct body",
                    None,
                ),
            };
        }
        match ast {
            LineAst::Empty => LineStatus::NothingDone,
            LineAst::Conditional(conditional) => {
                self.process_conditional_ast(conditional.kind, &conditional.exprs, conditional.span)
            }
            LineAst::Use(use_ast) => {
                if self.cond_stack.skipping() {
                    return LineStatus::Skip;
                }
                if !self.in_module() {
                    return self.failure_at_span(
                        LineStatus::Error,
                        AsmErrorKind::Directive,
                        ".use must appear inside a module",
                        None,
                        use_ast.span,
                    );
                }
                if self.symbol_scope.scope_stack.depth() != self.symbol_scope.module_scope_depth {
                    return self.failure_at_span(
                        LineStatus::Error,
                        AsmErrorKind::Directive,
                        ".use must appear at module scope",
                        None,
                        use_ast.span,
                    );
                }
                if self.pass == 1 {
                    let import = module_import_from_parser(
                        use_ast.module_id,
                        use_ast.alias,
                        use_ast.items,
                        use_ast.params,
                        use_ast.section_maps,
                        use_ast.span,
                    );
                    let module_name = self
                        .symbol_scope
                        .module_active
                        .as_deref()
                        .expect("module active");
                    match self.symbols.add_import(module_name, import) {
                        ImportResult::Ok => LineStatus::Ok,
                        ImportResult::AliasCollision => self.failure_at_span(
                            LineStatus::Error,
                            AsmErrorKind::Directive,
                            "Import alias already in use",
                            None,
                            use_ast.span,
                        ),
                        ImportResult::SelectiveCollision => self.failure_at_span(
                            LineStatus::Error,
                            AsmErrorKind::Directive,
                            "Selective import name already in use",
                            None,
                            use_ast.span,
                        ),
                    }
                } else {
                    LineStatus::Ok
                }
            }
            LineAst::Place(place) => {
                if self.cond_stack.skipping() {
                    return LineStatus::Skip;
                }
                self.process_place_ast(
                    &place.section,
                    &place.region,
                    place.align.as_ref(),
                    place.span,
                )
            }
            LineAst::Pack(pack) => {
                if self.cond_stack.skipping() {
                    return LineStatus::Skip;
                }
                self.process_pack_ast(&pack.region, &pack.sections, pack.span)
            }
            LineAst::StatementDef(..) => {
                if self.cond_stack.skipping() {
                    return LineStatus::Skip;
                }
                self.statement_depth = self.statement_depth.saturating_add(1);
                LineStatus::Skip
            }
            LineAst::StatementEnd(end) => {
                if self.cond_stack.skipping() {
                    return LineStatus::Skip;
                }
                self.failure_at_span(
                    LineStatus::Error,
                    AsmErrorKind::Parser,
                    "Found .endstatement without matching .statement",
                    None,
                    end.span,
                )
            }
            LineAst::Assignment(assignment) => {
                if self.cond_stack.skipping() {
                    return LineStatus::Skip;
                }
                self.process_assignment_ast(
                    &assignment.label,
                    assignment.op,
                    &assignment.expr,
                    assignment.span,
                )
            }
            LineAst::Statement(statement) => {
                let label = statement.label;
                let mnemonic = statement.mnemonic;
                let operands = statement.operands;
                self.label = label.as_ref().map(|l| l.name.clone());
                self.mnemonic = mnemonic.clone();

                if self.cond_stack.skipping() {
                    if let Some(name) = mnemonic.as_deref() {
                        if is_scope_directive(name) {
                            return self.process_directive_ast(name, &operands);
                        }
                    }
                    return LineStatus::Skip;
                }

                let mnemonic = match mnemonic {
                    Some(m) => m,
                    None => {
                        if let Some(label) = &label {
                            if let Some(status) = self.define_statement_label(label) {
                                return status;
                            }
                        }
                        return LineStatus::NothingDone;
                    }
                };

                if let Some(label) = &label {
                    if !is_symbol_assignment_directive(&mnemonic)
                        && !directive_handles_label_lifecycle(&mnemonic)
                    {
                        if let Some(status) = self.define_statement_label(label) {
                            return status;
                        }
                    }
                }

                let mut status = self.process_directive_ast(&mnemonic, &operands);
                if status == LineStatus::NothingDone {
                    if mnemonic.starts_with('.') {
                        if let Some(status_with_fixit) =
                            self.failure_for_unknown_directive_with_fixit(&mnemonic)
                        {
                            return status_with_fixit;
                        }
                    }
                    status = self.process_instruction_ast(&mnemonic, &operands);
                }
                status
            }
        }
    }

    fn define_statement_label(&mut self, label: &Label) -> Option<LineStatus> {
        if self.pass == 1 && self.selective_import_conflict(&label.name) {
            return Some(self.failure_at_span(
                LineStatus::Error,
                AsmErrorKind::Symbol,
                "Symbol conflicts with selective import",
                Some(&label.name),
                label.span,
            ));
        }

        let full_name = self.scoped_define_name(&label.name);
        let res = if self.pass == 1 {
            self.symbols.add(
                &full_name,
                self.start_addr,
                false,
                self.current_visibility(),
                self.symbol_scope.module_active.as_deref(),
            )
        } else {
            match self.symbols.entry_mut(&full_name) {
                Some(entry) if entry.rw => SymbolTableResult::Duplicate,
                Some(entry) => {
                    entry.val = self.start_addr;
                    entry.updated = true;
                    SymbolTableResult::Ok
                }
                None => self.symbols.add(
                    &full_name,
                    self.start_addr,
                    false,
                    self.current_visibility(),
                    self.symbol_scope.module_active.as_deref(),
                ),
            }
        };

        if res == SymbolTableResult::Duplicate {
            return Some(self.failure_at_span(
                LineStatus::Error,
                AsmErrorKind::Symbol,
                "Symbol defined more than once",
                Some(&label.name),
                label.span,
            ));
        }

        if res == SymbolTableResult::Ok {
            self.track_section_symbol(&full_name);
        }

        None
    }

    fn process_assignment_ast(
        &mut self,
        label: &Label,
        op: AssignOp,
        expr: &Expr,
        span: Span,
    ) -> LineStatus {
        self.label = Some(label.name.clone());

        match op {
            AssignOp::Const | AssignOp::Var | AssignOp::VarIfUndef => {
                let full_name = self.scoped_define_name(&label.name);
                if op == AssignOp::VarIfUndef {
                    if let Some(entry) = self.symbols.entry(&full_name) {
                        self.aux_value = entry.val;
                        return LineStatus::DirEqu;
                    }
                }
                let value = match self.eval_expr_for_scalar_context(expr) {
                    Ok(scalar) => match self.eval_value_ast(expr) {
                        Ok(
                            value @ (AsmValue::List(_)
                            | AsmValue::Range { .. }
                            | AsmValue::Struct(_)
                            | AsmValue::StructInstance(_)),
                        ) => value,
                        Ok(AsmValue::Scalar(_)) | Err(_) => AsmValue::Scalar(i64::from(scalar)),
                    },
                    Err(scalar_err) => match self.eval_value_ast(expr) {
                        Ok(value) => value,
                        Err(_) => {
                            return self.failure_at_span(
                                LineStatus::Error,
                                ast_eval_error_kind_to_asm(scalar_err.error.kind()),
                                scalar_err.error.message(),
                                None,
                                scalar_err.span,
                            )
                        }
                    },
                };
                let scalar_val = Self::scalar_shadow_for_value_symbol(&value);
                let is_rw = op != AssignOp::Const;
                if self.pass == 1 && self.selective_import_conflict(&label.name) {
                    return self.failure_at_span(
                        LineStatus::Error,
                        AsmErrorKind::Symbol,
                        "Symbol conflicts with selective import",
                        Some(&label.name),
                        label.span,
                    );
                }
                let res = if self.pass == 1 {
                    self.symbols.add(
                        &full_name,
                        scalar_val,
                        is_rw,
                        self.current_visibility(),
                        self.symbol_scope.module_active.as_deref(),
                    )
                } else {
                    match self.symbols.entry_mut(&full_name) {
                        Some(entry) if entry.rw && !is_rw => SymbolTableResult::Duplicate,
                        Some(entry) => {
                            entry.val = scalar_val;
                            entry.updated = true;
                            SymbolTableResult::Ok
                        }
                        None => self.symbols.add(
                            &full_name,
                            scalar_val,
                            is_rw,
                            self.current_visibility(),
                            self.symbol_scope.module_active.as_deref(),
                        ),
                    }
                };
                if res == SymbolTableResult::Duplicate {
                    return self.failure_at(
                        LineStatus::Error,
                        AsmErrorKind::Symbol,
                        "symbol has already been defined",
                        Some(&label.name),
                        Some(1),
                    );
                } else if res == SymbolTableResult::TableFull {
                    return self.failure_at(
                        LineStatus::Error,
                        AsmErrorKind::Symbol,
                        "could not add symbol, table full",
                        Some(&label.name),
                        Some(1),
                    );
                }
                self.sync_value_symbol(&full_name, &value);
                if op == AssignOp::Const && self.expr_is_absolute_constant_symbol_expr(expr) {
                    self.layout
                        .absolute_constant_symbols
                        .insert(full_name.clone());
                } else {
                    self.layout.absolute_constant_symbols.remove(&full_name);
                }
                self.aux_value = scalar_val;
                return LineStatus::DirEqu;
            }
            _ => {}
        }

        let target = match self.resolve_scoped_name(&label.name) {
            Ok(Some(name)) => name,
            Ok(None) => {
                return self.failure_at(
                    LineStatus::Error,
                    AsmErrorKind::Symbol,
                    "symbol has not been defined",
                    Some(&label.name),
                    Some(1),
                )
            }
            Err(err) => {
                return self.failure_at(
                    LineStatus::Error,
                    err.kind(),
                    err.message(),
                    Some(&label.name),
                    Some(1),
                )
            }
        };
        let (left_val, is_rw) = match self.symbols.entry(&target) {
            Some(entry) => (entry.val, entry.rw),
            None => {
                return self.failure_at(
                    LineStatus::Error,
                    AsmErrorKind::Symbol,
                    "symbol has not been defined",
                    Some(&label.name),
                    Some(1),
                )
            }
        };

        if !is_rw {
            return self.failure_at(
                LineStatus::Error,
                AsmErrorKind::Symbol,
                "symbol is read-only",
                Some(&label.name),
                Some(1),
            );
        }

        if let Some(value_symbol) = self.lookup_value_symbol(&target) {
            if matches!(value_symbol, AsmValue::StructInstance(_)) {
                let op_text = Self::assign_op_text(op).trim();
                let message = format!(
                    "operator '{op_text}' requires scalar symbol, found struct instance '{}'",
                    label.name
                );
                return self.failure_at(
                    LineStatus::Error,
                    AsmErrorKind::Symbol,
                    &message,
                    Some(&label.name),
                    Some(1),
                );
            }
            return self.failure_at(
                LineStatus::Error,
                AsmErrorKind::Symbol,
                "assignment operators require scalar symbols",
                Some(&label.name),
                Some(1),
            );
        }

        let rhs = match self.eval_expr_for_scalar_context(expr) {
            Ok(value) => value,
            Err(err) => {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                )
            }
        };
        let new_val = match apply_assignment_op(op, left_val, rhs, span) {
            Ok(val) => val,
            Err(err) => {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                )
            }
        };

        if let Some(entry) = self.symbols.entry_mut(&target) {
            entry.val = new_val;
            entry.updated = true;
        }
        self.aux_value = new_val;
        LineStatus::DirEqu
    }

    fn current_section_kind(&self) -> Option<SectionKind> {
        self.layout
            .current_section
            .as_ref()
            .and_then(|name| self.layout.sections.get(name))
            .map(|section| section.kind)
    }

    fn expr_is_relocation_free_literal(expr: &Expr) -> bool {
        match expr {
            Expr::Number(_, _) | Expr::String(_, _) => true,
            Expr::Indirect(inner, _)
            | Expr::IndirectLong(inner, _)
            | Expr::Immediate(inner, _)
            | Expr::Unary { expr: inner, .. } => Self::expr_is_relocation_free_literal(inner),
            Expr::List(items, _) | Expr::Tuple(items, _) => {
                items.iter().all(Self::expr_is_relocation_free_literal)
            }
            Expr::StructLiteral { fields, .. } => fields
                .iter()
                .all(|(_, value)| Self::expr_is_relocation_free_literal(value)),
            Expr::Binary { left, right, .. } => {
                Self::expr_is_relocation_free_literal(left)
                    && Self::expr_is_relocation_free_literal(right)
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => {
                Self::expr_is_relocation_free_literal(cond)
                    && Self::expr_is_relocation_free_literal(then_expr)
                    && Self::expr_is_relocation_free_literal(else_expr)
            }
            Expr::Range {
                start, end, step, ..
            } => {
                Self::expr_is_relocation_free_literal(start)
                    && Self::expr_is_relocation_free_literal(end)
                    && step
                        .as_ref()
                        .is_none_or(|step_expr| Self::expr_is_relocation_free_literal(step_expr))
            }
            Expr::Error(_, _)
            | Expr::Placeholder(_)
            | Expr::Dollar(_)
            | Expr::Identifier(_, _)
            | Expr::Register(_, _)
            | Expr::Index { .. }
            | Expr::Member { .. }
            | Expr::Call { .. } => false,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn expr_is_absolute_constant_symbol_expr(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Number(_, _) | Expr::String(_, _) => true,
            Expr::Identifier(name, _) => {
                let Some(resolved_name) = self.resolve_symbol_name_for_relocation(name) else {
                    return false;
                };
                self.layout
                    .absolute_constant_symbols
                    .contains(&resolved_name)
            }
            Expr::Indirect(inner, _)
            | Expr::IndirectLong(inner, _)
            | Expr::Immediate(inner, _)
            | Expr::Unary { expr: inner, .. } => self.expr_is_absolute_constant_symbol_expr(inner),
            Expr::List(items, _) | Expr::Tuple(items, _) => items
                .iter()
                .all(|item| self.expr_is_absolute_constant_symbol_expr(item)),
            Expr::StructLiteral { fields, .. } => fields
                .iter()
                .all(|(_, value)| self.expr_is_absolute_constant_symbol_expr(value)),
            Expr::Binary { left, right, .. } => {
                self.expr_is_absolute_constant_symbol_expr(left)
                    && self.expr_is_absolute_constant_symbol_expr(right)
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => {
                self.expr_is_absolute_constant_symbol_expr(cond)
                    && self.expr_is_absolute_constant_symbol_expr(then_expr)
                    && self.expr_is_absolute_constant_symbol_expr(else_expr)
            }
            Expr::Range {
                start, end, step, ..
            } => {
                self.expr_is_absolute_constant_symbol_expr(start)
                    && self.expr_is_absolute_constant_symbol_expr(end)
                    && step.as_ref().is_none_or(|step_expr| {
                        self.expr_is_absolute_constant_symbol_expr(step_expr)
                    })
            }
            Expr::Error(_, _)
            | Expr::Placeholder(_)
            | Expr::Dollar(_)
            | Expr::Register(_, _)
            | Expr::Index { .. }
            | Expr::Member { .. }
            | Expr::Call { .. } => false,
        }
    }

    #[cfg(feature = "vm-runtime-only")]
    #[allow(dead_code)]
    fn expr_is_absolute_constant_symbol_expr(&self, expr: &Expr) -> bool {
        Self::expr_is_relocation_free_literal(expr)
    }

    fn operands_are_relocation_free_literals(operands: &[Expr]) -> bool {
        operands.iter().all(Self::expr_is_relocation_free_literal)
    }

    fn emit_unit_is_relocation_free_literal(&self, unit: &Expr) -> bool {
        match unit {
            Expr::Identifier(name, _) | Expr::Register(name, _) => {
                name.eq_ignore_ascii_case("byte")
                    || name.eq_ignore_ascii_case("word")
                    || name.eq_ignore_ascii_case("long")
            }
            _ => Self::expr_is_relocation_free_literal(unit),
        }
    }

    fn max_program_address(&self) -> u32 {
        self.cpu_mode.program_address_max
    }

    fn validate_program_address(
        &self,
        value: u32,
        directive_name: &str,
        span: Span,
    ) -> Result<(), AstEvalError> {
        let max = self.max_program_address();
        if value <= max {
            return Ok(());
        }
        let message = format!(
            "{directive_name} address ${} exceeds max ${} for CPU {}",
            format_addr(value),
            format_addr(max),
            self.cpu.as_str()
        );
        Err(ast_eval_error(AsmErrorKind::Directive, &message, span))
    }

    fn validate_program_span(
        &self,
        size_bytes: u32,
        directive_name: &str,
        span: Span,
    ) -> Result<(), AstEvalError> {
        if size_bytes == 0 {
            return Ok(());
        }
        let max = self.max_program_address();
        let start = self.start_addr;
        let end = match start.checked_add(size_bytes - 1) {
            Some(end) => end,
            None => {
                let message = format!(
                    "{directive_name} size overflows address arithmetic for CPU {}",
                    self.cpu.as_str()
                );
                return Err(AstEvalError::directive(message, span));
            }
        };
        if end <= max {
            return Ok(());
        }
        let message = format!(
            "{directive_name} span ${}..${} exceeds max ${} for CPU {}",
            format_addr(start),
            format_addr(end),
            format_addr(max),
            self.cpu.as_str()
        );
        Err(AstEvalError::directive(message, span))
    }

    fn validate_instruction_emit_span(
        &self,
        mnemonic: &str,
        operands: &[Expr],
        byte_count: usize,
    ) -> Result<(), AstEvalError> {
        let size_bytes = match u32::try_from(byte_count) {
            Ok(size_bytes) => size_bytes,
            Err(_) => {
                return Err(ast_eval_error(
                    AsmErrorKind::Instruction,
                    "instruction size overflow exceeds supported range",
                    operands.first().map(expr_span).unwrap_or_default(),
                ));
            }
        };
        let span = operands.first().map(expr_span).unwrap_or_default();
        let label = format!("instruction {}", mnemonic.to_ascii_uppercase());
        self.validate_program_span(size_bytes, &label, span)
    }

    fn current_cpu_little_endian(&self) -> bool {
        self.cpu_mode.little_endian
    }

    fn cpu_word_size_bytes(&self) -> u32 {
        self.cpu_mode.word_size_bytes
    }

    fn section_kind_allows_data(&self) -> bool {
        self.current_section_kind() != Some(SectionKind::Bss)
    }

    fn section_kind_requires_bss(&self) -> bool {
        self.current_section_kind() == Some(SectionKind::Bss)
    }

    fn current_section_kind_label(&self) -> &'static str {
        self.current_section_kind()
            .map(section_kind_name)
            .unwrap_or("none")
    }

    fn parse_emit_unit_bytes(&self, unit: &Expr) -> Result<u32, AstEvalError> {
        match unit {
            Expr::Identifier(name, _) | Expr::Register(name, _) => {
                if name.eq_ignore_ascii_case("byte") {
                    Ok(1)
                } else if name.eq_ignore_ascii_case("word") {
                    Ok(self.cpu_word_size_bytes())
                } else if name.eq_ignore_ascii_case("long") {
                    Ok(4)
                } else {
                    self.eval_expr_for_non_negative_directive(unit, ".emit/.fill/.res unit")
                }
            }
            _ => self.eval_expr_for_non_negative_directive(unit, ".emit/.fill/.res unit"),
        }
    }

    pub fn eval_expr_for_non_negative_directive(
        &self,
        expr: &Expr,
        directive_name: &str,
    ) -> Result<u32, AstEvalError> {
        if let Some((name, span)) = self.find_private_symbol_in_expr(expr) {
            return Err(ast_eval_from_asm_error(self.visibility_error(&name), span));
        }

        match AssemblerContext::eval_expr(self, expr) {
            Ok(value) => {
                if value < 0 {
                    return Err(AstEvalError::expression(
                        format!("Expected non-negative value for {directive_name}"),
                        expr_span(expr),
                    ));
                }

                match u32::try_from(value) {
                    Ok(value) => Ok(value),
                    Err(_) => Err(AstEvalError::expression(
                        format!("Value out of supported range for {directive_name}"),
                        expr_span(expr),
                    )),
                }
            }
            Err(message) => Err(AstEvalError::expression(message, expr_span(expr))),
        }
    }

    pub(crate) fn eval_expr_for_scalar_context(&self, expr: &Expr) -> Result<u32, AstEvalError> {
        if let Some((name, span)) = self.find_private_symbol_in_expr(expr) {
            return Err(ast_eval_from_asm_error(self.visibility_error(&name), span));
        }

        if let Expr::Identifier(name, _) | Expr::Register(name, _) = expr {
            if let Some(AsmValue::Scalar(value)) = AssemblerContext::value_symbol(self, name) {
                return Ok(value as u32);
            }
        }

        match AssemblerContext::eval_expr(self, expr) {
            Ok(value) => Ok(value as u32),
            Err(message) => Err(AstEvalError::expression(message, expr_span(expr))),
        }
    }

    fn eval_expr_for_data_directive(&self, expr: &Expr) -> Result<u32, AstEvalError> {
        self.eval_expr_for_scalar_context(expr)
    }

    fn find_private_symbol_in_expr(&self, expr: &Expr) -> Option<(String, Span)> {
        match expr {
            Expr::Identifier(name, span) | Expr::Register(name, span) => {
                if self.lookup_loop_var(name).is_some() {
                    return None;
                }
                if let Some(entry) = self.lookup_scoped_entry(name) {
                    if !self.entry_is_visible(entry) {
                        return Some((name.clone(), *span));
                    }
                }
                None
            }
            Expr::Indirect(inner, _)
            | Expr::IndirectLong(inner, _)
            | Expr::Immediate(inner, _)
            | Expr::Unary { expr: inner, .. } => self.find_private_symbol_in_expr(inner),
            Expr::List(items, _) => items
                .iter()
                .find_map(|item| self.find_private_symbol_in_expr(item)),
            Expr::Index { base, index, .. } => self
                .find_private_symbol_in_expr(base)
                .or_else(|| self.find_private_symbol_in_expr(index)),
            Expr::Member { base, .. } => self.find_private_symbol_in_expr(base),
            Expr::StructLiteral { fields, .. } => fields
                .iter()
                .find_map(|(_, value)| self.find_private_symbol_in_expr(value)),
            Expr::Call { args, .. } => args
                .iter()
                .find_map(|arg| self.find_private_symbol_in_expr(arg)),
            Expr::Placeholder(_) => None,
            Expr::Tuple(items, _) => items
                .iter()
                .find_map(|item| self.find_private_symbol_in_expr(item)),
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => self
                .find_private_symbol_in_expr(cond)
                .or_else(|| self.find_private_symbol_in_expr(then_expr))
                .or_else(|| self.find_private_symbol_in_expr(else_expr)),
            Expr::Binary { left, right, .. } => self
                .find_private_symbol_in_expr(left)
                .or_else(|| self.find_private_symbol_in_expr(right)),
            Expr::Range {
                start, end, step, ..
            } => self
                .find_private_symbol_in_expr(start)
                .or_else(|| self.find_private_symbol_in_expr(end))
                .or_else(|| {
                    step.as_ref()
                        .and_then(|step_expr| self.find_private_symbol_in_expr(step_expr))
                }),
            Expr::Error(_, _) | Expr::Number(_, _) | Expr::Dollar(_) | Expr::String(_, _) => None,
        }
    }

    fn write_unit_value(
        &mut self,
        unit_bytes: usize,
        value: u32,
        span: Span,
    ) -> Result<(), AstEvalError> {
        let unit_bits = unit_bytes.saturating_mul(8);
        if unit_bits < 32 {
            let max = (1u64 << unit_bits) - 1;
            if (value as u64) > max {
                let hex_width = usize::max(2, unit_bytes.saturating_mul(2));
                let max_u32 = max as u32;
                let msg = format!(
                    "Value ${value:0hex_width$X} ({value}) does not fit in {unit_bytes}-byte unit (max ${max_u32:0hex_width$X})"
                );
                return Err(AstEvalError::directive(msg, span));
            }
        }

        let little_endian = self.current_cpu_little_endian();
        if little_endian {
            for shift in 0..unit_bytes {
                let byte = if shift < 4 {
                    (value >> (shift * 8)) as u8
                } else {
                    0
                };
                self.bytes.push(byte);
            }
        } else {
            for shift in (0..unit_bytes).rev() {
                let byte = if shift < 4 {
                    (value >> (shift * 8)) as u8
                } else {
                    0
                };
                self.bytes.push(byte);
            }
        }
        Ok(())
    }

    fn emit_directive_ast(&mut self, operands: &[Expr]) -> LineStatus {
        if operands.len() < 2 {
            return self.failure(
                LineStatus::Error,
                AsmErrorKind::Directive,
                "Missing unit or values for .emit",
                None,
            );
        }
        if !self.section_kind_allows_data() {
            let msg = format!(
                ".emit is not allowed in kind=bss section (current kind={})",
                self.current_section_kind_label()
            );
            return self.failure(LineStatus::Error, AsmErrorKind::Directive, &msg, None);
        }

        let unit_bytes = match self.parse_emit_unit_bytes(&operands[0]) {
            Ok(value) => value,
            Err(err) => {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                )
            }
        };
        if unit_bytes == 0 {
            return self.failure(
                LineStatus::Error,
                AsmErrorKind::Directive,
                "Unit size must be greater than zero",
                None,
            );
        }
        let emit_count = match u32::try_from(operands.len().saturating_sub(1)) {
            Ok(count) => count,
            Err(_) => {
                return self.failure(
                    LineStatus::Error,
                    AsmErrorKind::Directive,
                    ".emit operand list is too large",
                    None,
                )
            }
        };
        let total = match unit_bytes.checked_mul(emit_count) {
            Some(total) => total,
            None => {
                return self.failure(
                    LineStatus::Error,
                    AsmErrorKind::Directive,
                    ".emit total size overflow exceeds supported range",
                    None,
                )
            }
        };
        if let Err(err) = self.validate_program_span(total, ".emit", expr_span(&operands[0])) {
            return self.failure_at_span(
                LineStatus::Error,
                ast_eval_error_kind_to_asm(err.error.kind()),
                err.error.message(),
                None,
                err.span,
            );
        }

        let mut saw_supported_relocation = false;
        let mut unsupported_nonfree = !operands
            .first()
            .is_some_and(|unit| self.emit_unit_is_relocation_free_literal(unit));
        let mut unsupported_hunk_fixup = false;
        let mut planned_values: Vec<(u32, Option<String>, Span)> = Vec::new();
        for expr in &operands[1..] {
            let relocation = if unit_bytes == 4 {
                match self.eval_hunk_abs32_relocation_value(expr) {
                    Ok(value) => value,
                    Err(err) => {
                        return self.failure_at_span(
                            LineStatus::Error,
                            ast_eval_error_kind_to_asm(err.error.kind()),
                            err.error.message(),
                            None,
                            err.span,
                        )
                    }
                }
            } else {
                None
            };

            if let Some((value, target_section)) = relocation {
                saw_supported_relocation = true;
                planned_values.push((value, Some(target_section), expr_span(expr)));
                continue;
            }

            let value = match self.eval_expr_for_data_directive(expr) {
                Ok(value) => value,
                Err(err) => {
                    return self.failure_at_span(
                        LineStatus::Error,
                        ast_eval_error_kind_to_asm(err.error.kind()),
                        err.error.message(),
                        None,
                        err.span,
                    )
                }
            };
            if self.hunk_data_expression_requires_unsupported_fixup(expr) {
                unsupported_nonfree = true;
                unsupported_hunk_fixup = true;
            }
            planned_values.push((value, None, expr_span(expr)));
        }

        if unsupported_hunk_fixup {
            self.mark_current_section_hunk_fixup_error(
                "format=hunk does not support this symbolic .emit long expression in v0.2",
            );
        }

        if unsupported_nonfree {
            self.mark_current_section_not_relocation_free();
        } else if saw_supported_relocation {
            self.mark_current_section_hunk_relocatable();
        }

        for (value, target_section, span) in planned_values {
            let relocation_offset = match u32::try_from(self.bytes.len()) {
                Ok(offset) => offset,
                Err(_) => {
                    return self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Directive,
                        ".emit relocation offset exceeds supported range",
                        None,
                    )
                }
            };
            if let Err(err) = self.write_unit_value(unit_bytes as usize, value, span) {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                );
            }
            if let Some(target_section) = target_section {
                if let Some(fixup) =
                    self.hunk_abs32_output_fixup(relocation_offset, value, target_section)
                {
                    self.pending_output_fixups.push(fixup);
                }
            }
        }

        LineStatus::Ok
    }

    fn res_directive_ast(&mut self, operands: &[Expr]) -> LineStatus {
        if !self.section_kind_requires_bss()
            && !Self::operands_are_relocation_free_literals(operands)
        {
            self.mark_current_section_not_relocation_free();
        }

        if operands.len() != 2 {
            return self.failure(
                LineStatus::Error,
                AsmErrorKind::Directive,
                "Expected .res <unit>, <count>",
                None,
            );
        }
        if !self.section_kind_requires_bss() {
            let msg = format!(
                ".res is only allowed in kind=bss section (current kind={})",
                self.current_section_kind_label()
            );
            return self.failure(LineStatus::Error, AsmErrorKind::Directive, &msg, None);
        }

        let unit_bytes = match self.parse_emit_unit_bytes(&operands[0]) {
            Ok(value) => value,
            Err(err) => {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                )
            }
        };
        if unit_bytes == 0 {
            return self.failure(
                LineStatus::Error,
                AsmErrorKind::Directive,
                "Unit size must be greater than zero",
                None,
            );
        }
        let count = match self.eval_expr_for_non_negative_directive(&operands[1], ".res count") {
            Ok(value) => value,
            Err(err) => {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                )
            }
        };
        let total = match unit_bytes.checked_mul(count) {
            Some(total) => total,
            None => {
                let msg = format!(
                    ".res total size overflow (unit={unit_bytes}, count={count}) exceeds supported range"
                );
                return self.failure(LineStatus::Error, AsmErrorKind::Directive, &msg, None);
            }
        };
        if let Err(err) = self.validate_program_span(total, ".res", expr_span(&operands[1])) {
            return self.failure_at_span(
                LineStatus::Error,
                ast_eval_error_kind_to_asm(err.error.kind()),
                err.error.message(),
                None,
                err.span,
            );
        }
        self.aux_value = total;
        LineStatus::DirDs
    }

    fn fill_directive_ast(&mut self, operands: &[Expr]) -> LineStatus {
        let relocation_free = operands.len() == 3
            && self.emit_unit_is_relocation_free_literal(&operands[0])
            && Self::operands_are_relocation_free_literals(&operands[1..]);
        if !relocation_free {
            self.mark_current_section_not_relocation_free();
        }

        if operands.len() != 3 {
            return self.failure(
                LineStatus::Error,
                AsmErrorKind::Directive,
                "Expected .fill <unit>, <count>, <value>",
                None,
            );
        }
        if !self.section_kind_allows_data() {
            let msg = format!(
                ".fill is not allowed in kind=bss section (current kind={})",
                self.current_section_kind_label()
            );
            return self.failure(LineStatus::Error, AsmErrorKind::Directive, &msg, None);
        }

        let unit_bytes = match self.parse_emit_unit_bytes(&operands[0]) {
            Ok(value) => value,
            Err(err) => {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                )
            }
        };
        if unit_bytes == 0 {
            return self.failure(
                LineStatus::Error,
                AsmErrorKind::Directive,
                "Unit size must be greater than zero",
                None,
            );
        }
        let count = match self.eval_expr_for_non_negative_directive(&operands[1], ".fill count") {
            Ok(value) => value,
            Err(err) => {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                )
            }
        };
        let value = match self.eval_expr_for_data_directive(&operands[2]) {
            Ok(value) => value,
            Err(err) => {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                )
            }
        };
        let total = match unit_bytes.checked_mul(count) {
            Some(total) => total,
            None => {
                let msg = format!(
                    ".fill total size overflow (unit={unit_bytes}, count={count}) exceeds supported range"
                );
                return self.failure(LineStatus::Error, AsmErrorKind::Directive, &msg, None);
            }
        };
        if let Err(err) = self.validate_program_span(total, ".fill", expr_span(&operands[1])) {
            return self.failure_at_span(
                LineStatus::Error,
                ast_eval_error_kind_to_asm(err.error.kind()),
                err.error.message(),
                None,
                err.span,
            );
        }

        for _ in 0..count {
            if let Err(err) =
                self.write_unit_value(unit_bytes as usize, value, expr_span(&operands[2]))
            {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                );
            }
        }
        LineStatus::Ok
    }

    fn store_arg_list_ast(
        &mut self,
        operands: &[Expr],
        size: usize,
        directive_name: &str,
    ) -> LineStatus {
        if !self.section_kind_allows_data() {
            let msg = format!(
                "Data emit directives are not allowed in kind=bss section (current kind={})",
                self.current_section_kind_label()
            );
            return self.failure(LineStatus::Error, AsmErrorKind::Directive, &msg, None);
        }
        if operands.is_empty() {
            return self.failure(
                LineStatus::Error,
                AsmErrorKind::Directive,
                "Missing expression in data list",
                None,
            );
        }

        let unit_size = size as u32;
        let mut projected_total = 0u32;
        let mut saw_supported_relocation = false;
        let mut unsupported_nonfree = false;
        let mut unsupported_hunk_fixup = false;
        for expr in operands {
            if let Expr::String(raw_bytes, span) = expr {
                let encoded_bytes = match self.encode_text_bytes(
                    raw_bytes,
                    *span,
                    directive_name,
                    AsmErrorKind::Directive,
                ) {
                    Ok(bytes) => bytes,
                    Err(err) => {
                        return self.failure_at_span(
                            LineStatus::Error,
                            ast_eval_error_kind_to_asm(err.error.kind()),
                            err.error.message(),
                            None,
                            err.span,
                        );
                    }
                };
                if encoded_bytes.len() > 1 {
                    let string_len = match u32::try_from(encoded_bytes.len()) {
                        Ok(len) => len,
                        Err(_) => {
                            return self.failure_at_span(
                                LineStatus::Error,
                                AsmErrorKind::Directive,
                                "String literal too large to emit",
                                None,
                                *span,
                            );
                        }
                    };
                    projected_total = match projected_total.checked_add(string_len) {
                        Some(total) => total,
                        None => {
                            let msg = format!(
                                "{directive_name} total size overflow exceeds supported range"
                            );
                            return self.failure_at_span(
                                LineStatus::Error,
                                AsmErrorKind::Directive,
                                &msg,
                                None,
                                *span,
                            );
                        }
                    };
                    if let Err(err) =
                        self.validate_program_span(projected_total, directive_name, *span)
                    {
                        return self.failure_at_span(
                            LineStatus::Error,
                            ast_eval_error_kind_to_asm(err.error.kind()),
                            err.error.message(),
                            None,
                            err.span,
                        );
                    }
                    self.bytes.extend_from_slice(&encoded_bytes);
                    continue;
                }
                if encoded_bytes.is_empty() {
                    return self.failure_at_span(
                        LineStatus::Error,
                        AsmErrorKind::Directive,
                        "Empty string not allowed in expression list",
                        None,
                        *span,
                    );
                }
            }
            projected_total = match projected_total.checked_add(unit_size) {
                Some(total) => total,
                None => {
                    let msg =
                        format!("{directive_name} total size overflow exceeds supported range");
                    return self.failure_at_span(
                        LineStatus::Error,
                        AsmErrorKind::Directive,
                        &msg,
                        None,
                        expr_span(expr),
                    );
                }
            };
            if let Err(err) =
                self.validate_program_span(projected_total, directive_name, expr_span(expr))
            {
                return self.failure_at_span(
                    LineStatus::Error,
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message(),
                    None,
                    err.span,
                );
            }
            let relocation = if size == 4 {
                match self.eval_hunk_abs32_data_relocation_value(expr) {
                    Ok(value) => value,
                    Err(err) => {
                        return self.failure_at_span(
                            LineStatus::Error,
                            ast_eval_error_kind_to_asm(err.error.kind()),
                            err.error.message(),
                            None,
                            err.span,
                        )
                    }
                }
            } else {
                None
            };
            let (val, target_section) = if let Some((value, target_section)) = relocation {
                saw_supported_relocation = true;
                (value, Some(target_section))
            } else {
                let value = match self.eval_expr_for_data_directive(expr) {
                    Ok(value) => value,
                    Err(err) => {
                        return self.failure_at_span(
                            LineStatus::Error,
                            ast_eval_error_kind_to_asm(err.error.kind()),
                            err.error.message(),
                            None,
                            err.span,
                        )
                    }
                };
                if self.hunk_data_expression_requires_unsupported_fixup(expr) {
                    unsupported_hunk_fixup = true;
                    unsupported_nonfree = true;
                }
                (value, None)
            };
            let relocation_offset = match u32::try_from(self.bytes.len()) {
                Ok(offset) => offset,
                Err(_) => {
                    return self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Directive,
                        "data relocation offset exceeds supported range",
                        None,
                    )
                }
            };
            if size == 1 {
                if val > 0xff {
                    return self.failure(
                        LineStatus::Warning,
                        AsmErrorKind::Expression,
                        "Value truncated to byte",
                        None,
                    );
                }
                self.bytes.push((val & 0xff) as u8);
            } else if size == 2 {
                if self.current_cpu_little_endian() {
                    self.bytes.push((val & 0xff) as u8);
                    self.bytes.push((val >> 8) as u8);
                } else {
                    self.bytes.push((val >> 8) as u8);
                    self.bytes.push((val & 0xff) as u8);
                }
            } else if size == 4 {
                if self.current_cpu_little_endian() {
                    self.bytes.push((val & 0xff) as u8);
                    self.bytes.push(((val >> 8) & 0xff) as u8);
                    self.bytes.push(((val >> 16) & 0xff) as u8);
                    self.bytes.push(((val >> 24) & 0xff) as u8);
                } else {
                    self.bytes.push(((val >> 24) & 0xff) as u8);
                    self.bytes.push(((val >> 16) & 0xff) as u8);
                    self.bytes.push(((val >> 8) & 0xff) as u8);
                    self.bytes.push((val & 0xff) as u8);
                }
            } else {
                return self.failure(
                    LineStatus::Error,
                    AsmErrorKind::Directive,
                    "Unsupported data size for directive",
                    None,
                );
            }
            if let Some(target_section) = target_section {
                if let Some(fixup) =
                    self.hunk_abs32_output_fixup(relocation_offset, val, target_section)
                {
                    self.pending_output_fixups.push(fixup);
                }
            }
        }

        if unsupported_hunk_fixup {
            let directive = match size {
                1 => ".byte",
                2 => ".word",
                4 => ".long",
                _ => ".data",
            };
            self.mark_current_section_hunk_fixup_error(&format!(
                "format=hunk does not support this symbolic {directive} expression in v0.3"
            ));
        }

        if unsupported_nonfree {
            self.mark_current_section_not_relocation_free();
        } else if saw_supported_relocation {
            self.mark_current_section_hunk_relocatable();
        }

        LineStatus::Ok
    }
}

fn registry_error_message(err: RegistryError) -> String {
    match err {
        RegistryError::MissingFamily(family) => {
            format!("Missing family module for {family:?}")
        }
        RegistryError::MissingCpu(cpu) => format!("Missing CPU module for {cpu:?}"),
        RegistryError::MissingDialect { family, dialect } => {
            format!("Missing dialect '{dialect}' for {family:?}")
        }
    }
}

fn is_symbol_assignment_directive(mnemonic: &str) -> bool {
    matches!(
        mnemonic.to_ascii_uppercase().as_str(),
        ".CONST" | ".VAR" | ".SET"
    )
}

fn directive_handles_label_lifecycle(mnemonic: &str) -> bool {
    matches!(
        mnemonic.to_ascii_uppercase().as_str(),
        ".STRUCT" | ".ENDSTRUCT"
    )
}

fn is_scope_directive(mnemonic: &str) -> bool {
    matches!(
        mnemonic.to_ascii_uppercase().as_str(),
        ".BLOCK"
            | ".ENDBLOCK"
            | ".BEND"
            | ".NAMESPACE"
            | ".ENDN"
            | ".ENDNAMESPACE"
            | ".MODULE"
            | ".ENDMODULE"
            | ".META"
            | ".ENDMETA"
            | ".SECTION"
            | ".ENDSECTION"
    )
}

fn is_meta_block_directive(mnemonic: &str) -> bool {
    let upper = mnemonic.to_ascii_uppercase();
    matches!(
        upper.as_str(),
        ".META" | ".NAME" | ".VERSION" | ".OUTPUT" | ".ENDOUTPUT" | ".ENDMETA"
    ) || upper.starts_with(".OUTPUT.")
}

fn is_output_block_directive(mnemonic: &str) -> bool {
    let upper = mnemonic.to_ascii_uppercase();
    matches!(
        upper.as_str(),
        ".NAME" | ".LIST" | ".HEX" | ".BIN" | ".FILL" | ".OUTPUT" | ".ENDOUTPUT"
    ) || upper.starts_with(".OUTPUT.")
}

fn is_toplevel_directive(mnemonic: &str) -> bool {
    matches!(
        mnemonic.to_ascii_uppercase().as_str(),
        ".MODULE" | ".ENDMODULE" | ".END"
    )
}

fn is_identifierish(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == '.'
}
