// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Runtime execution model ownership for hierarchy-aware `.opasm` selection and parsing.
//!
//! Partition note:
//!
//! - this module belongs to the `.opasm` VM surface
//! - some helpers inside it are still transitional where assembler statement
//!   parsing delegates expression work back through explicit `opcore` requests
//! - those mixed seams remain inside `vm` and are tracked in the implementation
//!   plan inventory rather than being pushed out to non-VM crates

use std::cell::Cell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use opcore::expr_vm::PortableExprBudgets;
use opcore::parser::{Expr, ParseError};
use opcore::tokenizer::{Span, Token};
use package::{
    HierarchyChunks, ModeSelectorDescriptor, DIAG_OPTHREAD_FORCE_UNSUPPORTED_6502,
    DIAG_OPTHREAD_FORCE_UNSUPPORTED_65C02, DIAG_OPTHREAD_INVALID_FORCE_OVERRIDE,
    DIAG_OPTHREAD_MISSING_VM_PROGRAM,
};
use registry::family::AssemblerContext;
use registry::registry::{ModuleRegistry, OperandSet, VmEncodeCandidate};
use registry::syntax::RegisterChecker;
use types::hierarchy::ResolvedHierarchy;

use crate::portable_contract::{PortableLineAst, PortableToken};
use crate::runtime_contract_types::{
    RuntimeExprContract, RuntimeExprParserContract, RuntimeParserCertificationChecklists,
};
use crate::runtime_diagnostics::RuntimeBridgeDiagnostic;
use crate::runtime_error::RuntimeBridgeError;
use crate::runtime_model_core::RuntimeModelCore;
use crate::runtime_model_types::{
    RuntimeBudgetLimits, RuntimeBudgetProfile, RuntimeParserContract, RuntimeParserVmProgram,
    RuntimeTokenPolicy, RuntimeTokenizerMode, RuntimeTokenizerVmProgram,
};
use crate::runtime_portable_types::{PortableInstructionAdapter, PortableTokenizeRequest};

pub const HOST_PARSER_UNEXPECTED_END_OF_EXPRESSION: &str = "Unexpected end of expression";

mod contract_bridge;
pub(crate) mod directives;
mod encoding_bridge;
mod model_core_helpers;
pub(crate) mod parser_vm;
pub(crate) mod parser_vm_v2;
mod selector_bridge;
mod selector_encoding;
#[cfg(test)]
mod tests;
mod tokenizer_bridge;

pub use tokenizer_bridge::apply_token_policy_to_token;

thread_local! {
    pub static CORE_EXPR_PARSER_FAILPOINT: Cell<bool> = const { Cell::new(false) };
    pub static RUNTIME_EXPR_COMPATIBILITY_FAILPOINT: Cell<bool> = const { Cell::new(false) };
}

pub fn set_core_expr_parser_failpoint_for_tests(enabled: bool) {
    CORE_EXPR_PARSER_FAILPOINT.with(|flag| flag.set(enabled));
}

pub fn set_runtime_expr_compatibility_failpoint_for_tests(enabled: bool) {
    RUNTIME_EXPR_COMPATIBILITY_FAILPOINT.with(|flag| flag.set(enabled));
}

/// Family-keyed operand parse/resolve adapter used by expr-based runtime encode.
pub type ExprResolverFn = fn(
    &HierarchyExecutionModel,
    &ResolvedHierarchy,
    &str,
    &[Expr],
    &dyn AssemblerContext,
) -> Result<Option<Vec<VmEncodeCandidate>>, RuntimeBridgeError>;

pub(crate) type OperandSurfaceExprSubparser<'a> =
    dyn for<'tokens> FnMut(&'tokens [Token], Span, Option<String>) -> Result<Expr, ParseError> + 'a;

pub(crate) type OperandSurfaceExprParserFn = fn(
    &[Token],
    Option<&str>,
    usize,
    Span,
    Option<&str>,
    &mut OperandSurfaceExprSubparser<'_>,
    &mut OperandSurfaceExprSubparser<'_>,
) -> Result<Option<Expr>, ParseError>;

/// Generic family adapter contract for expr-based parse/resolve candidate generation.
pub trait FamilyExprResolver: std::fmt::Debug + Send + Sync {
    fn family_id(&self) -> &str;
    fn resolve_candidates(
        &self,
        model: &HierarchyExecutionModel,
        resolved: &ResolvedHierarchy,
        mnemonic: &str,
        operands: &[Expr],
        ctx: &dyn AssemblerContext,
    ) -> Result<Option<Vec<VmEncodeCandidate>>, RuntimeBridgeError>;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum SelectorOperandForce {
    DirectPage,
    DataBank,
    ProgramBank,
    Long,
}

#[derive(Debug)]
struct FnFamilyExprResolver {
    family_id: String,
    resolver: ExprResolverFn,
}

impl FamilyExprResolver for FnFamilyExprResolver {
    fn family_id(&self) -> &str {
        self.family_id.as_str()
    }

    fn resolve_candidates(
        &self,
        model: &HierarchyExecutionModel,
        resolved: &ResolvedHierarchy,
        mnemonic: &str,
        operands: &[Expr],
        ctx: &dyn AssemblerContext,
    ) -> Result<Option<Vec<VmEncodeCandidate>>, RuntimeBridgeError> {
        (self.resolver)(model, resolved, mnemonic, operands, ctx)
    }
}

#[derive(Debug)]
struct ExprResolverEntry {
    resolver: Box<dyn FamilyExprResolver>,
    strict: bool,
    defer_native_diagnostics_on_none: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ParserVmRouteCacheKey {
    cpu_id: String,
    dialect_override: Option<String>,
}

impl ParserVmRouteCacheKey {
    fn new(cpu_id: &str, dialect_override: Option<&str>) -> Self {
        Self {
            cpu_id: cpu_id.to_string(),
            dialect_override: dialect_override.map(str::to_string),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TokenizerVmRouteCacheKey {
    cpu_id: String,
    dialect_override: Option<String>,
}

impl TokenizerVmRouteCacheKey {
    fn new(cpu_id: &str, dialect_override: Option<&str>) -> Self {
        Self {
            cpu_id: cpu_id.to_string(),
            dialect_override: dialect_override.map(str::to_string),
        }
    }
}

#[derive(Debug)]
pub(crate) struct ResolvedParserVmRoute {
    pub(crate) parser_contract: RuntimeParserContract,
    pub(crate) parser_vm_program: RuntimeParserVmProgram,
    max_parser_tokens_per_line: usize,
    max_parser_ast_nodes_per_line: usize,
    parser_error_code: String,
}

impl ResolvedParserVmRoute {
    fn new(
        parser_contract: RuntimeParserContract,
        parser_vm_program: RuntimeParserVmProgram,
        max_parser_tokens_per_line: usize,
        max_parser_ast_nodes_per_line: usize,
    ) -> Self {
        let parser_error_code = parser_contract_error_code(&parser_contract).to_string();
        let max_parser_ast_nodes_per_line =
            (parser_contract.max_ast_nodes_per_line as usize).min(max_parser_ast_nodes_per_line);
        Self {
            parser_contract,
            parser_vm_program,
            max_parser_tokens_per_line,
            max_parser_ast_nodes_per_line,
            parser_error_code,
        }
    }

    pub(crate) fn enforce_line_budget(
        &self,
        estimated_ast_nodes: usize,
    ) -> Result<(), RuntimeBridgeError> {
        if estimated_ast_nodes > self.max_parser_tokens_per_line {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    self.parser_error_code.as_str(),
                    format!(
                        "parser token budget exceeded ({} > {})",
                        estimated_ast_nodes, self.max_parser_tokens_per_line
                    ),
                    None,
                ),
            ));
        }
        if estimated_ast_nodes > self.max_parser_ast_nodes_per_line {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    self.parser_error_code.as_str(),
                    format!(
                        "parser AST node budget exceeded ({} > {})",
                        estimated_ast_nodes, self.max_parser_ast_nodes_per_line
                    ),
                    None,
                ),
            ));
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct ResolvedTokenizerVmRoute {
    pub(crate) family_id: String,
    pub(crate) cpu_id: String,
    pub(crate) dialect_id: String,
    pub(crate) token_policy: RuntimeTokenPolicy,
    pub(crate) tokenizer_vm_program: RuntimeTokenizerVmProgram,
    pub(crate) use_default_dispatch_fast_path: bool,
}

impl ResolvedTokenizerVmRoute {
    fn new(
        resolved: &ResolvedHierarchy,
        token_policy: RuntimeTokenPolicy,
        tokenizer_vm_program: RuntimeTokenizerVmProgram,
        use_default_dispatch_fast_path: bool,
    ) -> Self {
        Self {
            family_id: resolved.family_id.clone(),
            cpu_id: resolved.cpu_id.clone(),
            dialect_id: resolved.dialect_id.clone(),
            token_policy,
            tokenizer_vm_program,
            use_default_dispatch_fast_path,
        }
    }
}

fn parser_contract_error_code(contract: &RuntimeParserContract) -> &str {
    let code = contract.diagnostics.invalid_statement.trim();
    if code.is_empty() {
        "vm-runtime"
    } else {
        code
    }
}

fn register_fn_resolver(
    map: &mut HashMap<String, ExprResolverEntry>,
    family_id: &str,
    resolver: ExprResolverFn,
    strict: bool,
    defer_native_diagnostics_on_none: bool,
) {
    let key = family_id.to_ascii_lowercase();
    map.insert(
        key.clone(),
        ExprResolverEntry {
            resolver: Box::new(FnFamilyExprResolver {
                family_id: key,
                resolver,
            }),
            strict,
            defer_native_diagnostics_on_none,
        },
    );
}

fn default_expr_resolvers() -> HashMap<String, ExprResolverEntry> {
    let mut expr_resolvers = HashMap::new();
    register_fn_resolver(
        &mut expr_resolvers,
        "mos6502",
        HierarchyExecutionModel::select_candidates_from_exprs_mos6502,
        true,
        false,
    );
    register_fn_resolver(
        &mut expr_resolvers,
        "intel8080",
        HierarchyExecutionModel::select_candidates_from_exprs_intel8080,
        true,
        true,
    );
    register_fn_resolver(
        &mut expr_resolvers,
        "motorola6800",
        HierarchyExecutionModel::select_candidates_from_exprs_m6800,
        true,
        false,
    );
    expr_resolvers
}

fn register_operand_surface_parser(
    map: &mut HashMap<String, OperandSurfaceExprParserFn>,
    family_id: &str,
    parser: OperandSurfaceExprParserFn,
) {
    map.insert(family_id.to_ascii_lowercase(), parser);
}

fn default_operand_surface_parsers() -> HashMap<String, OperandSurfaceExprParserFn> {
    let mut parsers = HashMap::new();
    register_operand_surface_parser(
        &mut parsers,
        "motorola68000",
        families::m68k::parse_runtime_operand_surface_expr,
    );
    parsers
}

#[derive(Debug)]
struct OperandSetInstructionAdapter<'a> {
    cpu_id: &'a str,
    dialect_override: Option<&'a str>,
    mnemonic: &'a str,
    candidates: &'a [VmEncodeCandidate],
}

impl PortableInstructionAdapter for OperandSetInstructionAdapter<'_> {
    fn cpu_id(&self) -> &str {
        self.cpu_id
    }

    fn dialect_override(&self) -> Option<&str> {
        self.dialect_override
    }

    fn mnemonic(&self) -> &str {
        self.mnemonic
    }

    fn vm_encode_candidates(&self) -> &[VmEncodeCandidate] {
        self.candidates
    }
}

/// Runtime view with resolved hierarchy bridge and scoped FORM ownership sets.
#[derive(Debug)]
pub struct HierarchyExecutionModel {
    core: RuntimeModelCore,
    expr_resolvers: HashMap<String, ExprResolverEntry>,
    operand_surface_parsers: HashMap<String, OperandSurfaceExprParserFn>,
    parser_vm_route_cache: Mutex<HashMap<ParserVmRouteCacheKey, Arc<ResolvedParserVmRoute>>>,
    tokenizer_vm_route_cache:
        Mutex<HashMap<TokenizerVmRouteCacheKey, Arc<ResolvedTokenizerVmRoute>>>,
}

impl HierarchyExecutionModel {
    pub(crate) fn from_runtime_model_core(core: RuntimeModelCore) -> Self {
        Self {
            core,
            expr_resolvers: default_expr_resolvers(),
            operand_surface_parsers: default_operand_surface_parsers(),
            parser_vm_route_cache: Mutex::new(HashMap::new()),
            tokenizer_vm_route_cache: Mutex::new(HashMap::new()),
        }
    }

    pub fn from_registry(registry: &ModuleRegistry) -> Result<Self, RuntimeBridgeError> {
        Ok(Self::from_runtime_model_core(
            RuntimeModelCore::from_registry(registry)?,
        ))
    }

    pub fn from_package_bytes(bytes: &[u8]) -> Result<Self, RuntimeBridgeError> {
        Ok(Self::from_runtime_model_core(
            RuntimeModelCore::from_package_bytes(bytes)?,
        ))
    }

    pub fn from_chunks(chunks: HierarchyChunks) -> Result<Self, RuntimeBridgeError> {
        Ok(Self::from_runtime_model_core(
            RuntimeModelCore::from_chunks(chunks)?,
        ))
    }

    pub fn runtime_budget_profile(&self) -> RuntimeBudgetProfile {
        self.core.budget_profile
    }

    pub fn runtime_budget_limits(&self) -> RuntimeBudgetLimits {
        self.core.budget_limits
    }

    pub fn set_runtime_budget_profile(&mut self, profile: RuntimeBudgetProfile) {
        self.core.budget_profile = profile;
        self.core.budget_limits = profile.limits();
    }

    pub fn tokenizer_mode(&self) -> RuntimeTokenizerMode {
        self.core.tokenizer_mode
    }

    pub fn set_tokenizer_mode(&mut self, mode: RuntimeTokenizerMode) {
        self.core.tokenizer_mode = mode;
    }

    #[doc(hidden)]
    pub fn set_runtime_budget_limits_for_tests(&mut self, limits: RuntimeBudgetLimits) {
        self.core.budget_limits = limits;
    }

    pub fn set_active_cpu(&mut self, cpu_id: &str) -> Result<(), RuntimeBridgeError> {
        Ok(self.core.set_active_cpu(cpu_id)?)
    }

    pub fn resolve_pipeline(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<ResolvedHierarchy, RuntimeBridgeError> {
        Ok(self.core.resolve_pipeline(cpu_id, dialect_override)?)
    }

    /// Resolve a package-owned selector choice for one opaque input key.
    pub fn resolve_selector_choice(
        &self,
        resolved: &ResolvedHierarchy,
        input: &str,
    ) -> Result<Option<crate::selector_vm::PortableSelectorOutcome>, RuntimeBridgeError> {
        self.core.resolve_selector_choice(resolved, input)
    }

    /// Materialize the initial values for one package-owned state profile.
    pub fn initial_package_state(
        &self,
        resolved: &ResolvedHierarchy,
        program_id: &str,
        profile: &str,
    ) -> Result<HashMap<String, u32>, crate::state_vm::StateVmError> {
        self.core
            .initial_package_state(resolved, program_id, profile)
    }

    /// Apply one package-owned state transition transactionally.
    pub fn apply_package_state_directive(
        &self,
        resolved: &ResolvedHierarchy,
        program_id: &str,
        profile: &str,
        directive: &str,
        arguments: &[String],
        state: &mut HashMap<String, u32>,
    ) -> Result<crate::state_vm::PortableStateDirectiveOutcome, crate::state_vm::StateVmError> {
        self.core.apply_package_state_directive(
            resolved, program_id, profile, directive, arguments, state,
        )
    }

    /// Query one opaque package-owned capability against the current state.
    pub fn package_capability_allowed(
        &self,
        resolved: &ResolvedHierarchy,
        program_id: &str,
        profile: &str,
        capability: &str,
        state: &HashMap<String, u32>,
    ) -> Result<Option<bool>, crate::state_vm::StateVmError> {
        self.core
            .package_capability_allowed(resolved, program_id, profile, capability, state)
    }

    /// Execute a package-owned, versioned semantic program for a resolved hierarchy.
    pub fn execute_semantic_program(
        &self,
        resolved: &ResolvedHierarchy,
        program_id: &str,
        operands: &[&[u8]],
    ) -> Result<Vec<u8>, RuntimeBridgeError> {
        self.core
            .execute_semantic_program(resolved, program_id, operands)
    }

    /// Materialize one scalar using a package-owned, versioned value program.
    pub fn execute_value_program(
        &self,
        resolved: &ResolvedHierarchy,
        program_id: &str,
        inputs: &[i64],
    ) -> Result<i64, RuntimeBridgeError> {
        self.core
            .execute_value_program(resolved, program_id, inputs)
    }

    /// Reconstruct one neutral operand record from a package-owned program.
    pub fn execute_operand_record_program(
        &self,
        resolved: &ResolvedHierarchy,
        program_id: &str,
        registers: &[crate::operand_record_vm::PortableRegisterRef],
        values: &[i64],
    ) -> Result<crate::operand_record_vm::PortableOperandRecord, RuntimeBridgeError> {
        self.core
            .execute_operand_record_program(resolved, program_id, registers, values)
    }

    /// Reconstruct one record that composes already-materialized nested records.
    pub fn execute_operand_record_program_with_records(
        &self,
        resolved: &ResolvedHierarchy,
        program_id: &str,
        registers: &[crate::operand_record_vm::PortableRegisterRef],
        values: &[i64],
        records: &[crate::operand_record_vm::PortableOperandRecord],
    ) -> Result<crate::operand_record_vm::PortableOperandRecord, RuntimeBridgeError> {
        self.core.execute_operand_record_program_with_records(
            resolved, program_id, registers, values, records,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn parse_family_operand_surface_expr(
        &self,
        family_id: &str,
        tokens: &[Token],
        mnemonic: Option<&str>,
        operand_index: usize,
        end_span: Span,
        end_token_text: Option<&str>,
        parse_expr: &mut OperandSurfaceExprSubparser<'_>,
        parse_wrapped_or_expr: &mut OperandSurfaceExprSubparser<'_>,
    ) -> Result<Option<Expr>, ParseError> {
        let key = family_id.to_ascii_lowercase();
        let Some(parser) = self.operand_surface_parsers.get(key.as_str()) else {
            return Ok(None);
        };
        parser(
            tokens,
            mnemonic,
            operand_index,
            end_span,
            end_token_text,
            parse_expr,
            parse_wrapped_or_expr,
        )
    }

    pub fn register_checker_for_resolved(&self, resolved: &ResolvedHierarchy) -> RegisterChecker {
        self.core.register_checker_for_resolved(resolved)
    }

    pub fn supports_mnemonic(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        mnemonic: &str,
    ) -> Result<bool, RuntimeBridgeError> {
        Ok(self
            .core
            .supports_mnemonic(cpu_id, dialect_override, mnemonic)?)
    }

    pub fn supported_family_ids(&self) -> Vec<String> {
        self.core.supported_family_ids()
    }

    pub fn supported_cpus(&self) -> Vec<(String, String, Option<String>)> {
        self.core.supported_cpus()
    }

    pub fn canonical_cpu_id_for_input(&self, requested: &str) -> Option<String> {
        self.core.canonical_cpu_id_for_input(requested)
    }

    pub fn resolve_token_policy(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<RuntimeTokenPolicy, RuntimeBridgeError> {
        self.core.resolve_token_policy(cpu_id, dialect_override)
    }

    pub fn resolve_parser_contract(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<RuntimeParserContract>, RuntimeBridgeError> {
        self.core.resolve_parser_contract(cpu_id, dialect_override)
    }

    pub fn validate_parser_contract_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        estimated_ast_nodes: usize,
    ) -> Result<RuntimeParserContract, RuntimeBridgeError> {
        self.core.validate_parser_contract_for_assembler(
            cpu_id,
            dialect_override,
            estimated_ast_nodes,
        )
    }

    pub fn resolve_parser_vm_program(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<RuntimeParserVmProgram>, RuntimeBridgeError> {
        self.core
            .resolve_parser_vm_program(cpu_id, dialect_override)
    }

    pub fn resolve_expr_contract(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<RuntimeExprContract>, RuntimeBridgeError> {
        self.core.resolve_expr_contract(cpu_id, dialect_override)
    }

    pub fn resolve_expr_parser_contract(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<RuntimeExprParserContract>, RuntimeBridgeError> {
        self.core
            .resolve_expr_parser_contract(cpu_id, dialect_override)
    }

    pub fn resolve_expr_budgets(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<PortableExprBudgets, RuntimeBridgeError> {
        self.core.resolve_expr_budgets(cpu_id, dialect_override)
    }

    pub fn enforce_parser_vm_program_budget_for_assembler(
        &self,
        parser_contract: &RuntimeParserContract,
        parser_vm_program: &RuntimeParserVmProgram,
    ) -> Result<(), RuntimeBridgeError> {
        self.core
            .enforce_parser_vm_program_budget_for_assembler(parser_contract, parser_vm_program)
    }

    pub(crate) fn resolve_parser_vm_route_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Arc<ResolvedParserVmRoute>, RuntimeBridgeError> {
        let key = ParserVmRouteCacheKey::new(cpu_id, dialect_override);
        if let Some(route) = self
            .parser_vm_route_cache
            .lock()
            .expect("parser VM route cache lock poisoned")
            .get(&key)
        {
            return Ok(Arc::clone(route));
        }

        let parser_contract =
            self.validate_parser_contract_for_assembler(cpu_id, dialect_override, 0)?;
        let parser_vm_program = self
            .resolve_parser_vm_program(cpu_id, dialect_override)?
            .ok_or_else(|| {
                RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic::new(
                    parser_contract.diagnostics.invalid_statement.as_str(),
                    "missing parser VM program for active CPU pipeline",
                    None,
                ))
            })?;
        self.enforce_parser_vm_program_budget_for_assembler(&parser_contract, &parser_vm_program)?;

        let route = Arc::new(ResolvedParserVmRoute::new(
            parser_contract,
            parser_vm_program,
            self.core.budget_limits.max_parser_tokens_per_line,
            self.core.budget_limits.max_parser_ast_nodes_per_line,
        ));
        self.parser_vm_route_cache
            .lock()
            .expect("parser VM route cache lock poisoned")
            .insert(key, Arc::clone(&route));
        Ok(route)
    }

    pub(crate) fn ensure_parser_vm_v2_expr_subcall_contract_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<(), RuntimeBridgeError> {
        self.core
            .ensure_parser_vm_v2_expr_subcall_contract_for_assembler(cpu_id, dialect_override)
    }

    pub fn parse_portable_line_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        line: &str,
        line_num: u32,
    ) -> Result<PortableLineAst, ParseError> {
        crate::vm_opasm_parse::parse_portable_line_for_assembler(
            self,
            cpu_id,
            dialect_override,
            line,
            line_num,
        )
    }

    pub fn resolve_tokenizer_vm_parity_checklist(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<&'static str>, RuntimeBridgeError> {
        self.core
            .resolve_tokenizer_vm_parity_checklist(cpu_id, dialect_override)
    }

    pub fn resolve_expr_parser_vm_parity_checklist(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<&'static str>, RuntimeBridgeError> {
        self.core
            .resolve_expr_parser_vm_parity_checklist(cpu_id, dialect_override)
    }

    pub fn resolve_parser_certification_checklists(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<RuntimeParserCertificationChecklists, RuntimeBridgeError> {
        self.core
            .resolve_parser_certification_checklists(cpu_id, dialect_override)
    }

    pub fn has_declared_diagnostic_code(&self, code: &str) -> bool {
        self.core.has_declared_diagnostic_code(code)
    }

    fn mode_exists_for_owner(
        &self,
        selector: &ModeSelectorDescriptor,
        owner_tag: u8,
        owner_id: u32,
        mnemonic_id: u32,
    ) -> bool {
        let mode_key = selector.mode_key.to_ascii_lowercase();
        let Some(mode_id) = self.interned_id(&mode_key) else {
            return false;
        };
        let key = (owner_tag, owner_id, mnemonic_id, mode_id);
        self.core.vm_programs.contains_key(&key)
    }

    fn diag_message(&self, code: &str, fallback: &str, args: &[(&str, &str)]) -> String {
        self.core.diag_message(code, fallback, args)
    }

    fn invalid_force_error(&self, force: SelectorOperandForce, context: &str) -> String {
        let force_token = force_suffix(force);
        let fallback = format!(
            "Explicit addressing override ',{}' is not valid for {}",
            force_token, context
        );
        self.diag_message(
            DIAG_OPTHREAD_INVALID_FORCE_OVERRIDE,
            fallback.as_str(),
            &[("force", force_token), ("context", context)],
        )
    }

    fn non_m65816_force_error(&self, cpu_id: &str) -> String {
        if cpu_id.eq_ignore_ascii_case("65c02") {
            let fallback = "65816-only addressing mode not supported on 65C02";
            self.diag_message(DIAG_OPTHREAD_FORCE_UNSUPPORTED_65C02, fallback, &[])
        } else {
            let fallback = "65816-only addressing mode not supported on base 6502";
            self.diag_message(DIAG_OPTHREAD_FORCE_UNSUPPORTED_6502, fallback, &[])
        }
    }
}

fn force_suffix(force: SelectorOperandForce) -> &'static str {
    match force {
        SelectorOperandForce::DirectPage => "d",
        SelectorOperandForce::DataBank => "b",
        SelectorOperandForce::ProgramBank => "k",
        SelectorOperandForce::Long => "l",
    }
}
