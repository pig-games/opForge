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

use families::mos6502::OperandForce;
use opcore::expr_vm::PortableExprBudgets;
use opcore::parser::{Expr, ParseError};
use package::{
    HierarchyChunks, ModeSelectorDescriptor, DIAG_OPTHREAD_FORCE_UNSUPPORTED_6502,
    DIAG_OPTHREAD_FORCE_UNSUPPORTED_65C02, DIAG_OPTHREAD_INVALID_FORCE_OVERRIDE,
    DIAG_OPTHREAD_MISSING_VM_PROGRAM,
};
use registry::family::AssemblerContext;
use registry::registry::{ModuleRegistry, OperandSet, VmEncodeCandidate};
use types::hierarchy::ResolvedHierarchy;

use crate::portable_contract::{PortableLineAst, PortableToken};
use crate::runtime_contract_types::{
    RuntimeExprContract, RuntimeExprParserContract, RuntimeParserCertificationChecklists,
};
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

pub use selector_bridge::{intel8080_candidate_from_resolved, intel8080_ld_indirect_candidate};
pub use tokenizer_bridge::apply_token_policy_to_token;

thread_local! {
    pub static CORE_EXPR_PARSER_FAILPOINT: Cell<bool> = const { Cell::new(false) };
}

pub fn set_core_expr_parser_failpoint_for_tests(enabled: bool) {
    CORE_EXPR_PARSER_FAILPOINT.with(|flag| flag.set(enabled));
}

/// Family-keyed operand parse/resolve adapter used by expr-based runtime encode.
pub type ExprResolverFn = fn(
    &HierarchyExecutionModel,
    &ResolvedHierarchy,
    &str,
    &[Expr],
    &dyn AssemblerContext,
) -> Result<Option<Vec<VmEncodeCandidate>>, RuntimeBridgeError>;

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
}

impl HierarchyExecutionModel {
    pub(crate) fn from_runtime_model_core(core: RuntimeModelCore) -> Self {
        Self {
            core,
            expr_resolvers: default_expr_resolvers(),
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

    fn invalid_force_error(&self, force: OperandForce, context: &str) -> String {
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

fn force_suffix(force: OperandForce) -> &'static str {
    match force {
        OperandForce::DirectPage => "d",
        OperandForce::DataBank => "b",
        OperandForce::ProgramBank => "k",
        OperandForce::Long => "l",
    }
}
