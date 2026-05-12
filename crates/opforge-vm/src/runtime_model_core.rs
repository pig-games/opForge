// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Shared runtime model state and package/chunk loading.

use std::collections::{HashMap, HashSet};

use opcore::expr_vm::PortableExprBudgets;
use opcore::tokenizer::Tokenizer;
use package::{
    decode_hierarchy_chunks, HierarchyChunks, ModeSelectorDescriptor, OpcpuCodecError,
    TokenCaseRule, TokenizerVmDiagnosticMap, TokenizerVmLimits, TokenizerVmOpcode,
    TokenizerVmStreamMode, DIAG_PARSER_OPASM_V2_SUBCALL_VERSION_MISMATCH,
    DIAG_PARSER_OPASM_V2_UNKNOWN_SUBCALL_CONTRACT, EXPR_VM_OPCODE_VERSION_V1,
    EXPR_VM_OPCODE_VERSION_V2, EXVM_OPCODE_VERSION_V1, PARSER_AST_SCHEMA_ID_LINE_V1,
    PARSER_GRAMMAR_ID_LINE_V1, PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
    TOKENIZER_VM_OPCODE_VERSION_V1, TOKENIZER_VM_STREAM_VERSION_V1,
};
use registry::registry::ModuleRegistry;
use registry::registry::VmEncodeCandidate;
use types::hierarchy::{HierarchyError, HierarchyPackage, ResolvedHierarchy, ScopedOwner};

use crate::builder::{build_hierarchy_package_from_registry, HierarchyBuildError};
use crate::bytecode::execute_program;
use crate::portable_contract::PortableToken;
use crate::rollout::{
    family_expr_parser_rollout_policy, parser_certification_checklists_for_family,
};
use crate::runtime_bridge::{HierarchyRuntimeBridge, HierarchyRuntimeBridgeError};
use crate::runtime_contract_types::{
    RuntimeExprContract, RuntimeExprDiagnosticMap, RuntimeExprParserContract,
    RuntimeExprParserDiagnosticMap, RuntimeParserCertificationChecklists,
};
use crate::runtime_diagnostics::RuntimeBridgeDiagnostic;
use crate::runtime_error::RuntimeBridgeError;
use crate::runtime_model_types::{
    RuntimeBudgetLimits, RuntimeBudgetProfile, RuntimeParserContract, RuntimeParserDiagnosticMap,
    RuntimeParserVmProgram, RuntimeTokenPolicy, RuntimeTokenizerMode, RuntimeTokenizerVmProgram,
};
use crate::runtime_portable_types::PortableTokenizeRequest;
use crate::tokenizer_runtime_utils::{
    self, AsciiCaseRule, TokenizerDiagCodes, VmTokenizerInputStream,
};

pub type VmProgramKey = (u8, u32, u32, u32);
pub type ModeSelectorKey = (u8, u32, u32, u32);
pub type TokenPolicyKey = (u8, u32);
pub type ParserContractKey = (u8, u32);
pub type ParserVmProgramKey = (u8, u32);
pub type ExprContractKey = (u8, u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeModelLoadError {
    Build(HierarchyBuildError),
    Package(OpcpuCodecError),
    Hierarchy(HierarchyError),
}

impl std::fmt::Display for RuntimeModelLoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Build(err) => write!(f, "runtime model build error: {}", err),
            Self::Package(err) => write!(f, "runtime package error: {}", err),
            Self::Hierarchy(err) => write!(f, "hierarchy resolution error: {}", err),
        }
    }
}

impl std::error::Error for RuntimeModelLoadError {}

impl From<HierarchyBuildError> for RuntimeModelLoadError {
    fn from(value: HierarchyBuildError) -> Self {
        Self::Build(value)
    }
}

impl From<OpcpuCodecError> for RuntimeModelLoadError {
    fn from(value: OpcpuCodecError) -> Self {
        Self::Package(value)
    }
}

impl From<HierarchyError> for RuntimeModelLoadError {
    fn from(value: HierarchyError) -> Self {
        Self::Hierarchy(value)
    }
}

#[derive(Debug)]
pub struct RuntimeModelCore {
    pub bridge: HierarchyRuntimeBridge,
    pub family_forms: HashMap<String, HashSet<String>>,
    pub cpu_forms: HashMap<String, HashSet<String>>,
    pub dialect_forms: HashMap<String, HashSet<String>>,
    pub vm_programs: HashMap<VmProgramKey, Vec<u8>>,
    pub mode_selectors: HashMap<ModeSelectorKey, Vec<ModeSelectorDescriptor>>,
    pub token_policies: HashMap<TokenPolicyKey, RuntimeTokenPolicy>,
    pub tokenizer_vm_programs: HashMap<TokenPolicyKey, RuntimeTokenizerVmProgram>,
    pub parser_contracts: HashMap<ParserContractKey, RuntimeParserContract>,
    pub parser_vm_programs: HashMap<ParserVmProgramKey, RuntimeParserVmProgram>,
    pub expr_contracts: HashMap<ExprContractKey, RuntimeExprContract>,
    pub expr_parser_contracts: HashMap<ParserContractKey, RuntimeExprParserContract>,
    pub interned_ids: HashMap<String, u32>,
    pub selector_gate_only_expr_runtime_cpus: HashSet<String>,
    pub diag_templates: HashMap<String, String>,
    pub tokenizer_mode: RuntimeTokenizerMode,
    pub budget_profile: RuntimeBudgetProfile,
    pub budget_limits: RuntimeBudgetLimits,
}

impl RuntimeModelCore {
    pub fn from_registry(registry: &ModuleRegistry) -> Result<Self, RuntimeModelLoadError> {
        let package_bytes = build_hierarchy_package_from_registry(registry)?;
        Self::from_package_bytes(package_bytes.as_slice())
    }

    pub fn from_package_bytes(bytes: &[u8]) -> Result<Self, RuntimeModelLoadError> {
        let chunks = decode_hierarchy_chunks(bytes)?;
        Self::from_chunks(chunks)
    }

    pub fn from_chunks(chunks: HierarchyChunks) -> Result<Self, RuntimeModelLoadError> {
        let HierarchyChunks {
            metadata: _,
            strings: _,
            diagnostics,
            token_policies,
            tokenizer_vm_programs,
            parser_contracts,
            parser_vm_programs,
            expr_contracts,
            expr_parser_contracts,
            families,
            cpus,
            dialects,
            registers: _,
            forms,
            tables,
            selectors,
        } = chunks;
        let package = HierarchyPackage::new(families, cpus, dialects)?;
        let mut interner = LowercaseIdInterner::default();
        let mut vm_programs = HashMap::new();
        for entry in tables {
            let (owner_tag, owner_id) = owner_key_parts(&entry.owner);
            let owner_id = interner.intern(owner_id.as_str());
            let mnemonic_id = interner.intern(entry.mnemonic.as_str());
            let mode_id = interner.intern(entry.mode_key.as_str());
            vm_programs.insert((owner_tag, owner_id, mnemonic_id, mode_id), entry.program);
        }
        let mut mode_selectors: HashMap<ModeSelectorKey, Vec<ModeSelectorDescriptor>> =
            HashMap::new();
        let mut selector_gate_only_expr_runtime_cpus: HashSet<String> = HashSet::new();
        for entry in selectors {
            if matches!(entry.owner, ScopedOwner::Cpu(_)) && entry.shape_key.contains("force_") {
                let (_, owner_id) = owner_key_parts(&entry.owner);
                selector_gate_only_expr_runtime_cpus.insert(owner_id.to_ascii_lowercase());
            }
            let (owner_tag, owner_id) = owner_key_parts(&entry.owner);
            let owner_id = interner.intern(owner_id.as_str());
            let mnemonic_id = interner.intern(entry.mnemonic.as_str());
            let shape_id = interner.intern(entry.shape_key.as_str());
            mode_selectors
                .entry((owner_tag, owner_id, mnemonic_id, shape_id))
                .or_default()
                .push(entry);
        }
        for entries in mode_selectors.values_mut() {
            entries.sort_by_key(|entry| (entry.priority, entry.width_rank, entry.mode_key.clone()));
        }

        let mut scoped_token_policies = HashMap::new();
        for entry in token_policies {
            let (owner_tag, owner_id) = owner_key_parts(&entry.owner);
            let owner_id = interner.intern(owner_id.as_str());
            scoped_token_policies.insert(
                (owner_tag, owner_id),
                RuntimeTokenPolicy {
                    case_rule: entry.case_rule,
                    identifier_start_class: entry.identifier_start_class,
                    identifier_continue_class: entry.identifier_continue_class,
                    punctuation_chars: entry.punctuation_chars,
                    comment_prefix: entry.comment_prefix,
                    quote_chars: entry.quote_chars,
                    escape_char: entry.escape_char,
                    number_prefix_chars: entry.number_prefix_chars,
                    number_suffix_binary: entry.number_suffix_binary,
                    number_suffix_octal: entry.number_suffix_octal,
                    number_suffix_decimal: entry.number_suffix_decimal,
                    number_suffix_hex: entry.number_suffix_hex,
                    operator_chars: entry.operator_chars,
                    multi_char_operators: entry.multi_char_operators,
                },
            );
        }

        let mut scoped_tokenizer_vm_programs = HashMap::new();
        for entry in tokenizer_vm_programs {
            let (owner_tag, owner_id) = owner_key_parts(&entry.owner);
            let owner_id = interner.intern(owner_id.as_str());
            scoped_tokenizer_vm_programs.insert(
                (owner_tag, owner_id),
                RuntimeTokenizerVmProgram {
                    opcode_version: entry.opcode_version,
                    start_state: entry.start_state,
                    state_entry_offsets: entry.state_entry_offsets,
                    stream: entry.stream,
                    limits: entry.limits,
                    diagnostics: entry.diagnostics,
                    program: entry.program,
                },
            );
        }

        let mut scoped_parser_contracts = HashMap::new();
        for entry in parser_contracts {
            let (owner_tag, owner_id) = owner_key_parts(&entry.owner);
            let owner_id = interner.intern(owner_id.as_str());
            scoped_parser_contracts.insert(
                (owner_tag, owner_id),
                RuntimeParserContract {
                    grammar_id: entry.grammar_id,
                    ast_schema_id: entry.ast_schema_id,
                    opcode_version: entry.opcode_version,
                    max_ast_nodes_per_line: entry.max_ast_nodes_per_line,
                    diagnostics: RuntimeParserDiagnosticMap {
                        unexpected_token: entry.diagnostics.unexpected_token,
                        expected_expression: entry.diagnostics.expected_expression,
                        expected_operand: entry.diagnostics.expected_operand,
                        invalid_statement: entry.diagnostics.invalid_statement,
                    },
                },
            );
        }

        let mut scoped_parser_vm_programs = HashMap::new();
        for entry in parser_vm_programs {
            let (owner_tag, owner_id) = owner_key_parts(&entry.owner);
            let owner_id = interner.intern(owner_id.as_str());
            scoped_parser_vm_programs.insert(
                (owner_tag, owner_id),
                RuntimeParserVmProgram {
                    opcode_version: entry.opcode_version,
                    program: entry.program,
                },
            );
        }

        let mut scoped_expr_contracts = HashMap::new();
        for entry in expr_contracts {
            let (owner_tag, owner_id) = owner_key_parts(&entry.owner);
            let owner_id = interner.intern(owner_id.as_str());
            scoped_expr_contracts.insert(
                (owner_tag, owner_id),
                RuntimeExprContract {
                    opcode_version: entry.opcode_version,
                    max_program_bytes: entry.max_program_bytes,
                    max_stack_depth: entry.max_stack_depth,
                    max_symbol_refs: entry.max_symbol_refs,
                    max_eval_steps: entry.max_eval_steps,
                    diagnostics: RuntimeExprDiagnosticMap {
                        invalid_opcode: entry.diagnostics.invalid_opcode,
                        stack_underflow: entry.diagnostics.stack_underflow,
                        stack_depth_exceeded: entry.diagnostics.stack_depth_exceeded,
                        unknown_symbol: entry.diagnostics.unknown_symbol,
                        eval_failure: entry.diagnostics.eval_failure,
                        unsupported_feature: entry.diagnostics.unsupported_feature,
                        budget_exceeded: entry.diagnostics.budget_exceeded,
                        invalid_program: entry.diagnostics.invalid_program,
                    },
                },
            );
        }

        let mut scoped_expr_parser_contracts = HashMap::new();
        for entry in expr_parser_contracts {
            let (owner_tag, owner_id) = owner_key_parts(&entry.owner);
            let owner_id = interner.intern(owner_id.as_str());
            scoped_expr_parser_contracts.insert(
                (owner_tag, owner_id),
                RuntimeExprParserContract {
                    opcode_version: entry.opcode_version,
                    diagnostics: RuntimeExprParserDiagnosticMap {
                        invalid_expression_program: entry.diagnostics.invalid_expression_program,
                    },
                },
            );
        }

        let mut family_forms: HashMap<String, HashSet<String>> = HashMap::new();
        let mut cpu_forms: HashMap<String, HashSet<String>> = HashMap::new();
        let mut dialect_forms: HashMap<String, HashSet<String>> = HashMap::new();
        for form in forms {
            let mnemonic = form.mnemonic.to_ascii_lowercase();
            match form.owner {
                ScopedOwner::Family(owner) => {
                    family_forms
                        .entry(owner.to_ascii_lowercase())
                        .or_default()
                        .insert(mnemonic);
                }
                ScopedOwner::Cpu(owner) => {
                    cpu_forms
                        .entry(owner.to_ascii_lowercase())
                        .or_default()
                        .insert(mnemonic);
                }
                ScopedOwner::Dialect(owner) => {
                    dialect_forms
                        .entry(owner.to_ascii_lowercase())
                        .or_default()
                        .insert(mnemonic);
                }
            }
        }

        let mut diag_templates = HashMap::new();
        for entry in diagnostics {
            diag_templates.insert(
                entry.code.to_ascii_lowercase(),
                entry.message_template.to_string(),
            );
        }

        Ok(Self {
            bridge: HierarchyRuntimeBridge::new(package),
            family_forms,
            cpu_forms,
            dialect_forms,
            vm_programs,
            mode_selectors,
            token_policies: scoped_token_policies,
            tokenizer_vm_programs: scoped_tokenizer_vm_programs,
            parser_contracts: scoped_parser_contracts,
            parser_vm_programs: scoped_parser_vm_programs,
            expr_contracts: scoped_expr_contracts,
            expr_parser_contracts: scoped_expr_parser_contracts,
            interned_ids: interner.into_ids(),
            selector_gate_only_expr_runtime_cpus,
            diag_templates,
            tokenizer_mode: RuntimeTokenizerMode::Auto,
            budget_profile: RuntimeBudgetProfile::HostDefault,
            budget_limits: RuntimeBudgetProfile::HostDefault.limits(),
        })
    }

    pub fn package(&self) -> &HierarchyPackage {
        self.bridge.package()
    }

    pub fn set_active_cpu(&mut self, cpu_id: &str) -> Result<(), HierarchyRuntimeBridgeError> {
        self.bridge.set_active_cpu(cpu_id)
    }

    pub fn resolve_pipeline(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<ResolvedHierarchy, HierarchyRuntimeBridgeError> {
        self.bridge.resolve_pipeline(cpu_id, dialect_override)
    }

    pub fn supports_mnemonic(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        mnemonic: &str,
    ) -> Result<bool, HierarchyRuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        let needle = mnemonic.to_ascii_lowercase();

        if contains_form(&self.dialect_forms, &resolved.dialect_id, &needle) {
            return Ok(true);
        }
        if contains_form(&self.cpu_forms, &resolved.cpu_id, &needle) {
            return Ok(true);
        }
        Ok(contains_form(
            &self.family_forms,
            &resolved.family_id,
            &needle,
        ))
    }

    pub fn supported_family_ids(&self) -> Vec<String> {
        self.package().family_ids()
    }

    pub fn supported_cpus(&self) -> Vec<(String, String, Option<String>)> {
        self.package()
            .cpu_descriptors()
            .into_iter()
            .map(|cpu| (cpu.id, cpu.family_id, cpu.default_dialect))
            .collect()
    }

    pub fn canonical_cpu_id_for_input(&self, requested: &str) -> Option<String> {
        let requested = requested.trim();
        if requested.is_empty() {
            return None;
        }

        let cpus = self.package().cpu_descriptors();
        if let Some(found) = cpus
            .iter()
            .find(|cpu| cpu.id.eq_ignore_ascii_case(requested))
            .map(|cpu| cpu.id.clone())
        {
            return Some(found);
        }

        let alias_target = match requested.to_ascii_lowercase().as_str() {
            "8080" => Some("8085"),
            "6502" => Some("m6502"),
            "65c816" => Some("65816"),
            "mega65" => Some("45gs02"),
            "6809" => Some("m6809"),
            "6309" => Some("hd6309"),
            _ => None,
        }?;

        cpus.iter()
            .find(|cpu| cpu.id.eq_ignore_ascii_case(alias_target))
            .map(|cpu| cpu.id.clone())
    }

    pub fn token_policy_for_resolved(&self, resolved: &ResolvedHierarchy) -> RuntimeTokenPolicy {
        self.lookup_scoped(&self.token_policies, resolved)
            .cloned()
            .unwrap_or_default()
    }

    pub fn resolve_token_policy(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<RuntimeTokenPolicy, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        Ok(self.token_policy_for_resolved(&resolved))
    }

    pub fn tokenizer_vm_program_for_resolved(
        &self,
        resolved: &ResolvedHierarchy,
    ) -> Option<&RuntimeTokenizerVmProgram> {
        self.lookup_scoped(&self.tokenizer_vm_programs, resolved)
    }

    pub fn resolve_tokenizer_vm_program(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<RuntimeTokenizerVmProgram>, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        Ok(self.tokenizer_vm_program_for_resolved(&resolved).cloned())
    }

    pub fn resolve_tokenizer_vm_limits(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<TokenizerVmLimits, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        Ok(self
            .tokenizer_vm_program_for_resolved(&resolved)
            .map(|entry| entry.limits)
            .unwrap_or_default())
    }

    pub fn ensure_tokenizer_vm_program_compatible_for_assembler(
        &self,
        vm_program: &RuntimeTokenizerVmProgram,
    ) -> Result<(), RuntimeBridgeError> {
        let error_code = tokenizer_vm_error_code(vm_program);
        if vm_program.opcode_version != TOKENIZER_VM_OPCODE_VERSION_V1 {
            return Err(RuntimeBridgeError::Resolve(format!(
                "{}: unsupported tokenizer VM opcode version {}",
                error_code, vm_program.opcode_version
            )));
        }
        if vm_program.stream.version != TOKENIZER_VM_STREAM_VERSION_V1 {
            return Err(RuntimeBridgeError::Resolve(format!(
                "{}: unsupported tokenizer VM stream version {}",
                error_code, vm_program.stream.version
            )));
        }
        if vm_program.stream.mode != TokenizerVmStreamMode::LineInputBytes {
            return Err(RuntimeBridgeError::Resolve(format!(
                "{}: unsupported tokenizer VM stream mode {:?}",
                error_code, vm_program.stream.mode
            )));
        }
        for (field_name, value) in [
            ("invalid_char", vm_program.diagnostics.invalid_char.as_str()),
            (
                "unterminated_string",
                vm_program.diagnostics.unterminated_string.as_str(),
            ),
            (
                "step_limit_exceeded",
                vm_program.diagnostics.step_limit_exceeded.as_str(),
            ),
            (
                "token_limit_exceeded",
                vm_program.diagnostics.token_limit_exceeded.as_str(),
            ),
            (
                "lexeme_limit_exceeded",
                vm_program.diagnostics.lexeme_limit_exceeded.as_str(),
            ),
            (
                "error_limit_exceeded",
                vm_program.diagnostics.error_limit_exceeded.as_str(),
            ),
        ] {
            if value.trim().is_empty() {
                return Err(RuntimeBridgeError::Resolve(format!(
                    "{}: missing tokenizer VM diagnostic mapping for '{}'",
                    error_code, field_name
                )));
            }
            self.ensure_diag_code_declared_in_package_catalog(error_code, "tokenizer VM", value)?;
        }
        Ok(())
    }

    pub fn encode_candidates(
        &self,
        resolved: &ResolvedHierarchy,
        mnemonic: &str,
        candidates: &[VmEncodeCandidate],
    ) -> Result<Option<Vec<u8>>, RuntimeBridgeError> {
        let normalized_mnemonic = mnemonic.to_ascii_lowercase();
        let Some(mnemonic_id) = self.interned_id(&normalized_mnemonic) else {
            return Ok(None);
        };
        let owner_order = self.scoped_owner_lookup_order(resolved);

        for candidate in candidates {
            let mode_key = candidate.mode_key.to_ascii_lowercase();
            let Some(mode_id) = self.interned_id(&mode_key) else {
                continue;
            };
            let operand_views: Vec<&[u8]> =
                candidate.operand_bytes.iter().map(Vec::as_slice).collect();
            for (owner_tag, owner_id) in owner_order {
                let Some(owner_id) = owner_id else {
                    continue;
                };
                let key = (owner_tag, owner_id, mnemonic_id, mode_id);
                if let Some(program) = self.vm_programs.get(&key) {
                    self.enforce_vm_program_budget(program.len())?;
                    return execute_program(program, operand_views.as_slice())
                        .map(Some)
                        .map_err(Into::into);
                }
            }
        }
        Ok(None)
    }

    pub fn enforce_candidate_budget(
        &self,
        candidates: &[VmEncodeCandidate],
    ) -> Result<(), RuntimeBridgeError> {
        if candidates.len() > self.budget_limits.max_candidate_count {
            return Err(Self::budget_error(
                "candidate_count",
                self.budget_limits.max_candidate_count,
                candidates.len(),
            ));
        }
        for candidate in candidates {
            if candidate.operand_bytes.len() > self.budget_limits.max_operand_count_per_candidate {
                return Err(Self::budget_error(
                    "operand_count_per_candidate",
                    self.budget_limits.max_operand_count_per_candidate,
                    candidate.operand_bytes.len(),
                ));
            }
            for operand_bytes in &candidate.operand_bytes {
                if operand_bytes.len() > self.budget_limits.max_operand_bytes_per_operand {
                    return Err(Self::budget_error(
                        "operand_bytes_per_operand",
                        self.budget_limits.max_operand_bytes_per_operand,
                        operand_bytes.len(),
                    ));
                }
            }
        }
        Ok(())
    }

    pub fn enforce_vm_program_budget(&self, program_len: usize) -> Result<(), RuntimeBridgeError> {
        if program_len > self.budget_limits.max_vm_program_bytes {
            return Err(Self::budget_error(
                "vm_program_bytes",
                self.budget_limits.max_vm_program_bytes,
                program_len,
            ));
        }
        Ok(())
    }

    pub fn budget_error(name: &str, limit: usize, observed: usize) -> RuntimeBridgeError {
        RuntimeBridgeError::Resolve(format!(
            "VM runtime budget exceeded ({name}): observed {observed}, limit {limit}"
        ))
    }

    pub fn tokenize_with_vm_core(
        &self,
        request: &PortableTokenizeRequest<'_>,
        vm_program: &RuntimeTokenizerVmProgram,
    ) -> Result<Vec<PortableToken>, RuntimeBridgeError> {
        self.ensure_tokenizer_vm_program_compatible_for_assembler(vm_program)?;
        if vm_program.state_entry_offsets.is_empty() {
            return Err(RuntimeBridgeError::Resolve(format!(
                "{}: tokenizer VM state table is empty",
                vm_program.diagnostics.invalid_char
            )));
        }
        let start_state = usize::from(vm_program.start_state);
        let Some(start_offset) = vm_program.state_entry_offsets.get(start_state).copied() else {
            return Err(RuntimeBridgeError::Resolve(format!(
                "{}: tokenizer VM start state {} out of range",
                vm_program.diagnostics.invalid_char, vm_program.start_state
            )));
        };

        if request.source_stream.contract != vm_program.stream {
            return Err(RuntimeBridgeError::Resolve(format!(
                "{}: tokenizer VM request stream contract {:?} does not match program contract {:?}",
                vm_program.diagnostics.invalid_char,
                request.source_stream.contract,
                vm_program.stream
            )));
        }
        if request.source_stream.bytes != request.source_line.as_bytes() {
            return Err(RuntimeBridgeError::Resolve(format!(
                "{}: tokenizer VM request source bytes do not match source line",
                vm_program.diagnostics.invalid_char
            )));
        }

        let bytes = request.source_stream.bytes;
        let max_steps_per_line = vm_program
            .limits
            .max_steps_per_line
            .min(self.budget_limits.max_tokenizer_steps_per_line);
        let max_tokens_per_line = vm_program
            .limits
            .max_tokens_per_line
            .min(self.budget_limits.max_tokenizer_tokens_per_line);
        let max_lexeme_bytes = vm_program
            .limits
            .max_lexeme_bytes
            .min(self.budget_limits.max_tokenizer_lexeme_bytes);
        let max_errors_per_line = vm_program
            .limits
            .max_errors_per_line
            .min(self.budget_limits.max_tokenizer_errors_per_line);
        let max_lexeme_bytes_usize = usize::try_from(max_lexeme_bytes).unwrap_or(usize::MAX);
        let max_tokens_per_line_usize = usize::try_from(max_tokens_per_line).unwrap_or(usize::MAX);
        let mut stream = VmTokenizerInputStream::new(bytes, vm_program.stream.mode);
        let lexeme_capacity = max_lexeme_bytes_usize.min(stream.len());
        let token_capacity = max_tokens_per_line_usize.min(stream.len().saturating_add(1));
        let mut pc = vm_offset_to_pc(
            vm_program.program.as_slice(),
            start_offset,
            vm_program.diagnostics.invalid_char.as_str(),
            "start state offset",
        )?;
        let mut current_byte: Option<u8> = None;
        let mut lexeme = Vec::with_capacity(lexeme_capacity);
        let mut lexeme_start = 0usize;
        let mut lexeme_end = 0usize;
        let mut tokens = Vec::with_capacity(token_capacity);
        let mut emitted_errors = 0u32;
        let mut step_count = 0u32;
        let mut core_tokenizer: Option<Tokenizer<'_>> = None;
        let mut push_token = |token: PortableToken| -> Result<(), RuntimeBridgeError> {
            if tokens.len() >= max_tokens_per_line_usize {
                return Err(RuntimeBridgeError::Resolve(format!(
                    "{}: tokenizer VM token budget exceeded ({}/{})",
                    vm_program.diagnostics.token_limit_exceeded,
                    tokens.len().saturating_add(1),
                    max_tokens_per_line
                )));
            }
            let lexeme_len = token.span.col_end.saturating_sub(token.span.col_start);
            if lexeme_len > max_lexeme_bytes_usize {
                return Err(RuntimeBridgeError::Resolve(format!(
                    "{}: tokenizer VM lexeme budget exceeded ({}/{})",
                    vm_program.diagnostics.lexeme_limit_exceeded, lexeme_len, max_lexeme_bytes
                )));
            }
            tokens.push(apply_token_policy_to_token(token, &request.token_policy));
            Ok(())
        };

        loop {
            step_count = step_count.saturating_add(1);
            if step_count > max_steps_per_line {
                return Err(RuntimeBridgeError::Resolve(format!(
                    "{}: tokenizer VM step budget exceeded ({}/{})",
                    vm_program.diagnostics.step_limit_exceeded, step_count, max_steps_per_line
                )));
            }

            let opcode_byte = vm_read_u8(
                vm_program.program.as_slice(),
                &mut pc,
                vm_program.diagnostics.invalid_char.as_str(),
                "opcode",
            )?;
            let Some(opcode) = TokenizerVmOpcode::from_u8(opcode_byte) else {
                return Err(RuntimeBridgeError::Resolve(format!(
                    "{}: unknown tokenizer VM opcode 0x{:02X}",
                    vm_program.diagnostics.invalid_char, opcode_byte
                )));
            };

            match opcode {
                TokenizerVmOpcode::End => break,
                TokenizerVmOpcode::ReadChar => {
                    current_byte = stream.current_byte();
                }
                TokenizerVmOpcode::Advance => {
                    stream.advance();
                }
                TokenizerVmOpcode::StartLexeme => {
                    lexeme.clear();
                    lexeme_start = stream.cursor();
                    lexeme_end = stream.cursor();
                }
                TokenizerVmOpcode::PushChar => {
                    let Some(byte) = current_byte else {
                        return Err(RuntimeBridgeError::Resolve(format!(
                            "{}: PushChar requires ReadChar at non-EOL",
                            vm_program.diagnostics.invalid_char
                        )));
                    };
                    if lexeme.len() >= max_lexeme_bytes_usize {
                        return Err(RuntimeBridgeError::Resolve(format!(
                            "{}: tokenizer VM lexeme budget exceeded ({}/{})",
                            vm_program.diagnostics.lexeme_limit_exceeded,
                            lexeme.len().saturating_add(1),
                            max_lexeme_bytes
                        )));
                    }
                    lexeme.push(byte);
                    lexeme_end = stream.cursor().saturating_add(1);
                }
                TokenizerVmOpcode::EmitToken => {
                    let token_kind = vm_read_u8(
                        vm_program.program.as_slice(),
                        &mut pc,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "emit token kind",
                    )?;
                    let token = vm_build_token(
                        token_kind,
                        lexeme.as_slice(),
                        request.line_num,
                        lexeme_start,
                        lexeme_end,
                        stream.cursor(),
                    )?;
                    push_token(token)?;
                }
                TokenizerVmOpcode::SetState => {
                    let state = usize::from(vm_read_u16(
                        vm_program.program.as_slice(),
                        &mut pc,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "state index",
                    )?);
                    let Some(offset) = vm_program.state_entry_offsets.get(state).copied() else {
                        return Err(RuntimeBridgeError::Resolve(format!(
                            "{}: state index {} out of range",
                            vm_program.diagnostics.invalid_char, state
                        )));
                    };
                    pc = vm_offset_to_pc(
                        vm_program.program.as_slice(),
                        offset,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "state entry offset",
                    )?;
                }
                TokenizerVmOpcode::Jump => {
                    let target = vm_read_u32(
                        vm_program.program.as_slice(),
                        &mut pc,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "jump target",
                    )?;
                    pc = vm_offset_to_pc(
                        vm_program.program.as_slice(),
                        target,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "jump target",
                    )?;
                }
                TokenizerVmOpcode::JumpIfEol => {
                    let target = vm_read_u32(
                        vm_program.program.as_slice(),
                        &mut pc,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "conditional jump target",
                    )?;
                    if stream.is_eol() {
                        pc = vm_offset_to_pc(
                            vm_program.program.as_slice(),
                            target,
                            vm_program.diagnostics.invalid_char.as_str(),
                            "conditional jump target",
                        )?;
                    }
                }
                TokenizerVmOpcode::JumpIfByteEq => {
                    let expected = vm_read_u8(
                        vm_program.program.as_slice(),
                        &mut pc,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "expected byte",
                    )?;
                    let target = vm_read_u32(
                        vm_program.program.as_slice(),
                        &mut pc,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "conditional jump target",
                    )?;
                    if current_byte.is_some_and(|byte| byte == expected) {
                        pc = vm_offset_to_pc(
                            vm_program.program.as_slice(),
                            target,
                            vm_program.diagnostics.invalid_char.as_str(),
                            "conditional jump target",
                        )?;
                    }
                }
                TokenizerVmOpcode::JumpIfClass => {
                    let class = vm_read_u8(
                        vm_program.program.as_slice(),
                        &mut pc,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "character class",
                    )?;
                    let target = vm_read_u32(
                        vm_program.program.as_slice(),
                        &mut pc,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "conditional jump target",
                    )?;
                    if vm_char_class_matches(current_byte, class, &request.token_policy) {
                        pc = vm_offset_to_pc(
                            vm_program.program.as_slice(),
                            target,
                            vm_program.diagnostics.invalid_char.as_str(),
                            "conditional jump target",
                        )?;
                    }
                }
                TokenizerVmOpcode::Fail => {
                    let reason = vm_read_u8(
                        vm_program.program.as_slice(),
                        &mut pc,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "failure reason",
                    )?;
                    return Err(RuntimeBridgeError::Resolve(format!(
                        "{}: tokenizer VM failure reason {}",
                        vm_program.diagnostics.invalid_char, reason
                    )));
                }
                TokenizerVmOpcode::EmitDiag => {
                    let slot = vm_read_u8(
                        vm_program.program.as_slice(),
                        &mut pc,
                        vm_program.diagnostics.invalid_char.as_str(),
                        "diagnostic slot",
                    )?;
                    emitted_errors = emitted_errors.saturating_add(1);
                    if emitted_errors > max_errors_per_line {
                        return Err(RuntimeBridgeError::Resolve(format!(
                            "{}: tokenizer VM diagnostic budget exceeded ({}/{})",
                            vm_program.diagnostics.error_limit_exceeded,
                            emitted_errors,
                            max_errors_per_line
                        )));
                    }
                    let code = vm_diag_code_for_slot(&vm_program.diagnostics, slot);
                    return Err(RuntimeBridgeError::Resolve(format!(
                        "{}: tokenizer VM emitted diagnostic slot {}",
                        code, slot
                    )));
                }
                TokenizerVmOpcode::DelegateCore => {
                    return Err(RuntimeBridgeError::Resolve(format!(
                        "{}: tokenizer VM DelegateCore opcode is forbidden in VM tokenizer execution mode",
                        vm_program.diagnostics.invalid_char
                    )));
                }
                TokenizerVmOpcode::ScanCoreToken => {
                    match vm_scan_next_core_token(request, stream.cursor(), &mut core_tokenizer)? {
                        Some((portable, next_cursor)) => {
                            push_token(portable)?;
                            stream.set_cursor(next_cursor);
                            current_byte = stream.current_byte();
                        }
                        None => {
                            stream.set_cursor(stream.len());
                            current_byte = None;
                        }
                    }
                }
                TokenizerVmOpcode::ScanIdentifier => {
                    let token = vm_scan_identifier_token(
                        &mut stream,
                        request.line_num,
                        request.token_policy.identifier_continue_class,
                    )?;
                    current_byte = stream.current_byte();
                    push_token(token)?;
                }
                TokenizerVmOpcode::ScanNumber => {
                    let token = vm_scan_number_token(
                        &mut stream,
                        request.line_num,
                        request.token_policy.number_suffix_binary.as_str(),
                        request.token_policy.number_suffix_octal.as_str(),
                        request.token_policy.number_suffix_decimal.as_str(),
                        request.token_policy.number_suffix_hex.as_str(),
                    )?;
                    current_byte = stream.current_byte();
                    push_token(token)?;
                }
                TokenizerVmOpcode::ScanString => {
                    let token = vm_scan_string_token(
                        &mut stream,
                        request.line_num,
                        request.token_policy.escape_char,
                    )?;
                    current_byte = stream.current_byte();
                    push_token(token)?;
                }
                TokenizerVmOpcode::ScanSymbol => {
                    let token = vm_scan_symbol_token(
                        &mut stream,
                        request.line_num,
                        request.token_policy.comment_prefix.as_str(),
                        request.token_policy.identifier_continue_class,
                    )?;
                    current_byte = stream.current_byte();
                    if let Some(token) = token {
                        push_token(token)?;
                    }
                }
            }
        }

        Ok(tokens)
    }

    pub fn parser_contract_for_resolved(
        &self,
        resolved: &ResolvedHierarchy,
    ) -> Option<&RuntimeParserContract> {
        self.lookup_scoped(&self.parser_contracts, resolved)
    }

    pub fn resolve_parser_contract(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<RuntimeParserContract>, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        Ok(self.parser_contract_for_resolved(&resolved).cloned())
    }

    pub fn validate_parser_contract_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        estimated_ast_nodes: usize,
    ) -> Result<RuntimeParserContract, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        let contract = self
            .parser_contract_for_resolved(&resolved)
            .ok_or_else(|| {
                RuntimeBridgeError::Resolve(format!(
                    "missing VM parser contract for family '{}'",
                    resolved.family_id
                ))
            })?;
        self.ensure_parser_contract_compatible_for_assembler(contract)?;
        let error_code = parser_contract_error_code(contract);
        let parser_token_budget = self.budget_limits.max_parser_tokens_per_line;
        if estimated_ast_nodes > parser_token_budget {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    error_code,
                    format!(
                        "parser token budget exceeded ({} > {})",
                        estimated_ast_nodes, parser_token_budget
                    ),
                    None,
                ),
            ));
        }
        let max_nodes = (contract.max_ast_nodes_per_line as usize)
            .min(self.budget_limits.max_parser_ast_nodes_per_line);
        if estimated_ast_nodes > max_nodes {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    error_code,
                    format!(
                        "parser AST node budget exceeded ({} > {})",
                        estimated_ast_nodes, max_nodes
                    ),
                    None,
                ),
            ));
        }
        Ok(contract.clone())
    }

    pub fn parser_vm_program_for_resolved(
        &self,
        resolved: &ResolvedHierarchy,
    ) -> Option<&RuntimeParserVmProgram> {
        self.lookup_scoped(&self.parser_vm_programs, resolved)
    }

    pub fn resolve_parser_vm_program(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<RuntimeParserVmProgram>, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        Ok(self.parser_vm_program_for_resolved(&resolved).cloned())
    }

    pub fn enforce_parser_vm_program_budget_for_assembler(
        &self,
        parser_contract: &RuntimeParserContract,
        parser_vm_program: &RuntimeParserVmProgram,
    ) -> Result<(), RuntimeBridgeError> {
        let max_bytes = self.budget_limits.max_parser_vm_program_bytes;
        let actual = parser_vm_program.program.len();
        if actual > max_bytes {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    parser_contract_error_code(parser_contract),
                    format!(
                        "parser VM program byte budget exceeded ({} > {})",
                        actual, max_bytes
                    ),
                    None,
                ),
            ));
        }
        Ok(())
    }

    pub fn expr_contract_for_resolved(
        &self,
        resolved: &ResolvedHierarchy,
    ) -> Option<&RuntimeExprContract> {
        self.lookup_scoped(&self.expr_contracts, resolved)
    }

    pub fn resolve_expr_contract(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<RuntimeExprContract>, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        Ok(self.expr_contract_for_resolved(&resolved).cloned())
    }

    pub fn expr_parser_contract_for_resolved(
        &self,
        resolved: &ResolvedHierarchy,
    ) -> Option<&RuntimeExprParserContract> {
        self.lookup_scoped(&self.expr_parser_contracts, resolved)
    }

    pub fn resolve_expr_parser_contract(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<RuntimeExprParserContract>, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        Ok(self.expr_parser_contract_for_resolved(&resolved).cloned())
    }

    pub fn resolve_expr_budgets(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<PortableExprBudgets, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        let Some(contract) = self.expr_contract_for_resolved(&resolved) else {
            return Ok(PortableExprBudgets::default());
        };
        if contract.opcode_version != EXPR_VM_OPCODE_VERSION_V1
            && contract.opcode_version != EXPR_VM_OPCODE_VERSION_V2
        {
            return Err(RuntimeBridgeError::Resolve(format!(
                "unsupported VM expression contract opcode version {}",
                contract.opcode_version
            )));
        }
        Ok(PortableExprBudgets {
            max_program_bytes: contract.max_program_bytes as usize,
            max_stack_depth: contract.max_stack_depth as usize,
            max_symbol_refs: contract.max_symbol_refs as usize,
            max_eval_steps: contract.max_eval_steps as usize,
        })
    }

    pub fn interned_id(&self, value_lower: &str) -> Option<u32> {
        self.interned_ids.get(value_lower).copied()
    }

    pub fn ensure_parser_contract_compatible_for_assembler(
        &self,
        contract: &RuntimeParserContract,
    ) -> Result<(), RuntimeBridgeError> {
        self.ensure_parser_diagnostic_map_compatible_for_assembler(contract)?;
        let error_code = parser_contract_error_code(contract);
        if contract.max_ast_nodes_per_line == 0 {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    error_code,
                    "parser contract max_ast_nodes_per_line must be > 0",
                    None,
                ),
            ));
        }
        if contract.opcode_version != PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    error_code,
                    format!(
                        "unsupported parser contract opcode version {}",
                        contract.opcode_version
                    ),
                    None,
                ),
            ));
        }
        if !contract
            .grammar_id
            .eq_ignore_ascii_case(PARSER_GRAMMAR_ID_LINE_V1)
        {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    error_code,
                    format!("unsupported parser grammar id '{}'", contract.grammar_id),
                    None,
                ),
            ));
        }
        if !contract
            .ast_schema_id
            .eq_ignore_ascii_case(PARSER_AST_SCHEMA_ID_LINE_V1)
        {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    error_code,
                    format!(
                        "unsupported parser AST schema id '{}'",
                        contract.ast_schema_id
                    ),
                    None,
                ),
            ));
        }
        Ok(())
    }

    fn ensure_parser_diagnostic_map_compatible_for_assembler(
        &self,
        contract: &RuntimeParserContract,
    ) -> Result<(), RuntimeBridgeError> {
        let error_code = parser_contract_error_code(contract);
        for (field_name, value) in [
            (
                "unexpected_token",
                contract.diagnostics.unexpected_token.as_str(),
            ),
            (
                "expected_expression",
                contract.diagnostics.expected_expression.as_str(),
            ),
            (
                "expected_operand",
                contract.diagnostics.expected_operand.as_str(),
            ),
            (
                "invalid_statement",
                contract.diagnostics.invalid_statement.as_str(),
            ),
        ] {
            if value.trim().is_empty() {
                return Err(RuntimeBridgeError::Diagnostic(
                    RuntimeBridgeDiagnostic::new(
                        error_code,
                        format!(
                            "missing parser contract diagnostic mapping for '{}'",
                            field_name
                        ),
                        None,
                    ),
                ));
            }
            self.ensure_diag_code_declared_in_package_catalog(
                error_code,
                "parser contract",
                value,
            )?;
        }
        Ok(())
    }

    pub fn ensure_expr_parser_contract_compatible_for_assembler(
        &self,
        contract: &RuntimeExprParserContract,
    ) -> Result<(), RuntimeBridgeError> {
        let error_code = if contract
            .diagnostics
            .invalid_expression_program
            .trim()
            .is_empty()
        {
            "vm-runtime"
        } else {
            contract.diagnostics.invalid_expression_program.as_str()
        };

        if contract.opcode_version != EXVM_OPCODE_VERSION_V1
            && contract.opcode_version != package::EXVM_OPCODE_VERSION_V2
        {
            return Err(RuntimeBridgeError::Resolve(format!(
                "{}: unsupported expression parser contract opcode version {}",
                error_code, contract.opcode_version
            )));
        }

        if contract
            .diagnostics
            .invalid_expression_program
            .trim()
            .is_empty()
        {
            return Err(RuntimeBridgeError::Resolve(format!(
                "{}: missing diagnostics.invalid_expression_program code",
                error_code
            )));
        }

        self.ensure_diag_code_declared_in_package_catalog(
            error_code,
            "expression parser contract diagnostics.invalid_expression_program",
            contract.diagnostics.invalid_expression_program.as_str(),
        )
    }

    pub fn ensure_parser_vm_v2_expr_subcall_contract_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<(), RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        let Some(contract) = self.expr_parser_contract_for_resolved(&resolved) else {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    DIAG_PARSER_OPASM_V2_UNKNOWN_SUBCALL_CONTRACT,
                    "opasm v2 could not resolve an opcore expression parser contract",
                    None,
                ),
            ));
        };
        if contract.opcode_version != EXVM_OPCODE_VERSION_V1
            && contract.opcode_version != package::EXVM_OPCODE_VERSION_V2
        {
            return Err(RuntimeBridgeError::Diagnostic(
                RuntimeBridgeDiagnostic::new(
                    DIAG_PARSER_OPASM_V2_SUBCALL_VERSION_MISMATCH,
                    format!(
                        "opasm v2 expression sub-call opcode version mismatch ({} unsupported)",
                        contract.opcode_version
                    ),
                    None,
                ),
            ));
        }
        Ok(())
    }

    pub fn ensure_diag_code_declared_in_package_catalog(
        &self,
        error_code: &str,
        context: &str,
        code: &str,
    ) -> Result<(), RuntimeBridgeError> {
        if self.diag_templates.contains_key(&code.to_ascii_lowercase()) {
            return Ok(());
        }
        Err(RuntimeBridgeError::Diagnostic(
            RuntimeBridgeDiagnostic::new(
                error_code,
                format!(
                    "{} diagnostic code '{}' is not declared in package DIAG catalog",
                    context, code
                ),
                None,
            ),
        ))
    }

    pub fn resolve_tokenizer_vm_parity_checklist(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<&'static str>, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        Ok(tokenizer_vm_parity_checklist_for_family(
            resolved.family_id.as_str(),
        ))
    }

    pub fn resolve_expr_parser_vm_parity_checklist(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<&'static str>, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        if family_expr_parser_rollout_policy(resolved.family_id.as_str()).is_none() {
            return Ok(None);
        }
        Ok(expr_parser_vm_parity_checklist_for_family(
            resolved.family_id.as_str(),
        ))
    }

    pub fn resolve_parser_certification_checklists(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<RuntimeParserCertificationChecklists, RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        let checklists = parser_certification_checklists_for_family(resolved.family_id.as_str());
        Ok(RuntimeParserCertificationChecklists {
            expression_parser_checklist: checklists.expression_parser_checklist,
            instruction_parse_encode_checklist: checklists.instruction_parse_encode_checklist,
        })
    }

    pub fn has_declared_diagnostic_code(&self, code: &str) -> bool {
        self.diag_templates.contains_key(&code.to_ascii_lowercase())
    }

    pub fn diag_message(&self, code: &str, fallback: &str, args: &[(&str, &str)]) -> String {
        let Some(template) = self.diag_templates.get(&code.to_ascii_lowercase()) else {
            return fallback.to_string();
        };
        render_diag_template(template, args)
    }

    fn scoped_owner_lookup_order(&self, resolved: &ResolvedHierarchy) -> [(u8, Option<u32>); 3] {
        let dialect_id = resolved.dialect_id.to_ascii_lowercase();
        let cpu_id = resolved.cpu_id.to_ascii_lowercase();
        let family_id = resolved.family_id.to_ascii_lowercase();
        [
            (2u8, self.interned_id(&dialect_id)),
            (1u8, self.interned_id(&cpu_id)),
            (0u8, self.interned_id(&family_id)),
        ]
    }

    fn lookup_scoped<'a, T>(
        &self,
        map: &'a HashMap<(u8, u32), T>,
        resolved: &ResolvedHierarchy,
    ) -> Option<&'a T> {
        for (owner_tag, owner_id) in self.scoped_owner_lookup_order(resolved) {
            let Some(owner_id) = owner_id else {
                continue;
            };
            if let Some(value) = map.get(&(owner_tag, owner_id)) {
                return Some(value);
            }
        }
        None
    }
}

#[derive(Debug, Default)]
struct LowercaseIdInterner {
    ids: HashMap<String, u32>,
}

impl LowercaseIdInterner {
    fn intern(&mut self, value: &str) -> u32 {
        let key = value.to_ascii_lowercase();
        if let Some(id) = self.ids.get(&key) {
            return *id;
        }
        let next = self.ids.len();
        let Ok(id) = u32::try_from(next) else {
            return u32::MAX;
        };
        self.ids.insert(key, id);
        id
    }

    fn into_ids(self) -> HashMap<String, u32> {
        self.ids
    }
}

fn owner_key_parts(owner: &ScopedOwner) -> (u8, String) {
    owner.key_parts_lowercase()
}

fn contains_form(map: &HashMap<String, HashSet<String>>, owner_id: &str, mnemonic: &str) -> bool {
    map.get(&owner_id.to_ascii_lowercase())
        .is_some_and(|forms| forms.contains(mnemonic))
}

fn tokenizer_vm_parity_checklist_for_family(family_id: &str) -> Option<&'static str> {
    match family_id.to_ascii_lowercase().as_str() {
        "mos6502" => Some("Phase 6 tokenizer VM parity matrix (full corpus)"),
        "intel8080" => Some("Phase 6 tokenizer VM parity matrix (full corpus)"),
        "motorola6800" => Some("Phase 6 tokenizer VM parity matrix (full corpus)"),
        "motorola68000" => Some(
            "Phase 6 tokenizer VM parity matrix (staged Rust corpus + opt-in native family corpus)",
        ),
        _ => None,
    }
}

fn expr_parser_vm_parity_checklist_for_family(family_id: &str) -> Option<&'static str> {
    match family_id.to_ascii_lowercase().as_str() {
        "mos6502" => {
            Some("Phase 8 expression parser VM parity corpus and deterministic diff gates")
        }
        "intel8080" => {
            Some("Phase 8 expression parser VM parity corpus and deterministic diff gates")
        }
        "motorola6800" => {
            Some("Phase 8 expression parser VM parity corpus and deterministic diff gates")
        }
        "motorola68000" => {
            Some("Phase 8 expression parser VM parity corpus and deterministic diff gates for staged motorola68000 support")
        }
        _ => None,
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

fn render_diag_template(template: &str, args: &[(&str, &str)]) -> String {
    let mut rendered = template.to_string();
    for (key, value) in args {
        rendered = rendered.replace(&format!("{{{}}}", key), value);
    }
    rendered
}

fn tokenizer_vm_error_code(program: &RuntimeTokenizerVmProgram) -> &str {
    tokenizer_runtime_utils::tokenizer_vm_error_code(program.diagnostics.invalid_char.as_str())
}

#[allow(dead_code)]
fn vm_diag_code_for_slot(diagnostics: &TokenizerVmDiagnosticMap, slot: u8) -> &str {
    let mapped = TokenizerDiagCodes {
        invalid_char: diagnostics.invalid_char.as_str(),
        unterminated_string: diagnostics.unterminated_string.as_str(),
        step_limit_exceeded: diagnostics.step_limit_exceeded.as_str(),
        token_limit_exceeded: diagnostics.token_limit_exceeded.as_str(),
        lexeme_limit_exceeded: diagnostics.lexeme_limit_exceeded.as_str(),
        error_limit_exceeded: diagnostics.error_limit_exceeded.as_str(),
    };
    tokenizer_runtime_utils::vm_diag_code_for_slot(&mapped, slot)
}

fn vm_read_u8(
    program: &[u8],
    pc: &mut usize,
    diag_code: &str,
    context: &str,
) -> Result<u8, RuntimeBridgeError> {
    tokenizer_runtime_utils::vm_read_u8(program, pc, diag_code, context)
        .map_err(RuntimeBridgeError::Resolve)
}

fn vm_read_u16(
    program: &[u8],
    pc: &mut usize,
    diag_code: &str,
    context: &str,
) -> Result<u16, RuntimeBridgeError> {
    tokenizer_runtime_utils::vm_read_u16(program, pc, diag_code, context)
        .map_err(RuntimeBridgeError::Resolve)
}

fn vm_read_u32(
    program: &[u8],
    pc: &mut usize,
    diag_code: &str,
    context: &str,
) -> Result<u32, RuntimeBridgeError> {
    tokenizer_runtime_utils::vm_read_u32(program, pc, diag_code, context)
        .map_err(RuntimeBridgeError::Resolve)
}

fn vm_offset_to_pc(
    program: &[u8],
    offset: u32,
    diag_code: &str,
    context: &str,
) -> Result<usize, RuntimeBridgeError> {
    tokenizer_runtime_utils::vm_offset_to_pc(program, offset, diag_code, context)
        .map_err(RuntimeBridgeError::Resolve)
}

fn vm_scan_next_core_token<'a>(
    request: &PortableTokenizeRequest<'a>,
    cursor: usize,
    tokenizer: &mut Option<Tokenizer<'a>>,
) -> Result<Option<(PortableToken, usize)>, RuntimeBridgeError> {
    tokenizer_runtime_utils::vm_scan_next_core_token(
        request.source_line,
        request.line_num,
        cursor,
        tokenizer,
    )
    .map_err(RuntimeBridgeError::Resolve)
}

fn vm_char_class_matches(byte: Option<u8>, class: u8, policy: &RuntimeTokenPolicy) -> bool {
    tokenizer_runtime_utils::vm_char_class_matches(
        byte,
        class,
        policy.identifier_start_class,
        policy.identifier_continue_class,
        policy.quote_chars.as_str(),
        policy.punctuation_chars.as_str(),
        policy.operator_chars.as_str(),
    )
}

fn vm_build_token(
    kind_code: u8,
    lexeme: &[u8],
    line_num: u32,
    lexeme_start: usize,
    lexeme_end: usize,
    cursor: usize,
) -> Result<PortableToken, RuntimeBridgeError> {
    tokenizer_runtime_utils::vm_build_token(
        kind_code,
        lexeme,
        line_num,
        lexeme_start,
        lexeme_end,
        cursor,
    )
    .map_err(RuntimeBridgeError::Resolve)
}

fn vm_scan_identifier_token(
    stream: &mut VmTokenizerInputStream<'_>,
    line_num: u32,
    identifier_continue_class: u32,
) -> Result<PortableToken, RuntimeBridgeError> {
    tokenizer_runtime_utils::vm_scan_identifier_token(stream, line_num, identifier_continue_class)
        .map_err(RuntimeBridgeError::Resolve)
}

fn vm_scan_number_token(
    stream: &mut VmTokenizerInputStream<'_>,
    line_num: u32,
    number_suffix_binary: &str,
    number_suffix_octal: &str,
    number_suffix_decimal: &str,
    number_suffix_hex: &str,
) -> Result<PortableToken, RuntimeBridgeError> {
    tokenizer_runtime_utils::vm_scan_number_token(
        stream,
        line_num,
        number_suffix_binary,
        number_suffix_octal,
        number_suffix_decimal,
        number_suffix_hex,
    )
    .map_err(RuntimeBridgeError::Resolve)
}

fn vm_scan_string_token(
    stream: &mut VmTokenizerInputStream<'_>,
    line_num: u32,
    escape_char: Option<char>,
) -> Result<PortableToken, RuntimeBridgeError> {
    tokenizer_runtime_utils::vm_scan_string_token(stream, line_num, escape_char)
        .map_err(RuntimeBridgeError::Resolve)
}

fn vm_scan_symbol_token(
    stream: &mut VmTokenizerInputStream<'_>,
    line_num: u32,
    comment_prefix: &str,
    identifier_continue_class: u32,
) -> Result<Option<PortableToken>, RuntimeBridgeError> {
    tokenizer_runtime_utils::vm_scan_symbol_token(
        stream,
        line_num,
        comment_prefix,
        identifier_continue_class,
    )
    .map_err(RuntimeBridgeError::Resolve)
}

fn apply_token_policy_to_token(token: PortableToken, policy: &RuntimeTokenPolicy) -> PortableToken {
    let mapped = match policy.case_rule {
        TokenCaseRule::Preserve => AsciiCaseRule::Preserve,
        TokenCaseRule::AsciiLower => AsciiCaseRule::AsciiLower,
        TokenCaseRule::AsciiUpper => AsciiCaseRule::AsciiUpper,
    };
    tokenizer_runtime_utils::apply_token_case_rule(token, mapped)
}
