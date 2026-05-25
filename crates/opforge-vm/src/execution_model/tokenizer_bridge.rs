use super::*;
use package::TokenizerVmLimits;

use crate::runtime_portable_types::PortableTokenizerByteStream;

impl HierarchyExecutionModel {
    pub fn tokenize_portable_statement(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        source_line: &str,
        line_num: u32,
    ) -> Result<Vec<PortableToken>, RuntimeBridgeError> {
        let resolved = self.core.resolve_pipeline(cpu_id, dialect_override)?;
        let request = PortableTokenizeRequest {
            family_id: resolved.family_id.as_str(),
            cpu_id: resolved.cpu_id.as_str(),
            dialect_id: resolved.dialect_id.as_str(),
            source_line,
            source_stream: PortableTokenizerByteStream::from_source_line(source_line),
            line_num,
            token_policy: self.token_policy_for_resolved(&resolved),
        };
        match self.effective_tokenizer_mode() {
            RuntimeTokenizerMode::Auto | RuntimeTokenizerMode::Vm => {
                let vm_program = self
                    .tokenizer_vm_program_for_resolved(&resolved)
                    .ok_or_else(|| {
                        RuntimeBridgeError::Resolve(format!(
                            "missing tokenizer VM program for family '{}'",
                            resolved.family_id
                        ))
                    })?;
                let tokens = self.tokenize_with_vm_core(&request, vm_program)?;
                if tokens.is_empty()
                    && !source_line_can_tokenize_to_empty(source_line, &request.token_policy)
                {
                    return Err(RuntimeBridgeError::Resolve(format!(
                        "{}: tokenizer VM produced no tokens for non-empty source line",
                        vm_program.diagnostics.invalid_char
                    )));
                }
                Ok(tokens)
            }
        }
    }

    pub fn tokenize_portable_statement_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        source_line: &str,
        line_num: u32,
    ) -> Result<Vec<PortableToken>, RuntimeBridgeError> {
        self.tokenize_portable_statement_vm_authoritative(
            cpu_id,
            dialect_override,
            source_line,
            line_num,
        )
    }

    pub fn tokenize_portable_statement_vm_authoritative(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        source_line: &str,
        line_num: u32,
    ) -> Result<Vec<PortableToken>, RuntimeBridgeError> {
        let route = self.resolve_tokenizer_vm_route_for_assembler(cpu_id, dialect_override)?;
        let request = PortableTokenizeRequest {
            family_id: route.family_id.as_str(),
            cpu_id: route.cpu_id.as_str(),
            dialect_id: route.dialect_id.as_str(),
            source_line,
            source_stream: PortableTokenizerByteStream::from_source_line(source_line),
            line_num,
            token_policy: route.token_policy.clone(),
        };
        let tokens =
            self.tokenize_with_prevalidated_vm_core(&request, &route.tokenizer_vm_program)?;
        if tokens.is_empty()
            && !source_line_can_tokenize_to_empty(source_line, &request.token_policy)
        {
            return Err(RuntimeBridgeError::Resolve(format!(
                "{}: tokenizer VM produced no tokens for non-empty source line",
                route.tokenizer_vm_program.diagnostics.invalid_char
            )));
        }
        Ok(tokens)
    }

    pub(crate) fn resolve_tokenizer_vm_route_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<std::sync::Arc<ResolvedTokenizerVmRoute>, RuntimeBridgeError> {
        let key = TokenizerVmRouteCacheKey::new(cpu_id, dialect_override);
        if let Some(route) = self
            .tokenizer_vm_route_cache
            .lock()
            .expect("tokenizer VM route cache lock poisoned")
            .get(&key)
        {
            return Ok(std::sync::Arc::clone(route));
        }

        let resolved = self.core.resolve_pipeline(cpu_id, dialect_override)?;
        let token_policy = self.token_policy_for_resolved(&resolved);
        let tokenizer_vm_program = self
            .tokenizer_vm_program_for_resolved(&resolved)
            .ok_or_else(|| {
                RuntimeBridgeError::Resolve(format!(
                    "missing tokenizer VM program for family '{}'",
                    resolved.family_id
                ))
            })?
            .clone();
        self.core
            .ensure_tokenizer_vm_program_compatible_for_assembler(&tokenizer_vm_program)?;
        let route = std::sync::Arc::new(ResolvedTokenizerVmRoute::new(
            &resolved,
            token_policy,
            tokenizer_vm_program,
        ));
        self.tokenizer_vm_route_cache
            .lock()
            .expect("tokenizer VM route cache lock poisoned")
            .insert(key, std::sync::Arc::clone(&route));
        Ok(route)
    }

    pub fn resolve_tokenizer_vm_program(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<Option<RuntimeTokenizerVmProgram>, RuntimeBridgeError> {
        self.core
            .resolve_tokenizer_vm_program(cpu_id, dialect_override)
    }

    pub fn resolve_tokenizer_vm_limits(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<TokenizerVmLimits, RuntimeBridgeError> {
        self.core
            .resolve_tokenizer_vm_limits(cpu_id, dialect_override)
    }

    pub fn token_policy_for_resolved(&self, resolved: &ResolvedHierarchy) -> RuntimeTokenPolicy {
        self.core.token_policy_for_resolved(resolved)
    }

    fn effective_tokenizer_mode(&self) -> RuntimeTokenizerMode {
        match self.core.tokenizer_mode {
            RuntimeTokenizerMode::Auto => RuntimeTokenizerMode::Vm,
            mode => mode,
        }
    }

    fn tokenizer_vm_program_for_resolved(
        &self,
        resolved: &ResolvedHierarchy,
    ) -> Option<&RuntimeTokenizerVmProgram> {
        self.core.tokenizer_vm_program_for_resolved(resolved)
    }

    pub fn tokenize_with_vm_core(
        &self,
        request: &PortableTokenizeRequest<'_>,
        vm_program: &RuntimeTokenizerVmProgram,
    ) -> Result<Vec<PortableToken>, RuntimeBridgeError> {
        self.core.tokenize_with_vm_core(request, vm_program)
    }

    pub(crate) fn tokenize_with_prevalidated_vm_core(
        &self,
        request: &PortableTokenizeRequest<'_>,
        vm_program: &RuntimeTokenizerVmProgram,
    ) -> Result<Vec<PortableToken>, RuntimeBridgeError> {
        self.core
            .tokenize_with_prevalidated_vm_core(request, vm_program)
    }
}

fn source_line_can_tokenize_to_empty(source_line: &str, policy: &RuntimeTokenPolicy) -> bool {
    crate::tokenizer_runtime_utils::source_line_can_tokenize_to_empty(
        source_line,
        policy.comment_prefix.as_str(),
    )
}

pub fn apply_token_policy_to_token(
    token: PortableToken,
    policy: &RuntimeTokenPolicy,
) -> PortableToken {
    crate::tokenizer_runtime_utils::apply_token_case_rule(
        token,
        match policy.case_rule {
            package::TokenCaseRule::Preserve => {
                crate::tokenizer_runtime_utils::AsciiCaseRule::Preserve
            }
            package::TokenCaseRule::AsciiLower => {
                crate::tokenizer_runtime_utils::AsciiCaseRule::AsciiLower
            }
            package::TokenCaseRule::AsciiUpper => {
                crate::tokenizer_runtime_utils::AsciiCaseRule::AsciiUpper
            }
        },
    )
}
