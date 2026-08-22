use super::selector_bridge::SelectorInput;
use super::*;

fn package_expr_is_register(
    model: &HierarchyExecutionModel,
    resolved: &ResolvedHierarchy,
    expr: &Expr,
) -> bool {
    match expr {
        Expr::Register(_, _) => true,
        Expr::Identifier(id, _) => model
            .core
            .register_encoding_for_resolved(resolved, id)
            .is_some(),
        _ => false,
    }
}

fn package_shape_input<'a>(
    model: &HierarchyExecutionModel,
    resolved: &ResolvedHierarchy,
    operands: &'a [Expr],
) -> Option<SelectorInput<'a>> {
    match operands {
        [] => Some(SelectorInput {
            shape_key: "implied".to_string(),
            expr0: None,
            expr1: None,
            extra_exprs: &[],
            force: None,
        }),
        [Expr::Immediate(expr, _)] => Some(SelectorInput {
            shape_key: "immediate".to_string(),
            expr0: Some(expr.as_ref()),
            expr1: None,
            extra_exprs: &[],
            force: None,
        }),
        [expr] if package_expr_is_register(model, resolved, expr) => Some(SelectorInput {
            shape_key: "register".to_string(),
            expr0: Some(expr),
            expr1: None,
            extra_exprs: &[],
            force: None,
        }),
        [Expr::Immediate(expr, _), register]
            if package_expr_is_register(model, resolved, register) =>
        {
            Some(SelectorInput {
                shape_key: "immediate_register".to_string(),
                expr0: Some(expr.as_ref()),
                expr1: Some(register),
                extra_exprs: &[],
                force: None,
            })
        }
        [Expr::Immediate(expr, _), destination] => Some(SelectorInput {
            shape_key: "immediate_direct".to_string(),
            expr0: Some(expr.as_ref()),
            expr1: Some(destination),
            extra_exprs: &[],
            force: None,
        }),
        [left, right]
            if package_expr_is_register(model, resolved, left)
                && package_expr_is_register(model, resolved, right) =>
        {
            Some(SelectorInput {
                shape_key: "register_register".to_string(),
                expr0: Some(left),
                expr1: Some(right),
                extra_exprs: &[],
                force: None,
            })
        }
        [register, Expr::Immediate(expr, _)]
            if package_expr_is_register(model, resolved, register) =>
        {
            Some(SelectorInput {
                shape_key: "register_immediate".to_string(),
                expr0: Some(register),
                expr1: Some(expr.as_ref()),
                extra_exprs: &[],
                force: None,
            })
        }
        [expr, register] if package_expr_is_register(model, resolved, register) => {
            Some(SelectorInput {
                shape_key: "direct_register".to_string(),
                expr0: Some(expr),
                expr1: Some(register),
                extra_exprs: &[],
                force: None,
            })
        }
        [register, expr] if package_expr_is_register(model, resolved, register) => {
            Some(SelectorInput {
                shape_key: "register_direct".to_string(),
                expr0: Some(register),
                expr1: Some(expr),
                extra_exprs: &[],
                force: None,
            })
        }
        [left, right] => Some(SelectorInput {
            shape_key: "direct_direct".to_string(),
            expr0: Some(left),
            expr1: Some(right),
            extra_exprs: &[],
            force: None,
        }),
        [expr] => Some(SelectorInput {
            shape_key: "direct".to_string(),
            expr0: Some(expr),
            expr1: None,
            extra_exprs: &[],
            force: None,
        }),
        _ => {
            let component = |expr: &Expr| match expr {
                Expr::Immediate(_, _) => "immediate",
                expr if package_expr_is_register(model, resolved, expr) => "register",
                _ => "direct",
            };
            Some(SelectorInput {
                shape_key: operands.iter().map(component).collect::<Vec<_>>().join("_"),
                expr0: operands.first().map(|expr| match expr {
                    Expr::Immediate(inner, _) => inner.as_ref(),
                    expr => expr,
                }),
                expr1: operands.get(1).map(|expr| match expr {
                    Expr::Immediate(inner, _) => inner.as_ref(),
                    expr => expr,
                }),
                extra_exprs: &operands[2..],
                force: None,
            })
        }
    }
}

impl HierarchyExecutionModel {
    pub fn encode_instruction(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        mnemonic: &str,
        operands: &dyn OperandSet,
    ) -> Result<Option<Vec<u8>>, RuntimeBridgeError> {
        let candidates = operands.vm_encode_candidates();
        let adapter = OperandSetInstructionAdapter {
            cpu_id,
            dialect_override,
            mnemonic,
            candidates: candidates.as_slice(),
        };
        self.encode_portable_instruction(&adapter)
    }

    pub fn encode_portable_instruction(
        &self,
        request: &dyn PortableInstructionAdapter,
    ) -> Result<Option<Vec<u8>>, RuntimeBridgeError> {
        let resolved = self
            .core
            .resolve_pipeline(request.cpu_id(), request.dialect_override())?;
        let candidates = request.vm_encode_candidates();
        if candidates.is_empty() {
            return Ok(None);
        }
        self.enforce_candidate_budget(candidates)?;
        self.encode_candidates(&resolved, request.mnemonic(), candidates)
    }

    pub fn encode_instruction_from_exprs(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        mnemonic: &str,
        operands: &[Expr],
        ctx: &dyn AssemblerContext,
    ) -> Result<Option<Vec<u8>>, RuntimeBridgeError> {
        self.encode_instruction_from_exprs_with_effects(
            cpu_id,
            dialect_override,
            mnemonic,
            operands,
            ctx,
        )
        .map(|result| result.map(|(bytes, _)| bytes))
    }

    pub fn encode_instruction_from_exprs_with_effects(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        mnemonic: &str,
        operands: &[Expr],
        ctx: &dyn AssemblerContext,
    ) -> Result<
        Option<(Vec<u8>, crate::runtime_model_types::VmInstructionEffects)>,
        RuntimeBridgeError,
    > {
        let resolved = self.core.resolve_pipeline(cpu_id, dialect_override)?;
        let (candidates, require_program) = match self
            .expr_resolver_entry(resolved.family_id.as_str())
        {
            Some(resolver) => match resolver
                .resolver
                .resolve_candidates(self, &resolved, mnemonic, operands, ctx)?
            {
                Some(candidates) => (candidates, true),
                None => {
                    let Some(input) = package_shape_input(self, &resolved, operands) else {
                        return Ok(None);
                    };
                    let Some(candidates) =
                        self.select_candidates_from_package_shape(&resolved, mnemonic, input, ctx)?
                    else {
                        return Ok(None);
                    };
                    (candidates, true)
                }
            },
            None => {
                let Some(input) = package_shape_input(self, &resolved, operands) else {
                    return Ok(None);
                };
                let Some(candidates) =
                    self.select_candidates_from_package_shape(&resolved, mnemonic, input, ctx)?
                else {
                    return Ok(None);
                };
                (candidates, true)
            }
        };
        self.enforce_candidate_budget(&candidates)?;
        match self
            .core
            .encode_candidates_with_effects(&resolved, mnemonic, &candidates)?
        {
            Some(result) => Ok(Some(result)),
            None if require_program => {
                let upper = mnemonic.to_ascii_uppercase();
                let fallback = format!("missing VM program for {}", upper);
                let message = self.diag_message(
                    DIAG_OPTHREAD_MISSING_VM_PROGRAM,
                    fallback.as_str(),
                    &[("mnemonic", upper.as_str())],
                );
                Err(RuntimeBridgeError::Resolve(message))
            }
            None => Ok(None),
        }
    }

    pub fn supports_expr_resolution_for_family(&self, family_id: &str) -> bool {
        self.expr_resolver_entry(family_id).is_some()
    }

    pub fn expr_resolution_is_strict_for_family(&self, family_id: &str) -> bool {
        self.expr_resolver_entry(family_id)
            .map(|entry| entry.strict)
            .unwrap_or(false)
    }

    pub fn defer_native_diagnostics_on_expr_none(&self, family_id: &str) -> bool {
        self.expr_resolver_entry(family_id)
            .map(|entry| entry.defer_native_diagnostics_on_none)
            .unwrap_or(false)
    }

    pub fn selector_gate_only_expr_runtime_for_cpu(&self, cpu_id: &str) -> bool {
        if self
            .core
            .selector_gate_only_expr_runtime_cpus
            .contains(cpu_id)
        {
            return true;
        }
        self.core
            .selector_gate_only_expr_runtime_cpus
            .contains(cpu_id.to_ascii_lowercase().as_str())
    }

    pub fn register_expr_resolver_for_family(
        &mut self,
        family_id: &str,
        resolver: ExprResolverFn,
    ) -> Option<Box<dyn FamilyExprResolver>> {
        self.register_expr_resolver_for_family_with_strict_mode(family_id, resolver, true)
    }

    pub fn register_family_expr_resolver(
        &mut self,
        resolver: Box<dyn FamilyExprResolver>,
    ) -> Option<Box<dyn FamilyExprResolver>> {
        self.register_family_expr_resolver_with_strict_mode(resolver, true)
    }

    fn register_expr_resolver_for_family_with_strict_mode(
        &mut self,
        family_id: &str,
        resolver: ExprResolverFn,
        strict: bool,
    ) -> Option<Box<dyn FamilyExprResolver>> {
        let key = family_id.to_ascii_lowercase();
        self.expr_resolvers
            .insert(
                key.clone(),
                ExprResolverEntry {
                    resolver: Box::new(FnFamilyExprResolver {
                        family_id: key,
                        resolver,
                    }),
                    strict,
                    defer_native_diagnostics_on_none: false,
                },
            )
            .map(|entry| entry.resolver)
    }

    fn register_family_expr_resolver_with_strict_mode(
        &mut self,
        resolver: Box<dyn FamilyExprResolver>,
        strict: bool,
    ) -> Option<Box<dyn FamilyExprResolver>> {
        let key = resolver.family_id().to_ascii_lowercase();
        self.expr_resolvers
            .insert(
                key,
                ExprResolverEntry {
                    resolver,
                    strict,
                    defer_native_diagnostics_on_none: false,
                },
            )
            .map(|entry| entry.resolver)
    }

    fn expr_resolver_entry(&self, family_id: &str) -> Option<&ExprResolverEntry> {
        if let Some(entry) = self.expr_resolvers.get(family_id) {
            return Some(entry);
        }
        self.expr_resolvers
            .get(family_id.to_ascii_lowercase().as_str())
    }
}
