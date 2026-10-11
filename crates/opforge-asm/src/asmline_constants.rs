// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Resolve immutable value dependencies independently of assembly pass count.
use super::*;
use crate::engine::repetition_trace::{GuardKey, LoopObservation};
use std::collections::HashSet;

#[derive(Clone)]
pub(super) struct Definition {
    name: String,
    expr: Expr,
    scope: AsmSymbolScopeState,
    cpu: CpuType,
    line: u32,
    loop_dependencies: Vec<String>,
    guards: Vec<GuardKey>,
}

impl AsmLine<'_> {
    pub(crate) fn seed_immutable_values(&mut self, values: HashMap<String, AsmValue>) {
        self.value_symbols.extend(values);
    }
    pub(crate) fn resolved_immutable_values(&self) -> HashMap<String, AsmValue> {
        self.constant_definitions
            .iter()
            .filter(|definition| {
                self.immutable_value_constants.contains(&definition.name)
                    && self
                        .active_constant_owners
                        .contains(&Self::value_symbol_key(&definition.name))
            })
            .filter_map(|definition| {
                let key = Self::value_symbol_key(&definition.name);
                self.value_symbols
                    .get(&key)
                    .cloned()
                    .map(|value| (key, value))
            })
            .collect()
    }
    /// A typed operand cannot consume a forward alias's scalar ABI placeholder.
    /// Resolve its captured immutable owner before ordinary statement evaluation.
    pub(super) fn resolve_pending_typed_operands(
        &mut self,
        operands: &[Expr],
    ) -> Result<(), (u32, AsmError)> {
        if self.pass != 1 || self.constant_definitions.is_empty() {
            return Ok(());
        }
        let mut roots = HashSet::new();
        for expr in operands {
            let mut names = Vec::new();
            let mut normalized = expr.clone();
            self.normalize_member_bindings(&mut normalized);
            immutable_dependencies(&normalized, &mut names);
            let structured = !scalar_dependencies(&normalized, &mut Vec::new());
            for name in names {
                let mut base = name;
                loop {
                    if let Ok(Some(resolved)) = self.resolve_scoped_name(base) {
                        if let Some(&index) = self
                            .constant_definition_indices
                            .get(&Self::value_symbol_key(&resolved))
                        {
                            if (base != name || structured)
                                && !self
                                    .immutable_value_constants
                                    .contains(&self.constant_definitions[index].name)
                                && !self
                                    .immutable_scalar_constants
                                    .contains(&self.constant_definitions[index].name)
                            {
                                roots.insert(index);
                            }
                        }
                    }
                    let Some((parent, _)) = base.rsplit_once('.') else {
                        break;
                    };
                    base = parent;
                }
            }
        }
        if roots.is_empty() {
            return Ok(());
        }
        let saved_scope = self.symbol_scope.clone();
        let saved_cpu = self.cpu;
        let saved_pass = self.pass;
        self.pass = 2;
        let mut closure = Vec::new();
        let mut stack: Vec<_> = roots.into_iter().collect();
        let mut visited = HashSet::new();
        while let Some(index) = stack.pop() {
            if !visited.insert(index) {
                continue;
            }
            let definition = self.constant_definitions[index].clone();
            self.symbol_scope = definition.scope.clone();
            self.cpu = definition.cpu;
            let mut names = Vec::new();
            let mut dependency_expr = definition.expr.clone();
            self.normalize_member_bindings(&mut dependency_expr);
            immutable_dependencies(&dependency_expr, &mut names);
            for name in names {
                if let Ok(Some(resolved)) = self.resolve_scoped_name(name) {
                    if let Some(&child) = self
                        .constant_definition_indices
                        .get(&Self::value_symbol_key(&resolved))
                    {
                        stack.push(child);
                    }
                }
            }
            self.immutable_scalar_constants.remove(&definition.name);
            self.immutable_value_constants.remove(&definition.name);
            closure.push(definition);
        }
        let result = self.resolve_constant_graph(&closure);
        self.symbol_scope = saved_scope;
        self.cpu = saved_cpu;
        self.pass = saved_pass;
        result?;
        Ok(())
    }
    /// A previous traversal may have selected a different conditional owner.
    /// Permit that replacement, but never two immutable owners executed in
    /// this traversal, including owners newly activated by guard replay.
    pub(super) fn claim_constant_owner(&mut self, name: &str) -> bool {
        self.active_constant_owners
            .insert(Self::value_symbol_key(name))
    }

    /// Defer the whole scalar count, rather than evaluating unknown leaves as
    /// zero. Captured loop observations resolve its final lexical owner later.
    pub(crate) fn counted_loop_has_provisional_dependencies(
        &self,
        expr: &Expr,
    ) -> Result<bool, AstEvalError> {
        if self.pass != 1 && !self.in_certified_activated_guard() {
            return Ok(false);
        }
        if let Some((name, span)) = self.find_private_symbol_in_expr(expr) {
            return Err(ast_eval_from_asm_error(self.visibility_error(&name), span));
        }
        let mut names = Vec::new();
        if !scalar_dependencies(expr, &mut names) {
            return Ok(false);
        }
        let mut unresolved = false;
        for name in names {
            if self.lookup_loop_var(name).is_some() {
                continue;
            }
            unresolved |= self
                .resolve_scoped_name(name)
                .map_err(|error| ast_eval_from_asm_error(error, expr_span(expr)))?
                .is_none();
        }
        Ok(unresolved)
    }

    pub(crate) fn in_certified_activated_guard(&self) -> bool {
        self.guard_stack.iter().flatten().any(|index| {
            self.activated_guard_certificates
                .contains(&self.guard_observations[*index].key)
        })
    }

    /// A newly activated scalar constant may be collected before its lexical
    /// dependencies exist. Defer the whole value, never arbitrary evaluation
    /// failures or ordinary data/instruction operands.
    pub(crate) fn certified_constant_is_provisional(
        &self,
        expr: &Expr,
    ) -> Result<bool, AstEvalError> {
        if !self.in_certified_activated_guard() {
            return Ok(false);
        }
        self.counted_loop_has_provisional_dependencies(expr)
    }

    pub(super) fn eval_constant_definition_value(
        &self,
        expr: &Expr,
        immutable: bool,
    ) -> Result<types::asm_value::AsmValue, AstEvalError> {
        use types::asm_value::AsmValue;
        if immutable && self.certified_constant_is_provisional(expr)? {
            return Ok(AsmValue::Scalar(0));
        }
        match self.eval_expr_for_signed_scalar_context(expr) {
            Ok(scalar) => match self.eval_value_ast(expr) {
                Ok(
                    value @ (AsmValue::List(_)
                    | AsmValue::Values(_)
                    | AsmValue::Record { .. }
                    | AsmValue::RelocatableScalar { .. }
                    | AsmValue::Range { .. }
                    | AsmValue::Struct(_)
                    | AsmValue::StructInstance(_)),
                ) => Ok(value),
                Ok(AsmValue::Scalar(_)) | Err(_) => Ok(AsmValue::Scalar(scalar)),
            },
            Err(scalar_error) => self.eval_value_ast(expr).map_err(|_| scalar_error),
        }
    }

    pub(crate) fn executed_constant_names(&self) -> HashSet<(String, u32)> {
        self.constant_definitions
            .iter()
            .map(|definition| (definition.name.clone(), definition.line))
            .collect()
    }

    /// New definitions can enter the graph only through a branch whose
    /// immutable predicate was certified before the retry. Count expansion by
    /// itself is deliberately insufficient proof of a new graph's ownership.
    pub(crate) fn certified_new_constant_definitions(
        &self,
        previous: &HashSet<(String, u32)>,
        activated_guards: &HashSet<GuardKey>,
    ) -> (bool, bool) {
        let mut any = false;
        let mut certified = true;
        for definition in &self.constant_definitions {
            if previous.contains(&(definition.name.clone(), definition.line)) {
                continue;
            }
            any = true;
            certified &= definition
                .guards
                .iter()
                .any(|key| activated_guards.contains(key));
        }
        (any, certified)
    }

    pub(crate) fn observe_repetition(
        &mut self,
        line: u32,
        count: u32,
        operands: &[Expr],
        counted: bool,
    ) {
        if matches!(self.profile_phase, AsmProfilePhase::Pass2) {
            return;
        }
        let value_certificate = (counted && operands.len() == 2)
            .then(|| self.value_replay_certificate(&operands[1]))
            .flatten();
        self.loop_observations.push(LoopObservation {
            line,
            guards: self
                .guard_stack
                .iter()
                .flatten()
                .map(|index| self.guard_observations[*index].key.clone())
                .collect(),
            count,
            original_count: count,
            path: self.repetition_path.clone(),
            operands: operands.to_vec(),
            scope: self.symbol_scope.clone(),
            cpu: self.cpu,
            counted,
            immutable: value_certificate.is_some(),
            value_owner_replay: value_certificate.is_some_and(|guards| !guards.is_empty()),
            loop_variable_dependent: operands.iter().any(|expr| {
                let mut names = Vec::new();
                scalar_dependencies(expr, &mut names);
                names
                    .into_iter()
                    .any(|name| self.lookup_loop_var(name).is_some())
            }),
        });
    }

    pub(crate) fn immutable_loop_count(
        &mut self,
        observation: &LoopObservation,
        max_loop_iterations: u32,
    ) -> Result<Option<u32>, (u32, AsmError)> {
        if !observation.counted
            || observation.operands.len() != 1
            || observation.loop_variable_dependent
        {
            return Ok(None);
        }
        let saved_scope = std::mem::replace(&mut self.symbol_scope, observation.scope.clone());
        let saved_cpu = std::mem::replace(&mut self.cpu, observation.cpu);
        let saved_pass = std::mem::replace(&mut self.pass, 2);
        let result = (|| {
            let expr = &observation.operands[0];
            let mut names = Vec::new();
            if !scalar_dependencies(expr, &mut names) {
                return Ok(None);
            }
            for name in names {
                let Some(name) = self
                    .resolve_scoped_name(name)
                    .map_err(|e| (observation.line, e))?
                else {
                    return Ok(None);
                };
                if !self.immutable_scalar_constants.contains(&name)
                    || self.symbols.entry(&name).is_none_or(|entry| entry.rw)
                {
                    return Ok(None);
                }
            }
            self.eval_expr_for_non_negative_directive(expr, ".for count")
                .and_then(|count| {
                    if count > max_loop_iterations {
                        Err(AstEvalError::directive(
                            format!(
                                "loop exceeded maximum iteration limit ({max_loop_iterations})"
                            ),
                            expr_span(expr),
                        ))
                    } else {
                        Ok(Some(count))
                    }
                })
                .map_err(|e| {
                    (
                        observation.line,
                        AsmError::new(
                            ast_eval_error_kind_to_asm(e.error.kind()),
                            e.error.message(),
                            None,
                        ),
                    )
                })
        })();
        self.symbol_scope = saved_scope;
        self.cpu = saved_cpu;
        self.pass = saved_pass;
        result
    }
    pub(super) fn capture_constant(&mut self, name: &str, expr: &Expr) {
        if matches!(self.profile_phase, AsmProfilePhase::Pass2) {
            return;
        }
        if self.pass == 1 {
            // Guard replay can select another executed immutable declaration.
            self.declaration_block_owners
                .remove(&name.to_ascii_uppercase());
            self.track_declaration_block_owner(name);
        }
        let mut names = Vec::new();
        immutable_dependencies(expr, &mut names);
        let loop_dependencies = names
            .into_iter()
            .filter(|name| self.lookup_loop_var(name).is_some())
            .map(str::to_string)
            .collect();
        self.capture_value_replay_certificate(name, expr);
        self.constant_definition_indices.insert(
            Self::value_symbol_key(name),
            self.constant_definitions.len(),
        );
        self.constant_definitions.push(Definition {
            name: name.into(),
            expr: expr.clone(),
            scope: self.symbol_scope.clone(),
            cpu: self.cpu,
            line: self.current_line_num,
            loop_dependencies,
            guards: self
                .guard_stack
                .iter()
                .flatten()
                .map(|index| self.guard_observations[*index].key.clone())
                .collect(),
        });
    }

    /// Resolve context-independent immutable value DAGs. Definitions involving
    /// PC, labels or mutable values retain source-order snapshot behavior.
    /// The graph is built from executed definitions, so inactive source and macro
    /// expansion do not require a second parser or another assembly traversal.
    pub(crate) fn resolve_absolute_constants(&mut self) -> Result<bool, (u32, AsmError)> {
        let definitions = std::mem::take(&mut self.constant_definitions);
        if definitions.is_empty() {
            return Ok(false);
        }
        let saved_scope = std::mem::replace(&mut self.symbol_scope, AsmSymbolScopeState::new());
        let saved_cpu = self.cpu;
        let saved_pass = self.pass;
        // The full symbol table now exists: use normal finalized scope/import
        // lookup rather than pass-one provisional block-local lookup.
        self.pass = 2;
        // Recertify each executed owner: a newly selected contextual definition
        // must not inherit a same-name immutable certificate from an old branch.
        for definition in &definitions {
            self.immutable_scalar_constants.remove(&definition.name);
            self.immutable_value_constants.remove(&definition.name);
        }
        let result = self.resolve_constant_graph(&definitions);
        self.constant_definitions = definitions;
        self.symbol_scope = saved_scope;
        self.cpu = saved_cpu;
        self.pass = saved_pass;
        result
    }

    fn resolve_constant_graph(
        &mut self,
        definitions: &[Definition],
    ) -> Result<bool, (u32, AsmError)> {
        let indices: HashMap<_, _> = definitions
            .iter()
            .enumerate()
            .map(|(index, definition)| (Self::value_symbol_key(&definition.name), index))
            .collect();
        let mut edges = vec![Vec::new(); definitions.len()];
        let mut absolute = vec![false; definitions.len()];
        for (index, definition) in definitions.iter().enumerate() {
            self.symbol_scope = definition.scope.clone();
            self.cpu = definition.cpu;
            let mut names = Vec::new();
            let mut dependency_expr = definition.expr.clone();
            self.normalize_member_bindings(&mut dependency_expr);
            absolute[index] = immutable_dependencies(&dependency_expr, &mut names);
            for name in names {
                if definition
                    .loop_dependencies
                    .iter()
                    .any(|dependency| dependency.eq_ignore_ascii_case(name))
                {
                    // Iterator bindings are source-order inputs, even when an
                    // immutable global symbol has the same spelling.
                    absolute[index] = false;
                    continue;
                }
                let resolved = self
                    .resolve_scoped_name(name)
                    .map_err(|error| (definition.line, error))?;
                let Some(resolved) = resolved else {
                    // Preserve ordinary missing-symbol diagnostics at the use.
                    absolute[index] = false;
                    continue;
                };
                let dependency = indices.get(&Self::value_symbol_key(&resolved));
                if let Some(&dependency) = dependency {
                    edges[index].push(dependency);
                } else {
                    // Labels, variables and other symbol producers are not DAG
                    // constants even when their current value happens to be known.
                    absolute[index] = false;
                }
            }
        }

        // Explicit DFS avoids consuming the process stack on a long forward
        // chain. Each definition and dependency edge is visited a bounded number
        // of times; layout retries never serve as dependency propagation.
        let mut states = vec![0u8; definitions.len()];
        let mut repaired = vec![false; definitions.len()];
        let mut stack = Vec::new();
        let mut changed = false;
        for root in 0..definitions.len() {
            if states[root] != 0 {
                continue;
            }
            states[root] = 1;
            stack.push((root, 0));
            while let Some(&(node, cursor)) = stack.last() {
                if let Some(&child) = edges[node].get(cursor) {
                    stack.last_mut().expect("active DFS frame").1 += 1;
                    match states[child] {
                        0 => {
                            states[child] = 1;
                            stack.push((child, 0));
                        }
                        1 => {
                            let definition = &definitions[node];
                            return Err((
                                definition.line,
                                AsmError::new(
                                    AsmErrorKind::Symbol,
                                    "cyclic immutable constant dependency",
                                    Some(&definition.name),
                                ),
                            ));
                        }
                        _ => {}
                    }
                    continue;
                }
                absolute[node] &= edges[node].iter().all(|&child| absolute[child]);
                let definition = &definitions[node];
                if absolute[node] {
                    self.symbol_scope = definition.scope.clone();
                    self.cpu = definition.cpu;
                    let value = self
                        .eval_constant_definition_value(&definition.expr, true)
                        .map_err(|error| {
                            (
                                definition.line,
                                AsmError::new(
                                    ast_eval_error_kind_to_asm(error.error.kind()),
                                    error.error.message(),
                                    Some(&definition.name),
                                ),
                            )
                        })?;
                    let entry = self
                        .symbols
                        .entry_mut(&definition.name)
                        .expect("captured constant has a symbol entry");
                    repaired[node] = match &value {
                        AsmValue::Scalar(scalar) => {
                            self.scalar_value_symbols
                                .get(&Self::value_symbol_key(&definition.name))
                                .copied()
                                != Some(*scalar)
                        }
                        _ => {
                            self.value_symbols
                                .get(&Self::value_symbol_key(&definition.name))
                                != Some(&value)
                        }
                    };
                    entry.val = Self::scalar_shadow_for_value_symbol(&value);
                    entry.updated = true;
                    // Keep the signed semantic value and ABI shadow synchronized.
                    self.sync_value_symbol(&definition.name, &value);
                    repaired[node] |= self
                        .layout
                        .absolute_constant_symbols
                        .insert(definition.name.clone());
                    self.layout.symbol_relocations.insert(
                        definition.name.clone(),
                        crate::state::SymbolRelocation::Absolute,
                    );
                    changed |= repaired[node];
                }
                if absolute[node] && self.has_scalar_value_symbol(&definition.name) {
                    self.immutable_scalar_constants
                        .insert(definition.name.clone());
                }
                if absolute[node] && self.lookup_value_symbol(&definition.name).is_some() {
                    self.immutable_value_constants
                        .insert(definition.name.clone());
                }
                states[node] = 2;
                stack.pop();
            }
        }
        Ok(changed)
    }
}

/// Collect dependencies even in layout-dependent expressions, so adding PC to
/// a cycle cannot hide it. Only scalar operations with no contextual inputs are
/// eligible for scalar certificates; typed owners use the normal value evaluator.
pub(super) fn scalar_dependencies<'a>(expr: &'a Expr, names: &mut Vec<&'a str>) -> bool {
    expression_dependencies(expr, names, false, false)
}

pub(super) fn value_dependencies<'a>(expr: &'a Expr, names: &mut Vec<&'a str>) -> bool {
    expression_dependencies(expr, names, true, false)
}

/// Typed immutable values share the dependency graph, but do not certify scalar
/// loop counts. Mutable and layout-dependent leaves still exclude their owners.
fn immutable_dependencies<'a>(expr: &'a Expr, names: &mut Vec<&'a str>) -> bool {
    expression_dependencies(expr, names, true, true)
}

fn expression_dependencies<'a>(
    expr: &'a Expr,
    names: &mut Vec<&'a str>,
    structured: bool,
    struct_literals: bool,
) -> bool {
    match expr {
        Expr::Number(_, _) => true,
        Expr::Identifier(name, _) | Expr::Register(name, _) => {
            names.push(name);
            true
        }
        Expr::Indirect(inner, _)
        | Expr::IndirectLong(inner, _)
        | Expr::Immediate(inner, _)
        | Expr::Unary { expr: inner, .. } => {
            expression_dependencies(inner, names, structured, struct_literals)
        }
        Expr::Binary { left, right, .. } => {
            let left = expression_dependencies(left, names, structured, struct_literals);
            let right = expression_dependencies(right, names, structured, struct_literals);
            left && right
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => {
            let cond = expression_dependencies(cond, names, structured, struct_literals);
            let then_expr = expression_dependencies(then_expr, names, structured, struct_literals);
            let else_expr = expression_dependencies(else_expr, names, structured, struct_literals);
            cond && then_expr && else_expr
        }
        Expr::List(items, _) | Expr::Tuple(items, _) => {
            let mut eligible = structured;
            for item in items {
                eligible &= expression_dependencies(item, names, structured, struct_literals);
            }
            eligible
        }
        Expr::Index { base, index, .. } => {
            let base = expression_dependencies(base, names, structured, struct_literals);
            let index = expression_dependencies(index, names, structured, struct_literals);
            structured && base && index
        }
        Expr::Member { base, .. } => {
            expression_dependencies(base, names, structured, struct_literals) && structured
        }
        Expr::StructLiteral { fields, .. } => {
            let mut eligible = struct_literals;
            for (_, value) in fields {
                eligible &= expression_dependencies(value, names, structured, struct_literals);
            }
            eligible
        }
        Expr::Call { args, .. } => {
            for arg in args {
                scalar_dependencies(arg, names);
            }
            false
        }
        Expr::Range {
            start, end, step, ..
        } => {
            let start = expression_dependencies(start, names, structured, struct_literals);
            let end = expression_dependencies(end, names, structured, struct_literals);
            let step = step.as_ref().is_none_or(|step| {
                expression_dependencies(step, names, structured, struct_literals)
            });
            structured && start && end && step
        }
        _ => false,
    }
}
