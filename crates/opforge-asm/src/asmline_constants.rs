// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Resolve immutable scalar dependencies independently of assembly pass count.
use super::*;
use crate::engine::repetition_trace::LoopObservation;
use std::collections::HashSet;

pub(super) struct Definition {
    name: String,
    expr: Expr,
    scope: AsmSymbolScopeState,
    cpu: CpuType,
    line: u32,
    loop_dependencies: Vec<String>,
}

impl AsmLine<'_> {
    /// Defer the whole scalar count, rather than evaluating unknown leaves as
    /// zero. Captured loop observations resolve its final lexical owner later.
    pub(crate) fn counted_loop_has_provisional_dependencies(
        &self,
        expr: &Expr,
    ) -> Result<bool, AstEvalError> {
        if self.pass != 1 {
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

    pub(crate) fn executed_constant_names(&self) -> HashSet<(String, u32)> {
        self.constant_definitions
            .iter()
            .map(|definition| (definition.name.clone(), definition.line))
            .collect()
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
        self.loop_observations.push(LoopObservation {
            line,
            count,
            original_count: count,
            path: self.repetition_path.clone(),
            operands: operands.to_vec(),
            scope: self.symbol_scope.clone(),
            cpu: self.cpu,
            counted,
            immutable: false,
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
        let mut names = Vec::new();
        scalar_dependencies(expr, &mut names);
        let loop_dependencies = names
            .into_iter()
            .filter(|name| self.lookup_loop_var(name).is_some())
            .map(str::to_string)
            .collect();
        self.constant_definitions.push(Definition {
            name: name.into(),
            expr: expr.clone(),
            scope: self.symbol_scope.clone(),
            cpu: self.cpu,
            line: self.current_line_num,
            loop_dependencies,
        });
    }

    /// Resolve only context-independent scalar DAGs. Definitions involving PC,
    /// labels, mutable values or structured values retain source-order behavior.
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
        let result = self.resolve_constant_graph(&definitions);
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
            absolute[index] = scalar_dependencies(&definition.expr, &mut names);
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
                let needs_evaluation = !self
                    .layout
                    .absolute_constant_symbols
                    .contains(&definition.name)
                    || edges[node].iter().any(|&child| repaired[child]);
                if absolute[node] {
                    self.immutable_scalar_constants
                        .insert(definition.name.clone());
                }
                if absolute[node] && needs_evaluation {
                    self.symbol_scope = definition.scope.clone();
                    self.cpu = definition.cpu;
                    let value = self
                        .eval_expr_for_signed_scalar_context(&definition.expr)
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
                    repaired[node] = self
                        .scalar_value_symbols
                        .get(&Self::value_symbol_key(&definition.name))
                        .copied()
                        != Some(value);
                    entry.val = value as u32;
                    entry.updated = true;
                    // Keep the signed semantic value and ABI shadow synchronized.
                    self.sync_value_symbol(&definition.name, &AsmValue::Scalar(value));
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
                states[node] = 2;
                stack.pop();
            }
        }
        Ok(changed)
    }
}

/// Collect dependencies even in layout-dependent expressions, so adding PC to
/// a cycle cannot hide it. Only scalar operations with no contextual inputs are
/// eligible for early evaluation; structured values keep their normal semantics.
fn scalar_dependencies<'a>(expr: &'a Expr, names: &mut Vec<&'a str>) -> bool {
    match expr {
        Expr::Number(_, _) => true,
        Expr::Identifier(name, _) | Expr::Register(name, _) => {
            names.push(name);
            true
        }
        Expr::Indirect(inner, _)
        | Expr::IndirectLong(inner, _)
        | Expr::Immediate(inner, _)
        | Expr::Unary { expr: inner, .. } => scalar_dependencies(inner, names),
        Expr::Binary { left, right, .. } => {
            let left = scalar_dependencies(left, names);
            let right = scalar_dependencies(right, names);
            left && right
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => {
            let cond = scalar_dependencies(cond, names);
            let then_expr = scalar_dependencies(then_expr, names);
            let else_expr = scalar_dependencies(else_expr, names);
            cond && then_expr && else_expr
        }
        Expr::List(items, _) | Expr::Tuple(items, _) => {
            for item in items {
                scalar_dependencies(item, names);
            }
            false
        }
        Expr::Index { base, index, .. } => {
            scalar_dependencies(base, names);
            scalar_dependencies(index, names);
            false
        }
        Expr::Member { base, .. } => {
            scalar_dependencies(base, names);
            false
        }
        Expr::StructLiteral { fields, .. } => {
            for (_, value) in fields {
                scalar_dependencies(value, names);
            }
            false
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
            scalar_dependencies(start, names);
            scalar_dependencies(end, names);
            if let Some(step) = step {
                scalar_dependencies(step, names);
            }
            false
        }
        _ => false,
    }
}
