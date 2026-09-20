// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Resolve immutable scalar dependencies independently of assembly pass count.
use super::*;

pub(super) struct Definition {
    name: String,
    expr: Expr,
    scope: AsmSymbolScopeState,
    cpu: CpuType,
    line: u32,
}

impl AsmLine<'_> {
    pub(super) fn capture_constant(&mut self, name: &str, expr: &Expr) {
        self.constant_definitions.push(Definition {
            name: name.into(),
            expr: expr.clone(),
            scope: self.symbol_scope.clone(),
            cpu: self.cpu,
            line: self.current_line_num,
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
                if absolute[node] && needs_evaluation {
                    self.symbol_scope = definition.scope.clone();
                    self.cpu = definition.cpu;
                    let value = self
                        .eval_expr_for_scalar_context(&definition.expr)
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
                    repaired[node] = entry.val != value;
                    entry.val = value;
                    entry.updated = true;
                    // Scalar value symbols use the symbol-table shadow, exactly
                    // as the ordinary assignment path does; clear stale typed
                    // storage through the shared synchronization helper.
                    self.sync_value_symbol(&definition.name, &AsmValue::Scalar(i64::from(value)));
                    repaired[node] |= self
                        .layout
                        .absolute_constant_symbols
                        .insert(definition.name.clone());
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
