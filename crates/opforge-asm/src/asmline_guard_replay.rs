// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Immutable conditional predicates used to certify loop layout replay.
use super::asmline_constants::scalar_dependencies;
use super::*;
use crate::engine::repetition_trace::GuardObservation;

impl AsmLine<'_> {
    /// Capture a branch predicate using the complete if/elseif chain. Else
    /// requires all preceding tests to be false; only the final test of an
    /// if/elseif branch must be true.
    pub(super) fn observe_guard_branch(&mut self, expr: Option<&Expr>, active: bool, first: bool) {
        if matches!(self.profile_phase, AsmProfilePhase::Pass2) {
            return;
        }
        let previous = if first {
            None
        } else {
            self.guard_stack
                .last()
                .copied()
                .flatten()
                .map(|index| &self.guard_observations[index])
        };
        let mut tests = previous
            .as_ref()
            .map(|guard| guard.tests.clone())
            .unwrap_or_default();
        let mut loop_variable_dependent = previous
            .as_ref()
            .is_some_and(|guard| guard.loop_variable_dependent);
        if let Some(expr) = expr {
            let mut names = Vec::new();
            scalar_dependencies(expr, &mut names);
            loop_variable_dependent |= names
                .into_iter()
                .any(|name| self.lookup_loop_var(name).is_some());
            tests.push(expr.clone());
        }
        let index = self.guard_observations.len();
        self.guard_observations.push(GuardObservation {
            key: (self.current_line_num, self.repetition_path.clone()),
            tests,
            last_test_positive: expr.is_some(),
            active,
            scope: self.symbol_scope.clone(),
            cpu: self.cpu,
            loop_variable_dependent,
        });
        if first {
            self.guard_stack.push(Some(index));
        } else if let Some(branch) = self.guard_stack.last_mut() {
            *branch = Some(index);
        }
    }

    /// Only an immutable scalar predicate can authorize a layout traversal
    /// change. Contextual values retain ordinary pass-instability diagnostics.
    pub(crate) fn immutable_guard_activity(&mut self, guard: &GuardObservation) -> Option<bool> {
        if guard.loop_variable_dependent {
            return None;
        }
        let saved_scope = std::mem::replace(&mut self.symbol_scope, guard.scope.clone());
        let saved_cpu = std::mem::replace(&mut self.cpu, guard.cpu);
        let saved_pass = std::mem::replace(&mut self.pass, 2);
        let result = (|| {
            let mut active = true;
            for (index, expr) in guard.tests.iter().enumerate() {
                let mut names = Vec::new();
                if !scalar_dependencies(expr, &mut names) {
                    return None;
                }
                for name in names {
                    let name = self.resolve_scoped_name(name).ok()??;
                    if !self.immutable_scalar_constants.contains(&name)
                        || self.symbols.entry(&name).is_none_or(|entry| entry.rw)
                    {
                        return None;
                    }
                }
                let positive = guard.last_test_positive && index + 1 == guard.tests.len();
                active &= (self.eval_expr_for_signed_scalar_context(expr).ok()? != 0) == positive;
            }
            Some(active)
        })();
        self.symbol_scope = saved_scope;
        self.cpu = saved_cpu;
        self.pass = saved_pass;
        result
    }
}
