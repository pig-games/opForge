// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Immutable conditional predicates used to certify loop layout replay.
use super::asmline_constants::scalar_dependencies;
use super::*;
use crate::engine::repetition_trace::{GuardObservation, GuardPredicate};

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
        let mut tests = match previous.map(|guard| &guard.predicate) {
            Some(GuardPredicate::If { tests, .. }) => tests.clone(),
            _ => Vec::new(),
        };
        if let Some(expr) = expr {
            tests.push(expr.clone());
        }
        let predicate = GuardPredicate::If {
            tests,
            last_test_positive: expr.is_some(),
        };
        let index = self.capture_guard(predicate, active);
        if first {
            self.guard_stack.push(Some(index));
        } else if let Some(branch) = self.guard_stack.last_mut() {
            *branch = Some(index);
        }
    }

    /// Capture match equality without rewriting its u32 scalar projection into
    /// signed truth expressions. Each case is an ordered, short-circuit OR arm.
    pub(super) fn observe_match_opening(&mut self, selector: Option<&Expr>) {
        if matches!(self.profile_phase, AsmProfilePhase::Pass2) {
            return;
        }
        let index = self.capture_guard(
            GuardPredicate::Match {
                selector: selector.cloned(),
                cases: Vec::new(),
                last_case_positive: true,
            },
            false,
        );
        self.guard_stack.push(Some(index));
    }

    pub(super) fn observe_match_arm(&mut self, exprs: Option<&[Expr]>, active: bool) {
        if matches!(self.profile_phase, AsmProfilePhase::Pass2) {
            return;
        }
        let Some(previous) = self.guard_stack.last().copied().flatten() else {
            return;
        };
        let GuardPredicate::Match {
            selector,
            mut cases,
            ..
        } = self.guard_observations[previous].predicate.clone()
        else {
            return;
        };
        if let Some(exprs) = exprs {
            cases.push(exprs.to_vec());
        }
        let index = self.capture_guard(
            GuardPredicate::Match {
                selector,
                cases,
                last_case_positive: exprs.is_some(),
            },
            active,
        );
        *self.guard_stack.last_mut().expect("match guard frame") = Some(index);
    }

    fn capture_guard(&mut self, predicate: GuardPredicate, active: bool) -> usize {
        let expressions: Vec<&Expr> = match &predicate {
            GuardPredicate::If { tests, .. } => tests.iter().collect(),
            GuardPredicate::Match {
                selector, cases, ..
            } => selector.iter().chain(cases.iter().flatten()).collect(),
        };
        let loop_variable_dependent = expressions.iter().any(|expr| {
            let mut names = Vec::new();
            scalar_dependencies(expr, &mut names);
            names
                .into_iter()
                .any(|name| self.lookup_loop_var(name).is_some())
        });
        let unresolved_at_observation = expressions.iter().any(|expr| {
            self.eval_scalar_ast_requiring_defined_symbols(expr)
                .is_err()
        });
        let index = self.guard_observations.len();
        self.guard_observations.push(GuardObservation {
            key: (self.current_line_num, self.repetition_path.clone()),
            predicate,
            active,
            unresolved_at_observation,
            scope: self.symbol_scope.clone(),
            cpu: self.cpu,
            loop_variable_dependent,
        });
        index
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
        let result = match &guard.predicate {
            GuardPredicate::If {
                tests,
                last_test_positive,
            } => (|| {
                let mut active = true;
                for (index, expr) in tests.iter().enumerate() {
                    let value = self.immutable_guard_scalar(expr)?;
                    let positive = *last_test_positive && index + 1 == tests.len();
                    active &= (value != 0) == positive;
                }
                Some(active)
            })(),
            GuardPredicate::Match {
                selector,
                cases,
                last_case_positive,
            } => (|| {
                let value = match selector {
                    Some(expr) => self.immutable_guard_scalar(expr)? as u32,
                    None => 0,
                };
                for (index, arm) in cases.iter().enumerate() {
                    let mut matched = false;
                    for expr in arm {
                        if self.immutable_guard_scalar(expr)? as u32 == value {
                            matched = true;
                            break;
                        }
                    }
                    if matched {
                        return Some(*last_case_positive && index + 1 == cases.len());
                    }
                }
                Some(!last_case_positive)
            })(),
        };
        self.symbol_scope = saved_scope;
        self.cpu = saved_cpu;
        self.pass = saved_pass;
        result
    }

    fn immutable_guard_scalar(&self, expr: &Expr) -> Option<i64> {
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
        self.eval_expr_for_signed_scalar_context(expr).ok()
    }
}
