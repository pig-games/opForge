// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Source-order provenance for typed immutable values consumed by iterable loops.
use super::asmline_constants::value_dependencies;
use super::*;
use crate::engine::repetition_trace::GuardKey;

impl AsmLine<'_> {
    /// Capture provenance at the definition, before later mutable assignments or
    /// alternative owners can change the meaning of its dependencies. Compound
    /// values retain normal evaluator semantics and never enter the scalar DAG.
    pub(super) fn capture_value_replay_certificate(&mut self, name: &str, expr: &Expr) {
        if self.activated_guard_certificates.is_empty() {
            return;
        }
        let certificate = self.value_replay_certificate(expr).map(|mut guards| {
            guards.extend(self.guard_stack.iter().flatten().filter_map(|index| {
                let key = &self.guard_observations[*index].key;
                self.activated_guard_certificates
                    .contains(key)
                    .then(|| key.clone())
            }));
            guards
        });
        self.value_replay_owners
            .insert(Self::value_symbol_key(name), certificate);
    }

    pub(super) fn value_replay_certificate(&self, expr: &Expr) -> Option<HashSet<GuardKey>> {
        if self.activated_guard_certificates.is_empty() {
            return None;
        }
        let mut names = Vec::new();
        if !value_dependencies(expr, &mut names) {
            return None;
        }
        let mut guards = HashSet::new();
        for name in names {
            if self.lookup_loop_var(name).is_some() {
                return None;
            }
            let resolved = self.resolve_scoped_name(name).ok()??;
            if self.symbols.entry(&resolved).is_none_or(|entry| entry.rw) {
                return None;
            }
            match self
                .value_replay_owners
                .get(&Self::value_symbol_key(&resolved))
            {
                Some(Some(owners)) => guards.extend(owners.iter().cloned()),
                Some(None) => return None,
                // A prior traversal's scalar certificate does not identify the
                // owner selected at this source position in the current retry.
                None => return None,
            }
        }
        Some(guards)
    }
}
