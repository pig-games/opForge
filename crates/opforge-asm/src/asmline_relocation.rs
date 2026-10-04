// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Preserve assignment relocation meaning independently of the numeric value.
//! A snapshot freezes both its value and its section identity; looking up its
//! initializer again at emission would observe later mutations instead.
use super::*;
use crate::state::SymbolRelocation;

impl AsmLine<'_> {
    /// Logical source bytes are appended to a concrete Hunk segment. Relocation
    /// addends are relative to that segment's base, including its existing bytes.
    pub(super) fn hunk_output_section(&self, name: &str) -> String {
        self.relayout
            .as_ref()
            .and_then(|plan| plan.mapped_sections.get(name))
            .map_or_else(|| name.into(), Clone::clone)
    }

    pub(super) fn record_symbol_relocation(&mut self, name: &str, expr: &Expr) {
        let provenance = self.expression_relocation(expr);
        self.layout
            .symbol_relocations
            .insert(name.into(), provenance);
    }

    fn expression_relocation(&self, expr: &Expr) -> SymbolRelocation {
        if self.expr_is_absolute_constant_symbol_expr(expr) {
            SymbolRelocation::Absolute
        } else if let Some(section) = self.hunk_abs32_target_section_for_data_expr(expr) {
            SymbolRelocation::Section(section)
        } else {
            SymbolRelocation::Unsupported
        }
    }

    pub(super) fn record_compound_symbol_relocation(
        &mut self,
        name: &str,
        op: AssignOp,
        rhs: &Expr,
        span: Span,
    ) {
        use SymbolRelocation::{Absolute, Section, Unsupported};
        let left = self.expression_relocation(&Expr::Identifier(name.into(), span));
        let right = self.expression_relocation(rhs);
        let provenance = match (left, right) {
            (Absolute, Absolute) => Absolute,
            (Section(section), Absolute) if matches!(op, AssignOp::Add | AssignOp::Sub) => {
                Section(section)
            }
            (Absolute, Section(section)) if op == AssignOp::Add => Section(section),
            (Section(left), Section(right)) if op == AssignOp::Sub && left == right => Absolute,
            _ => Unsupported,
        };
        self.layout
            .symbol_relocations
            .insert(name.into(), provenance);
    }
}
