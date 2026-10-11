// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Select actual symbol owners before interpreting remaining member operations.
use super::*;

impl AsmLine<'_> {
    pub(super) fn track_declaration_block_owner(&mut self, name: &str) {
        if matches!(self.profile_phase, AsmProfilePhase::Pass2) {
            return;
        }
        let owner = self.block_unit_stack.iter().rev().find_map(|(index, _)| {
            index
                .and_then(|index| self.reachable_blocks.get(index))
                .map(|block| block.symbol.clone())
        });
        // Assignment updates do not move a mutable declaration's source owner.
        self.declaration_block_owners
            .entry(name.to_ascii_uppercase())
            .or_insert(owner);
    }

    /// Only identifier/member chains are names. Indexes, calls and literals must
    /// retain their expression semantics rather than become textual symbols.
    pub(super) fn member_path(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(name, _) | Expr::Register(name, _) => Some(name.clone()),
            Expr::Member { base, field, .. } => {
                Some(format!("{}.{field}", Self::member_path(base)?))
            }
            _ => None,
        }
    }

    pub(super) fn member_owner_expression(&self, name: &str, span: Span) -> Option<Expr> {
        if self.lookup_loop_var(name.split('.').next()?).is_some() {
            return None;
        }
        let mut prefix = name;
        loop {
            if self.lookup_loop_var(prefix).is_some() {
                return None;
            }
            let owner = match self.resolve_scoped_name(prefix) {
                Ok(Some(owner)) => Some(owner),
                Ok(None) => self.resolve_scoped_value_name(prefix),
                Err(_) => return None,
            };
            if let Some(owner) = owner {
                let mut expr = Expr::Identifier(owner, span);
                for field in name[prefix.len()..]
                    .split('.')
                    .filter(|part| !part.is_empty())
                {
                    expr = Expr::Member {
                        base: Box::new(expr),
                        field: field.into(),
                        span,
                    };
                }
                return Some(expr);
            }
            prefix = prefix.rsplit_once('.')?.0;
        }
    }

    /// Unresolved guards and forward module qualifiers can postpone owner
    /// selection during collection without providing a symbol or typed value.
    pub(super) fn member_binding_is_deferred(&self, expr: &Expr) -> bool {
        // A forward import qualifier may later replace a scalar lexical prefix.
        // Defer this incomplete path during collection; finalized evaluation
        // must resolve the actual qualified owner or reject scalar members.
        if self.pass == 1 && self.symbol_scope.module_active.is_some() {
            if let Some(path) = Self::member_path(expr) {
                if path.contains('.') && self.resolve_scoped_name(&path).ok().flatten().is_none() {
                    if let Some(owner_expr) = self.member_owner_expression(&path, expr_span(expr)) {
                        let mut base = &owner_expr;
                        while let Expr::Member { base: inner, .. } = base {
                            base = inner;
                        }
                        if let Expr::Identifier(owner, _) = base {
                            if self.symbols.entry(owner).is_some()
                                && self.lookup_value_symbol(owner).is_none()
                            {
                                return true;
                            }
                        }
                    }
                }
            }
        }
        let Some(path) = Self::member_path(expr) else {
            return match expr {
                Expr::Binary { left, right, .. }
                | Expr::Index {
                    base: left,
                    index: right,
                    ..
                } => {
                    self.member_binding_is_deferred(left) || self.member_binding_is_deferred(right)
                }
                Expr::Unary { expr, .. }
                | Expr::Immediate(expr, _)
                | Expr::Indirect(expr, _)
                | Expr::IndirectLong(expr, _) => self.member_binding_is_deferred(expr),
                Expr::List(items, _) | Expr::Tuple(items, _) | Expr::Call { args: items, .. } => {
                    items
                        .iter()
                        .any(|item| self.member_binding_is_deferred(item))
                }
                Expr::StructLiteral { fields, .. } => fields
                    .iter()
                    .any(|(_, value)| self.member_binding_is_deferred(value)),
                _ => false,
            };
        };
        let path = path.to_ascii_uppercase();
        let owner_expr = self.member_owner_expression(&path, expr_span(expr));
        let mut base = owner_expr.as_ref();
        while let Some(Expr::Member { base: inner, .. }) = base {
            base = Some(inner);
        }
        let owner = match base {
            Some(Expr::Identifier(owner, _)) => owner.as_str(),
            _ => "",
        };
        if self
            .deferred_value_names
            .contains(&owner.to_ascii_uppercase())
        {
            return true;
        }
        self.qualified_name_candidates(&path)
            .is_ok_and(|candidates| {
                candidates.into_iter().any(|candidate| {
                    let candidate = candidate.to_ascii_uppercase();
                    self.deferred_declaration_names.iter().any(|name| {
                        name.len() > owner.len()
                            && (candidate == *name || candidate.starts_with(&format!("{name}.")))
                    })
                })
            })
    }

    pub(super) fn normalize_member_bindings(&self, expr: &mut Expr) {
        if let Some(path) = Self::member_path(expr) {
            if path.contains('.') {
                if let Some(normalized) = self.member_owner_expression(&path, expr_span(expr)) {
                    *expr = normalized;
                    return;
                }
            }
        }
        match expr {
            Expr::List(items, _) | Expr::Tuple(items, _) | Expr::Call { args: items, .. } => {
                for item in items {
                    self.normalize_member_bindings(item);
                }
            }
            Expr::Index { base, index, .. }
            | Expr::Binary {
                left: base,
                right: index,
                ..
            } => {
                self.normalize_member_bindings(base);
                self.normalize_member_bindings(index);
            }
            Expr::Member { base, .. }
            | Expr::Unary { expr: base, .. }
            | Expr::Indirect(base, _)
            | Expr::IndirectLong(base, _)
            | Expr::Immediate(base, _) => self.normalize_member_bindings(base),
            Expr::StructLiteral { fields, .. } => {
                for (_, value) in fields {
                    self.normalize_member_bindings(value);
                }
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => {
                for value in [cond, then_expr, else_expr] {
                    self.normalize_member_bindings(value);
                }
            }
            Expr::Range {
                start, end, step, ..
            } => {
                self.normalize_member_bindings(start);
                self.normalize_member_bindings(end);
                if let Some(step) = step {
                    self.normalize_member_bindings(step);
                }
            }
            _ => {}
        }
    }

    /// Defer owner selection until all executed declarations are available.
    /// A later longer declaration must supersede an earlier shorter base.
    pub(super) fn record_member_path_reference(&mut self, name: &str) {
        let root = name.split('.').next().unwrap_or(name);
        if self.lookup_loop_var(root).is_some() {
            return;
        }
        let source = self
            .block_unit_stack
            .iter()
            .any(|(index, _)| index.is_some())
            .then(|| self.current_unit_symbol.clone())
            .flatten();
        let module = self.symbol_scope.module_active.clone();
        if source.is_none() && module.is_none() {
            return;
        }
        let mut candidates = Vec::new();
        let mut prefix = name;
        loop {
            if prefix.contains('.') {
                if let Ok(names) = self.qualified_name_candidates(prefix) {
                    candidates.extend(names);
                }
            } else {
                candidates.extend(self.lexical_name_candidates(prefix));
            }
            let Some((parent, _)) = prefix.rsplit_once('.') else {
                break;
            };
            prefix = parent;
        }
        self.pending_unit_references.push(PendingUnitReference {
            source,
            name: name.into(),
            candidates,
            module,
            section: self.layout.current_section.clone(),
        });
    }
}
