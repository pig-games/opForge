// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Construct immutable iteration records at scope close. Member evaluation and
//! address arithmetic belong to the shared expression VM, not scope-name replay.
use super::*;
use types::asm_value::ScalarProvenance;

impl AsmLine<'_> {
    pub(super) fn scalar_with_section(&self, value: i64, section: Option<&str>) -> AsmValue {
        section.map_or(AsmValue::Scalar(value), |section| {
            AsmValue::RelocatableScalar {
                value,
                provenance: ScalarProvenance::Section(section.into()),
            }
        })
    }

    pub(super) fn scalar_with_symbol_provenance(&self, name: &str, value: i64) -> AsmValue {
        let provenance = self
            .layout
            .symbol_relocations
            .get(name)
            .cloned()
            .unwrap_or_else(|| {
                if self.layout.absolute_constant_symbols.contains(name) {
                    ScalarProvenance::Absolute
                } else {
                    self.resolved_symbol_section_name(name)
                        .map_or(ScalarProvenance::Absolute, ScalarProvenance::Section)
                }
            });
        if provenance == ScalarProvenance::Absolute {
            AsmValue::Scalar(value)
        } else {
            AsmValue::RelocatableScalar { value, provenance }
        }
    }

    pub(crate) fn capture_repeat_record(
        &self,
        base_addr: u32,
        section: Option<&str>,
        scope: &str,
    ) -> AsmValue {
        let prefix = format!("{scope}.");
        let mut fields = Vec::new();
        for entry in self.symbols.entries() {
            let Some(field) = entry
                .name
                .strip_prefix(&prefix)
                .filter(|name| !name.contains('.'))
            else {
                continue;
            };
            let value = self
                .lookup_value_symbol(&entry.name)
                .cloned()
                .unwrap_or_else(|| {
                    let scalar = self
                        .scalar_value_symbols
                        .get(&Self::value_symbol_key(&entry.name))
                        .copied()
                        .unwrap_or(i64::from(entry.val));
                    self.scalar_with_symbol_provenance(&entry.name, scalar)
                });
            fields.push((field.to_string(), value));
        }
        // Nested named repetitions have compound bindings without scalar symbol
        // table entries. Snapshot these too, retaining their members and targets.
        let prefix = prefix.to_ascii_uppercase();
        for (name, value) in &self.value_symbols {
            let Some(field) = name
                .strip_prefix(&prefix)
                .filter(|name| !name.contains('.'))
            else {
                continue;
            };
            if !fields
                .iter()
                .any(|(existing, _)| existing.eq_ignore_ascii_case(field))
            {
                fields.push((field.to_string(), value.clone()));
            }
        }
        AsmValue::Record {
            scalar: Box::new(self.scalar_with_section(i64::from(base_addr), section)),
            fields,
        }
    }

    pub(super) fn expression_uses_typed_value(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Index { .. } | Expr::Member { .. } => true,
            Expr::Unary { expr: base, .. }
            | Expr::Immediate(base, _)
            | Expr::Indirect(base, _)
            | Expr::IndirectLong(base, _) => self.expression_uses_typed_value(base),
            Expr::Binary { left, right, .. } => {
                self.expression_uses_typed_value(left) || self.expression_uses_typed_value(right)
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => [cond, then_expr, else_expr]
                .iter()
                .any(|expr| self.expression_uses_typed_value(expr)),
            Expr::List(items, _) | Expr::Tuple(items, _) | Expr::Call { args: items, .. } => items
                .iter()
                .any(|expr| self.expression_uses_typed_value(expr)),
            Expr::Identifier(name, _) | Expr::Register(name, _) => {
                self.record_member_expression(name).is_some()
                    || matches!(
                        AssemblerContext::value_symbol(self, name),
                        Some(
                            AsmValue::Record { .. }
                                | AsmValue::Values(_)
                                | AsmValue::RelocatableScalar { .. }
                        )
                    )
            }
            _ => false,
        }
    }

    // A dotted spelling can denote a qualified symbol or a member. Resolve the
    // longest actual record binding at compilation; GetMember owns evaluation.
    pub(super) fn record_member_expression(&self, name: &str) -> Option<Expr> {
        let parts: Vec<_> = name.split('.').collect();
        for split in (1..parts.len()).rev() {
            let base = parts[..split].join(".");
            let Some(full) = self.resolve_scoped_value_name(&base) else {
                continue;
            };
            if !matches!(
                self.lookup_value_symbol(&full),
                Some(AsmValue::Record { .. })
            ) {
                continue;
            }
            let mut expr = Expr::Identifier(base, Span::default());
            for field in &parts[split..] {
                expr = Expr::Member {
                    base: Box::new(expr),
                    field: field.to_string(),
                    span: Span::default(),
                };
            }
            return Some(expr);
        }
        None
    }

    pub(super) fn eval_record_member_name(
        &self,
        name: &str,
    ) -> Option<Result<AsmValue, AstEvalError>> {
        self.eval_shared_value(&self.record_member_expression(name)?)
    }

    pub(super) fn eval_shared_value(&self, expr: &Expr) -> Option<Result<AsmValue, AstEvalError>> {
        let model = self.opthread_execution_model.as_ref()?;
        // Typed record operations have one shared semantic implementation, even
        // when the scalar/operand comparison route is explicitly host-forced.
        Some(
            vm::vm_opcore::evaluate_expression_value_for_assembler(
                model,
                self.cpu.as_str(),
                None,
                expr,
                self,
            )
            .map_err(|message| {
                AstEvalError::expression(
                    Self::normalize_vm_eval_error_message(&message),
                    expr_span(expr),
                )
            }),
        )
    }

    pub(super) fn typed_expression_provenance(&self, expr: &Expr) -> Option<ScalarProvenance> {
        if !self.expression_uses_typed_value(expr) {
            return None;
        }
        let value = match expr {
            Expr::Identifier(name, _) | Expr::Register(name, _) => self
                .eval_record_member_name(name)
                .or_else(|| self.eval_shared_value(expr))?,
            _ => self.eval_shared_value(expr)?,
        };
        value.ok()?.scalar_provenance()
    }
}
