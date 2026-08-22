use families::hd6309::HD6309CpuHandler;
use families::intel8080::vm_encode_candidates_from_exprs as intel8080_vm_encode_candidates_from_exprs;
use families::m65816::input_shape_requires_runtime_family_support as m65816_input_shape_requires_runtime_family_support;
use families::m6800::module::vm_encode_candidates_for_operands as vm_candidates_m6800;
use families::m6800::M6800FamilyHandler;
use families::m6809::M6809CpuHandler;
use families::mos6502::{
    selector_input_from_exprs as mos6502_selector_input_from_exprs,
    OperandForce as Mos6502OperandForce,
};
use opcore::parser::Expr;
use registry::family::{expr_has_unstable_symbols, AssemblerContext, CpuHandler, FamilyHandler};
use registry::registry::VmEncodeCandidate;

use super::selector_encoding::selector_to_candidate;
use super::{HierarchyExecutionModel, ResolvedHierarchy, RuntimeBridgeError, SelectorOperandForce};

pub(super) struct SelectorInput<'a> {
    pub(super) shape_key: String,
    pub(super) expr0: Option<&'a Expr>,
    pub(super) expr1: Option<&'a Expr>,
    pub(super) extra_exprs: &'a [Expr],
    pub(super) force: Option<SelectorOperandForce>,
}

impl<'a> SelectorInput<'a> {
    pub(super) fn expr(&self, index: usize) -> Option<&'a Expr> {
        match index {
            0 => self.expr0,
            1 => self.expr1,
            _ => self.extra_exprs.get(index - 2).map(|expr| match expr {
                Expr::Immediate(inner, _) => inner.as_ref(),
                expr => expr,
            }),
        }
    }

    pub(super) fn expr_count(&self) -> usize {
        usize::from(self.expr0.is_some())
            + usize::from(self.expr1.is_some())
            + self.extra_exprs.len()
    }
}

pub(super) struct SelectorExprContext<'a> {
    pub(super) model: &'a HierarchyExecutionModel,
    pub(super) resolved: &'a ResolvedHierarchy,
    pub(super) assembler_ctx: &'a dyn AssemblerContext,
    pub(super) use_portable_eval: bool,
}

impl<'a> SelectorExprContext<'a> {
    fn is_unknown_symbol_error(message: &str) -> bool {
        let trimmed = message.trim_start();
        trimmed == "ope004" || trimmed.starts_with("ope004:")
    }

    fn allows_host_eval_compat_fallback(message: &str) -> bool {
        Self::is_unknown_symbol_error(message)
    }

    pub(super) fn new(
        model: &'a HierarchyExecutionModel,
        resolved: &'a ResolvedHierarchy,
        assembler_ctx: &'a dyn AssemblerContext,
    ) -> Self {
        let use_portable_eval =
            crate::rollout::package_runtime_default_enabled_for_family(resolved.family_id.as_str())
                && !assembler_ctx.force_host_expression_eval(resolved.family_id.as_str());
        Self {
            model,
            resolved,
            assembler_ctx,
            use_portable_eval,
        }
    }

    pub(super) fn eval_expr(&self, expr: &Expr) -> Result<i64, String> {
        if !self.use_portable_eval {
            return self.assembler_ctx.eval_expr(expr);
        }
        match crate::vm_opcore::evaluate_expression_for_assembler(
            self.model,
            self.resolved.cpu_id.as_str(),
            Some(self.resolved.dialect_id.as_str()),
            expr,
            self.assembler_ctx,
        ) {
            Ok(value) => Ok(value),
            Err(err) => {
                let message = err.to_string();
                if expr_uses_current_address(expr) {
                    return self.assembler_ctx.eval_expr(expr);
                }
                if Self::allows_host_eval_compat_fallback(message.as_str()) {
                    return self.assembler_ctx.eval_expr(expr);
                }
                Err(message)
            }
        }
    }

    pub(super) fn has_unstable_symbols(&self, expr: &Expr) -> Result<bool, String> {
        if !self.use_portable_eval {
            return Ok(expr_has_unstable_symbols(expr, self.assembler_ctx));
        }
        self.portable_structural_expr_has_unstable_symbols(expr)
    }

    fn portable_structural_expr_has_unstable_symbols(&self, expr: &Expr) -> Result<bool, String> {
        let child_is_unstable =
            |child: &Expr| self.portable_structural_expr_has_unstable_symbols(child);
        match expr {
            Expr::List(items, _) | Expr::Tuple(items, _) => {
                for item in items {
                    if child_is_unstable(item)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Expr::Index { base, index, .. } => {
                Ok(child_is_unstable(base)? || child_is_unstable(index)?)
            }
            Expr::Member { base, .. }
            | Expr::Immediate(base, _)
            | Expr::Indirect(base, _)
            | Expr::IndirectLong(base, _)
            | Expr::Unary { expr: base, .. } => child_is_unstable(base),
            Expr::StructLiteral { fields, .. } => {
                for (_, value) in fields {
                    if child_is_unstable(value)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Expr::Call { args, .. } => {
                for argument in args {
                    if child_is_unstable(argument)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => Ok(child_is_unstable(cond)?
                || child_is_unstable(then_expr)?
                || child_is_unstable(else_expr)?),
            Expr::Binary { left, right, .. } => {
                Ok(child_is_unstable(left)? || child_is_unstable(right)?)
            }
            Expr::Range {
                start, end, step, ..
            } => {
                if child_is_unstable(start)? || child_is_unstable(end)? {
                    return Ok(true);
                }
                match step.as_deref() {
                    Some(step) => child_is_unstable(step),
                    None => Ok(false),
                }
            }
            Expr::Number(_, _)
            | Expr::Register(_, _)
            | Expr::Placeholder(_)
            | Expr::String(_, _)
            | Expr::Dollar(_)
            | Expr::Error(_, _) => Ok(false),
            Expr::Identifier(_, _) => self.portable_scalar_expr_has_unstable_symbols(expr),
        }
    }

    fn portable_scalar_expr_has_unstable_symbols(&self, expr: &Expr) -> Result<bool, String> {
        match crate::vm_opcore::expression_has_unstable_symbols_for_assembler(
            self.model,
            self.resolved.cpu_id.as_str(),
            Some(self.resolved.dialect_id.as_str()),
            expr,
            self.assembler_ctx,
        ) {
            Ok(value) => Ok(value),
            Err(err) => {
                let message = err.to_string();
                if expr_uses_current_address(expr) {
                    return Ok(expr_has_unstable_symbols(expr, self.assembler_ctx));
                }
                if Self::allows_host_eval_compat_fallback(message.as_str()) {
                    return Ok(expr_has_unstable_symbols(expr, self.assembler_ctx));
                }
                Err(message)
            }
        }
    }
}

fn expr_uses_current_address(expr: &Expr) -> bool {
    match expr {
        Expr::Dollar(_) => true,
        Expr::List(items, _) | Expr::Tuple(items, _) => items.iter().any(expr_uses_current_address),
        Expr::Index { base, index, .. } => {
            expr_uses_current_address(base) || expr_uses_current_address(index)
        }
        Expr::Member { base, .. } => expr_uses_current_address(base),
        Expr::StructLiteral { fields, .. } => fields
            .iter()
            .any(|(_, field_expr)| expr_uses_current_address(field_expr)),
        Expr::Call { args, .. } => args.iter().any(expr_uses_current_address),
        Expr::Immediate(inner, _) | Expr::Indirect(inner, _) | Expr::IndirectLong(inner, _) => {
            expr_uses_current_address(inner)
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => {
            expr_uses_current_address(cond)
                || expr_uses_current_address(then_expr)
                || expr_uses_current_address(else_expr)
        }
        Expr::Unary { expr, .. } => expr_uses_current_address(expr),
        Expr::Binary { left, right, .. } => {
            expr_uses_current_address(left) || expr_uses_current_address(right)
        }
        Expr::Range {
            start, end, step, ..
        } => {
            expr_uses_current_address(start)
                || expr_uses_current_address(end)
                || step.as_deref().is_some_and(expr_uses_current_address)
        }
        Expr::Number(_, _)
        | Expr::Identifier(_, _)
        | Expr::Register(_, _)
        | Expr::Placeholder(_)
        | Expr::String(_, _)
        | Expr::Error(_, _) => false,
    }
}

fn selector_operand_force_from_mos6502(force: Mos6502OperandForce) -> SelectorOperandForce {
    match force {
        Mos6502OperandForce::DirectPage => SelectorOperandForce::DirectPage,
        Mos6502OperandForce::DataBank => SelectorOperandForce::DataBank,
        Mos6502OperandForce::ProgramBank => SelectorOperandForce::ProgramBank,
        Mos6502OperandForce::Long => SelectorOperandForce::Long,
    }
}

impl HierarchyExecutionModel {
    pub fn select_candidates_from_exprs_m6800(
        &self,
        resolved: &ResolvedHierarchy,
        mnemonic: &str,
        operands: &[Expr],
        ctx: &dyn AssemblerContext,
    ) -> Result<Option<Vec<VmEncodeCandidate>>, RuntimeBridgeError> {
        let family = M6800FamilyHandler::new();
        let parsed = match family.parse_operands(mnemonic, operands) {
            Ok(parsed) => parsed,
            Err(_) => return Ok(None),
        };

        let resolved_operands = if resolved.cpu_id.eq_ignore_ascii_case("hd6309") {
            HD6309CpuHandler::new().resolve_operands(mnemonic, &parsed, ctx)
        } else {
            M6809CpuHandler::new().resolve_operands(mnemonic, &parsed, ctx)
        }
        .map_err(RuntimeBridgeError::Resolve)?;

        let cpu_result = if resolved.cpu_id.eq_ignore_ascii_case("hd6309") {
            HD6309CpuHandler::new().encode_instruction(mnemonic, &resolved_operands, ctx)
        } else {
            M6809CpuHandler::new().encode_instruction(mnemonic, &resolved_operands, ctx)
        };
        let _native_bytes = match cpu_result {
            registry::family::EncodeResult::Ok(bytes) => bytes,
            registry::family::EncodeResult::Error(message, _) => {
                return Err(RuntimeBridgeError::Resolve(message))
            }
            registry::family::EncodeResult::NotFound => {
                match family.encode_instruction(mnemonic, &resolved_operands, ctx) {
                    registry::family::EncodeResult::Ok(bytes) => bytes,
                    registry::family::EncodeResult::Error(message, _) => {
                        return Err(RuntimeBridgeError::Resolve(message))
                    }
                    registry::family::EncodeResult::NotFound => return Ok(None),
                }
            }
        };

        let candidates = vm_candidates_m6800(resolved_operands.as_slice());
        if candidates.is_empty() {
            return Ok(None);
        }
        Ok(Some(candidates))
    }

    pub fn select_candidates_from_exprs_mos6502(
        &self,
        resolved: &ResolvedHierarchy,
        mnemonic: &str,
        operands: &[Expr],
        ctx: &dyn AssemblerContext,
    ) -> Result<Option<Vec<VmEncodeCandidate>>, RuntimeBridgeError> {
        let family_input = match mos6502_selector_input_from_exprs(mnemonic, operands) {
            Ok(Some(input)) => input,
            Ok(None) | Err(_) => return Ok(None),
        };
        let input = SelectorInput {
            shape_key: family_input.shape_key,
            expr0: family_input.expr0.as_ref(),
            expr1: family_input.expr1.as_ref(),
            extra_exprs: &[],
            force: family_input.force.map(selector_operand_force_from_mos6502),
        };
        self.select_candidates_from_package_shape(resolved, mnemonic, input, ctx)
    }

    pub(super) fn select_candidates_from_package_shape(
        &self,
        resolved: &ResolvedHierarchy,
        mnemonic: &str,
        input: SelectorInput<'_>,
        ctx: &dyn AssemblerContext,
    ) -> Result<Option<Vec<VmEncodeCandidate>>, RuntimeBridgeError> {
        let expr_ctx = SelectorExprContext::new(self, resolved, ctx);
        if input.shape_key.is_empty() {
            return Ok(None);
        }

        let upper_mnemonic = mnemonic.to_ascii_uppercase();
        let lower_mnemonic = mnemonic.to_ascii_lowercase();
        let Some(mnemonic_id) = self.interned_id(&lower_mnemonic) else {
            return Ok(None);
        };
        let shape_key = input.shape_key.to_ascii_lowercase();
        let Some(shape_id) = self.interned_id(&shape_key) else {
            return Ok(None);
        };
        if !resolved.cpu_id.eq_ignore_ascii_case("65816")
            && m65816_input_shape_requires_runtime_family_support(&input.shape_key)
        {
            return Err(RuntimeBridgeError::Resolve(
                self.non_m65816_force_error(&resolved.cpu_id),
            ));
        }
        let owner_order = self.scoped_owner_lookup_order(resolved);

        let unstable_expr = match input.expr0 {
            Some(expr) => expr_ctx
                .has_unstable_symbols(expr)
                .map_err(RuntimeBridgeError::Resolve)?,
            None => false,
        };
        let mut candidates = Vec::new();
        let mut candidate_error: Option<String> = None;
        let mut saw_selector = false;
        let mut selectors_scanned = 0usize;

        for (owner_tag, owner_id) in owner_order {
            let Some(owner_id) = owner_id else {
                continue;
            };
            let key = (owner_tag, owner_id, mnemonic_id, shape_id);
            let Some(selectors) = self.core.mode_selectors.get(&key) else {
                continue;
            };
            saw_selector = true;
            // The outer loop preserves dialect -> CPU -> family precedence.
            // Within one owner, later diagnostic bands are more specific.
            let mut owner_candidate_error: Option<(u16, String)> = None;

            let has_wider = selectors.iter().any(|entry| {
                entry.width_rank > 1
                    && self.mode_exists_for_owner(entry, owner_tag, owner_id, mnemonic_id)
            });

            for selector in selectors {
                selectors_scanned += 1;
                if selectors_scanned
                    > self
                        .core
                        .budget_limits
                        .max_selectors_scanned_per_instruction
                {
                    return Err(Self::budget_error(
                        "selector_scan_count",
                        self.core
                            .budget_limits
                            .max_selectors_scanned_per_instruction,
                        selectors_scanned,
                    ));
                }
                if unstable_expr && selector.unstable_widen && has_wider {
                    continue;
                }
                match selector_to_candidate(selector, &input, &upper_mnemonic, &expr_ctx) {
                    Ok(Some(candidate)) => {
                        candidates.push(candidate);
                        if candidates.len() > self.core.budget_limits.max_candidate_count {
                            return Err(Self::budget_error(
                                "candidate_count",
                                self.core.budget_limits.max_candidate_count,
                                candidates.len(),
                            ));
                        }
                    }
                    Ok(None) => {}
                    Err(message) => {
                        if owner_candidate_error
                            .as_ref()
                            .is_none_or(|(priority, _)| selector.priority > *priority)
                        {
                            owner_candidate_error = Some((selector.priority, message));
                        }
                    }
                }
            }
            if candidate_error.is_none() {
                candidate_error = owner_candidate_error.map(|(_, message)| message);
            }
        }

        if !candidates.is_empty() {
            return Ok(Some(candidates));
        }

        if let Some(force) = input.force {
            if !resolved.cpu_id.eq_ignore_ascii_case("65816") {
                return Err(RuntimeBridgeError::Resolve(
                    self.non_m65816_force_error(&resolved.cpu_id),
                ));
            }
            if let Some(message) = candidate_error {
                return Err(RuntimeBridgeError::Resolve(message));
            }
            if !saw_selector {
                return Err(RuntimeBridgeError::Resolve(
                    self.invalid_force_error(force, &upper_mnemonic),
                ));
            }
        }

        if let Some(message) = candidate_error {
            return Err(RuntimeBridgeError::Resolve(message));
        }

        // A zero-operand instruction has one unambiguous neutral candidate.
        // Packages therefore do not need to repeat an MSEL row for every
        // fixed instruction; the existence of its implied TABL entry is the
        // complete declaration.
        if input.shape_key.eq_ignore_ascii_case("implied") {
            let implied_mode = "implied";
            if let Some(mode_id) = self.interned_id(implied_mode) {
                let has_program = self
                    .scoped_owner_lookup_order(resolved)
                    .into_iter()
                    .filter_map(|(owner_tag, owner_id)| owner_id.map(|id| (owner_tag, id)))
                    .any(|(owner_tag, owner_id)| {
                        self.core.vm_programs.contains_key(&(
                            owner_tag,
                            owner_id,
                            mnemonic_id,
                            mode_id,
                        ))
                    });
                if has_program {
                    return Ok(Some(vec![VmEncodeCandidate {
                        mode_key: implied_mode.to_string(),
                        operand_bytes: Vec::new(),
                    }]));
                }
            }
        }

        Ok(None)
    }

    pub fn select_candidates_from_exprs_intel8080(
        &self,
        resolved: &ResolvedHierarchy,
        mnemonic: &str,
        operands: &[Expr],
        ctx: &dyn AssemblerContext,
    ) -> Result<Option<Vec<VmEncodeCandidate>>, RuntimeBridgeError> {
        intel8080_vm_encode_candidates_from_exprs(resolved.cpu_id.as_str(), mnemonic, operands, ctx)
            .map_err(RuntimeBridgeError::Resolve)
    }
}
