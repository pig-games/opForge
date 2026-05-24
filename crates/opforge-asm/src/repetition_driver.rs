// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use crate::error::{AsmError, AsmErrorKind, Diagnostic};
use crate::line::{repetition, AsmLine, CachedRuntimeParseResult};
use opcore::expression::{expr_span, AstEvalError, AstEvalErrorKind};
use opcore::scope::ScopeKind;
use types::asm_value::AsmValue;

#[derive(Clone, Copy)]
pub(crate) enum UnscopedRepeatKind {
    For,
    While,
}

fn ast_eval_error_kind_to_asm(kind: AstEvalErrorKind) -> AsmErrorKind {
    match kind {
        AstEvalErrorKind::Expression => AsmErrorKind::Expression,
        AstEvalErrorKind::Directive => AsmErrorKind::Directive,
        AstEvalErrorKind::Symbol => AsmErrorKind::Symbol,
        AstEvalErrorKind::Instruction => AsmErrorKind::Instruction,
    }
}

pub(crate) trait RepetitionPass {
    type Error;

    fn before_label_restriction_error(&mut self, _asm_line: &mut AsmLine<'_>, _line_num: u32) {}

    fn before_unmatched_end_error(&mut self, _asm_line: &mut AsmLine<'_>, _line_num: u32) {}

    fn emit_error(
        &mut self,
        diagnostic: Diagnostic,
        all_lines: &[String],
    ) -> Result<(), Self::Error>;

    fn observe_loop_iterations(
        &mut self,
        line_num: u32,
        iterations: u32,
        all_lines: &[String],
    ) -> Result<(), Self::Error>;

    fn execute_regular_line(
        &mut self,
        asm_line: &mut AsmLine<'_>,
        src: &str,
        line_num: u32,
        addr: &mut u32,
        parsed_line: Option<CachedRuntimeParseResult>,
        all_lines: &[String],
    ) -> Result<(), Self::Error>;
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn execute_lines<C: RepetitionPass>(
    ctx: &mut C,
    lines: &[String],
    start_idx: usize,
    end_idx_exclusive: usize,
    asm_line: &mut AsmLine<'_>,
    addr: &mut u32,
    unscoped_repeat_kind: Option<UnscopedRepeatKind>,
    max_loop_iterations: u32,
) -> Result<(), C::Error> {
    let mut idx = start_idx;
    while idx < end_idx_exclusive {
        let line_num = u32::try_from(idx)
            .unwrap_or(u32::MAX.saturating_sub(1))
            .saturating_add(1);
        let src = &lines[idx];

        let parsed_line = repetition::parse_line_for_repetition(asm_line, src, line_num).ok();
        if let Some(parsed) = parsed_line.as_ref() {
            let statement_parts = repetition::statement_parts(&parsed.ast);

            if let Some((_, mnemonic, _)) = statement_parts.as_ref() {
                if asm_line.cond_stack.skipping() {
                    if repetition::is_for_like_directive_name(mnemonic) {
                        let Some(end_idx) = repetition::find_matching_endfor(
                            lines,
                            asm_line,
                            idx.saturating_add(1),
                            end_idx_exclusive,
                        ) else {
                            let message =
                                format!("unterminated {mnemonic} (opened at line {line_num})");
                            ctx.emit_error(
                                Diagnostic::new(
                                    line_num,
                                    crate::error::Severity::Error,
                                    AsmError::new(AsmErrorKind::Directive, &message, None),
                                ),
                                lines,
                            )?;
                            return Ok(());
                        };
                        idx = end_idx.saturating_add(1);
                        continue;
                    }
                    if repetition::is_while_like_directive_name(mnemonic) {
                        let Some(end_idx) = repetition::find_matching_endwhile(
                            lines,
                            asm_line,
                            idx.saturating_add(1),
                            end_idx_exclusive,
                        ) else {
                            let message =
                                format!("unterminated {mnemonic} (opened at line {line_num})");
                            ctx.emit_error(
                                Diagnostic::new(
                                    line_num,
                                    crate::error::Severity::Error,
                                    AsmError::new(AsmErrorKind::Directive, &message, None),
                                ),
                                lines,
                            )?;
                            return Ok(());
                        };
                        idx = end_idx.saturating_add(1);
                        continue;
                    }
                    if repetition::is_endfor_directive_name(mnemonic)
                        || repetition::is_endwhile_directive_name(mnemonic)
                    {
                        idx = idx.saturating_add(1);
                        continue;
                    }
                }
            }

            if !asm_line.cond_stack.skipping() {
                if let Some(repeat_kind) = unscoped_repeat_kind {
                    if let Some(label) = repetition::line_label(&parsed.ast) {
                        ctx.before_label_restriction_error(asm_line, line_num);
                        let message = match repeat_kind {
                            UnscopedRepeatKind::For => format!(
                                "label '{}' not allowed inside .for (use .bfor for scoped repetition)",
                                label.name
                            ),
                            UnscopedRepeatKind::While => format!(
                                "label '{}' not allowed inside .while (use .bwhile for scoped repetition)",
                                label.name
                            ),
                        };
                        ctx.emit_error(
                            Diagnostic::new(
                                line_num,
                                crate::error::Severity::Error,
                                AsmError::new(AsmErrorKind::Directive, &message, None),
                            )
                            .with_column(Some(label.span.col_start)),
                            lines,
                        )?;
                        idx = idx.saturating_add(1);
                        continue;
                    }
                }
            }

            if let Some((label, mnemonic, operands)) = statement_parts {
                if repetition::is_endfor_directive_name(&mnemonic)
                    || repetition::is_endwhile_directive_name(&mnemonic)
                {
                    ctx.before_unmatched_end_error(asm_line, line_num);
                    let (message, column) = if let Some(label) = label {
                        (
                            "label not allowed on .endfor / .endwhile".to_string(),
                            Some(label.span.col_start),
                        )
                    } else if repetition::is_endwhile_directive_name(&mnemonic) {
                        (".endwhile without matching .while".to_string(), None)
                    } else {
                        (".endfor without matching .for".to_string(), None)
                    };
                    ctx.emit_error(
                        Diagnostic::new(
                            line_num,
                            crate::error::Severity::Error,
                            AsmError::new(AsmErrorKind::Directive, &message, None),
                        )
                        .with_column(column),
                        lines,
                    )?;
                    idx = idx.saturating_add(1);
                    continue;
                }

                if repetition::is_for_like_directive_name(&mnemonic) {
                    let scoped_repeat = repetition::is_scoped_for_directive_name(&mnemonic);
                    let Some(end_idx) = repetition::find_matching_endfor(
                        lines,
                        asm_line,
                        idx.saturating_add(1),
                        end_idx_exclusive,
                    ) else {
                        let message =
                            format!("unterminated {mnemonic} (opened at line {line_num})");
                        ctx.emit_error(
                            Diagnostic::new(
                                line_num,
                                crate::error::Severity::Error,
                                AsmError::new(AsmErrorKind::Directive, &message, None),
                            ),
                            lines,
                        )?;
                        return Ok(());
                    };

                    let plan = match repetition::evaluate_for_plan(
                        asm_line,
                        &operands,
                        max_loop_iterations,
                    ) {
                        Ok(plan) => plan,
                        Err(err) => {
                            ctx.emit_error(
                                Diagnostic::new(
                                    line_num,
                                    crate::error::Severity::Error,
                                    AsmError::new(
                                        ast_eval_error_kind_to_asm(err.error.kind()),
                                        err.error.message(),
                                        None,
                                    ),
                                )
                                .with_column(Some(err.span.col_start)),
                                lines,
                            )?;
                            idx = end_idx.saturating_add(1);
                            continue;
                        }
                    };

                    ctx.observe_loop_iterations(
                        line_num,
                        u32::try_from(plan.values.len()).unwrap_or(u32::MAX),
                        lines,
                    )?;

                    let mut iteration_bases = Vec::with_capacity(plan.values.len());
                    let mut iteration_scopes = Vec::with_capacity(plan.values.len());
                    for value in &plan.values {
                        if scoped_repeat {
                            asm_line
                                .symbol_scope
                                .scope_stack
                                .push_anonymous_with_kind(ScopeKind::Repeat);
                            asm_line.push_visibility();
                        }
                        if scoped_repeat && label.is_some() {
                            let base_addr = asm_line.current_addr(*addr).unwrap_or(*addr);
                            iteration_bases.push(i64::from(base_addr));
                            iteration_scopes.push(
                                asm_line
                                    .symbol_scope
                                    .scope_stack
                                    .prefix(asm_line.symbol_scope.scope_stack.depth()),
                            );
                        }
                        if let Some(var_name) = plan.var_name.as_deref() {
                            asm_line.push_loop_var(var_name, *value);
                        }
                        execute_lines(
                            ctx,
                            lines,
                            idx.saturating_add(1),
                            end_idx,
                            asm_line,
                            addr,
                            if scoped_repeat {
                                None
                            } else {
                                Some(UnscopedRepeatKind::For)
                            },
                            max_loop_iterations,
                        )?;
                        if plan.var_name.is_some() {
                            asm_line.pop_loop_var();
                        }
                        if scoped_repeat {
                            let _ = asm_line
                                .symbol_scope
                                .scope_stack
                                .pop_expected(ScopeKind::Repeat);
                            let _ = asm_line.pop_visibility();
                        }
                    }

                    if scoped_repeat {
                        if let Some(loop_label) = label.as_ref() {
                            let full_name = asm_line.scoped_define_name(&loop_label.name);
                            asm_line.set_value_symbol(&full_name, AsmValue::List(iteration_bases));
                            asm_line.set_repeat_iteration_scopes(&full_name, iteration_scopes);
                        }
                    }

                    idx = end_idx.saturating_add(1);
                    continue;
                }

                if repetition::is_while_like_directive_name(&mnemonic) {
                    let scoped_repeat = repetition::is_scoped_while_directive_name(&mnemonic);
                    let Some(end_idx) = repetition::find_matching_endwhile(
                        lines,
                        asm_line,
                        idx.saturating_add(1),
                        end_idx_exclusive,
                    ) else {
                        let message =
                            format!("unterminated {mnemonic} (opened at line {line_num})");
                        ctx.emit_error(
                            Diagnostic::new(
                                line_num,
                                crate::error::Severity::Error,
                                AsmError::new(AsmErrorKind::Directive, &message, None),
                            ),
                            lines,
                        )?;
                        return Ok(());
                    };

                    let mut while_error: Option<AstEvalError> = None;
                    let mut loop_count = 0u32;
                    let mut iteration_bases = Vec::new();
                    let mut iteration_scopes = Vec::new();

                    loop {
                        let should_continue =
                            match repetition::evaluate_while_condition(asm_line, &operands) {
                                Ok(value) => value,
                                Err(err) => {
                                    while_error = Some(err);
                                    break;
                                }
                            };
                        if !should_continue {
                            break;
                        }

                        let next_count = loop_count.saturating_add(1);
                        if next_count > max_loop_iterations {
                            while_error = Some(AstEvalError::directive(
                                format!(
                                    "loop exceeded maximum iteration limit ({max_loop_iterations})"
                                ),
                                operands.first().map(expr_span).unwrap_or_default(),
                            ));
                            break;
                        }
                        loop_count = next_count;

                        if scoped_repeat {
                            asm_line
                                .symbol_scope
                                .scope_stack
                                .push_anonymous_with_kind(ScopeKind::Repeat);
                            asm_line.push_visibility();
                        }
                        if scoped_repeat && label.is_some() {
                            let base_addr = asm_line.current_addr(*addr).unwrap_or(*addr);
                            iteration_bases.push(i64::from(base_addr));
                            iteration_scopes.push(
                                asm_line
                                    .symbol_scope
                                    .scope_stack
                                    .prefix(asm_line.symbol_scope.scope_stack.depth()),
                            );
                        }

                        execute_lines(
                            ctx,
                            lines,
                            idx.saturating_add(1),
                            end_idx,
                            asm_line,
                            addr,
                            if scoped_repeat {
                                None
                            } else {
                                Some(UnscopedRepeatKind::While)
                            },
                            max_loop_iterations,
                        )?;

                        if scoped_repeat {
                            let _ = asm_line
                                .symbol_scope
                                .scope_stack
                                .pop_expected(ScopeKind::Repeat);
                            let _ = asm_line.pop_visibility();
                        }
                    }

                    if let Some(err) = while_error {
                        ctx.emit_error(
                            Diagnostic::new(
                                line_num,
                                crate::error::Severity::Error,
                                AsmError::new(
                                    ast_eval_error_kind_to_asm(err.error.kind()),
                                    err.error.message(),
                                    None,
                                ),
                            )
                            .with_column(Some(err.span.col_start)),
                            lines,
                        )?;
                        idx = end_idx.saturating_add(1);
                        continue;
                    }

                    ctx.observe_loop_iterations(line_num, loop_count, lines)?;

                    if scoped_repeat {
                        if let Some(loop_label) = label.as_ref() {
                            let full_name = asm_line.scoped_define_name(&loop_label.name);
                            asm_line.set_value_symbol(&full_name, AsmValue::List(iteration_bases));
                            asm_line.set_repeat_iteration_scopes(&full_name, iteration_scopes);
                        }
                    }

                    idx = end_idx.saturating_add(1);
                    continue;
                }
            }
        }

        let parsed_line =
            parsed_line.filter(|parsed| asm_line.can_process_cached_runtime_parse(parsed));
        ctx.execute_regular_line(asm_line, src, line_num, addr, parsed_line, lines)?;
        idx = idx.saturating_add(1);
    }

    Ok(())
}
