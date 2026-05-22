// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;
use registry::parser_from_line_with_registers;
use types::asm_value::AsmValue;
use types::processing::ProcessingOutcome;
use vm::vm_opasm::parse_statement_line_with_model;

pub const DEFAULT_MAX_LOOP_ITERATIONS: u32 = 65_536;

pub struct ForPlan {
    pub var_name: Option<String>,
    pub values: Vec<u32>,
}

pub fn parse_line_ast_for_repetition(
    asm_line: &AsmLine<'_>,
    src: &str,
    line_num: u32,
) -> Result<LineAst, ParseError> {
    if let Some(model) = asm_line.opthread_execution_model.as_ref() {
        let started = std::time::Instant::now();
        let res = parse_statement_line_with_model(
            model,
            asm_line.cpu.as_str(),
            None,
            src,
            line_num,
            &asm_line.register_checker,
        )
        .map(|(ast, _, _)| ast);
        let elapsed = started.elapsed();
        let bucket = if asm_line.pass == 1 {
            crate::phase_profile::PhaseBucket::Pass1ParseLineAst
        } else {
            crate::phase_profile::PhaseBucket::Pass2ParseLineAst
        };
        crate::phase_profile::record_execution_path(Some(bucket), "vm.parse", elapsed);
        return res;
    }

    let started = std::time::Instant::now();
    let mut parser =
        parser_from_line_with_registers(src, line_num, asm_line.register_checker.clone())?;
    match parser.process_opcore_statement_request() {
        ProcessingOutcome::Done(ast) => Ok(ast),
        ProcessingOutcome::Error(err) => Err(err),
        ProcessingOutcome::Return(_) => Err(ParseError {
            message: "Assembler statement delegated during repetition scan".to_string(),
            span: parser.end_span(),
        }),
    }
    .inspect(|_ast| {
        let elapsed = started.elapsed();
        let bucket = if asm_line.pass == 1 {
            crate::phase_profile::PhaseBucket::Pass1ParseLineAst
        } else {
            crate::phase_profile::PhaseBucket::Pass2ParseLineAst
        };
        crate::phase_profile::record_execution_path(Some(bucket), "rust.parse", elapsed);
    })
}

pub fn statement_parts(ast: &LineAst) -> Option<(Option<Label>, String, Vec<Expr>)> {
    let LineAst::Statement(statement) = ast else {
        return None;
    };
    let label = statement.label.clone();
    let mnemonic = statement.mnemonic.clone();
    let operands = statement.operands.clone();
    let mnemonic = mnemonic?;
    Some((label, mnemonic, operands))
}

pub fn is_for_directive_name(name: &str) -> bool {
    name.eq_ignore_ascii_case(".for")
}

pub fn is_for_like_directive_name(name: &str) -> bool {
    is_for_directive_name(name) || is_scoped_for_directive_name(name)
}

pub fn is_scoped_for_directive_name(name: &str) -> bool {
    name.eq_ignore_ascii_case(".bfor")
}

pub fn is_endfor_directive_name(name: &str) -> bool {
    name.eq_ignore_ascii_case(".endfor")
}

pub fn is_while_directive_name(name: &str) -> bool {
    name.eq_ignore_ascii_case(".while")
}

pub fn is_while_like_directive_name(name: &str) -> bool {
    is_while_directive_name(name) || is_scoped_while_directive_name(name)
}

pub fn is_scoped_while_directive_name(name: &str) -> bool {
    name.eq_ignore_ascii_case(".bwhile")
}

pub fn is_endwhile_directive_name(name: &str) -> bool {
    name.eq_ignore_ascii_case(".endwhile")
}

pub fn find_matching_endfor(
    lines: &[String],
    asm_line: &AsmLine<'_>,
    start_idx: usize,
    end_idx_exclusive: usize,
) -> Option<usize> {
    let mut depth = 1usize;
    for (idx, line) in lines
        .iter()
        .enumerate()
        .take(end_idx_exclusive)
        .skip(start_idx)
    {
        let line_num = (idx as u32).saturating_add(1);
        let Ok(ast) = parse_line_ast_for_repetition(asm_line, line, line_num) else {
            continue;
        };
        let Some((_, mnemonic, _)) = statement_parts(&ast) else {
            continue;
        };
        if is_for_like_directive_name(&mnemonic) {
            depth = depth.saturating_add(1);
            continue;
        }
        if is_endfor_directive_name(&mnemonic) {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                return Some(idx);
            }
        }
    }
    None
}

pub fn find_matching_endwhile(
    lines: &[String],
    asm_line: &AsmLine<'_>,
    start_idx: usize,
    end_idx_exclusive: usize,
) -> Option<usize> {
    let mut depth = 1usize;
    for (idx, line) in lines
        .iter()
        .enumerate()
        .take(end_idx_exclusive)
        .skip(start_idx)
    {
        let line_num = (idx as u32).saturating_add(1);
        let Ok(ast) = parse_line_ast_for_repetition(asm_line, line, line_num) else {
            continue;
        };
        let Some((_, mnemonic, _)) = statement_parts(&ast) else {
            continue;
        };
        if is_while_like_directive_name(&mnemonic) {
            depth = depth.saturating_add(1);
            continue;
        }
        if is_endwhile_directive_name(&mnemonic) {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                return Some(idx);
            }
        }
    }
    None
}

pub fn evaluate_for_plan(
    asm_line: &AsmLine<'_>,
    operands: &[Expr],
    max_loop_iterations: u32,
) -> Result<ForPlan, AstEvalError> {
    if operands.is_empty() {
        return Err(AstEvalError::directive(
            "Missing loop expression for .for",
            Span::default(),
        ));
    }

    let (var_name, values) = if operands.len() == 1 {
        let count = asm_line.eval_expr_for_non_negative_directive(&operands[0], ".for count")?;
        let values = (0..count).collect::<Vec<_>>();
        (None, values)
    } else if operands.len() == 2 {
        let var_name = match &operands[0] {
            Expr::Identifier(name, _) | Expr::Register(name, _) => name.clone(),
            _ => {
                return Err(AstEvalError::directive(
                    "Expected loop variable name before 'in'",
                    expr_span(&operands[0]),
                ));
            }
        };
        let iterable = evaluate_for_iterable_value(asm_line, &operands[1])?;
        let mut values = Vec::new();
        match iterable {
            AsmValue::List(items) => {
                for value in items {
                    let converted = u32::try_from(value).map_err(|_| {
                        AstEvalError::expression(
                            "loop iterator value out of supported range",
                            expr_span(&operands[1]),
                        )
                    })?;
                    values.push(converted);
                }
            }
            AsmValue::Range { start, end, step } => {
                let iterable = AsmValue::Range { start, end, step };
                if let Some(iter) = iterable.iter() {
                    for value in iter {
                        let converted = u32::try_from(value).map_err(|_| {
                            AstEvalError::expression(
                                "loop iterator value out of supported range",
                                expr_span(&operands[1]),
                            )
                        })?;
                        values.push(converted);
                    }
                }
            }
            AsmValue::Scalar(_) | AsmValue::Struct(_) | AsmValue::StructInstance(_) => {
                return Err(AstEvalError::directive(
                    "expected range or list after 'in', found scalar",
                    expr_span(&operands[1]),
                ));
            }
        }
        (Some(var_name), values)
    } else {
        return Err(AstEvalError::directive(
            "Expected '.for <count>' or '.for <var> in <iterable>'",
            expr_span(&operands[0]),
        ));
    };

    let iter_count = u32::try_from(values.len()).unwrap_or(u32::MAX);
    if iter_count > max_loop_iterations {
        return Err(AstEvalError::directive(
            format!("loop exceeded maximum iteration limit ({max_loop_iterations})"),
            expr_span(&operands[0]),
        ));
    }

    Ok(ForPlan { var_name, values })
}

fn evaluate_for_iterable_value(
    asm_line: &AsmLine<'_>,
    expr: &Expr,
) -> Result<AsmValue, AstEvalError> {
    match expr {
        Expr::List(items, _) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(i64::from(asm_line.eval_expr_for_scalar_context(item)?));
            }
            Ok(AsmValue::List(values))
        }
        Expr::Range {
            start,
            end,
            step,
            inclusive,
            span,
        } => {
            let start = i64::from(asm_line.eval_expr_for_scalar_context(start)?);
            let end = i64::from(asm_line.eval_expr_for_scalar_context(end)?);
            let step = match step {
                Some(step_expr) => {
                    Some(i64::from(asm_line.eval_expr_for_scalar_context(step_expr)?))
                }
                None => None,
            };
            AsmValue::try_range(start, end, *inclusive, step).map_err(|err| {
                let message = match err {
                    types::asm_value::AsmValueError::ZeroStep => {
                        "range step must be non-zero".to_string()
                    }
                    types::asm_value::AsmValueError::DirectionMismatch { .. } => {
                        "range step direction conflicts with start..end".to_string()
                    }
                    types::asm_value::AsmValueError::EndOverflow => {
                        "range end overflows supported integer range".to_string()
                    }
                };
                AstEvalError::expression(message, *span)
            })
        }
        _ => asm_line.eval_value_ast(expr),
    }
}

pub fn evaluate_while_condition(
    asm_line: &AsmLine<'_>,
    operands: &[Expr],
) -> Result<bool, AstEvalError> {
    let [condition] = operands else {
        let span = operands.first().map(expr_span).unwrap_or_default();
        return Err(AstEvalError::directive(
            "Expected '.while <condition>'",
            span,
        ));
    };

    let condition = asm_line.eval_expr_for_scalar_context(condition)?;
    Ok(condition != 0)
}

pub fn line_label(ast: &LineAst) -> Option<Label> {
    match ast {
        LineAst::Assignment(assignment) => Some(assignment.label.clone()),
        LineAst::Statement(statement) => statement.label.clone(),
        _ => None,
    }
}
