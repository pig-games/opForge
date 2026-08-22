use crate::selector_encoding_utils::{
    encode_fixed_width_value, encode_relative_offset, mode_key_operand_size,
};
use families::m65816::encode_runtime_operand_plan as encode_m65816_runtime_operand_plan;
use opcore::parser::{BinaryOp, Expr, UnaryOp};
use package::{
    ModeSelectorDescriptor, MODE_SELECTOR_PLAN_BOUNDED_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_TUPLE_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_CALL_ARG_MEMBER_PREFIX, MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX, MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX,
    MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR, MODE_SELECTOR_PLAN_DISTINCT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_DUPLICATE_REGISTER_PREFIX, MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX,
    MODE_SELECTOR_PLAN_IMMEDIATE_PREFIX, MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_IDENTITY_SCALE_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_NONIDENTITY_SCALE_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX, MODE_SELECTOR_PLAN_INPUT_SEPARATOR,
    MODE_SELECTOR_PLAN_LITERAL_PREFIX, MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR,
    MODE_SELECTOR_PLAN_MEMBER_INDIRECT_PREFIX, MODE_SELECTOR_PLAN_MEMBER_PREFIX,
    MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX, MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_NAMED_REGISTER_RANGE_COUNT_PREFIX,
    MODE_SELECTOR_PLAN_NAMED_REGISTER_RANGE_PREFIX, MODE_SELECTOR_PLAN_OUT_OF_RANGE_PREFIX,
    MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX, MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX,
    MODE_SELECTOR_PLAN_REGISTER_INDEX_XOR_PREFIX, MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX,
    MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX,
    MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR, MODE_SELECTOR_PLAN_REGISTER_SEQUENCE_PREFIX,
    MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX, MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX,
    MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX, MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX,
    MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX, MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX,
    MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX, MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR,
    MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX, MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR,
};
use registry::registry::VmEncodeCandidate;

use super::selector_bridge::{SelectorExprContext, SelectorInput};

pub(super) fn selector_to_candidate(
    selector: &ModeSelectorDescriptor,
    input: &SelectorInput<'_>,
    upper_mnemonic: &str,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Option<VmEncodeCandidate>, String> {
    if let Some(guarded_plan) = selector
        .operand_plan
        .strip_prefix(MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX)
    {
        let Some((requirement, nested_plan)) = guarded_plan.split_once(';') else {
            return Err("state-required operand plan is missing its nested plan".to_string());
        };
        let (requirement, mismatch_diagnostic) = requirement
            .split_once('?')
            .map_or((requirement, None), |(requirement, diagnostic)| {
                (requirement, Some(diagnostic))
            });
        let Some((key, allowed_values)) = requirement.split_once('=') else {
            return Err("state-required operand plan is missing its allowed values".to_string());
        };
        if key.is_empty() || allowed_values.is_empty() {
            return Err("state-required operand plan has an empty key or value set".to_string());
        }
        let allowed = allowed_values
            .split('+')
            .map(|value| {
                value.parse::<u32>().map_err(|_| {
                    "state-required operand plan contains an invalid value".to_string()
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let actual = expr_ctx.assembler_ctx.cpu_state_flag(key);
        if actual.is_none_or(|actual| !allowed.contains(&actual)) {
            if let Some(diagnostic) = mismatch_diagnostic {
                if diagnostic.is_empty() {
                    return Err(
                        "state-required operand plan has an empty mismatch diagnostic".to_string(),
                    );
                }
                return Err(expr_ctx.model.diag_message(
                    diagnostic,
                    "runtime state requirement was not satisfied",
                    &[("mnemonic", upper_mnemonic)],
                ));
            }
            return Ok(None);
        }
        let mut nested_selector = selector.clone();
        nested_selector.operand_plan = nested_plan.to_string();
        return selector_to_candidate(&nested_selector, input, upper_mnemonic, expr_ctx);
    }
    let mut relocation_free = false;
    let mut output_fixups = Vec::new();
    let mode_key = selector.mode_key.to_ascii_lowercase();
    let Some(mode_operand_size) = mode_key_operand_size(mode_key.as_str()) else {
        return Ok(None);
    };
    let mut operand_bytes = if let Some(reject_spec) = selector
        .operand_plan
        .strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX)
    {
        let Some((diagnostic_code, input_plan)) =
            reject_spec.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR)
        else {
            return Err("semantic reject operand plan is missing its input list".to_string());
        };
        let Some(captures) = semantic_reject_plan_inputs(input_plan, input, expr_ctx)? else {
            return Ok(None);
        };
        let diagnostic_mnemonic = upper_mnemonic
            .split_once('.')
            .map_or(upper_mnemonic, |(base, _)| base);
        let mut diagnostic_values =
            vec![("mnemonic", diagnostic_mnemonic), ("form", upper_mnemonic)];
        diagnostic_values.extend(captures.iter().map(|(key, value)| (*key, value.as_str())));
        return Err(expr_ctx.model.diag_message(
            diagnostic_code,
            "instruction rejected by package selector",
            diagnostic_values.as_slice(),
        ));
    } else if let Some(program_spec) = selector
        .operand_plan
        .strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX)
    {
        let (program_and_inputs, diagnostic_code) = split_semantic_diagnostic(program_spec);
        let Some((program_id, input_plan)) =
            program_and_inputs.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR)
        else {
            return Err("semantic branch operand plan is missing its input list".to_string());
        };
        let inputs = input_plan.split(',').collect::<Vec<_>>();
        if inputs.len() != 4 || inputs[1] != "expr0" {
            return Err(
                "semantic branch operand plan requires opcode,expr0,candidate,class".to_string(),
            );
        }
        let opcode = inputs[0]
            .parse::<u8>()
            .map_err(|_| "semantic branch opcode is invalid".to_string())?;
        let requested_candidate = if inputs[2] == "auto" {
            None
        } else {
            Some(
                inputs[2]
                    .parse::<u8>()
                    .map_err(|_| "semantic branch candidate is invalid".to_string())?,
            )
        };
        let automatic_class = inputs[3]
            .parse::<u8>()
            .map_err(|_| "semantic branch automatic class is invalid".to_string())?;
        let target_expr = input
            .expr0
            .ok_or_else(|| "semantic branch operand plan requires one expression".to_string())?;
        let unstable = expr_ctx.has_unstable_symbols(target_expr)?;
        // The first symbol-discovery pass needs the package's unresolved
        // placeholder. Once a later pass has an address, even an explicitly
        // sized branch must resolve it so local scope selection and hard range
        // checks observe the final target.
        let defer_unstable = (unstable && expr_ctx.assembler_ctx.pass() == 1)
            || (requested_candidate.is_some()
                && expr_ctx.assembler_ctx.should_defer_unstable_branch_target()
                && registry::expr_has_symbol_references(target_expr));
        let target = if defer_unstable {
            crate::fixup_vm::PortableDeferredValue::Unresolved
        } else {
            crate::fixup_vm::PortableDeferredValue::Resolved(expr_ctx.eval_expr(target_expr)?)
        };
        let result = expr_ctx
            .model
            .execute_branch_program(
                expr_ctx.resolved,
                program_id,
                &[i64::from(opcode)],
                &[target],
                crate::branch_vm::PortableBranchRequest {
                    requested_candidate,
                    previous_output_size: None,
                    automatic_class,
                },
                crate::branch_vm::PortableBranchContext {
                    position: i64::from(expr_ctx.assembler_ctx.current_address()),
                },
            )
            .map_err(|err| {
                diagnostic_code.map_or_else(
                    || match &err {
                        crate::runtime_error::RuntimeBridgeError::BranchVm(
                            crate::branch_vm::BranchVmError::ValueOutOfRange { .. },
                        ) => format!("{upper_mnemonic} branch displacement out of range"),
                        _ => err.to_string(),
                    },
                    |code| {
                        expr_ctx.model.diag_message(
                            code,
                            err.to_string().as_str(),
                            &[("mnemonic", upper_mnemonic)],
                        )
                    },
                )
            })?;
        relocation_free = true;
        vec![result.bytes]
    } else if let Some(program_spec) = selector
        .operand_plan
        .strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX)
    {
        let (sequence, diagnostic_code) = split_semantic_diagnostic(program_spec);
        let mut encoded = Vec::new();
        for step in sequence.split(';') {
            let Some((kind, program_and_inputs)) = step.split_once(':') else {
                return Err("semantic sequence step is missing its kind".to_string());
            };
            let Some((program_id, input_plan)) =
                program_and_inputs.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR)
            else {
                return Err("semantic sequence step is missing its input list".to_string());
            };
            let step_bytes = match kind {
                "match" => {
                    let Some(_) =
                        semantic_plan_inputs(input_plan, input, expr_ctx).map_err(|err| {
                            diagnostic_code.map_or(err.clone(), |code| {
                                expr_ctx.model.diag_message(
                                    code,
                                    err.as_str(),
                                    &[("mnemonic", upper_mnemonic)],
                                )
                            })
                        })?
                    else {
                        return Ok(None);
                    };
                    Vec::new()
                }
                "encode" => {
                    let Some(values) =
                        semantic_plan_inputs(input_plan, input, expr_ctx).map_err(|err| {
                            diagnostic_code.map_or(err.clone(), |code| {
                                expr_ctx.model.diag_message(
                                    code,
                                    err.as_str(),
                                    &[("mnemonic", upper_mnemonic)],
                                )
                            })
                        })?
                    else {
                        return Ok(None);
                    };
                    expr_ctx
                        .model
                        .execute_encoding_program(expr_ctx.resolved, program_id, values.as_slice())
                        .map_err(|err| err.to_string())?
                }
                "fixup" => {
                    let Some(values) = semantic_fixup_plan_inputs(input_plan, input, expr_ctx)?
                    else {
                        return Ok(None);
                    };
                    let result = expr_ctx
                        .model
                        .execute_fixup_program(
                            expr_ctx.resolved,
                            program_id,
                            values.as_slice(),
                            crate::fixup_vm::PortableFixupContext {
                                position: i64::from(expr_ctx.assembler_ctx.current_address()),
                            },
                        )
                        .map_err(|err| {
                            let err = err.to_string();
                            diagnostic_code.map_or(err.clone(), |code| {
                                expr_ctx.model.diag_message(
                                    code,
                                    err.as_str(),
                                    &[("mnemonic", upper_mnemonic)],
                                )
                            })
                        })?;
                    let step_offset = u32::try_from(encoded.len())
                        .map_err(|_| "semantic sequence output offset exceeds supported range")?;
                    for mut fixup in result.fixups {
                        fixup.offset = fixup.offset.checked_add(step_offset).ok_or_else(|| {
                            "semantic sequence output fixup offset exceeds supported range"
                                .to_string()
                        })?;
                        output_fixups.push(fixup);
                    }
                    relocation_free = output_fixups.is_empty();
                    result.bytes
                }
                _ => {
                    return Err(format!(
                        "semantic sequence step kind '{kind}' is unsupported"
                    ))
                }
            };
            encoded.extend(step_bytes);
        }
        if let Some(code) = diagnostic_code {
            if encoded.is_empty() {
                return Err(expr_ctx.model.diag_message(
                    code,
                    "semantic sequence emitted no bytes",
                    &[("mnemonic", upper_mnemonic)],
                ));
            }
        }
        vec![encoded]
    } else if let Some(program_spec) = selector
        .operand_plan
        .strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX)
    {
        let (program_and_inputs, diagnostic_code) = split_semantic_diagnostic(program_spec);
        let Some((program_id, input_plan)) =
            program_and_inputs.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR)
        else {
            return Err("semantic inputs operand plan is missing its input list".to_string());
        };
        if program_id.is_empty() {
            return Err("semantic inputs operand plan is missing a program id".to_string());
        }
        let Some(values) = semantic_plan_inputs(input_plan, input, expr_ctx).map_err(|err| {
            diagnostic_code.map_or(err.clone(), |code| {
                expr_ctx
                    .model
                    .diag_message(code, err.as_str(), &[("mnemonic", upper_mnemonic)])
            })
        })?
        else {
            return Ok(None);
        };
        let encoded = expr_ctx
            .model
            .execute_encoding_program(expr_ctx.resolved, program_id, values.as_slice())
            .map_err(|err| {
                diagnostic_code.map_or_else(
                    || err.to_string(),
                    |code| {
                        let value_text = values.first().copied().unwrap_or_default().to_string();
                        expr_ctx.model.diag_message(
                            code,
                            err.to_string().as_str(),
                            &[("value", value_text.as_str()), ("mnemonic", upper_mnemonic)],
                        )
                    },
                )
            })?;
        vec![encoded]
    } else if let Some(program_spec) = selector
        .operand_plan
        .strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX)
    {
        let (program_id, diagnostic_code) = split_semantic_diagnostic(program_spec);
        if program_id.is_empty() {
            return Err("semantic scalar operand plan is missing a program id".to_string());
        }
        let expr = input
            .expr0
            .ok_or_else(|| "semantic scalar operand plan requires one expression".to_string())?;
        let value = expr_ctx.eval_expr(expr)?;
        let encoded = expr_ctx
            .model
            .execute_encoding_program(expr_ctx.resolved, program_id, &[value])
            .map_err(|err| {
                diagnostic_code.map_or_else(
                    || err.to_string(),
                    |code| {
                        let value_text = value.to_string();
                        expr_ctx.model.diag_message(
                            code,
                            err.to_string().as_str(),
                            &[("value", value_text.as_str())],
                        )
                    },
                )
            })?;
        vec![encoded]
    } else {
        match selector.operand_plan.as_str() {
            "none" => Vec::new(),
            "u8" => {
                let Some(expr0) = input.expr0 else {
                    return Ok(None);
                };
                vec![encode_expr_u8(expr0, expr_ctx)?]
            }
            "u16" => {
                let Some(expr0) = input.expr0 else {
                    return Ok(None);
                };
                vec![encode_expr_u16(expr0, expr_ctx)?]
            }
            "u24" => {
                let Some(expr0) = input.expr0 else {
                    return Ok(None);
                };
                vec![encode_expr_u24(expr0, expr_ctx)?]
            }
            "rel8" => {
                let Some(expr0) = input.expr0 else {
                    return Ok(None);
                };
                vec![encode_expr_rel8(expr0, expr_ctx, 2)?]
            }
            "rel16" => {
                let Some(expr0) = input.expr0 else {
                    return Ok(None);
                };
                vec![encode_expr_rel16(expr0, expr_ctx, 3)?]
            }
            "pair_u8_rel8" => vec![
                encode_expr_u8(
                    input
                        .expr0
                        .ok_or_else(|| "missing first operand".to_string())?,
                    expr_ctx,
                )?,
                encode_expr_rel8(
                    input
                        .expr1
                        .ok_or_else(|| "missing second operand".to_string())?,
                    expr_ctx,
                    3,
                )?,
            ],
            "u8u8_packed" => vec![{
                let mut packed = encode_expr_u8(
                    input
                        .expr0
                        .ok_or_else(|| "missing first operand".to_string())?,
                    expr_ctx,
                )?;
                packed.extend(encode_expr_u8(
                    input
                        .expr1
                        .ok_or_else(|| "missing second operand".to_string())?,
                    expr_ctx,
                )?);
                packed
            }],
            _ => match encode_m65816_runtime_operand_plan(
                selector.operand_plan.as_str(),
                input.expr0,
                input.expr1,
                upper_mnemonic,
                expr_ctx.assembler_ctx,
                |expr| expr_ctx.eval_expr(expr),
                |expr| expr_ctx.has_unstable_symbols(expr),
            )? {
                Some(operand_bytes) => operand_bytes,
                None => return Ok(None),
            },
        }
    };

    if mode_operand_size == 0 && !operand_bytes.is_empty() {
        return Ok(None);
    }
    if relocation_free {
        operand_bytes.push(crate::runtime_model_core::RELOCATION_FREE_CANDIDATE_MARKER.to_vec());
    }
    for fixup in &output_fixups {
        operand_bytes.push(crate::runtime_model_core::output_fixup_candidate_marker(
            fixup,
        )?);
    }
    Ok(Some(VmEncodeCandidate {
        mode_key,
        operand_bytes,
    }))
}

fn split_semantic_diagnostic(spec: &str) -> (&str, Option<&str>) {
    spec.split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
        .map_or((spec, None), |(program, code)| {
            (program, (!code.is_empty()).then_some(code))
        })
}

fn semantic_register_position(
    expr: &Expr,
    mappings: &[(u16, u16)],
    expr_ctx: &SelectorExprContext<'_>,
) -> Option<(u16, u16)> {
    let register_id = match expr {
        Expr::Register(id, _) | Expr::Identifier(id, _) => id,
        _ => return None,
    };
    let register = expr_ctx
        .model
        .register_encoding_for_resolved(expr_ctx.resolved, register_id)?;
    let offset = mappings
        .iter()
        .find_map(|(class, offset)| (*class == register.class).then_some(*offset))?;
    let bit = offset.checked_add(register.index)?;
    (bit < 16).then_some((register.class, bit))
}

fn semantic_register_mask(
    expr: &Expr,
    mappings: &[(u16, u16)],
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Option<u16>, String> {
    match expr {
        Expr::List(items, _) | Expr::Tuple(items, _) => {
            let mut mask = 0_u16;
            for item in items {
                let Some(item_mask) = semantic_register_mask(item, mappings, expr_ctx)? else {
                    return Ok(None);
                };
                mask |= item_mask;
            }
            Ok(Some(mask))
        }
        Expr::Binary {
            op: BinaryOp::Divide | BinaryOp::Subtract,
            ..
        } => {
            let mut registers = Vec::new();
            let mut operators = Vec::new();
            if !flatten_semantic_register_list(
                expr,
                mappings,
                expr_ctx,
                &mut registers,
                &mut operators,
            ) {
                return Ok(None);
            }
            let Some((first_class, first_bit)) = registers.first().copied() else {
                return Ok(None);
            };
            if operators.len() + 1 != registers.len() {
                return Ok(None);
            }
            let mut mask = 1_u16 << first_bit;
            let mut previous = (first_class, first_bit);
            for (operator, current) in operators.iter().zip(registers.iter().copied().skip(1)) {
                match operator {
                    BinaryOp::Divide => mask |= 1_u16 << current.1,
                    BinaryOp::Subtract if previous.0 == current.0 && previous.1 <= current.1 => {
                        for bit in previous.1..=current.1 {
                            mask |= 1_u16 << bit;
                        }
                    }
                    _ => return Ok(None),
                }
                previous = current;
            }
            Ok(Some(mask))
        }
        Expr::Range {
            start, end, step, ..
        } => {
            if step.is_some() {
                return Ok(None);
            }
            let Some((start_class, start_bit)) =
                semantic_register_position(start, mappings, expr_ctx)
            else {
                return Ok(None);
            };
            let Some((end_class, end_bit)) = semantic_register_position(end, mappings, expr_ctx)
            else {
                return Ok(None);
            };
            if start_class != end_class {
                return Ok(None);
            }
            let (low, high) = if start_bit <= end_bit {
                (start_bit, end_bit)
            } else {
                (end_bit, start_bit)
            };
            let mut mask = 0_u16;
            for bit in low..=high {
                mask |= 1_u16 << bit;
            }
            Ok(Some(mask))
        }
        _ => Ok(semantic_register_position(expr, mappings, expr_ctx).map(|(_, bit)| 1_u16 << bit)),
    }
}

fn semantic_duplicate_register(expr: &Expr) -> Option<String> {
    fn visit(expr: &Expr, seen: &mut Vec<String>) -> Option<String> {
        match expr {
            Expr::List(items, _) | Expr::Tuple(items, _) | Expr::Call { args: items, .. } => {
                for item in items {
                    if let Some(duplicate) = visit(item, seen) {
                        return Some(duplicate);
                    }
                }
                None
            }
            Expr::Binary {
                op: BinaryOp::Divide,
                left,
                right,
                ..
            } => visit(left, seen).or_else(|| visit(right, seen)),
            Expr::Register(id, _) | Expr::Identifier(id, _) => {
                let canonical = id.to_ascii_uppercase();
                if seen.iter().any(|item| item == &canonical) {
                    Some(canonical)
                } else {
                    seen.push(canonical);
                    None
                }
            }
            _ => None,
        }
    }

    visit(expr, &mut Vec::new())
}

fn semantic_reject_plan_inputs(
    plan: &str,
    input: &SelectorInput<'_>,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Option<Vec<(&'static str, String)>>, String> {
    let exprs = (0..input.expr_count())
        .map(|index| input.expr(index))
        .collect::<Vec<_>>();
    let mut ordinary_sources = Vec::new();
    let mut captures = Vec::new();
    for source in plan.split(',') {
        if let Some(spec) = source.strip_prefix(MODE_SELECTOR_PLAN_OUT_OF_RANGE_PREFIX) {
            let Some((index, bounds)) = spec.split_once(".min") else {
                return Err(format!(
                    "semantic out-of-range source '{source}' requires a minimum"
                ));
            };
            let Some((min, max)) = bounds.split_once(".max") else {
                return Err(format!(
                    "semantic out-of-range source '{source}' requires a maximum"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic out-of-range source '{source}' has an invalid expression")
            })?;
            let min = min.parse::<i64>().map_err(|_| {
                format!("semantic out-of-range source '{source}' has an invalid minimum")
            })?;
            let max = max.parse::<i64>().map_err(|_| {
                format!("semantic out-of-range source '{source}' has an invalid maximum")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let value = expr_ctx.eval_expr(expr)?;
            if (min..=max).contains(&value) {
                return Ok(None);
            }
            captures.push(("value", value.to_string()));
            continue;
        }
        let Some(index) = source.strip_prefix(MODE_SELECTOR_PLAN_DUPLICATE_REGISTER_PREFIX) else {
            ordinary_sources.push(source);
            continue;
        };
        let index = index.parse::<usize>().map_err(|_| {
            format!("semantic duplicate-register source '{source}' has an invalid expression")
        })?;
        let Some(expr) = exprs.get(index).copied().flatten() else {
            return Ok(None);
        };
        let Some(duplicate) = semantic_duplicate_register(expr) else {
            return Ok(None);
        };
        captures.push(("register", duplicate));
    }
    if !ordinary_sources.is_empty()
        && semantic_plan_inputs(ordinary_sources.join(",").as_str(), input, expr_ctx)?.is_none()
    {
        return Ok(None);
    }
    Ok(Some(captures))
}

fn flatten_semantic_register_list(
    expr: &Expr,
    mappings: &[(u16, u16)],
    expr_ctx: &SelectorExprContext<'_>,
    registers: &mut Vec<(u16, u16)>,
    operators: &mut Vec<BinaryOp>,
) -> bool {
    match expr {
        Expr::Binary {
            op: op @ (BinaryOp::Divide | BinaryOp::Subtract),
            left,
            right,
            ..
        } => {
            if !flatten_semantic_register_list(left, mappings, expr_ctx, registers, operators) {
                return false;
            }
            operators.push(*op);
            flatten_semantic_register_list(right, mappings, expr_ctx, registers, operators)
        }
        _ => semantic_register_position(expr, mappings, expr_ctx)
            .map(|register| registers.push(register))
            .is_some(),
    }
}

fn project_expr_path(
    spec: &str,
    exprs: &[Option<&Expr>],
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Option<i64>, String> {
    let mut steps = spec.split('/');
    let operand = steps
        .next()
        .ok_or_else(|| "expression path is missing its operand index".to_string())?
        .parse::<usize>()
        .map_err(|_| format!("expression path '{spec}' has an invalid operand index"))?;
    let Some(mut current) = exprs.get(operand).copied().flatten() else {
        return Ok(None);
    };
    let steps = steps.collect::<Vec<_>>();
    let Some((terminal, containers)) = steps.split_last() else {
        return Err(format!("expression path '{spec}' is missing a terminal"));
    };
    for step in containers {
        current = match (*step, current) {
            ("i", Expr::Indirect(inner, _)) => inner,
            ("b", Expr::IndirectLong(inner, _)) => inner,
            ("l", Expr::Binary { left, .. }) => left,
            ("r", Expr::Binary { right, .. }) => right,
            (step, Expr::Tuple(items, _) | Expr::List(items, _)) if step.starts_with('t') => {
                let index = step[1..].parse::<usize>().map_err(|_| {
                    format!("expression path '{spec}' has an invalid tuple step '{step}'")
                })?;
                let Some(item) = items.get(index) else {
                    return Ok(None);
                };
                item
            }
            _ => return Ok(None),
        };
    }

    if let Some(class) = terminal.strip_prefix('r') {
        let class = class
            .parse::<u16>()
            .map_err(|_| format!("expression path '{spec}' has an invalid register class"))?;
        let id = match current {
            Expr::Register(id, _) | Expr::Identifier(id, _) => id.as_str(),
            _ => return Ok(None),
        };
        let Some(register) = expr_ctx
            .model
            .register_encoding_for_resolved(expr_ctx.resolved, id)
        else {
            return Ok(None);
        };
        return Ok((register.class == class).then_some(i64::from(register.index)));
    }
    if let Some(expected) = terminal.strip_prefix('n') {
        let id = match current {
            Expr::Register(id, _) | Expr::Identifier(id, _) => id.as_str(),
            _ => return Ok(None),
        };
        if !id.eq_ignore_ascii_case(expected) {
            return Ok(None);
        }
        let Some(register) = expr_ctx
            .model
            .register_encoding_for_resolved(expr_ctx.resolved, id)
        else {
            return Ok(None);
        };
        return Ok(Some(i64::from(register.index)));
    }
    if let Some(qualified) = terminal.strip_prefix('q') {
        let Some((qualifier, class)) = qualified.split_once(".c") else {
            return Err(format!(
                "expression path '{spec}' qualified register is missing its class"
            ));
        };
        let class = class
            .parse::<u16>()
            .map_err(|_| format!("expression path '{spec}' has an invalid register class"))?;
        let register_expr = match current {
            Expr::Binary {
                op: BinaryOp::Multiply,
                left,
                right,
                ..
            } if expr_ctx.eval_expr(right).is_ok() => left.as_ref(),
            Expr::Binary {
                op: BinaryOp::Multiply,
                left,
                right,
                ..
            } if expr_ctx.eval_expr(left).is_ok() => right.as_ref(),
            expr => expr,
        };
        let id = match register_expr {
            Expr::Member { base, field, .. } if field.eq_ignore_ascii_case(qualifier) => {
                match base.as_ref() {
                    Expr::Register(id, _) | Expr::Identifier(id, _) => id.as_str(),
                    _ => return Ok(None),
                }
            }
            Expr::Register(id, _) | Expr::Identifier(id, _) => {
                let Some((id, actual_qualifier)) = id.rsplit_once('.') else {
                    return Ok(None);
                };
                if !actual_qualifier.eq_ignore_ascii_case(qualifier) {
                    return Ok(None);
                }
                id
            }
            _ => return Ok(None),
        };
        let Some(register) = expr_ctx
            .model
            .register_encoding_for_resolved(expr_ctx.resolved, id)
        else {
            return Ok(None);
        };
        return Ok((register.class == class).then_some(i64::from(register.index)));
    }
    if *terminal == "s" {
        let Expr::Binary {
            op: BinaryOp::Multiply,
            left,
            right,
            ..
        } = current
        else {
            return Ok(None);
        };
        let scale = expr_ctx
            .eval_expr(right)
            .ok()
            .or_else(|| expr_ctx.eval_expr(left).ok());
        let Some(scale @ (1 | 2 | 4 | 8)) = scale else {
            return Ok(None);
        };
        return Ok(Some(i64::from(scale.trailing_zeros())));
    }
    if let Some(field) = terminal.strip_prefix('m') {
        let Expr::Member {
            base,
            field: actual,
            ..
        } = current
        else {
            return Ok(None);
        };
        if !actual.eq_ignore_ascii_case(field) {
            return Ok(None);
        }
        return expr_ctx.eval_expr(base).map(Some);
    }
    if let Some(expected) = terminal.strip_prefix('a') {
        let expected = expected
            .parse::<usize>()
            .map_err(|_| format!("expression path '{spec}' has an invalid arity"))?;
        let actual = match current {
            Expr::Tuple(items, _) | Expr::List(items, _) => items.len(),
            _ => return Ok(None),
        };
        return Ok((actual == expected).then_some(actual as i64));
    }
    Err(format!(
        "expression path '{spec}' has unsupported terminal '{terminal}'"
    ))
}

fn parse_named_register_range<'a>(
    pattern: &'a str,
    source: &str,
) -> Result<(&'a str, u32, u32), String> {
    let Some((prefix, bounds)) = pattern.split_once(".min") else {
        return Err(format!(
            "semantic named-register-range source '{source}' requires a minimum"
        ));
    };
    let Some((minimum, maximum)) = bounds.split_once(".max") else {
        return Err(format!(
            "semantic named-register-range source '{source}' requires a maximum"
        ));
    };
    let minimum = minimum.parse::<u32>().map_err(|_| {
        format!("semantic named-register-range source '{source}' has an invalid minimum")
    })?;
    let maximum = maximum.parse::<u32>().map_err(|_| {
        format!("semantic named-register-range source '{source}' has an invalid maximum")
    })?;
    if prefix.is_empty() || minimum > maximum {
        return Err(format!(
            "semantic named-register-range source '{source}' has an invalid range"
        ));
    }
    Ok((prefix, minimum, maximum))
}

fn named_register_range_value(
    expr: &Expr,
    prefix: &str,
    minimum: u32,
    maximum: u32,
) -> Option<u32> {
    let actual = match expr {
        Expr::Register(actual, _) | Expr::Identifier(actual, _) => actual,
        _ => return None,
    };
    let actual_prefix = actual.get(..prefix.len())?;
    let suffix = actual.get(prefix.len()..)?;
    if !actual_prefix.eq_ignore_ascii_case(prefix)
        || suffix.is_empty()
        || !suffix.bytes().all(|byte| byte.is_ascii_digit())
    {
        return None;
    }
    let value = suffix.parse::<u32>().ok()?;
    (minimum..=maximum).contains(&value).then_some(value)
}

fn semantic_plan_inputs(
    plan: &str,
    input: &SelectorInput<'_>,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Option<Vec<i64>>, String> {
    if plan.is_empty() {
        return Err("semantic inputs operand plan has no inputs".to_string());
    }
    let exprs = (0..input.expr_count())
        .map(|index| input.expr(index))
        .collect::<Vec<_>>();
    let mut values = Vec::new();
    for source in plan.split(',') {
        if let Some(spec) = source.strip_prefix("target:call_arg_value") {
            let Some((index, arg)) = spec.split_once(".arg") else {
                return Err(format!(
                    "semantic target call-value source '{source}' requires an argument index"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic target source '{source}' has an invalid expression")
            })?;
            let arg = arg.parse::<usize>().map_err(|_| {
                format!("semantic target source '{source}' has an invalid argument")
            })?;
            let Some(Expr::Call { args, .. }) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Some(arg_expr) = args.get(arg) else {
                return Ok(None);
            };
            if !expression_can_be_relocation_target(arg_expr) {
                return Ok(None);
            }
            values.push(0);
            continue;
        }
        if let Some(spec) = source.strip_prefix("target:call_arg_member") {
            let Some((index, arg_and_field)) = spec.split_once(".arg") else {
                return Err(format!(
                    "semantic target call-member source '{source}' requires an argument index"
                ));
            };
            let Some((arg, expected_field)) = arg_and_field.split_once(".field") else {
                return Err(format!(
                    "semantic target call-member source '{source}' requires an expected field"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic target source '{source}' has an invalid expression")
            })?;
            let arg = arg.parse::<usize>().map_err(|_| {
                format!("semantic target source '{source}' has an invalid argument")
            })?;
            let Some(Expr::Call { args, .. }) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Some(Expr::Member { base, field, .. }) = args.get(arg) else {
                return Ok(None);
            };
            if !field.eq_ignore_ascii_case(expected_field)
                || !expression_can_be_relocation_target(base)
            {
                return Ok(None);
            }
            values.push(0);
            continue;
        }
        if let Some(spec) = source.strip_prefix(MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX) {
            let Some(value) = project_expr_path(spec, exprs.as_slice(), expr_ctx)? else {
                return Ok(None);
            };
            values.push(value);
            continue;
        }
        if let Some(index) = source.strip_prefix("target_atom:expr") {
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic atomic target source '{source}' has an invalid expression")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            if !expression_is_atomic_relocation_target(expr) {
                return Ok(None);
            }
            values.push(0);
            continue;
        }
        if let Some(index) = source.strip_prefix("target:expr") {
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic target source '{source}' has an invalid expression")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            if !expression_can_be_relocation_target(expr) {
                return Ok(None);
            }
            values.push(0);
            continue;
        }
        if let Some(index) = source.strip_prefix(MODE_SELECTOR_PLAN_DISTINCT_REGISTER_PREFIX) {
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic distinct-register source '{source}' has an invalid expression")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            if semantic_duplicate_register(expr).is_some() {
                return Ok(None);
            }
            values.push(0);
            continue;
        }
        if let Some(spec) = source.strip_prefix(MODE_SELECTOR_PLAN_REGISTER_SEQUENCE_PREFIX) {
            let Some((index, rest)) = spec.split_once(".class") else {
                return Err(format!(
                    "semantic register-sequence source '{source}' requires a register class"
                ));
            };
            let Some((class_spec, rest)) = rest.split_once(".count") else {
                return Err(format!(
                    "semantic register-sequence source '{source}' requires a count"
                ));
            };
            let Some((count, alignment)) = rest.split_once(".align") else {
                return Err(format!(
                    "semantic register-sequence source '{source}' requires an alignment"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic register-sequence source '{source}' has an invalid expression")
            })?;
            let (class, projection) = parse_register_index_projection(class_spec, source)?;
            let count = count.parse::<u16>().map_err(|_| {
                format!("semantic register-sequence source '{source}' has an invalid count")
            })?;
            let (alignment, expected_violation) = alignment
                .split_once(".violation-")
                .map_or((alignment, None), |(alignment, violation)| {
                    (alignment, Some(violation))
                });
            let alignment = alignment.parse::<u16>().map_err(|_| {
                format!("semantic register-sequence source '{source}' has an invalid alignment")
            })?;
            if count == 0 || alignment == 0 {
                return Err(format!(
                    "semantic register-sequence source '{source}' requires nonzero count and alignment"
                ));
            }
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let endpoints = match expr {
                Expr::Binary {
                    op: BinaryOp::Subtract,
                    left,
                    right,
                    ..
                } => Some((left.as_ref(), right.as_ref())),
                Expr::Range {
                    start,
                    end,
                    step: None,
                    ..
                } => Some((start.as_ref(), end.as_ref())),
                _ => None,
            };
            let Some((start_expr, end_expr)) = endpoints else {
                return Ok(None);
            };
            let resolve_register = |expr: &Expr| {
                let register_id = match expr {
                    Expr::Register(id, _) | Expr::Identifier(id, _) => id,
                    _ => return None,
                };
                expr_ctx
                    .model
                    .register_encoding_for_resolved(expr_ctx.resolved, register_id)
            };
            let Some(start) = resolve_register(start_expr) else {
                return Ok(None);
            };
            let Some(end) = resolve_register(end_expr) else {
                return Ok(None);
            };
            if start.class != class || end.class != class {
                return Ok(None);
            }
            let aligned = start.index % alignment == 0;
            let consecutive = end.index == start.index.saturating_add(count - 1);
            let matches_relation = match expected_violation {
                None => aligned && consecutive,
                Some("alignment") => !aligned && consecutive,
                Some("sequence") => aligned && !consecutive,
                Some(violation) => {
                    return Err(format!(
                        "semantic register-sequence source '{source}' has unsupported violation '{violation}'"
                    ));
                }
            };
            if !matches_relation {
                return Ok(None);
            }
            values.push(i64::from(project_register_index(start.index, projection)));
            continue;
        }
        if let Some(spec) = source.strip_prefix(MODE_SELECTOR_PLAN_BOUNDED_REGISTER_PREFIX) {
            let Some((index, rest)) = spec.split_once(".class") else {
                return Err(format!(
                    "semantic bounded-register source '{source}' requires a register class"
                ));
            };
            let Some((class, rest)) = rest.split_once(".min") else {
                return Err(format!(
                    "semantic bounded-register source '{source}' requires a minimum index"
                ));
            };
            let Some((min, max)) = rest.split_once(".max") else {
                return Err(format!(
                    "semantic bounded-register source '{source}' requires a maximum index"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic bounded-register source '{source}' has an invalid expression")
            })?;
            let class = class.parse::<u16>().map_err(|_| {
                format!("semantic bounded-register source '{source}' has an invalid class")
            })?;
            let min = min.parse::<u16>().map_err(|_| {
                format!("semantic bounded-register source '{source}' has an invalid minimum")
            })?;
            let (max, outside) = max
                .strip_suffix(".outside")
                .map_or((max, false), |max| (max, true));
            let max = max.parse::<u16>().map_err(|_| {
                format!("semantic bounded-register source '{source}' has an invalid maximum")
            })?;
            if min > max {
                return Err(format!(
                    "semantic bounded-register source '{source}' has an inverted range"
                ));
            }
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let register_id = match expr {
                Expr::Register(id, _) | Expr::Identifier(id, _) => id,
                _ => return Ok(None),
            };
            let Some(register) = expr_ctx
                .model
                .register_encoding_for_resolved(expr_ctx.resolved, register_id)
            else {
                return Ok(None);
            };
            let in_range = (min..=max).contains(&register.index);
            if register.class != class || in_range == outside {
                return Ok(None);
            }
            values.push(i64::from(register.index));
            continue;
        }
        if let Some(spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX)
        {
            let Some((index, rest)) = spec.split_once(".arg") else {
                return Err(format!(
                    "semantic call-argument register-sequence source '{source}' requires a first argument"
                ));
            };
            let Some((first_arg, rest)) = rest.split_once(".arg") else {
                return Err(format!(
                    "semantic call-argument register-sequence source '{source}' requires a second argument"
                ));
            };
            let Some((second_arg, rest)) = rest.split_once(".class") else {
                return Err(format!(
                    "semantic call-argument register-sequence source '{source}' requires a register class"
                ));
            };
            let Some((class, alignment)) = rest.split_once(".align") else {
                return Err(format!(
                    "semantic call-argument register-sequence source '{source}' requires an alignment"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!(
                    "semantic call-argument register-sequence source '{source}' has an invalid expression"
                )
            })?;
            let first_arg = first_arg.parse::<usize>().map_err(|_| {
                format!(
                    "semantic call-argument register-sequence source '{source}' has an invalid first argument"
                )
            })?;
            let second_arg = second_arg.parse::<usize>().map_err(|_| {
                format!(
                    "semantic call-argument register-sequence source '{source}' has an invalid second argument"
                )
            })?;
            let (class, projection) = parse_register_index_projection(class, source)?;
            let (alignment, expected_violation) = alignment
                .split_once(".violation-")
                .map_or((alignment, None), |(alignment, violation)| {
                    (alignment, Some(violation))
                });
            let alignment = alignment.parse::<u16>().map_err(|_| {
                format!(
                    "semantic call-argument register-sequence source '{source}' has an invalid alignment"
                )
            })?;
            if alignment == 0 {
                return Err(format!(
                    "semantic call-argument register-sequence source '{source}' has zero alignment"
                ));
            }
            let Some(Expr::Call { args, .. }) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let resolve_arg = |arg: usize| {
                let arg_expr = args.get(arg)?;
                let register_id = match arg_expr {
                    Expr::Register(id, _) | Expr::Identifier(id, _) => id,
                    _ => return None,
                };
                expr_ctx
                    .model
                    .register_encoding_for_resolved(expr_ctx.resolved, register_id)
            };
            let Some(first) = resolve_arg(first_arg) else {
                return Ok(None);
            };
            let Some(second) = resolve_arg(second_arg) else {
                return Ok(None);
            };
            if first.class != class || second.class != class {
                return Ok(None);
            }
            let aligned = first.index % alignment == 0;
            let consecutive = second.index == first.index.saturating_add(1);
            let matches_relation = match expected_violation {
                None => aligned && consecutive,
                Some("alignment") => !aligned && consecutive,
                Some("sequence") => aligned && !consecutive,
                Some(violation) => {
                    return Err(format!(
                        "semantic call-argument register-sequence source '{source}' has unsupported violation '{violation}'"
                    ));
                }
            };
            if !matches_relation {
                return Ok(None);
            }
            values.push(i64::from(project_register_index(first.index, projection)));
            continue;
        }
        let call_arg_register = source
            .strip_prefix(MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX)
            .map(|spec| (spec, true))
            .or_else(|| {
                source
                    .strip_prefix(MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX)
                    .map(|spec| (spec, false))
            });
        if let Some((spec, indirect)) = call_arg_register {
            let Some((index, arg_and_class)) = spec.split_once(".arg") else {
                return Err(format!(
                    "semantic call-argument source '{source}' requires an argument index"
                ));
            };
            let Some((arg, class)) = arg_and_class.split_once(".class") else {
                return Err(format!(
                    "semantic call-argument source '{source}' requires a register class"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic call-argument source '{source}' has an invalid expression")
            })?;
            let arg = arg.parse::<usize>().map_err(|_| {
                format!("semantic call-argument source '{source}' has an invalid argument")
            })?;
            let (class, projection) = parse_register_index_projection(class, source)?;
            let Some(Expr::Call { args, .. }) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Some(arg_expr) = args.get(arg) else {
                return Ok(None);
            };
            let register_expr = if indirect {
                let Expr::Indirect(inner, _) = arg_expr else {
                    return Ok(None);
                };
                inner.as_ref()
            } else {
                arg_expr
            };
            let register_id = match register_expr {
                Expr::Register(id, _) | Expr::Identifier(id, _) => id,
                _ => return Ok(None),
            };
            let Some(register) = expr_ctx
                .model
                .register_encoding_for_resolved(expr_ctx.resolved, register_id)
            else {
                return Ok(None);
            };
            if register.class != class {
                return Ok(None);
            }
            values.push(i64::from(project_register_index(
                register.index,
                projection,
            )));
            continue;
        }
        if let Some(spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_TUPLE_REGISTER_PREFIX)
        {
            let Some((index, arg_item_class)) = spec.split_once(".arg") else {
                return Err(format!(
                    "semantic call-tuple source '{source}' requires an argument index"
                ));
            };
            let Some((arg, item_class)) = arg_item_class.split_once(".item") else {
                return Err(format!(
                    "semantic call-tuple source '{source}' requires a tuple item"
                ));
            };
            let Some((item, class)) = item_class.split_once(".class") else {
                return Err(format!(
                    "semantic call-tuple source '{source}' requires a register class"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic call-tuple source '{source}' has an invalid expression")
            })?;
            let arg = arg.parse::<usize>().map_err(|_| {
                format!("semantic call-tuple source '{source}' has an invalid argument")
            })?;
            let item = item.parse::<usize>().map_err(|_| {
                format!("semantic call-tuple source '{source}' has an invalid tuple item")
            })?;
            let class = class.parse::<u16>().map_err(|_| {
                format!("semantic call-tuple source '{source}' has an invalid class")
            })?;
            let Some(Expr::Call { args, .. }) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Some(Expr::Indirect(inner, _)) = args.get(arg) else {
                return Ok(None);
            };
            let Expr::Tuple(items, _) = inner.as_ref() else {
                return Ok(None);
            };
            let Some(register_expr) = items.get(item) else {
                return Ok(None);
            };
            let register_id = match register_expr {
                Expr::Register(id, _) | Expr::Identifier(id, _) => id,
                _ => return Ok(None),
            };
            let Some(register) = expr_ctx
                .model
                .register_encoding_for_resolved(expr_ctx.resolved, register_id)
            else {
                return Ok(None);
            };
            if register.class != class {
                return Ok(None);
            }
            values.push(i64::from(register.index));
            continue;
        }
        if let Some(spec) = source.strip_prefix(MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX) {
            let Some((index, arg)) = spec.split_once(".arg") else {
                return Err(format!(
                    "semantic call-value source '{source}' requires an argument index"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic call-value source '{source}' has an invalid expression")
            })?;
            let arg = arg.parse::<usize>().map_err(|_| {
                format!("semantic call-value source '{source}' has an invalid argument")
            })?;
            let Some(Expr::Call { args, .. }) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Some(arg_expr) = args.get(arg) else {
                return Ok(None);
            };
            values.push(expr_ctx.eval_expr(arg_expr)?);
            continue;
        }
        if let Some(spec) = source.strip_prefix(MODE_SELECTOR_PLAN_CALL_ARG_MEMBER_PREFIX) {
            let Some((index, arg_and_field)) = spec.split_once(".arg") else {
                return Err(format!(
                    "semantic call-member source '{source}' requires an argument index"
                ));
            };
            let Some((arg, expected_field)) = arg_and_field.split_once(".field") else {
                return Err(format!(
                    "semantic call-member source '{source}' requires an expected field"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic call-member source '{source}' has an invalid expression")
            })?;
            let arg = arg.parse::<usize>().map_err(|_| {
                format!("semantic call-member source '{source}' has an invalid argument")
            })?;
            let Some(Expr::Call { args, .. }) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Some(Expr::Member { base, field, .. }) = args.get(arg) else {
                return Ok(None);
            };
            if !field.eq_ignore_ascii_case(expected_field) {
                return Ok(None);
            }
            values.push(expr_ctx.eval_expr(base)?);
            continue;
        }
        if let Some(count_spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_NAMED_REGISTER_RANGE_COUNT_PREFIX)
        {
            let Some((positions, pattern_and_count)) = count_spec.split_once(".prefix") else {
                return Err(format!(
                    "semantic named-register-range-count source '{source}' requires a prefix"
                ));
            };
            let Some((pattern, required)) = pattern_and_count.rsplit_once(".atleast") else {
                return Err(format!(
                    "semantic named-register-range-count source '{source}' requires a count"
                ));
            };
            let required = required.parse::<usize>().map_err(|_| {
                format!(
                    "semantic named-register-range-count source '{source}' has an invalid count"
                )
            })?;
            let (prefix, minimum, maximum) = parse_named_register_range(pattern, source)?;
            let mut count = 0_usize;
            for position in positions.split('+') {
                let index = position.parse::<usize>().map_err(|_| {
                    format!("semantic named-register-range-count source '{source}' has an invalid expression")
                })?;
                if exprs
                    .get(index)
                    .copied()
                    .flatten()
                    .and_then(|expr| named_register_range_value(expr, prefix, minimum, maximum))
                    .is_some()
                {
                    count += 1;
                }
            }
            if count < required {
                return Ok(None);
            }
            values.push(count as i64);
            continue;
        }
        if let Some(spec) = source.strip_prefix(MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX) {
            let Some((index_and_classes, pattern)) = spec.split_once(".prefix") else {
                return Err(format!(
                    "semantic register-or-named-range source '{source}' requires a prefix"
                ));
            };
            let Some((index, classes)) = index_and_classes.split_once(".classes") else {
                return Err(format!(
                    "semantic register-or-named-range source '{source}' requires classes"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!(
                    "semantic register-or-named-range source '{source}' has an invalid expression"
                )
            })?;
            let classes = classes
                .split('+')
                .map(|class| class.parse::<u16>())
                .collect::<Result<Vec<_>, _>>()
                .map_err(|_| {
                    format!(
                        "semantic register-or-named-range source '{source}' has an invalid class"
                    )
                })?;
            let (prefix, minimum, maximum) = parse_named_register_range(pattern, source)?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            if let Some(register) = semantic_resolved_register(expr, expr_ctx) {
                if classes.contains(&register.class) {
                    values.push(i64::from(register.index));
                    continue;
                }
            }
            let Some(value) = named_register_range_value(expr, prefix, minimum, maximum) else {
                return Ok(None);
            };
            values.push(i64::from(value));
            continue;
        }
        if let Some(named_spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_NAMED_REGISTER_RANGE_PREFIX)
        {
            let Some((index, pattern)) = named_spec.split_once(".prefix") else {
                return Err(format!(
                    "semantic named-register-range source '{source}' requires a prefix"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic named-register-range source '{source}' has an invalid expression")
            })?;
            let (prefix, minimum, maximum) = parse_named_register_range(pattern, source)?;
            let Some(value) = exprs
                .get(index)
                .copied()
                .flatten()
                .and_then(|expr| named_register_range_value(expr, prefix, minimum, maximum))
            else {
                return Ok(None);
            };
            values.push(i64::from(value));
            continue;
        }
        if let Some(named_spec) = source.strip_prefix(MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX) {
            let Some((index, expected)) = named_spec.split_once('=') else {
                return Err(format!(
                    "semantic named-register source '{source}' requires an expected name"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic named-register source '{source}' has an invalid expression")
            })?;
            let Some(Expr::Register(actual, _) | Expr::Identifier(actual, _)) =
                exprs.get(index).copied().flatten()
            else {
                return Ok(None);
            };
            if !actual.eq_ignore_ascii_case(expected) {
                return Ok(None);
            }
            values.push(0);
            continue;
        }
        if let Some(index) = source.strip_prefix(MODE_SELECTOR_PLAN_IMMEDIATE_PREFIX) {
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic immediate source '{source}' has an invalid expression")
            })?;
            let Some(Expr::Immediate(inner, _)) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            values.push(expr_ctx.eval_expr(inner)?);
            continue;
        }
        if let Some(mask_spec) = source.strip_prefix(MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX) {
            let Some((index, mapping_spec)) = mask_spec.split_once(".map") else {
                return Err(format!(
                    "semantic register-mask source '{source}' requires a class mapping"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic register-mask source '{source}' has an invalid expression")
            })?;
            let (mapping_spec, reverse_bits) = mapping_spec
                .strip_suffix(".reverse16")
                .map_or((mapping_spec, false), |mapping| (mapping, true));
            let mappings = mapping_spec
                .split('+')
                .map(|mapping| {
                    let Some((class, offset)) = mapping.split_once('=') else {
                        return Err(format!(
                            "semantic register-mask source '{source}' has an invalid class mapping"
                        ));
                    };
                    Ok((
                        class.parse::<u16>().map_err(|_| {
                            format!("semantic register-mask source '{source}' has an invalid class")
                        })?,
                        offset.parse::<u16>().map_err(|_| {
                            format!(
                                "semantic register-mask source '{source}' has an invalid offset"
                            )
                        })?,
                    ))
                })
                .collect::<Result<Vec<_>, String>>()?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Some(mut mask) = semantic_register_mask(expr, mappings.as_slice(), expr_ctx)?
            else {
                return Ok(None);
            };
            if reverse_bits {
                mask = mask.reverse_bits();
            }
            values.push(i64::from(mask));
            continue;
        }
        let value_program = source
            .strip_prefix(MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX)
            .map(|spec| (spec, true))
            .or_else(|| {
                source
                    .strip_prefix(MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX)
                    .map(|spec| (spec, false))
            });
        if let Some((value_spec, required)) = value_program {
            let Some((program_id, projected_source)) =
                value_spec.split_once(MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR)
            else {
                return Err(format!(
                    "semantic value-program source '{source}' requires a projected source"
                ));
            };
            if program_id.is_empty() {
                return Err(format!(
                    "semantic value-program source '{source}' has an empty program id"
                ));
            }
            let projected = if let Some(index) = projected_source.strip_prefix("expr") {
                let index = index.parse::<usize>().map_err(|_| {
                    format!("semantic value-program source '{source}' has an invalid expression")
                })?;
                let Some(expr) = exprs.get(index).copied().flatten() else {
                    return Ok(None);
                };
                expr_ctx.eval_expr(expr)?
            } else if let Some(member_spec) =
                projected_source.strip_prefix(MODE_SELECTOR_PLAN_MEMBER_PREFIX)
            {
                let Some((index, expected_field)) =
                    member_spec.split_once(MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR)
                else {
                    return Err(format!(
                        "semantic value-program member source '{source}' requires a field"
                    ));
                };
                let index = index.parse::<usize>().map_err(|_| {
                    format!("semantic value-program source '{source}' has an invalid member")
                })?;
                let Some(expr) = exprs.get(index).copied().flatten() else {
                    return Ok(None);
                };
                match expr {
                    Expr::Member { base, field, .. } => {
                        if !field.eq_ignore_ascii_case(expected_field) {
                            return Ok(None);
                        }
                        expr_ctx.eval_expr(base)?
                    }
                    Expr::Identifier(qualified, span) => {
                        let Some((base, field)) = qualified.rsplit_once('.') else {
                            return Ok(None);
                        };
                        if !field.eq_ignore_ascii_case(expected_field) {
                            return Ok(None);
                        }
                        expr_ctx.eval_expr(&Expr::Identifier(base.to_string(), *span))?
                    }
                    _ => return Ok(None),
                }
            } else if let Some(tuple_spec) =
                projected_source.strip_prefix(MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX)
            {
                let Some((index, item)) =
                    tuple_spec.split_once(MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR)
                else {
                    return Err(format!(
                        "semantic value-program tuple source '{source}' requires an item index"
                    ));
                };
                let index = index.parse::<usize>().map_err(|_| {
                    format!("semantic value-program source '{source}' has an invalid tuple")
                })?;
                let item = item.parse::<usize>().map_err(|_| {
                    format!("semantic value-program source '{source}' has an invalid tuple item")
                })?;
                let Some(Expr::Indirect(inner, _)) = exprs.get(index).copied().flatten() else {
                    return Ok(None);
                };
                let Expr::Tuple(items, _) = inner.as_ref() else {
                    return Ok(None);
                };
                let Some(item) = items.get(item) else {
                    return Ok(None);
                };
                if matches!(item, Expr::Placeholder(_) | Expr::Member { .. }) {
                    return Ok(None);
                }
                expr_ctx.eval_expr(item)?
            } else {
                return Err(format!(
                    "semantic value-program source '{source}' uses an unsupported v1 projection"
                ));
            };
            match expr_ctx
                .model
                .execute_value_program(expr_ctx.resolved, program_id, &[projected])
            {
                Ok(value) => values.push(value),
                Err(crate::runtime_error::RuntimeBridgeError::ValueVm(
                    crate::value_vm::ValueVmError::ConstraintViolation { .. },
                )) if !required => return Ok(None),
                Err(err) => return Err(err.to_string()),
            }
            continue;
        }
        if let Some(value) = source.strip_prefix(MODE_SELECTOR_PLAN_LITERAL_PREFIX) {
            let value = value.parse::<i64>().map_err(|_| {
                format!("semantic inputs operand plan has invalid literal source '{source}'")
            })?;
            values.push(value);
            continue;
        }
        if let Some(tuple_spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX)
        {
            let Some((index, expected)) = tuple_spec.split_once(".value") else {
                return Err(format!(
                    "semantic indirect tuple arity source '{source}' requires an expected value"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple source '{source}'")
            })?;
            let expected = expected.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple arity '{source}'")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Expr::Indirect(inner, _) = expr else {
                return Ok(None);
            };
            let Expr::Tuple(items, _) = inner.as_ref() else {
                return Ok(None);
            };
            if items.len() != expected {
                return Ok(None);
            }
            values.push(items.len() as i64);
            continue;
        }
        if let Some(tuple_spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_INDIRECT_TUPLE_IDENTITY_SCALE_PREFIX)
        {
            let Some((index, item)) =
                tuple_spec.split_once(MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR)
            else {
                return Err(format!(
                    "semantic indirect tuple identity-scale source '{source}' requires an item index"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple source '{source}'")
            })?;
            let item = item.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple item '{source}'")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Expr::Indirect(inner, _) = expr else {
                return Ok(None);
            };
            let Expr::Tuple(items, _) = inner.as_ref() else {
                return Ok(None);
            };
            let Some(Expr::Binary {
                op: BinaryOp::Multiply,
                left,
                right,
                ..
            }) = items.get(item)
            else {
                return Ok(None);
            };
            let identity = expr_ctx
                .eval_expr(right)
                .ok()
                .or_else(|| expr_ctx.eval_expr(left).ok());
            if identity != Some(1) {
                return Ok(None);
            }
            values.push(1);
            continue;
        }
        if let Some(tuple_spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_INDIRECT_TUPLE_NONIDENTITY_SCALE_PREFIX)
        {
            let Some((index, item)) =
                tuple_spec.split_once(MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR)
            else {
                return Err(format!(
                    "semantic indirect tuple scale source '{source}' requires an item index"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple source '{source}'")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            fn find_scale(expr: &Expr, expr_ctx: &SelectorExprContext<'_>) -> Option<i64> {
                match expr {
                    Expr::Binary {
                        op: BinaryOp::Multiply,
                        left,
                        right,
                        ..
                    } => expr_ctx
                        .eval_expr(right)
                        .ok()
                        .or_else(|| expr_ctx.eval_expr(left).ok())
                        .filter(|scale| *scale != 1)
                        .or_else(|| find_scale(left, expr_ctx))
                        .or_else(|| find_scale(right, expr_ctx)),
                    Expr::Tuple(items, _) | Expr::List(items, _) => items
                        .iter()
                        .find_map(|candidate| find_scale(candidate, expr_ctx)),
                    Expr::Member { base, .. }
                    | Expr::Indirect(base, _)
                    | Expr::Immediate(base, _)
                    | Expr::Unary { expr: base, .. } => find_scale(base, expr_ctx),
                    _ => None,
                }
            }
            let scale = if item == "any" {
                find_scale(expr, expr_ctx)
            } else {
                let Expr::Indirect(inner, _) = expr else {
                    return Ok(None);
                };
                let Expr::Tuple(items, _) = inner.as_ref() else {
                    return Ok(None);
                };
                let item = item.parse::<usize>().map_err(|_| {
                    format!("semantic inputs operand plan has invalid tuple item '{source}'")
                })?;
                let Some(candidate) = items.get(item) else {
                    return Ok(None);
                };
                find_scale(candidate, expr_ctx)
            };
            let Some(scale) = scale.filter(|scale| *scale != 1) else {
                return Ok(None);
            };
            values.push(scale);
            continue;
        }
        if let Some(tuple_spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX)
        {
            let Some((index, item)) =
                tuple_spec.split_once(MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR)
            else {
                return Err(format!(
                    "semantic indirect tuple value source '{source}' requires an item index"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple source '{source}'")
            })?;
            let item = item.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple item '{source}'")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Expr::Indirect(inner, _) = expr else {
                return Ok(None);
            };
            let Expr::Tuple(items, _) = inner.as_ref() else {
                return Ok(None);
            };
            let Some(item) = items.get(item) else {
                return Ok(None);
            };
            if matches!(item, Expr::Placeholder(_) | Expr::Member { .. }) {
                return Ok(None);
            }
            values.push(expr_ctx.eval_expr(item)?);
            continue;
        }
        if let Some(tuple_spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX)
        {
            let Some((index, item_qualifier_class)) =
                tuple_spec.split_once(MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR)
            else {
                return Err(format!(
                    "semantic indirect tuple qualified-register source '{source}' requires an item index"
                ));
            };
            let Some((item, qualifier_class)) =
                item_qualifier_class.split_once(MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR)
            else {
                return Err(format!(
                    "semantic indirect tuple qualified-register source '{source}' requires a qualifier"
                ));
            };
            let Some((expected_qualifier, class)) = qualifier_class.split_once(".class") else {
                return Err(format!(
                    "semantic indirect tuple qualified-register source '{source}' requires an expected class"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple source '{source}'")
            })?;
            let item = item.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple item '{source}'")
            })?;
            let class = class.parse::<u16>().map_err(|_| {
                format!("semantic inputs operand plan has invalid register class '{source}'")
            })?;
            if expected_qualifier.is_empty() {
                return Err(format!(
                    "semantic indirect tuple qualified-register source '{source}' has an empty qualifier"
                ));
            }
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Expr::Indirect(inner, _) = expr else {
                return Ok(None);
            };
            let Expr::Tuple(items, _) = inner.as_ref() else {
                return Ok(None);
            };
            let Some(item) = items.get(item) else {
                return Ok(None);
            };
            let item = match item {
                Expr::Binary {
                    op: BinaryOp::Multiply,
                    left,
                    right,
                    ..
                } if expr_ctx.eval_expr(right).ok() == Some(1) => left.as_ref(),
                Expr::Binary {
                    op: BinaryOp::Multiply,
                    left,
                    right,
                    ..
                } if expr_ctx.eval_expr(left).ok() == Some(1) => right.as_ref(),
                item => item,
            };
            let register_id = match item {
                Expr::Member { base, field, .. } => {
                    if !field.eq_ignore_ascii_case(expected_qualifier) {
                        return Ok(None);
                    }
                    match base.as_ref() {
                        Expr::Register(id, _) | Expr::Identifier(id, _) => id.as_str(),
                        _ => return Ok(None),
                    }
                }
                Expr::Register(id, _) | Expr::Identifier(id, _) => {
                    let Some((base, qualifier)) = id.rsplit_once('.') else {
                        return Ok(None);
                    };
                    if !qualifier.eq_ignore_ascii_case(expected_qualifier) {
                        return Ok(None);
                    }
                    base
                }
                _ => return Ok(None),
            };
            let Some(register) = expr_ctx
                .model
                .register_encoding_for_resolved(expr_ctx.resolved, register_id)
            else {
                return Ok(None);
            };
            if register.class != class {
                return Ok(None);
            }
            values.push(i64::from(register.index));
            continue;
        }
        if let Some(tuple_spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX)
        {
            let Some((index, item_and_class)) =
                tuple_spec.split_once(MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR)
            else {
                return Err(format!(
                    "semantic indirect tuple register source '{source}' requires an item index"
                ));
            };
            let Some((item, class)) = item_and_class.split_once(".class") else {
                return Err(format!(
                    "semantic indirect tuple register source '{source}' requires an expected class"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple source '{source}'")
            })?;
            let item = item.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid tuple item '{source}'")
            })?;
            let class = class.parse::<u16>().map_err(|_| {
                format!("semantic inputs operand plan has invalid register class '{source}'")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Expr::Indirect(inner, _) = expr else {
                return Ok(None);
            };
            let Expr::Tuple(items, _) = inner.as_ref() else {
                return Ok(None);
            };
            let Some(item) = items.get(item) else {
                return Ok(None);
            };
            let item = match item {
                Expr::Binary {
                    op: BinaryOp::Multiply,
                    left,
                    right,
                    ..
                } if expr_ctx.eval_expr(right).ok() == Some(1) => left.as_ref(),
                Expr::Binary {
                    op: BinaryOp::Multiply,
                    left,
                    right,
                    ..
                } if expr_ctx.eval_expr(left).ok() == Some(1) => right.as_ref(),
                item => item,
            };
            let register_id = match item {
                Expr::Register(id, _) | Expr::Identifier(id, _) => id,
                _ => return Ok(None),
            };
            let Some(register) = expr_ctx
                .model
                .register_encoding_for_resolved(expr_ctx.resolved, register_id)
            else {
                return Ok(None);
            };
            if register.class != class {
                return Ok(None);
            }
            values.push(i64::from(register.index));
            continue;
        }
        if let Some(register_spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX)
        {
            let Some((index, class)) = register_spec.split_once(".class") else {
                return Err(format!(
                    "semantic indirect register source '{source}' requires an expected class"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid indirect source '{source}'")
            })?;
            let class = class.parse::<u16>().map_err(|_| {
                format!("semantic inputs operand plan has invalid register class '{source}'")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Expr::Indirect(inner, _) = expr else {
                return Ok(None);
            };
            let register_id = match inner.as_ref() {
                Expr::Register(id, _) | Expr::Identifier(id, _) => id,
                _ => return Ok(None),
            };
            let Some(register) = expr_ctx
                .model
                .register_encoding_for_resolved(expr_ctx.resolved, register_id)
            else {
                return Ok(None);
            };
            if register.class != class {
                return Ok(None);
            }
            values.push(i64::from(register.index));
            continue;
        }
        let unary_indirect = [
            (
                MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
                UnaryOp::Plus,
            ),
            (
                MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
                UnaryOp::Minus,
            ),
        ]
        .into_iter()
        .find_map(|(prefix, expected_op)| {
            source
                .strip_prefix(prefix)
                .map(|register_spec| (register_spec, expected_op))
        });
        if let Some((register_spec, expected_op)) = unary_indirect {
            let Some((index, class)) = register_spec.split_once(".class") else {
                return Err(format!(
                    "semantic unary indirect register source '{source}' requires an expected class"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid unary indirect source '{source}'")
            })?;
            let class = class.parse::<u16>().map_err(|_| {
                format!("semantic inputs operand plan has invalid register class '{source}'")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Expr::Unary {
                op,
                expr: unary_inner,
                ..
            } = expr
            else {
                return Ok(None);
            };
            if *op != expected_op {
                return Ok(None);
            }
            let Expr::Indirect(indirect_inner, _) = unary_inner.as_ref() else {
                return Ok(None);
            };
            let register_id = match indirect_inner.as_ref() {
                Expr::Register(id, _) | Expr::Identifier(id, _) => id,
                _ => return Ok(None),
            };
            let Some(register) = expr_ctx
                .model
                .register_encoding_for_resolved(expr_ctx.resolved, register_id)
            else {
                return Ok(None);
            };
            if register.class != class {
                return Ok(None);
            }
            values.push(i64::from(register.index));
            continue;
        }
        if let Some(member_spec) = source.strip_prefix(MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX) {
            let Some((index, expected_field)) =
                member_spec.split_once(MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR)
            else {
                return Err(format!(
                    "semantic member-shape source '{source}' requires an expected field"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic member-shape source '{source}' has an invalid expression")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let matches = match expr {
                Expr::Member { field, .. } => field.eq_ignore_ascii_case(expected_field),
                Expr::Identifier(qualified, _) => qualified
                    .rsplit_once('.')
                    .is_some_and(|(_, field)| field.eq_ignore_ascii_case(expected_field)),
                _ => false,
            };
            if !matches {
                return Ok(None);
            }
            values.push(0);
            continue;
        }
        if let Some(member_spec) = source.strip_prefix(MODE_SELECTOR_PLAN_MEMBER_INDIRECT_PREFIX) {
            let Some((index, expected_field)) =
                member_spec.split_once(MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR)
            else {
                return Err(format!(
                    "semantic member-indirect source '{source}' requires an expected field"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid member source '{source}'")
            })?;
            if expected_field.is_empty() {
                return Err(format!(
                    "semantic member-indirect source '{source}' has an empty field"
                ));
            }
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Expr::Member { base, field, .. } = expr else {
                return Ok(None);
            };
            if !field.eq_ignore_ascii_case(expected_field) {
                return Ok(None);
            }
            let Expr::Indirect(inner, _) = base.as_ref() else {
                return Ok(None);
            };
            values.push(expr_ctx.eval_expr(inner)?);
            continue;
        }
        if let Some(member_spec) = source.strip_prefix(MODE_SELECTOR_PLAN_MEMBER_PREFIX) {
            let Some((index, expected_field)) =
                member_spec.split_once(MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR)
            else {
                return Err(format!(
                    "semantic member source '{source}' requires an expected field"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid member source '{source}'")
            })?;
            if expected_field.is_empty() {
                return Err(format!(
                    "semantic member source '{source}' has an empty field"
                ));
            }
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            match expr {
                Expr::Member { base, field, .. } => {
                    if !field.eq_ignore_ascii_case(expected_field) {
                        return Ok(None);
                    }
                    values.push(expr_ctx.eval_expr(base)?);
                }
                Expr::Identifier(qualified, span) => {
                    let Some((base, field)) = qualified.rsplit_once('.') else {
                        return Ok(None);
                    };
                    if !field.eq_ignore_ascii_case(expected_field) {
                        return Ok(None);
                    }
                    values.push(expr_ctx.eval_expr(&Expr::Identifier(base.to_string(), *span))?);
                }
                _ => return Ok(None),
            }
            continue;
        }
        if let Some(index) = source.strip_prefix("expr") {
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic inputs operand plan has invalid expression source '{source}'")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            if matches!(expr, Expr::Member { .. } | Expr::Placeholder(_)) {
                return Ok(None);
            }
            if expression_is_target_reference(expr, expr_ctx.assembler_ctx) {
                return Ok(None);
            }
            values.push(expr_ctx.eval_expr(expr)?);
            continue;
        }

        if let Some(spec) = source.strip_prefix(MODE_SELECTOR_PLAN_REGISTER_INDEX_XOR_PREFIX) {
            let Some((left_spec, right_spec)) = spec.split_once(".with") else {
                return Err(format!(
                    "semantic register-index XOR source '{source}' requires two registers"
                ));
            };
            let Some((left_index, left_class)) = left_spec.split_once(".class") else {
                return Err(format!(
                    "semantic register-index XOR source '{source}' requires a left class"
                ));
            };
            let Some((right_index, right_class_spec)) = right_spec.split_once(".class") else {
                return Err(format!(
                    "semantic register-index XOR source '{source}' requires a right class"
                ));
            };
            let left_index = left_index.parse::<usize>().map_err(|_| {
                format!("semantic register-index XOR source '{source}' has an invalid left operand")
            })?;
            let right_index = right_index.parse::<usize>().map_err(|_| {
                format!(
                    "semantic register-index XOR source '{source}' has an invalid right operand"
                )
            })?;
            let left_class = left_class.parse::<u16>().map_err(|_| {
                format!("semantic register-index XOR source '{source}' has an invalid left class")
            })?;
            let (right_class, projection) =
                parse_register_index_projection(right_class_spec, source)?;
            let Some(left_expr) = exprs.get(left_index).copied().flatten() else {
                return Ok(None);
            };
            let Some(right_expr) = exprs.get(right_index).copied().flatten() else {
                return Ok(None);
            };
            let Some(left_register) = semantic_resolved_register(left_expr, expr_ctx) else {
                return Ok(None);
            };
            let Some(right_register) = semantic_resolved_register(right_expr, expr_ctx) else {
                return Ok(None);
            };
            if left_register.class != left_class || right_register.class != right_class {
                return Ok(None);
            }
            values.push(i64::from(project_register_index(
                left_register.index ^ right_register.index,
                projection,
            )));
            continue;
        }

        let Some(register_spec) = source.strip_prefix("reg") else {
            return Err(format!(
                "semantic inputs operand plan has unknown source '{source}'"
            ));
        };
        let Some((index, class_spec)) = register_spec.split_once(".class") else {
            return Err(format!(
                "semantic register source '{source}' requires an expected class"
            ));
        };
        let index = index.parse::<usize>().map_err(|_| {
            format!("semantic inputs operand plan has invalid register source '{source}'")
        })?;
        let (class, projection) = parse_register_index_projection(class_spec, source)?;
        let Some(expr) = exprs.get(index).copied().flatten() else {
            return Ok(None);
        };
        let register_id = match expr {
            Expr::Register(id, _) | Expr::Identifier(id, _) => id,
            _ => return Ok(None),
        };
        let Some(register) = expr_ctx
            .model
            .register_encoding_for_resolved(expr_ctx.resolved, register_id)
        else {
            return Ok(None);
        };
        if register.class != class {
            return Ok(None);
        }
        values.push(i64::from(project_register_index(
            register.index,
            projection,
        )));
    }
    Ok(Some(values))
}

fn semantic_resolved_register(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
) -> Option<crate::operand_record_vm::PortableRegisterRef> {
    let register_id = match expr {
        Expr::Register(id, _) | Expr::Identifier(id, _) => id,
        _ => return None,
    };
    expr_ctx
        .model
        .register_encoding_for_resolved(expr_ctx.resolved, register_id)
}

#[derive(Clone, Copy)]
enum RegisterIndexProjection {
    Identity,
    ShiftRight(u8),
    Mask(u16),
    ShiftRightAndMask(u8, u16),
}

fn parse_register_index_projection(
    class_spec: &str,
    source: &str,
) -> Result<(u16, RegisterIndexProjection), String> {
    let (class, projection) = if let Some((class, shift_spec)) =
        class_spec.split_once(MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX)
    {
        let (shift, mask) = shift_spec
            .split_once(MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX)
            .map_or((shift_spec, None), |(shift, mask)| (shift, Some(mask)));
        let shift = shift.parse::<u8>().map_err(|_| {
            format!("semantic register source '{source}' has an invalid right shift")
        })?;
        if shift >= 16 {
            return Err(format!(
                "semantic register source '{source}' right shift exceeds register index width"
            ));
        }
        let projection = if let Some(mask) = mask {
            let mask = mask.parse::<u16>().map_err(|_| {
                format!("semantic register source '{source}' has an invalid index mask")
            })?;
            RegisterIndexProjection::ShiftRightAndMask(shift, mask)
        } else {
            RegisterIndexProjection::ShiftRight(shift)
        };
        (class, projection)
    } else if let Some((class, mask)) =
        class_spec.split_once(MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX)
    {
        let mask = mask.parse::<u16>().map_err(|_| {
            format!("semantic register source '{source}' has an invalid index mask")
        })?;
        (class, RegisterIndexProjection::Mask(mask))
    } else {
        (class_spec, RegisterIndexProjection::Identity)
    };
    let class = class.parse::<u16>().map_err(|_| {
        format!("semantic inputs operand plan has invalid register class '{source}'")
    })?;
    Ok((class, projection))
}

fn project_register_index(index: u16, projection: RegisterIndexProjection) -> u16 {
    match projection {
        RegisterIndexProjection::Identity => index,
        RegisterIndexProjection::ShiftRight(shift) => index >> shift,
        RegisterIndexProjection::Mask(mask) => index & mask,
        RegisterIndexProjection::ShiftRightAndMask(shift, mask) => (index >> shift) & mask,
    }
}

fn expression_can_be_relocation_target(expr: &Expr) -> bool {
    match expr {
        Expr::Identifier(_, _) => true,
        Expr::Unary { expr, .. }
        | Expr::Immediate(expr, _)
        | Expr::Indirect(expr, _)
        | Expr::IndirectLong(expr, _)
        | Expr::Member { base: expr, .. } => expression_can_be_relocation_target(expr),
        Expr::Binary { left, right, .. } => {
            expression_can_be_relocation_target(left) || expression_can_be_relocation_target(right)
        }
        _ => false,
    }
}

fn expression_is_atomic_relocation_target(expr: &Expr) -> bool {
    match expr {
        Expr::Identifier(_, _) => true,
        Expr::Immediate(expr, _)
        | Expr::Indirect(expr, _)
        | Expr::IndirectLong(expr, _)
        | Expr::Member { base: expr, .. } => expression_is_atomic_relocation_target(expr),
        _ => false,
    }
}

fn semantic_fixup_plan_inputs(
    plan: &str,
    input: &SelectorInput<'_>,
    expr_ctx: &SelectorExprContext<'_>,
) -> Result<Option<Vec<crate::fixup_vm::PortableFixupInput>>, String> {
    let exprs = (0..input.expr_count())
        .map(|index| input.expr(index))
        .collect::<Vec<_>>();
    let mut values = Vec::new();
    for source in plan.split(',') {
        let (source, forced_target) = source
            .strip_prefix("target:")
            .map_or((source, false), |source| (source, true));
        let source_expr = if let Some(index) = source.strip_prefix("expr") {
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic fixup plan has invalid expression source '{source}'")
            })?;
            exprs.get(index).copied().flatten().cloned()
        } else if let Some(member_spec) = source.strip_prefix(MODE_SELECTOR_PLAN_MEMBER_PREFIX) {
            let Some((index, expected_field)) =
                member_spec.split_once(MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR)
            else {
                return Err(format!(
                    "semantic fixup member source '{source}' requires an expected field"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic fixup member source '{source}' has an invalid expression")
            })?;
            let Some(expr) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            match expr {
                Expr::Member { base, field, .. } => {
                    if !field.eq_ignore_ascii_case(expected_field) {
                        return Ok(None);
                    }
                    Some(base.as_ref().clone())
                }
                Expr::Identifier(qualified, span) => {
                    let Some((base, field)) = qualified.rsplit_once('.') else {
                        return Ok(None);
                    };
                    if !field.eq_ignore_ascii_case(expected_field) {
                        return Ok(None);
                    }
                    Some(Expr::Identifier(base.to_string(), *span))
                }
                _ => return Ok(None),
            }
        } else if let Some(spec) = source.strip_prefix(MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX) {
            let Some((index, arg)) = spec.split_once(".arg") else {
                return Err(format!(
                    "semantic fixup call-value source '{source}' requires an argument index"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic fixup source '{source}' has an invalid expression")
            })?;
            let arg = arg
                .parse::<usize>()
                .map_err(|_| format!("semantic fixup source '{source}' has an invalid argument"))?;
            let Some(Expr::Call { args, .. }) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            args.get(arg).cloned()
        } else if let Some(spec) = source.strip_prefix(MODE_SELECTOR_PLAN_CALL_ARG_MEMBER_PREFIX) {
            let Some((index, arg_and_field)) = spec.split_once(".arg") else {
                return Err(format!(
                    "semantic fixup call-member source '{source}' requires an argument index"
                ));
            };
            let Some((arg, expected_field)) = arg_and_field.split_once(".field") else {
                return Err(format!(
                    "semantic fixup call-member source '{source}' requires an expected field"
                ));
            };
            let index = index.parse::<usize>().map_err(|_| {
                format!("semantic fixup source '{source}' has an invalid expression")
            })?;
            let arg = arg
                .parse::<usize>()
                .map_err(|_| format!("semantic fixup source '{source}' has an invalid argument"))?;
            let Some(Expr::Call { args, .. }) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Some(Expr::Member { base, field, .. }) = args.get(arg) else {
                return Ok(None);
            };
            if !field.eq_ignore_ascii_case(expected_field) {
                return Ok(None);
            }
            Some(base.as_ref().clone())
        } else if let Some(tuple_spec) =
            source.strip_prefix(MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX)
        {
            let Some((index, item)) =
                tuple_spec.split_once(MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR)
            else {
                return Err(format!(
                    "semantic fixup tuple source '{source}' requires an item index"
                ));
            };
            let index = index
                .parse::<usize>()
                .map_err(|_| format!("semantic fixup plan has invalid tuple source '{source}'"))?;
            let item = item
                .parse::<usize>()
                .map_err(|_| format!("semantic fixup plan has invalid tuple item '{source}'"))?;
            let Some(Expr::Indirect(inner, _)) = exprs.get(index).copied().flatten() else {
                return Ok(None);
            };
            let Expr::Tuple(items, _) = inner.as_ref() else {
                return Ok(None);
            };
            items.get(item).cloned()
        } else {
            None
        };
        if let Some(expr) = source_expr {
            let relocation =
                if forced_target || expression_is_target_reference(&expr, expr_ctx.assembler_ctx) {
                    expr_ctx.assembler_ctx.absolute_relocation(&expr)?
                } else {
                    None
                };
            let value = if let Some((addend, _)) = relocation.as_ref() {
                crate::fixup_vm::PortableDeferredValue::Resolved(*addend)
            } else if expr_ctx.assembler_ctx.should_defer_unstable_symbols()
                && expr_ctx.has_unstable_symbols(&expr)?
            {
                crate::fixup_vm::PortableDeferredValue::Unresolved
            } else {
                crate::fixup_vm::PortableDeferredValue::Resolved(expr_ctx.eval_expr(&expr)?)
            };
            values.push(crate::fixup_vm::PortableFixupInput {
                value,
                target_reference: forced_target
                    || expression_is_target_reference(&expr, expr_ctx.assembler_ctx),
                relocation_target: relocation.map(|(_, target)| target),
            });
            continue;
        }
        let Some(resolved) = semantic_plan_inputs(source, input, expr_ctx)? else {
            return Ok(None);
        };
        if resolved.len() != 1 {
            return Err(format!(
                "semantic fixup source '{source}' did not resolve to one value"
            ));
        }
        values.push(crate::fixup_vm::PortableFixupInput {
            value: crate::fixup_vm::PortableDeferredValue::Resolved(resolved[0]),
            target_reference: false,
            relocation_target: None,
        });
    }
    Ok(Some(values))
}

fn expression_is_target_reference(
    expr: &Expr,
    ctx: &dyn registry::family::AssemblerContext,
) -> bool {
    match expr {
        Expr::Identifier(name, _) | Expr::Register(name, _) => ctx.symbol_is_target_reference(name),
        Expr::Unary { expr, .. } => expression_is_target_reference(expr, ctx),
        Expr::Binary { left, right, .. } => {
            expression_is_target_reference(left, ctx) || expression_is_target_reference(right, ctx)
        }
        Expr::Member { base, .. } | Expr::Indirect(base, _) => {
            expression_is_target_reference(base, ctx)
        }
        Expr::Tuple(items, _) => items
            .iter()
            .any(|item| expression_is_target_reference(item, ctx)),
        _ => false,
    }
}

fn encode_expr_u8(expr: &Expr, expr_ctx: &SelectorExprContext<'_>) -> Result<Vec<u8>, String> {
    encode_expr_fixed_width(expr, expr_ctx, 1, 0xFF, "invalid u8 operand")
}

fn encode_expr_u16(expr: &Expr, expr_ctx: &SelectorExprContext<'_>) -> Result<Vec<u8>, String> {
    encode_expr_fixed_width(expr, expr_ctx, 2, 0xFFFF, "invalid u16 operand")
}

fn encode_expr_u24(expr: &Expr, expr_ctx: &SelectorExprContext<'_>) -> Result<Vec<u8>, String> {
    encode_expr_fixed_width(expr, expr_ctx, 3, 0xFF_FFFF, "invalid u24 operand")
}

fn encode_expr_fixed_width(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
    byte_count: usize,
    max_value: i64,
    error_message: &str,
) -> Result<Vec<u8>, String> {
    let value = expr_ctx.eval_expr(expr)?;
    encode_fixed_width_value(value, byte_count, max_value, error_message)
}

fn encode_expr_rel8(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
    instr_len: i64,
) -> Result<Vec<u8>, String> {
    encode_expr_relative(
        expr,
        expr_ctx,
        instr_len,
        -128,
        127,
        1,
        "Branch target out of range",
    )
}

fn encode_expr_rel16(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
    instr_len: i64,
) -> Result<Vec<u8>, String> {
    encode_expr_relative(
        expr,
        expr_ctx,
        instr_len,
        -32768,
        32767,
        2,
        "Long branch target out of range",
    )
}

fn encode_expr_relative(
    expr: &Expr,
    expr_ctx: &SelectorExprContext<'_>,
    instr_len: i64,
    min_offset: i64,
    max_offset: i64,
    byte_count: usize,
    error_label: &str,
) -> Result<Vec<u8>, String> {
    let value = expr_ctx.eval_expr(expr)?;
    let current = expr_ctx.assembler_ctx.current_address() as i64 + instr_len;
    let offset = value - current;
    encode_relative_offset(
        offset,
        min_offset,
        max_offset,
        byte_count,
        error_label,
        expr_ctx.assembler_ctx.pass(),
    )
}
