// SPDX-License-Identifier: GPL-3.0-or-later

//! `.opcore` VM surface.
//!
//! This groups VM-side functionality that primarily supports the language/core
//! processor domain rather than assembler instruction selection or encoding.

use opcore::expr_vm::compile_core_expr_to_portable_program;
use opcore::expr_vm::{
    eval_portable_expr_program, expr_program_has_unstable_symbols, PortableExprBudgets,
    PortableExprEvalContext, PortableExprEvaluation, PortableExprProgram, PortableExprRef,
};
use opcore::parser::{Expr, ParseError, Parser};
use opcore::tokenizer::{Span, Token};
use registry::family::AssemblerContext;
use registry::syntax::RegisterChecker;
use types::processing::{
    OpcoreRequestKind, ProcessingOutcome, ProcessingRequestKind, ProcessingReturn,
};

#[cfg(test)]
use crate::execution_model::CORE_EXPR_PARSER_FAILPOINT;
pub use crate::expr_vm_compat;
use crate::rollout::portable_expr_parser_runtime_enabled_for_family;
use crate::runtime_diagnostics::RuntimeBridgeDiagnostic;
use crate::runtime_error::RuntimeBridgeError;
use crate::runtime_parse_utils::runtime_bridge_error_to_parse_error;
pub use crate::vm_core::HierarchyExecutionModel;
use crate::vm_opasm_parse::VmExprParseContext;
use std::collections::HashMap;
use std::sync::LazyLock;

const EXVM_DEFAULT_PROGRAM_V1: &[u8] = &[
    package::ExvmOpcode::ParseExpression as u8,
    package::ExvmOpcode::End as u8,
];

static EXVM_DEFAULT_PROGRAM_V2: LazyLock<Vec<u8>> = LazyLock::new(build_default_exvm_program_v2);

struct ExvmV2DefaultProgramBuilder {
    bytes: Vec<u8>,
    labels: HashMap<&'static str, usize>,
    patches: Vec<(&'static str, usize)>,
}

impl ExvmV2DefaultProgramBuilder {
    fn new() -> Self {
        Self {
            bytes: Vec::new(),
            labels: HashMap::new(),
            patches: Vec::new(),
        }
    }

    fn mark(&mut self, label: &'static str) {
        let prev = self.labels.insert(label, self.bytes.len());
        assert!(prev.is_none(), "duplicate EXVM v2 default label: {label}");
    }

    fn opcode(&mut self, opcode: package::ExvmOpcodeV2) {
        self.bytes.push(opcode as u8);
    }

    fn operator(&mut self, operator: package::ExvmOperatorKindV2) {
        self.bytes.push(operator as u8);
    }

    fn token_kind(&mut self, kind: package::ExvmTokenKindV2) {
        self.bytes.push(kind as u8);
    }

    fn push_label_target(&mut self, label: &'static str) {
        let offset = self.bytes.len();
        self.bytes.extend_from_slice(&0u16.to_le_bytes());
        self.patches.push((label, offset));
    }

    fn call(&mut self, label: &'static str) {
        self.opcode(package::ExvmOpcodeV2::Call);
        self.push_label_target(label);
    }

    fn jump(&mut self, label: &'static str) {
        self.opcode(package::ExvmOpcodeV2::Jump);
        self.push_label_target(label);
    }

    fn jump_if_true(&mut self, label: &'static str) {
        self.opcode(package::ExvmOpcodeV2::JumpIfTrue);
        self.push_label_target(label);
    }

    fn ret(&mut self) {
        self.opcode(package::ExvmOpcodeV2::Return);
    }

    fn peek_kind_jump_if_true(&mut self, kind: package::ExvmTokenKindV2, label: &'static str) {
        self.opcode(package::ExvmOpcodeV2::PeekKind);
        self.token_kind(kind);
        self.jump_if_true(label);
    }

    fn peek_operator_jump_if_true(
        &mut self,
        operator: package::ExvmOperatorKindV2,
        label: &'static str,
    ) {
        self.opcode(package::ExvmOpcodeV2::PeekOperator);
        self.operator(operator);
        self.jump_if_true(label);
    }

    fn consume_operator(&mut self, operator: package::ExvmOperatorKindV2) {
        self.opcode(package::ExvmOpcodeV2::ConsumeOperator);
        self.operator(operator);
    }

    fn consume_kind(&mut self, kind: package::ExvmTokenKindV2) {
        self.opcode(package::ExvmOpcodeV2::ConsumeKind);
        self.token_kind(kind);
    }

    fn build_unary(&mut self, operator: package::ExvmOperatorKindV2) {
        self.opcode(package::ExvmOpcodeV2::BuildUnary);
        self.operator(operator);
    }

    fn build_binary(&mut self, operator: package::ExvmOperatorKindV2) {
        self.opcode(package::ExvmOpcodeV2::BuildBinary);
        self.operator(operator);
    }

    fn build_ternary(&mut self) {
        self.opcode(package::ExvmOpcodeV2::BuildTernary);
    }

    fn finish(mut self) -> Vec<u8> {
        for (label, offset) in self.patches {
            let target = *self
                .labels
                .get(label)
                .unwrap_or_else(|| panic!("missing EXVM v2 default label: {label}"));
            let target = u16::try_from(target).expect("EXVM v2 default program exceeds u16");
            self.bytes[offset..offset + 2].copy_from_slice(&target.to_le_bytes());
        }
        self.bytes
    }
}

fn build_default_exvm_program_v2() -> Vec<u8> {
    let mut builder = ExvmV2DefaultProgramBuilder::new();

    builder.call("ternary");
    builder.opcode(package::ExvmOpcodeV2::End);

    builder.mark("ternary");
    builder.call("logical_or");
    builder.peek_kind_jump_if_true(package::ExvmTokenKindV2::Question, "ternary_build");
    builder.ret();
    builder.mark("ternary_build");
    builder.consume_kind(package::ExvmTokenKindV2::Question);
    builder.call("ternary");
    builder.consume_kind(package::ExvmTokenKindV2::Colon);
    builder.call("ternary");
    builder.build_ternary();
    builder.ret();

    builder.mark("logical_or");
    builder.call("logical_and");
    builder.mark("logical_or_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::LogicOr, "logical_or_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::LogicXor, "logical_xor_build");
    builder.ret();
    builder.mark("logical_or_build");
    builder.consume_operator(package::ExvmOperatorKindV2::LogicOr);
    builder.call("logical_and");
    builder.build_binary(package::ExvmOperatorKindV2::LogicOr);
    builder.jump("logical_or_loop");
    builder.mark("logical_xor_build");
    builder.consume_operator(package::ExvmOperatorKindV2::LogicXor);
    builder.call("logical_and");
    builder.build_binary(package::ExvmOperatorKindV2::LogicXor);
    builder.jump("logical_or_loop");

    builder.mark("logical_and");
    builder.call("bit_or");
    builder.mark("logical_and_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::LogicAnd, "logical_and_build");
    builder.ret();
    builder.mark("logical_and_build");
    builder.consume_operator(package::ExvmOperatorKindV2::LogicAnd);
    builder.call("bit_or");
    builder.build_binary(package::ExvmOperatorKindV2::LogicAnd);
    builder.jump("logical_and_loop");

    builder.mark("bit_or");
    builder.call("bit_xor");
    builder.mark("bit_or_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::BitOr, "bit_or_build");
    builder.ret();
    builder.mark("bit_or_build");
    builder.consume_operator(package::ExvmOperatorKindV2::BitOr);
    builder.call("bit_xor");
    builder.build_binary(package::ExvmOperatorKindV2::BitOr);
    builder.jump("bit_or_loop");

    builder.mark("bit_xor");
    builder.call("bit_and");
    builder.mark("bit_xor_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::BitXor, "bit_xor_build");
    builder.ret();
    builder.mark("bit_xor_build");
    builder.consume_operator(package::ExvmOperatorKindV2::BitXor);
    builder.call("bit_and");
    builder.build_binary(package::ExvmOperatorKindV2::BitXor);
    builder.jump("bit_xor_loop");

    builder.mark("bit_and");
    builder.call("compare");
    builder.mark("bit_and_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::BitAnd, "bit_and_build");
    builder.ret();
    builder.mark("bit_and_build");
    builder.consume_operator(package::ExvmOperatorKindV2::BitAnd);
    builder.call("compare");
    builder.build_binary(package::ExvmOperatorKindV2::BitAnd);
    builder.jump("bit_and_loop");

    builder.mark("compare");
    builder.call("shift");
    builder.mark("compare_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Eq, "compare_eq_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Ne, "compare_ne_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Ge, "compare_ge_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Gt, "compare_gt_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Le, "compare_le_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Lt, "compare_lt_build");
    builder.ret();
    builder.mark("compare_eq_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Eq);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKindV2::Eq);
    builder.jump("compare_loop");
    builder.mark("compare_ne_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Ne);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKindV2::Ne);
    builder.jump("compare_loop");
    builder.mark("compare_ge_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Ge);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKindV2::Ge);
    builder.jump("compare_loop");
    builder.mark("compare_gt_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Gt);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKindV2::Gt);
    builder.jump("compare_loop");
    builder.mark("compare_le_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Le);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKindV2::Le);
    builder.jump("compare_loop");
    builder.mark("compare_lt_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Lt);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKindV2::Lt);
    builder.jump("compare_loop");

    builder.mark("shift");
    builder.call("sum");
    builder.mark("shift_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Shl, "shift_shl_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Shr, "shift_shr_build");
    builder.ret();
    builder.mark("shift_shl_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Shl);
    builder.call("sum");
    builder.build_binary(package::ExvmOperatorKindV2::Shl);
    builder.jump("shift_loop");
    builder.mark("shift_shr_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Shr);
    builder.call("sum");
    builder.build_binary(package::ExvmOperatorKindV2::Shr);
    builder.jump("shift_loop");

    builder.mark("sum");
    builder.call("term");
    builder.mark("sum_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Plus, "sum_plus_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Minus, "sum_minus_build");
    builder.ret();
    builder.mark("sum_plus_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Plus);
    builder.call("term");
    builder.build_binary(package::ExvmOperatorKindV2::Plus);
    builder.jump("sum_loop");
    builder.mark("sum_minus_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Minus);
    builder.call("term");
    builder.build_binary(package::ExvmOperatorKindV2::Minus);
    builder.jump("sum_loop");

    builder.mark("term");
    builder.call("power");
    builder.mark("term_loop");
    builder
        .peek_operator_jump_if_true(package::ExvmOperatorKindV2::Multiply, "term_multiply_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Divide, "term_divide_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Mod, "term_mod_build");
    builder.ret();
    builder.mark("term_multiply_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Multiply);
    builder.call("power");
    builder.build_binary(package::ExvmOperatorKindV2::Multiply);
    builder.jump("term_loop");
    builder.mark("term_divide_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Divide);
    builder.call("power");
    builder.build_binary(package::ExvmOperatorKindV2::Divide);
    builder.jump("term_loop");
    builder.mark("term_mod_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Mod);
    builder.call("power");
    builder.build_binary(package::ExvmOperatorKindV2::Mod);
    builder.jump("term_loop");

    builder.mark("power");
    builder.call("unary");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Power, "power_build");
    builder.ret();
    builder.mark("power_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Power);
    builder.call("power");
    builder.build_binary(package::ExvmOperatorKindV2::Power);
    builder.ret();

    builder.mark("unary");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Plus, "unary_plus_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Minus, "unary_minus_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::BitNot, "unary_bit_not_build");
    builder.peek_operator_jump_if_true(
        package::ExvmOperatorKindV2::LogicNot,
        "unary_logic_not_build",
    );
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Lt, "unary_low_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKindV2::Gt, "unary_high_build");
    builder.call("primary");
    builder.ret();
    builder.mark("unary_plus_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Plus);
    builder.call("unary");
    builder.build_unary(package::ExvmOperatorKindV2::Plus);
    builder.ret();
    builder.mark("unary_minus_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Minus);
    builder.call("unary");
    builder.build_unary(package::ExvmOperatorKindV2::Minus);
    builder.ret();
    builder.mark("unary_bit_not_build");
    builder.consume_operator(package::ExvmOperatorKindV2::BitNot);
    builder.call("unary");
    builder.build_unary(package::ExvmOperatorKindV2::BitNot);
    builder.ret();
    builder.mark("unary_logic_not_build");
    builder.consume_operator(package::ExvmOperatorKindV2::LogicNot);
    builder.call("unary");
    builder.build_unary(package::ExvmOperatorKindV2::LogicNot);
    builder.ret();
    builder.mark("unary_low_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Lt);
    builder.call("unary");
    builder.build_unary(package::ExvmOperatorKindV2::Lt);
    builder.ret();
    builder.mark("unary_high_build");
    builder.consume_operator(package::ExvmOperatorKindV2::Gt);
    builder.call("unary");
    builder.build_unary(package::ExvmOperatorKindV2::Gt);
    builder.ret();

    builder.mark("primary");
    builder.peek_kind_jump_if_true(package::ExvmTokenKindV2::Number, "primary_number");
    builder.peek_kind_jump_if_true(package::ExvmTokenKindV2::Identifier, "primary_identifier");
    builder.peek_kind_jump_if_true(package::ExvmTokenKindV2::Dollar, "primary_dollar");
    builder.peek_kind_jump_if_true(package::ExvmTokenKindV2::OpenParen, "primary_grouping");
    builder.opcode(package::ExvmOpcodeV2::EmitDiag);
    builder.mark("primary_number");
    builder.opcode(package::ExvmOpcodeV2::LoadTokenText);
    builder.opcode(package::ExvmOpcodeV2::BuildNumber);
    builder.opcode(package::ExvmOpcodeV2::Advance);
    builder.ret();
    builder.mark("primary_identifier");
    builder.opcode(package::ExvmOpcodeV2::LoadTokenText);
    builder.opcode(package::ExvmOpcodeV2::BuildIdentifier);
    builder.opcode(package::ExvmOpcodeV2::Advance);
    builder.ret();
    builder.mark("primary_dollar");
    builder.opcode(package::ExvmOpcodeV2::BuildCurrentAddress);
    builder.opcode(package::ExvmOpcodeV2::Advance);
    builder.ret();
    builder.mark("primary_grouping");
    builder.opcode(package::ExvmOpcodeV2::ParseGrouping);
    builder.ret();

    builder.finish()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct ExvmExecutionBudgets {
    pub max_steps: usize,
    pub max_token_count: usize,
    pub max_stack_depth: usize,
    pub allow_out_of_scope_compatibility: bool,
}

impl ExvmExecutionBudgets {
    pub(crate) fn for_tokens(token_count: usize) -> Self {
        Self {
            max_steps: token_count.saturating_mul(128).max(128),
            max_token_count: token_count,
            max_stack_depth: token_count.max(1),
            allow_out_of_scope_compatibility: true,
        }
    }
}

struct RuntimePortableExprEvalContext<'a> {
    assembler_ctx: &'a dyn AssemblerContext,
}

impl PortableExprEvalContext for RuntimePortableExprEvalContext<'_> {
    fn lookup_symbol(&self, name: &str) -> Option<i64> {
        if !self.assembler_ctx.has_symbol(name) {
            return None;
        }
        self.assembler_ctx
            .eval_expr(&Expr::Identifier(name.to_string(), Span::default()))
            .ok()
    }

    fn current_address(&self) -> Option<i64> {
        Some(self.assembler_ctx.current_address() as i64)
    }

    fn pass(&self) -> u8 {
        self.assembler_ctx.pass()
    }

    fn symbol_is_finalized(&self, name: &str) -> Option<bool> {
        self.assembler_ctx.symbol_is_finalized(name)
    }

    fn eval_string_literal(&self, bytes: &[u8]) -> Result<i64, String> {
        self.assembler_ctx
            .eval_expr(&Expr::String(bytes.to_vec(), Span::default()))
    }
}

/// Runnable `.opcore` VM stage: parse an expression from tokenized input using
/// the VM-side runtime expression parser.
pub fn parse_expression_tokens(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Expr, ParseError> {
    parse_expression_tokens_with_opcode_version(
        tokens,
        end_span,
        end_token_text,
        package::EXVM_OPCODE_VERSION_V1,
    )
}

pub(crate) fn parse_expression_tokens_with_opcode_version(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    opcode_version: u16,
) -> Result<Expr, ParseError> {
    let budgets = ExvmExecutionBudgets::for_tokens(tokens.len());
    let program = match opcode_version {
        package::EXVM_OPCODE_VERSION_V1 => EXVM_DEFAULT_PROGRAM_V1,
        package::EXVM_OPCODE_VERSION_V2 => EXVM_DEFAULT_PROGRAM_V2.as_slice(),
        _ => {
            return Err(ParseError {
                message: format!("unsupported EXVM opcode version {}", opcode_version),
                span: end_span,
            })
        }
    };
    run_exvm_expression_parser_program_with_opcode_version(
        tokens,
        end_span,
        end_token_text,
        program,
        budgets,
        opcode_version,
    )
}

#[cfg_attr(not(test), allow(dead_code))]
pub(crate) fn run_exvm_expression_parser_program(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    program: &[u8],
    budgets: ExvmExecutionBudgets,
) -> Result<Expr, ParseError> {
    run_exvm_expression_parser_program_with_opcode_version(
        tokens,
        end_span,
        end_token_text,
        program,
        budgets,
        package::EXVM_OPCODE_VERSION_V1,
    )
}

pub(crate) fn run_exvm_expression_parser_program_with_opcode_version(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    program: &[u8],
    budgets: ExvmExecutionBudgets,
    opcode_version: u16,
) -> Result<Expr, ParseError> {
    match opcode_version {
        package::EXVM_OPCODE_VERSION_V1 => run_exvm_v1_expression_parser_program(
            tokens,
            end_span,
            end_token_text,
            program,
            budgets,
        ),
        package::EXVM_OPCODE_VERSION_V2 => {
            let strict_tokens = tokens.clone();
            let strict_end_token_text = end_token_text.clone();
            crate::exvm_v2_runtime::run_exvm_expression_parser_program(
                tokens,
                end_span,
                end_token_text,
                program,
                budgets,
            )
            .map_err(|err| {
                if let Some(parse_error) = strict_out_of_scope_value_node_error(
                    strict_tokens,
                    end_span,
                    strict_end_token_text,
                ) {
                    parse_error
                } else {
                    err
                }
            })
        }
        _ => Err(ParseError {
            message: format!("unsupported EXVM opcode version {}", opcode_version),
            span: end_span,
        }),
    }
}

fn run_exvm_v1_expression_parser_program(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    program: &[u8],
    budgets: ExvmExecutionBudgets,
) -> Result<Expr, ParseError> {
    if tokens.len() > budgets.max_token_count {
        return Err(ParseError {
            message: format!(
                "EXVM token budget exceeded ({}/{})",
                tokens.len(),
                budgets.max_token_count
            ),
            span: end_span,
        });
    }

    let mut pc = 0usize;
    let mut steps = 0usize;
    let mut output_stack = Vec::new();

    while pc < program.len() {
        if steps >= budgets.max_steps {
            return Err(ParseError {
                message: format!(
                    "EXVM step budget exceeded ({}/{})",
                    steps, budgets.max_steps
                ),
                span: end_span,
            });
        }
        steps += 1;

        let opcode_pc = pc;
        let opcode_byte = program[pc];
        pc += 1;
        let opcode = package::ExvmOpcode::from_u8(opcode_byte).ok_or_else(|| ParseError {
            message: format!("invalid EXVM opcode 0x{opcode_byte:02X} at pc={opcode_pc}"),
            span: end_span,
        })?;

        match opcode {
            package::ExvmOpcode::End => {
                return match output_stack.pop() {
                    Some(expr) if output_stack.is_empty() => Ok(expr),
                    Some(_) => Err(ParseError {
                        message: "EXVM program ended with multiple expressions".to_string(),
                        span: end_span,
                    }),
                    None => Err(ParseError {
                        message: "EXVM program ended without expression".to_string(),
                        span: end_span,
                    }),
                };
            }
            package::ExvmOpcode::ParseExpression => {
                if output_stack.len() >= budgets.max_stack_depth {
                    return Err(ParseError {
                        message: format!(
                            "EXVM output stack depth exceeded ({}/{})",
                            output_stack.len() + 1,
                            budgets.max_stack_depth
                        ),
                        span: end_span,
                    });
                }
                let scalar_result =
                    crate::runtime_expr_parser::parse_exvm_scalar_expression_tokens(
                        tokens.clone(),
                        end_span,
                        end_token_text.clone(),
                    )
                    .or_else(|err| {
                        if budgets.allow_out_of_scope_compatibility {
                            if let Some(expr) = parse_out_of_scope_compatibility_expr(
                                tokens.clone(),
                                end_span,
                                end_token_text.clone(),
                            ) {
                                return Ok(expr);
                            }
                        }

                        if let Some(parse_error) = strict_out_of_scope_value_node_error(
                            tokens.clone(),
                            end_span,
                            end_token_text.clone(),
                        ) {
                            Err(parse_error)
                        } else {
                            Err(err)
                        }
                    })?;
                output_stack.push(scalar_result);
            }
            package::ExvmOpcode::EmitDiag => {
                return Err(ParseError {
                    message: "EXVM emitted diagnostic".to_string(),
                    span: end_span,
                });
            }
            package::ExvmOpcode::Fail => {
                return Err(ParseError {
                    message: "EXVM program failed".to_string(),
                    span: end_span,
                });
            }
        }
    }

    Err(ParseError {
        message: "EXVM program missing End opcode".to_string(),
        span: end_span,
    })
}

enum StrictOutOfScopeValueNode {
    Call(Span),
    Placeholder(Span),
}

impl StrictOutOfScopeValueNode {
    fn message(&self) -> &'static str {
        match self {
            Self::Call(_) => "EXVM strict mode does not cover function/call expressions",
            Self::Placeholder(_) => "EXVM strict mode does not cover placeholder expressions",
        }
    }

    fn span(&self) -> Span {
        match self {
            Self::Call(span) | Self::Placeholder(span) => *span,
        }
    }
}

fn parse_out_of_scope_compatibility_expr(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> Option<Expr> {
    let expr =
        crate::runtime_expr_parser::RuntimeExpressionParser::new(tokens, end_span, end_token_text)
            .parse_expr_from_tokens()
            .ok()?;
    find_strict_out_of_scope_value_node(&expr)?;
    Some(expr)
}

fn strict_out_of_scope_value_node_error(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> Option<ParseError> {
    let expr = parse_out_of_scope_compatibility_expr(tokens, end_span, end_token_text)?;
    let node = find_strict_out_of_scope_value_node(&expr)?;
    Some(ParseError {
        message: node.message().to_string(),
        span: node.span(),
    })
}

fn find_strict_out_of_scope_value_node(expr: &Expr) -> Option<StrictOutOfScopeValueNode> {
    match expr {
        Expr::Call { span, .. } => Some(StrictOutOfScopeValueNode::Call(*span)),
        Expr::Placeholder(span) => Some(StrictOutOfScopeValueNode::Placeholder(*span)),
        Expr::List(elements, _) | Expr::Tuple(elements, _) => elements
            .iter()
            .find_map(find_strict_out_of_scope_value_node),
        Expr::Index { base, index, .. } => find_strict_out_of_scope_value_node(base)
            .or_else(|| find_strict_out_of_scope_value_node(index)),
        Expr::Member { base, .. } => find_strict_out_of_scope_value_node(base),
        Expr::StructLiteral { fields, .. } => fields
            .iter()
            .find_map(|(_, field_expr)| find_strict_out_of_scope_value_node(field_expr)),
        Expr::Indirect(expr, _)
        | Expr::Immediate(expr, _)
        | Expr::IndirectLong(expr, _)
        | Expr::Unary { expr, .. } => find_strict_out_of_scope_value_node(expr),
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => find_strict_out_of_scope_value_node(cond)
            .or_else(|| find_strict_out_of_scope_value_node(then_expr))
            .or_else(|| find_strict_out_of_scope_value_node(else_expr)),
        Expr::Binary { left, right, .. } => find_strict_out_of_scope_value_node(left)
            .or_else(|| find_strict_out_of_scope_value_node(right)),
        Expr::Range {
            start, end, step, ..
        } => find_strict_out_of_scope_value_node(start)
            .or_else(|| find_strict_out_of_scope_value_node(end))
            .or_else(|| {
                step.as_deref()
                    .and_then(find_strict_out_of_scope_value_node)
            }),
        Expr::Number(_, _)
        | Expr::Identifier(_, _)
        | Expr::Register(_, _)
        | Expr::Dollar(_)
        | Expr::String(_, _)
        | Expr::Error(_, _) => None,
    }
}

/// Runnable `.opcore` VM stage: evaluate an expression for assembler use
/// through the VM-backed portable expression runtime and resolved budgets.
pub fn evaluate_expression_for_assembler(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    expr: &Expr,
    ctx: &dyn AssemblerContext,
) -> Result<i64, String> {
    let program = compile_core_expr_to_portable_program(expr).map_err(|err| err.to_string())?;
    model
        .evaluate_portable_expression_program_with_contract_for_assembler(
            cpu_id,
            dialect_override,
            &program,
            ctx,
        )
        .map(|evaluation| evaluation.value)
        .map_err(|err| err.to_string())
}

/// Runnable `.opcore` VM stage: determine whether an expression still depends
/// on unstable symbols through the VM-backed portable expression runtime.
pub fn expression_has_unstable_symbols_for_assembler(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    expr: &Expr,
    ctx: &dyn AssemblerContext,
) -> Result<bool, String> {
    let program = compile_core_expr_to_portable_program(expr).map_err(|err| err.to_string())?;
    model
        .portable_expression_has_unstable_symbols_with_contract_for_assembler(
            cpu_id,
            dialect_override,
            &program,
            ctx,
        )
        .map_err(|err| err.to_string())
}

/// Runnable `.opcore` VM stage: parse a core-language module/import line
/// through the VM-backed line parser and keep only core-owned module-item
/// forms.
pub fn process_module_item_request_with_model(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> ProcessingOutcome<opcore::parser::LineAst, ParseError> {
    match crate::vm_opasm::parse_statement_line_with_model(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
    ) {
        Ok((ast, _, _)) => match ast {
            opcore::parser::LineAst::Use(..) => ProcessingOutcome::Done(ast),
            ref line_ast @ opcore::parser::LineAst::Statement(ref statement) => {
                let Some(mnemonic) = statement.mnemonic.as_deref() else {
                    return ProcessingOutcome::Return(ProcessingReturn::Unknown);
                };
                if mnemonic.eq_ignore_ascii_case(".module")
                    || mnemonic.eq_ignore_ascii_case(".endmodule")
                {
                    ProcessingOutcome::Done(line_ast.clone())
                } else {
                    ProcessingOutcome::Return(ProcessingReturn::Unknown)
                }
            }
            _ => ProcessingOutcome::Return(ProcessingReturn::Unknown),
        },
        Err(err) => ProcessingOutcome::Error(err),
    }
}

pub(crate) fn enforce_expr_token_budget(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
) -> Result<(), ParseError> {
    let token_budget = expr_parse_ctx
        .model
        .runtime_budget_limits()
        .max_parser_tokens_per_line;
    if tokens.len() > token_budget {
        let fallback_message = format!(
            "parser token budget exceeded ({} > {})",
            tokens.len(),
            token_budget
        );
        if let Some(contract) = expr_parse_ctx
            .model
            .resolve_parser_contract(expr_parse_ctx.cpu_id, expr_parse_ctx.dialect_override)
            .ok()
            .flatten()
        {
            return Err(runtime_bridge_error_to_parse_error(
                RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic::new(
                    contract.diagnostics.invalid_statement,
                    fallback_message,
                    Some(end_span),
                )),
                end_span,
            ));
        }
        return Err(ParseError {
            message: fallback_message,
            span: end_span,
        });
    }
    Ok(())
}

#[allow(dead_code)]
pub(crate) fn parse_expr_program_ref_with_vm_contract(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    parser_vm_opcode_version: Option<u16>,
) -> Result<(PortableExprRef, PortableExprProgram), ParseError> {
    enforce_expr_token_budget(expr_parse_ctx, tokens, end_span)?;
    let mut owned_tokens = Vec::with_capacity(tokens.len());
    owned_tokens.extend_from_slice(tokens);
    let program = expr_parse_ctx
        .model
        .compile_expression_program_with_parser_vm_opt_in_for_assembler(
            expr_parse_ctx.cpu_id,
            expr_parse_ctx.dialect_override,
            owned_tokens,
            end_span,
            end_token_text,
            parser_vm_opcode_version,
        )?;
    Ok((PortableExprRef { index: 0 }, program))
}

pub(crate) fn parse_expr_with_vm_contract(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Expr, ParseError> {
    if let Some(expr) =
        try_process_expr_request(expr_parse_ctx, tokens, end_span, end_token_text.clone())?
    {
        return Ok(expr);
    }
    enforce_expr_token_budget(expr_parse_ctx, tokens, end_span)?;
    expr_parse_ctx
        .model
        .validate_expression_parser_contract_for_assembler(
            expr_parse_ctx.cpu_id,
            expr_parse_ctx.dialect_override,
        )
        .map_err(|err| runtime_bridge_error_to_parse_error(err, end_span))?;

    let mut owned_tokens = Vec::with_capacity(tokens.len());
    owned_tokens.extend_from_slice(tokens);
    expr_parse_ctx.model.parse_expression_for_assembler(
        expr_parse_ctx.cpu_id,
        expr_parse_ctx.dialect_override,
        owned_tokens,
        end_span,
        end_token_text,
    )
}

fn try_process_expr_request(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Option<Expr>, ParseError> {
    let Some(ref handler_cell) = expr_parse_ctx.expr_handler else {
        return Ok(None);
    };
    let mut handler = handler_cell.borrow_mut();
    match handler.process_expr_request(
        ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr),
        tokens.to_vec(),
        end_span,
        end_token_text,
    ) {
        ProcessingOutcome::Done(expr) => Ok(Some(expr)),
        ProcessingOutcome::Error(err) => Err(err),
        ProcessingOutcome::Return(ProcessingReturn::Unknown) => Ok(None),
        ProcessingOutcome::Return(ProcessingReturn::Request { request }) => Err(ParseError {
            message: format!("Unsupported returned expression request: {request:?}"),
            span: end_span,
        }),
    }
}

pub(crate) fn parse_expr_with_vm_contract_and_boundary(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    boundary_token: Option<&Token>,
) -> Result<Expr, ParseError> {
    match parse_expr_with_vm_contract(expr_parse_ctx, tokens, end_span, end_token_text) {
        Ok(expr) => Ok(expr),
        Err(err)
            if err.message == crate::execution_model::HOST_PARSER_UNEXPECTED_END_OF_EXPRESSION
                && boundary_token.is_some() =>
        {
            let boundary_span = boundary_token.map(|token| token.span).unwrap_or(err.span);
            Err(ParseError {
                message: "Unexpected token in expression".to_string(),
                span: boundary_span,
            })
        }
        Err(err) => Err(err),
    }
}

pub(crate) fn parse_expr_with_authoritative_exvm_contract(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Expr, ParseError> {
    if expr_parse_ctx.expr_handler.is_some() {
        return parse_expr_with_vm_contract(expr_parse_ctx, tokens, end_span, end_token_text);
    }

    enforce_expr_token_budget(expr_parse_ctx, tokens, end_span)?;
    expr_parse_ctx
        .model
        .ensure_parser_vm_v2_expr_subcall_contract_for_assembler(
            expr_parse_ctx.cpu_id,
            expr_parse_ctx.dialect_override,
        )
        .map_err(|err| runtime_bridge_error_to_parse_error(err, end_span))?;

    let mut owned_tokens = Vec::with_capacity(tokens.len());
    owned_tokens.extend_from_slice(tokens);
    let opcode_version = expr_parse_ctx
        .model
        .resolve_expr_parser_opcode_version_for_assembler(
            expr_parse_ctx.cpu_id,
            expr_parse_ctx.dialect_override,
            end_span,
        )?;
    expr_parse_ctx
        .model
        .parse_expression_with_mode_for_assembler(
            expr_parse_ctx.cpu_id,
            expr_parse_ctx.dialect_override,
            owned_tokens,
            end_span,
            end_token_text,
            Some(opcode_version),
        )
}

pub(crate) fn parse_expr_with_authoritative_exvm_contract_and_boundary(
    expr_parse_ctx: &VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    boundary_token: Option<&Token>,
) -> Result<Expr, ParseError> {
    match parse_expr_with_authoritative_exvm_contract(
        expr_parse_ctx,
        tokens,
        end_span,
        end_token_text,
    ) {
        Ok(expr) => Ok(expr),
        Err(err)
            if err.message == crate::execution_model::HOST_PARSER_UNEXPECTED_END_OF_EXPRESSION
                && boundary_token.is_some() =>
        {
            let boundary_span = boundary_token.map(|token| token.span).unwrap_or(err.span);
            Err(ParseError {
                message: "Unexpected token in expression".to_string(),
                span: boundary_span,
            })
        }
        Err(err) => Err(err),
    }
}

pub fn load_model_from_registry(
    registry: &registry::registry::ModuleRegistry,
) -> Result<HierarchyExecutionModel, crate::vm_core::RuntimeModelLoadError> {
    crate::vm_core::load_execution_model_from_registry(registry)
}

pub fn load_model_from_chunks(
    chunks: package::HierarchyChunks,
) -> Result<HierarchyExecutionModel, crate::vm_core::RuntimeModelLoadError> {
    crate::vm_core::load_execution_model_from_chunks(chunks)
}

pub fn load_model_from_package_bytes(
    bytes: &[u8],
) -> Result<HierarchyExecutionModel, crate::vm_core::RuntimeModelLoadError> {
    crate::vm_core::load_execution_model_from_package_bytes(bytes)
}

impl HierarchyExecutionModel {
    pub fn parse_expression_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<Expr, ParseError> {
        let use_vm_parser = self.resolve_expr_parser_vm_rollout_for_assembler(
            cpu_id,
            dialect_override,
            false,
            end_span,
        )?;

        let expr_parser_opcode_version = if use_vm_parser {
            Some(self.resolve_expr_parser_opcode_version_for_assembler(
                cpu_id,
                dialect_override,
                end_span,
            )?)
        } else {
            None
        };

        self.parse_expression_with_mode_for_assembler(
            cpu_id,
            dialect_override,
            tokens,
            end_span,
            end_token_text,
            expr_parser_opcode_version,
        )
    }

    fn parse_expression_with_mode_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
        expr_parser_opcode_version: Option<u16>,
    ) -> Result<Expr, ParseError> {
        self.validate_parser_contract_for_assembler(cpu_id, dialect_override, tokens.len())
            .map_err(|err| ParseError {
                message: err.to_string(),
                span: end_span,
            })?;

        if let Some(opcode_version) = expr_parser_opcode_version {
            return parse_expression_tokens_with_opcode_version(
                tokens,
                end_span,
                end_token_text,
                opcode_version,
            );
        }

        #[cfg(test)]
        if CORE_EXPR_PARSER_FAILPOINT.with(|flag| flag.get()) {
            return Err(ParseError {
                message: "core expression parser failpoint".to_string(),
                span: end_span,
            });
        }

        Parser::parse_expr_from_tokens(tokens, end_span, end_token_text)
    }

    fn resolve_expr_parser_vm_rollout_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        force_vm_parser: bool,
        end_span: Span,
    ) -> Result<bool, ParseError> {
        if force_vm_parser {
            return Ok(true);
        }

        let resolved = self
            .resolve_pipeline(cpu_id, dialect_override)
            .map_err(|err| ParseError {
                message: err.to_string(),
                span: end_span,
            })?;

        Ok(portable_expr_parser_runtime_enabled_for_family(
            resolved.family_id.as_str(),
            &[],
            &[],
        ))
    }

    fn compile_parsed_expression_for_assembler(
        expr: &Expr,
        end_span: Span,
    ) -> Result<PortableExprProgram, ParseError> {
        compile_core_expr_to_portable_program(expr).map_err(|err| ParseError {
            message: err.to_string(),
            span: err.span.unwrap_or(end_span),
        })
    }

    pub fn compile_expression_program_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<PortableExprProgram, ParseError> {
        let expr = self.parse_expression_for_assembler(
            cpu_id,
            dialect_override,
            tokens,
            end_span,
            end_token_text,
        )?;
        Self::compile_parsed_expression_for_assembler(&expr, end_span)
    }

    fn resolve_expr_parser_opcode_version_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        end_span: Span,
    ) -> Result<u16, ParseError> {
        let contract = self
            .resolve_expr_parser_contract(cpu_id, dialect_override)
            .map_err(|err| ParseError {
                message: err.to_string(),
                span: end_span,
            })?;
        Ok(contract
            .as_ref()
            .map(|entry| entry.opcode_version)
            .unwrap_or(package::EXVM_OPCODE_VERSION_V1))
    }

    pub fn parse_expression_program_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<PortableExprProgram, ParseError> {
        self.compile_expression_program_with_parser_vm_opt_in_for_assembler(
            cpu_id,
            dialect_override,
            tokens,
            end_span,
            end_token_text,
            None,
        )
    }

    pub fn validate_expression_parser_contract_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<(), RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        let use_expr_parser_vm =
            portable_expr_parser_runtime_enabled_for_family(resolved.family_id.as_str(), &[], &[]);
        if !use_expr_parser_vm {
            return Ok(());
        }

        let contract = self.resolve_expr_parser_contract(cpu_id, dialect_override)?;
        if let Some(contract) = contract.as_ref() {
            self.ensure_expr_parser_contract_compatible_for_assembler(contract)?;
        }
        Ok(())
    }

    pub fn compile_expression_program_with_parser_vm_opt_in_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
        parser_vm_opcode_version: Option<u16>,
    ) -> Result<PortableExprProgram, ParseError> {
        let use_expr_parser_vm = self.resolve_expr_parser_vm_rollout_for_assembler(
            cpu_id,
            dialect_override,
            parser_vm_opcode_version.is_some(),
            end_span,
        )?;
        if !use_expr_parser_vm {
            let expr = self.parse_expression_with_mode_for_assembler(
                cpu_id,
                dialect_override,
                tokens,
                end_span,
                end_token_text,
                None,
            );
            return expr
                .and_then(|expr| Self::compile_parsed_expression_for_assembler(&expr, end_span));
        }

        let contract = self
            .resolve_expr_parser_contract(cpu_id, dialect_override)
            .map_err(|err| ParseError {
                message: err.to_string(),
                span: end_span,
            })?;

        if let Some(contract) = contract.as_ref() {
            self.ensure_expr_parser_contract_compatible_for_assembler(contract)
                .map_err(|err| ParseError {
                    message: err.to_string(),
                    span: end_span,
                })?;
        }

        let opcode_version = parser_vm_opcode_version
            .or_else(|| contract.as_ref().map(|entry| entry.opcode_version))
            .unwrap_or(package::EXVM_OPCODE_VERSION_V1);
        if opcode_version != package::EXVM_OPCODE_VERSION_V1
            && opcode_version != package::EXVM_OPCODE_VERSION_V2
        {
            return Err(ParseError {
                message: format!("unsupported EXVM opcode version {}", opcode_version),
                span: end_span,
            });
        }

        let expr = self.parse_expression_with_mode_for_assembler(
            cpu_id,
            dialect_override,
            tokens,
            end_span,
            end_token_text,
            Some(opcode_version),
        )?;
        Self::compile_parsed_expression_for_assembler(&expr, end_span)
    }

    pub fn evaluate_portable_expression_program_for_assembler(
        &self,
        program: &PortableExprProgram,
        budgets: PortableExprBudgets,
        ctx: &dyn AssemblerContext,
    ) -> Result<PortableExprEvaluation, RuntimeBridgeError> {
        let adapter = RuntimePortableExprEvalContext { assembler_ctx: ctx };
        eval_portable_expr_program(program, &adapter, budgets)
            .map_err(|err| RuntimeBridgeError::Resolve(err.to_string()))
    }

    pub fn evaluate_portable_expression_program_with_contract_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        program: &PortableExprProgram,
        ctx: &dyn AssemblerContext,
    ) -> Result<PortableExprEvaluation, RuntimeBridgeError> {
        let budgets = self.resolve_expr_budgets(cpu_id, dialect_override)?;
        self.evaluate_portable_expression_program_for_assembler(program, budgets, ctx)
    }

    pub fn portable_expression_has_unstable_symbols_for_assembler(
        &self,
        program: &PortableExprProgram,
        budgets: PortableExprBudgets,
        ctx: &dyn AssemblerContext,
    ) -> Result<bool, RuntimeBridgeError> {
        let adapter = RuntimePortableExprEvalContext { assembler_ctx: ctx };
        expr_program_has_unstable_symbols(program, &adapter, budgets)
            .map_err(|err| RuntimeBridgeError::Resolve(err.to_string()))
    }

    pub fn portable_expression_has_unstable_symbols_with_contract_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        program: &PortableExprProgram,
        ctx: &dyn AssemblerContext,
    ) -> Result<bool, RuntimeBridgeError> {
        let budgets = self.resolve_expr_budgets(cpu_id, dialect_override)?;
        self.portable_expression_has_unstable_symbols_for_assembler(program, budgets, ctx)
    }
}
