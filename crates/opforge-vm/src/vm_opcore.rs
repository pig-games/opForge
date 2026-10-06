// SPDX-License-Identifier: GPL-3.0-or-later

//! `.opcore` VM surface.
//!
//! This groups VM-side functionality that primarily supports the language/core
//! processor domain rather than assembler instruction selection or encoding.

use opcore::expr_vm::{
    compile_core_expr_to_portable_program_with_opcode_version,
    compile_portable_expr_direct_leaf_to_program_with_opcode_version,
    compile_portable_expr_direct_member_index_to_program_with_opcode_version,
    compile_portable_expr_direct_scalar_to_program_with_opcode_version,
    compile_portable_expr_direct_structural_to_program_with_opcode_version,
    eval_portable_expr_program, expr_is_supported_by_direct_member_index_lowering,
    expr_is_supported_by_direct_scalar_lowering, expr_is_supported_by_direct_structural_lowering,
    expr_program_has_unstable_symbols, PortableExprBudgets, PortableExprDirectLeaf,
    PortableExprEvalContext, PortableExprEvaluation, PortableExprProgram, PortableExprRangeValueV2,
    PortableExprRef, PortableExprStructFieldValueV2, PortableExprStructLiteralValueV2,
    PortableExprStructTypeFieldValueV2, PortableExprStructTypeValueV2, PortableExprValueV2,
};
use opcore::parser::{Expr, ParseError, Parser};
use opcore::tokenizer::{Span, Token, TokenKind};
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
use types::asm_value::AsmValue;

static EXVM_DEFAULT_PROGRAM: LazyLock<Vec<u8>> = LazyLock::new(build_expression_parser_program);

/// Shared current expression grammar for preparation runtimes.
/// Consumers adapt their token payloads; precedence and construction remain in
/// this program rather than a platform-specific parser.
pub fn expression_parser_program() -> &'static [u8] {
    EXVM_DEFAULT_PROGRAM.as_slice()
}

struct ExvmDefaultProgramBuilder {
    bytes: Vec<u8>,
    labels: HashMap<&'static str, usize>,
    patches: Vec<(&'static str, usize)>,
}

impl ExvmDefaultProgramBuilder {
    fn new() -> Self {
        Self {
            bytes: Vec::new(),
            labels: HashMap::new(),
            patches: Vec::new(),
        }
    }

    fn mark(&mut self, label: &'static str) {
        let prev = self.labels.insert(label, self.bytes.len());
        assert!(prev.is_none(), "duplicate EXVM current label: {label}");
    }

    fn opcode(&mut self, opcode: package::ExvmOpcode) {
        self.bytes.push(opcode as u8);
    }

    fn operator(&mut self, operator: package::ExvmOperatorKind) {
        self.bytes.push(operator as u8);
    }

    fn token_kind(&mut self, kind: package::ExvmTokenKind) {
        self.bytes.push(kind as u8);
    }

    fn byte(&mut self, value: u8) {
        self.bytes.push(value);
    }

    fn push_label_target(&mut self, label: &'static str) {
        let offset = self.bytes.len();
        self.bytes.extend_from_slice(&0u16.to_le_bytes());
        self.patches.push((label, offset));
    }

    fn call(&mut self, label: &'static str) {
        self.opcode(package::ExvmOpcode::Call);
        self.push_label_target(label);
    }

    fn jump(&mut self, label: &'static str) {
        self.opcode(package::ExvmOpcode::Jump);
        self.push_label_target(label);
    }

    fn jump_if_true(&mut self, label: &'static str) {
        self.opcode(package::ExvmOpcode::JumpIfTrue);
        self.push_label_target(label);
    }

    fn ret(&mut self) {
        self.opcode(package::ExvmOpcode::Return);
    }

    fn peek_kind_jump_if_true(&mut self, kind: package::ExvmTokenKind, label: &'static str) {
        self.opcode(package::ExvmOpcode::PeekKind);
        self.token_kind(kind);
        self.jump_if_true(label);
    }

    fn peek_operator_jump_if_true(
        &mut self,
        operator: package::ExvmOperatorKind,
        label: &'static str,
    ) {
        self.opcode(package::ExvmOpcode::PeekOperator);
        self.operator(operator);
        self.jump_if_true(label);
    }

    fn consume_operator(&mut self, operator: package::ExvmOperatorKind) {
        self.opcode(package::ExvmOpcode::ConsumeOperator);
        self.operator(operator);
    }

    fn consume_kind(&mut self, kind: package::ExvmTokenKind) {
        self.opcode(package::ExvmOpcode::ConsumeKind);
        self.token_kind(kind);
    }

    fn build_unary(&mut self, operator: package::ExvmOperatorKind) {
        self.opcode(package::ExvmOpcode::BuildUnary);
        self.operator(operator);
    }

    fn build_binary(&mut self, operator: package::ExvmOperatorKind) {
        self.opcode(package::ExvmOpcode::BuildBinary);
        self.operator(operator);
    }

    fn build_ternary(&mut self) {
        self.opcode(package::ExvmOpcode::BuildTernary);
    }

    fn build_range(&mut self, inclusive: bool, has_step: bool) {
        self.opcode(package::ExvmOpcode::BuildRange);
        self.byte(u8::from(inclusive) | (u8::from(has_step) << 1));
    }

    fn parse_struct_literal_if_present(&mut self) {
        self.opcode(package::ExvmOpcode::ParseStructLiteralIfPresent);
    }

    fn parse_postfix_chain(&mut self) {
        self.opcode(package::ExvmOpcode::ParsePostfixChain);
    }

    fn finish(mut self) -> Vec<u8> {
        for (label, offset) in self.patches {
            let target = *self
                .labels
                .get(label)
                .unwrap_or_else(|| panic!("missing EXVM current label: {label}"));
            let target = u16::try_from(target).expect("EXVM current program exceeds u16");
            self.bytes[offset..offset + 2].copy_from_slice(&target.to_le_bytes());
        }
        self.bytes
    }
}

fn build_expression_parser_program() -> Vec<u8> {
    let mut builder = ExvmDefaultProgramBuilder::new();

    builder.call("expression");
    builder.opcode(package::ExvmOpcode::End);

    builder.mark("expression");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Lt, "expression_low");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Gt, "expression_high");
    builder.call("ternary");
    builder.ret();
    builder.mark("expression_low");
    builder.consume_operator(package::ExvmOperatorKind::Lt);
    builder.call("expression");
    builder.build_unary(package::ExvmOperatorKind::Lt);
    builder.ret();
    builder.mark("expression_high");
    builder.consume_operator(package::ExvmOperatorKind::Gt);
    builder.call("expression");
    builder.build_unary(package::ExvmOperatorKind::Gt);
    builder.ret();

    builder.mark("ternary");
    builder.call("logical_or");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::Question, "ternary_build");
    builder.ret();
    builder.mark("ternary_build");
    builder.consume_kind(package::ExvmTokenKind::Question);
    builder.call("expression");
    builder.consume_kind(package::ExvmTokenKind::Colon);
    builder.call("expression");
    builder.build_ternary();
    builder.ret();

    builder.mark("logical_or");
    builder.call("logical_and");
    builder.mark("logical_or_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::LogicOr, "logical_or_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::LogicXor, "logical_xor_build");
    builder.ret();
    builder.mark("logical_or_build");
    builder.consume_operator(package::ExvmOperatorKind::LogicOr);
    builder.call("logical_and");
    builder.build_binary(package::ExvmOperatorKind::LogicOr);
    builder.jump("logical_or_loop");
    builder.mark("logical_xor_build");
    builder.consume_operator(package::ExvmOperatorKind::LogicXor);
    builder.call("logical_and");
    builder.build_binary(package::ExvmOperatorKind::LogicXor);
    builder.jump("logical_or_loop");

    builder.mark("logical_and");
    builder.call("bit_or");
    builder.mark("logical_and_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::LogicAnd, "logical_and_build");
    builder.ret();
    builder.mark("logical_and_build");
    builder.consume_operator(package::ExvmOperatorKind::LogicAnd);
    builder.call("bit_or");
    builder.build_binary(package::ExvmOperatorKind::LogicAnd);
    builder.jump("logical_and_loop");

    builder.mark("bit_or");
    builder.call("bit_xor");
    builder.mark("bit_or_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::BitOr, "bit_or_build");
    builder.ret();
    builder.mark("bit_or_build");
    builder.consume_operator(package::ExvmOperatorKind::BitOr);
    builder.call("bit_xor");
    builder.build_binary(package::ExvmOperatorKind::BitOr);
    builder.jump("bit_or_loop");

    builder.mark("bit_xor");
    builder.call("bit_and");
    builder.mark("bit_xor_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::BitXor, "bit_xor_build");
    builder.ret();
    builder.mark("bit_xor_build");
    builder.consume_operator(package::ExvmOperatorKind::BitXor);
    builder.call("bit_and");
    builder.build_binary(package::ExvmOperatorKind::BitXor);
    builder.jump("bit_xor_loop");

    builder.mark("bit_and");
    builder.call("range");
    builder.mark("bit_and_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::BitAnd, "bit_and_build");
    builder.ret();
    builder.mark("bit_and_build");
    builder.consume_operator(package::ExvmOperatorKind::BitAnd);
    builder.call("range");
    builder.build_binary(package::ExvmOperatorKind::BitAnd);
    builder.jump("bit_and_loop");

    builder.mark("range");
    builder.call("compare");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Range, "range_exclusive");
    builder
        .peek_operator_jump_if_true(package::ExvmOperatorKind::RangeInclusive, "range_inclusive");
    builder.ret();
    builder.mark("range_exclusive");
    builder.consume_operator(package::ExvmOperatorKind::Range);
    builder.call("compare");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::Colon, "range_exclusive_step");
    builder.build_range(false, false);
    builder.ret();
    builder.mark("range_exclusive_step");
    builder.consume_kind(package::ExvmTokenKind::Colon);
    builder.call("compare");
    builder.build_range(false, true);
    builder.ret();
    builder.mark("range_inclusive");
    builder.consume_operator(package::ExvmOperatorKind::RangeInclusive);
    builder.call("compare");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::Colon, "range_inclusive_step");
    builder.build_range(true, false);
    builder.ret();
    builder.mark("range_inclusive_step");
    builder.consume_kind(package::ExvmTokenKind::Colon);
    builder.call("compare");
    builder.build_range(true, true);
    builder.ret();

    builder.mark("compare");
    builder.call("shift");
    builder.mark("compare_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Eq, "compare_eq_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Ne, "compare_ne_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Ge, "compare_ge_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Gt, "compare_gt_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Le, "compare_le_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Lt, "compare_lt_build");
    builder.ret();
    builder.mark("compare_eq_build");
    builder.consume_operator(package::ExvmOperatorKind::Eq);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKind::Eq);
    builder.jump("compare_loop");
    builder.mark("compare_ne_build");
    builder.consume_operator(package::ExvmOperatorKind::Ne);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKind::Ne);
    builder.jump("compare_loop");
    builder.mark("compare_ge_build");
    builder.consume_operator(package::ExvmOperatorKind::Ge);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKind::Ge);
    builder.jump("compare_loop");
    builder.mark("compare_gt_build");
    builder.consume_operator(package::ExvmOperatorKind::Gt);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKind::Gt);
    builder.jump("compare_loop");
    builder.mark("compare_le_build");
    builder.consume_operator(package::ExvmOperatorKind::Le);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKind::Le);
    builder.jump("compare_loop");
    builder.mark("compare_lt_build");
    builder.consume_operator(package::ExvmOperatorKind::Lt);
    builder.call("shift");
    builder.build_binary(package::ExvmOperatorKind::Lt);
    builder.jump("compare_loop");

    builder.mark("shift");
    builder.call("sum");
    builder.mark("shift_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Shl, "shift_shl_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Shr, "shift_shr_build");
    builder.ret();
    builder.mark("shift_shl_build");
    builder.consume_operator(package::ExvmOperatorKind::Shl);
    builder.call("sum");
    builder.build_binary(package::ExvmOperatorKind::Shl);
    builder.jump("shift_loop");
    builder.mark("shift_shr_build");
    builder.consume_operator(package::ExvmOperatorKind::Shr);
    builder.call("sum");
    builder.build_binary(package::ExvmOperatorKind::Shr);
    builder.jump("shift_loop");

    builder.mark("sum");
    builder.call("term");
    builder.mark("sum_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Plus, "sum_plus_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Minus, "sum_minus_build");
    builder.ret();
    builder.mark("sum_plus_build");
    builder.consume_operator(package::ExvmOperatorKind::Plus);
    builder.call("term");
    builder.build_binary(package::ExvmOperatorKind::Plus);
    builder.jump("sum_loop");
    builder.mark("sum_minus_build");
    builder.consume_operator(package::ExvmOperatorKind::Minus);
    builder.call("term");
    builder.build_binary(package::ExvmOperatorKind::Minus);
    builder.jump("sum_loop");

    builder.mark("term");
    builder.call("power");
    builder.mark("term_loop");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Multiply, "term_multiply_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Divide, "term_divide_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Mod, "term_mod_build");
    builder.ret();
    builder.mark("term_multiply_build");
    builder.consume_operator(package::ExvmOperatorKind::Multiply);
    builder.call("power");
    builder.build_binary(package::ExvmOperatorKind::Multiply);
    builder.jump("term_loop");
    builder.mark("term_divide_build");
    builder.consume_operator(package::ExvmOperatorKind::Divide);
    builder.call("power");
    builder.build_binary(package::ExvmOperatorKind::Divide);
    builder.jump("term_loop");
    builder.mark("term_mod_build");
    builder.consume_operator(package::ExvmOperatorKind::Mod);
    builder.call("power");
    builder.build_binary(package::ExvmOperatorKind::Mod);
    builder.jump("term_loop");

    builder.mark("power");
    builder.call("unary");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Power, "power_build");
    builder.ret();
    builder.mark("power_build");
    builder.consume_operator(package::ExvmOperatorKind::Power);
    builder.call("power");
    builder.build_binary(package::ExvmOperatorKind::Power);
    builder.ret();

    builder.mark("unary");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Plus, "unary_plus_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::Minus, "unary_minus_build");
    builder.peek_operator_jump_if_true(package::ExvmOperatorKind::BitNot, "unary_bit_not_build");
    builder
        .peek_operator_jump_if_true(package::ExvmOperatorKind::LogicNot, "unary_logic_not_build");
    builder.call("primary");
    builder.ret();
    builder.mark("unary_plus_build");
    builder.consume_operator(package::ExvmOperatorKind::Plus);
    builder.call("unary");
    builder.build_unary(package::ExvmOperatorKind::Plus);
    builder.ret();
    builder.mark("unary_minus_build");
    builder.consume_operator(package::ExvmOperatorKind::Minus);
    builder.call("unary");
    builder.build_unary(package::ExvmOperatorKind::Minus);
    builder.ret();
    builder.mark("unary_bit_not_build");
    builder.consume_operator(package::ExvmOperatorKind::BitNot);
    builder.call("unary");
    builder.build_unary(package::ExvmOperatorKind::BitNot);
    builder.ret();
    builder.mark("unary_logic_not_build");
    builder.consume_operator(package::ExvmOperatorKind::LogicNot);
    builder.call("unary");
    builder.build_unary(package::ExvmOperatorKind::LogicNot);
    builder.ret();
    builder.mark("primary");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::Number, "primary_number");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::String, "primary_string");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::Identifier, "primary_identifier");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::Register, "primary_register");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::Question, "primary_placeholder");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::Dot, "primary_call");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::Dollar, "primary_dollar");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::OpenParen, "primary_grouping");
    builder.peek_kind_jump_if_true(package::ExvmTokenKind::OpenBrace, "primary_list");
    builder.opcode(package::ExvmOpcode::EmitDiag);
    builder.mark("primary_number");
    builder.opcode(package::ExvmOpcode::LoadTokenText);
    builder.opcode(package::ExvmOpcode::BuildNumber);
    builder.opcode(package::ExvmOpcode::Advance);
    builder.parse_postfix_chain();
    builder.ret();
    builder.mark("primary_string");
    builder.opcode(package::ExvmOpcode::BuildString);
    builder.opcode(package::ExvmOpcode::Advance);
    builder.parse_postfix_chain();
    builder.ret();
    builder.mark("primary_identifier");
    builder.opcode(package::ExvmOpcode::LoadTokenText);
    builder.opcode(package::ExvmOpcode::BuildIdentifier);
    builder.opcode(package::ExvmOpcode::Advance);
    builder.parse_struct_literal_if_present();
    builder.parse_postfix_chain();
    builder.ret();
    builder.mark("primary_register");
    builder.opcode(package::ExvmOpcode::BuildRegister);
    builder.opcode(package::ExvmOpcode::Advance);
    builder.parse_struct_literal_if_present();
    builder.parse_postfix_chain();
    builder.ret();
    builder.mark("primary_placeholder");
    builder.opcode(package::ExvmOpcode::BuildPlaceholder);
    builder.opcode(package::ExvmOpcode::Advance);
    builder.parse_postfix_chain();
    builder.ret();
    builder.mark("primary_call");
    builder.opcode(package::ExvmOpcode::ParseCall);
    builder.parse_postfix_chain();
    builder.ret();
    builder.mark("primary_dollar");
    builder.opcode(package::ExvmOpcode::BuildCurrentAddress);
    builder.opcode(package::ExvmOpcode::Advance);
    builder.parse_postfix_chain();
    builder.ret();
    builder.mark("primary_grouping");
    builder.opcode(package::ExvmOpcode::ParseGrouping);
    builder.parse_postfix_chain();
    builder.ret();
    builder.mark("primary_list");
    builder.opcode(package::ExvmOpcode::ParseList);
    builder.parse_postfix_chain();
    builder.ret();

    builder.finish()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct ExvmExecutionBudgets {
    pub max_steps: usize,
    pub max_token_count: usize,
    pub max_stack_depth: usize,
}

impl ExvmExecutionBudgets {
    pub(crate) fn for_tokens(token_count: usize) -> Self {
        Self {
            max_steps: token_count.saturating_mul(128).max(128),
            max_token_count: token_count,
            max_stack_depth: token_count.max(1),
        }
    }
}

struct RuntimePortableExprEvalContext<'a> {
    assembler_ctx: &'a dyn AssemblerContext,
}

fn asm_value_to_portable_expr_value(value: AsmValue) -> PortableExprValueV2 {
    match value {
        AsmValue::Scalar(value) => PortableExprValueV2::Int(value),
        AsmValue::Range { start, end, step } => {
            PortableExprValueV2::Range(PortableExprRangeValueV2 {
                start: Box::new(PortableExprValueV2::Int(start)),
                end: Box::new(PortableExprValueV2::Int(end)),
                step: Some(Box::new(PortableExprValueV2::Int(step))),
                inclusive: false,
            })
        }
        AsmValue::List(items) => {
            PortableExprValueV2::List(items.into_iter().map(PortableExprValueV2::Int).collect())
        }
        AsmValue::Struct(def) => PortableExprValueV2::StructType(PortableExprStructTypeValueV2 {
            type_name: def.name,
            fields: def
                .fields
                .into_iter()
                .map(|field| PortableExprStructTypeFieldValueV2 {
                    field_name: field.name,
                    offset: field.offset,
                    size: field.size,
                })
                .collect(),
            size: def.size,
        }),
        AsmValue::StructInstance(instance) => {
            let mut fields: Vec<_> = instance.fields.into_iter().collect();
            fields.sort_by(|left, right| {
                left.0
                    .to_ascii_lowercase()
                    .cmp(&right.0.to_ascii_lowercase())
            });
            PortableExprValueV2::StructLiteral(PortableExprStructLiteralValueV2 {
                type_name: instance.type_name,
                fields: fields
                    .into_iter()
                    .map(|(field_name, value)| PortableExprStructFieldValueV2 {
                        field_name,
                        value: PortableExprValueV2::Int(value),
                    })
                    .collect(),
            })
        }
    }
}

impl PortableExprEvalContext for RuntimePortableExprEvalContext<'_> {
    fn lookup_symbol(&self, name: &str) -> Option<i64> {
        if let Some(value) = self.assembler_ctx.value_symbol(name) {
            return match value {
                AsmValue::Scalar(value) => Some(value),
                _ => None,
            };
        }
        if !self.assembler_ctx.has_symbol(name) {
            return None;
        }
        self.assembler_ctx
            .eval_expr(&Expr::Identifier(name.to_string(), Span::default()))
            .ok()
    }

    fn lookup_symbol_value(&self, name: &str) -> Option<PortableExprValueV2> {
        if let Some(value) = self.assembler_ctx.value_symbol(name) {
            return Some(asm_value_to_portable_expr_value(value));
        }
        if !self.assembler_ctx.has_symbol(name) {
            return None;
        }
        self.assembler_ctx
            .eval_expr(&Expr::Identifier(name.to_string(), Span::default()))
            .ok()
            .map(PortableExprValueV2::Int)
    }

    fn symbol_exists(&self, name: &str) -> bool {
        self.assembler_ctx.has_symbol(name)
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
        package::EXVM_OPCODE_VERSION,
    )
}

/// Execute the current shared grammar used by numeric preparation compilers.
pub(crate) fn parse_expression_tokens_with_opcode_version(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    opcode_version: u16,
) -> Result<Expr, ParseError> {
    let budgets = ExvmExecutionBudgets::for_tokens(tokens.len());
    run_exvm_expression_parser_program_with_opcode_version(
        tokens,
        end_span,
        end_token_text,
        expression_parser_program(),
        budgets,
        opcode_version,
    )
}

#[cfg_attr(not(test), allow(dead_code))]
pub(crate) fn compile_expression_tokens_to_portable_program_with_opcode_versions(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    expr_parser_opcode_version: u16,
    expr_opcode_version: u16,
) -> Result<PortableExprProgram, ParseError> {
    let budgets = ExvmExecutionBudgets::for_tokens(tokens.len());
    if expr_parser_opcode_version != package::EXVM_OPCODE_VERSION {
        return Err(ParseError {
            message: format!(
                "unsupported EXVM opcode version {}",
                expr_parser_opcode_version
            ),
            span: end_span,
        });
    }
    crate::exvm_runtime::run_exvm_expression_parser_program_to_portable_program(
        tokens,
        end_span,
        end_token_text,
        expression_parser_program(),
        budgets,
        expr_opcode_version,
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
        package::EXVM_OPCODE_VERSION,
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
    if opcode_version != package::EXVM_OPCODE_VERSION {
        return Err(ParseError {
            message: format!("unsupported EXVM opcode version {}", opcode_version),
            span: end_span,
        });
    }
    crate::exvm_runtime::run_exvm_expression_parser_program(
        tokens,
        end_span,
        end_token_text,
        program,
        budgets,
    )
}

fn parse_expression_with_core_parser_compatibility_for_assembler(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Expr, ParseError> {
    #[cfg(test)]
    if CORE_EXPR_PARSER_FAILPOINT.with(|flag| flag.get()) {
        return Err(ParseError {
            message: "core expression parser failpoint".to_string(),
            span: end_span,
        });
    }

    Parser::parse_expr_from_tokens(tokens, end_span, end_token_text)
}

fn direct_leaf_from_tokens(tokens: &[Token]) -> Option<PortableExprDirectLeaf> {
    let [token] = tokens else {
        return None;
    };

    match &token.kind {
        TokenKind::Number(text) => Some(PortableExprDirectLeaf::NumberText(text.text.clone())),
        TokenKind::Identifier(name) | TokenKind::Register(name) => {
            Some(PortableExprDirectLeaf::SymbolName(name.clone()))
        }
        TokenKind::Dollar => Some(PortableExprDirectLeaf::CurrentAddress),
        TokenKind::String(bytes) => {
            Some(PortableExprDirectLeaf::StringLiteral(bytes.bytes.clone()))
        }
        _ => None,
    }
}

fn tokens_require_ast_portable_program_fallback(tokens: &[Token]) -> bool {
    tokens
        .iter()
        .any(|token| matches!(token.kind, TokenKind::String(_) | TokenKind::Register(_)))
}

fn try_compile_direct_leaf_expression_program_for_assembler(
    tokens: &[Token],
    expr_opcode_version: u16,
    expr_parser_opcode_version: u16,
    end_span: Span,
) -> Result<Option<PortableExprProgram>, ParseError> {
    if expr_parser_opcode_version != package::EXVM_OPCODE_VERSION {
        return Ok(None);
    }

    let Some(leaf) = direct_leaf_from_tokens(tokens) else {
        return Ok(None);
    };

    compile_portable_expr_direct_leaf_to_program_with_opcode_version(&leaf, expr_opcode_version)
        .map(Some)
        .map_err(|err| ParseError {
            message: err.to_string(),
            span: err.span.unwrap_or(end_span),
        })
}

fn try_compile_direct_scalar_expression_program_for_assembler(
    expr: &Expr,
    expr_opcode_version: u16,
    expr_parser_opcode_version: u16,
    end_span: Span,
) -> Result<Option<PortableExprProgram>, ParseError> {
    if expr_parser_opcode_version != package::EXVM_OPCODE_VERSION {
        return Ok(None);
    }

    if !expr_is_supported_by_direct_scalar_lowering(expr) {
        return Ok(None);
    }

    compile_portable_expr_direct_scalar_to_program_with_opcode_version(expr, expr_opcode_version)
        .map(Some)
        .map_err(|err| ParseError {
            message: err.to_string(),
            span: err.span.unwrap_or(end_span),
        })
}

fn try_compile_direct_structural_expression_program_for_assembler(
    expr: &Expr,
    expr_opcode_version: u16,
    expr_parser_opcode_version: u16,
    end_span: Span,
) -> Result<Option<PortableExprProgram>, ParseError> {
    if expr_parser_opcode_version != package::EXVM_OPCODE_VERSION {
        return Ok(None);
    }

    if !expr_is_supported_by_direct_structural_lowering(expr) {
        return Ok(None);
    }

    compile_portable_expr_direct_structural_to_program_with_opcode_version(
        expr,
        expr_opcode_version,
    )
    .map(Some)
    .map_err(|err| ParseError {
        message: err.to_string(),
        span: err.span.unwrap_or(end_span),
    })
}

fn try_compile_direct_member_index_expression_program_for_assembler(
    expr: &Expr,
    expr_opcode_version: u16,
    expr_parser_opcode_version: u16,
    end_span: Span,
) -> Result<Option<PortableExprProgram>, ParseError> {
    if expr_parser_opcode_version != package::EXVM_OPCODE_VERSION {
        return Ok(None);
    }

    if !expr_is_supported_by_direct_member_index_lowering(expr) {
        return Ok(None);
    }

    compile_portable_expr_direct_member_index_to_program_with_opcode_version(
        expr,
        expr_opcode_version,
    )
    .map(Some)
    .map_err(|err| ParseError {
        message: err.to_string(),
        span: err.span.unwrap_or(end_span),
    })
}

fn compile_expression_program_for_direct_stage(
    expr: &Expr,
    expr_opcode_version: u16,
) -> Result<PortableExprProgram, String> {
    if expr_opcode_version == package::EXPR_VM_OPCODE_VERSION_V2 {
        if expr_is_supported_by_direct_scalar_lowering(expr) {
            return compile_portable_expr_direct_scalar_to_program_with_opcode_version(
                expr,
                expr_opcode_version,
            )
            .map_err(|err| err.to_string());
        }

        if expr_is_supported_by_direct_structural_lowering(expr) {
            return compile_portable_expr_direct_structural_to_program_with_opcode_version(
                expr,
                expr_opcode_version,
            )
            .map_err(|err| err.to_string());
        }

        if expr_is_supported_by_direct_member_index_lowering(expr) {
            return compile_portable_expr_direct_member_index_to_program_with_opcode_version(
                expr,
                expr_opcode_version,
            )
            .map_err(|err| err.to_string());
        }
    }

    compile_core_expr_to_portable_program_with_opcode_version(expr, expr_opcode_version)
        .map_err(|err| err.to_string())
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
    let opcode_version = model
        .resolve_expr_contract(cpu_id, dialect_override)
        .map_err(|err| err.to_string())?
        .as_ref()
        .map(|entry| entry.opcode_version)
        .unwrap_or(package::EXPR_VM_OPCODE_VERSION_V1);
    if opcode_version != package::EXPR_VM_OPCODE_VERSION_V1
        && opcode_version != package::EXPR_VM_OPCODE_VERSION_V2
    {
        return Err(format!(
            "unsupported EXPR opcode version {}",
            opcode_version
        ));
    }
    let program = compile_expression_program_for_direct_stage(expr, opcode_version)?;
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
    let opcode_version = model
        .resolve_expr_contract(cpu_id, dialect_override)
        .map_err(|err| err.to_string())?
        .as_ref()
        .map(|entry| entry.opcode_version)
        .unwrap_or(package::EXPR_VM_OPCODE_VERSION_V1);
    if opcode_version != package::EXPR_VM_OPCODE_VERSION_V1
        && opcode_version != package::EXPR_VM_OPCODE_VERSION_V2
    {
        return Err(format!(
            "unsupported EXPR opcode version {}",
            opcode_version
        ));
    }
    let program = compile_expression_program_for_direct_stage(expr, opcode_version)?;
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
    let use_vm_parser = expr_parse_ctx
        .model
        .resolve_expr_parser_vm_rollout_for_assembler(
            expr_parse_ctx.cpu_id,
            expr_parse_ctx.dialect_override,
            expr_parse_ctx.expr_parser_opt_in_families,
            expr_parse_ctx.expr_parser_force_host_families,
            false,
            end_span,
        )?;

    if expr_parse_ctx.expr_handler.is_some() || !use_vm_parser {
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
            expr_parse_ctx.expr_parser_opt_in_families,
            expr_parse_ctx.expr_parser_force_host_families,
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
        self.parse_expression_for_assembler_with_rollout_overrides(
            cpu_id,
            dialect_override,
            &[],
            &[],
            tokens,
            end_span,
            end_token_text,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub fn parse_expression_for_assembler_with_rollout_overrides(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        expr_parser_opt_in_families: &[String],
        expr_parser_force_host_families: &[String],
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<Expr, ParseError> {
        let use_vm_parser = self.resolve_expr_parser_vm_rollout_for_assembler(
            cpu_id,
            dialect_override,
            expr_parser_opt_in_families,
            expr_parser_force_host_families,
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
            expr_parser_opt_in_families,
            expr_parser_force_host_families,
            tokens,
            end_span,
            end_token_text,
            expr_parser_opcode_version,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn parse_expression_with_mode_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        expr_parser_opt_in_families: &[String],
        expr_parser_force_host_families: &[String],
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

        let _ = (expr_parser_opt_in_families, expr_parser_force_host_families);

        parse_expression_with_core_parser_compatibility_for_assembler(
            tokens,
            end_span,
            end_token_text,
        )
    }

    fn resolve_expr_parser_vm_rollout_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        expr_parser_opt_in_families: &[String],
        expr_parser_force_host_families: &[String],
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
            expr_parser_opt_in_families,
            expr_parser_force_host_families,
        ))
    }

    fn compile_parsed_expression_for_assembler(
        expr: &Expr,
        opcode_version: u16,
        end_span: Span,
    ) -> Result<PortableExprProgram, ParseError> {
        compile_core_expr_to_portable_program_with_opcode_version(expr, opcode_version).map_err(
            |err| ParseError {
                message: err.to_string(),
                span: err.span.unwrap_or(end_span),
            },
        )
    }

    fn resolve_expr_opcode_version_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        end_span: Span,
    ) -> Result<u16, ParseError> {
        let contract = self
            .resolve_expr_contract(cpu_id, dialect_override)
            .map_err(|err| ParseError {
                message: err.to_string(),
                span: end_span,
            })?;
        let opcode_version = contract
            .as_ref()
            .map(|entry| entry.opcode_version)
            .unwrap_or(package::EXPR_VM_OPCODE_VERSION_V1);
        if opcode_version != package::EXPR_VM_OPCODE_VERSION_V1
            && opcode_version != package::EXPR_VM_OPCODE_VERSION_V2
        {
            return Err(ParseError {
                message: format!("unsupported EXPR opcode version {}", opcode_version),
                span: end_span,
            });
        }
        Ok(opcode_version)
    }

    pub fn compile_expression_program_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<PortableExprProgram, ParseError> {
        let expr = self.parse_expression_for_assembler_with_rollout_overrides(
            cpu_id,
            dialect_override,
            &[],
            &[],
            tokens,
            end_span,
            end_token_text,
        )?;
        let opcode_version =
            self.resolve_expr_opcode_version_for_assembler(cpu_id, dialect_override, end_span)?;
        Self::compile_parsed_expression_for_assembler(&expr, opcode_version, end_span)
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
            .unwrap_or(package::EXVM_OPCODE_VERSION))
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
        self.validate_expression_parser_contract_with_rollout_overrides_for_assembler(
            cpu_id,
            dialect_override,
            &[],
            &[],
        )
    }

    pub fn validate_expression_parser_contract_with_rollout_overrides_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        expr_parser_opt_in_families: &[String],
        expr_parser_force_host_families: &[String],
    ) -> Result<(), RuntimeBridgeError> {
        let resolved = self.resolve_pipeline(cpu_id, dialect_override)?;
        let use_expr_parser_vm = portable_expr_parser_runtime_enabled_for_family(
            resolved.family_id.as_str(),
            expr_parser_opt_in_families,
            expr_parser_force_host_families,
        );
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
        self.compile_expression_program_with_parser_vm_rollout_overrides_for_assembler(
            cpu_id,
            dialect_override,
            &[],
            &[],
            tokens,
            end_span,
            end_token_text,
            parser_vm_opcode_version,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub fn compile_expression_program_with_parser_vm_rollout_overrides_for_assembler(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
        expr_parser_opt_in_families: &[String],
        expr_parser_force_host_families: &[String],
        tokens: Vec<Token>,
        end_span: Span,
        end_token_text: Option<String>,
        parser_vm_opcode_version: Option<u16>,
    ) -> Result<PortableExprProgram, ParseError> {
        let use_expr_parser_vm = self.resolve_expr_parser_vm_rollout_for_assembler(
            cpu_id,
            dialect_override,
            expr_parser_opt_in_families,
            expr_parser_force_host_families,
            parser_vm_opcode_version.is_some(),
            end_span,
        )?;
        let expr_opcode_version =
            self.resolve_expr_opcode_version_for_assembler(cpu_id, dialect_override, end_span)?;
        if !use_expr_parser_vm {
            let expr = self.parse_expression_with_mode_for_assembler(
                cpu_id,
                dialect_override,
                expr_parser_opt_in_families,
                expr_parser_force_host_families,
                tokens,
                end_span,
                end_token_text,
                None,
            );
            return expr.and_then(|expr| {
                Self::compile_parsed_expression_for_assembler(&expr, expr_opcode_version, end_span)
            });
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
            .unwrap_or(package::EXVM_OPCODE_VERSION);
        if opcode_version != package::EXVM_OPCODE_VERSION {
            return Err(ParseError {
                message: format!("unsupported EXVM opcode version {}", opcode_version),
                span: end_span,
            });
        }

        if opcode_version == package::EXVM_OPCODE_VERSION
            && expr_opcode_version == package::EXPR_VM_OPCODE_VERSION_V2
            && !tokens_require_ast_portable_program_fallback(&tokens)
        {
            return compile_expression_tokens_to_portable_program_with_opcode_versions(
                tokens,
                end_span,
                end_token_text,
                opcode_version,
                expr_opcode_version,
            );
        }

        if let Some(program) = try_compile_direct_leaf_expression_program_for_assembler(
            &tokens,
            expr_opcode_version,
            opcode_version,
            end_span,
        )? {
            return Ok(program);
        }

        let expr = self.parse_expression_with_mode_for_assembler(
            cpu_id,
            dialect_override,
            expr_parser_opt_in_families,
            expr_parser_force_host_families,
            tokens,
            end_span,
            end_token_text,
            Some(opcode_version),
        )?;
        if let Some(program) = try_compile_direct_scalar_expression_program_for_assembler(
            &expr,
            expr_opcode_version,
            opcode_version,
            end_span,
        )? {
            return Ok(program);
        }
        if let Some(program) = try_compile_direct_structural_expression_program_for_assembler(
            &expr,
            expr_opcode_version,
            opcode_version,
            end_span,
        )? {
            return Ok(program);
        }
        if let Some(program) = try_compile_direct_member_index_expression_program_for_assembler(
            &expr,
            expr_opcode_version,
            opcode_version,
            end_span,
        )? {
            return Ok(program);
        }
        Self::compile_parsed_expression_for_assembler(&expr, expr_opcode_version, end_span)
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
