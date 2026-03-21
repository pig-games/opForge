// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared VM runtime contract value types used by bridge adapters.

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RuntimeExprParserDiagnosticMap {
    pub invalid_expression_program: String,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RuntimeExprDiagnosticMap {
    pub invalid_opcode: String,
    pub stack_underflow: String,
    pub stack_depth_exceeded: String,
    pub unknown_symbol: String,
    pub eval_failure: String,
    pub unsupported_feature: String,
    pub budget_exceeded: String,
    pub invalid_program: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeExprContract {
    pub opcode_version: u16,
    pub max_program_bytes: u32,
    pub max_stack_depth: u32,
    pub max_symbol_refs: u32,
    pub max_eval_steps: u32,
    pub diagnostics: RuntimeExprDiagnosticMap,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeExprParserContract {
    pub opcode_version: u16,
    pub diagnostics: RuntimeExprParserDiagnosticMap,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuntimeParserCertificationChecklists {
    pub expression_parser_checklist: Option<&'static str>,
    pub instruction_parse_encode_checklist: Option<&'static str>,
}
