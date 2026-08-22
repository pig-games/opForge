// SPDX-License-Identifier: GPL-3.0-or-later

use package::{
    default_token_policy_lexical_defaults, TokenCaseRule, TokenizerVmDiagnosticMap,
    TokenizerVmLimits, TokenizerVmStreamDescriptor,
};

use crate::fixup_vm::PortableOutputFixup;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct VmInstructionEffects {
    pub relocation_free: bool,
    pub output_fixups: Vec<PortableOutputFixup>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuntimeTokenizerMode {
    Auto,
    Vm,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuntimeBudgetProfile {
    HostDefault,
    RetroConstrained,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuntimeBudgetLimits {
    pub max_candidate_count: usize,
    pub max_operand_count_per_candidate: usize,
    pub max_operand_bytes_per_operand: usize,
    pub max_vm_program_bytes: usize,
    pub max_selectors_scanned_per_instruction: usize,
    pub max_parser_tokens_per_line: usize,
    pub max_parser_ast_nodes_per_line: usize,
    pub max_parser_vm_program_bytes: usize,
    pub max_tokenizer_steps_per_line: u32,
    pub max_tokenizer_tokens_per_line: u32,
    pub max_tokenizer_lexeme_bytes: u32,
    pub max_tokenizer_errors_per_line: u32,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RuntimeParserDiagnosticMap {
    pub unexpected_token: String,
    pub expected_expression: String,
    pub expected_operand: String,
    pub invalid_statement: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeParserContract {
    pub grammar_id: String,
    pub ast_schema_id: String,
    pub opcode_version: u16,
    pub max_ast_nodes_per_line: u32,
    pub diagnostics: RuntimeParserDiagnosticMap,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeParserVmProgram {
    pub opcode_version: u16,
    pub program: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeTokenizerVmProgram {
    pub opcode_version: u16,
    pub start_state: u16,
    pub state_entry_offsets: Vec<u32>,
    pub stream: TokenizerVmStreamDescriptor,
    pub limits: TokenizerVmLimits,
    pub diagnostics: TokenizerVmDiagnosticMap,
    pub program: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeTokenPolicy {
    pub case_rule: TokenCaseRule,
    pub identifier_start_class: u32,
    pub identifier_continue_class: u32,
    pub punctuation_chars: String,
    pub comment_prefix: String,
    pub quote_chars: String,
    pub escape_char: Option<char>,
    pub number_prefix_chars: String,
    pub number_suffix_binary: String,
    pub number_suffix_octal: String,
    pub number_suffix_decimal: String,
    pub number_suffix_hex: String,
    pub operator_chars: String,
    pub multi_char_operators: Vec<String>,
}

impl Default for RuntimeTokenPolicy {
    fn default() -> Self {
        let defaults = default_token_policy_lexical_defaults();
        Self {
            case_rule: TokenCaseRule::Preserve,
            identifier_start_class: 0,
            identifier_continue_class: 0,
            punctuation_chars: String::new(),
            comment_prefix: defaults.comment_prefix,
            quote_chars: defaults.quote_chars,
            escape_char: defaults.escape_char,
            number_prefix_chars: defaults.number_prefix_chars,
            number_suffix_binary: defaults.number_suffix_binary,
            number_suffix_octal: defaults.number_suffix_octal,
            number_suffix_decimal: defaults.number_suffix_decimal,
            number_suffix_hex: defaults.number_suffix_hex,
            operator_chars: defaults.operator_chars,
            multi_char_operators: defaults.multi_char_operators,
        }
    }
}

impl RuntimeBudgetProfile {
    #[must_use]
    pub fn limits(self) -> RuntimeBudgetLimits {
        match self {
            Self::HostDefault => RuntimeBudgetLimits {
                max_candidate_count: 64,
                max_operand_count_per_candidate: 8,
                max_operand_bytes_per_operand: 32,
                max_vm_program_bytes: 128,
                max_selectors_scanned_per_instruction: 512,
                max_parser_tokens_per_line: 512,
                max_parser_ast_nodes_per_line: 1024,
                max_parser_vm_program_bytes: 256,
                max_tokenizer_steps_per_line: 4096,
                max_tokenizer_tokens_per_line: 256,
                max_tokenizer_lexeme_bytes: 1024,
                max_tokenizer_errors_per_line: 16,
            },
            Self::RetroConstrained => RuntimeBudgetLimits {
                max_candidate_count: 16,
                max_operand_count_per_candidate: 4,
                max_operand_bytes_per_operand: 32,
                max_vm_program_bytes: 48,
                max_selectors_scanned_per_instruction: 128,
                max_parser_tokens_per_line: 128,
                max_parser_ast_nodes_per_line: 128,
                max_parser_vm_program_bytes: 96,
                max_tokenizer_steps_per_line: 512,
                max_tokenizer_tokens_per_line: 64,
                max_tokenizer_lexeme_bytes: 32,
                max_tokenizer_errors_per_line: 4,
            },
        }
    }
}
