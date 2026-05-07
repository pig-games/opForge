// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Binary CPU package (`*.opasm`) container support for hierarchy chunks.
//!
//! This module currently implements read/write for:
//! - `META` (package metadata)
//! - `STRS` (string pool)
//! - `DIAG` (diagnostic catalog)
//! - `TOKS` (token policy hints)
//! - `FAMS` (family descriptors)
//! - `CPUS` (cpu descriptors)
//! - `DIAL` (dialect descriptors)
//! - `REGS` (scoped register descriptors)
//! - `FORM` (scoped form descriptors)
//! - `TABL` (scoped VM instruction program descriptors)
//! - `TKVM` (scoped tokenizer VM program descriptors)
//! - `PARS` (scoped parser/AST contract descriptors)
//! - `PRVM` (scoped parser VM program descriptors)
//! - `EXPR` (scoped expression VM contract descriptors)

use std::collections::HashMap;

use types::hierarchy::{
    CpuDescriptor, DialectDescriptor, FamilyDescriptor, HierarchyError, HierarchyPackage,
    ScopedFormDescriptor, ScopedOwner, ScopedRegisterDescriptor,
};

mod canonicalize;
#[cfg(test)]
mod tests;

use canonicalize::canonicalize_package_support_chunks;
pub use canonicalize::{
    canonicalize_expr_contracts, canonicalize_expr_parser_contracts,
    canonicalize_hierarchy_metadata, canonicalize_parser_contracts,
    canonicalize_parser_vm_programs, canonicalize_token_policies,
    canonicalize_tokenizer_vm_programs,
};

pub const OPASM_MAGIC: [u8; 4] = *b"OPCP";
pub const OPASM_VERSION_V1: u16 = 0x0001;
pub const OPASM_ENDIAN_MARKER: u16 = 0x1234;

const HEADER_SIZE: usize = 12;
const TOC_ENTRY_SIZE: usize = 12;
const MAX_DECODE_ENTRY_COUNT: usize = 100_000;

const CHUNK_META: [u8; 4] = *b"META";
const CHUNK_STRS: [u8; 4] = *b"STRS";
const CHUNK_DIAG: [u8; 4] = *b"DIAG";
const CHUNK_TOKS: [u8; 4] = *b"TOKS";
const CHUNK_FAMS: [u8; 4] = *b"FAMS";
const CHUNK_CPUS: [u8; 4] = *b"CPUS";
const CHUNK_DIAL: [u8; 4] = *b"DIAL";
const CHUNK_REGS: [u8; 4] = *b"REGS";
const CHUNK_FORM: [u8; 4] = *b"FORM";
const CHUNK_TABL: [u8; 4] = *b"TABL";
const CHUNK_MSEL: [u8; 4] = *b"MSEL";
const CHUNK_TKVM: [u8; 4] = *b"TKVM";
const CHUNK_PARS: [u8; 4] = *b"PARS";
const CHUNK_PRVM: [u8; 4] = *b"PRVM";
const CHUNK_EXPR: [u8; 4] = *b"EXPR";
const CHUNK_EXVM: [u8; 4] = *b"EXVM";

pub const DIAG_OPTHREAD_MISSING_VM_PROGRAM: &str = "OTR001";
pub const DIAG_OPTHREAD_INVALID_FORCE_OVERRIDE: &str = "OTR002";
pub const DIAG_OPTHREAD_FORCE_UNSUPPORTED_65C02: &str = "OTR003";
pub const DIAG_OPTHREAD_FORCE_UNSUPPORTED_6502: &str = "OTR004";
pub const DIAG_TOKENIZER_INVALID_CHAR: &str = "ott001";
pub const DIAG_TOKENIZER_UNTERMINATED_STRING: &str = "ott002";
pub const DIAG_TOKENIZER_STEP_LIMIT_EXCEEDED: &str = "ott003";
pub const DIAG_TOKENIZER_TOKEN_LIMIT_EXCEEDED: &str = "ott004";
pub const DIAG_TOKENIZER_LEXEME_LIMIT_EXCEEDED: &str = "ott005";
pub const DIAG_TOKENIZER_ERROR_LIMIT_EXCEEDED: &str = "ott006";
pub const DIAG_PARSER_UNEXPECTED_TOKEN: &str = "otp001";
pub const DIAG_PARSER_EXPECTED_EXPRESSION: &str = "otp002";
pub const DIAG_PARSER_EXPECTED_OPERAND: &str = "otp003";
pub const DIAG_PARSER_INVALID_STATEMENT: &str = "otp004";
pub const DIAG_PARSER_OPASM_V2_ENTRY_BOUNDARY_VIOLATION: &str =
    "parser.opasm_v2.entry_boundary_violation";
pub const DIAG_PARSER_OPASM_V2_FORBIDDEN_CROSS_CONTRACT_OPCODE: &str =
    "parser.opasm_v2.forbidden_cross_contract_opcode";
pub const DIAG_PARSER_OPASM_V2_UNKNOWN_SUBCALL_CONTRACT: &str =
    "parser.opasm_v2.unknown_subcall_contract";
pub const DIAG_PARSER_OPASM_V2_SUBCALL_VERSION_MISMATCH: &str =
    "parser.opasm_v2.subcall_version_mismatch";
pub const DIAG_PARSER_OPASM_V2_MISROUTED_OPCORE_DIRECTIVE: &str =
    "parser.opasm_v2.misrouted_opcore_directive";
pub const DIAG_PARSER_OPASM_V2_CHECKPOINT_DEPTH_EXCEEDED: &str =
    "parser.opasm_v2.checkpoint_depth_exceeded";
pub const DIAG_ASM_GENERIC_ERRORS_DETECTED: &str = "asm001";
pub const DIAG_ASM_PREPROCESS_ERROR: &str = "asm102";
pub const DIAG_ASM_CLI_ERROR: &str = "asm101";
pub const DIAG_ASM_CONDITIONAL_STRUCTURE: &str = "asm201";
pub const DIAG_ASM_DIRECTIVE_STRUCTURE: &str = "asm202";
pub const DIAG_ASM_SYMBOL_ERROR: &str = "asm301";
pub const DIAG_ASM_EXPRESSION_ERROR: &str = "asm401";
pub const DIAG_ASM_INSTRUCTION_ERROR: &str = "asm402";
pub const DIAG_ASM_IO_ERROR: &str = "asm501";

/// VM opcode-version compatibility matrix for package-scoped contracts/programs.
///
/// - `TOKENIZER_VM_OPCODE_VERSION_V1`: tokenizer VM (`TKVM`) payloads.
/// - `PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT`: `.opasm` statement PRVM v2 payloads.
/// - `EXVM_OPCODE_VERSION_V1`: expression parser VM (`EXVM`) payloads.
/// - `EXVM_OPCODE_VERSION_V2`: staged expression parser VM v2 contract payloads.
/// - `EXPR_VM_OPCODE_VERSION_V1`: expression evaluator VM contracts (`EXPR`),
///   sourced from `core::expr_vm` to keep runtime/package compatibility strict.
/// - `EXPR_VM_OPCODE_VERSION_V2`: staged expression evaluator VM v2 contract
///   payloads.
///
/// Decode/validation policy for all versioned VM payloads:
/// - exact version match required for the active decoder.
/// - unknown versions must produce deterministic errors.
pub const TOKENIZER_VM_OPCODE_VERSION_V1: u16 = 0x0001;
pub const PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT: u16 = 0x0002;
pub const EXVM_OPCODE_VERSION_V1: u16 = 0x0001;
pub const EXVM_OPCODE_VERSION_V2: u16 = 0x0002;
pub const PARSER_GRAMMAR_ID_LINE_V1: &str = "opforge.line.v1";
pub const PARSER_AST_SCHEMA_ID_LINE_V1: &str = "opforge.ast.line.v1";
pub const EXPR_VM_OPCODE_VERSION_V1: u16 = super::expr_vm_compat::EXPR_VM_OPCODE_VERSION_V1;
pub const EXPR_VM_OPCODE_VERSION_V2: u16 = super::expr_vm_compat::EXPR_VM_OPCODE_VERSION_V2;
pub const DIAG_EXPR_INVALID_OPCODE: &str = super::expr_vm_compat::DIAG_EXPR_INVALID_OPCODE;
pub const DIAG_EXPR_STACK_UNDERFLOW: &str = super::expr_vm_compat::DIAG_EXPR_STACK_UNDERFLOW;
pub const DIAG_EXPR_STACK_DEPTH_EXCEEDED: &str =
    super::expr_vm_compat::DIAG_EXPR_STACK_DEPTH_EXCEEDED;
pub const DIAG_EXPR_UNKNOWN_SYMBOL: &str = super::expr_vm_compat::DIAG_EXPR_UNKNOWN_SYMBOL;
pub const DIAG_EXPR_EVAL_FAILURE: &str = super::expr_vm_compat::DIAG_EXPR_EVAL_FAILURE;
pub const DIAG_EXPR_UNSUPPORTED_FEATURE: &str =
    super::expr_vm_compat::DIAG_EXPR_UNSUPPORTED_FEATURE;
pub const DIAG_EXPR_BUDGET_EXCEEDED: &str = super::expr_vm_compat::DIAG_EXPR_BUDGET_EXCEEDED;
pub const DIAG_EXPR_INVALID_PROGRAM: &str = super::expr_vm_compat::DIAG_EXPR_INVALID_PROGRAM;

/// Package metadata descriptor (`META` chunk).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PackageMetaDescriptor {
    pub package_id: String,
    pub package_version: String,
    pub capability_flags: u32,
}

impl Default for PackageMetaDescriptor {
    fn default() -> Self {
        Self {
            package_id: "opforge.generated".to_string(),
            package_version: "0.1.0".to_string(),
            capability_flags: 0,
        }
    }
}

/// Diagnostic descriptor (`DIAG` chunk).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DiagnosticDescriptor {
    pub code: String,
    pub message_template: String,
}

/// Scoped VM program descriptor for one mnemonic + mode-key encode template.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VmProgramDescriptor {
    pub owner: ScopedOwner,
    pub mnemonic: String,
    pub mode_key: String,
    pub program: Vec<u8>,
}

/// Scoped mode selector descriptor for Expr/family-operand to VM mode candidate mapping.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModeSelectorDescriptor {
    pub owner: ScopedOwner,
    pub mnemonic: String,
    pub shape_key: String,
    pub mode_key: String,
    pub operand_plan: String,
    pub priority: u16,
    pub unstable_widen: bool,
    pub width_rank: u8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenizerVmLimits {
    pub max_steps_per_line: u32,
    pub max_tokens_per_line: u32,
    pub max_lexeme_bytes: u32,
    pub max_errors_per_line: u32,
}

impl Default for TokenizerVmLimits {
    fn default() -> Self {
        Self {
            max_steps_per_line: 2048,
            max_tokens_per_line: 256,
            max_lexeme_bytes: 1024,
            max_errors_per_line: 16,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TokenizerVmDiagnosticMap {
    pub invalid_char: String,
    pub unterminated_string: String,
    pub step_limit_exceeded: String,
    pub token_limit_exceeded: String,
    pub lexeme_limit_exceeded: String,
    pub error_limit_exceeded: String,
}

pub const TOKENIZER_VM_STREAM_VERSION_V1: u16 = 1;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenizerVmStreamMode {
    LineInputBytes = 0x01,
}

impl TokenizerVmStreamMode {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x01 => Some(Self::LineInputBytes),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenizerVmStreamDescriptor {
    pub version: u16,
    pub mode: TokenizerVmStreamMode,
}

impl Default for TokenizerVmStreamDescriptor {
    fn default() -> Self {
        Self {
            version: TOKENIZER_VM_STREAM_VERSION_V1,
            mode: TokenizerVmStreamMode::LineInputBytes,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenizerVmOpcode {
    End = 0x00,
    ReadChar = 0x01,
    Advance = 0x02,
    StartLexeme = 0x03,
    PushChar = 0x04,
    EmitToken = 0x05,
    SetState = 0x06,
    Jump = 0x07,
    JumpIfEol = 0x08,
    JumpIfByteEq = 0x09,
    JumpIfClass = 0x0A,
    Fail = 0x0B,
    EmitDiag = 0x0C,
    DelegateCore = 0x0D,
    ScanCoreToken = 0x0E,
    ScanIdentifier = 0x0F,
    ScanNumber = 0x10,
    ScanString = 0x11,
    ScanSymbol = 0x12,
}

impl TokenizerVmOpcode {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x00 => Some(Self::End),
            0x01 => Some(Self::ReadChar),
            0x02 => Some(Self::Advance),
            0x03 => Some(Self::StartLexeme),
            0x04 => Some(Self::PushChar),
            0x05 => Some(Self::EmitToken),
            0x06 => Some(Self::SetState),
            0x07 => Some(Self::Jump),
            0x08 => Some(Self::JumpIfEol),
            0x09 => Some(Self::JumpIfByteEq),
            0x0A => Some(Self::JumpIfClass),
            0x0B => Some(Self::Fail),
            0x0C => Some(Self::EmitDiag),
            0x0D => Some(Self::DelegateCore),
            0x0E => Some(Self::ScanCoreToken),
            0x0F => Some(Self::ScanIdentifier),
            0x10 => Some(Self::ScanNumber),
            0x11 => Some(Self::ScanString),
            0x12 => Some(Self::ScanSymbol),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenizerVmProgramDescriptor {
    pub owner: ScopedOwner,
    pub opcode_version: u16,
    pub start_state: u16,
    pub state_entry_offsets: Vec<u32>,
    pub stream: TokenizerVmStreamDescriptor,
    pub limits: TokenizerVmLimits,
    pub diagnostics: TokenizerVmDiagnosticMap,
    pub program: Vec<u8>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ParserDiagnosticMap {
    pub unexpected_token: String,
    pub expected_expression: String,
    pub expected_operand: String,
    pub invalid_statement: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParserContractDescriptor {
    pub owner: ScopedOwner,
    pub grammar_id: String,
    pub ast_schema_id: String,
    pub opcode_version: u16,
    pub max_ast_nodes_per_line: u32,
    pub diagnostics: ParserDiagnosticMap,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ParserVmOpcode {
    End = 0x00,
    Jump = 0x01,
    JumpIfTrue = 0x02,
    JumpIfFalse = 0x03,
    Checkpoint = 0x04,
    Rollback = 0x05,
    Commit = 0x06,
    PeekKind = 0x10,
    PeekIdentifier = 0x11,
    PeekOperator = 0x12,
    IsEol = 0x13,
    PeekAssignmentOperator = 0x14,
    PeekStarOrg = 0x15,
    Advance = 0x20,
    ConsumeKind = 0x21,
    ConsumeOperator = 0x22,
    LoadIdentifier = 0x30,
    LoadSpan = 0x31,
    LoadTokenText = 0x32,
    LoadInlineText = 0x33,
    ParseOptionalLeadingLabel = 0x40,
    ScanTopLevelCommaBoundaries = 0x41,
    RequireNoTrailingTokens = 0x42,
    ParseOperandExprRange = 0x50,
    BeginStatement = 0x60,
    SetLabel = 0x61,
    SetMnemonic = 0x62,
    PushOperand = 0x63,
    FinishLine = 0x64,
    SetDotMnemonic = 0x65,
    FinishAssignment = 0x66,
    EmitDiag = 0x70,
    EmitDiagIfNoResult = 0x71,
    Fail = 0x72,
}

impl ParserVmOpcode {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x00 => Some(Self::End),
            0x01 => Some(Self::Jump),
            0x02 => Some(Self::JumpIfTrue),
            0x03 => Some(Self::JumpIfFalse),
            0x04 => Some(Self::Checkpoint),
            0x05 => Some(Self::Rollback),
            0x06 => Some(Self::Commit),
            0x10 => Some(Self::PeekKind),
            0x11 => Some(Self::PeekIdentifier),
            0x12 => Some(Self::PeekOperator),
            0x13 => Some(Self::IsEol),
            0x14 => Some(Self::PeekAssignmentOperator),
            0x15 => Some(Self::PeekStarOrg),
            0x20 => Some(Self::Advance),
            0x21 => Some(Self::ConsumeKind),
            0x22 => Some(Self::ConsumeOperator),
            0x30 => Some(Self::LoadIdentifier),
            0x31 => Some(Self::LoadSpan),
            0x32 => Some(Self::LoadTokenText),
            0x33 => Some(Self::LoadInlineText),
            0x40 => Some(Self::ParseOptionalLeadingLabel),
            0x41 => Some(Self::ScanTopLevelCommaBoundaries),
            0x42 => Some(Self::RequireNoTrailingTokens),
            0x50 => Some(Self::ParseOperandExprRange),
            0x60 => Some(Self::BeginStatement),
            0x61 => Some(Self::SetLabel),
            0x62 => Some(Self::SetMnemonic),
            0x63 => Some(Self::PushOperand),
            0x64 => Some(Self::FinishLine),
            0x65 => Some(Self::SetDotMnemonic),
            0x66 => Some(Self::FinishAssignment),
            0x70 => Some(Self::EmitDiag),
            0x71 => Some(Self::EmitDiagIfNoResult),
            0x72 => Some(Self::Fail),
            _ => None,
        }
    }
}

pub type ParserVmOpcodeV2 = ParserVmOpcode;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ExvmOpcode {
    End = 0x00,
    ParseExpression = 0x01,
    EmitDiag = 0x02,
    Fail = 0x03,
}

pub type ExvmOpcodeV1 = ExvmOpcode;

impl ExvmOpcode {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x00 => Some(Self::End),
            0x01 => Some(Self::ParseExpression),
            0x02 => Some(Self::EmitDiag),
            0x03 => Some(Self::Fail),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ExvmOpcodeV2 {
    End = 0x00,
    PeekKind = 0x10,
    Advance = 0x20,
    LoadTokenText = 0x32,
    BuildIdentifier = 0x60,
    EmitDiag = 0x70,
    Fail = 0x72,
}

impl ExvmOpcodeV2 {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x00 => Some(Self::End),
            0x10 => Some(Self::PeekKind),
            0x20 => Some(Self::Advance),
            0x32 => Some(Self::LoadTokenText),
            0x60 => Some(Self::BuildIdentifier),
            0x70 => Some(Self::EmitDiag),
            0x72 => Some(Self::Fail),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParserVmProgramDescriptor {
    pub owner: ScopedOwner,
    pub opcode_version: u16,
    pub program: Vec<u8>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ExprDiagnosticMap {
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
pub struct ExprContractDescriptor {
    pub owner: ScopedOwner,
    pub opcode_version: u16,
    pub max_program_bytes: u32,
    pub max_stack_depth: u32,
    pub max_symbol_refs: u32,
    pub max_eval_steps: u32,
    pub diagnostics: ExprDiagnosticMap,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ExprParserDiagnosticMap {
    pub invalid_expression_program: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExprParserContractDescriptor {
    pub owner: ScopedOwner,
    pub opcode_version: u16,
    pub diagnostics: ExprParserDiagnosticMap,
}

/// Case-folding behavior for tokenizer/literal matching policy.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenCaseRule {
    Preserve = 0,
    AsciiLower = 1,
    AsciiUpper = 2,
}

impl TokenCaseRule {
    fn from_u8(value: u8, chunk: &str) -> Result<Self, OpcpuCodecError> {
        match value {
            0 => Ok(Self::Preserve),
            1 => Ok(Self::AsciiLower),
            2 => Ok(Self::AsciiUpper),
            other => Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: chunk.to_string(),
                detail: format!("invalid token case rule: {}", other),
            }),
        }
    }
}

/// Bit flags describing allowed identifier characters for tokenizer policy hints.
pub mod token_identifier_class {
    pub const ASCII_ALPHA: u32 = 1 << 0;
    pub const ASCII_DIGIT: u32 = 1 << 1;
    pub const UNDERSCORE: u32 = 1 << 2;
    pub const DOLLAR: u32 = 1 << 3;
    pub const AT_SIGN: u32 = 1 << 4;
    pub const DOT: u32 = 1 << 5;
}

const TOKS_EXT_MARKER: u8 = 0xFF;
const TOKS_DEFAULT_COMMENT_PREFIX: &str = ";";
const TOKS_DEFAULT_QUOTE_CHARS: &str = "\"'";
const TOKS_DEFAULT_NUMBER_PREFIX_CHARS: &str = "$%@";
const TOKS_DEFAULT_NUMBER_SUFFIX_BINARY: &str = "bB";
const TOKS_DEFAULT_NUMBER_SUFFIX_OCTAL: &str = "oOqQ";
const TOKS_DEFAULT_NUMBER_SUFFIX_DECIMAL: &str = "dD";
const TOKS_DEFAULT_NUMBER_SUFFIX_HEX: &str = "hH";
const TOKS_DEFAULT_OPERATOR_CHARS: &str = "+-*/%~!&|^<>=?";
const TOKS_DEFAULT_MULTI_CHAR_OPERATORS: [&str; 11] = [
    "**", "==", "!=", "&&", "||", "^^", "<<", ">>", "<=", ">=", "<>",
];

/// Token policy descriptor (`TOKS` chunk), scoped by family/cpu/dialect owner.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenPolicyDescriptor {
    pub owner: ScopedOwner,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenPolicyLexicalDefaults {
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

pub fn default_token_policy_lexical_defaults() -> TokenPolicyLexicalDefaults {
    TokenPolicyLexicalDefaults {
        comment_prefix: TOKS_DEFAULT_COMMENT_PREFIX.to_string(),
        quote_chars: TOKS_DEFAULT_QUOTE_CHARS.to_string(),
        escape_char: Some('\\'),
        number_prefix_chars: TOKS_DEFAULT_NUMBER_PREFIX_CHARS.to_string(),
        number_suffix_binary: TOKS_DEFAULT_NUMBER_SUFFIX_BINARY.to_string(),
        number_suffix_octal: TOKS_DEFAULT_NUMBER_SUFFIX_OCTAL.to_string(),
        number_suffix_decimal: TOKS_DEFAULT_NUMBER_SUFFIX_DECIMAL.to_string(),
        number_suffix_hex: TOKS_DEFAULT_NUMBER_SUFFIX_HEX.to_string(),
        operator_chars: TOKS_DEFAULT_OPERATOR_CHARS.to_string(),
        multi_char_operators: TOKS_DEFAULT_MULTI_CHAR_OPERATORS
            .iter()
            .map(|value| value.to_string())
            .collect(),
    }
}

/// Decoded hierarchy-chunk payload set.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HierarchyChunks {
    pub metadata: PackageMetaDescriptor,
    pub strings: Vec<String>,
    pub diagnostics: Vec<DiagnosticDescriptor>,
    pub token_policies: Vec<TokenPolicyDescriptor>,
    pub tokenizer_vm_programs: Vec<TokenizerVmProgramDescriptor>,
    pub parser_contracts: Vec<ParserContractDescriptor>,
    pub parser_vm_programs: Vec<ParserVmProgramDescriptor>,
    pub expr_contracts: Vec<ExprContractDescriptor>,
    pub expr_parser_contracts: Vec<ExprParserContractDescriptor>,
    pub families: Vec<FamilyDescriptor>,
    pub cpus: Vec<CpuDescriptor>,
    pub dialects: Vec<DialectDescriptor>,
    pub registers: Vec<ScopedRegisterDescriptor>,
    pub forms: Vec<ScopedFormDescriptor>,
    pub tables: Vec<VmProgramDescriptor>,
    pub selectors: Vec<ModeSelectorDescriptor>,
}

/// Deterministic package codec errors for malformed container/schema data.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OpcpuCodecError {
    InvalidMagic {
        found: [u8; 4],
    },
    UnsupportedVersion {
        found: u16,
    },
    InvalidEndiannessMarker {
        found: u16,
    },
    UnexpectedEof {
        context: String,
    },
    DuplicateChunk {
        chunk: String,
    },
    MissingRequiredChunk {
        chunk: String,
    },
    ChunkOutOfBounds {
        chunk: String,
        offset: u32,
        length: u32,
        file_len: usize,
    },
    CountOutOfRange {
        context: String,
    },
    InvalidChunkFormat {
        chunk: String,
        detail: String,
    },
    InvalidUtf8 {
        chunk: String,
    },
    Hierarchy(HierarchyError),
}

impl OpcpuCodecError {
    pub fn code(&self) -> &'static str {
        match self {
            Self::InvalidMagic { .. } => "OPC001",
            Self::UnsupportedVersion { .. } => "OPC002",
            Self::InvalidEndiannessMarker { .. } => "OPC003",
            Self::UnexpectedEof { .. } => "OPC004",
            Self::DuplicateChunk { .. } => "OPC005",
            Self::MissingRequiredChunk { .. } => "OPC006",
            Self::ChunkOutOfBounds { .. } => "OPC007",
            Self::CountOutOfRange { .. } => "OPC008",
            Self::InvalidChunkFormat { .. } => "OPC009",
            Self::InvalidUtf8 { .. } => "OPC010",
            Self::Hierarchy(_) => "OPC011",
        }
    }
}

impl std::fmt::Display for OpcpuCodecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidMagic { found } => write!(
                f,
                "[{}] invalid package magic: found {:?}",
                self.code(),
                found
            ),
            Self::UnsupportedVersion { found } => write!(
                f,
                "[{}] unsupported package version: {}",
                self.code(),
                found
            ),
            Self::InvalidEndiannessMarker { found } => write!(
                f,
                "[{}] invalid endianness marker: 0x{:04X}",
                self.code(),
                found
            ),
            Self::UnexpectedEof { context } => {
                write!(f, "[{}] unexpected end of file: {}", self.code(), context)
            }
            Self::DuplicateChunk { chunk } => {
                write!(f, "[{}] duplicate chunk '{}'", self.code(), chunk)
            }
            Self::MissingRequiredChunk { chunk } => {
                write!(f, "[{}] missing required chunk '{}'", self.code(), chunk)
            }
            Self::ChunkOutOfBounds {
                chunk,
                offset,
                length,
                file_len,
            } => write!(
                f,
                "[{}] chunk '{}' out of bounds (offset={}, length={}, file_len={})",
                self.code(),
                chunk,
                offset,
                length,
                file_len
            ),
            Self::CountOutOfRange { context } => {
                write!(f, "[{}] count out of range: {}", self.code(), context)
            }
            Self::InvalidChunkFormat { chunk, detail } => write!(
                f,
                "[{}] invalid chunk '{}' format: {}",
                self.code(),
                chunk,
                detail
            ),
            Self::InvalidUtf8 { chunk } => {
                write!(f, "[{}] invalid UTF-8 in chunk '{}'", self.code(), chunk)
            }
            Self::Hierarchy(err) => {
                write!(f, "[{}] hierarchy validation error: {}", self.code(), err)
            }
        }
    }
}

impl std::error::Error for OpcpuCodecError {}

impl From<HierarchyError> for OpcpuCodecError {
    fn from(value: HierarchyError) -> Self {
        Self::Hierarchy(value)
    }
}

mod codec;

#[cfg(test)]
#[allow(unused_imports)]
use codec::*;

pub fn encode_hierarchy_chunks(
    families: &[FamilyDescriptor],
    cpus: &[CpuDescriptor],
    dialects: &[DialectDescriptor],
    registers: &[ScopedRegisterDescriptor],
    forms: &[ScopedFormDescriptor],
    tables: &[VmProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    codec::encode_hierarchy_chunks(families, cpus, dialects, registers, forms, tables)
}

pub fn encode_hierarchy_chunks_full(
    families: &[FamilyDescriptor],
    cpus: &[CpuDescriptor],
    dialects: &[DialectDescriptor],
    registers: &[ScopedRegisterDescriptor],
    forms: &[ScopedFormDescriptor],
    tables: &[VmProgramDescriptor],
    selectors: &[ModeSelectorDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    codec::encode_hierarchy_chunks_full(
        families, cpus, dialects, registers, forms, tables, selectors,
    )
}

pub fn encode_hierarchy_chunks_from_chunks(
    chunks: &HierarchyChunks,
) -> Result<Vec<u8>, OpcpuCodecError> {
    codec::encode_hierarchy_chunks_from_chunks(chunks)
}

pub fn default_runtime_diagnostic_catalog() -> Vec<DiagnosticDescriptor> {
    codec::default_runtime_diagnostic_catalog()
}

pub fn decode_hierarchy_chunks(bytes: &[u8]) -> Result<HierarchyChunks, OpcpuCodecError> {
    codec::decode_hierarchy_chunks(bytes)
}

pub fn load_hierarchy_package(bytes: &[u8]) -> Result<HierarchyPackage, OpcpuCodecError> {
    codec::load_hierarchy_package(bytes)
}
