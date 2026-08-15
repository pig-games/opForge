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
//! - `SEMV` (versioned scoped semantic VM program descriptors)
//! - `VALP` (versioned scoped scalar-value program descriptors)
//! - `OPRD` (versioned scoped operand-record program descriptors)
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
    canonicalize_hierarchy_metadata, canonicalize_operand_record_programs,
    canonicalize_parser_contracts, canonicalize_parser_vm_programs, canonicalize_semantic_programs,
    canonicalize_token_policies, canonicalize_tokenizer_vm_programs, canonicalize_value_programs,
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
const CHUNK_SEMV: [u8; 4] = *b"SEMV";
const CHUNK_VALP: [u8; 4] = *b"VALP";
const CHUNK_OPRD: [u8; 4] = *b"OPRD";
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
/// - `VALUE_VM_OPCODE_VERSION_V1`: scalar value materialization (`VALP`)
///   payloads.
/// - `OPERAND_RECORD_VM_VERSION_V1`: neutral operand-record construction
///   (`OPRD`) payloads.
///
/// Decode/validation policy for all versioned VM payloads:
/// - exact version match required for the active decoder.
/// - unknown versions must produce deterministic errors.
pub const TOKENIZER_VM_OPCODE_VERSION_V1: u16 = 0x0001;
pub const SEMANTIC_VM_OPCODE_VERSION_V1: u16 = 0x0001;
pub const SEMANTIC_VM_OP_EMIT_U8: u8 = 0x01;
pub const SEMANTIC_VM_OP_EMIT_OPERAND: u8 = 0x02;
pub const SEMANTIC_VM_OP_END: u8 = 0xFF;
pub const VALUE_VM_OPCODE_VERSION_V1: u16 = 0x0001;
pub const VALUE_VM_OP_PUSH_LITERAL_I64: u8 = 0x01;
pub const VALUE_VM_OP_PUSH_INPUT: u8 = 0x02;
pub const VALUE_VM_OP_NORMALIZE_TWOS_COMPLEMENT: u8 = 0x03;
pub const VALUE_VM_OP_REQUIRE_SIGNED_BITS: u8 = 0x04;
pub const VALUE_VM_OP_REQUIRE_UNSIGNED_BITS: u8 = 0x05;
pub const VALUE_VM_OP_REQUIRE_RANGE_I64: u8 = 0x06;
pub const VALUE_VM_OP_END: u8 = 0xFF;
pub const OPERAND_RECORD_VM_VERSION_V1: u16 = 0x0001;
pub const OPERAND_RECORD_OP_REGISTER: u8 = 0x01;
pub const OPERAND_RECORD_OP_INDIRECT: u8 = 0x02;
pub const OPERAND_RECORD_OP_DISPLACEMENT: u8 = 0x03;
pub const OPERAND_RECORD_OP_INDEXED: u8 = 0x04;
pub const OPERAND_RECORD_OP_ABSOLUTE: u8 = 0x05;
pub const OPERAND_RECORD_OP_IMMEDIATE: u8 = 0x06;
pub const OPERAND_RECORD_OP_END: u8 = 0xFF;
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

/// Independently versioned, package-owned semantic bytecode selected by scope and id.
///
/// This descriptor is intentionally CPU-neutral. Families define the meaning and
/// contents of programs; the shared package/runtime only validate and execute the
/// portable byte-emission contract.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SemanticProgramDescriptor {
    pub owner: ScopedOwner,
    pub id: String,
    pub opcode_version: u16,
    pub program: Vec<u8>,
}

/// Independently versioned scalar materialization selected by scope and id.
///
/// Programs consume only literal values or already-evaluated scalar inputs. CPU
/// families own program construction; the package and runtime own only the
/// portable numeric operations and their deterministic bounds behavior.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValueProgramDescriptor {
    pub owner: ScopedOwner,
    pub id: String,
    pub opcode_version: u16,
    pub program: Vec<u8>,
}

/// Independently versioned constructor for one CPU-neutral operand record.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OperandRecordProgramDescriptor {
    pub owner: ScopedOwner,
    pub id: String,
    pub schema_version: u16,
    pub program: Vec<u8>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperandRecordBaseSource {
    Register(u8),
    ProgramCounter,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperandRecordUpdate {
    None,
    Postincrement,
    Predecrement,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperandRecordProgram {
    Register {
        register_input: u8,
    },
    Indirect {
        register_input: u8,
        update: OperandRecordUpdate,
    },
    Displacement {
        base: OperandRecordBaseSource,
        value_input: u8,
    },
    Indexed {
        base: OperandRecordBaseSource,
        index_register_input: u8,
        index_width_bits: u8,
        scale: u8,
        value_input: u8,
    },
    Absolute {
        value_input: u8,
        width_bits: u8,
    },
    Immediate {
        value_input: u8,
    },
}

/// Compile one neutral operand-record constructor.
pub fn compile_operand_record_program(
    record: OperandRecordProgram,
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut program = Vec::new();
    let push_base = |program: &mut Vec<u8>, base: OperandRecordBaseSource| match base {
        OperandRecordBaseSource::Register(input) => program.extend_from_slice(&[0, input]),
        OperandRecordBaseSource::ProgramCounter => program.extend_from_slice(&[1, 0]),
    };
    match record {
        OperandRecordProgram::Register { register_input } => {
            program.extend_from_slice(&[OPERAND_RECORD_OP_REGISTER, register_input]);
        }
        OperandRecordProgram::Indirect {
            register_input,
            update,
        } => {
            program.extend_from_slice(&[OPERAND_RECORD_OP_INDIRECT, register_input, update as u8]);
        }
        OperandRecordProgram::Displacement { base, value_input } => {
            program.push(OPERAND_RECORD_OP_DISPLACEMENT);
            push_base(&mut program, base);
            program.push(value_input);
        }
        OperandRecordProgram::Indexed {
            base,
            index_register_input,
            index_width_bits,
            scale,
            value_input,
        } => {
            program.push(OPERAND_RECORD_OP_INDEXED);
            push_base(&mut program, base);
            program.extend_from_slice(&[
                index_register_input,
                index_width_bits,
                scale,
                value_input,
            ]);
        }
        OperandRecordProgram::Absolute {
            value_input,
            width_bits,
        } => program.extend_from_slice(&[OPERAND_RECORD_OP_ABSOLUTE, value_input, width_bits]),
        OperandRecordProgram::Immediate { value_input } => {
            program.extend_from_slice(&[OPERAND_RECORD_OP_IMMEDIATE, value_input]);
        }
    }
    program.push(OPERAND_RECORD_OP_END);
    validate_operand_record_program(OPERAND_RECORD_VM_VERSION_V1, &program)?;
    Ok(program)
}

/// Validate one operand-record constructor without interpreting family meaning.
pub fn validate_operand_record_program(
    schema_version: u16,
    program: &[u8],
) -> Result<(), OpcpuCodecError> {
    fn invalid(detail: impl Into<String>) -> OpcpuCodecError {
        OpcpuCodecError::InvalidChunkFormat {
            chunk: "OPRD".to_string(),
            detail: detail.into(),
        }
    }
    if schema_version != OPERAND_RECORD_VM_VERSION_V1 {
        return Err(invalid(format!(
            "unsupported operand-record schema version {schema_version}"
        )));
    }
    let opcode = *program
        .first()
        .ok_or_else(|| invalid("operand-record program is empty"))?;
    let payload_len = match opcode {
        OPERAND_RECORD_OP_REGISTER | OPERAND_RECORD_OP_IMMEDIATE => 1,
        OPERAND_RECORD_OP_INDIRECT | OPERAND_RECORD_OP_ABSOLUTE => 2,
        OPERAND_RECORD_OP_DISPLACEMENT => 3,
        OPERAND_RECORD_OP_INDEXED => 6,
        other => {
            return Err(invalid(format!(
                "unknown operand-record opcode {other:#04x}"
            )))
        }
    };
    if program.len() != payload_len + 2 || program.last() != Some(&OPERAND_RECORD_OP_END) {
        return Err(invalid(
            "operand-record program has a truncated or trailing payload",
        ));
    }
    match opcode {
        OPERAND_RECORD_OP_INDIRECT if program[2] > OperandRecordUpdate::Predecrement as u8 => {
            return Err(invalid("operand-record update mode is invalid"));
        }
        OPERAND_RECORD_OP_DISPLACEMENT | OPERAND_RECORD_OP_INDEXED if program[1] > 1 => {
            return Err(invalid("operand-record base kind is invalid"));
        }
        OPERAND_RECORD_OP_DISPLACEMENT | OPERAND_RECORD_OP_INDEXED
            if program[1] == 1 && program[2] != 0 =>
        {
            return Err(invalid(
                "operand-record program-counter base has a nonzero reserved byte",
            ));
        }
        OPERAND_RECORD_OP_INDEXED => {
            if !matches!(program[4], 8 | 16 | 32 | 64) {
                return Err(invalid("operand-record index width is invalid"));
            }
            if !program[5].is_power_of_two() {
                return Err(invalid(
                    "operand-record index scale must be a nonzero power of two",
                ));
            }
        }
        OPERAND_RECORD_OP_ABSOLUTE if !matches!(program[2], 8 | 16 | 24 | 32 | 64) => {
            return Err(invalid("operand-record absolute width is invalid"));
        }
        _ => {}
    }
    Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueProgramSource {
    Literal(i64),
    Input(u8),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueConstraint {
    NormalizeTwosComplement(u8),
    SignedBits(u8),
    UnsignedBits(u8),
    InclusiveRange { min: i64, max: i64 },
}

/// Compile a CPU-neutral scalar program from one source and ordered constraints.
pub fn compile_value_program(
    source: ValueProgramSource,
    constraints: &[ValueConstraint],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut program = Vec::new();
    match source {
        ValueProgramSource::Literal(value) => {
            program.push(VALUE_VM_OP_PUSH_LITERAL_I64);
            program.extend_from_slice(&value.to_le_bytes());
        }
        ValueProgramSource::Input(index) => {
            program.push(VALUE_VM_OP_PUSH_INPUT);
            program.push(index);
        }
    }
    for constraint in constraints {
        match *constraint {
            ValueConstraint::NormalizeTwosComplement(bits) => {
                program.push(VALUE_VM_OP_NORMALIZE_TWOS_COMPLEMENT);
                program.push(bits);
            }
            ValueConstraint::SignedBits(bits) => {
                program.push(VALUE_VM_OP_REQUIRE_SIGNED_BITS);
                program.push(bits);
            }
            ValueConstraint::UnsignedBits(bits) => {
                program.push(VALUE_VM_OP_REQUIRE_UNSIGNED_BITS);
                program.push(bits);
            }
            ValueConstraint::InclusiveRange { min, max } => {
                program.push(VALUE_VM_OP_REQUIRE_RANGE_I64);
                program.extend_from_slice(&min.to_le_bytes());
                program.extend_from_slice(&max.to_le_bytes());
            }
        }
    }
    program.push(VALUE_VM_OP_END);
    validate_value_program(VALUE_VM_OPCODE_VERSION_V1, &program)?;
    Ok(program)
}

/// Validate one scalar-value program without interpreting family semantics.
pub fn validate_value_program(opcode_version: u16, program: &[u8]) -> Result<(), OpcpuCodecError> {
    fn invalid(detail: impl Into<String>) -> OpcpuCodecError {
        OpcpuCodecError::InvalidChunkFormat {
            chunk: "VALP".to_string(),
            detail: detail.into(),
        }
    }

    if opcode_version != VALUE_VM_OPCODE_VERSION_V1 {
        return Err(invalid(format!(
            "unsupported value VM opcode version {opcode_version}"
        )));
    }

    let mut pc = 0usize;
    let mut has_value = false;
    loop {
        let opcode = *program
            .get(pc)
            .ok_or_else(|| invalid("value VM program is truncated before END"))?;
        pc += 1;
        match opcode {
            VALUE_VM_OP_PUSH_LITERAL_I64 => {
                if has_value {
                    return Err(invalid("value VM program defines more than one source"));
                }
                pc = pc
                    .checked_add(8)
                    .filter(|end| *end <= program.len())
                    .ok_or_else(|| invalid("value VM literal is truncated"))?;
                has_value = true;
            }
            VALUE_VM_OP_PUSH_INPUT => {
                if has_value {
                    return Err(invalid("value VM program defines more than one source"));
                }
                pc = pc
                    .checked_add(1)
                    .filter(|end| *end <= program.len())
                    .ok_or_else(|| invalid("value VM input index is truncated"))?;
                has_value = true;
            }
            VALUE_VM_OP_NORMALIZE_TWOS_COMPLEMENT
            | VALUE_VM_OP_REQUIRE_SIGNED_BITS
            | VALUE_VM_OP_REQUIRE_UNSIGNED_BITS => {
                if !has_value {
                    return Err(invalid("value VM constraint precedes its source"));
                }
                let bits = *program
                    .get(pc)
                    .ok_or_else(|| invalid("value VM bit width is truncated"))?;
                pc += 1;
                if !(1..=64).contains(&bits) {
                    return Err(invalid(format!(
                        "value VM bit width {bits} is outside 1..=64"
                    )));
                }
            }
            VALUE_VM_OP_REQUIRE_RANGE_I64 => {
                if !has_value {
                    return Err(invalid("value VM constraint precedes its source"));
                }
                let end = pc
                    .checked_add(16)
                    .filter(|end| *end <= program.len())
                    .ok_or_else(|| invalid("value VM inclusive range is truncated"))?;
                let min = i64::from_le_bytes(program[pc..pc + 8].try_into().expect("8 bytes"));
                let max = i64::from_le_bytes(program[pc + 8..end].try_into().expect("8 bytes"));
                if min > max {
                    return Err(invalid(format!(
                        "value VM inclusive range minimum {min} exceeds maximum {max}"
                    )));
                }
                pc = end;
            }
            VALUE_VM_OP_END if !has_value => {
                return Err(invalid("value VM program ends without a source"));
            }
            VALUE_VM_OP_END if pc == program.len() => return Ok(()),
            VALUE_VM_OP_END => {
                return Err(invalid("value VM program has trailing bytes after END"));
            }
            _ => {
                return Err(invalid(format!(
                    "invalid value VM opcode 0x{opcode:02X} at pc={}",
                    pc - 1
                )));
            }
        }
    }
}

/// Build the v1 semantic bytecode for a fixed byte sequence.
pub fn compile_fixed_semantic_program(bytes: &[u8]) -> Vec<u8> {
    let mut program = Vec::with_capacity(bytes.len().saturating_mul(2).saturating_add(1));
    for byte in bytes {
        program.push(SEMANTIC_VM_OP_EMIT_U8);
        program.push(*byte);
    }
    program.push(SEMANTIC_VM_OP_END);
    program
}

/// Validate one portable semantic program without interpreting CPU semantics.
pub fn validate_semantic_program(
    opcode_version: u16,
    program: &[u8],
) -> Result<(), OpcpuCodecError> {
    if opcode_version != SEMANTIC_VM_OPCODE_VERSION_V1 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "SEMV".to_string(),
            detail: format!("unsupported semantic VM opcode version {opcode_version}"),
        });
    }

    let mut pc = 0usize;
    loop {
        let opcode = *program
            .get(pc)
            .ok_or_else(|| OpcpuCodecError::InvalidChunkFormat {
                chunk: "SEMV".to_string(),
                detail: "semantic VM program is truncated before END".to_string(),
            })?;
        pc += 1;
        match opcode {
            SEMANTIC_VM_OP_EMIT_U8 | SEMANTIC_VM_OP_EMIT_OPERAND => {
                if program.get(pc).is_none() {
                    return Err(OpcpuCodecError::InvalidChunkFormat {
                        chunk: "SEMV".to_string(),
                        detail: format!("semantic VM opcode 0x{opcode:02X} is truncated"),
                    });
                }
                pc += 1;
            }
            SEMANTIC_VM_OP_END if pc == program.len() => return Ok(()),
            SEMANTIC_VM_OP_END => {
                return Err(OpcpuCodecError::InvalidChunkFormat {
                    chunk: "SEMV".to_string(),
                    detail: "semantic VM program has trailing bytes after END".to_string(),
                });
            }
            _ => {
                return Err(OpcpuCodecError::InvalidChunkFormat {
                    chunk: "SEMV".to_string(),
                    detail: format!("invalid semantic VM opcode 0x{opcode:02X} at pc={}", pc - 1),
                });
            }
        }
    }
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
    Jump = 0x01,
    JumpIfTrue = 0x02,
    Call = 0x03,
    Return = 0x04,
    PeekKind = 0x10,
    PeekOperator = 0x11,
    Advance = 0x20,
    ConsumeOperator = 0x21,
    ConsumeKind = 0x22,
    LoadTokenText = 0x32,
    BuildUnary = 0x40,
    BuildBinary = 0x41,
    BuildTernary = 0x42,
    BuildRange = 0x43,
    BuildIdentifier = 0x60,
    BuildNumber = 0x61,
    BuildCurrentAddress = 0x62,
    ParseGrouping = 0x63,
    ParseList = 0x64,
    ParseStructLiteralIfPresent = 0x65,
    ParsePostfixChain = 0x66,
    EmitDiag = 0x70,
    Fail = 0x72,
}

impl ExvmOpcodeV2 {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x00 => Some(Self::End),
            0x01 => Some(Self::Jump),
            0x02 => Some(Self::JumpIfTrue),
            0x03 => Some(Self::Call),
            0x04 => Some(Self::Return),
            0x10 => Some(Self::PeekKind),
            0x11 => Some(Self::PeekOperator),
            0x20 => Some(Self::Advance),
            0x21 => Some(Self::ConsumeOperator),
            0x22 => Some(Self::ConsumeKind),
            0x32 => Some(Self::LoadTokenText),
            0x40 => Some(Self::BuildUnary),
            0x41 => Some(Self::BuildBinary),
            0x42 => Some(Self::BuildTernary),
            0x43 => Some(Self::BuildRange),
            0x60 => Some(Self::BuildIdentifier),
            0x61 => Some(Self::BuildNumber),
            0x62 => Some(Self::BuildCurrentAddress),
            0x63 => Some(Self::ParseGrouping),
            0x64 => Some(Self::ParseList),
            0x65 => Some(Self::ParseStructLiteralIfPresent),
            0x66 => Some(Self::ParsePostfixChain),
            0x70 => Some(Self::EmitDiag),
            0x72 => Some(Self::Fail),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ExvmOperatorKindV2 {
    Plus = 0x01,
    Minus = 0x02,
    Multiply = 0x03,
    Divide = 0x04,
    Mod = 0x05,
    Power = 0x06,
    BitNot = 0x07,
    LogicNot = 0x08,
    Lt = 0x09,
    Gt = 0x0A,
    Shl = 0x0B,
    Shr = 0x0C,
    Eq = 0x0D,
    Ne = 0x0E,
    Ge = 0x0F,
    Le = 0x10,
    BitAnd = 0x11,
    BitOr = 0x12,
    BitXor = 0x13,
    LogicAnd = 0x14,
    LogicOr = 0x15,
    LogicXor = 0x16,
    Range = 0x17,
    RangeInclusive = 0x18,
}

impl ExvmOperatorKindV2 {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x01 => Some(Self::Plus),
            0x02 => Some(Self::Minus),
            0x03 => Some(Self::Multiply),
            0x04 => Some(Self::Divide),
            0x05 => Some(Self::Mod),
            0x06 => Some(Self::Power),
            0x07 => Some(Self::BitNot),
            0x08 => Some(Self::LogicNot),
            0x09 => Some(Self::Lt),
            0x0A => Some(Self::Gt),
            0x0B => Some(Self::Shl),
            0x0C => Some(Self::Shr),
            0x0D => Some(Self::Eq),
            0x0E => Some(Self::Ne),
            0x0F => Some(Self::Ge),
            0x10 => Some(Self::Le),
            0x11 => Some(Self::BitAnd),
            0x12 => Some(Self::BitOr),
            0x13 => Some(Self::BitXor),
            0x14 => Some(Self::LogicAnd),
            0x15 => Some(Self::LogicOr),
            0x16 => Some(Self::LogicXor),
            0x17 => Some(Self::Range),
            0x18 => Some(Self::RangeInclusive),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ExvmTokenKindV2 {
    Number = 0x01,
    Identifier = 0x02,
    Dollar = 0x03,
    OpenParen = 0x04,
    CloseParen = 0x05,
    Question = 0x06,
    Colon = 0x07,
    OpenBrace = 0x08,
}

impl ExvmTokenKindV2 {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x01 => Some(Self::Number),
            0x02 => Some(Self::Identifier),
            0x03 => Some(Self::Dollar),
            0x04 => Some(Self::OpenParen),
            0x05 => Some(Self::CloseParen),
            0x06 => Some(Self::Question),
            0x07 => Some(Self::Colon),
            0x08 => Some(Self::OpenBrace),
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
    pub semantic_programs: Vec<SemanticProgramDescriptor>,
    pub value_programs: Vec<ValueProgramDescriptor>,
    pub operand_record_programs: Vec<OperandRecordProgramDescriptor>,
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
