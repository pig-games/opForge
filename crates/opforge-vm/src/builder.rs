// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Build VM hierarchy chunks from the live opForge module registry.

use crate::bytecode::{OP_EMIT_OPERAND, OP_EMIT_U8, OP_END};
use crate::hierarchy::{
    CpuDescriptor, DialectDescriptor, FamilyDescriptor, HierarchyError, HierarchyPackage,
    ScopedFormDescriptor, ScopedOwner, ScopedRegisterDescriptor,
};
use crate::intel8080_vm::{
    compile_vm_program_for_instruction_entry, compile_vm_program_for_z80_cb_register,
    compile_vm_program_for_z80_half_index, compile_vm_program_for_z80_indexed_cb,
    compile_vm_program_for_z80_indexed_memory, compile_vm_program_for_z80_interrupt_mode,
    compile_vm_program_for_z80_ld_indirect, mode_key_for_instruction_entry,
    mode_key_for_z80_cb_register, mode_key_for_z80_half_index, mode_key_for_z80_indexed_cb,
    mode_key_for_z80_indexed_memory, mode_key_for_z80_interrupt_mode, mode_key_for_z80_ld_indirect,
};
use families::hd6309::instructions::CPU_INSTRUCTION_TABLE as HD6309_CPU_INSTRUCTION_TABLE;
use families::hd6309::module::CPU_ID as HD6309_CPU_ID;
use families::i8085::module::CPU_ID as I8085_CPU_ID;
use families::i8085::I8085_EXTENSION_TABLE;
use families::intel8080::module::FAMILY_ID as INTEL8080_FAMILY_ID;
use families::intel8080::{
    FAMILY_INSTRUCTION_TABLE as INTEL8080_FAMILY_INSTRUCTION_TABLE, Z80_EXTENSION_TABLE,
};
use families::m45gs02::instructions::CPU_INSTRUCTION_TABLE as M45GS02_CPU_INSTRUCTION_TABLE;
use families::m45gs02::module::CPU_ID as M45GS02_CPU_ID;
use families::m65816::instructions::CPU_INSTRUCTION_TABLE as M65816_CPU_INSTRUCTION_TABLE;
use families::m65816::module::CPU_ID as M65816_CPU_ID;
use families::m65816::M65816CpuHandler;
use families::m65c02::instructions::CPU_INSTRUCTION_TABLE as M65C02_CPU_INSTRUCTION_TABLE;
use families::m65c02::module::CPU_ID as M65C02_CPU_ID;
use families::m6800::module::FAMILY_ID as M6800_FAMILY_ID;
use families::m6800::AddressMode as M6800AddressMode;
use families::m6800::{
    FAMILY_INSTRUCTION_TABLE as M6800_FAMILY_INSTRUCTION_TABLE,
    PREFIXED_FAMILY_INSTRUCTION_TABLE as M6800_PREFIXED_FAMILY_INSTRUCTION_TABLE,
};
use families::mos6502::module::FAMILY_ID as MOS6502_FAMILY_ID;
use families::mos6502::{AddressMode, FAMILY_INSTRUCTION_TABLE};
use families::z80::module::CPU_ID as Z80_CPU_ID;
use opcore::expr_vm::PortableExprBudgets;
use package::{
    canonicalize_expr_parser_contracts, canonicalize_hierarchy_metadata,
    canonicalize_parser_contracts, canonicalize_parser_vm_programs, canonicalize_token_policies,
    canonicalize_tokenizer_vm_programs, default_runtime_diagnostic_catalog,
    default_token_policy_lexical_defaults, encode_hierarchy_chunks_from_chunks,
    token_identifier_class, ExprContractDescriptor, ExprDiagnosticMap,
    ExprParserContractDescriptor, ExprParserDiagnosticMap, HierarchyChunks, ModeSelectorDescriptor,
    OpcpuCodecError, ParserContractDescriptor, ParserDiagnosticMap, ParserVmOpcodeV2,
    ParserVmProgramDescriptor, TokenCaseRule, TokenPolicyDescriptor, TokenizerVmDiagnosticMap,
    TokenizerVmLimits, TokenizerVmOpcode, TokenizerVmProgramDescriptor,
    TokenizerVmStreamDescriptor, VmProgramDescriptor, DIAG_EXPR_BUDGET_EXCEEDED,
    DIAG_EXPR_EVAL_FAILURE, DIAG_EXPR_INVALID_OPCODE, DIAG_EXPR_INVALID_PROGRAM,
    DIAG_EXPR_STACK_DEPTH_EXCEEDED, DIAG_EXPR_STACK_UNDERFLOW, DIAG_EXPR_UNKNOWN_SYMBOL,
    DIAG_EXPR_UNSUPPORTED_FEATURE, DIAG_PARSER_EXPECTED_EXPRESSION, DIAG_PARSER_EXPECTED_OPERAND,
    DIAG_PARSER_INVALID_STATEMENT, DIAG_PARSER_UNEXPECTED_TOKEN,
    DIAG_TOKENIZER_ERROR_LIMIT_EXCEEDED, DIAG_TOKENIZER_INVALID_CHAR,
    DIAG_TOKENIZER_LEXEME_LIMIT_EXCEEDED, DIAG_TOKENIZER_STEP_LIMIT_EXCEEDED,
    DIAG_TOKENIZER_TOKEN_LIMIT_EXCEEDED, DIAG_TOKENIZER_UNTERMINATED_STRING,
    EXPR_PARSER_VM_OPCODE_VERSION_V1, EXPR_VM_OPCODE_VERSION_V1, PARSER_AST_SCHEMA_ID_LINE_V1,
    PARSER_GRAMMAR_ID_LINE_V1, PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
    TOKENIZER_VM_OPCODE_VERSION_V1,
};
use registry::family::CpuHandler;
use registry::registry::ModuleRegistry;

const OPCODE_NEG: u8 = 0x42;
const OPCODE_NOP: u8 = 0xEA;

/// Errors emitted while building hierarchy package data from registry metadata.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HierarchyBuildError {
    MissingFamilyMetadata { family_id: String },
    MissingCpuMetadata { cpu_id: String },
    Hierarchy(HierarchyError),
    Codec(OpcpuCodecError),
}

impl std::fmt::Display for HierarchyBuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingFamilyMetadata { family_id } => {
                write!(f, "missing registry metadata for family '{}'", family_id)
            }
            Self::MissingCpuMetadata { cpu_id } => {
                write!(f, "missing registry metadata for cpu '{}'", cpu_id)
            }
            Self::Hierarchy(err) => write!(f, "hierarchy validation error: {}", err),
            Self::Codec(err) => write!(f, "package codec error: {}", err),
        }
    }
}

impl std::error::Error for HierarchyBuildError {}

impl From<HierarchyError> for HierarchyBuildError {
    fn from(value: HierarchyError) -> Self {
        Self::Hierarchy(value)
    }
}

impl From<OpcpuCodecError> for HierarchyBuildError {
    fn from(value: OpcpuCodecError) -> Self {
        Self::Codec(value)
    }
}

/// Build `TOKS`/`TKVM`/`PARS`/`PRVM` + hierarchy chunks from registry metadata.
pub fn build_hierarchy_chunks_from_registry(
    registry: &ModuleRegistry,
) -> Result<HierarchyChunks, HierarchyBuildError> {
    let family_ids = registry.family_ids();

    let mut families = Vec::with_capacity(family_ids.len());
    for family in &family_ids {
        let canonical = registry
            .canonical_dialect_for_family(*family)
            .ok_or_else(|| HierarchyBuildError::MissingFamilyMetadata {
                family_id: family.as_str().to_string(),
            })?;
        families.push(FamilyDescriptor {
            id: family.as_str().to_string(),
            canonical_dialect: canonical.to_string(),
        });
    }

    let cpu_ids = registry.cpu_ids();
    let mut cpus = Vec::with_capacity(cpu_ids.len());
    for cpu in cpu_ids {
        let family_id =
            registry
                .cpu_family_id(cpu)
                .ok_or_else(|| HierarchyBuildError::MissingCpuMetadata {
                    cpu_id: cpu.as_str().to_string(),
                })?;
        let default_dialect = registry.cpu_default_dialect(cpu).map(ToString::to_string);
        cpus.push(CpuDescriptor {
            id: cpu.as_str().to_string(),
            family_id: family_id.as_str().to_string(),
            default_dialect,
        });
    }

    let mut dialects = Vec::new();
    for family in &family_ids {
        let family_id = family.as_str().to_string();
        for dialect in registry.dialect_ids_for_family(*family) {
            dialects.push(DialectDescriptor {
                id: dialect,
                family_id: family_id.clone(),
                cpu_allow_list: None,
            });
        }
    }
    let mut token_policies = family_ids
        .iter()
        .map(|family| default_family_token_policy(family.as_str()))
        .collect();
    let mut tokenizer_vm_programs = family_ids
        .iter()
        .map(|family| default_family_tokenizer_vm_program(family.as_str()))
        .collect();
    let mut parser_contracts = family_ids
        .iter()
        .map(|family| default_family_parser_contract(family.as_str()))
        .collect();
    let mut parser_vm_programs = family_ids
        .iter()
        .map(|family| default_family_parser_vm_program(family.as_str()))
        .collect();
    let expr_budget_defaults = PortableExprBudgets::default();
    let expr_budget_defaults = (
        expr_budget_defaults.max_program_bytes,
        expr_budget_defaults.max_stack_depth,
        expr_budget_defaults.max_symbol_refs,
        expr_budget_defaults.max_eval_steps,
    );
    let mut expr_contracts = family_ids
        .iter()
        .map(|family| default_family_expr_contract(family.as_str(), expr_budget_defaults))
        .collect();
    let mut expr_parser_contracts = family_ids
        .iter()
        .map(|family| default_family_expr_parser_contract(family.as_str()))
        .collect();

    let mut registers = Vec::new();
    for family in &family_ids {
        for register_id in registry.family_register_ids(*family) {
            registers.push(ScopedRegisterDescriptor {
                owner: ScopedOwner::Family(family.as_str().to_string()),
                id: register_id,
            });
        }
    }
    for cpu in registry.cpu_ids() {
        for register_id in registry.cpu_register_ids(cpu) {
            registers.push(ScopedRegisterDescriptor {
                owner: ScopedOwner::Cpu(cpu.as_str().to_string()),
                id: register_id,
            });
        }
    }

    let mut forms = Vec::new();
    for family in &family_ids {
        for mnemonic in registry.family_form_mnemonics(*family) {
            forms.push(ScopedFormDescriptor {
                owner: ScopedOwner::Family(family.as_str().to_string()),
                mnemonic,
            });
        }
    }
    for cpu in registry.cpu_ids() {
        for mnemonic in registry.cpu_form_mnemonics(cpu) {
            forms.push(ScopedFormDescriptor {
                owner: ScopedOwner::Cpu(cpu.as_str().to_string()),
                mnemonic,
            });
        }
    }
    for family in &family_ids {
        for dialect_id in registry.dialect_ids_for_family(*family) {
            for mnemonic in registry.dialect_form_mnemonics(*family, &dialect_id) {
                forms.push(ScopedFormDescriptor {
                    owner: ScopedOwner::Dialect(dialect_id.clone()),
                    mnemonic,
                });
            }
        }
    }

    let mut tables = Vec::new();
    let mut selectors = Vec::new();
    let registered_family_ids: std::collections::HashSet<String> = family_ids
        .iter()
        .map(|family| family.as_str().to_ascii_lowercase())
        .collect();
    let registered_cpu_ids: std::collections::HashSet<String> = registry
        .cpu_ids()
        .iter()
        .map(|cpu| cpu.as_str().to_ascii_lowercase())
        .collect();
    let has_m65816 = registered_cpu_ids.contains(M65816_CPU_ID.as_str());

    if registered_family_ids.contains(INTEL8080_FAMILY_ID.as_str()) {
        for entry in INTEL8080_FAMILY_INSTRUCTION_TABLE {
            let Some(program) = compile_vm_program_for_instruction_entry(entry) else {
                continue;
            };
            tables.push(VmProgramDescriptor {
                owner: ScopedOwner::Family(INTEL8080_FAMILY_ID.as_str().to_string()),
                mnemonic: entry.mnemonic.to_string(),
                mode_key: mode_key_for_instruction_entry(entry),
                program,
            });
        }
    }
    if registered_cpu_ids.contains(I8085_CPU_ID.as_str()) {
        for entry in I8085_EXTENSION_TABLE {
            let Some(program) = compile_vm_program_for_instruction_entry(entry) else {
                continue;
            };
            tables.push(VmProgramDescriptor {
                owner: ScopedOwner::Cpu(I8085_CPU_ID.as_str().to_string()),
                mnemonic: entry.mnemonic.to_string(),
                mode_key: mode_key_for_instruction_entry(entry),
                program,
            });
        }
    }
    if registered_cpu_ids.contains(Z80_CPU_ID.as_str()) {
        for mnemonic in [
            "BIT", "RES", "SET", "RLC", "RRC", "RL", "RR", "SLA", "SRA", "SLL", "SRL",
        ] {
            forms.push(ScopedFormDescriptor {
                owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                mnemonic: mnemonic.to_string(),
            });
        }

        for register in ["B", "C", "D", "E", "H", "L", "M", "A"] {
            for mnemonic in ["RLC", "RRC", "RL", "RR", "SLA", "SRA", "SLL", "SRL"] {
                let Some(mode_key) = mode_key_for_z80_cb_register(mnemonic, None, register) else {
                    continue;
                };
                let Some(program) =
                    compile_vm_program_for_z80_cb_register(mnemonic, None, register)
                else {
                    continue;
                };
                tables.push(VmProgramDescriptor {
                    owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                    mnemonic: mnemonic.to_string(),
                    mode_key,
                    program,
                });
            }

            for mnemonic in ["BIT", "RES", "SET"] {
                for bit in 0u8..=7 {
                    let Some(mode_key) =
                        mode_key_for_z80_cb_register(mnemonic, Some(bit), register)
                    else {
                        continue;
                    };
                    let Some(program) =
                        compile_vm_program_for_z80_cb_register(mnemonic, Some(bit), register)
                    else {
                        continue;
                    };
                    tables.push(VmProgramDescriptor {
                        owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                        mnemonic: mnemonic.to_string(),
                        mode_key,
                        program,
                    });
                }
            }
        }

        for base in ["IX", "IY"] {
            for mnemonic in ["RLC", "RRC", "RL", "RR", "SLA", "SRA", "SLL", "SRL"] {
                let Some(mode_key) = mode_key_for_z80_indexed_cb(base, mnemonic, None) else {
                    continue;
                };
                let Some(program) = compile_vm_program_for_z80_indexed_cb(base, mnemonic, None)
                else {
                    continue;
                };
                tables.push(VmProgramDescriptor {
                    owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                    mnemonic: mnemonic.to_string(),
                    mode_key,
                    program,
                });
            }
            for bit in 0u8..=7 {
                for mnemonic in ["BIT", "RES", "SET"] {
                    let Some(mode_key) = mode_key_for_z80_indexed_cb(base, mnemonic, Some(bit))
                    else {
                        continue;
                    };
                    let Some(program) =
                        compile_vm_program_for_z80_indexed_cb(base, mnemonic, Some(bit))
                    else {
                        continue;
                    };
                    tables.push(VmProgramDescriptor {
                        owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                        mnemonic: mnemonic.to_string(),
                        mode_key,
                        program,
                    });
                }
            }

            for (reg, code) in [
                ("B", 0u8),
                ("C", 1),
                ("D", 2),
                ("E", 3),
                ("H", 4),
                ("L", 5),
                ("A", 7),
            ] {
                let from_idx_form = format!("ld_r_from_idx_{}", reg.to_ascii_lowercase());
                let Some(mode_key) = mode_key_for_z80_indexed_memory(base, from_idx_form.as_str())
                else {
                    continue;
                };
                let Some(program) =
                    compile_vm_program_for_z80_indexed_memory(base, 0x46 | (code << 3), 1)
                else {
                    continue;
                };
                tables.push(VmProgramDescriptor {
                    owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                    mnemonic: "LD".to_string(),
                    mode_key,
                    program,
                });

                let to_idx_form = format!("ld_idx_from_r_{}", reg.to_ascii_lowercase());
                let Some(mode_key) = mode_key_for_z80_indexed_memory(base, to_idx_form.as_str())
                else {
                    continue;
                };
                let Some(program) = compile_vm_program_for_z80_indexed_memory(base, 0x70 | code, 1)
                else {
                    continue;
                };
                tables.push(VmProgramDescriptor {
                    owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                    mnemonic: "LD".to_string(),
                    mode_key,
                    program,
                });
            }

            for (form, opcode, operand_count, mnemonic) in [
                ("ld_idx_imm", 0x36u8, 2u8, "LD"),
                ("inc_idx", 0x34, 1, "INC"),
                ("dec_idx", 0x35, 1, "DEC"),
                ("add_a_idx", 0x86, 1, "ADD"),
                ("adc_a_idx", 0x8E, 1, "ADC"),
                ("sub_idx", 0x96, 1, "SUB"),
                ("sbc_a_idx", 0x9E, 1, "SBC"),
                ("and_idx", 0xA6, 1, "AND"),
                ("xor_idx", 0xAE, 1, "XOR"),
                ("or_idx", 0xB6, 1, "OR"),
                ("cp_idx", 0xBE, 1, "CP"),
            ] {
                let Some(mode_key) = mode_key_for_z80_indexed_memory(base, form) else {
                    continue;
                };
                let Some(program) =
                    compile_vm_program_for_z80_indexed_memory(base, opcode, operand_count)
                else {
                    continue;
                };
                tables.push(VmProgramDescriptor {
                    owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                    mnemonic: mnemonic.to_string(),
                    mode_key,
                    program,
                });
            }
        }

        for register in ["A", "HL", "BC", "DE", "SP", "IX", "IY"] {
            let Some(load_mode_key) = mode_key_for_z80_ld_indirect(register, false) else {
                continue;
            };
            let Some(load_program) = compile_vm_program_for_z80_ld_indirect(register, false) else {
                continue;
            };
            tables.push(VmProgramDescriptor {
                owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                mnemonic: "LD".to_string(),
                mode_key: load_mode_key,
                program: load_program,
            });

            let Some(store_mode_key) = mode_key_for_z80_ld_indirect(register, true) else {
                continue;
            };
            let Some(store_program) = compile_vm_program_for_z80_ld_indirect(register, true) else {
                continue;
            };
            tables.push(VmProgramDescriptor {
                owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                mnemonic: "LD".to_string(),
                mode_key: store_mode_key,
                program: store_program,
            });
        }

        for prefix in ["IX", "IY"] {
            for dst_code in [0u8, 1, 2, 3, 4, 5, 7] {
                for src_code in [0u8, 1, 2, 3, 4, 5, 7] {
                    if dst_code != 4 && dst_code != 5 && src_code != 4 && src_code != 5 {
                        continue;
                    }
                    let opcode = 0x40 | (dst_code << 3) | src_code;
                    let form = format!("rr:{dst_code}:{src_code}");
                    let Some(mode_key) = mode_key_for_z80_half_index(prefix, "LD", form.as_str())
                    else {
                        continue;
                    };
                    let Some(program) = compile_vm_program_for_z80_half_index(prefix, opcode, 0)
                    else {
                        continue;
                    };
                    tables.push(VmProgramDescriptor {
                        owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                        mnemonic: "LD".to_string(),
                        mode_key,
                        program,
                    });
                }
            }

            for dst_code in [4u8, 5] {
                let opcode = 0x06 | (dst_code << 3);
                let form = format!("ri:{dst_code}");
                let Some(mode_key) = mode_key_for_z80_half_index(prefix, "LD", form.as_str())
                else {
                    continue;
                };
                let Some(program) = compile_vm_program_for_z80_half_index(prefix, opcode, 1) else {
                    continue;
                };
                tables.push(VmProgramDescriptor {
                    owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                    mnemonic: "LD".to_string(),
                    mode_key,
                    program,
                });
            }

            for (mnemonic, base_opcode) in [("INC", 0x04u8), ("DEC", 0x05)] {
                for code in [4u8, 5] {
                    let opcode = base_opcode | (code << 3);
                    let form = format!("r:{code}");
                    let Some(mode_key) =
                        mode_key_for_z80_half_index(prefix, mnemonic, form.as_str())
                    else {
                        continue;
                    };
                    let Some(program) = compile_vm_program_for_z80_half_index(prefix, opcode, 0)
                    else {
                        continue;
                    };
                    tables.push(VmProgramDescriptor {
                        owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                        mnemonic: mnemonic.to_string(),
                        mode_key,
                        program,
                    });
                }
            }

            for (mnemonic, base_opcode) in [
                ("ADD", 0x80u8),
                ("ADC", 0x88),
                ("SUB", 0x90),
                ("SBC", 0x98),
                ("AND", 0xA0),
                ("XOR", 0xA8),
                ("OR", 0xB0),
                ("CP", 0xB8),
            ] {
                for code in [4u8, 5] {
                    let opcode = base_opcode | code;
                    let form = format!("r:{code}");
                    let Some(mode_key) =
                        mode_key_for_z80_half_index(prefix, mnemonic, form.as_str())
                    else {
                        continue;
                    };
                    let Some(program) = compile_vm_program_for_z80_half_index(prefix, opcode, 0)
                    else {
                        continue;
                    };
                    tables.push(VmProgramDescriptor {
                        owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                        mnemonic: mnemonic.to_string(),
                        mode_key,
                        program,
                    });
                }
            }
        }

        for entry in Z80_EXTENSION_TABLE {
            if entry.mnemonic.eq_ignore_ascii_case("IM") {
                for mode in 0u8..=2 {
                    let Some(mode_key) = mode_key_for_z80_interrupt_mode(mode) else {
                        continue;
                    };
                    let Some(program) = compile_vm_program_for_z80_interrupt_mode(mode) else {
                        continue;
                    };
                    tables.push(VmProgramDescriptor {
                        owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                        mnemonic: entry.mnemonic.to_string(),
                        mode_key,
                        program,
                    });
                }
                continue;
            }
            let Some(program) = compile_vm_program_for_instruction_entry(entry) else {
                continue;
            };
            tables.push(VmProgramDescriptor {
                owner: ScopedOwner::Cpu(Z80_CPU_ID.as_str().to_string()),
                mnemonic: entry.mnemonic.to_string(),
                mode_key: mode_key_for_instruction_entry(entry),
                program,
            });
        }
    }

    if registered_family_ids.contains(M6800_FAMILY_ID.as_str()) {
        emit_m6800_table_programs(
            M6800_FAMILY_INSTRUCTION_TABLE,
            ScopedOwner::Family(M6800_FAMILY_ID.as_str().to_string()),
            &mut tables,
            |entry| entry.mnemonic,
            |entry| entry.mode,
            |entry| entry.opcode,
        );
        emit_m6800_prefixed_table_programs(
            M6800_PREFIXED_FAMILY_INSTRUCTION_TABLE,
            ScopedOwner::Family(M6800_FAMILY_ID.as_str().to_string()),
            &mut tables,
            |entry| entry.mnemonic,
            |entry| entry.mode,
            |entry| entry.opcode_bytes,
        );
    }
    if registered_cpu_ids.contains(HD6309_CPU_ID.as_str()) {
        emit_m6800_prefixed_table_programs(
            HD6309_CPU_INSTRUCTION_TABLE,
            ScopedOwner::Cpu(HD6309_CPU_ID.as_str().to_string()),
            &mut tables,
            |entry| entry.mnemonic,
            |entry| entry.mode,
            |entry| entry.opcode_bytes,
        );
    }

    if registered_family_ids.contains(MOS6502_FAMILY_ID.as_str()) {
        emit_mos_style_table_programs(
            FAMILY_INSTRUCTION_TABLE,
            ScopedOwner::Family(MOS6502_FAMILY_ID.as_str().to_string()),
            &mut tables,
            &mut selectors,
            false,
            has_m65816,
            has_m65816,
            false,
            |_| true,
            |entry| entry.mnemonic,
            |entry| entry.mode,
            |entry| entry.opcode,
        );
    }
    if registered_cpu_ids.contains(M65C02_CPU_ID.as_str()) {
        emit_mos_style_table_programs(
            M65C02_CPU_INSTRUCTION_TABLE,
            ScopedOwner::Cpu(M65C02_CPU_ID.as_str().to_string()),
            &mut tables,
            &mut selectors,
            false,
            false,
            false,
            false,
            |_| true,
            |entry| entry.mnemonic,
            |entry| entry.mode,
            |entry| entry.opcode,
        );
        tables.extend(compile_m65c02_bit_branch_programs());
        selectors.extend(compile_m65c02_bit_branch_selectors());
    }
    if registered_cpu_ids.contains(M45GS02_CPU_ID.as_str()) {
        emit_mos_style_table_programs(
            M45GS02_CPU_INSTRUCTION_TABLE,
            ScopedOwner::Cpu(M45GS02_CPU_ID.as_str().to_string()),
            &mut tables,
            &mut selectors,
            false,
            false,
            false,
            false,
            |_| true,
            |entry| entry.mnemonic,
            |entry| entry.mode,
            |entry| entry.opcode,
        );
        emit_m45gs02_prefixed_programs(&mut tables, &mut selectors);
    }
    if registered_cpu_ids.contains(M65816_CPU_ID.as_str()) {
        let m65816_handler = M65816CpuHandler::new();
        let m65816_owner = ScopedOwner::Cpu(M65816_CPU_ID.as_str().to_string());
        emit_mos_style_table_programs(
            M65816_CPU_INSTRUCTION_TABLE,
            m65816_owner.clone(),
            &mut tables,
            &mut selectors,
            true,
            false,
            true,
            true,
            |_| true,
            |entry| entry.mnemonic,
            |entry| entry.mode,
            |entry| entry.opcode,
        );
        emit_mos_style_table_programs(
            M65C02_CPU_INSTRUCTION_TABLE,
            m65816_owner,
            &mut tables,
            &mut selectors,
            true,
            false,
            true,
            true,
            |entry| {
                <M65816CpuHandler as CpuHandler>::supports_mnemonic(&m65816_handler, entry.mnemonic)
            },
            |entry| entry.mnemonic,
            |entry| entry.mode,
            |entry| entry.opcode,
        );
    }

    canonicalize_hierarchy_metadata(
        &mut families,
        &mut cpus,
        &mut dialects,
        &mut registers,
        &mut forms,
        &mut tables,
        &mut selectors,
    );
    canonicalize_token_policies(&mut token_policies);
    canonicalize_tokenizer_vm_programs(&mut tokenizer_vm_programs);
    canonicalize_parser_contracts(&mut parser_contracts);
    canonicalize_parser_vm_programs(&mut parser_vm_programs);
    package::canonicalize_expr_contracts(&mut expr_contracts);
    canonicalize_expr_parser_contracts(&mut expr_parser_contracts);

    // Ensure the materialized metadata is coherent before returning.
    HierarchyPackage::new(families.clone(), cpus.clone(), dialects.clone())?;

    Ok(HierarchyChunks {
        metadata: package::PackageMetaDescriptor::default(),
        strings: Vec::new(),
        diagnostics: default_runtime_diagnostic_catalog(),
        token_policies,
        tokenizer_vm_programs,
        parser_contracts,
        parser_vm_programs,
        expr_contracts,
        expr_parser_contracts,
        families,
        cpus,
        dialects,
        registers,
        forms,
        tables,
        selectors,
    })
}

fn default_family_expr_contract(
    family_id: &str,
    budget_defaults: (usize, usize, usize, usize),
) -> ExprContractDescriptor {
    ExprContractDescriptor {
        owner: ScopedOwner::Family(family_id.to_string()),
        opcode_version: EXPR_VM_OPCODE_VERSION_V1,
        max_program_bytes: budget_defaults.0 as u32,
        max_stack_depth: budget_defaults.1 as u32,
        max_symbol_refs: budget_defaults.2 as u32,
        max_eval_steps: budget_defaults.3 as u32,
        diagnostics: ExprDiagnosticMap {
            invalid_opcode: DIAG_EXPR_INVALID_OPCODE.to_string(),
            stack_underflow: DIAG_EXPR_STACK_UNDERFLOW.to_string(),
            stack_depth_exceeded: DIAG_EXPR_STACK_DEPTH_EXCEEDED.to_string(),
            unknown_symbol: DIAG_EXPR_UNKNOWN_SYMBOL.to_string(),
            eval_failure: DIAG_EXPR_EVAL_FAILURE.to_string(),
            unsupported_feature: DIAG_EXPR_UNSUPPORTED_FEATURE.to_string(),
            budget_exceeded: DIAG_EXPR_BUDGET_EXCEEDED.to_string(),
            invalid_program: DIAG_EXPR_INVALID_PROGRAM.to_string(),
        },
    }
}

fn default_family_expr_parser_contract(family_id: &str) -> ExprParserContractDescriptor {
    ExprParserContractDescriptor {
        owner: ScopedOwner::Family(family_id.to_string()),
        opcode_version: EXPR_PARSER_VM_OPCODE_VERSION_V1,
        diagnostics: ExprParserDiagnosticMap {
            invalid_expression_program: DIAG_PARSER_INVALID_STATEMENT.to_string(),
        },
    }
}

fn default_family_token_policy(family_id: &str) -> TokenPolicyDescriptor {
    let defaults = default_token_policy_lexical_defaults();
    TokenPolicyDescriptor {
        owner: ScopedOwner::Family(family_id.to_string()),
        case_rule: TokenCaseRule::AsciiLower,
        identifier_start_class: token_identifier_class::ASCII_ALPHA
            | token_identifier_class::UNDERSCORE
            | token_identifier_class::DOT,
        identifier_continue_class: token_identifier_class::ASCII_ALPHA
            | token_identifier_class::ASCII_DIGIT
            | token_identifier_class::UNDERSCORE
            | token_identifier_class::DOLLAR
            | token_identifier_class::AT_SIGN
            | token_identifier_class::DOT,
        punctuation_chars: ",()[]{}+-*/#<>:=.&|^%!~;".to_string(),
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

fn default_family_tokenizer_vm_program(family_id: &str) -> TokenizerVmProgramDescriptor {
    let program = default_family_tokenizer_vm_program_bytes();
    TokenizerVmProgramDescriptor {
        owner: ScopedOwner::Family(family_id.to_string()),
        opcode_version: TOKENIZER_VM_OPCODE_VERSION_V1,
        start_state: 0,
        state_entry_offsets: vec![0],
        stream: TokenizerVmStreamDescriptor::default(),
        limits: TokenizerVmLimits {
            max_steps_per_line: 2048,
            max_tokens_per_line: 256,
            max_lexeme_bytes: 1024,
            max_errors_per_line: 16,
        },
        diagnostics: TokenizerVmDiagnosticMap {
            invalid_char: DIAG_TOKENIZER_INVALID_CHAR.to_string(),
            unterminated_string: DIAG_TOKENIZER_UNTERMINATED_STRING.to_string(),
            step_limit_exceeded: DIAG_TOKENIZER_STEP_LIMIT_EXCEEDED.to_string(),
            token_limit_exceeded: DIAG_TOKENIZER_TOKEN_LIMIT_EXCEEDED.to_string(),
            lexeme_limit_exceeded: DIAG_TOKENIZER_LEXEME_LIMIT_EXCEEDED.to_string(),
            error_limit_exceeded: DIAG_TOKENIZER_ERROR_LIMIT_EXCEEDED.to_string(),
        },
        // Default tokenizer VM loop:
        // - scan exactly one core token from the current cursor
        // - detect EOL/comment termination
        // - loop until done
        //
        // This keeps assembler tokenization VM-authoritative while preserving
        // parity with core token semantics for now.
        program,
    }
}

fn default_family_parser_contract(family_id: &str) -> ParserContractDescriptor {
    ParserContractDescriptor {
        owner: ScopedOwner::Family(family_id.to_string()),
        grammar_id: PARSER_GRAMMAR_ID_LINE_V1.to_string(),
        ast_schema_id: PARSER_AST_SCHEMA_ID_LINE_V1.to_string(),
        opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
        max_ast_nodes_per_line: 1024,
        diagnostics: ParserDiagnosticMap {
            unexpected_token: DIAG_PARSER_UNEXPECTED_TOKEN.to_string(),
            expected_expression: DIAG_PARSER_EXPECTED_EXPRESSION.to_string(),
            expected_operand: DIAG_PARSER_EXPECTED_OPERAND.to_string(),
            invalid_statement: DIAG_PARSER_INVALID_STATEMENT.to_string(),
        },
    }
}

fn default_family_parser_vm_program(family_id: &str) -> ParserVmProgramDescriptor {
    ParserVmProgramDescriptor {
        owner: ScopedOwner::Family(family_id.to_string()),
        opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
        program: default_family_parser_vm_program_bytes(),
    }
}

fn default_family_parser_vm_program_bytes() -> Vec<u8> {
    vec![
        ParserVmOpcodeV2::BeginStatement as u8,
        ParserVmOpcodeV2::ParseOptionalLeadingLabel as u8,
        ParserVmOpcodeV2::IsEol as u8,
        ParserVmOpcodeV2::JumpIfFalse as u8,
        8,
        0,
        ParserVmOpcodeV2::FinishLine as u8,
        ParserVmOpcodeV2::End as u8,
        ParserVmOpcodeV2::PeekAssignmentOperator as u8,
        ParserVmOpcodeV2::JumpIfFalse as u8,
        14,
        0,
        ParserVmOpcodeV2::FinishAssignment as u8,
        ParserVmOpcodeV2::End as u8,
        ParserVmOpcodeV2::PeekStarOrg as u8,
        ParserVmOpcodeV2::JumpIfFalse as u8,
        36,
        0,
        ParserVmOpcodeV2::LoadInlineText as u8,
        4,
        b'.',
        b'o',
        b'r',
        b'g',
        ParserVmOpcodeV2::SetMnemonic as u8,
        ParserVmOpcodeV2::Advance as u8,
        ParserVmOpcodeV2::ConsumeOperator as u8,
        0x02,
        ParserVmOpcodeV2::ScanTopLevelCommaBoundaries as u8,
        ParserVmOpcodeV2::ParseOperandExprRange as u8,
        0xFF,
        0xFF,
        0xFF,
        0xFF,
        ParserVmOpcodeV2::FinishLine as u8,
        ParserVmOpcodeV2::End as u8,
        ParserVmOpcodeV2::PeekKind as u8,
        0x03,
        ParserVmOpcodeV2::JumpIfFalse as u8,
        48,
        0,
        ParserVmOpcodeV2::Advance as u8,
        ParserVmOpcodeV2::LoadIdentifier as u8,
        ParserVmOpcodeV2::SetDotMnemonic as u8,
        ParserVmOpcodeV2::Advance as u8,
        ParserVmOpcodeV2::Jump as u8,
        51,
        0,
        ParserVmOpcodeV2::LoadIdentifier as u8,
        ParserVmOpcodeV2::SetMnemonic as u8,
        ParserVmOpcodeV2::Advance as u8,
        ParserVmOpcodeV2::ScanTopLevelCommaBoundaries as u8,
        ParserVmOpcodeV2::ParseOperandExprRange as u8,
        0xFF,
        0xFF,
        0xFF,
        0xFF,
        ParserVmOpcodeV2::FinishLine as u8,
        ParserVmOpcodeV2::End as u8,
    ]
}

fn default_family_tokenizer_vm_program_bytes() -> Vec<u8> {
    let loop_offset = 0u32;
    let mut program = Vec::new();

    // Dispatch tokenization by the current byte class and keep scanning until EOL.
    program.push(TokenizerVmOpcode::ReadChar as u8);
    program.push(TokenizerVmOpcode::JumpIfEol as u8);
    let eol_target_patch = program.len();
    program.extend_from_slice(&0u32.to_le_bytes());

    program.push(TokenizerVmOpcode::JumpIfClass as u8);
    program.push(1);
    let whitespace_target_patch = program.len();
    program.extend_from_slice(&0u32.to_le_bytes());

    program.push(TokenizerVmOpcode::JumpIfByteEq as u8);
    program.push(b'.');
    let symbol_target_patch = program.len();
    program.extend_from_slice(&0u32.to_le_bytes());

    program.push(TokenizerVmOpcode::JumpIfClass as u8);
    program.push(2);
    let identifier_target_patch = program.len();
    program.extend_from_slice(&0u32.to_le_bytes());

    program.push(TokenizerVmOpcode::JumpIfClass as u8);
    program.push(4);
    let number_target_patch = program.len();
    program.extend_from_slice(&0u32.to_le_bytes());

    program.push(TokenizerVmOpcode::JumpIfClass as u8);
    program.push(5);
    let string_target_patch = program.len();
    program.extend_from_slice(&0u32.to_le_bytes());

    let symbol_offset = program.len() as u32;
    program.push(TokenizerVmOpcode::ScanSymbol as u8);
    program.push(TokenizerVmOpcode::Jump as u8);
    program.extend_from_slice(&loop_offset.to_le_bytes());

    let whitespace_offset = program.len() as u32;
    program.push(TokenizerVmOpcode::Advance as u8);
    program.push(TokenizerVmOpcode::Jump as u8);
    program.extend_from_slice(&loop_offset.to_le_bytes());

    let identifier_offset = program.len() as u32;
    program.push(TokenizerVmOpcode::ScanIdentifier as u8);
    program.push(TokenizerVmOpcode::Jump as u8);
    program.extend_from_slice(&loop_offset.to_le_bytes());

    let number_offset = program.len() as u32;
    program.push(TokenizerVmOpcode::ScanNumber as u8);
    program.push(TokenizerVmOpcode::Jump as u8);
    program.extend_from_slice(&loop_offset.to_le_bytes());

    let string_offset = program.len() as u32;
    program.push(TokenizerVmOpcode::ScanString as u8);
    program.push(TokenizerVmOpcode::Jump as u8);
    program.extend_from_slice(&loop_offset.to_le_bytes());

    let end_offset = program.len() as u32;
    program[eol_target_patch..eol_target_patch + 4].copy_from_slice(&end_offset.to_le_bytes());
    program[whitespace_target_patch..whitespace_target_patch + 4]
        .copy_from_slice(&whitespace_offset.to_le_bytes());
    program[symbol_target_patch..symbol_target_patch + 4]
        .copy_from_slice(&symbol_offset.to_le_bytes());
    program[identifier_target_patch..identifier_target_patch + 4]
        .copy_from_slice(&identifier_offset.to_le_bytes());
    program[number_target_patch..number_target_patch + 4]
        .copy_from_slice(&number_offset.to_le_bytes());
    program[string_target_patch..string_target_patch + 4]
        .copy_from_slice(&string_offset.to_le_bytes());
    program.push(TokenizerVmOpcode::End as u8);

    program
}

fn compile_opcode_program(opcode: u8, operand_count: usize) -> Vec<u8> {
    let mut program = vec![OP_EMIT_U8, opcode];
    for operand_index in 0..operand_count {
        program.push(OP_EMIT_OPERAND);
        program.push(operand_index as u8);
    }
    program.push(OP_END);
    program
}

fn compile_prefixed_opcode_program(opcode_bytes: &[u8], operand_count: usize) -> Vec<u8> {
    let mut program = Vec::with_capacity((opcode_bytes.len() * 2) + (operand_count * 2) + 1);
    for opcode in opcode_bytes {
        program.push(OP_EMIT_U8);
        program.push(*opcode);
    }
    for operand_index in 0..operand_count {
        program.push(OP_EMIT_OPERAND);
        program.push(operand_index as u8);
    }
    program.push(OP_END);
    program
}

fn m6800_mode_operand_count(mode: M6800AddressMode) -> usize {
    match mode {
        M6800AddressMode::Inherent => 0,
        M6800AddressMode::Immediate8
        | M6800AddressMode::Immediate16
        | M6800AddressMode::Direct
        | M6800AddressMode::Extended
        | M6800AddressMode::Indexed
        | M6800AddressMode::Relative8
        | M6800AddressMode::Relative16
        | M6800AddressMode::RegisterPair
        | M6800AddressMode::RegisterList => 1,
    }
}

fn emit_m6800_table_programs<T, I, FMnemonic, FMode, FOpcode>(
    entries: I,
    owner: ScopedOwner,
    tables: &mut Vec<VmProgramDescriptor>,
    mnemonic_of: FMnemonic,
    mode_of: FMode,
    opcode_of: FOpcode,
) where
    I: IntoIterator<Item = T>,
    FMnemonic: Fn(&T) -> &str,
    FMode: Fn(&T) -> M6800AddressMode,
    FOpcode: Fn(&T) -> u8,
{
    for entry in entries {
        let mode = mode_of(&entry);
        tables.push(VmProgramDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic_of(&entry).to_string(),
            mode_key: format!("{mode:?}"),
            program: compile_opcode_program(opcode_of(&entry), m6800_mode_operand_count(mode)),
        });
    }
}

fn emit_m6800_prefixed_table_programs<T, I, FMnemonic, FMode, FOpcode>(
    entries: I,
    owner: ScopedOwner,
    tables: &mut Vec<VmProgramDescriptor>,
    mnemonic_of: FMnemonic,
    mode_of: FMode,
    opcode_bytes_of: FOpcode,
) where
    I: IntoIterator<Item = T>,
    FMnemonic: Fn(&T) -> &str,
    FMode: Fn(&T) -> M6800AddressMode,
    FOpcode: Fn(&T) -> &[u8],
{
    for entry in entries {
        let mode = mode_of(&entry);
        tables.push(VmProgramDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic_of(&entry).to_string(),
            mode_key: format!("{mode:?}"),
            program: compile_prefixed_opcode_program(
                opcode_bytes_of(&entry),
                m6800_mode_operand_count(mode),
            ),
        });
    }
}

fn prepend_u8_prefixes_to_program(program: &[u8], prefixes: &[u8]) -> Vec<u8> {
    let mut prefixed = Vec::with_capacity(program.len() + prefixes.len() * 2);
    for prefix in prefixes {
        prefixed.push(OP_EMIT_U8);
        prefixed.push(*prefix);
    }
    prefixed.extend_from_slice(program);
    prefixed
}

fn m45gs02_q_mnemonics_for_base(base_mnemonic: &str) -> &'static [&'static str] {
    match base_mnemonic.to_ascii_uppercase().as_str() {
        "LDA" => &["LDQ", "LDAQ"],
        "STA" => &["STQ"],
        "ADC" => &["ADCQ"],
        "AND" => &["ANDQ"],
        "CMP" => &["CMPQ"],
        "EOR" => &["EORQ"],
        "ORA" => &["ORAQ"],
        "SBC" => &["SBCQ"],
        _ => &[],
    }
}

fn emit_m45gs02_prefixed_programs(
    tables: &mut Vec<VmProgramDescriptor>,
    selectors: &mut Vec<ModeSelectorDescriptor>,
) {
    let owner = ScopedOwner::Cpu(M45GS02_CPU_ID.as_str().to_string());
    let existing_tables = tables.clone();

    let mut seen_program_keys: std::collections::HashSet<(String, String)> =
        std::collections::HashSet::new();
    let mut seen_selector_keys: std::collections::HashSet<(String, String, String)> =
        std::collections::HashSet::new();

    for entry in &existing_tables {
        let base_upper = entry.mnemonic.to_ascii_uppercase();
        for q_mnemonic in m45gs02_q_mnemonics_for_base(base_upper.as_str()) {
            let q_mnemonic_lower = q_mnemonic.to_ascii_lowercase();
            let program_key = (
                q_mnemonic_lower.clone(),
                entry.mode_key.to_ascii_lowercase(),
            );
            if seen_program_keys.insert(program_key) {
                tables.push(VmProgramDescriptor {
                    owner: owner.clone(),
                    mnemonic: q_mnemonic_lower.clone(),
                    mode_key: entry.mode_key.clone(),
                    program: prepend_u8_prefixes_to_program(
                        &entry.program,
                        &[OPCODE_NEG, OPCODE_NEG],
                    ),
                });
            }

            if let Some(mode) = parse_mode_key_lower(entry.mode_key.to_ascii_lowercase().as_str()) {
                if let Some(selector) =
                    compile_mode_selector(owner.clone(), q_mnemonic_lower.as_str(), mode, false)
                {
                    let selector_key = (
                        selector.mnemonic.to_ascii_lowercase(),
                        selector.shape_key.to_ascii_lowercase(),
                        selector.mode_key.to_ascii_lowercase(),
                    );
                    if seen_selector_keys.insert(selector_key) {
                        selectors.push(selector);
                    }
                }
            }
        }
    }

    let indirect_indexed_y_mode_key = format!("{:?}", AddressMode::IndirectIndexedY);
    let flat_mode_targets = [
        AddressMode::IndirectIndexedZ,
        AddressMode::DirectPageIndirectLongZ,
    ];
    for entry in &existing_tables {
        if !entry
            .mode_key
            .eq_ignore_ascii_case(indirect_indexed_y_mode_key.as_str())
        {
            continue;
        }

        for target_mode in flat_mode_targets {
            let target_mode_key = format!("{:?}", target_mode);
            let program_key = (
                entry.mnemonic.to_ascii_lowercase(),
                target_mode_key.to_ascii_lowercase(),
            );
            if seen_program_keys.insert(program_key) {
                tables.push(VmProgramDescriptor {
                    owner: owner.clone(),
                    mnemonic: entry.mnemonic.clone(),
                    mode_key: target_mode_key.clone(),
                    program: prepend_u8_prefixes_to_program(&entry.program, &[OPCODE_NOP]),
                });
            }

            if let Some(selector) =
                compile_mode_selector(owner.clone(), entry.mnemonic.as_str(), target_mode, false)
            {
                let selector_key = (
                    selector.mnemonic.to_ascii_lowercase(),
                    selector.shape_key.to_ascii_lowercase(),
                    selector.mode_key.to_ascii_lowercase(),
                );
                if seen_selector_keys.insert(selector_key) {
                    selectors.push(selector);
                }
            }
        }
    }
}

fn parse_mode_key_lower(mode_key_lower: &str) -> Option<AddressMode> {
    match mode_key_lower {
        "implied" => Some(AddressMode::Implied),
        "accumulator" => Some(AddressMode::Accumulator),
        "immediate" => Some(AddressMode::Immediate),
        "zeropage" => Some(AddressMode::ZeroPage),
        "zeropagex" => Some(AddressMode::ZeroPageX),
        "zeropagey" => Some(AddressMode::ZeroPageY),
        "absolute" => Some(AddressMode::Absolute),
        "absolutex" => Some(AddressMode::AbsoluteX),
        "absolutey" => Some(AddressMode::AbsoluteY),
        "indirect" => Some(AddressMode::Indirect),
        "indexedindirectx" => Some(AddressMode::IndexedIndirectX),
        "indirectindexedy" => Some(AddressMode::IndirectIndexedY),
        "indirectindexedz" => Some(AddressMode::IndirectIndexedZ),
        "relative" => Some(AddressMode::Relative),
        "relativelong" => Some(AddressMode::RelativeLong),
        "zeropageindirect" => Some(AddressMode::ZeroPageIndirect),
        "absoluteindexedindirect" => Some(AddressMode::AbsoluteIndexedIndirect),
        "stackrelative" => Some(AddressMode::StackRelative),
        "stackrelativeindirectindexedy" => Some(AddressMode::StackRelativeIndirectIndexedY),
        "absolutelong" => Some(AddressMode::AbsoluteLong),
        "absolutelongx" => Some(AddressMode::AbsoluteLongX),
        "indirectlong" => Some(AddressMode::IndirectLong),
        "directpageindirectlong" => Some(AddressMode::DirectPageIndirectLong),
        "directpageindirectlongy" => Some(AddressMode::DirectPageIndirectLongY),
        "directpageindirectlongz" => Some(AddressMode::DirectPageIndirectLongZ),
        "blockmove" => Some(AddressMode::BlockMove),
        _ => None,
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_mos_style_table_programs<T, I, FFilter, FMnemonic, FMode, FOpcode>(
    entries: I,
    owner: ScopedOwner,
    tables: &mut Vec<VmProgramDescriptor>,
    selectors: &mut Vec<ModeSelectorDescriptor>,
    is_m65816: bool,
    include_m65816_immediate_width_selectors: bool,
    include_m65816_force_selectors_flag: bool,
    include_m65816_long_mode_selectors_flag: bool,
    include_entry: FFilter,
    mnemonic_of: FMnemonic,
    mode_of: FMode,
    opcode_of: FOpcode,
) where
    I: IntoIterator<Item = T>,
    FFilter: Fn(&T) -> bool,
    FMnemonic: Fn(&T) -> &str,
    FMode: Fn(&T) -> AddressMode,
    FOpcode: Fn(&T) -> u8,
{
    for entry in entries {
        if !include_entry(&entry) {
            continue;
        }

        let mnemonic = mnemonic_of(&entry);
        let mode = mode_of(&entry);
        let opcode = opcode_of(&entry);

        tables.push(VmProgramDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            mode_key: format!("{:?}", mode),
            program: compile_opcode_program(opcode, if mode.operand_size() > 0 { 1 } else { 0 }),
        });

        if let Some(selector) = compile_mode_selector(owner.clone(), mnemonic, mode, is_m65816) {
            selectors.push(selector);
        }

        if include_m65816_immediate_width_selectors {
            if let Some(selector) = compile_m65816_immediate_width_selector(mnemonic, mode) {
                selectors.push(selector);
            }
        }
        if include_m65816_force_selectors_flag {
            selectors.extend(compile_m65816_force_selectors(mnemonic, mode));
        }
        if include_m65816_long_mode_selectors_flag {
            selectors.extend(compile_m65816_long_mode_selectors(mnemonic, mode));
        }
    }
}

fn compile_mode_selector(
    owner: ScopedOwner,
    mnemonic: &str,
    mode: AddressMode,
    is_m65816: bool,
) -> Option<ModeSelectorDescriptor> {
    let shape_key = selector_shape_key(mode)?;
    let operand_plan = selector_operand_plan(mode, mnemonic, is_m65816)?;
    Some(ModeSelectorDescriptor {
        owner,
        mnemonic: mnemonic.to_string(),
        shape_key: shape_key.to_string(),
        mode_key: format!("{:?}", mode),
        operand_plan: operand_plan.to_string(),
        priority: selector_priority(mode),
        unstable_widen: matches!(
            mode,
            AddressMode::ZeroPage | AddressMode::ZeroPageX | AddressMode::ZeroPageY
        ),
        width_rank: selector_width_rank(mode),
    })
}

fn compile_m65c02_bit_branch_selectors() -> Vec<ModeSelectorDescriptor> {
    let mut selectors = Vec::with_capacity(16);
    for bit in 0u8..=7 {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(M65C02_CPU_ID.as_str().to_string()),
            mnemonic: format!("BBR{bit}"),
            shape_key: "pair_direct".to_string(),
            mode_key: format!("{:?}", AddressMode::ZeroPage),
            operand_plan: "pair_u8_rel8".to_string(),
            priority: 0,
            unstable_widen: false,
            width_rank: 1,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(M65C02_CPU_ID.as_str().to_string()),
            mnemonic: format!("BBS{bit}"),
            shape_key: "pair_direct".to_string(),
            mode_key: format!("{:?}", AddressMode::ZeroPage),
            operand_plan: "pair_u8_rel8".to_string(),
            priority: 0,
            unstable_widen: false,
            width_rank: 1,
        });
    }
    selectors
}

fn compile_m65816_force_selectors(
    mnemonic: &str,
    mode: AddressMode,
) -> Vec<ModeSelectorDescriptor> {
    let mut selectors = Vec::new();
    let forced_shape_key = match mode {
        AddressMode::AbsoluteLong => "direct",
        AddressMode::AbsoluteLongX => "direct_x",
        other => match selector_shape_key(other) {
            Some(shape_key) => shape_key,
            None => return selectors,
        },
    };
    let upper_mnemonic = mnemonic.to_ascii_uppercase();

    let mut emit = |suffix: &str, operand_plan: &str| {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(M65816_CPU_ID.as_str().to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: format!("{forced_shape_key}:force_{suffix}"),
            mode_key: format!("{:?}", mode),
            operand_plan: operand_plan.to_string(),
            priority: selector_priority(mode),
            unstable_widen: false,
            width_rank: selector_width_rank(mode),
        });
    };

    match mode {
        AddressMode::ZeroPage
        | AddressMode::ZeroPageX
        | AddressMode::ZeroPageY
        | AddressMode::IndexedIndirectX
        | AddressMode::IndirectIndexedY
        | AddressMode::ZeroPageIndirect => emit("d", "force_d_u8"),
        AddressMode::Absolute => {
            if matches!(upper_mnemonic.as_str(), "JMP" | "JSR") {
                emit("k", "force_k_abs16_pbr");
            } else {
                emit("b", "force_b_abs16_dbr");
            }
        }
        AddressMode::AbsoluteX | AddressMode::AbsoluteY => emit("b", "force_b_abs16_dbr"),
        AddressMode::AbsoluteIndexedIndirect => {
            if matches!(upper_mnemonic.as_str(), "JMP" | "JSR") {
                emit("k", "force_k_abs16_pbr");
            }
        }
        AddressMode::Indirect if upper_mnemonic == "JMP" => {
            emit("k", "force_k_abs16_pbr");
        }
        AddressMode::AbsoluteLong | AddressMode::AbsoluteLongX => emit("l", "force_l_u24"),
        _ => {}
    }

    selectors
}

fn compile_m65816_immediate_width_selector(
    mnemonic: &str,
    mode: AddressMode,
) -> Option<ModeSelectorDescriptor> {
    if mode != AddressMode::Immediate || !m65816_immediate_width_mnemonic(mnemonic) {
        return None;
    }
    Some(ModeSelectorDescriptor {
        owner: ScopedOwner::Cpu(M65816_CPU_ID.as_str().to_string()),
        mnemonic: mnemonic.to_string(),
        shape_key: "immediate".to_string(),
        mode_key: format!("{:?}", AddressMode::Immediate),
        operand_plan: "imm_mx".to_string(),
        priority: selector_priority(AddressMode::Immediate),
        unstable_widen: false,
        width_rank: selector_width_rank(AddressMode::Immediate),
    })
}

fn compile_m65816_long_mode_selectors(
    mnemonic: &str,
    mode: AddressMode,
) -> Vec<ModeSelectorDescriptor> {
    let (shape_key, base_mode, base_plan) = match mode {
        AddressMode::AbsoluteLong => (
            "direct",
            AddressMode::Absolute,
            "m65816_abs16_bank_fold_dbr",
        ),
        AddressMode::AbsoluteLongX => (
            "direct_x",
            AddressMode::AbsoluteX,
            "m65816_abs16_bank_fold_dbr",
        ),
        _ => return Vec::new(),
    };
    let has_short_alternative = FAMILY_INSTRUCTION_TABLE
        .iter()
        .any(|entry| entry.mode == base_mode && entry.mnemonic.eq_ignore_ascii_case(mnemonic));
    let long_plan = if has_short_alternative {
        "m65816_long_pref_u24"
    } else {
        "u24"
    };
    let mut selectors = vec![ModeSelectorDescriptor {
        owner: ScopedOwner::Cpu(M65816_CPU_ID.as_str().to_string()),
        mnemonic: mnemonic.to_string(),
        shape_key: shape_key.to_string(),
        mode_key: format!("{:?}", mode),
        operand_plan: long_plan.to_string(),
        priority: selector_priority(mode),
        unstable_widen: false,
        width_rank: selector_width_rank(mode),
    }];

    if has_short_alternative {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(M65816_CPU_ID.as_str().to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: shape_key.to_string(),
            mode_key: format!("{:?}", base_mode),
            operand_plan: base_plan.to_string(),
            priority: selector_priority(base_mode),
            unstable_widen: false,
            width_rank: selector_width_rank(base_mode),
        });
    }

    selectors
}

fn selector_shape_key(mode: AddressMode) -> Option<&'static str> {
    match mode {
        AddressMode::Implied => Some("implied"),
        AddressMode::Accumulator => Some("accumulator"),
        AddressMode::Immediate => Some("immediate"),
        AddressMode::ZeroPage
        | AddressMode::Absolute
        | AddressMode::Relative
        | AddressMode::RelativeLong => Some("direct"),
        AddressMode::ZeroPageX | AddressMode::AbsoluteX => Some("direct_x"),
        AddressMode::ZeroPageY | AddressMode::AbsoluteY => Some("direct_y"),
        AddressMode::IndexedIndirectX | AddressMode::AbsoluteIndexedIndirect => {
            Some("indexed_indirect_x")
        }
        AddressMode::IndirectIndexedY => Some("indirect_indexed_y"),
        AddressMode::IndirectIndexedZ => Some("indirect_indexed_z"),
        AddressMode::Indirect | AddressMode::ZeroPageIndirect => Some("indirect"),
        AddressMode::IndirectLong | AddressMode::DirectPageIndirectLong => Some("indirect_long"),
        AddressMode::DirectPageIndirectLongY => Some("indirect_long_y"),
        AddressMode::DirectPageIndirectLongZ => Some("indirect_long_z"),
        AddressMode::StackRelative => Some("stack_relative"),
        AddressMode::StackRelativeIndirectIndexedY => Some("stack_relative_indirect_y"),
        AddressMode::AbsoluteLong => Some("absolute_long"),
        AddressMode::AbsoluteLongX => Some("absolute_long_x"),
        AddressMode::BlockMove => Some("pair_direct"),
    }
}

fn selector_operand_plan(
    mode: AddressMode,
    mnemonic: &str,
    is_m65816: bool,
) -> Option<&'static str> {
    match mode {
        AddressMode::Implied | AddressMode::Accumulator => Some("none"),
        AddressMode::Immediate => {
            if is_m65816 && m65816_immediate_width_mnemonic(mnemonic) {
                Some("imm_mx")
            } else {
                Some("u8")
            }
        }
        AddressMode::Relative => Some("rel8"),
        AddressMode::RelativeLong => Some("rel16"),
        AddressMode::BlockMove => Some("u8u8_packed"),
        AddressMode::AbsoluteLong | AddressMode::AbsoluteLongX => Some("u24"),
        mode => {
            let size = mode.operand_size();
            match size {
                1 => Some("u8"),
                2 => Some("u16"),
                3 => Some("u24"),
                _ => None,
            }
        }
    }
}

fn selector_priority(mode: AddressMode) -> u16 {
    match mode {
        AddressMode::Relative | AddressMode::RelativeLong => 0,
        AddressMode::ZeroPage
        | AddressMode::ZeroPageX
        | AddressMode::ZeroPageY
        | AddressMode::IndexedIndirectX
        | AddressMode::IndirectIndexedY
        | AddressMode::IndirectIndexedZ
        | AddressMode::ZeroPageIndirect
        | AddressMode::DirectPageIndirectLong
        | AddressMode::DirectPageIndirectLongY
        | AddressMode::DirectPageIndirectLongZ
        | AddressMode::StackRelative
        | AddressMode::StackRelativeIndirectIndexedY => 10,
        AddressMode::Absolute
        | AddressMode::AbsoluteX
        | AddressMode::AbsoluteY
        | AddressMode::Indirect
        | AddressMode::AbsoluteIndexedIndirect
        | AddressMode::IndirectLong => 20,
        AddressMode::AbsoluteLong | AddressMode::AbsoluteLongX => 30,
        AddressMode::BlockMove => 40,
        AddressMode::Implied | AddressMode::Accumulator | AddressMode::Immediate => 0,
    }
}

fn selector_width_rank(mode: AddressMode) -> u8 {
    match mode {
        AddressMode::ZeroPage
        | AddressMode::ZeroPageX
        | AddressMode::ZeroPageY
        | AddressMode::IndexedIndirectX
        | AddressMode::IndirectIndexedY
        | AddressMode::IndirectIndexedZ
        | AddressMode::ZeroPageIndirect
        | AddressMode::DirectPageIndirectLong
        | AddressMode::DirectPageIndirectLongY
        | AddressMode::DirectPageIndirectLongZ
        | AddressMode::StackRelative
        | AddressMode::StackRelativeIndirectIndexedY => 1,
        AddressMode::Absolute
        | AddressMode::AbsoluteX
        | AddressMode::AbsoluteY
        | AddressMode::Indirect
        | AddressMode::AbsoluteIndexedIndirect
        | AddressMode::Relative
        | AddressMode::RelativeLong
        | AddressMode::IndirectLong => 2,
        AddressMode::AbsoluteLong | AddressMode::AbsoluteLongX => 3,
        AddressMode::Implied
        | AddressMode::Accumulator
        | AddressMode::Immediate
        | AddressMode::BlockMove => 0,
    }
}

fn m65816_immediate_width_mnemonic(mnemonic: &str) -> bool {
    matches!(
        mnemonic.to_ascii_uppercase().as_str(),
        "ADC"
            | "AND"
            | "BIT"
            | "CMP"
            | "EOR"
            | "LDA"
            | "ORA"
            | "SBC"
            | "CPX"
            | "CPY"
            | "LDX"
            | "LDY"
    )
}

fn compile_m65c02_bit_branch_programs() -> Vec<VmProgramDescriptor> {
    let mut programs = Vec::with_capacity(16);
    for bit in 0u8..=7 {
        programs.push(VmProgramDescriptor {
            owner: ScopedOwner::Cpu(M65C02_CPU_ID.as_str().to_string()),
            mnemonic: format!("BBR{bit}"),
            mode_key: format!("{:?}", AddressMode::ZeroPage),
            program: compile_opcode_program(m65c02_bit_branch_opcode(bit, false), 2),
        });
        programs.push(VmProgramDescriptor {
            owner: ScopedOwner::Cpu(M65C02_CPU_ID.as_str().to_string()),
            mnemonic: format!("BBS{bit}"),
            mode_key: format!("{:?}", AddressMode::ZeroPage),
            program: compile_opcode_program(m65c02_bit_branch_opcode(bit, true), 2),
        });
    }
    programs
}

fn m65c02_bit_branch_opcode(bit: u8, is_set: bool) -> u8 {
    if is_set {
        0x8F + (bit << 4)
    } else {
        0x0F + (bit << 4)
    }
}

/// Build and encode an `.opasm` container with hierarchy chunks from registry metadata.
///
/// This remains the primary Rust-table-driven authoring path for onboarding
/// new families/CPUs, even when runtime execution consumes loaded package bytes
/// as source of truth.
pub fn build_hierarchy_package_from_registry(
    registry: &ModuleRegistry,
) -> Result<Vec<u8>, HierarchyBuildError> {
    let chunks = build_hierarchy_chunks_from_registry(registry)?;
    encode_hierarchy_chunks_from_chunks(&chunks).map_err(Into::into)
}
