use super::*;

mod scoped_schema;

use scoped_schema::{
    decode_scoped_schema_chunk, decode_simple_schema_chunk, decode_simple_schema_record,
    encode_scoped_schema_chunk, encode_simple_schema_chunk, encode_simple_schema_record,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct TocEntry {
    pub(super) offset: u32,
    pub(super) length: u32,
}

pub(super) fn encode_hierarchy_chunks(
    families: &[FamilyDescriptor],
    cpus: &[CpuDescriptor],
    dialects: &[DialectDescriptor],
    registers: &[ScopedRegisterDescriptor],
    forms: &[ScopedFormDescriptor],
    tables: &[VmProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_hierarchy_chunks_full(families, cpus, dialects, registers, forms, tables, &[])
}

pub(super) fn encode_hierarchy_chunks_full(
    families: &[FamilyDescriptor],
    cpus: &[CpuDescriptor],
    dialects: &[DialectDescriptor],
    registers: &[ScopedRegisterDescriptor],
    forms: &[ScopedFormDescriptor],
    tables: &[VmProgramDescriptor],
    selectors: &[ModeSelectorDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let chunks = HierarchyChunks {
        metadata: PackageMetaDescriptor::default(),
        strings: Vec::new(),
        diagnostics: Vec::new(),
        token_policies: Vec::new(),
        tokenizer_vm_programs: Vec::new(),
        parser_contracts: Vec::new(),
        parser_vm_programs: Vec::new(),
        expr_contracts: Vec::new(),
        expr_parser_contracts: Vec::new(),
        families: families.to_vec(),
        cpus: cpus.to_vec(),
        dialects: dialects.to_vec(),
        registers: registers.to_vec(),
        register_encodings: Vec::new(),
        forms: forms.to_vec(),
        tables: tables.to_vec(),
        semantic_programs: Vec::new(),
        value_programs: Vec::new(),
        operand_record_programs: Vec::new(),
        selector_programs: Vec::new(),
        state_programs: Vec::new(),
        selectors: selectors.to_vec(),
    };
    encode_hierarchy_chunks_from_chunks(&chunks)
}

pub(super) fn encode_hierarchy_chunks_from_chunks(
    chunks: &HierarchyChunks,
) -> Result<Vec<u8>, OpcpuCodecError> {
    // Validate cross references and compatibility before encoding.
    HierarchyPackage::new(
        chunks.families.clone(),
        chunks.cpus.clone(),
        chunks.dialects.clone(),
    )?;

    let metadata = chunks.metadata.clone();
    let mut strings = chunks.strings.to_vec();
    let mut diagnostics = chunks.diagnostics.to_vec();
    let mut token_policies = chunks.token_policies.to_vec();
    let mut tokenizer_vm_programs = chunks.tokenizer_vm_programs.to_vec();
    let mut parser_contracts = chunks.parser_contracts.to_vec();
    let mut parser_vm_programs = chunks.parser_vm_programs.to_vec();
    let mut expr_contracts = chunks.expr_contracts.to_vec();
    let mut expr_parser_contracts = chunks.expr_parser_contracts.to_vec();
    let mut fams = chunks.families.to_vec();
    let mut cpus = chunks.cpus.to_vec();
    let mut dials = chunks.dialects.to_vec();
    let mut regs = chunks.registers.to_vec();
    let mut register_encodings = chunks.register_encodings.to_vec();
    let mut forms = chunks.forms.to_vec();
    let mut tables = chunks.tables.to_vec();
    let mut semantic_programs = chunks.semantic_programs.to_vec();
    let mut value_programs = chunks.value_programs.to_vec();
    let mut operand_record_programs = chunks.operand_record_programs.to_vec();
    let mut selector_programs = chunks.selector_programs.to_vec();
    let mut state_programs = chunks.state_programs.to_vec();
    let mut selectors = chunks.selectors.to_vec();
    canonicalize_hierarchy_metadata(
        &mut fams,
        &mut cpus,
        &mut dials,
        &mut regs,
        &mut forms,
        &mut tables,
        &mut selectors,
    );
    canonicalize_register_encodings(&mut register_encodings);
    validate_register_encoding_set(&register_encodings, &regs)?;
    canonicalize_semantic_programs(&mut semantic_programs);
    validate_semantic_program_set(&semantic_programs)?;
    canonicalize_value_programs(&mut value_programs);
    validate_value_program_set(&value_programs)?;
    canonicalize_operand_record_programs(&mut operand_record_programs);
    validate_operand_record_program_set(&operand_record_programs)?;
    canonicalize_selector_programs(&mut selector_programs);
    validate_selector_program_set(&selector_programs)?;
    canonicalize_state_programs(&mut state_programs);
    validate_state_program_set(&state_programs)?;
    validate_mode_selector_set(&selectors)?;
    canonicalize_token_policies(&mut token_policies);
    canonicalize_tokenizer_vm_programs(&mut tokenizer_vm_programs);
    canonicalize_parser_contracts(&mut parser_contracts);
    canonicalize_parser_vm_programs(&mut parser_vm_programs);
    canonicalize_expr_contracts(&mut expr_contracts);
    canonicalize_expr_parser_contracts(&mut expr_parser_contracts);
    canonicalize_package_support_chunks(&mut strings, &mut diagnostics);

    let mut chunks = vec![
        (CHUNK_META, encode_meta_chunk(&metadata)?),
        (CHUNK_STRS, encode_strs_chunk(&strings)?),
        (CHUNK_DIAG, encode_diag_chunk(&diagnostics)?),
    ];
    if !token_policies.is_empty() {
        chunks.push((CHUNK_TOKS, encode_toks_chunk(&token_policies)?));
    }
    if !tokenizer_vm_programs.is_empty() {
        chunks.push((CHUNK_TKVM, encode_tkvm_chunk(&tokenizer_vm_programs)?));
    }
    if !parser_contracts.is_empty() {
        chunks.push((CHUNK_PARS, encode_pars_chunk(&parser_contracts)?));
    }
    if !parser_vm_programs.is_empty() {
        chunks.push((CHUNK_PRVM, encode_prvm_chunk(&parser_vm_programs)?));
    }
    if !expr_contracts.is_empty() {
        chunks.push((CHUNK_EXPR, encode_expr_chunk(&expr_contracts)?));
    }
    if !expr_parser_contracts.is_empty() {
        chunks.push((CHUNK_EXVM, encode_exvm_chunk(&expr_parser_contracts)?));
    }
    if !semantic_programs.is_empty() {
        let legacy_programs = encode_semv_chunk(&semantic_programs)?;
        let compact_programs = encode_compact_semv_chunk(&semantic_programs)?;
        if legacy_programs.len() >= COMPACT_SEMANTIC_PROGRAM_THRESHOLD_BYTES
            && compact_programs.len() < legacy_programs.len()
        {
            chunks.push((CHUNK_CSEM, compact_programs));
        } else {
            chunks.push((CHUNK_SEMV, legacy_programs));
        }
    }
    if !value_programs.is_empty() {
        chunks.push((CHUNK_VALP, encode_valp_chunk(&value_programs)?));
    }
    if !operand_record_programs.is_empty() {
        let structured_programs_present = semantic_programs
            .iter()
            .any(|program| program.opcode_version == SEMANTIC_VM_OPCODE_VERSION_V3);
        if !structured_programs_present {
            chunks.push((CHUNK_OPRD, encode_oprd_chunk(&operand_record_programs)?));
        } else {
            chunks.push((
                CHUNK_CPRD,
                encode_compact_oprd_chunk(&operand_record_programs)?,
            ));
        }
    }
    if !selector_programs.is_empty() {
        chunks.push((CHUNK_SLCT, encode_slct_chunk(&selector_programs)?));
    }
    if !state_programs.is_empty() {
        chunks.push((CHUNK_STVM, encode_stvm_chunk(&state_programs)?));
    }
    let compact_cpu_aliases = !state_programs.is_empty();
    let cpus_chunk = cpus
        .iter()
        .filter(|cpu| !compact_cpu_aliases || !is_compact_cals_alias(cpu, &cpus))
        .cloned()
        .collect::<Vec<_>>();
    let cpu_aliases = cpus
        .iter()
        .filter(|cpu| compact_cpu_aliases && is_compact_cals_alias(cpu, &cpus))
        .cloned()
        .collect::<Vec<_>>();
    chunks.extend_from_slice(&[
        (CHUNK_FAMS, encode_fams_chunk(&fams)?),
        (CHUNK_CPUS, encode_cpus_chunk(&cpus_chunk)?),
    ]);
    if !cpu_aliases.is_empty() {
        chunks.push((CHUNK_CALS, encode_cals_chunk(&cpus_chunk, &cpu_aliases)?));
    }
    chunks.extend_from_slice(&[
        (CHUNK_DIAL, encode_dial_chunk(&dials)?),
        (CHUNK_REGS, encode_regs_chunk(&regs)?),
    ]);
    if !register_encodings.is_empty() {
        chunks.push((CHUNK_RENC, encode_renc_chunk(&register_encodings)?));
    }
    let legacy_forms = encode_form_chunk(&forms)?;
    let compact_forms = encode_compact_form_chunk(&forms);
    match compact_forms {
        Ok(compact)
            if legacy_forms.len() >= COMPACT_FORM_THRESHOLD_BYTES
                && compact.len() < legacy_forms.len() =>
        {
            chunks.push((CHUNK_CFOR, compact));
        }
        _ => chunks.push((CHUNK_FORM, legacy_forms)),
    }
    let legacy_tables = encode_tabl_chunk(&tables)?;
    let compact_tables = encode_compact_tabl_chunk(&tables);
    match compact_tables {
        Ok(compact)
            if legacy_tables.len() >= COMPACT_TABLE_THRESHOLD_BYTES
                && compact.len() < legacy_tables.len() =>
        {
            chunks.push((CHUNK_CTBL, compact));
        }
        _ => chunks.push((CHUNK_TABL, legacy_tables)),
    }
    let legacy_selectors = encode_msel_chunk(&selectors)?;
    if legacy_selectors.len() >= COMPACT_MODE_SELECTOR_THRESHOLD_BYTES {
        chunks.push((CHUNK_CMSE, encode_compact_msel_chunk(&selectors)?));
    } else {
        chunks.push((CHUNK_MSEL, legacy_selectors));
    }

    encode_container(&chunks)
}

fn is_compact_cals_alias(alias: &CpuDescriptor, cpus: &[CpuDescriptor]) -> bool {
    let Some(canonical_id) = alias.canonical_cpu_id.as_deref() else {
        return false;
    };
    alias.default_dialect.is_none()
        && cpus.iter().any(|canonical| {
            canonical.id.eq_ignore_ascii_case(canonical_id)
                && canonical.canonical_cpu_id.is_none()
                && canonical.family_id.eq_ignore_ascii_case(&alias.family_id)
        })
}

pub(super) fn default_runtime_diagnostic_catalog() -> Vec<DiagnosticDescriptor> {
    vec![
        DiagnosticDescriptor {
            code: DIAG_OPTHREAD_MISSING_VM_PROGRAM.to_string(),
            message_template: "missing VM program for {mnemonic}".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_OPTHREAD_INVALID_FORCE_OVERRIDE.to_string(),
            message_template: "Explicit addressing override ',{force}' is not valid for {context}"
                .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_OPTHREAD_FORCE_UNSUPPORTED_65C02.to_string(),
            message_template: "65816-only addressing mode not supported on 65C02".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_OPTHREAD_FORCE_UNSUPPORTED_6502.to_string(),
            message_template: "65816-only addressing mode not supported on base 6502".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TOKENIZER_INVALID_CHAR.to_string(),
            message_template: "invalid tokenizer character".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TOKENIZER_UNTERMINATED_STRING.to_string(),
            message_template: "unterminated string literal".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TOKENIZER_STEP_LIMIT_EXCEEDED.to_string(),
            message_template: "tokenizer step budget exceeded".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TOKENIZER_TOKEN_LIMIT_EXCEEDED.to_string(),
            message_template: "tokenizer token budget exceeded".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TOKENIZER_LEXEME_LIMIT_EXCEEDED.to_string(),
            message_template: "tokenizer lexeme budget exceeded".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TOKENIZER_ERROR_LIMIT_EXCEEDED.to_string(),
            message_template: "tokenizer diagnostic budget exceeded".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PARSER_UNEXPECTED_TOKEN.to_string(),
            message_template: "unexpected token".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PARSER_EXPECTED_EXPRESSION.to_string(),
            message_template: "expected expression".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PARSER_EXPECTED_OPERAND.to_string(),
            message_template: "expected operand".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PARSER_INVALID_STATEMENT.to_string(),
            message_template: "invalid statement".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ASM_GENERIC_ERRORS_DETECTED.to_string(),
            message_template: "errors detected in source; output not created".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ASM_CLI_ERROR.to_string(),
            message_template: "invalid command-line usage".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ASM_PREPROCESS_ERROR.to_string(),
            message_template: "preprocessor or macro expansion failure".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ASM_CONDITIONAL_STRUCTURE.to_string(),
            message_template: "conditional structure error".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ASM_DIRECTIVE_STRUCTURE.to_string(),
            message_template: "directive structure error".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ASM_SYMBOL_ERROR.to_string(),
            message_template: "symbol resolution error".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ASM_EXPRESSION_ERROR.to_string(),
            message_template: "expression evaluation error".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ASM_INSTRUCTION_ERROR.to_string(),
            message_template: "instruction encoding error".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ASM_IO_ERROR.to_string(),
            message_template: "input/output error".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXPR_INVALID_OPCODE.to_string(),
            message_template: "invalid expression VM opcode".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXPR_STACK_UNDERFLOW.to_string(),
            message_template: "expression VM stack underflow".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXPR_STACK_DEPTH_EXCEEDED.to_string(),
            message_template: "expression VM stack depth exceeded".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXPR_UNKNOWN_SYMBOL.to_string(),
            message_template: "undefined expression symbol".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXPR_EVAL_FAILURE.to_string(),
            message_template: "expression VM evaluation failure".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXPR_UNSUPPORTED_FEATURE.to_string(),
            message_template: "expression VM unsupported feature".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXPR_BUDGET_EXCEEDED.to_string(),
            message_template: "expression VM budget exceeded".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXPR_INVALID_PROGRAM.to_string(),
            message_template: "expression VM program is invalid".to_string(),
        },
    ]
}

pub(super) fn decode_hierarchy_chunks(bytes: &[u8]) -> Result<HierarchyChunks, OpcpuCodecError> {
    let toc = parse_toc(bytes)?;
    let meta_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_META)?;
    let strs_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_STRS)?;
    let diag_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_DIAG)?;
    let toks_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_TOKS)?;
    let tkvm_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_TKVM)?;
    let pars_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_PARS)?;
    let prvm_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_PRVM)?;
    let expr_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_EXPR)?;
    let exvm_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_EXVM)?;
    let semv_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_SEMV)?;
    let csem_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_CSEM)?;
    let valp_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_VALP)?;
    let oprd_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_OPRD)?;
    let cprd_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_CPRD)?;
    let slct_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_SLCT)?;
    let stvm_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_STVM)?;
    if semv_bytes.is_some() && csem_bytes.is_some() {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CSEM".to_string(),
            detail: "package contains both SEMV and CSEM".to_string(),
        });
    }
    if cprd_bytes.is_some() {
        let semantic_programs = match (semv_bytes, csem_bytes) {
            (Some(payload), None) => decode_semv_chunk(payload)?,
            (None, Some(payload)) => decode_compact_semv_chunk(payload)?,
            (None, None) => {
                return Err(OpcpuCodecError::InvalidChunkFormat {
                    chunk: "CPRD".to_string(),
                    detail: "compact operand records require structured semantic programs"
                        .to_string(),
                })
            }
            (Some(_), Some(_)) => unreachable!("duplicate semantic chunks rejected above"),
        };
        if !semantic_programs
            .iter()
            .any(|program| program.opcode_version == SEMANTIC_VM_OPCODE_VERSION_V3)
        {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CPRD".to_string(),
                detail: "compact operand records require a SEMV v3 program".to_string(),
            });
        }
    }
    let fams_bytes = slice_for_chunk(bytes, &toc, CHUNK_FAMS)?;
    let cpus_bytes = slice_for_chunk(bytes, &toc, CHUNK_CPUS)?;
    let cals_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_CALS)?;
    let dial_bytes = slice_for_chunk(bytes, &toc, CHUNK_DIAL)?;
    let regs_bytes = slice_for_chunk(bytes, &toc, CHUNK_REGS)?;
    let renc_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_RENC)?;
    let form_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_FORM)?;
    let cfor_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_CFOR)?;
    if form_bytes.is_some() && cfor_bytes.is_some() {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CFOR".to_string(),
            detail: "package contains both FORM and CFOR".to_string(),
        });
    }
    let tabl_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_TABL)?;
    let ctbl_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_CTBL)?;
    if tabl_bytes.is_some() && ctbl_bytes.is_some() {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CTBL".to_string(),
            detail: "package contains both TABL and CTBL".to_string(),
        });
    }
    let msel_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_MSEL)?;
    let cmse_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_CMSE)?;

    let mut cpus = decode_cpus_chunk(cpus_bytes)?;
    if let Some(payload) = cals_bytes {
        cpus.extend(decode_cals_chunk(payload, &cpus)?);
    }

    let registers = decode_regs_chunk(regs_bytes)?;
    let register_encodings = match renc_bytes {
        Some(payload) => decode_renc_chunk(payload)?,
        None => Vec::new(),
    };
    validate_register_encoding_set(&register_encodings, &registers)?;

    Ok(HierarchyChunks {
        metadata: match meta_bytes {
            Some(payload) => decode_meta_chunk(payload)?,
            None => PackageMetaDescriptor::default(),
        },
        strings: match strs_bytes {
            Some(payload) => decode_strs_chunk(payload)?,
            None => Vec::new(),
        },
        diagnostics: match diag_bytes {
            Some(payload) => decode_diag_chunk(payload)?,
            None => Vec::new(),
        },
        token_policies: match toks_bytes {
            Some(payload) => decode_toks_chunk(payload)?,
            None => Vec::new(),
        },
        tokenizer_vm_programs: match tkvm_bytes {
            Some(payload) => decode_tkvm_chunk(payload)?,
            None => Vec::new(),
        },
        parser_contracts: match pars_bytes {
            Some(payload) => decode_pars_chunk(payload)?,
            None => Vec::new(),
        },
        parser_vm_programs: match prvm_bytes {
            Some(payload) => decode_prvm_chunk(payload)?,
            None => Vec::new(),
        },
        expr_contracts: match expr_bytes {
            Some(payload) => decode_expr_chunk(payload)?,
            None => Vec::new(),
        },
        expr_parser_contracts: match exvm_bytes {
            Some(payload) => decode_exvm_chunk(payload)?,
            None => Vec::new(),
        },
        families: decode_fams_chunk(fams_bytes)?,
        cpus,
        dialects: decode_dial_chunk(dial_bytes)?,
        registers,
        register_encodings,
        forms: match (form_bytes, cfor_bytes) {
            (Some(payload), None) => decode_form_chunk(payload)?,
            (None, Some(payload)) => decode_compact_form_chunk(payload)?,
            (None, None) => {
                return Err(OpcpuCodecError::MissingRequiredChunk {
                    chunk: "FORM".to_string(),
                })
            }
            (Some(_), Some(_)) => unreachable!("duplicate form chunks rejected above"),
        },
        tables: match (tabl_bytes, ctbl_bytes) {
            (Some(payload), None) => decode_tabl_chunk(payload)?,
            (None, Some(payload)) => decode_compact_tabl_chunk(payload)?,
            (None, None) => {
                return Err(OpcpuCodecError::MissingRequiredChunk {
                    chunk: "TABL".to_string(),
                })
            }
            (Some(_), Some(_)) => unreachable!("duplicate table chunks rejected above"),
        },
        semantic_programs: match (semv_bytes, csem_bytes) {
            (Some(payload), None) => decode_semv_chunk(payload)?,
            (None, Some(payload)) => decode_compact_semv_chunk(payload)?,
            (None, None) => Vec::new(),
            (Some(_), Some(_)) => unreachable!("duplicate semantic chunks rejected above"),
        },
        value_programs: match valp_bytes {
            Some(payload) => decode_valp_chunk(payload)?,
            None => Vec::new(),
        },
        operand_record_programs: match (oprd_bytes, cprd_bytes) {
            (Some(_), Some(_)) => {
                return Err(OpcpuCodecError::InvalidChunkFormat {
                    chunk: "CPRD".to_string(),
                    detail: "package contains both OPRD and CPRD".to_string(),
                })
            }
            (Some(payload), None) => decode_oprd_chunk(payload)?,
            (None, Some(payload)) => decode_compact_oprd_chunk(payload)?,
            (None, None) => Vec::new(),
        },
        selector_programs: match slct_bytes {
            Some(payload) => decode_slct_chunk(payload)?,
            None => Vec::new(),
        },
        state_programs: match stvm_bytes {
            Some(payload) => decode_stvm_chunk(payload)?,
            None => Vec::new(),
        },
        selectors: match (msel_bytes, cmse_bytes) {
            (Some(_), Some(_)) => {
                return Err(OpcpuCodecError::InvalidChunkFormat {
                    chunk: "CMSE".to_string(),
                    detail: "package contains both MSEL and CMSE".to_string(),
                })
            }
            (Some(payload), None) => decode_msel_chunk(payload)?,
            (None, Some(payload)) => decode_compact_msel_chunk(payload)?,
            (None, None) => Vec::new(),
        },
    })
}

pub(super) fn load_hierarchy_package(bytes: &[u8]) -> Result<HierarchyPackage, OpcpuCodecError> {
    let decoded = decode_hierarchy_chunks(bytes)?;
    HierarchyPackage::new(decoded.families, decoded.cpus, decoded.dialects).map_err(Into::into)
}

pub(super) fn encode_container(chunks: &[([u8; 4], Vec<u8>)]) -> Result<Vec<u8>, OpcpuCodecError> {
    let toc_count = u16::try_from(chunks.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
        context: "TOC entry count exceeds u16".to_string(),
    })?;

    let header_and_toc_len = HEADER_SIZE
        .checked_add(chunks.len().checked_mul(TOC_ENTRY_SIZE).ok_or_else(|| {
            OpcpuCodecError::CountOutOfRange {
                context: "TOC byte size overflow".to_string(),
            }
        })?)
        .ok_or_else(|| OpcpuCodecError::CountOutOfRange {
            context: "header size overflow".to_string(),
        })?;

    let mut toc_entries = Vec::with_capacity(chunks.len());
    let mut next_offset =
        u32::try_from(header_and_toc_len).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "container header offset exceeds u32".to_string(),
        })?;

    for (tag, payload) in chunks {
        let length =
            u32::try_from(payload.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
                context: format!("chunk '{}' length exceeds u32", chunk_name(tag)),
            })?;
        toc_entries.push((
            *tag,
            TocEntry {
                offset: next_offset,
                length,
            },
        ));
        next_offset =
            next_offset
                .checked_add(length)
                .ok_or_else(|| OpcpuCodecError::CountOutOfRange {
                    context: "container size exceeds u32".to_string(),
                })?;
    }

    let mut out = Vec::new();
    out.extend_from_slice(&OPASM_MAGIC);
    out.extend_from_slice(&OPASM_VERSION_V1.to_le_bytes());
    out.extend_from_slice(&OPASM_ENDIAN_MARKER.to_le_bytes());
    out.extend_from_slice(&toc_count.to_le_bytes());
    out.extend_from_slice(&0u16.to_le_bytes());

    for (tag, entry) in &toc_entries {
        out.extend_from_slice(tag);
        out.extend_from_slice(&entry.offset.to_le_bytes());
        out.extend_from_slice(&entry.length.to_le_bytes());
    }

    for (_, payload) in chunks {
        out.extend_from_slice(payload);
    }

    Ok(out)
}

pub(super) fn parse_toc(bytes: &[u8]) -> Result<HashMap<[u8; 4], TocEntry>, OpcpuCodecError> {
    if bytes.len() < HEADER_SIZE {
        return Err(OpcpuCodecError::UnexpectedEof {
            context: "container header".to_string(),
        });
    }

    let found_magic = [bytes[0], bytes[1], bytes[2], bytes[3]];
    if found_magic != OPASM_MAGIC {
        return Err(OpcpuCodecError::InvalidMagic { found: found_magic });
    }

    let version = u16::from_le_bytes([bytes[4], bytes[5]]);
    if version != OPASM_VERSION_V1 {
        return Err(OpcpuCodecError::UnsupportedVersion { found: version });
    }

    let marker = u16::from_le_bytes([bytes[6], bytes[7]]);
    if marker != OPASM_ENDIAN_MARKER {
        return Err(OpcpuCodecError::InvalidEndiannessMarker { found: marker });
    }

    let toc_count = u16::from_le_bytes([bytes[8], bytes[9]]) as usize;
    let toc_bytes = toc_count
        .checked_mul(TOC_ENTRY_SIZE)
        .and_then(|size| HEADER_SIZE.checked_add(size))
        .ok_or_else(|| OpcpuCodecError::CountOutOfRange {
            context: "TOC length overflow".to_string(),
        })?;

    if bytes.len() < toc_bytes {
        return Err(OpcpuCodecError::UnexpectedEof {
            context: "TOC entries".to_string(),
        });
    }

    let mut toc = HashMap::new();
    for idx in 0..toc_count {
        let start = HEADER_SIZE + idx * TOC_ENTRY_SIZE;
        let tag = [
            bytes[start],
            bytes[start + 1],
            bytes[start + 2],
            bytes[start + 3],
        ];
        let offset = u32::from_le_bytes([
            bytes[start + 4],
            bytes[start + 5],
            bytes[start + 6],
            bytes[start + 7],
        ]);
        let length = u32::from_le_bytes([
            bytes[start + 8],
            bytes[start + 9],
            bytes[start + 10],
            bytes[start + 11],
        ]);

        if toc.contains_key(&tag) {
            return Err(OpcpuCodecError::DuplicateChunk {
                chunk: chunk_name(&tag),
            });
        }

        let start_usize =
            usize::try_from(offset).map_err(|_| OpcpuCodecError::ChunkOutOfBounds {
                chunk: chunk_name(&tag),
                offset,
                length,
                file_len: bytes.len(),
            })?;
        let len_usize = usize::try_from(length).map_err(|_| OpcpuCodecError::ChunkOutOfBounds {
            chunk: chunk_name(&tag),
            offset,
            length,
            file_len: bytes.len(),
        })?;
        let end = start_usize.checked_add(len_usize).ok_or_else(|| {
            OpcpuCodecError::ChunkOutOfBounds {
                chunk: chunk_name(&tag),
                offset,
                length,
                file_len: bytes.len(),
            }
        })?;
        if end > bytes.len() {
            return Err(OpcpuCodecError::ChunkOutOfBounds {
                chunk: chunk_name(&tag),
                offset,
                length,
                file_len: bytes.len(),
            });
        }

        toc.insert(tag, TocEntry { offset, length });
    }

    Ok(toc)
}

pub(super) fn slice_for_chunk<'a>(
    bytes: &'a [u8],
    toc: &HashMap<[u8; 4], TocEntry>,
    tag: [u8; 4],
) -> Result<&'a [u8], OpcpuCodecError> {
    let entry = toc
        .get(&tag)
        .ok_or_else(|| OpcpuCodecError::MissingRequiredChunk {
            chunk: chunk_name(&tag),
        })?;
    let start = usize::try_from(entry.offset).map_err(|_| OpcpuCodecError::ChunkOutOfBounds {
        chunk: chunk_name(&tag),
        offset: entry.offset,
        length: entry.length,
        file_len: bytes.len(),
    })?;
    let len = usize::try_from(entry.length).map_err(|_| OpcpuCodecError::ChunkOutOfBounds {
        chunk: chunk_name(&tag),
        offset: entry.offset,
        length: entry.length,
        file_len: bytes.len(),
    })?;
    let end = start
        .checked_add(len)
        .ok_or_else(|| OpcpuCodecError::ChunkOutOfBounds {
            chunk: chunk_name(&tag),
            offset: entry.offset,
            length: entry.length,
            file_len: bytes.len(),
        })?;
    bytes
        .get(start..end)
        .ok_or_else(|| OpcpuCodecError::ChunkOutOfBounds {
            chunk: chunk_name(&tag),
            offset: entry.offset,
            length: entry.length,
            file_len: bytes.len(),
        })
}

pub(super) fn slice_for_chunk_optional<'a>(
    bytes: &'a [u8],
    toc: &HashMap<[u8; 4], TocEntry>,
    tag: [u8; 4],
) -> Result<Option<&'a [u8]>, OpcpuCodecError> {
    let Some(entry) = toc.get(&tag) else {
        return Ok(None);
    };
    let start = usize::try_from(entry.offset).map_err(|_| OpcpuCodecError::ChunkOutOfBounds {
        chunk: chunk_name(&tag),
        offset: entry.offset,
        length: entry.length,
        file_len: bytes.len(),
    })?;
    let len = usize::try_from(entry.length).map_err(|_| OpcpuCodecError::ChunkOutOfBounds {
        chunk: chunk_name(&tag),
        offset: entry.offset,
        length: entry.length,
        file_len: bytes.len(),
    })?;
    let end = start
        .checked_add(len)
        .ok_or_else(|| OpcpuCodecError::ChunkOutOfBounds {
            chunk: chunk_name(&tag),
            offset: entry.offset,
            length: entry.length,
            file_len: bytes.len(),
        })?;
    bytes
        .get(start..end)
        .map(Some)
        .ok_or_else(|| OpcpuCodecError::ChunkOutOfBounds {
            chunk: chunk_name(&tag),
            offset: entry.offset,
            length: entry.length,
            file_len: bytes.len(),
        })
}

pub(super) fn encode_fams_chunk(families: &[FamilyDescriptor]) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_simple_schema_chunk(families)
}

pub(super) fn decode_fams_chunk(bytes: &[u8]) -> Result<Vec<FamilyDescriptor>, OpcpuCodecError> {
    decode_simple_schema_chunk(bytes)
}

pub(super) fn encode_meta_chunk(
    metadata: &PackageMetaDescriptor,
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_simple_schema_record(metadata)
}

pub(super) fn decode_meta_chunk(bytes: &[u8]) -> Result<PackageMetaDescriptor, OpcpuCodecError> {
    decode_simple_schema_record(bytes)
}

pub(super) fn encode_strs_chunk(strings: &[String]) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_simple_schema_chunk(strings)
}

pub(super) fn decode_strs_chunk(bytes: &[u8]) -> Result<Vec<String>, OpcpuCodecError> {
    decode_simple_schema_chunk(bytes)
}

pub(super) fn encode_diag_chunk(
    diagnostics: &[DiagnosticDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_simple_schema_chunk(diagnostics)
}

pub(super) fn decode_diag_chunk(
    bytes: &[u8],
) -> Result<Vec<DiagnosticDescriptor>, OpcpuCodecError> {
    decode_simple_schema_chunk(bytes)
}

pub(super) fn encode_scoped_owner(
    out: &mut Vec<u8>,
    chunk: &str,
    owner: &ScopedOwner,
) -> Result<(), OpcpuCodecError> {
    out.push(owner.owner_tag());
    write_string(out, chunk, owner.owner_id())
}

pub(super) fn decode_scoped_owner(
    cur: &mut Decoder<'_>,
    chunk: &'static str,
) -> Result<ScopedOwner, OpcpuCodecError> {
    let owner_tag = cur.read_u8()?;
    let owner_id = cur.read_string()?;
    ScopedOwner::from_owner_tag(owner_tag, owner_id).ok_or_else(|| {
        OpcpuCodecError::InvalidChunkFormat {
            chunk: chunk.to_string(),
            detail: format!("invalid owner tag: {}", owner_tag),
        }
    })
}

pub(super) fn encode_toks_chunk(
    policies: &[TokenPolicyDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(policies)
}

pub(super) fn decode_toks_chunk(
    bytes: &[u8],
) -> Result<Vec<TokenPolicyDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
}

pub(super) fn encode_cpus_chunk(cpus: &[CpuDescriptor]) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_simple_schema_chunk(cpus)
}

pub(super) fn decode_cpus_chunk(bytes: &[u8]) -> Result<Vec<CpuDescriptor>, OpcpuCodecError> {
    decode_simple_schema_chunk(bytes)
}

pub(super) fn encode_cals_chunk(
    canonical_cpus: &[CpuDescriptor],
    aliases: &[CpuDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut out = Vec::new();
    write_u32(&mut out, u32_count(aliases.len(), "CALS alias count")?);
    for alias in aliases {
        let canonical_id = alias.canonical_cpu_id.as_deref().ok_or_else(|| {
            OpcpuCodecError::InvalidChunkFormat {
                chunk: "CALS".to_string(),
                detail: format!("CPU alias '{}' has no canonical target", alias.id),
            }
        })?;
        let index = canonical_cpus
            .iter()
            .position(|cpu| cpu.id.eq_ignore_ascii_case(canonical_id))
            .ok_or_else(|| OpcpuCodecError::InvalidChunkFormat {
                chunk: "CALS".to_string(),
                detail: format!(
                    "CPU alias '{}' references unknown canonical CPU '{canonical_id}'",
                    alias.id
                ),
            })?;
        write_string(&mut out, "CALS", &alias.id)?;
        write_u16(
            &mut out,
            u16::try_from(index).map_err(|_| OpcpuCodecError::CountOutOfRange {
                context: "CALS canonical CPU index exceeds u16".to_string(),
            })?,
        );
    }
    Ok(out)
}

pub(super) fn decode_cals_chunk(
    bytes: &[u8],
    canonical_cpus: &[CpuDescriptor],
) -> Result<Vec<CpuDescriptor>, OpcpuCodecError> {
    let mut cur = Decoder::new(bytes, "CALS");
    let count = cur.read_u32()? as usize;
    if count > MAX_DECODE_ENTRY_COUNT {
        return Err(OpcpuCodecError::CountOutOfRange {
            context: "CALS alias count exceeds hard limit".to_string(),
        });
    }
    let mut aliases = Vec::with_capacity(count);
    let mut seen = std::collections::HashSet::new();
    for _ in 0..count {
        let id = cur.read_string()?;
        let index = cur.read_u16()? as usize;
        let canonical =
            canonical_cpus
                .get(index)
                .ok_or_else(|| OpcpuCodecError::InvalidChunkFormat {
                    chunk: "CALS".to_string(),
                    detail: format!("canonical CPU index {index} is out of range"),
                })?;
        if canonical.canonical_cpu_id.is_some() {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CALS".to_string(),
                detail: format!("CPU alias '{id}' references another alias"),
            });
        }
        if id.is_empty()
            || id.eq_ignore_ascii_case(&canonical.id)
            || !seen.insert(id.to_ascii_lowercase())
            || canonical_cpus
                .iter()
                .any(|cpu| cpu.id.eq_ignore_ascii_case(&id))
        {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CALS".to_string(),
                detail: format!("invalid or duplicate CPU alias '{id}'"),
            });
        }
        aliases.push(CpuDescriptor {
            id,
            family_id: canonical.family_id.clone(),
            default_dialect: None,
            canonical_cpu_id: Some(canonical.id.clone()),
        });
    }
    cur.finish()?;
    Ok(aliases)
}

pub(super) fn encode_dial_chunk(
    dialects: &[DialectDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_simple_schema_chunk(dialects)
}

pub(super) fn decode_dial_chunk(bytes: &[u8]) -> Result<Vec<DialectDescriptor>, OpcpuCodecError> {
    decode_simple_schema_chunk(bytes)
}

pub(super) fn encode_regs_chunk(
    registers: &[ScopedRegisterDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(registers)
}

pub(super) fn decode_regs_chunk(
    bytes: &[u8],
) -> Result<Vec<ScopedRegisterDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
}

pub(super) fn encode_renc_chunk(
    entries: &[RegisterEncodingDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut out = Vec::new();
    write_u16(&mut out, REGISTER_ENCODING_CHUNK_VERSION_V1);
    out.extend_from_slice(&encode_scoped_schema_chunk(entries)?);
    Ok(out)
}

pub(super) fn decode_renc_chunk(
    bytes: &[u8],
) -> Result<Vec<RegisterEncodingDescriptor>, OpcpuCodecError> {
    let Some(version_bytes) = bytes.get(..2) else {
        return Err(OpcpuCodecError::UnexpectedEof {
            context: "chunk RENC version".to_string(),
        });
    };
    let version = u16::from_le_bytes([version_bytes[0], version_bytes[1]]);
    if version != REGISTER_ENCODING_CHUNK_VERSION_V1 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "RENC".to_string(),
            detail: format!("unsupported register encoding chunk version {version}"),
        });
    }
    decode_scoped_schema_chunk(&bytes[2..])
}

fn validate_register_encoding_set(
    entries: &[RegisterEncodingDescriptor],
    registers: &[ScopedRegisterDescriptor],
) -> Result<(), OpcpuCodecError> {
    for (index, entry) in entries.iter().enumerate() {
        if entries[..index].iter().any(|prior| {
            prior.owner.key_parts_lowercase() == entry.owner.key_parts_lowercase()
                && prior.id.eq_ignore_ascii_case(&entry.id)
        }) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "RENC".to_string(),
                detail: format!(
                    "duplicate register encoding '{}' in one owner scope",
                    entry.id
                ),
            });
        }
        if !registers.iter().any(|register| {
            register.owner.key_parts_lowercase() == entry.owner.key_parts_lowercase()
                && register.id.eq_ignore_ascii_case(&entry.id)
        }) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "RENC".to_string(),
                detail: format!(
                    "register encoding '{}' has no matching REGS descriptor in its owner scope",
                    entry.id
                ),
            });
        }
    }
    Ok(())
}

pub(super) fn encode_form_chunk(
    forms: &[ScopedFormDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(forms)
}

pub(super) fn decode_form_chunk(
    bytes: &[u8],
) -> Result<Vec<ScopedFormDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
}

pub(super) fn encode_compact_form_chunk(
    forms: &[ScopedFormDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut owners = Vec::<ScopedOwner>::new();
    let mut strings = Vec::<String>::new();
    for form in forms {
        if !owners.contains(&form.owner) {
            owners.push(form.owner.clone());
        }
        if !strings.contains(&form.mnemonic) {
            strings.push(form.mnemonic.clone());
        }
    }
    strings.sort();
    let owner_count =
        u16::try_from(owners.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "CFOR owner count exceeds u16".to_string(),
        })?;
    let string_count =
        u16::try_from(strings.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "CFOR string count exceeds u16".to_string(),
        })?;

    let mut out = Vec::new();
    write_u16(&mut out, COMPACT_FORM_CHUNK_VERSION_V1);
    write_u16(&mut out, owner_count);
    for owner in &owners {
        encode_scoped_owner(&mut out, "CFOR", owner)?;
    }
    write_u16(&mut out, string_count);
    let mut previous = "";
    for value in &strings {
        let mut prefix_len = previous
            .as_bytes()
            .iter()
            .zip(value.as_bytes())
            .take_while(|(left, right)| left == right)
            .count();
        while !value.is_char_boundary(prefix_len) || !previous.is_char_boundary(prefix_len) {
            prefix_len -= 1;
        }
        write_u16(
            &mut out,
            u16::try_from(prefix_len).map_err(|_| OpcpuCodecError::CountOutOfRange {
                context: "CFOR string prefix length exceeds u16".to_string(),
            })?,
        );
        write_string(&mut out, "CFOR", &value[prefix_len..])?;
        previous = value;
    }
    write_u32(&mut out, u32_count(forms.len(), "CFOR form count")?);
    for form in forms {
        let owner_index = owners
            .iter()
            .position(|owner| owner == &form.owner)
            .expect("CFOR owner table is built from forms");
        let mnemonic_index = strings
            .iter()
            .position(|mnemonic| mnemonic == &form.mnemonic)
            .expect("CFOR string table is built from forms");
        write_u16(
            &mut out,
            u16::try_from(owner_index).expect("CFOR owner index fits its u16 count"),
        );
        write_u16(
            &mut out,
            u16::try_from(mnemonic_index).expect("CFOR string index fits its u16 count"),
        );
    }
    Ok(out)
}

pub(super) fn decode_compact_form_chunk(
    bytes: &[u8],
) -> Result<Vec<ScopedFormDescriptor>, OpcpuCodecError> {
    let mut cur = Decoder::new(bytes, "CFOR");
    let version = cur.read_u16()?;
    if version != COMPACT_FORM_CHUNK_VERSION_V1 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CFOR".to_string(),
            detail: format!("unsupported compact form version {version}"),
        });
    }
    let owner_count = cur.read_u16()? as usize;
    let mut owners = Vec::with_capacity(owner_count);
    for _ in 0..owner_count {
        let owner = decode_scoped_owner(&mut cur, "CFOR")?;
        if owners.contains(&owner) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CFOR".to_string(),
                detail: "duplicate owner table entry".to_string(),
            });
        }
        owners.push(owner);
    }
    let string_count = cur.read_u16()? as usize;
    let mut strings = Vec::<String>::with_capacity(string_count);
    for _ in 0..string_count {
        let prefix_len = cur.read_u16()? as usize;
        let suffix = cur.read_string()?;
        let previous = strings.last().map_or("", String::as_str);
        if prefix_len > previous.len() || !previous.is_char_boundary(prefix_len) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CFOR".to_string(),
                detail: format!("string prefix length {prefix_len} is out of range"),
            });
        }
        let value = format!("{}{suffix}", &previous[..prefix_len]);
        if strings.contains(&value) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CFOR".to_string(),
                detail: "duplicate string table entry".to_string(),
            });
        }
        strings.push(value);
    }
    let form_count = cur.read_u32()? as usize;
    if form_count > MAX_DECODE_ENTRY_COUNT {
        return Err(OpcpuCodecError::CountOutOfRange {
            context: "CFOR form count exceeds hard limit".to_string(),
        });
    }
    let mut forms = Vec::with_capacity(form_count);
    for _ in 0..form_count {
        let owner_index = cur.read_u16()? as usize;
        let mnemonic_index = cur.read_u16()? as usize;
        let owner = owners.get(owner_index).cloned().ok_or_else(|| {
            OpcpuCodecError::InvalidChunkFormat {
                chunk: "CFOR".to_string(),
                detail: format!("owner index {owner_index} is out of range"),
            }
        })?;
        let mnemonic = strings.get(mnemonic_index).cloned().ok_or_else(|| {
            OpcpuCodecError::InvalidChunkFormat {
                chunk: "CFOR".to_string(),
                detail: format!("string index {mnemonic_index} is out of range"),
            }
        })?;
        forms.push(ScopedFormDescriptor { owner, mnemonic });
    }
    cur.finish()?;
    Ok(forms)
}

pub(super) fn encode_tabl_chunk(
    tables: &[VmProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(tables)
}

pub(super) fn decode_tabl_chunk(bytes: &[u8]) -> Result<Vec<VmProgramDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
}

pub(super) fn encode_compact_tabl_chunk(
    tables: &[VmProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut owners = Vec::<ScopedOwner>::new();
    let mut strings = Vec::<String>::new();
    let mut programs = Vec::<Vec<u8>>::new();
    for table in tables {
        if !owners.contains(&table.owner) {
            owners.push(table.owner.clone());
        }
        if !strings.contains(&table.mnemonic) {
            strings.push(table.mnemonic.clone());
        }
        if !strings.contains(&table.mode_key) {
            strings.push(table.mode_key.clone());
        }
        if !programs.contains(&table.program) {
            programs.push(table.program.clone());
        }
    }
    strings.sort();
    let owner_count =
        u16::try_from(owners.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "CTBL owner count exceeds u16".to_string(),
        })?;
    let string_count =
        u16::try_from(strings.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "CTBL string count exceeds u16".to_string(),
        })?;
    let program_count =
        u16::try_from(programs.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "CTBL program count exceeds u16".to_string(),
        })?;

    let mut out = Vec::new();
    write_u16(&mut out, COMPACT_TABLE_CHUNK_VERSION_V1);
    write_u16(&mut out, owner_count);
    for owner in &owners {
        encode_scoped_owner(&mut out, "CTBL", owner)?;
    }
    write_u16(&mut out, string_count);
    let mut previous = "";
    for value in &strings {
        let mut prefix_len = previous
            .as_bytes()
            .iter()
            .zip(value.as_bytes())
            .take_while(|(left, right)| left == right)
            .count();
        while !value.is_char_boundary(prefix_len) || !previous.is_char_boundary(prefix_len) {
            prefix_len -= 1;
        }
        write_u16(
            &mut out,
            u16::try_from(prefix_len).map_err(|_| OpcpuCodecError::CountOutOfRange {
                context: "CTBL string prefix length exceeds u16".to_string(),
            })?,
        );
        write_string(&mut out, "CTBL", &value[prefix_len..])?;
        previous = value;
    }
    write_u16(&mut out, program_count);
    for program in &programs {
        write_u32(
            &mut out,
            u32_count(program.len(), "CTBL program byte length")?,
        );
        out.extend_from_slice(program);
    }
    write_u32(&mut out, u32_count(tables.len(), "CTBL table count")?);
    for table in tables {
        let owner_index = owners
            .iter()
            .position(|owner| owner == &table.owner)
            .expect("CTBL owner table is built from table entries");
        let mnemonic_index = strings
            .iter()
            .position(|value| value == &table.mnemonic)
            .expect("CTBL string table includes every mnemonic");
        let mode_key_index = strings
            .iter()
            .position(|value| value == &table.mode_key)
            .expect("CTBL string table includes every mode key");
        let program_index = programs
            .iter()
            .position(|program| program == &table.program)
            .expect("CTBL program table includes every program");
        for index in [owner_index, mnemonic_index, mode_key_index, program_index] {
            write_u16(
                &mut out,
                u16::try_from(index).expect("CTBL index fits its u16 table count"),
            );
        }
    }
    Ok(out)
}

pub(super) fn decode_compact_tabl_chunk(
    bytes: &[u8],
) -> Result<Vec<VmProgramDescriptor>, OpcpuCodecError> {
    let mut cur = Decoder::new(bytes, "CTBL");
    let version = cur.read_u16()?;
    if version != COMPACT_TABLE_CHUNK_VERSION_V1 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CTBL".to_string(),
            detail: format!("unsupported compact table version {version}"),
        });
    }
    let owner_count = cur.read_u16()? as usize;
    let mut owners = Vec::with_capacity(owner_count);
    for _ in 0..owner_count {
        let owner = decode_scoped_owner(&mut cur, "CTBL")?;
        if owners.contains(&owner) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CTBL".to_string(),
                detail: "duplicate owner table entry".to_string(),
            });
        }
        owners.push(owner);
    }
    let string_count = cur.read_u16()? as usize;
    let mut strings = Vec::<String>::with_capacity(string_count);
    for _ in 0..string_count {
        let prefix_len = cur.read_u16()? as usize;
        let suffix = cur.read_string()?;
        let previous = strings.last().map_or("", String::as_str);
        if prefix_len > previous.len() || !previous.is_char_boundary(prefix_len) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CTBL".to_string(),
                detail: format!("string prefix length {prefix_len} is out of range"),
            });
        }
        let value = format!("{}{suffix}", &previous[..prefix_len]);
        if strings.contains(&value) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CTBL".to_string(),
                detail: "duplicate string table entry".to_string(),
            });
        }
        strings.push(value);
    }
    let program_count = cur.read_u16()? as usize;
    let mut programs = Vec::<Vec<u8>>::with_capacity(program_count);
    for _ in 0..program_count {
        let len = cur.read_u32()? as usize;
        let program = cur.read_exact(len, "program bytes")?.to_vec();
        if programs.contains(&program) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CTBL".to_string(),
                detail: "duplicate program table entry".to_string(),
            });
        }
        programs.push(program);
    }
    let table_count = cur.read_u32()? as usize;
    if table_count > MAX_DECODE_ENTRY_COUNT {
        return Err(OpcpuCodecError::CountOutOfRange {
            context: "CTBL table count exceeds hard limit".to_string(),
        });
    }
    let mut tables = Vec::with_capacity(table_count);
    for _ in 0..table_count {
        let owner_index = cur.read_u16()? as usize;
        let mnemonic_index = cur.read_u16()? as usize;
        let mode_key_index = cur.read_u16()? as usize;
        let program_index = cur.read_u16()? as usize;
        let owner = owners.get(owner_index).cloned().ok_or_else(|| {
            OpcpuCodecError::InvalidChunkFormat {
                chunk: "CTBL".to_string(),
                detail: format!("owner index {owner_index} is out of range"),
            }
        })?;
        let mnemonic = strings.get(mnemonic_index).cloned().ok_or_else(|| {
            OpcpuCodecError::InvalidChunkFormat {
                chunk: "CTBL".to_string(),
                detail: format!("mnemonic index {mnemonic_index} is out of range"),
            }
        })?;
        let mode_key = strings.get(mode_key_index).cloned().ok_or_else(|| {
            OpcpuCodecError::InvalidChunkFormat {
                chunk: "CTBL".to_string(),
                detail: format!("mode-key index {mode_key_index} is out of range"),
            }
        })?;
        let program = programs.get(program_index).cloned().ok_or_else(|| {
            OpcpuCodecError::InvalidChunkFormat {
                chunk: "CTBL".to_string(),
                detail: format!("program index {program_index} is out of range"),
            }
        })?;
        tables.push(VmProgramDescriptor {
            owner,
            mnemonic,
            mode_key,
            program,
        });
    }
    cur.finish()?;
    Ok(tables)
}

pub(super) fn encode_semv_chunk(
    programs: &[SemanticProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(programs)
}

pub(super) fn decode_semv_chunk(
    bytes: &[u8],
) -> Result<Vec<SemanticProgramDescriptor>, OpcpuCodecError> {
    let programs = decode_scoped_schema_chunk(bytes)?;
    validate_semantic_program_set(&programs)?;
    Ok(programs)
}

pub(super) fn encode_compact_semv_chunk(
    programs: &[SemanticProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut owners = Vec::<ScopedOwner>::new();
    for program in programs {
        if !owners.contains(&program.owner) {
            owners.push(program.owner.clone());
        }
    }
    let owner_count =
        u16::try_from(owners.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "CSEM owner count exceeds u16".to_string(),
        })?;
    let mut out = Vec::new();
    write_u16(&mut out, COMPACT_SEMANTIC_PROGRAM_CHUNK_VERSION_V1);
    write_u16(&mut out, owner_count);
    for owner in &owners {
        encode_scoped_owner(&mut out, "CSEM", owner)?;
    }
    write_u32(&mut out, u32_count(programs.len(), "CSEM program count")?);
    for program in programs {
        let owner_index = owners
            .iter()
            .position(|owner| owner == &program.owner)
            .expect("CSEM owner table is built from programs");
        write_u16(
            &mut out,
            u16::try_from(owner_index).expect("CSEM owner index fits its u16 count"),
        );
        write_string(&mut out, "CSEM", &program.id)?;
        write_u16(&mut out, program.opcode_version);
        write_u32(
            &mut out,
            u32_count(program.program.len(), "CSEM program byte length")?,
        );
        out.extend_from_slice(&program.program);
    }
    Ok(out)
}

pub(super) fn decode_compact_semv_chunk(
    bytes: &[u8],
) -> Result<Vec<SemanticProgramDescriptor>, OpcpuCodecError> {
    let mut cur = Decoder::new(bytes, "CSEM");
    let version = cur.read_u16()?;
    if version != COMPACT_SEMANTIC_PROGRAM_CHUNK_VERSION_V1 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CSEM".to_string(),
            detail: format!("unsupported compact semantic-program version {version}"),
        });
    }
    let owner_count = cur.read_u16()? as usize;
    let mut owners = Vec::with_capacity(owner_count);
    for _ in 0..owner_count {
        let owner = decode_scoped_owner(&mut cur, "CSEM")?;
        if owners.contains(&owner) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CSEM".to_string(),
                detail: "duplicate owner table entry".to_string(),
            });
        }
        owners.push(owner);
    }
    let count = cur.read_u32()? as usize;
    if count > MAX_DECODE_ENTRY_COUNT {
        return Err(OpcpuCodecError::CountOutOfRange {
            context: "CSEM program count exceeds hard limit".to_string(),
        });
    }
    let mut programs = Vec::with_capacity(count);
    for _ in 0..count {
        let owner_index = cur.read_u16()? as usize;
        let owner = owners.get(owner_index).cloned().ok_or_else(|| {
            OpcpuCodecError::InvalidChunkFormat {
                chunk: "CSEM".to_string(),
                detail: format!("owner index {owner_index} is out of range"),
            }
        })?;
        let id = cur.read_string()?;
        let opcode_version = cur.read_u16()?;
        let length = cur.read_u32()? as usize;
        let program = cur.read_exact(length, "semantic program bytes")?.to_vec();
        programs.push(SemanticProgramDescriptor {
            owner,
            id,
            opcode_version,
            program,
        });
    }
    cur.finish()?;
    validate_semantic_program_set(&programs)?;
    Ok(programs)
}

fn validate_semantic_program_set(
    programs: &[SemanticProgramDescriptor],
) -> Result<(), OpcpuCodecError> {
    for (index, entry) in programs.iter().enumerate() {
        if entry.id.is_empty() {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "SEMV".to_string(),
                detail: "semantic VM program id must not be empty".to_string(),
            });
        }
        validate_semantic_program(entry.opcode_version, &entry.program)?;
        if programs[..index].iter().any(|prior| {
            prior.owner.key_parts_lowercase() == entry.owner.key_parts_lowercase()
                && prior.id.eq_ignore_ascii_case(&entry.id)
        }) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "SEMV".to_string(),
                detail: format!(
                    "duplicate semantic VM program id '{}' in one owner scope",
                    entry.id
                ),
            });
        }
    }
    Ok(())
}

pub(super) fn encode_valp_chunk(
    programs: &[ValueProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(programs)
}

pub(super) fn decode_valp_chunk(
    bytes: &[u8],
) -> Result<Vec<ValueProgramDescriptor>, OpcpuCodecError> {
    let programs = decode_scoped_schema_chunk(bytes)?;
    validate_value_program_set(&programs)?;
    Ok(programs)
}

fn validate_value_program_set(programs: &[ValueProgramDescriptor]) -> Result<(), OpcpuCodecError> {
    for (index, entry) in programs.iter().enumerate() {
        if entry.id.is_empty() {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "VALP".to_string(),
                detail: "value VM program id must not be empty".to_string(),
            });
        }
        validate_value_program(entry.opcode_version, &entry.program)?;
        if programs[..index].iter().any(|prior| {
            prior.owner.key_parts_lowercase() == entry.owner.key_parts_lowercase()
                && prior.id.eq_ignore_ascii_case(&entry.id)
        }) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "VALP".to_string(),
                detail: format!(
                    "duplicate value VM program id '{}' in one owner scope",
                    entry.id
                ),
            });
        }
    }
    Ok(())
}

pub(super) fn encode_oprd_chunk(
    programs: &[OperandRecordProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(programs)
}

pub(super) fn decode_oprd_chunk(
    bytes: &[u8],
) -> Result<Vec<OperandRecordProgramDescriptor>, OpcpuCodecError> {
    let programs = decode_scoped_schema_chunk(bytes)?;
    validate_operand_record_program_set(&programs)?;
    Ok(programs)
}

pub(super) fn encode_compact_oprd_chunk(
    programs: &[OperandRecordProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut owners: Vec<ScopedOwner> = Vec::new();
    for program in programs {
        if !owners
            .iter()
            .any(|owner| owner.key_parts_lowercase() == program.owner.key_parts_lowercase())
        {
            owners.push(program.owner.clone());
        }
    }
    let owner_count =
        u16::try_from(owners.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "CPRD owner count exceeds u16".to_string(),
        })?;
    let mut out = Vec::new();
    write_u16(&mut out, COMPACT_OPERAND_RECORD_CHUNK_VERSION_V1);
    write_u16(&mut out, owner_count);
    for owner in &owners {
        encode_scoped_owner(&mut out, "CPRD", owner)?;
    }
    write_u32(&mut out, u32_count(programs.len(), "CPRD program count")?);
    for program in programs {
        let owner_index = owners
            .iter()
            .position(|owner| owner.key_parts_lowercase() == program.owner.key_parts_lowercase())
            .expect("owner table was built from every program");
        write_u16(
            &mut out,
            u16::try_from(owner_index).expect("owner count already fits in u16"),
        );
        write_string(&mut out, "CPRD", &program.id)?;
        write_u16(&mut out, program.schema_version);
        write_u32(
            &mut out,
            u32_count(program.program.len(), "CPRD program byte length")?,
        );
        out.extend_from_slice(&program.program);
    }
    Ok(out)
}

pub(super) fn decode_compact_oprd_chunk(
    bytes: &[u8],
) -> Result<Vec<OperandRecordProgramDescriptor>, OpcpuCodecError> {
    let mut cur = Decoder::new(bytes, "CPRD");
    let version = cur.read_u16()?;
    if version != COMPACT_OPERAND_RECORD_CHUNK_VERSION_V1 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CPRD".to_string(),
            detail: format!("unsupported compact operand-record chunk version {version}"),
        });
    }
    let owner_count = cur.read_u16()? as usize;
    if owner_count == 0 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CPRD".to_string(),
            detail: "compact owner table must not be empty".to_string(),
        });
    }
    let mut owners = Vec::with_capacity(owner_count);
    for _ in 0..owner_count {
        let owner = decode_scoped_owner(&mut cur, "CPRD")?;
        if owners
            .iter()
            .any(|prior: &ScopedOwner| prior.key_parts_lowercase() == owner.key_parts_lowercase())
        {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CPRD".to_string(),
                detail: "compact owner table contains a duplicate owner".to_string(),
            });
        }
        owners.push(owner);
    }
    let count = cur.read_u32()? as usize;
    if count > MAX_DECODE_ENTRY_COUNT {
        return Err(OpcpuCodecError::CountOutOfRange {
            context: "CPRD program count exceeds hard limit".to_string(),
        });
    }
    let mut programs = Vec::with_capacity(count);
    for _ in 0..count {
        let owner_index = cur.read_u16()? as usize;
        let owner = owners.get(owner_index).cloned().ok_or_else(|| {
            OpcpuCodecError::InvalidChunkFormat {
                chunk: "CPRD".to_string(),
                detail: format!("compact owner index {owner_index} is out of range"),
            }
        })?;
        let id = cur.read_string()?;
        let schema_version = cur.read_u16()?;
        let len = cur.read_u32()? as usize;
        let program = cur
            .read_exact(len, "operand-record program bytes")?
            .to_vec();
        programs.push(OperandRecordProgramDescriptor {
            owner,
            id,
            schema_version,
            program,
        });
    }
    cur.finish()?;
    validate_operand_record_program_set(&programs)?;
    Ok(programs)
}

fn validate_operand_record_program_set(
    programs: &[OperandRecordProgramDescriptor],
) -> Result<(), OpcpuCodecError> {
    for (index, entry) in programs.iter().enumerate() {
        if entry.id.is_empty() {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "OPRD".to_string(),
                detail: "operand-record program id must not be empty".to_string(),
            });
        }
        validate_operand_record_program(entry.schema_version, &entry.program)?;
        if programs[..index].iter().any(|prior| {
            prior.owner.key_parts_lowercase() == entry.owner.key_parts_lowercase()
                && prior.id.eq_ignore_ascii_case(&entry.id)
        }) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "OPRD".to_string(),
                detail: format!(
                    "duplicate operand-record program id '{}' in one owner scope",
                    entry.id
                ),
            });
        }
    }
    Ok(())
}

pub(super) fn encode_slct_chunk(
    programs: &[SelectorProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(programs)
}

pub(super) fn decode_slct_chunk(
    bytes: &[u8],
) -> Result<Vec<SelectorProgramDescriptor>, OpcpuCodecError> {
    let programs = decode_scoped_schema_chunk(bytes)?;
    validate_selector_program_set(&programs)?;
    Ok(programs)
}

fn validate_selector_program_set(
    programs: &[SelectorProgramDescriptor],
) -> Result<(), OpcpuCodecError> {
    for (index, entry) in programs.iter().enumerate() {
        if entry.id.is_empty() {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "SLCT".to_string(),
                detail: "selector VM program id must not be empty".to_string(),
            });
        }
        validate_selector_program(entry.opcode_version, &entry.program)?;
        if programs[..index].iter().any(|prior| {
            prior.owner.key_parts_lowercase() == entry.owner.key_parts_lowercase()
                && prior.id.eq_ignore_ascii_case(&entry.id)
        }) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "SLCT".to_string(),
                detail: format!(
                    "duplicate selector VM program id '{}' in one owner scope",
                    entry.id
                ),
            });
        }
    }
    Ok(())
}

pub(super) fn validate_mode_selector_set(
    selectors: &[ModeSelectorDescriptor],
) -> Result<(), OpcpuCodecError> {
    for selector in selectors {
        validate_mode_selector_operand_plan(selector.operand_plan.as_str())?;
    }
    Ok(())
}

fn validate_mode_selector_operand_plan(plan: &str) -> Result<(), OpcpuCodecError> {
    let invalid = |detail: String| OpcpuCodecError::InvalidChunkFormat {
        chunk: "MSEL".to_string(),
        detail,
    };
    if plan.starts_with("state.require.v")
        && !plan.starts_with(MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX)
    {
        return Err(invalid(format!(
            "unsupported state-required operand-plan version in '{plan}'"
        )));
    }
    if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX) {
        let Some((requirement, nested)) = spec.split_once(';') else {
            return Err(invalid(
                "state-required operand plan is missing its nested plan".to_string(),
            ));
        };
        let requirement = requirement
            .split_once('?')
            .map_or(requirement, |(requirement, _)| requirement);
        let Some((key, values)) = requirement.split_once('=') else {
            return Err(invalid(
                "state-required operand plan is missing its value set".to_string(),
            ));
        };
        if key.is_empty()
            || values.is_empty()
            || values.split('+').any(|value| value.parse::<u32>().is_err())
        {
            return Err(invalid(
                "state-required operand plan has an invalid key or value set".to_string(),
            ));
        }
        return validate_mode_selector_operand_plan(nested);
    }
    for (family, supported) in [
        ("semv.inputs.v", MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX),
        ("semv.scalar.v", MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX),
        ("semv.branch.v", MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX),
        (
            "semv.sequence.v",
            MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX,
        ),
        ("semv.reject.v", MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX),
    ] {
        if plan.starts_with(family) && !plan.starts_with(supported) {
            return Err(invalid(format!(
                "unsupported semantic operand-plan version in '{plan}'"
            )));
        }
    }
    if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX) {
        let program_and_inputs = spec
            .split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
            .map_or(spec, |(program, _)| program);
        let Some((program_id, inputs)) =
            program_and_inputs.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR)
        else {
            return Err(invalid(
                "semantic inputs v1 operand plan is missing its input list".to_string(),
            ));
        };
        if program_id.is_empty() || inputs.is_empty() || inputs.split(',').any(str::is_empty) {
            return Err(invalid(
                "semantic inputs v1 operand plan has an empty program or input source".to_string(),
            ));
        }
    }
    if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX) {
        let program_id = spec
            .split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
            .map_or(spec, |(program, _)| program);
        if program_id.is_empty() || program_id.contains(MODE_SELECTOR_PLAN_INPUT_SEPARATOR) {
            return Err(invalid(
                "semantic scalar v1 operand plan has an invalid program id".to_string(),
            ));
        }
    }
    if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX) {
        let program_and_inputs = spec
            .split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
            .map_or(spec, |(program, _)| program);
        let Some((program_id, inputs)) =
            program_and_inputs.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR)
        else {
            return Err(invalid(
                "semantic branch v1 operand plan is missing its input list".to_string(),
            ));
        };
        let inputs = inputs.split(',').collect::<Vec<_>>();
        if program_id.is_empty()
            || inputs.len() != 4
            || inputs.iter().any(|input| input.is_empty())
            || inputs[1] != "expr0"
            || (inputs[2] != "auto" && inputs[2].parse::<u8>().is_err())
            || inputs[0].parse::<u8>().is_err()
            || inputs[3].parse::<u8>().is_err()
        {
            return Err(invalid(
                "semantic branch v1 requires program@opcode,expr0,auto-or-candidate,class"
                    .to_string(),
            ));
        }
    }
    if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX) {
        let sequence = spec
            .split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
            .map_or(spec, |(program, _)| program);
        let mut step_count = 0_usize;
        for step in sequence.split(';') {
            step_count += 1;
            let Some((kind, program_and_inputs)) = step.split_once(':') else {
                return Err(invalid(
                    "semantic sequence v1 step is missing its kind".to_string(),
                ));
            };
            let Some((program_id, inputs)) =
                program_and_inputs.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR)
            else {
                return Err(invalid(
                    "semantic sequence v1 step is missing its input list".to_string(),
                ));
            };
            if !matches!(kind, "encode" | "fixup" | "match")
                || program_id.is_empty()
                || inputs.is_empty()
                || inputs.split(',').any(str::is_empty)
            {
                return Err(invalid(
                    "semantic sequence v1 requires kind:program@input[,input] steps".to_string(),
                ));
            }
        }
        if step_count < 2 {
            return Err(invalid(
                "semantic sequence v1 requires at least two ordered steps".to_string(),
            ));
        }
    }
    if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX) {
        let Some((diagnostic, inputs)) = spec.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR) else {
            return Err(invalid(
                "semantic reject v1 operand plan is missing its input list".to_string(),
            ));
        };
        if diagnostic.is_empty() || inputs.is_empty() || inputs.split(',').any(str::is_empty) {
            return Err(invalid(
                "semantic reject v1 operand plan has an empty diagnostic or input source"
                    .to_string(),
            ));
        }
    }
    Ok(())
}

pub(super) fn encode_stvm_chunk(
    programs: &[StateProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(programs)
}

pub(super) fn decode_stvm_chunk(
    bytes: &[u8],
) -> Result<Vec<StateProgramDescriptor>, OpcpuCodecError> {
    let programs = decode_scoped_schema_chunk(bytes)?;
    validate_state_program_set(&programs)?;
    Ok(programs)
}

fn validate_state_program_set(programs: &[StateProgramDescriptor]) -> Result<(), OpcpuCodecError> {
    for (index, entry) in programs.iter().enumerate() {
        if entry.id.is_empty() {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "STVM".to_string(),
                detail: "state VM program id must not be empty".to_string(),
            });
        }
        validate_state_program(entry.opcode_version, &entry.program)?;
        if programs[..index].iter().any(|prior| {
            prior.owner.key_parts_lowercase() == entry.owner.key_parts_lowercase()
                && prior.id.eq_ignore_ascii_case(&entry.id)
        }) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "STVM".to_string(),
                detail: format!(
                    "duplicate state VM program id '{}' in one owner scope",
                    entry.id
                ),
            });
        }
    }
    Ok(())
}

pub(super) fn encode_msel_chunk(
    selectors: &[ModeSelectorDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(selectors)
}

pub(super) fn decode_msel_chunk(
    bytes: &[u8],
) -> Result<Vec<ModeSelectorDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
}

enum CompactSelectorPlan<'a> {
    Raw(&'a str),
    SemanticInputs {
        program: &'a str,
        inputs: &'a str,
        diagnostic: Option<&'a str>,
    },
    SemanticScalar {
        program: &'a str,
        diagnostic: Option<&'a str>,
    },
    SemanticBranch {
        program: &'a str,
        inputs: &'a str,
        diagnostic: Option<&'a str>,
    },
    SemanticSequence {
        steps: Vec<(&'a str, &'a str, &'a str)>,
        diagnostic: Option<&'a str>,
    },
    SemanticReject {
        diagnostic: &'a str,
        inputs: &'a str,
    },
    StateRequired {
        requirement: &'a str,
        nested: Box<CompactSelectorPlan<'a>>,
    },
}

fn compact_selector_plan(plan: &str) -> CompactSelectorPlan<'_> {
    if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX) {
        if let Some((requirement, nested)) = spec.split_once(';') {
            if !requirement.is_empty() && !nested.is_empty() {
                return CompactSelectorPlan::StateRequired {
                    requirement,
                    nested: Box::new(compact_selector_plan(nested)),
                };
            }
        }
    } else if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX) {
        let (program_and_inputs, diagnostic) = spec
            .split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
            .map_or((spec, None), |(body, code)| (body, Some(code)));
        if let Some((program, inputs)) =
            program_and_inputs.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR)
        {
            return CompactSelectorPlan::SemanticInputs {
                program,
                inputs,
                diagnostic,
            };
        }
    } else if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX) {
        let (program, diagnostic) = spec
            .split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
            .map_or((spec, None), |(body, code)| (body, Some(code)));
        return CompactSelectorPlan::SemanticScalar {
            program,
            diagnostic,
        };
    } else if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX) {
        let (program_and_inputs, diagnostic) = spec
            .split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
            .map_or((spec, None), |(body, code)| (body, Some(code)));
        if let Some((program, inputs)) =
            program_and_inputs.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR)
        {
            return CompactSelectorPlan::SemanticBranch {
                program,
                inputs,
                diagnostic,
            };
        }
    } else if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX) {
        let (sequence, diagnostic) = spec
            .split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
            .map_or((spec, None), |(body, code)| (body, Some(code)));
        let steps = sequence
            .split(';')
            .filter_map(|step| {
                let (kind, program_and_inputs) = step.split_once(':')?;
                let (program, inputs) =
                    program_and_inputs.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR)?;
                Some((kind, program, inputs))
            })
            .collect::<Vec<_>>();
        if !steps.is_empty() {
            return CompactSelectorPlan::SemanticSequence { steps, diagnostic };
        }
    } else if let Some(spec) = plan.strip_prefix(MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX) {
        if let Some((diagnostic, inputs)) = spec.split_once(MODE_SELECTOR_PLAN_INPUT_SEPARATOR) {
            if !diagnostic.is_empty() && !inputs.is_empty() {
                return CompactSelectorPlan::SemanticReject { diagnostic, inputs };
            }
        }
    }
    CompactSelectorPlan::Raw(plan)
}

fn compact_selector_input_strings<'a>(
    inputs: &'a str,
    split_inputs: bool,
    strings: &mut Vec<&'a str>,
) {
    if split_inputs {
        strings.extend(inputs.split(','));
    } else {
        strings.push(inputs);
    }
}

fn compact_selector_plan_strings<'a>(
    plan: CompactSelectorPlan<'a>,
    split_inputs: bool,
    strings: &mut Vec<&'a str>,
) {
    match plan {
        CompactSelectorPlan::Raw(plan) => strings.push(plan),
        CompactSelectorPlan::SemanticInputs {
            program,
            inputs,
            diagnostic,
        }
        | CompactSelectorPlan::SemanticBranch {
            program,
            inputs,
            diagnostic,
        } => {
            strings.push(program);
            compact_selector_input_strings(inputs, split_inputs, strings);
            strings.extend(diagnostic);
        }
        CompactSelectorPlan::SemanticScalar {
            program,
            diagnostic,
        } => {
            strings.push(program);
            strings.extend(diagnostic);
        }
        CompactSelectorPlan::SemanticSequence { steps, diagnostic } => {
            for (kind, program, inputs) in steps {
                strings.extend([kind, program]);
                compact_selector_input_strings(inputs, split_inputs, strings);
            }
            strings.extend(diagnostic);
        }
        CompactSelectorPlan::SemanticReject { diagnostic, inputs } => {
            strings.push(diagnostic);
            compact_selector_input_strings(inputs, split_inputs, strings);
        }
        CompactSelectorPlan::StateRequired {
            requirement,
            nested,
        } => {
            strings.push(requirement);
            compact_selector_plan_strings(*nested, split_inputs, strings);
        }
    }
}

fn encode_compact_selector_plan(
    out: &mut Vec<u8>,
    plan: CompactSelectorPlan<'_>,
    string_index: &impl Fn(&str) -> u16,
    version: u16,
) -> Result<(), OpcpuCodecError> {
    let encode_inputs = |out: &mut Vec<u8>, inputs: &str| -> Result<(), OpcpuCodecError> {
        if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V7 {
            let inputs = inputs.split(',').collect::<Vec<_>>();
            out.push(
                u8::try_from(inputs.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
                    context: "CMSE semantic input count exceeds u8".to_string(),
                })?,
            );
            for input in inputs {
                write_u16(out, string_index(input));
            }
        } else {
            write_u16(out, string_index(inputs));
        }
        Ok(())
    };
    match plan {
        CompactSelectorPlan::Raw(plan) => {
            out.push(0);
            write_u16(out, string_index(plan));
        }
        CompactSelectorPlan::SemanticInputs {
            program,
            inputs,
            diagnostic,
        } => {
            out.push(1);
            write_u16(out, string_index(program));
            encode_inputs(out, inputs)?;
            write_u16(out, diagnostic.map_or(u16::MAX, string_index));
        }
        CompactSelectorPlan::SemanticScalar {
            program,
            diagnostic,
        } => {
            out.push(2);
            write_u16(out, string_index(program));
            write_u16(out, diagnostic.map_or(u16::MAX, string_index));
        }
        CompactSelectorPlan::SemanticBranch {
            program,
            inputs,
            diagnostic,
        } => {
            out.push(3);
            write_u16(out, string_index(program));
            encode_inputs(out, inputs)?;
            write_u16(out, diagnostic.map_or(u16::MAX, string_index));
        }
        CompactSelectorPlan::SemanticSequence { steps, diagnostic } => {
            out.push(4);
            out.push(
                u8::try_from(steps.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
                    context: "CMSE semantic sequence step count exceeds u8".to_string(),
                })?,
            );
            for (kind, program, inputs) in steps {
                out.push(match kind {
                    "match" => 0,
                    "encode" => 1,
                    "fixup" => 2,
                    _ => unreachable!("validated semantic sequence kind"),
                });
                write_u16(out, string_index(program));
                encode_inputs(out, inputs)?;
            }
            write_u16(out, diagnostic.map_or(u16::MAX, string_index));
        }
        CompactSelectorPlan::SemanticReject { diagnostic, inputs } => {
            if version < COMPACT_MODE_SELECTOR_CHUNK_VERSION_V7 {
                unreachable!("semantic rejection plans are structured by CMSE v7 and later");
            }
            out.push(6);
            write_u16(out, string_index(diagnostic));
            encode_inputs(out, inputs)?;
        }
        CompactSelectorPlan::StateRequired {
            requirement,
            nested,
        } => {
            out.push(5);
            write_u16(out, string_index(requirement));
            encode_compact_selector_plan(out, *nested, string_index, version)?;
        }
    }
    Ok(())
}

pub(super) fn encode_compact_msel_chunk(
    selectors: &[ModeSelectorDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut owners = Vec::<ScopedOwner>::new();
    for selector in selectors {
        if !owners.contains(&selector.owner) {
            owners.push(selector.owner.clone());
        }
    }
    let owner_count =
        u16::try_from(owners.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "CMSE owner count exceeds u16".to_string(),
        })?;
    let compact_version = if owner_count <= u16::from(u8::MAX) + 1 {
        COMPACT_MODE_SELECTOR_CHUNK_VERSION_V7
    } else {
        COMPACT_MODE_SELECTOR_CHUNK_VERSION_V3
    };
    let split_inputs = compact_version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V7;

    let mut strings = Vec::<String>::new();
    for selector in selectors {
        for value in [&selector.mnemonic, &selector.shape_key, &selector.mode_key] {
            if !strings.contains(value) {
                strings.push(value.clone());
            }
        }
        let mut plan_parts = Vec::new();
        compact_selector_plan_strings(
            compact_selector_plan(&selector.operand_plan),
            split_inputs,
            &mut plan_parts,
        );
        for value in plan_parts {
            if !strings.iter().any(|entry| entry == value) {
                strings.push(value.to_string());
            }
        }
    }
    strings.sort();
    let string_count =
        u16::try_from(strings.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
            context: "CMSE string count exceeds u16".to_string(),
        })?;

    let mut out = Vec::new();
    write_u16(&mut out, compact_version);
    write_u16(&mut out, owner_count);
    for owner in &owners {
        encode_scoped_owner(&mut out, "CMSE", owner)?;
    }
    write_u16(&mut out, string_count);
    let mut previous = "";
    for value in &strings {
        let mut prefix_len = previous
            .as_bytes()
            .iter()
            .zip(value.as_bytes())
            .take_while(|(left, right)| left == right)
            .count();
        while !value.is_char_boundary(prefix_len) || !previous.is_char_boundary(prefix_len) {
            prefix_len -= 1;
        }
        write_u16(
            &mut out,
            u16::try_from(prefix_len).map_err(|_| OpcpuCodecError::CountOutOfRange {
                context: "CMSE string prefix length exceeds u16".to_string(),
            })?,
        );
        write_string(&mut out, "CMSE", &value[prefix_len..])?;
        previous = value;
    }
    write_u32(&mut out, u32_count(selectors.len(), "CMSE selector count")?);
    for selector in selectors {
        let owner_index = owners
            .iter()
            .position(|owner| owner == &selector.owner)
            .expect("CMSE owner table is built from selectors");
        if compact_version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V4 {
            out.push(u8::try_from(owner_index).expect("CMSE v4 owner index fits u8"));
        } else {
            write_u16(
                &mut out,
                u16::try_from(owner_index).expect("CMSE owner index fits its u16 count"),
            );
        }
        let string_index = |value: &str| -> u16 {
            let index = strings
                .iter()
                .position(|entry| entry == value)
                .expect("CMSE string table is built from selectors");
            u16::try_from(index).expect("CMSE string index fits its u16 count")
        };
        for value in [&selector.mnemonic, &selector.shape_key] {
            write_u16(&mut out, string_index(value));
        }
        if compact_version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V6 {
            if selector.mode_key == "semantic" {
                out.push(0);
            } else {
                out.push(1);
                write_u16(&mut out, string_index(&selector.mode_key));
            }
        } else {
            write_u16(&mut out, string_index(&selector.mode_key));
        }
        encode_compact_selector_plan(
            &mut out,
            compact_selector_plan(&selector.operand_plan),
            &string_index,
            compact_version,
        )?;
        if compact_version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V5 {
            if selector.priority < u16::from(u8::MAX) {
                out.push(selector.priority as u8);
            } else {
                out.push(u8::MAX);
                write_u16(&mut out, selector.priority);
            }
            if compact_version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V7 {
                out.push(u8::from(selector.unstable_widen));
                out.push(selector.width_rank);
            } else {
                out.push((selector.width_rank << 1) | u8::from(selector.unstable_widen));
            }
        } else {
            write_u16(&mut out, selector.priority);
            out.push(u8::from(selector.unstable_widen));
            out.push(selector.width_rank);
        }
    }
    Ok(out)
}

fn read_compact_msel_string(
    cur: &mut Decoder<'_>,
    strings: &[String],
) -> Result<String, OpcpuCodecError> {
    let index = cur.read_u16()? as usize;
    strings
        .get(index)
        .cloned()
        .ok_or_else(|| OpcpuCodecError::InvalidChunkFormat {
            chunk: "CMSE".to_string(),
            detail: format!("string index {index} is out of range"),
        })
}

fn read_optional_compact_msel_string<'a>(
    cur: &mut Decoder<'_>,
    strings: &'a [String],
) -> Result<Option<&'a str>, OpcpuCodecError> {
    let index = cur.read_u16()?;
    if index == u16::MAX {
        return Ok(None);
    }
    strings
        .get(index as usize)
        .map(|value| Some(value.as_str()))
        .ok_or_else(|| OpcpuCodecError::InvalidChunkFormat {
            chunk: "CMSE".to_string(),
            detail: format!("string index {index} is out of range"),
        })
}

fn read_compact_selector_inputs(
    cur: &mut Decoder<'_>,
    strings: &[String],
    version: u16,
) -> Result<String, OpcpuCodecError> {
    if version < COMPACT_MODE_SELECTOR_CHUNK_VERSION_V7 {
        return read_compact_msel_string(cur, strings);
    }
    let input_count = cur.read_u8()? as usize;
    if input_count == 0 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CMSE".to_string(),
            detail: "semantic input list must not be empty".to_string(),
        });
    }
    let mut inputs = Vec::with_capacity(input_count);
    for _ in 0..input_count {
        inputs.push(read_compact_msel_string(cur, strings)?);
    }
    Ok(inputs.join(","))
}

fn decode_compact_selector_plan(
    cur: &mut Decoder<'_>,
    strings: &[String],
    version: u16,
) -> Result<String, OpcpuCodecError> {
    let plan_kind = cur.read_u8()?;
    match plan_kind {
        0 => read_compact_msel_string(cur, strings),
        1 => {
            let program = read_compact_msel_string(cur, strings)?;
            let inputs = read_compact_selector_inputs(cur, strings, version)?;
            let diagnostic = read_optional_compact_msel_string(cur, strings)?;
            Ok(format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}{}",
                diagnostic.map_or_else(String::new, |code| {
                    format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{code}")
                })
            ))
        }
        2 => {
            let program = read_compact_msel_string(cur, strings)?;
            let diagnostic = read_optional_compact_msel_string(cur, strings)?;
            Ok(format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX}{program}{}",
                diagnostic.map_or_else(String::new, |code| {
                    format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{code}")
                })
            ))
        }
        3 if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V2 => {
            let program = read_compact_msel_string(cur, strings)?;
            let inputs = read_compact_selector_inputs(cur, strings, version)?;
            let diagnostic = read_optional_compact_msel_string(cur, strings)?;
            Ok(format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}{}",
                diagnostic.map_or_else(String::new, |code| {
                    format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{code}")
                })
            ))
        }
        4 if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V2 => {
            let step_count = cur.read_u8()? as usize;
            if step_count < 2 {
                return Err(OpcpuCodecError::InvalidChunkFormat {
                    chunk: "CMSE".to_string(),
                    detail: "semantic sequence requires at least two steps".to_string(),
                });
            }
            let mut steps = Vec::with_capacity(step_count);
            for _ in 0..step_count {
                let kind = match cur.read_u8()? {
                    0 => "match",
                    1 => "encode",
                    2 => "fixup",
                    tag => {
                        return Err(OpcpuCodecError::InvalidChunkFormat {
                            chunk: "CMSE".to_string(),
                            detail: format!("unsupported semantic sequence kind {tag}"),
                        })
                    }
                };
                let program = read_compact_msel_string(cur, strings)?;
                let inputs = read_compact_selector_inputs(cur, strings, version)?;
                steps.push(format!(
                    "{kind}:{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
                ));
            }
            let diagnostic = read_optional_compact_msel_string(cur, strings)?;
            Ok(format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}{}{}",
                steps.join(";"),
                diagnostic.map_or_else(String::new, |code| {
                    format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{code}")
                })
            ))
        }
        5 if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V3 => {
            let requirement = read_compact_msel_string(cur, strings)?;
            let nested = decode_compact_selector_plan(cur, strings, version)?;
            Ok(format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}{requirement};{nested}"
            ))
        }
        6 if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V7 => {
            let diagnostic = read_compact_msel_string(cur, strings)?;
            let inputs = read_compact_selector_inputs(cur, strings, version)?;
            Ok(format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ))
        }
        _ => Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CMSE".to_string(),
            detail: format!("unsupported compact operand-plan kind {plan_kind}"),
        }),
    }
}

pub(super) fn decode_compact_msel_chunk(
    bytes: &[u8],
) -> Result<Vec<ModeSelectorDescriptor>, OpcpuCodecError> {
    let mut cur = Decoder::new(bytes, "CMSE");
    let version = cur.read_u16()?;
    if version != COMPACT_MODE_SELECTOR_CHUNK_VERSION_V1
        && version != COMPACT_MODE_SELECTOR_CHUNK_VERSION_V2
        && version != COMPACT_MODE_SELECTOR_CHUNK_VERSION_V3
        && version != COMPACT_MODE_SELECTOR_CHUNK_VERSION_V4
        && version != COMPACT_MODE_SELECTOR_CHUNK_VERSION_V5
        && version != COMPACT_MODE_SELECTOR_CHUNK_VERSION_V6
        && version != COMPACT_MODE_SELECTOR_CHUNK_VERSION_V7
    {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "CMSE".to_string(),
            detail: format!("unsupported compact mode-selector version {version}"),
        });
    }
    let owner_count = cur.read_u16()? as usize;
    let mut owners = Vec::with_capacity(owner_count);
    for _ in 0..owner_count {
        let owner = decode_scoped_owner(&mut cur, "CMSE")?;
        if owners.contains(&owner) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CMSE".to_string(),
                detail: "duplicate owner table entry".to_string(),
            });
        }
        owners.push(owner);
    }
    let string_count = cur.read_u16()? as usize;
    let mut strings = Vec::with_capacity(string_count);
    for _ in 0..string_count {
        let value = if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V2 {
            let prefix_len = cur.read_u16()? as usize;
            let suffix = cur.read_string()?;
            let previous = strings.last().map_or("", String::as_str);
            if prefix_len > previous.len() || !previous.is_char_boundary(prefix_len) {
                return Err(OpcpuCodecError::InvalidChunkFormat {
                    chunk: "CMSE".to_string(),
                    detail: format!("string prefix length {prefix_len} is out of range"),
                });
            }
            format!("{}{suffix}", &previous[..prefix_len])
        } else {
            cur.read_string()?
        };
        if strings.contains(&value) {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CMSE".to_string(),
                detail: "duplicate string table entry".to_string(),
            });
        }
        strings.push(value);
    }
    let selector_count = cur.read_u32()? as usize;
    if selector_count > MAX_DECODE_ENTRY_COUNT {
        return Err(OpcpuCodecError::CountOutOfRange {
            context: "CMSE selector count exceeds hard limit".to_string(),
        });
    }
    let mut selectors = Vec::with_capacity(selector_count);
    for _ in 0..selector_count {
        let owner_index = if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V4 {
            cur.read_u8()? as usize
        } else {
            cur.read_u16()? as usize
        };
        let owner = owners.get(owner_index).cloned().ok_or_else(|| {
            OpcpuCodecError::InvalidChunkFormat {
                chunk: "CMSE".to_string(),
                detail: format!("owner index {owner_index} is out of range"),
            }
        })?;
        let mnemonic = read_compact_msel_string(&mut cur, &strings)?;
        let shape_key = read_compact_msel_string(&mut cur, &strings)?;
        let mode_key = if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V6 {
            match cur.read_u8()? {
                0 => "semantic".to_string(),
                1 => read_compact_msel_string(&mut cur, &strings)?,
                tag => {
                    return Err(OpcpuCodecError::InvalidChunkFormat {
                        chunk: "CMSE".to_string(),
                        detail: format!("unsupported compact mode-key tag {tag}"),
                    })
                }
            }
        } else {
            read_compact_msel_string(&mut cur, &strings)?
        };
        let plan_kind = cur.read_u8()?;
        let operand_plan = match plan_kind {
            0 => read_compact_msel_string(&mut cur, &strings)?,
            1 => {
                let program = read_compact_msel_string(&mut cur, &strings)?;
                let inputs = read_compact_selector_inputs(&mut cur, &strings, version)?;
                let diagnostic_index = cur.read_u16()?;
                let diagnostic = if diagnostic_index == u16::MAX {
                    None
                } else {
                    Some(strings.get(diagnostic_index as usize).ok_or_else(|| {
                        OpcpuCodecError::InvalidChunkFormat {
                            chunk: "CMSE".to_string(),
                            detail: format!(
                                "diagnostic string index {diagnostic_index} is out of range"
                            ),
                        }
                    })?)
                };
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}{}",
                    diagnostic.map_or_else(String::new, |code| {
                        format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{code}")
                    })
                )
            }
            2 => {
                let program = read_compact_msel_string(&mut cur, &strings)?;
                let diagnostic_index = cur.read_u16()?;
                let diagnostic = if diagnostic_index == u16::MAX {
                    None
                } else {
                    Some(strings.get(diagnostic_index as usize).ok_or_else(|| {
                        OpcpuCodecError::InvalidChunkFormat {
                            chunk: "CMSE".to_string(),
                            detail: format!(
                                "diagnostic string index {diagnostic_index} is out of range"
                            ),
                        }
                    })?)
                };
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX}{program}{}",
                    diagnostic.map_or_else(String::new, |code| {
                        format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{code}")
                    })
                )
            }
            3 if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V2 => {
                let program = read_compact_msel_string(&mut cur, &strings)?;
                let inputs = read_compact_selector_inputs(&mut cur, &strings, version)?;
                let diagnostic_index = cur.read_u16()?;
                let diagnostic = if diagnostic_index == u16::MAX {
                    None
                } else {
                    Some(strings.get(diagnostic_index as usize).ok_or_else(|| {
                        OpcpuCodecError::InvalidChunkFormat {
                            chunk: "CMSE".to_string(),
                            detail: format!(
                                "diagnostic string index {diagnostic_index} is out of range"
                            ),
                        }
                    })?)
                };
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}{}",
                    diagnostic.map_or_else(String::new, |code| {
                        format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{code}")
                    })
                )
            }
            4 if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V2 => {
                let step_count = cur.read_u8()? as usize;
                if step_count < 2 {
                    return Err(OpcpuCodecError::InvalidChunkFormat {
                        chunk: "CMSE".to_string(),
                        detail: "semantic sequence requires at least two steps".to_string(),
                    });
                }
                let mut steps = Vec::with_capacity(step_count);
                for _ in 0..step_count {
                    let kind = match cur.read_u8()? {
                        0 => "match",
                        1 => "encode",
                        2 => "fixup",
                        tag => {
                            return Err(OpcpuCodecError::InvalidChunkFormat {
                                chunk: "CMSE".to_string(),
                                detail: format!("unsupported semantic sequence kind {tag}"),
                            })
                        }
                    };
                    let program = read_compact_msel_string(&mut cur, &strings)?;
                    let inputs = read_compact_selector_inputs(&mut cur, &strings, version)?;
                    steps.push(format!(
                        "{kind}:{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
                    ));
                }
                let diagnostic_index = cur.read_u16()?;
                let diagnostic = if diagnostic_index == u16::MAX {
                    None
                } else {
                    Some(strings.get(diagnostic_index as usize).ok_or_else(|| {
                        OpcpuCodecError::InvalidChunkFormat {
                            chunk: "CMSE".to_string(),
                            detail: format!(
                                "diagnostic string index {diagnostic_index} is out of range"
                            ),
                        }
                    })?)
                };
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}{}{}",
                    steps.join(";"),
                    diagnostic.map_or_else(String::new, |code| {
                        format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{code}")
                    })
                )
            }
            5 if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V3 => {
                let requirement = read_compact_msel_string(&mut cur, &strings)?;
                let nested = decode_compact_selector_plan(&mut cur, &strings, version)?;
                format!("{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}{requirement};{nested}")
            }
            6 if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V7 => {
                let diagnostic = read_compact_msel_string(&mut cur, &strings)?;
                let inputs = read_compact_selector_inputs(&mut cur, &strings, version)?;
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
                )
            }
            _ => {
                return Err(OpcpuCodecError::InvalidChunkFormat {
                    chunk: "CMSE".to_string(),
                    detail: format!("unsupported compact operand-plan kind {plan_kind}"),
                })
            }
        };
        let (priority, unstable_widen, width_rank) =
            if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V5 {
                let compact_priority = cur.read_u8()?;
                let priority = if compact_priority == u8::MAX {
                    cur.read_u16()?
                } else {
                    u16::from(compact_priority)
                };
                if version >= COMPACT_MODE_SELECTOR_CHUNK_VERSION_V7 {
                    let flags = cur.read_u8()?;
                    if flags & !1 != 0 {
                        return Err(OpcpuCodecError::InvalidChunkFormat {
                            chunk: "CMSE".to_string(),
                            detail: format!("unsupported selector flags 0x{flags:02x}"),
                        });
                    }
                    (priority, flags & 1 != 0, cur.read_u8()?)
                } else {
                    let packed = cur.read_u8()?;
                    (priority, packed & 1 != 0, packed >> 1)
                }
            } else {
                let priority = cur.read_u16()?;
                let flags = cur.read_u8()?;
                if flags & !1 != 0 {
                    return Err(OpcpuCodecError::InvalidChunkFormat {
                        chunk: "CMSE".to_string(),
                        detail: format!("unsupported selector flags 0x{flags:02x}"),
                    });
                }
                (priority, flags & 1 != 0, cur.read_u8()?)
            };
        selectors.push(ModeSelectorDescriptor {
            owner,
            mnemonic,
            shape_key,
            mode_key,
            operand_plan,
            priority,
            unstable_widen,
            width_rank,
        });
    }
    cur.finish()?;
    validate_mode_selector_set(&selectors)?;
    Ok(selectors)
}

pub(super) fn encode_tkvm_chunk(
    programs: &[TokenizerVmProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(programs)
}

pub(super) fn decode_tkvm_chunk(
    bytes: &[u8],
) -> Result<Vec<TokenizerVmProgramDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
}

pub(super) fn encode_pars_chunk(
    contracts: &[ParserContractDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(contracts)
}

pub(super) fn decode_pars_chunk(
    bytes: &[u8],
) -> Result<Vec<ParserContractDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
}

pub(super) fn encode_prvm_chunk(
    programs: &[ParserVmProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(programs)
}

pub(super) fn decode_prvm_chunk(
    bytes: &[u8],
) -> Result<Vec<ParserVmProgramDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
}

pub(super) fn encode_expr_chunk(
    contracts: &[ExprContractDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(contracts)
}

pub(super) fn decode_expr_chunk(
    bytes: &[u8],
) -> Result<Vec<ExprContractDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
}

pub(super) fn encode_exvm_chunk(
    contracts: &[ExprParserContractDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(contracts)
}

pub(super) fn decode_exvm_chunk(
    bytes: &[u8],
) -> Result<Vec<ExprParserContractDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
}

pub(super) fn validate_expr_contract_descriptor(
    descriptor: &ExprContractDescriptor,
) -> Result<(), OpcpuCodecError> {
    if descriptor.opcode_version != EXPR_VM_OPCODE_VERSION_V1
        && descriptor.opcode_version != EXPR_VM_OPCODE_VERSION_V2
    {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "EXPR".to_string(),
            detail: format!("unsupported opcode_version: {}", descriptor.opcode_version),
        });
    }

    if descriptor.max_program_bytes == 0 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "EXPR".to_string(),
            detail: "max_program_bytes must be > 0".to_string(),
        });
    }
    if descriptor.max_stack_depth == 0 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "EXPR".to_string(),
            detail: "max_stack_depth must be > 0".to_string(),
        });
    }
    if descriptor.max_symbol_refs == 0 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "EXPR".to_string(),
            detail: "max_symbol_refs must be > 0".to_string(),
        });
    }
    if descriptor.max_eval_steps == 0 {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "EXPR".to_string(),
            detail: "max_eval_steps must be > 0".to_string(),
        });
    }

    let diagnostics = &descriptor.diagnostics;
    let required_codes = [
        (
            "diagnostics.invalid_opcode",
            diagnostics.invalid_opcode.as_str(),
        ),
        (
            "diagnostics.stack_underflow",
            diagnostics.stack_underflow.as_str(),
        ),
        (
            "diagnostics.stack_depth_exceeded",
            diagnostics.stack_depth_exceeded.as_str(),
        ),
        (
            "diagnostics.unknown_symbol",
            diagnostics.unknown_symbol.as_str(),
        ),
        (
            "diagnostics.eval_failure",
            diagnostics.eval_failure.as_str(),
        ),
        (
            "diagnostics.unsupported_feature",
            diagnostics.unsupported_feature.as_str(),
        ),
        (
            "diagnostics.budget_exceeded",
            diagnostics.budget_exceeded.as_str(),
        ),
        (
            "diagnostics.invalid_program",
            diagnostics.invalid_program.as_str(),
        ),
    ];
    for (name, code) in required_codes {
        if code.trim().is_empty() {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "EXPR".to_string(),
                detail: format!("missing {} code", name),
            });
        }
    }

    Ok(())
}

pub(super) fn validate_expr_parser_contract_descriptor(
    descriptor: &ExprParserContractDescriptor,
) -> Result<(), OpcpuCodecError> {
    if descriptor.opcode_version != EXVM_OPCODE_VERSION_V1
        && descriptor.opcode_version != EXVM_OPCODE_VERSION_V2
    {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "EXVM".to_string(),
            detail: format!("unsupported opcode_version: {}", descriptor.opcode_version),
        });
    }

    if descriptor
        .diagnostics
        .invalid_expression_program
        .trim()
        .is_empty()
    {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: "EXVM".to_string(),
            detail: "missing diagnostics.invalid_expression_program code".to_string(),
        });
    }

    Ok(())
}

pub(super) fn write_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

pub(super) fn write_u16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_le_bytes());
}

pub(super) fn write_string(
    out: &mut Vec<u8>,
    chunk: &str,
    value: &str,
) -> Result<(), OpcpuCodecError> {
    let len = u32::try_from(value.len()).map_err(|_| OpcpuCodecError::CountOutOfRange {
        context: format!("{} string length exceeds u32", chunk),
    })?;
    write_u32(out, len);
    out.extend_from_slice(value.as_bytes());
    Ok(())
}

pub(super) fn u32_count(count: usize, context: &str) -> Result<u32, OpcpuCodecError> {
    u32::try_from(count).map_err(|_| OpcpuCodecError::CountOutOfRange {
        context: context.to_string(),
    })
}

pub(super) fn chunk_name(tag: &[u8; 4]) -> String {
    std::str::from_utf8(tag)
        .map(|value| value.to_string())
        .unwrap_or_else(|_| format!("{:02X?}", tag))
}

pub(super) fn read_bounded_count(
    cur: &mut Decoder<'_>,
    min_record_bytes: usize,
    detail: &str,
) -> Result<usize, OpcpuCodecError> {
    let count = cur.read_u32()? as usize;
    if min_record_bytes == 0 {
        if count > MAX_DECODE_ENTRY_COUNT {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: cur.chunk.to_string(),
                detail: format!(
                    "{} count {} exceeds hard limit {}",
                    detail, count, MAX_DECODE_ENTRY_COUNT
                ),
            });
        }
        return Ok(count);
    }

    let max_by_payload = cur.remaining_len() / min_record_bytes;
    if count > MAX_DECODE_ENTRY_COUNT {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: cur.chunk.to_string(),
            detail: format!(
                "{} count {} exceeds hard limit {}",
                detail, count, MAX_DECODE_ENTRY_COUNT
            ),
        });
    }

    if count > max_by_payload {
        return Err(OpcpuCodecError::InvalidChunkFormat {
            chunk: cur.chunk.to_string(),
            detail: format!(
                "{} count {} exceeds remaining payload bound {}",
                detail, count, max_by_payload
            ),
        });
    }

    Ok(count)
}

pub(super) struct Decoder<'a> {
    bytes: &'a [u8],
    pos: usize,
    chunk: &'static str,
}

impl<'a> Decoder<'a> {
    fn new(bytes: &'a [u8], chunk: &'static str) -> Self {
        Self {
            bytes,
            pos: 0,
            chunk,
        }
    }

    fn read_u8(&mut self) -> Result<u8, OpcpuCodecError> {
        let slice = self.read_exact(1, "u8")?;
        Ok(slice[0])
    }

    fn peek_u8(&self) -> Result<u8, OpcpuCodecError> {
        self.bytes
            .get(self.pos)
            .copied()
            .ok_or_else(|| OpcpuCodecError::UnexpectedEof {
                context: format!("chunk {} u8", self.chunk),
            })
    }

    fn has_remaining(&self) -> bool {
        self.pos < self.bytes.len()
    }

    fn remaining_len(&self) -> usize {
        self.bytes.len().saturating_sub(self.pos)
    }

    fn read_u32(&mut self) -> Result<u32, OpcpuCodecError> {
        let slice = self.read_exact(4, "u32")?;
        Ok(u32::from_le_bytes([slice[0], slice[1], slice[2], slice[3]]))
    }

    fn read_u16(&mut self) -> Result<u16, OpcpuCodecError> {
        let slice = self.read_exact(2, "u16")?;
        Ok(u16::from_le_bytes([slice[0], slice[1]]))
    }

    fn read_string(&mut self) -> Result<String, OpcpuCodecError> {
        let len = self.read_u32()? as usize;
        let bytes = self.read_exact(len, "string bytes")?;
        String::from_utf8(bytes.to_vec()).map_err(|_| OpcpuCodecError::InvalidUtf8 {
            chunk: self.chunk.to_string(),
        })
    }

    fn read_exact(&mut self, len: usize, detail: &str) -> Result<&'a [u8], OpcpuCodecError> {
        let end = self
            .pos
            .checked_add(len)
            .ok_or_else(|| OpcpuCodecError::InvalidChunkFormat {
                chunk: self.chunk.to_string(),
                detail: format!("{} overflow", detail),
            })?;
        if end > self.bytes.len() {
            return Err(OpcpuCodecError::UnexpectedEof {
                context: format!("chunk {} {}", self.chunk, detail),
            });
        }
        let out = &self.bytes[self.pos..end];
        self.pos = end;
        Ok(out)
    }

    fn finish(&self) -> Result<(), OpcpuCodecError> {
        if self.pos != self.bytes.len() {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: self.chunk.to_string(),
                detail: "trailing bytes".to_string(),
            });
        }
        Ok(())
    }
}
