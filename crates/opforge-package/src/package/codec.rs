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
        chunks.push((CHUNK_SEMV, encode_semv_chunk(&semantic_programs)?));
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
        (CHUNK_FORM, encode_form_chunk(&forms)?),
        (CHUNK_TABL, encode_tabl_chunk(&tables)?),
        (CHUNK_MSEL, encode_msel_chunk(&selectors)?),
    ]);

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
    let valp_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_VALP)?;
    let oprd_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_OPRD)?;
    let cprd_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_CPRD)?;
    let slct_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_SLCT)?;
    let stvm_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_STVM)?;
    if cprd_bytes.is_some() {
        let Some(payload) = semv_bytes else {
            return Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: "CPRD".to_string(),
                detail: "compact operand records require structured semantic programs".to_string(),
            });
        };
        if !decode_semv_chunk(payload)?
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
    let form_bytes = slice_for_chunk(bytes, &toc, CHUNK_FORM)?;
    let tabl_bytes = slice_for_chunk(bytes, &toc, CHUNK_TABL)?;
    let msel_bytes = slice_for_chunk_optional(bytes, &toc, CHUNK_MSEL)?;

    let mut cpus = decode_cpus_chunk(cpus_bytes)?;
    if let Some(payload) = cals_bytes {
        cpus.extend(decode_cals_chunk(payload, &cpus)?);
    }

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
        registers: decode_regs_chunk(regs_bytes)?,
        forms: decode_form_chunk(form_bytes)?,
        tables: decode_tabl_chunk(tabl_bytes)?,
        semantic_programs: match semv_bytes {
            Some(payload) => decode_semv_chunk(payload)?,
            None => Vec::new(),
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
        selectors: match msel_bytes {
            Some(payload) => decode_msel_chunk(payload)?,
            None => Vec::new(),
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

pub(super) fn encode_tabl_chunk(
    tables: &[VmProgramDescriptor],
) -> Result<Vec<u8>, OpcpuCodecError> {
    encode_scoped_schema_chunk(tables)
}

pub(super) fn decode_tabl_chunk(bytes: &[u8]) -> Result<Vec<VmProgramDescriptor>, OpcpuCodecError> {
    decode_scoped_schema_chunk(bytes)
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
