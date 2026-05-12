use super::*;
use proptest::prelude::*;

fn sample_families() -> Vec<FamilyDescriptor> {
    vec![
        FamilyDescriptor {
            id: "mos6502".to_string(),
            canonical_dialect: "mos".to_string(),
        },
        FamilyDescriptor {
            id: "intel8080".to_string(),
            canonical_dialect: "intel".to_string(),
        },
    ]
}

fn sample_cpus() -> Vec<CpuDescriptor> {
    vec![
        CpuDescriptor {
            id: "z80".to_string(),
            family_id: "intel8080".to_string(),
            default_dialect: Some("zilog".to_string()),
        },
        CpuDescriptor {
            id: "8085".to_string(),
            family_id: "intel8080".to_string(),
            default_dialect: Some("intel".to_string()),
        },
        CpuDescriptor {
            id: "6502".to_string(),
            family_id: "mos6502".to_string(),
            default_dialect: Some("mos".to_string()),
        },
    ]
}

fn sample_dialects() -> Vec<DialectDescriptor> {
    vec![
        DialectDescriptor {
            id: "mos".to_string(),
            family_id: "mos6502".to_string(),
            cpu_allow_list: None,
        },
        DialectDescriptor {
            id: "intel".to_string(),
            family_id: "intel8080".to_string(),
            cpu_allow_list: None,
        },
        DialectDescriptor {
            id: "zilog".to_string(),
            family_id: "intel8080".to_string(),
            cpu_allow_list: Some(vec!["z80".to_string(), "Z80".to_string()]),
        },
    ]
}

fn sample_registers() -> Vec<ScopedRegisterDescriptor> {
    vec![
        ScopedRegisterDescriptor {
            owner: ScopedOwner::Family("intel8080".to_string()),
            id: "A".to_string(),
        },
        ScopedRegisterDescriptor {
            owner: ScopedOwner::Family("intel8080".to_string()),
            id: "HL".to_string(),
        },
        ScopedRegisterDescriptor {
            owner: ScopedOwner::Cpu("z80".to_string()),
            id: "IX".to_string(),
        },
        ScopedRegisterDescriptor {
            owner: ScopedOwner::Cpu("z80".to_string()),
            id: "ix".to_string(),
        },
    ]
}

fn sample_forms() -> Vec<ScopedFormDescriptor> {
    vec![
        ScopedFormDescriptor {
            owner: ScopedOwner::Family("intel8080".to_string()),
            mnemonic: "mov".to_string(),
        },
        ScopedFormDescriptor {
            owner: ScopedOwner::Family("intel8080".to_string()),
            mnemonic: "MOV".to_string(),
        },
        ScopedFormDescriptor {
            owner: ScopedOwner::Cpu("z80".to_string()),
            mnemonic: "djnz".to_string(),
        },
        ScopedFormDescriptor {
            owner: ScopedOwner::Dialect("zilog".to_string()),
            mnemonic: "ld".to_string(),
        },
    ]
}

fn sample_tables() -> Vec<VmProgramDescriptor> {
    vec![
        VmProgramDescriptor {
            owner: ScopedOwner::Cpu("m6502".to_string()),
            mnemonic: "lda".to_string(),
            mode_key: "immediate".to_string(),
            program: vec![0x01, 0xA9, 0x02, 0x00, 0xFF],
        },
        VmProgramDescriptor {
            owner: ScopedOwner::Cpu("m6502".to_string()),
            mnemonic: "LDA".to_string(),
            mode_key: "Immediate".to_string(),
            program: vec![0x01, 0xA9, 0x02, 0x00, 0xFF],
        },
    ]
}

fn sample_selectors() -> Vec<ModeSelectorDescriptor> {
    vec![
        ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu("m6502".to_string()),
            mnemonic: "lda".to_string(),
            shape_key: "imm".to_string(),
            mode_key: "immediate".to_string(),
            operand_plan: "expr".to_string(),
            priority: 1,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: ScopedOwner::Dialect("w65c02".to_string()),
            mnemonic: "lda".to_string(),
            shape_key: "zp_indexed".to_string(),
            mode_key: "zero_page_x".to_string(),
            operand_plan: "expr,indexed_x".to_string(),
            priority: 7,
            unstable_widen: true,
            width_rank: 2,
        },
    ]
}

fn assert_scoped_schema_round_trip<T>(
    entries: &[T],
    encode: impl Fn(&[T]) -> Result<Vec<u8>, OpcpuCodecError>,
    decode: impl Fn(&[u8]) -> Result<Vec<T>, OpcpuCodecError>,
) where
    T: Clone + std::fmt::Debug + PartialEq + Eq,
{
    let bytes = encode(entries).expect("encode should succeed");
    let decoded = decode(&bytes).expect("decode should succeed");
    assert_eq!(decoded, entries);
    let reencoded = encode(&decoded).expect("re-encode should succeed");
    assert_eq!(reencoded, bytes);
}

fn sample_metadata() -> PackageMetaDescriptor {
    PackageMetaDescriptor {
        package_id: "opforge.test".to_string(),
        package_version: "9.9.9".to_string(),
        capability_flags: 0xA5A5_5A5A,
    }
}

fn sample_strings() -> Vec<String> {
    vec![
        "mos6502".to_string(),
        "".to_string(),
        "intel8080".to_string(),
    ]
}

fn sample_diagnostics() -> Vec<DiagnosticDescriptor> {
    vec![
        DiagnosticDescriptor {
            code: "pkg001".to_string(),
            message_template: "first diagnostic".to_string(),
        },
        DiagnosticDescriptor {
            code: "pkg002".to_string(),
            message_template: "second diagnostic with {placeholder}".to_string(),
        },
    ]
}

fn sample_cpus_with_optional_default() -> Vec<CpuDescriptor> {
    let mut cpus = sample_cpus();
    cpus.push(CpuDescriptor {
        id: "6510".to_string(),
        family_id: "mos6502".to_string(),
        default_dialect: None,
    });
    cpus
}

#[test]
fn simple_chunk_schema_round_trip_fams() {
    assert_scoped_schema_round_trip(&sample_families(), encode_fams_chunk, decode_fams_chunk);
}

#[test]
fn simple_chunk_schema_round_trip_meta() {
    let metadata = sample_metadata();
    let bytes = encode_meta_chunk(&metadata).expect("encode should succeed");
    let decoded = decode_meta_chunk(&bytes).expect("decode should succeed");
    assert_eq!(decoded, metadata);
    let reencoded = encode_meta_chunk(&decoded).expect("re-encode should succeed");
    assert_eq!(reencoded, bytes);
}

#[test]
fn simple_chunk_schema_round_trip_strs() {
    assert_scoped_schema_round_trip(&sample_strings(), encode_strs_chunk, decode_strs_chunk);
}

#[test]
fn simple_chunk_schema_round_trip_diag() {
    assert_scoped_schema_round_trip(&sample_diagnostics(), encode_diag_chunk, decode_diag_chunk);
}

#[test]
fn simple_chunk_schema_round_trip_cpus() {
    assert_scoped_schema_round_trip(
        &sample_cpus_with_optional_default(),
        encode_cpus_chunk,
        decode_cpus_chunk,
    );
}

#[test]
fn simple_chunk_schema_round_trip_dial() {
    assert_scoped_schema_round_trip(&sample_dialects(), encode_dial_chunk, decode_dial_chunk);
}

fn token_policy_for_test(
    owner: ScopedOwner,
    case_rule: TokenCaseRule,
    identifier_start_class: u32,
    identifier_continue_class: u32,
    punctuation_chars: &str,
) -> TokenPolicyDescriptor {
    let defaults = default_token_policy_lexical_defaults();
    TokenPolicyDescriptor {
        owner,
        case_rule,
        identifier_start_class,
        identifier_continue_class,
        punctuation_chars: punctuation_chars.to_string(),
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

fn sample_token_policies() -> Vec<TokenPolicyDescriptor> {
    vec![
        token_policy_for_test(
            ScopedOwner::Family("MOS6502".to_string()),
            TokenCaseRule::AsciiLower,
            token_identifier_class::ASCII_ALPHA | token_identifier_class::UNDERSCORE,
            token_identifier_class::ASCII_ALPHA
                | token_identifier_class::ASCII_DIGIT
                | token_identifier_class::UNDERSCORE,
            ")(,+-",
        ),
        token_policy_for_test(
            ScopedOwner::Family("mos6502".to_string()),
            TokenCaseRule::AsciiLower,
            token_identifier_class::ASCII_ALPHA | token_identifier_class::UNDERSCORE,
            token_identifier_class::ASCII_ALPHA
                | token_identifier_class::ASCII_DIGIT
                | token_identifier_class::UNDERSCORE,
            "-+(),",
        ),
        token_policy_for_test(
            ScopedOwner::Cpu("z80".to_string()),
            TokenCaseRule::Preserve,
            token_identifier_class::ASCII_ALPHA,
            token_identifier_class::ASCII_ALPHA | token_identifier_class::ASCII_DIGIT,
            "[]()",
        ),
    ]
}

fn tokenizer_vm_program_for_test(owner: ScopedOwner) -> TokenizerVmProgramDescriptor {
    TokenizerVmProgramDescriptor {
        owner,
        opcode_version: TOKENIZER_VM_OPCODE_VERSION_V1,
        start_state: 0,
        state_entry_offsets: vec![0],
        stream: TokenizerVmStreamDescriptor::default(),
        limits: TokenizerVmLimits {
            max_steps_per_line: 2048,
            max_tokens_per_line: 256,
            max_lexeme_bytes: 256,
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
        program: vec![TokenizerVmOpcode::End as u8],
    }
}

fn sample_tokenizer_vm_programs() -> Vec<TokenizerVmProgramDescriptor> {
    vec![
        tokenizer_vm_program_for_test(ScopedOwner::Family("MOS6502".to_string())),
        tokenizer_vm_program_for_test(ScopedOwner::Family("mos6502".to_string())),
        tokenizer_vm_program_for_test(ScopedOwner::Cpu("z80".to_string())),
    ]
}

fn parser_contract_for_test(owner: ScopedOwner) -> ParserContractDescriptor {
    ParserContractDescriptor {
        owner,
        grammar_id: PARSER_GRAMMAR_ID_LINE_V1.to_string(),
        ast_schema_id: PARSER_AST_SCHEMA_ID_LINE_V1.to_string(),
        opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
        max_ast_nodes_per_line: 256,
        diagnostics: ParserDiagnosticMap {
            unexpected_token: DIAG_PARSER_UNEXPECTED_TOKEN.to_string(),
            expected_expression: DIAG_PARSER_EXPECTED_EXPRESSION.to_string(),
            expected_operand: DIAG_PARSER_EXPECTED_OPERAND.to_string(),
            invalid_statement: DIAG_PARSER_INVALID_STATEMENT.to_string(),
        },
    }
}

fn sample_parser_contracts() -> Vec<ParserContractDescriptor> {
    vec![
        parser_contract_for_test(ScopedOwner::Family("MOS6502".to_string())),
        parser_contract_for_test(ScopedOwner::Family("mos6502".to_string())),
        parser_contract_for_test(ScopedOwner::Cpu("z80".to_string())),
    ]
}

fn parser_vm_program_for_test(owner: ScopedOwner) -> ParserVmProgramDescriptor {
    ParserVmProgramDescriptor {
        owner,
        opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
        program: vec![
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
        ],
    }
}

fn sample_parser_vm_programs() -> Vec<ParserVmProgramDescriptor> {
    vec![
        parser_vm_program_for_test(ScopedOwner::Family("MOS6502".to_string())),
        parser_vm_program_for_test(ScopedOwner::Family("mos6502".to_string())),
        parser_vm_program_for_test(ScopedOwner::Cpu("z80".to_string())),
    ]
}

#[test]
fn parser_vm_opcode_byte_round_trip_is_stable() {
    let opcodes = [
        (0x00, ParserVmOpcode::End),
        (0x01, ParserVmOpcode::Jump),
        (0x02, ParserVmOpcode::JumpIfTrue),
        (0x03, ParserVmOpcode::JumpIfFalse),
        (0x04, ParserVmOpcode::Checkpoint),
        (0x05, ParserVmOpcode::Rollback),
        (0x06, ParserVmOpcode::Commit),
        (0x10, ParserVmOpcode::PeekKind),
        (0x11, ParserVmOpcode::PeekIdentifier),
        (0x12, ParserVmOpcode::PeekOperator),
        (0x13, ParserVmOpcode::IsEol),
        (0x14, ParserVmOpcode::PeekAssignmentOperator),
        (0x15, ParserVmOpcode::PeekStarOrg),
        (0x20, ParserVmOpcode::Advance),
        (0x21, ParserVmOpcode::ConsumeKind),
        (0x22, ParserVmOpcode::ConsumeOperator),
        (0x30, ParserVmOpcode::LoadIdentifier),
        (0x31, ParserVmOpcode::LoadSpan),
        (0x32, ParserVmOpcode::LoadTokenText),
        (0x33, ParserVmOpcode::LoadInlineText),
        (0x40, ParserVmOpcode::ParseOptionalLeadingLabel),
        (0x41, ParserVmOpcode::ScanTopLevelCommaBoundaries),
        (0x42, ParserVmOpcode::RequireNoTrailingTokens),
        (0x50, ParserVmOpcode::ParseOperandExprRange),
        (0x60, ParserVmOpcode::BeginStatement),
        (0x61, ParserVmOpcode::SetLabel),
        (0x62, ParserVmOpcode::SetMnemonic),
        (0x63, ParserVmOpcode::PushOperand),
        (0x64, ParserVmOpcode::FinishLine),
        (0x65, ParserVmOpcode::SetDotMnemonic),
        (0x66, ParserVmOpcode::FinishAssignment),
        (0x70, ParserVmOpcode::EmitDiag),
        (0x71, ParserVmOpcode::EmitDiagIfNoResult),
        (0x72, ParserVmOpcode::Fail),
    ];

    for (byte, opcode) in opcodes {
        assert_eq!(ParserVmOpcode::from_u8(byte), Some(opcode));
        assert_eq!(opcode as u8, byte);
    }
    assert_eq!(ParserVmOpcode::from_u8(0x07), None);
    assert_eq!(ParserVmOpcode::from_u8(0x43), None);
    assert_eq!(ParserVmOpcode::from_u8(0x80), None);
}

fn expr_contract_for_test(owner: ScopedOwner) -> ExprContractDescriptor {
    ExprContractDescriptor {
        owner,
        opcode_version: EXPR_VM_OPCODE_VERSION_V1,
        max_program_bytes: 2048,
        max_stack_depth: 64,
        max_symbol_refs: 128,
        max_eval_steps: 2048,
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

fn sample_expr_contracts() -> Vec<ExprContractDescriptor> {
    vec![
        expr_contract_for_test(ScopedOwner::Family("MOS6502".to_string())),
        expr_contract_for_test(ScopedOwner::Family("mos6502".to_string())),
        expr_contract_for_test(ScopedOwner::Cpu("z80".to_string())),
    ]
}

fn expr_parser_contract_for_test(owner: ScopedOwner) -> ExprParserContractDescriptor {
    ExprParserContractDescriptor {
        owner,
        opcode_version: EXVM_OPCODE_VERSION_V1,
        diagnostics: ExprParserDiagnosticMap {
            invalid_expression_program: DIAG_PARSER_INVALID_STATEMENT.to_string(),
        },
    }
}

fn sample_expr_parser_contracts() -> Vec<ExprParserContractDescriptor> {
    vec![
        expr_parser_contract_for_test(ScopedOwner::Family("MOS6502".to_string())),
        expr_parser_contract_for_test(ScopedOwner::Family("mos6502".to_string())),
        expr_parser_contract_for_test(ScopedOwner::Cpu("z80".to_string())),
    ]
}

#[test]
fn encode_decode_round_trip_is_deterministic() {
    let bytes = encode_hierarchy_chunks(
        &sample_families(),
        &sample_cpus(),
        &sample_dialects(),
        &sample_registers(),
        &sample_forms(),
        &sample_tables(),
    )
    .expect("encode should succeed");
    let decoded = decode_hierarchy_chunks(&bytes).expect("decode should succeed");
    let reencoded = encode_hierarchy_chunks(
        &decoded.families,
        &decoded.cpus,
        &decoded.dialects,
        &decoded.registers,
        &decoded.forms,
        &decoded.tables,
    )
    .expect("re-encode should succeed");
    assert_eq!(bytes, reencoded);
}

#[test]
fn load_hierarchy_package_validates_and_resolves() {
    let bytes = encode_hierarchy_chunks(
        &sample_families(),
        &sample_cpus(),
        &sample_dialects(),
        &sample_registers(),
        &sample_forms(),
        &sample_tables(),
    )
    .expect("encode should succeed");
    let package = load_hierarchy_package(&bytes).expect("load should succeed");

    let resolved_8085 = package
        .resolve_pipeline("8085", None)
        .expect("8085 should resolve");
    assert_eq!(resolved_8085.dialect_id, "intel");

    let resolved_z80 = package
        .resolve_pipeline("z80", None)
        .expect("z80 should resolve");
    assert_eq!(resolved_z80.dialect_id, "zilog");
}

#[test]
fn encoding_is_stable_across_input_order() {
    let mut families = sample_families();
    families.reverse();
    let mut cpus = sample_cpus();
    cpus.reverse();
    let mut dialects = sample_dialects();
    dialects.reverse();
    let mut registers = sample_registers();
    registers.reverse();
    let mut forms = sample_forms();
    forms.reverse();
    let mut tables = sample_tables();
    tables.reverse();

    let a = encode_hierarchy_chunks(
        &sample_families(),
        &sample_cpus(),
        &sample_dialects(),
        &sample_registers(),
        &sample_forms(),
        &sample_tables(),
    )
    .expect("ordered encode should succeed");
    let b = encode_hierarchy_chunks(&families, &cpus, &dialects, &registers, &forms, &tables)
        .expect("shuffled encode should succeed");
    assert_eq!(a, b);
}

#[test]
fn metadata_snapshot_is_stable() {
    let bytes = encode_hierarchy_chunks(
        &sample_families(),
        &sample_cpus(),
        &sample_dialects(),
        &sample_registers(),
        &sample_forms(),
        &sample_tables(),
    )
    .expect("encode should succeed");
    let decoded = decode_hierarchy_chunks(&bytes).expect("decode should succeed");
    assert_eq!(decoded.metadata.package_id, "opforge.generated");
    assert_eq!(decoded.metadata.package_version, "0.1.0");
    assert_eq!(decoded.metadata.capability_flags, 0);
    assert!(decoded.strings.is_empty());
    assert!(decoded.diagnostics.is_empty());
    assert!(decoded.token_policies.is_empty());
    assert!(decoded.parser_contracts.is_empty());
    assert!(decoded.parser_vm_programs.is_empty());
    assert!(decoded.expr_contracts.is_empty());

    let family_snapshot: Vec<String> = decoded
        .families
        .iter()
        .map(|entry| format!("{}->{}", entry.id, entry.canonical_dialect))
        .collect();
    assert_eq!(family_snapshot, vec!["intel8080->intel", "mos6502->mos"]);

    let cpu_snapshot: Vec<String> = decoded
        .cpus
        .iter()
        .map(|entry| {
            format!(
                "{}:{}:{}",
                entry.id,
                entry.family_id,
                entry.default_dialect.as_deref().unwrap_or("-")
            )
        })
        .collect();
    assert_eq!(
        cpu_snapshot,
        vec![
            "6502:mos6502:mos",
            "8085:intel8080:intel",
            "z80:intel8080:zilog"
        ]
    );

    let dialect_snapshot: Vec<String> = decoded
        .dialects
        .iter()
        .map(|entry| format!("{}:{}", entry.family_id, entry.id))
        .collect();
    assert_eq!(
        dialect_snapshot,
        vec!["intel8080:intel", "intel8080:zilog", "mos6502:mos"]
    );
}

#[test]
fn encode_decode_round_trip_scoped_schema_registers() {
    let entries = sample_registers();
    assert_scoped_schema_round_trip(&entries, encode_regs_chunk, decode_regs_chunk);
}

#[test]
fn encode_decode_round_trip_scoped_schema_forms() {
    let entries = sample_forms();
    assert_scoped_schema_round_trip(&entries, encode_form_chunk, decode_form_chunk);
}

#[test]
fn encode_decode_round_trip_scoped_schema_tables() {
    let entries = sample_tables();
    assert_scoped_schema_round_trip(&entries, encode_tabl_chunk, decode_tabl_chunk);
}

#[test]
fn encode_decode_round_trip_scoped_schema_selectors() {
    let entries = sample_selectors();
    assert_scoped_schema_round_trip(&entries, encode_msel_chunk, decode_msel_chunk);
}

#[test]
fn encode_decode_round_trip_contract_schema_tokenizer_vm_programs() {
    let entries = sample_tokenizer_vm_programs();
    assert_scoped_schema_round_trip(&entries, encode_tkvm_chunk, decode_tkvm_chunk);
}

#[test]
fn encode_decode_round_trip_contract_schema_parser_contracts() {
    let entries = sample_parser_contracts();
    assert_scoped_schema_round_trip(&entries, encode_pars_chunk, decode_pars_chunk);
}

#[test]
fn encode_decode_round_trip_contract_schema_parser_vm_programs() {
    let entries = sample_parser_vm_programs();
    assert_scoped_schema_round_trip(&entries, encode_prvm_chunk, decode_prvm_chunk);
}

#[test]
fn encode_decode_round_trip_contract_schema_expr_contracts() {
    let entries = sample_expr_contracts();
    assert_scoped_schema_round_trip(&entries, encode_expr_chunk, decode_expr_chunk);
}

#[test]
fn encode_decode_round_trip_contract_schema_expr_parser_contracts() {
    let entries = sample_expr_parser_contracts();
    assert_scoped_schema_round_trip(&entries, encode_exvm_chunk, decode_exvm_chunk);
}

#[test]
fn toc_snapshot_is_stable() {
    let bytes = encode_hierarchy_chunks(
        &sample_families(),
        &sample_cpus(),
        &sample_dialects(),
        &sample_registers(),
        &sample_forms(),
        &sample_tables(),
    )
    .expect("encode should succeed");

    let toc_count = u16::from_le_bytes([bytes[8], bytes[9]]) as usize;
    let mut toc_entries = Vec::new();
    for idx in 0..toc_count {
        let base = HEADER_SIZE + idx * TOC_ENTRY_SIZE;
        let chunk_id = String::from_utf8_lossy(&bytes[base..base + 4]).to_string();
        let offset = u32::from_le_bytes([
            bytes[base + 4],
            bytes[base + 5],
            bytes[base + 6],
            bytes[base + 7],
        ]);
        let length = u32::from_le_bytes([
            bytes[base + 8],
            bytes[base + 9],
            bytes[base + 10],
            bytes[base + 11],
        ]);
        toc_entries.push(format!("{}@{}+{}", chunk_id, offset, length));
    }

    assert_eq!(
        toc_entries,
        vec![
            "META@132+34",
            "STRS@166+4",
            "DIAG@170+4",
            "FAMS@174+44",
            "CPUS@218+92",
            "DIAL@310+80",
            "REGS@390+57",
            "FORM@447+57",
            "TABL@504+43",
            "MSEL@547+4"
        ]
    );
}

#[test]
fn ultimate64_abi_header_is_little_endian_v1() {
    let chunks = HierarchyChunks {
        metadata: PackageMetaDescriptor::default(),
        strings: Vec::new(),
        diagnostics: default_runtime_diagnostic_catalog(),
        token_policies: sample_token_policies(),
        tokenizer_vm_programs: sample_tokenizer_vm_programs(),
        parser_contracts: sample_parser_contracts(),
        parser_vm_programs: sample_parser_vm_programs(),
        expr_contracts: sample_expr_contracts(),
        expr_parser_contracts: sample_expr_parser_contracts(),
        families: sample_families(),
        cpus: sample_cpus(),
        dialects: sample_dialects(),
        registers: sample_registers(),
        forms: sample_forms(),
        tables: sample_tables(),
        selectors: Vec::new(),
    };
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).expect("encode should succeed");

    assert_eq!(&bytes[0..4], OPASM_MAGIC.as_slice());
    assert_eq!(&bytes[4..6], OPASM_VERSION_V1.to_le_bytes().as_slice());
    assert_eq!(&bytes[6..8], OPASM_ENDIAN_MARKER.to_le_bytes().as_slice());
    assert_eq!(u16::from_le_bytes([bytes[4], bytes[5]]), OPASM_VERSION_V1);
    assert_eq!(
        u16::from_le_bytes([bytes[6], bytes[7]]),
        OPASM_ENDIAN_MARKER
    );
}

#[test]
fn ultimate64_abi_toc_payload_layout_is_contiguous() {
    let chunks = HierarchyChunks {
        metadata: PackageMetaDescriptor::default(),
        strings: Vec::new(),
        diagnostics: default_runtime_diagnostic_catalog(),
        token_policies: sample_token_policies(),
        tokenizer_vm_programs: sample_tokenizer_vm_programs(),
        parser_contracts: sample_parser_contracts(),
        parser_vm_programs: sample_parser_vm_programs(),
        expr_contracts: sample_expr_contracts(),
        expr_parser_contracts: sample_expr_parser_contracts(),
        families: sample_families(),
        cpus: sample_cpus(),
        dialects: sample_dialects(),
        registers: sample_registers(),
        forms: sample_forms(),
        tables: sample_tables(),
        selectors: Vec::new(),
    };
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).expect("encode should succeed");
    let toc = parse_toc(&bytes).expect("TOC parse should succeed");
    let mut entries: Vec<TocEntry> = toc.values().copied().collect();
    entries.sort_by_key(|entry| entry.offset);
    assert!(!entries.is_empty(), "expected non-empty TOC entries");
    for idx in 1..entries.len() {
        let prev = entries[idx - 1];
        let current = entries[idx];
        assert_eq!(
            prev.offset.saturating_add(prev.length),
            current.offset,
            "expected contiguous payload layout for TOC entries"
        );
    }
    let last = entries.last().expect("entries not empty");
    let end = usize::try_from(last.offset.saturating_add(last.length))
        .expect("payload end must fit usize");
    assert_eq!(end, bytes.len());
}

#[test]
fn ultimate64_abi_default_diag_catalog_covers_parser_and_tokenizer_codes() {
    let diagnostics = default_runtime_diagnostic_catalog();
    let mut codes: Vec<String> = diagnostics.iter().map(|entry| entry.code.clone()).collect();
    codes.sort();
    assert!(codes.iter().any(|code| code == DIAG_TOKENIZER_INVALID_CHAR));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_TOKENIZER_UNTERMINATED_STRING));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_TOKENIZER_STEP_LIMIT_EXCEEDED));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_TOKENIZER_TOKEN_LIMIT_EXCEEDED));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_TOKENIZER_LEXEME_LIMIT_EXCEEDED));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_TOKENIZER_ERROR_LIMIT_EXCEEDED));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_PARSER_UNEXPECTED_TOKEN));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_PARSER_EXPECTED_EXPRESSION));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_PARSER_EXPECTED_OPERAND));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_PARSER_INVALID_STATEMENT));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_ASM_GENERIC_ERRORS_DETECTED));
    assert!(codes.iter().any(|code| code == DIAG_ASM_CLI_ERROR));
    assert!(codes.iter().any(|code| code == DIAG_ASM_PREPROCESS_ERROR));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_ASM_CONDITIONAL_STRUCTURE));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_ASM_DIRECTIVE_STRUCTURE));
    assert!(codes.iter().any(|code| code == DIAG_ASM_SYMBOL_ERROR));
    assert!(codes.iter().any(|code| code == DIAG_ASM_EXPRESSION_ERROR));
    assert!(codes.iter().any(|code| code == DIAG_ASM_INSTRUCTION_ERROR));
    assert!(codes.iter().any(|code| code == DIAG_ASM_IO_ERROR));
    assert!(codes.iter().any(|code| code == DIAG_EXPR_INVALID_OPCODE));
    assert!(codes.iter().any(|code| code == DIAG_EXPR_STACK_UNDERFLOW));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_EXPR_STACK_DEPTH_EXCEEDED));
    assert!(codes.iter().any(|code| code == DIAG_EXPR_UNKNOWN_SYMBOL));
    assert!(codes.iter().any(|code| code == DIAG_EXPR_EVAL_FAILURE));
    assert!(codes
        .iter()
        .any(|code| code == DIAG_EXPR_UNSUPPORTED_FEATURE));
    assert!(codes.iter().any(|code| code == DIAG_EXPR_BUDGET_EXCEEDED));
    assert!(codes.iter().any(|code| code == DIAG_EXPR_INVALID_PROGRAM));
}

#[test]
fn decode_rejects_missing_required_chunk() {
    let bytes = encode_container(&[]).expect("container encode should succeed");
    let err = decode_hierarchy_chunks(&bytes).expect_err("missing FAMS should fail");
    assert!(matches!(err, OpcpuCodecError::MissingRequiredChunk { .. }));
    assert_eq!(err.code(), "OPC006");
}

#[test]
fn decode_rejects_truncated_payload() {
    let mut bytes = encode_hierarchy_chunks(
        &sample_families(),
        &sample_cpus(),
        &sample_dialects(),
        &sample_registers(),
        &sample_forms(),
        &sample_tables(),
    )
    .expect("encode should succeed");
    bytes.pop();
    let err = decode_hierarchy_chunks(&bytes).expect_err("truncated payload should fail");
    assert!(matches!(
        err,
        OpcpuCodecError::ChunkOutOfBounds { .. } | OpcpuCodecError::UnexpectedEof { .. }
    ));
}

#[test]
fn decode_rejects_invalid_endian_marker() {
    let mut bytes = encode_hierarchy_chunks(
        &sample_families(),
        &sample_cpus(),
        &sample_dialects(),
        &sample_registers(),
        &sample_forms(),
        &sample_tables(),
    )
    .expect("encode should succeed");
    bytes[6] = 0x78;
    bytes[7] = 0x56;
    let err = decode_hierarchy_chunks(&bytes).expect_err("invalid marker should fail");
    assert!(matches!(
        err,
        OpcpuCodecError::InvalidEndiannessMarker { .. }
    ));
    assert_eq!(err.code(), "OPC003");
}

#[test]
fn load_rejects_cross_reference_errors() {
    let families = vec![FamilyDescriptor {
        id: "intel8080".to_string(),
        canonical_dialect: "intel".to_string(),
    }];
    let cpus = vec![CpuDescriptor {
        id: "8085".to_string(),
        family_id: "missing".to_string(),
        default_dialect: Some("intel".to_string()),
    }];
    let dials = vec![DialectDescriptor {
        id: "intel".to_string(),
        family_id: "intel8080".to_string(),
        cpu_allow_list: None,
    }];
    let chunks = vec![
        (CHUNK_FAMS, encode_fams_chunk(&families).expect("fams")),
        (CHUNK_CPUS, encode_cpus_chunk(&cpus).expect("cpus")),
        (CHUNK_DIAL, encode_dial_chunk(&dials).expect("dial")),
        (CHUNK_REGS, encode_regs_chunk(&[]).expect("regs")),
        (CHUNK_FORM, encode_form_chunk(&[]).expect("form")),
        (CHUNK_TABL, encode_tabl_chunk(&[]).expect("tabl")),
    ];
    let bytes = encode_container(&chunks).expect("container");

    let err = load_hierarchy_package(&bytes).expect_err("cross-reference should fail");
    assert!(matches!(err, OpcpuCodecError::Hierarchy(_)));
    assert_eq!(err.code(), "OPC011");
}

#[test]
fn decode_legacy_container_defaults_meta_strs_diag() {
    let families = sample_families();
    let cpus = sample_cpus();
    let dials = sample_dialects();
    let chunks = vec![
        (CHUNK_FAMS, encode_fams_chunk(&families).expect("fams")),
        (CHUNK_CPUS, encode_cpus_chunk(&cpus).expect("cpus")),
        (CHUNK_DIAL, encode_dial_chunk(&dials).expect("dial")),
        (CHUNK_REGS, encode_regs_chunk(&[]).expect("regs")),
        (CHUNK_FORM, encode_form_chunk(&[]).expect("form")),
        (CHUNK_TABL, encode_tabl_chunk(&[]).expect("tabl")),
    ];
    let bytes = encode_container(&chunks).expect("container");
    let decoded = decode_hierarchy_chunks(&bytes).expect("decode");
    assert_eq!(decoded.metadata, PackageMetaDescriptor::default());
    assert!(decoded.strings.is_empty());
    assert!(decoded.diagnostics.is_empty());
    assert!(decoded.token_policies.is_empty());
    assert!(decoded.parser_contracts.is_empty());
    assert!(decoded.parser_vm_programs.is_empty());
    assert!(decoded.expr_contracts.is_empty());
    assert!(decoded.expr_parser_contracts.is_empty());
}

#[test]
fn encode_decode_round_trip_preserves_toks_policy() {
    let chunks = HierarchyChunks {
        metadata: PackageMetaDescriptor::default(),
        strings: Vec::new(),
        diagnostics: Vec::new(),
        token_policies: sample_token_policies(),
        tokenizer_vm_programs: Vec::new(),
        parser_contracts: Vec::new(),
        parser_vm_programs: Vec::new(),
        expr_contracts: Vec::new(),
        expr_parser_contracts: Vec::new(),
        families: sample_families(),
        cpus: sample_cpus(),
        dialects: sample_dialects(),
        registers: sample_registers(),
        forms: sample_forms(),
        tables: sample_tables(),
        selectors: Vec::new(),
    };
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).expect("encode should succeed");
    let decoded = decode_hierarchy_chunks(&bytes).expect("decode should succeed");

    assert_eq!(decoded.token_policies.len(), 2);
    assert!(matches!(
        &decoded.token_policies[0].owner,
        ScopedOwner::Family(owner) if owner == "mos6502"
    ));
    assert_eq!(
        decoded.token_policies[0].case_rule,
        TokenCaseRule::AsciiLower
    );
    assert_eq!(decoded.token_policies[0].punctuation_chars, "()+,-");
    assert_eq!(decoded.token_policies[0].comment_prefix, ";");
    assert_eq!(decoded.token_policies[0].quote_chars, "\"'");
    assert_eq!(decoded.token_policies[0].escape_char, Some('\\'));
    assert_eq!(decoded.token_policies[0].number_prefix_chars, "$%@");
    assert_eq!(
        decoded.token_policies[0].multi_char_operators,
        vec!["!=", "&&", "**", "<<", "<=", "<>", "==", ">=", ">>", "^^", "||"]
    );

    assert!(matches!(
        &decoded.token_policies[1].owner,
        ScopedOwner::Cpu(owner) if owner == "z80"
    ));
    assert_eq!(decoded.token_policies[1].case_rule, TokenCaseRule::Preserve);
    assert_eq!(decoded.token_policies[1].punctuation_chars, "()[]");
}

#[test]
fn encode_decode_round_trip_preserves_toks_policy_chunk_schema() {
    assert_scoped_schema_round_trip(
        &sample_token_policies(),
        encode_toks_chunk,
        decode_toks_chunk,
    );
}

#[test]
fn encode_decode_round_trip_preserves_parser_contracts() {
    let chunks = HierarchyChunks {
        metadata: PackageMetaDescriptor::default(),
        strings: Vec::new(),
        diagnostics: Vec::new(),
        token_policies: Vec::new(),
        tokenizer_vm_programs: Vec::new(),
        parser_contracts: sample_parser_contracts(),
        parser_vm_programs: Vec::new(),
        expr_contracts: Vec::new(),
        expr_parser_contracts: Vec::new(),
        families: sample_families(),
        cpus: sample_cpus(),
        dialects: sample_dialects(),
        registers: sample_registers(),
        forms: sample_forms(),
        tables: sample_tables(),
        selectors: Vec::new(),
    };
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).expect("encode should succeed");
    let decoded = decode_hierarchy_chunks(&bytes).expect("decode should succeed");

    assert_eq!(decoded.parser_contracts.len(), 2);
    assert!(matches!(
        &decoded.parser_contracts[0].owner,
        ScopedOwner::Family(owner) if owner == "mos6502"
    ));
    assert_eq!(
        decoded.parser_contracts[0].grammar_id,
        PARSER_GRAMMAR_ID_LINE_V1
    );
    assert_eq!(
        decoded.parser_contracts[0].ast_schema_id,
        PARSER_AST_SCHEMA_ID_LINE_V1
    );
    assert_eq!(
        decoded.parser_contracts[0].opcode_version,
        PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT
    );
    assert_eq!(decoded.parser_contracts[0].max_ast_nodes_per_line, 256);
    assert_eq!(
        decoded.parser_contracts[0].diagnostics.unexpected_token,
        "otp001"
    );

    assert!(matches!(
        &decoded.parser_contracts[1].owner,
        ScopedOwner::Cpu(owner) if owner == "z80"
    ));
}

#[test]
fn encode_decode_round_trip_preserves_parser_vm_programs() {
    let chunks = HierarchyChunks {
        metadata: PackageMetaDescriptor::default(),
        strings: Vec::new(),
        diagnostics: Vec::new(),
        token_policies: Vec::new(),
        tokenizer_vm_programs: Vec::new(),
        parser_contracts: Vec::new(),
        parser_vm_programs: sample_parser_vm_programs(),
        expr_contracts: Vec::new(),
        expr_parser_contracts: Vec::new(),
        families: sample_families(),
        cpus: sample_cpus(),
        dialects: sample_dialects(),
        registers: sample_registers(),
        forms: sample_forms(),
        tables: sample_tables(),
        selectors: Vec::new(),
    };
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).expect("encode should succeed");
    let decoded = decode_hierarchy_chunks(&bytes).expect("decode should succeed");

    assert_eq!(decoded.parser_vm_programs.len(), 2);
    assert!(matches!(
        &decoded.parser_vm_programs[0].owner,
        ScopedOwner::Family(owner) if owner == "mos6502"
    ));
    assert_eq!(
        decoded.parser_vm_programs[0].opcode_version,
        PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT
    );
    assert_eq!(
        decoded.parser_vm_programs[0].program,
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
            ParserVmOpcodeV2::End as u8
        ]
    );
    assert!(matches!(
        &decoded.parser_vm_programs[1].owner,
        ScopedOwner::Cpu(owner) if owner == "z80"
    ));
}

#[test]
fn encode_decode_round_trip_preserves_expr_contracts() {
    let chunks = HierarchyChunks {
        metadata: PackageMetaDescriptor::default(),
        strings: Vec::new(),
        diagnostics: Vec::new(),
        token_policies: Vec::new(),
        tokenizer_vm_programs: Vec::new(),
        parser_contracts: Vec::new(),
        parser_vm_programs: Vec::new(),
        expr_contracts: sample_expr_contracts(),
        expr_parser_contracts: Vec::new(),
        families: sample_families(),
        cpus: sample_cpus(),
        dialects: sample_dialects(),
        registers: sample_registers(),
        forms: sample_forms(),
        tables: sample_tables(),
        selectors: Vec::new(),
    };
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).expect("encode should succeed");
    let decoded = decode_hierarchy_chunks(&bytes).expect("decode should succeed");

    assert_eq!(decoded.expr_contracts.len(), 2);
    assert!(matches!(
        &decoded.expr_contracts[0].owner,
        ScopedOwner::Family(owner) if owner == "mos6502"
    ));
    assert_eq!(
        decoded.expr_contracts[0].opcode_version,
        EXPR_VM_OPCODE_VERSION_V1
    );
    assert_eq!(decoded.expr_contracts[0].max_program_bytes, 2048);
    assert_eq!(
        decoded.expr_contracts[0].diagnostics.invalid_opcode,
        "ope001"
    );
    assert!(matches!(
        &decoded.expr_contracts[1].owner,
        ScopedOwner::Cpu(owner) if owner == "z80"
    ));
}

#[test]
fn encode_decode_round_trip_preserves_expr_parser_contracts() {
    let chunks = HierarchyChunks {
        metadata: PackageMetaDescriptor::default(),
        strings: Vec::new(),
        diagnostics: Vec::new(),
        token_policies: Vec::new(),
        tokenizer_vm_programs: Vec::new(),
        parser_contracts: Vec::new(),
        parser_vm_programs: Vec::new(),
        expr_contracts: Vec::new(),
        expr_parser_contracts: sample_expr_parser_contracts(),
        families: sample_families(),
        cpus: sample_cpus(),
        dialects: sample_dialects(),
        registers: sample_registers(),
        forms: sample_forms(),
        tables: sample_tables(),
        selectors: Vec::new(),
    };
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).expect("encode should succeed");
    let decoded = decode_hierarchy_chunks(&bytes).expect("decode should succeed");

    assert_eq!(decoded.expr_parser_contracts.len(), 2);
    assert!(matches!(
        &decoded.expr_parser_contracts[0].owner,
        ScopedOwner::Family(owner) if owner == "mos6502"
    ));
    assert_eq!(
        decoded.expr_parser_contracts[0].opcode_version,
        EXVM_OPCODE_VERSION_V1
    );
    assert_eq!(
        decoded.expr_parser_contracts[0]
            .diagnostics
            .invalid_expression_program,
        "otp004"
    );
    assert!(matches!(
        &decoded.expr_parser_contracts[1].owner,
        ScopedOwner::Cpu(owner) if owner == "z80"
    ));
}

#[test]
fn decode_rejects_invalid_toks_case_rule() {
    let families = sample_families();
    let cpus = sample_cpus();
    let dials = sample_dialects();
    let mut toks = Vec::new();
    write_u32(&mut toks, 1);
    toks.push(0);
    write_string(&mut toks, "TOKS", "mos6502").expect("owner");
    toks.push(9);
    write_u32(
        &mut toks,
        token_identifier_class::ASCII_ALPHA | token_identifier_class::UNDERSCORE,
    );
    write_u32(
        &mut toks,
        token_identifier_class::ASCII_ALPHA
            | token_identifier_class::ASCII_DIGIT
            | token_identifier_class::UNDERSCORE,
    );
    write_string(&mut toks, "TOKS", ",").expect("punctuation");
    let chunks = vec![
        (CHUNK_TOKS, toks),
        (CHUNK_FAMS, encode_fams_chunk(&families).expect("fams")),
        (CHUNK_CPUS, encode_cpus_chunk(&cpus).expect("cpus")),
        (CHUNK_DIAL, encode_dial_chunk(&dials).expect("dial")),
        (CHUNK_REGS, encode_regs_chunk(&[]).expect("regs")),
        (CHUNK_FORM, encode_form_chunk(&[]).expect("form")),
        (CHUNK_TABL, encode_tabl_chunk(&[]).expect("tabl")),
    ];
    let bytes = encode_container(&chunks).expect("container");
    let err = decode_hierarchy_chunks(&bytes).expect_err("invalid case rule should fail");
    assert!(matches!(err, OpcpuCodecError::InvalidChunkFormat { .. }));
    assert_eq!(err.code(), "OPC009");
}

#[test]
fn decode_rejects_bounded_count_overflow_before_allocation() {
    let mut fams = Vec::new();
    write_u32(&mut fams, u32::MAX);

    let err = decode_fams_chunk(&fams).expect_err("oversized count should be rejected");
    assert!(matches!(err, OpcpuCodecError::InvalidChunkFormat { .. }));
    assert!(
        err.to_string().contains("family entry count") || err.to_string().contains("family entry")
    );
}

#[test]
fn decode_rejects_hard_limited_count_before_allocation() {
    let count = MAX_DECODE_ENTRY_COUNT + 1;
    let mut fams = Vec::new();
    write_u32(&mut fams, count as u32);
    fams.resize(4 + (count * 8), 0);

    let err = decode_fams_chunk(&fams).expect_err("hard-limited count should be rejected");
    assert!(matches!(err, OpcpuCodecError::InvalidChunkFormat { .. }));
    assert!(err.to_string().contains("hard limit"));
}

#[test]
fn decode_rejects_invalid_msel_unstable_widen_flag() {
    let mut msel = Vec::new();
    write_u32(&mut msel, 1);
    msel.push(0);
    write_string(&mut msel, "MSEL", "mos6502").expect("owner");
    write_string(&mut msel, "MSEL", "lda").expect("mnemonic");
    write_string(&mut msel, "MSEL", "shape").expect("shape");
    write_string(&mut msel, "MSEL", "mode").expect("mode");
    write_string(&mut msel, "MSEL", "plan").expect("plan");
    msel.extend_from_slice(&0u16.to_le_bytes());
    msel.push(2);
    msel.push(0);

    let chunks = vec![
        (CHUNK_MSEL, msel),
        (
            CHUNK_FAMS,
            encode_fams_chunk(&sample_families()).expect("fams"),
        ),
        (CHUNK_CPUS, encode_cpus_chunk(&sample_cpus()).expect("cpus")),
        (
            CHUNK_DIAL,
            encode_dial_chunk(&sample_dialects()).expect("dial"),
        ),
        (CHUNK_REGS, encode_regs_chunk(&[]).expect("regs")),
        (CHUNK_FORM, encode_form_chunk(&[]).expect("form")),
        (CHUNK_TABL, encode_tabl_chunk(&[]).expect("tabl")),
    ];
    let bytes = encode_container(&chunks).expect("container");

    let err = decode_hierarchy_chunks(&bytes).expect_err("invalid unstable_widen should fail");
    assert!(matches!(err, OpcpuCodecError::InvalidChunkFormat { .. }));
    assert!(err.to_string().contains("unstable_widen"));
}

#[test]
fn decode_rejects_truncated_msel_payload() {
    let mut msel = Vec::new();
    write_u32(&mut msel, 1);
    msel.push(0);
    write_string(&mut msel, "MSEL", "mos6502").expect("owner");

    let chunks = vec![
        (CHUNK_MSEL, msel),
        (
            CHUNK_FAMS,
            encode_fams_chunk(&sample_families()).expect("fams"),
        ),
        (CHUNK_CPUS, encode_cpus_chunk(&sample_cpus()).expect("cpus")),
        (
            CHUNK_DIAL,
            encode_dial_chunk(&sample_dialects()).expect("dial"),
        ),
        (CHUNK_REGS, encode_regs_chunk(&[]).expect("regs")),
        (CHUNK_FORM, encode_form_chunk(&[]).expect("form")),
        (CHUNK_TABL, encode_tabl_chunk(&[]).expect("tabl")),
    ];
    let bytes = encode_container(&chunks).expect("container");

    let err = decode_hierarchy_chunks(&bytes).expect_err("truncated MSEL should fail");
    assert!(matches!(
        err,
        OpcpuCodecError::InvalidChunkFormat { .. } | OpcpuCodecError::UnexpectedEof { .. }
    ));
    assert!(err.to_string().contains("MSEL"));
}

#[test]
fn decode_rejects_invalid_msel_owner_tag() {
    let mut msel = Vec::new();
    write_u32(&mut msel, 1);
    msel.push(9);
    write_string(&mut msel, "MSEL", "mos6502").expect("owner");
    write_string(&mut msel, "MSEL", "lda").expect("mnemonic");
    write_string(&mut msel, "MSEL", "shape").expect("shape");
    write_string(&mut msel, "MSEL", "mode").expect("mode");
    write_string(&mut msel, "MSEL", "plan").expect("plan");
    msel.extend_from_slice(&0u16.to_le_bytes());
    msel.push(0);
    msel.push(0);

    let chunks = vec![
        (CHUNK_MSEL, msel),
        (
            CHUNK_FAMS,
            encode_fams_chunk(&sample_families()).expect("fams"),
        ),
        (CHUNK_CPUS, encode_cpus_chunk(&sample_cpus()).expect("cpus")),
        (
            CHUNK_DIAL,
            encode_dial_chunk(&sample_dialects()).expect("dial"),
        ),
        (CHUNK_REGS, encode_regs_chunk(&[]).expect("regs")),
        (CHUNK_FORM, encode_form_chunk(&[]).expect("form")),
        (CHUNK_TABL, encode_tabl_chunk(&[]).expect("tabl")),
    ];
    let bytes = encode_container(&chunks).expect("container");

    let err = decode_hierarchy_chunks(&bytes).expect_err("invalid MSEL owner tag should fail");
    assert!(matches!(err, OpcpuCodecError::InvalidChunkFormat { .. }));
    assert!(err.to_string().contains("owner tag"));
}

#[test]
fn decode_legacy_toks_entries_default_extended_fields() {
    let families = sample_families();
    let cpus = sample_cpus();
    let dials = sample_dialects();
    let mut toks = Vec::new();
    write_u32(&mut toks, 1);
    toks.push(0);
    write_string(&mut toks, "TOKS", "mos6502").expect("owner");
    toks.push(TokenCaseRule::AsciiLower as u8);
    write_u32(
        &mut toks,
        token_identifier_class::ASCII_ALPHA | token_identifier_class::UNDERSCORE,
    );
    write_u32(
        &mut toks,
        token_identifier_class::ASCII_ALPHA
            | token_identifier_class::ASCII_DIGIT
            | token_identifier_class::UNDERSCORE,
    );
    write_string(&mut toks, "TOKS", ",()").expect("punctuation");
    let chunks = vec![
        (CHUNK_TOKS, toks),
        (CHUNK_FAMS, encode_fams_chunk(&families).expect("fams")),
        (CHUNK_CPUS, encode_cpus_chunk(&cpus).expect("cpus")),
        (CHUNK_DIAL, encode_dial_chunk(&dials).expect("dial")),
        (CHUNK_REGS, encode_regs_chunk(&[]).expect("regs")),
        (CHUNK_FORM, encode_form_chunk(&[]).expect("form")),
        (CHUNK_TABL, encode_tabl_chunk(&[]).expect("tabl")),
    ];
    let bytes = encode_container(&chunks).expect("container");
    let decoded = decode_hierarchy_chunks(&bytes).expect("legacy TOKS decode should succeed");
    assert_eq!(decoded.token_policies.len(), 1);
    let policy = &decoded.token_policies[0];
    assert_eq!(policy.comment_prefix, ";");
    assert_eq!(policy.quote_chars, "\"'");
    assert_eq!(policy.escape_char, Some('\\'));
    assert_eq!(policy.number_prefix_chars, "$%@");
    assert_eq!(
        policy.multi_char_operators,
        vec!["**", "==", "!=", "&&", "||", "^^", "<<", ">>", "<=", ">=", "<>"]
    );
}

fn expr_chunk_with_single_contract(contract: &ExprContractDescriptor) -> Vec<u8> {
    encode_expr_chunk(std::slice::from_ref(contract)).expect("EXPR chunk encode")
}

fn base_required_chunks_with_expr(expr_chunk: Vec<u8>) -> Vec<([u8; 4], Vec<u8>)> {
    vec![
        (CHUNK_EXPR, expr_chunk),
        (
            CHUNK_FAMS,
            encode_fams_chunk(&sample_families()).expect("fams"),
        ),
        (CHUNK_CPUS, encode_cpus_chunk(&sample_cpus()).expect("cpus")),
        (
            CHUNK_DIAL,
            encode_dial_chunk(&sample_dialects()).expect("dial"),
        ),
        (CHUNK_REGS, encode_regs_chunk(&[]).expect("regs")),
        (CHUNK_FORM, encode_form_chunk(&[]).expect("form")),
        (CHUNK_TABL, encode_tabl_chunk(&[]).expect("tabl")),
    ]
}

#[test]
fn decode_rejects_expr_contract_with_unsupported_opcode_version() {
    let mut contract = expr_contract_for_test(ScopedOwner::Family("mos6502".to_string()));
    contract.opcode_version = EXPR_VM_OPCODE_VERSION_V2 + 1;

    let bytes = encode_container(&base_required_chunks_with_expr(
        expr_chunk_with_single_contract(&contract),
    ))
    .expect("container");

    let err = decode_hierarchy_chunks(&bytes)
        .expect_err("unsupported EXPR opcode version should fail decode");
    assert!(matches!(err, OpcpuCodecError::InvalidChunkFormat { .. }));
    assert_eq!(err.code(), "OPC009");
    assert!(err.to_string().contains("unsupported opcode_version"));
}

#[test]
fn decode_accepts_expr_contract_with_v2_opcode_version() {
    let mut contract = expr_contract_for_test(ScopedOwner::Family("mos6502".to_string()));
    contract.opcode_version = EXPR_VM_OPCODE_VERSION_V2;

    let bytes = encode_container(&base_required_chunks_with_expr(
        expr_chunk_with_single_contract(&contract),
    ))
    .expect("container");

    let decoded = decode_hierarchy_chunks(&bytes).expect("v2 EXPR contract should decode");
    assert_eq!(decoded.expr_contracts.len(), 1);
    assert_eq!(
        decoded.expr_contracts[0].opcode_version,
        EXPR_VM_OPCODE_VERSION_V2
    );
}

#[test]
fn decode_accepts_expr_parser_contract_with_v2_opcode_version() {
    let mut contract = expr_parser_contract_for_test(ScopedOwner::Family("mos6502".to_string()));
    contract.opcode_version = EXVM_OPCODE_VERSION_V2;

    let bytes = encode_container(&[
        (
            CHUNK_EXVM,
            encode_exvm_chunk(std::slice::from_ref(&contract)).expect("EXVM chunk encode"),
        ),
        (
            CHUNK_FAMS,
            encode_fams_chunk(&sample_families()).expect("fams"),
        ),
        (CHUNK_CPUS, encode_cpus_chunk(&sample_cpus()).expect("cpus")),
        (
            CHUNK_DIAL,
            encode_dial_chunk(&sample_dialects()).expect("dial"),
        ),
        (CHUNK_REGS, encode_regs_chunk(&[]).expect("regs")),
        (CHUNK_FORM, encode_form_chunk(&[]).expect("form")),
        (CHUNK_TABL, encode_tabl_chunk(&[]).expect("tabl")),
    ])
    .expect("container");

    let decoded = decode_hierarchy_chunks(&bytes).expect("v2 EXVM contract should decode");
    assert_eq!(decoded.expr_parser_contracts.len(), 1);
    assert_eq!(
        decoded.expr_parser_contracts[0].opcode_version,
        EXVM_OPCODE_VERSION_V2
    );
}

#[test]
fn decode_rejects_expr_contract_with_zero_budget() {
    let mut contract = expr_contract_for_test(ScopedOwner::Family("mos6502".to_string()));
    contract.max_eval_steps = 0;

    let bytes = encode_container(&base_required_chunks_with_expr(
        expr_chunk_with_single_contract(&contract),
    ))
    .expect("container");

    let err = decode_hierarchy_chunks(&bytes).expect_err("zero EXPR budget should fail decode");
    assert!(matches!(err, OpcpuCodecError::InvalidChunkFormat { .. }));
    assert_eq!(err.code(), "OPC009");
    assert!(err.to_string().contains("max_eval_steps must be > 0"));
}

#[test]
fn decode_rejects_expr_contract_with_missing_diag_mapping() {
    let mut contract = expr_contract_for_test(ScopedOwner::Family("mos6502".to_string()));
    contract.diagnostics.invalid_program.clear();

    let bytes = encode_container(&base_required_chunks_with_expr(
        expr_chunk_with_single_contract(&contract),
    ))
    .expect("container");

    let err = decode_hierarchy_chunks(&bytes)
        .expect_err("missing EXPR diagnostic mapping should fail decode");
    assert!(matches!(err, OpcpuCodecError::InvalidChunkFormat { .. }));
    assert_eq!(err.code(), "OPC009");
    assert!(err
        .to_string()
        .contains("missing diagnostics.invalid_program code"));
}

#[test]
fn expr_parser_vm_opcode_from_u8_round_trip_and_unknown_rejection() {
    assert_eq!(
        ExvmOpcode::from_u8(ExvmOpcode::End as u8),
        Some(ExvmOpcode::End)
    );
    assert_eq!(
        ExvmOpcode::from_u8(ExvmOpcode::ParseExpression as u8),
        Some(ExvmOpcode::ParseExpression)
    );
    assert_eq!(
        ExvmOpcode::from_u8(ExvmOpcode::EmitDiag as u8),
        Some(ExvmOpcode::EmitDiag)
    );
    assert_eq!(
        ExvmOpcode::from_u8(ExvmOpcode::Fail as u8),
        Some(ExvmOpcode::Fail)
    );
    assert_eq!(ExvmOpcode::from_u8(0x04), None);
    assert_eq!(ExvmOpcode::from_u8(0xFF), None);
}

#[test]
fn expr_parser_vm_v2_opcode_from_u8_round_trip_and_unknown_rejection() {
    let opcodes = [
        (0x00, ExvmOpcodeV2::End),
        (0x01, ExvmOpcodeV2::Jump),
        (0x02, ExvmOpcodeV2::JumpIfTrue),
        (0x03, ExvmOpcodeV2::Call),
        (0x04, ExvmOpcodeV2::Return),
        (0x10, ExvmOpcodeV2::PeekKind),
        (0x11, ExvmOpcodeV2::PeekOperator),
        (0x20, ExvmOpcodeV2::Advance),
        (0x21, ExvmOpcodeV2::ConsumeOperator),
        (0x22, ExvmOpcodeV2::ConsumeKind),
        (0x32, ExvmOpcodeV2::LoadTokenText),
        (0x40, ExvmOpcodeV2::BuildUnary),
        (0x41, ExvmOpcodeV2::BuildBinary),
        (0x42, ExvmOpcodeV2::BuildTernary),
        (0x43, ExvmOpcodeV2::BuildRange),
        (0x60, ExvmOpcodeV2::BuildIdentifier),
        (0x61, ExvmOpcodeV2::BuildNumber),
        (0x62, ExvmOpcodeV2::BuildCurrentAddress),
        (0x63, ExvmOpcodeV2::ParseGrouping),
        (0x64, ExvmOpcodeV2::ParseList),
        (0x65, ExvmOpcodeV2::ParseStructLiteralIfPresent),
        (0x66, ExvmOpcodeV2::ParsePostfixChain),
        (0x70, ExvmOpcodeV2::EmitDiag),
        (0x72, ExvmOpcodeV2::Fail),
    ];

    for (byte, opcode) in opcodes {
        assert_eq!(ExvmOpcodeV2::from_u8(byte), Some(opcode));
        assert_eq!(opcode as u8, byte);
    }
    assert_eq!(ExvmOpcodeV2::from_u8(0xFF), None);
}

#[test]
fn expr_parser_vm_v2_operator_kind_from_u8_round_trip_and_unknown_rejection() {
    let kinds = [
        (0x01, ExvmOperatorKindV2::Plus),
        (0x02, ExvmOperatorKindV2::Minus),
        (0x03, ExvmOperatorKindV2::Multiply),
        (0x04, ExvmOperatorKindV2::Divide),
        (0x05, ExvmOperatorKindV2::Mod),
        (0x06, ExvmOperatorKindV2::Power),
        (0x07, ExvmOperatorKindV2::BitNot),
        (0x08, ExvmOperatorKindV2::LogicNot),
        (0x09, ExvmOperatorKindV2::Lt),
        (0x0A, ExvmOperatorKindV2::Gt),
        (0x0B, ExvmOperatorKindV2::Shl),
        (0x0C, ExvmOperatorKindV2::Shr),
        (0x0D, ExvmOperatorKindV2::Eq),
        (0x0E, ExvmOperatorKindV2::Ne),
        (0x0F, ExvmOperatorKindV2::Ge),
        (0x10, ExvmOperatorKindV2::Le),
        (0x11, ExvmOperatorKindV2::BitAnd),
        (0x12, ExvmOperatorKindV2::BitOr),
        (0x13, ExvmOperatorKindV2::BitXor),
        (0x14, ExvmOperatorKindV2::LogicAnd),
        (0x15, ExvmOperatorKindV2::LogicOr),
        (0x16, ExvmOperatorKindV2::LogicXor),
        (0x17, ExvmOperatorKindV2::Range),
        (0x18, ExvmOperatorKindV2::RangeInclusive),
    ];

    for (byte, kind) in kinds {
        assert_eq!(ExvmOperatorKindV2::from_u8(byte), Some(kind));
        assert_eq!(kind as u8, byte);
    }
    assert_eq!(ExvmOperatorKindV2::from_u8(0x00), None);
    assert_eq!(ExvmOperatorKindV2::from_u8(0xFF), None);
}

#[test]
fn expr_parser_vm_v2_token_kind_from_u8_round_trip_and_unknown_rejection() {
    let kinds = [
        (0x01, ExvmTokenKindV2::Number),
        (0x02, ExvmTokenKindV2::Identifier),
        (0x03, ExvmTokenKindV2::Dollar),
        (0x04, ExvmTokenKindV2::OpenParen),
        (0x05, ExvmTokenKindV2::CloseParen),
        (0x06, ExvmTokenKindV2::Question),
        (0x07, ExvmTokenKindV2::Colon),
        (0x08, ExvmTokenKindV2::OpenBrace),
    ];

    for (byte, kind) in kinds {
        assert_eq!(ExvmTokenKindV2::from_u8(byte), Some(kind));
        assert_eq!(kind as u8, byte);
    }
    assert_eq!(ExvmTokenKindV2::from_u8(0x00), None);
    assert_eq!(ExvmTokenKindV2::from_u8(0xFF), None);
}

#[test]
fn decode_malformed_count_stress_never_panics_and_returns_errors() {
    let cpus = encode_cpus_chunk(&sample_cpus()).expect("cpus");
    let dials = encode_dial_chunk(&sample_dialects()).expect("dial");
    let regs = encode_regs_chunk(&[]).expect("regs");
    let forms = encode_form_chunk(&[]).expect("form");
    let tabl = encode_tabl_chunk(&[]).expect("tabl");

    let mut seed = 0xC0FF_EE01u32;
    for _ in 0..128 {
        seed = seed.wrapping_mul(1664525).wrapping_add(1013904223);
        let mut fams = Vec::new();
        write_u32(&mut fams, seed);

        let chunks = vec![
            (CHUNK_FAMS, fams),
            (CHUNK_CPUS, cpus.clone()),
            (CHUNK_DIAL, dials.clone()),
            (CHUNK_REGS, regs.clone()),
            (CHUNK_FORM, forms.clone()),
            (CHUNK_TABL, tabl.clone()),
        ];
        let bytes = encode_container(&chunks).expect("container");

        let result = decode_hierarchy_chunks(&bytes);
        assert!(result.is_err(), "seeded malformed count should fail decode");
        if let Err(error) = result {
            assert!(
                matches!(
                    error,
                    OpcpuCodecError::InvalidChunkFormat { .. }
                        | OpcpuCodecError::UnexpectedEof { .. }
                        | OpcpuCodecError::CountOutOfRange { .. }
                ),
                "unexpected decoder error variant: {error:?}"
            );
        }
    }
}

#[test]
fn decode_mutated_container_deterministic_fuzz_never_panics() {
    let baseline = encode_hierarchy_chunks(
        &sample_families(),
        &sample_cpus(),
        &sample_dialects(),
        &sample_registers(),
        &sample_forms(),
        &sample_tables(),
    )
    .expect("baseline encode should succeed");

    let mut seed = 0xD1CE_BA11u32;
    for _ in 0..256 {
        seed = seed.wrapping_mul(1664525).wrapping_add(1013904223);
        let mut mutated = baseline.clone();
        let index = (seed as usize) % mutated.len();

        seed = seed.wrapping_mul(1664525).wrapping_add(1013904223);
        let mask = ((seed as u8) | 1).wrapping_add(0x3d);
        mutated[index] ^= mask;

        if mutated.len() > 12 && (seed & 1) == 0 {
            let trim = (seed as usize % 8) + 1;
            mutated.truncate(mutated.len().saturating_sub(trim));
        }

        let first = decode_hierarchy_chunks(&mutated);
        let second = decode_hierarchy_chunks(&mutated);

        match (first, second) {
            (Ok(left), Ok(right)) => {
                assert_eq!(left.families.len(), right.families.len());
                assert_eq!(left.cpus.len(), right.cpus.len());
                assert_eq!(left.dialects.len(), right.dialects.len());
                assert_eq!(left.forms.len(), right.forms.len());
                assert_eq!(left.tables.len(), right.tables.len());
            }
            (Err(left), Err(right)) => {
                assert_eq!(left.code(), right.code());
            }
            (left, right) => {
                panic!("decode outcome changed for same bytes: first={left:?}, second={right:?}")
            }
        }
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 96,
        .. ProptestConfig::default()
    })]

    #[test]
    fn decode_property_harness_is_deterministic_for_arbitrary_bytes(
        bytes in proptest::collection::vec(any::<u8>(), 0..4096)
    ) {
        let first = decode_hierarchy_chunks(&bytes);
        let second = decode_hierarchy_chunks(&bytes);

        match (first, second) {
            (Ok(left), Ok(right)) => {
                prop_assert_eq!(left.families.len(), right.families.len());
                prop_assert_eq!(left.cpus.len(), right.cpus.len());
                prop_assert_eq!(left.dialects.len(), right.dialects.len());
                prop_assert_eq!(left.forms.len(), right.forms.len());
                prop_assert_eq!(left.tables.len(), right.tables.len());
                prop_assert_eq!(left.selectors.len(), right.selectors.len());
            }
            (Err(left), Err(right)) => {
                prop_assert_eq!(left.code(), right.code());
            }
            (left, right) => {
                prop_assert!(
                    false,
                    "decode outcome changed for same bytes: first={left:?}, second={right:?}"
                );
            }
        }
    }
}
