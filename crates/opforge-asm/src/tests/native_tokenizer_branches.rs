//! Actual-native branch decoding proof, using the live generic Rust interpreter.
use super::*;
use vm::portable_contract::{PortableOperatorKind, PortableTokenKind};
use vm::runtime_portable_types::{PortableTokenizeRequest, PortableTokenizerByteStream};

struct Case {
    name: String,
    source: &'static str,
    program: Vec<u8>,
    budget: u32,
    expected: u32,
}

fn cases() -> Vec<Case> {
    let mut cases = Vec::new();
    // READ_CHAR; conditional branch; ADVANCE; END. Successful nonempty
    // cases consume their byte; empty cases exercise the EOF sentinel.
    for (opcode, operand, source, taken) in [
        (8, None, "", true),
        (8, None, "x", false),
        (9, Some(b'x'), "x", true),
        (9, Some(b'y'), "x", false),
        (9, Some(255), "", false),
        (10, Some(2), "x", true),
        (10, Some(4), "x", false),
        (10, Some(255), "x", false),
        (10, Some(2), "", false),
    ] {
        let prefix: Vec<u8> = [Some(1), Some(opcode), operand]
            .into_iter()
            .flatten()
            .collect();
        let mut program = prefix.clone();
        program.extend(u32::MAX.to_le_bytes());
        program.extend([2, 0]);
        cases.push(Case {
            name: format!("invalid-target-{opcode}-{operand:?}-{source:?}"),
            source,
            program,
            budget: 16,
            expected: if taken { 6 } else { 0 },
        });
        for bytes in 0..4 {
            let mut program = prefix.clone();
            program.extend(std::iter::repeat_n(0, bytes));
            cases.push(Case {
                name: format!("truncated-{opcode}-{operand:?}-{source:?}-{bytes}"),
                source,
                program,
                budget: 16,
                expected: 6,
            });
        }
    }
    for budget in [2, 3] {
        // READ_CHAR; JUMP_IF_EOL to END; END. Exactly three logical steps.
        cases.push(Case {
            name: format!("step-boundary-{budget}"),
            source: "",
            program: vec![1, 8, 6, 0, 0, 0, 0],
            budget,
            expected: if budget == 2 { 7 } else { 0 },
        });
    }
    cases
}

fn batch() -> (Vec<u8>, Vec<u8>) {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let template = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    let cases = cases();
    let mut input = (cases.len() as u32).to_be_bytes().to_vec();
    let mut oracle = Vec::new();
    for case in cases {
        let mut program = template.clone();
        program.program = case.program.clone();
        program.limits.max_steps_per_line = case.budget;
        let request = PortableTokenizeRequest {
            family_id: "motorola68000",
            cpu_id: "m68020",
            dialect_id: "motorola68k",
            source_line: case.source,
            source_stream: PortableTokenizerByteStream::from_source_line(case.source),
            line_num: 1,
            token_policy: policy.clone(),
        };
        // Direct generic execution intentionally bypasses automatic dispatch selection.
        let status: u32 = match model.tokenize_with_vm_core(&request, &program) {
            Ok(tokens) => {
                assert!(tokens.is_empty(), "{}", case.name);
                0
            }
            Err(error) => {
                let message = error.to_string();
                if message.contains("step budget exceeded") {
                    7
                } else {
                    assert!(
                        message.contains("truncated") || message.contains("exceeds program length"),
                        "{}: unexpected Rust failure: {message}",
                        case.name
                    );
                    6
                }
            }
        };
        assert_eq!(status, case.expected, "{}", case.name);
        input.extend(case.budget.to_be_bytes());
        input.extend((case.program.len() as u16).to_be_bytes());
        input.extend((case.source.len() as u16).to_be_bytes());
        input.extend(case.program);
        input.extend([case.source.as_bytes().first().copied().unwrap_or(0), 0]);
        oracle.extend(status.to_be_bytes());
    }
    (input, oracle)
}

#[test]
fn tokenizer_branch_live_oracle_covers_boundaries() {
    let (input, oracle) = batch();
    assert!(input.len() < 8192);
    assert_eq!(oracle.len(), 47 * 4);
}

#[test]
#[ignore = "requires configured FS-UAE; one bounded fresh native batch"]
fn tokenizer_branch_fs_uae() {
    let (input, oracle) = batch();
    match crate::fs_uae_smoke::run_tkvm_branch_harness_from_env(&workspace_root(), &input, &oracle)
        .expect("native tokenizer branch proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            panic!("native proof required: {reason}")
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            assert!(runs[0].success, "{}\n{}", runs[0].stdout, runs[0].stderr);
            println!("47 tokenizer branch cases matched the live generic Rust interpreter");
        }
    }
}

fn fragment_kind_code(kind: &PortableTokenKind) -> u16 {
    match kind {
        PortableTokenKind::Identifier(_) => 0,
        PortableTokenKind::Register(_) => 1,
        PortableTokenKind::Number { .. } => 2,
        PortableTokenKind::String { .. } => 3,
        PortableTokenKind::Comma => 4,
        PortableTokenKind::Colon => 5,
        PortableTokenKind::Dollar => 6,
        PortableTokenKind::Dot => 7,
        PortableTokenKind::Hash => 8,
        PortableTokenKind::Question => 9,
        PortableTokenKind::OpenBracket => 10,
        PortableTokenKind::CloseBracket => 11,
        PortableTokenKind::OpenBrace => 12,
        PortableTokenKind::CloseBrace => 13,
        PortableTokenKind::OpenParen => 14,
        PortableTokenKind::CloseParen => 15,
        PortableTokenKind::At => 40,
        PortableTokenKind::Operator(op) => {
            let ordered = [
                PortableOperatorKind::Range,
                PortableOperatorKind::RangeInclusive,
                PortableOperatorKind::Plus,
                PortableOperatorKind::Minus,
                PortableOperatorKind::Multiply,
                PortableOperatorKind::Power,
                PortableOperatorKind::Divide,
                PortableOperatorKind::Mod,
                PortableOperatorKind::Shl,
                PortableOperatorKind::Shr,
                PortableOperatorKind::BitNot,
                PortableOperatorKind::LogicNot,
                PortableOperatorKind::BitAnd,
                PortableOperatorKind::BitOr,
                PortableOperatorKind::BitXor,
                PortableOperatorKind::LogicAnd,
                PortableOperatorKind::LogicOr,
                PortableOperatorKind::LogicXor,
                PortableOperatorKind::Eq,
                PortableOperatorKind::Ne,
                PortableOperatorKind::Ge,
                PortableOperatorKind::Gt,
                PortableOperatorKind::Le,
                PortableOperatorKind::Lt,
            ];
            16 + ordered.iter().position(|value| value == op).unwrap() as u16
        }
    }
}

fn fragment_batch() -> (Vec<u8>, Vec<u8>, usize) {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let program = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    // Midpoint boundaries land inside identifiers, escape pairs and punctuation.
    let sources = [
        "",
        "joinedname",
        "a<<b",
        "a**b",
        "a!=b",
        "a..=b",
        "[a,b]",
        "{a,b}",
        "(a,b)",
        "a:b",
        "#42",
        "1_234",
        "$ff",
        "0b1010",
        "\"a;\\\"b\"",
        "'x,y'",
        "a ; ignored, tail",
        "; comment",
    ];
    assert!(program.program.len() <= 256);
    let mut input = ((sources.len() + 1) as u32).to_be_bytes().to_vec();
    let mut oracle = Vec::new();
    for (source, invalid) in sources
        .into_iter()
        .map(|source| (source, false))
        .chain([("untouched", true)])
    {
        let request = PortableTokenizeRequest {
            family_id: "motorola68000",
            cpu_id: "m68020",
            dialect_id: "motorola68k",
            source_line: source,
            source_stream: PortableTokenizerByteStream::from_source_line(source),
            line_num: 1,
            token_policy: policy.clone(),
        };
        let mut tokens = model
            .tokenize_with_vm_core(&request, &program)
            .unwrap_or_else(|error| panic!("fragment oracle {source:?}: {error}"));
        assert!(tokens.len() <= 8, "{source:?}");
        if invalid {
            tokens.clear();
        }
        input.extend(program.limits.max_steps_per_line.to_be_bytes());
        input.extend((program.program.len() as u16).to_be_bytes());
        input.extend(
            ((source.len() as u16) | 0x1800 | if invalid { 0x0400 } else { 0 }).to_be_bytes(),
        );
        input.extend(&program.program);
        input.extend(source.as_bytes());
        input.resize(
            input.len() + (source.len().max(1).next_multiple_of(2) - source.len()),
            0,
        );
        oracle.extend((if invalid { 5u32 } else { 0 }).to_be_bytes());
        oracle.extend((tokens.len() as u32).to_be_bytes());
        for index in 0..8 {
            let kind = tokens
                .get(index)
                .map(|token| u32::from(fragment_kind_code(&token.kind)))
                .unwrap_or(u32::MAX);
            oracle.extend(kind.to_be_bytes());
        }
    }
    (input, oracle, sources.len() + 1)
}

#[test]
fn tokenizer_fragments_live_oracle_covers_joins() {
    let (input, oracle, count) = fragment_batch();
    assert!(input.len() < 8192);
    assert_eq!(oracle.len(), count * 40);
}

#[test]
#[ignore = "requires configured FS-UAE; one bounded fresh native batch"]
fn tokenizer_fragments_fs_uae() {
    let (input, oracle, count) = fragment_batch();
    match crate::fs_uae_smoke::run_tkvm_branch_harness_from_env(&workspace_root(), &input, &oracle)
        .expect("native tokenizer fragment proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            panic!("native proof required: {reason}")
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            assert!(runs[0].success, "{}\n{}", runs[0].stdout, runs[0].stderr);
            println!("{count} tokenizer fragment cases matched the live generic Rust interpreter");
        }
    }
}
