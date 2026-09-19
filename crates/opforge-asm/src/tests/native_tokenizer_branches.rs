//! Actual-native branch decoding proof, using the live generic Rust interpreter.
use super::*;
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
