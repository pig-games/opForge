//! Native string-data directive parity proofs.

use super::*;

const DATA_STRING_SOURCE: &[u8] = b".org 0\
\n        .byte 'abcd'\
\n        .byte \"Labels can be up to 32 characters && must start with an alpha.\"\
\n        .byte \"\\r\\n\\t\\x2a\\x2B\\0\"\
\n        .byte '\\\\','\\\'',\"'\"\
\n        .byte 3,\"red\",4,'blue'\
\n        rts\n";

fn rust_data_string_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(DATA_STRING_SOURCE).expect("data-string fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust data-string authority");
    assert!(
        diagnostics.is_empty(),
        "Rust data-string diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

fn expected_data_string_bytes() -> Vec<u8> {
    let mut expected = Vec::new();
    expected.extend_from_slice(b"abcd");
    expected.extend_from_slice(b"Labels can be up to 32 characters && must start with an alpha.");
    expected.extend_from_slice(&[13, 10, 9, 0x2a, 0x2b, 0]);
    expected.extend_from_slice(b"\\''");
    expected.extend_from_slice(&[3]);
    expected.extend_from_slice(b"red");
    expected.extend_from_slice(&[4]);
    expected.extend_from_slice(b"blue");
    expected.push(0x60);
    expected
}

#[test]
fn native_data_string_rust_oracle() {
    // Proof level A. This proves the live Rust byte contract for both quote
    // styles, an exactly-64-byte operand, escapes, mixed lists, and final RTS.
    // It does not prove native parsing or guest execution.
    assert_eq!(rust_data_string_bytes(), expected_data_string_bytes());
}

#[test]
fn native_data_string_parser_contract() {
    // Proof level B. This proves truncated operand snapshots fall back to the
    // validated full source span, while ordinary operands retain their copied
    // fast path, and the data parser admits both delimiters plus xNN escapes.
    // It does not execute native code or prove final artifacts.
    let root = workspace_root();
    let engine =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read native opasm engine");
    assert!(source_contains_in_order(
        &engine,
        &[
            "cmpi.w #TOKEN_BUFFER_CAPACITY - 2, d2",
            "blo.s copiedOperand",
            "bsr.w sourceOperandLengthV1",
            "move.l a0, OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(a2)",
            "copiedOperand",
        ]
    ));

    let driver = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    let start = driver
        .find("parseTextDirectiveForStatement\t.block")
        .expect("native text parser block");
    let block = &driver[start..];
    let end = block
        .find("\t.bend  ; parseTextDirectiveForStatement")
        .expect("native text parser end");
    let block = &block[..end];
    assert!(source_contains_in_order(
        block,
        &[
            "cmpi.b #'\"', d3",
            "cmpi.b #39, d3",
            "cmp.b d3, d1",
            "cmpi.b #'x', d1",
            "bsr.w hexNibbleValue",
            "lsl.b #4, d0",
            "or.b d0, d1",
        ]
    ));
}

#[test]
fn native_data_string_fs_uae() {
    // Proof level D. This proves the real native CLI emits the exact live Rust
    // artifact for the complete focused string-data matrix.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("data-string FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "data-string",
        cpu_id: "65c02",
        source: DATA_STRING_SOURCE,
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(
            &rust_data_string_bytes(),
        ),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("data-string FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native data-string fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(
                native,
                expected_data_string_bytes(),
                "native string bytes differ"
            );
            assert_eq!(
                native,
                rust_data_string_bytes(),
                "native/Rust string bytes differ"
            );
        }
    }
}

#[test]
fn native_data_string_invalid_fs_uae() {
    // Proof level D. This proves malformed and incomplete hexadecimal strings
    // are guest-completed failures, not emulator crashes or silent truncation.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("invalid data-string FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let sources = [
        (
            "data-string-unterminated",
            b".org 0\n.byte \"abc\n".as_slice(),
        ),
        (
            "data-string-bad-hex",
            b".org 0\n.byte \"\\xG0\"\n".as_slice(),
        ),
        (
            "data-string-short-hex",
            b".org 0\n.byte '\\x1'\n".as_slice(),
        ),
    ];
    let cases = sources
        .iter()
        .map(
            |(name, source)| crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
                name,
                cpu_id: "65c02",
                source,
                package_bytes: package.as_slice(),
                proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExpectedFailureWithDiagnostic,
            },
        )
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("invalid data-string FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), sources.len());
            for (run, (name, _)) in runs.iter().zip(sources.iter()) {
                let guest_exit_code = run.exit_code.unwrap_or_else(|| {
                    panic!(
                        "invalid data-string case {name} produced no guest exit code\nstdout:\n{}\nstderr:\n{}",
                        run.stdout, run.stderr
                    )
                });
                assert_ne!(
                    guest_exit_code, 0,
                    "invalid data-string case {name} must fail"
                );
                assert!(
                    run.protocol_completed,
                    "invalid data-string case {name} must complete the exact fresh guest protocol"
                );
                assert!(
                    !run.stderr.trim().is_empty(),
                    "invalid data-string case {name} must produce diagnostics"
                );
            }
        }
    }
}
