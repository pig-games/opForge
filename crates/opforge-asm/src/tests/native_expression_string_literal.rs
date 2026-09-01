//! Native scalar string-literal expression parity proofs.

use super::*;

const STRING_LITERAL_SOURCE: &[u8] = b"single .const 'A'\
\npair .const 'AB'\
\nnewline .const '\\n'\
\ncarriage .const '\\r'\
\ntab .const '\\t'\
\nzero .const '\\0'\
\nhex_char .const '\\x43'\
\ndouble_quote .const \"D\"\
\nstart lda #single\
\n      lda #<pair\
\n      lda #>pair\
\n      lda #newline\
\n      lda #carriage\
\n      lda #tab\
\n      lda #zero\
\n      lda #'\\x43'\
\n      lda #\"D\"\
\n      .word pair\
\n      rts\n";

fn rust_string_literal_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(STRING_LITERAL_SOURCE).expect("string-literal fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust string-literal authority");
    assert!(
        diagnostics.is_empty(),
        "Rust string-literal diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_string_literal_rust_oracle() {
    // Proof level A. This proves live Rust one/two-byte packing, both quote
    // styles, escape decoding, direct selected-operand use, and final bytes. It
    // does not prove native compilation or guest execution.
    assert_eq!(
        rust_string_literal_bytes(),
        [
            0xa9, 0x41, 0xa9, 0x42, 0xa9, 0x41, 0xa9, 0x0a, 0xa9, 0x0d, 0xa9, 0x09, 0xa9, 0x00,
            0xa9, 0x43, 0xa9, 0x44, 0x42, 0x41, 0x60,
        ]
    );
    for source in ["value .const ''", "value .const 'ABC'", "value .const 'A"] {
        let (_, diagnostics) = assemble_source_entries_with_runtime_mode(
            &[".cpu 65c02", source, "start lda #value"],
            true,
        )
        .expect("invalid Rust string literal should report diagnostics");
        assert!(
            !diagnostics.is_empty(),
            "Rust must reject malformed scalar literal in `{source}`"
        );
    }
}

#[test]
fn native_expression_string_literal_invalid_fs_uae() {
    // Proof level D. This proves the real native CLI rejects empty, over-width,
    // and unterminated scalar literals. It does not prove the precise Rust
    // diagnostic wording, which remains CLI-diagnostics scope.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("invalid string-literal FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let sources = [
        (
            "expression-string-empty",
            b"value .const ''\nstart lda #value\n".as_slice(),
        ),
        (
            "expression-string-over-width",
            b"value .const 'ABC'\nstart lda #value\n".as_slice(),
        ),
        (
            "expression-string-unterminated",
            b"value .const 'A\nstart lda #value\n".as_slice(),
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
        .expect("invalid string-literal FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), sources.len());
            for (run, (name, _)) in runs.iter().zip(sources.iter()) {
                let guest_exit_code = run.exit_code.unwrap_or_else(|| {
                    panic!(
                        "native malformed string-literal case {name} did not produce a guest exit code\nstdout:\n{}\nstderr:\n{}",
                        run.stdout, run.stderr
                    )
                });
                assert_ne!(
                    guest_exit_code, 0,
                    "native malformed string-literal case {name} must return a nonzero guest exit code"
                );
                assert!(
                    run.protocol_completed,
                    "native malformed string-literal case {name} must complete the exact fresh guest protocol"
                );
                assert!(
                    !run.success,
                    "native malformed string-literal case {name} must fail\nstdout:\n{}\nstderr:\n{}",
                    run.stdout, run.stderr
                );
                assert!(
                    !run.stderr.trim().is_empty(),
                    "native malformed string-literal case {name} must diagnose the failure"
                );
            }
        }
    }
}

#[test]
fn native_expression_string_literal_parser_contract() {
    // Proof level B. This proves quote-leading selected operands reach the
    // compiler and its checked literal parser admits exactly one or two
    // decoded bytes before emitting an ordinary scalar literal. It does not
    // prove real 68020 execution or final native CLI bytes.
    let root = workspace_root();
    let operand_runtime = normalize_tkpkg_fragment(
        &fs::read_to_string(
            root.join("native/motorola68000/amigaos/tkpkg/tkpkg_operand_runtime.asm"),
        )
        .expect("read native operand runtime"),
    );
    assert!(source_contains_in_order(
        &operand_runtime,
        &[
            "CMPI.B #39, D7",
            "CMPI.B #'\"', D7",
            "textOk",
            "JSR expr_bridge.opcoreExvmEvalOperandWithResolverV1",
        ]
    ));

    let compiler = normalize_tkpkg_fragment(
        &fs::read_to_string(
            root.join("native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm"),
        )
        .expect("read native expression bridge"),
    );
    assert!(source_contains_in_order(
        &compiler,
        &[
            "compileSingleTerm .block",
            "CMPI.B #39, (A0)",
            "CMPI.B #'\"', (A0)",
            "stringLiteral",
            "stringScan",
            "CMPI.L #2, D2",
            "LSL.L #8, D3",
            "OR.L D1, D3",
            "stringClose",
            "BSR.W emitPushLiteralD3",
        ]
    ));
}

#[test]
fn native_expression_string_literal_fs_uae() {
    // Proof level D. This proves the real Amiga-native CLI matches live Rust
    // for all nine scalar literal uses and the packed word. It does not prove
    // malformed or longer-than-two-byte diagnostics.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("string-literal FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "expression-string-literal",
        cpu_id: "65c02",
        source: STRING_LITERAL_SOURCE,
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(
            &rust_string_literal_bytes(),
        ),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("string-literal FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native string-literal fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(
                native,
                rust_string_literal_bytes(),
                "native string-literal bytes differ"
            );
        }
    }
}
