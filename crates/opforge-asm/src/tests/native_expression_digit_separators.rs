//! Native scalar digit-separator parity proofs.

use super::*;

#[test]
fn native_expression_digit_separators_rust_oracle() {
    // Proof level A. This proves Rust accepts the retained separator forms and
    // preserves their scalar values. It does not prove native execution.
    let cases = [
        ("decimal", "1_0", 10),
        ("hex-prefix", "$F_F", 0xff),
        ("binary-prefix", "%1010_1010", 0xaa),
        ("binary-suffix", "1010_1010b", 0xaa),
    ];
    for (name, literal, expected) in cases {
        let value_line = format!("value .const {literal}");
        let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
            &[".cpu 65c02", value_line.as_str(), "start lda #value", "rts"],
            true,
        )
        .unwrap_or_else(|err| panic!("assemble digit-separator oracle {name}: {err}"));
        assert!(
            diagnostics.is_empty(),
            "Rust diagnostics for {name}: {diagnostics:?}"
        );
        assert_eq!(entries[1].1, expected, "separator value for {name}");
    }
    let (_, invalid_diagnostics) = assemble_source_entries_with_runtime_mode(
        &[".cpu 65c02", "value .const $___+1", "start lda #value"],
        true,
    )
    .expect("invalid Rust source should report diagnostics without panicking");
    assert!(
        !invalid_diagnostics.is_empty(),
        "underscore-only prefixed literal must remain invalid"
    );
}

#[test]
fn native_expression_digit_separator_parser_contract() {
    // Proof level B. This proves every native scalar radix parser skips the
    // separator without shifting or accumulating it. It does not prove runtime
    // register preservation or real AmigaOS execution.
    let source = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm"),
    )
    .expect("read native expression bridge");
    for block in ["parseHex", "parseBinary", "parseOctal", "parseDecimal"] {
        let start = source
            .find(&format!("{block}\t.block"))
            .unwrap_or_else(|| panic!("find {block}"));
        let end = source[start..]
            .find(&format!(".bend  ; {block}"))
            .map(|offset| start + offset)
            .unwrap_or_else(|| panic!("find end of {block}"));
        let body = &source[start..end];
        assert!(
            body.contains("cmpi.b #'_', d1"),
            "{block} must recognize digit separators"
        );
        assert!(
            body.contains("beq.s fail"),
            "{block} must reject a token with no real digit"
        );
    }
}

#[test]
fn native_expression_digit_separator_invalid_fs_uae() {
    // Proof level D. This proves the real native CLI rejects an underscore-only
    // hexadecimal literal instead of silently treating it as zero. It does not
    // prove other malformed-token diagnostic classes.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("invalid-separator FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "separator-invalid-underscore-only",
        cpu_id: "65c02",
        source: b"value .const $___+1\nstart lda #value\nrts\n",
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExpectedFailureContaining(
            "OTR921: expression bridge reported trailing text",
        ),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("invalid digit-separator FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                !run.success,
                "native underscore-only literal must fail\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            assert!(
                run.stderr.contains("OTR921: expression bridge reported trailing text"),
                "native underscore-only literal must report deterministic trailing-text failure\nstderr:\n{}",
                run.stderr
            );
        }
    }
}

#[test]
fn native_expression_digit_separators_fs_uae() {
    // Proof level D. This proves the real native CLI evaluates every retained
    // digit-separator form to the same emitted bytes as Rust. It does not prove
    // later expression tiers or the complete additive syntax fixture.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("digit-separator FS-UAE lock poisoned");
    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let sources = [
        (
            "separator-decimal",
            b"value .const 1_0\nstart lda #value\nrts\n".as_slice(),
        ),
        (
            "separator-hex-prefix",
            b"value .const $F_F\nstart lda #value\nrts\n".as_slice(),
        ),
        (
            "separator-binary-prefix",
            b"value .const %1010_1010\nstart lda #value\nrts\n".as_slice(),
        ),
        (
            "separator-binary-suffix",
            b"value .const 1010_1010b\nstart lda #value\nrts\n".as_slice(),
        ),
    ];
    let mut rust_bins = Vec::with_capacity(sources.len());
    for (name, source) in &sources {
        let text = std::str::from_utf8(source).expect("separator fixture UTF-8");
        let mut lines = vec![".cpu 65c02"];
        lines.extend(text.lines());
        let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
            .unwrap_or_else(|err| panic!("assemble Rust separator authority {name}: {err}"));
        assert!(
            diagnostics.is_empty(),
            "Rust diagnostics for {name}: {diagnostics:?}"
        );
        rust_bins.push(
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>(),
        );
    }
    let cases = sources
        .iter()
        .zip(rust_bins.iter())
        .map(
            |((name, source), rust_bin)| crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
                name,
                cpu_id: "65c02",
                source,
                package_bytes: package_bytes.as_slice(),
                proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(rust_bin),
            },
        )
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("digit-separator FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), sources.len());
            for ((run, (name, _)), rust_bin) in
                runs.iter().zip(sources.iter()).zip(rust_bins.iter())
            {
                assert!(
                    run.success,
                    "native separator fixture {name} failed\nstdout:\n{}\nstderr:\n{}",
                    run.stdout, run.stderr
                );
                let native_bin = verified_fs_uae_output(run);
                assert_eq!(
                    native_bin, rust_bin,
                    "native separator bytes differ for {name}"
                );
            }
        }
    }
}
