//! Native scalar comparison parity proofs.

use super::*;

const COMPARISON_SOURCE: &[u8] = b"eq .const 3 == 3\
\nne .const 3 != 4\
\nne_alt .const 3 <> 4\
\nle .const 3 <= 4\
\nlt .const 3 < 4\
\nge .const 4 >= 3\
\ngt .const 4 > 3\
\nsigned .const -2 < -1\
\nprecedence .const 1 << 2 == 4\
\nfalse_value .const 4 < 3\
\n        .byte eq,ne,ne_alt,le,lt,ge,gt,signed,precedence,false_value\n";

fn rust_comparison_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(COMPARISON_SOURCE).expect("comparison fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust comparison authority");
    assert!(
        diagnostics.is_empty(),
        "Rust comparison diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_comparison_rust_oracle() {
    // Proof level A. This proves live Rust values for every retained spelling,
    // signed ordering, false projection, and shift/comparison precedence. It
    // does not prove native parser or evaluator execution.
    assert_eq!(rust_comparison_bytes(), [1, 1, 1, 1, 1, 1, 1, 1, 1, 0]);
}

#[test]
fn native_expression_comparison_parser_runtime_contract() {
    // Proof level B. This proves the native comparison tier delegates operands
    // to shift parsing, recognizes all retained spellings, and projects signed
    // 68020 conditions to scalar zero/one. It does not prove real execution.
    let root = workspace_root();
    let compiler = normalize_tkpkg_fragment(
        &fs::read_to_string(
            root.join("native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm"),
        )
        .expect("read native expression bridge"),
    );
    assert!(source_contains_in_order(
        &compiler,
        &[
            "compileLogicalOr .block",
            "BSR.W compare",
            "compare",
            "BSR.W compileShift",
            "compareEq",
            "MOVEQ #runtime.EXPRVM_BINARY_EQ, D6",
            "compareNe",
            "MOVEQ #runtime.EXPRVM_BINARY_NE, D6",
            "compareLt",
            "compareNeAlt",
            "compareGt",
            "compareGe",
            "compareApply",
            "BSR.W compileShift",
        ]
    ));
    assert!(source_contains_in_order(
        &compiler,
        &[
            "compileAdditive .block",
            "CMPI.B #'=', D6",
            "BEQ.W ok",
            "CMPI.B #'!', D6",
            "BEQ.W ok",
            "compileShift .block",
            "CMPI.B #'<', 1(A0)",
            "BNE.W ok",
            "CMPI.B #'>', 1(A0)",
            "BNE.W ok",
        ]
    ));

    let runtime = normalize_tkpkg_fragment(
        &fs::read_to_string(root.join("native/motorola68000/amigaos/exprvm/exprvm_runtime.asm"))
            .expect("read native ExprVM runtime"),
    );
    for (handler, condition) in [
        ("applyBinaryEq", "SEQ D3"),
        ("applyBinaryNe", "SNE D3"),
        ("applyBinaryGe", "SGE D3"),
        ("applyBinaryGt", "SGT D3"),
        ("applyBinaryLe", "SLE D3"),
        ("applyBinaryLt", "SLT D3"),
    ] {
        assert!(
            source_contains_in_order(
                &runtime,
                &[handler, "CMP.L D2, D3", condition, "ANDI.L #1, D3"]
            ),
            "missing native comparison projection for {handler}"
        );
    }
}

#[test]
fn native_expression_comparison_fs_uae() {
    // Proof level D. This proves the real native CLI matches live Rust for all
    // retained comparison spellings, signed conditions, precedence, and both
    // boolean results. It does not prove later bitwise/logical tiers.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("comparison FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "expression-comparison",
        cpu_id: "65c02",
        source: COMPARISON_SOURCE,
        package_bytes: package.as_slice(),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("comparison FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native comparison fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = fs::read(
                run.artifact_dir
                    .join("Work")
                    .join(crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE),
            )
            .expect("read native comparison output");
            assert_eq!(
                native,
                rust_comparison_bytes(),
                "native comparison bytes differ"
            );
        }
    }
}
