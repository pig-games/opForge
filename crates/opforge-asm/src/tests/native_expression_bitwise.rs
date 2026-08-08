//! Native scalar bitwise-expression parity proofs.

use super::*;

const BITWISE_SOURCE: &[u8] = b"bit_and .const $f0 & $0f\
\nbit_or .const $f0 | $0f\
\nbit_xor .const $f0 ^ $0f\
\nprecedence .const 1 | 2 ^ 3 & 1\
\ncompare_first .const 2 == 2 & 1\
\nlogical_yield .const 0 || 3\
\n        .byte bit_and,bit_or,bit_xor,precedence,compare_first,logical_yield\n";

fn rust_bitwise_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(BITWISE_SOURCE).expect("bitwise fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust bitwise authority");
    assert!(
        diagnostics.is_empty(),
        "Rust bitwise diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_bitwise_rust_oracle() {
    // Proof level A. This proves live Rust values and the comparison/AND/XOR/OR
    // precedence ladder, including yielding `||` to the logical tier. It does
    // not prove native parser or evaluator execution.
    assert_eq!(rust_bitwise_bytes(), [0, 0xff, 0xff, 3, 1, 1]);
}

#[test]
fn native_expression_bitwise_parser_runtime_contract() {
    // Proof level B. This proves the native scoped tiers delegate in Rust order,
    // reject double-character logical tokens, and apply bitwise runtime ops. It
    // does not prove real 68020 execution or final native CLI bytes.
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
            "BSR.W bitOr",
            "bitOr",
            "BSR.W bitXor",
            "CMPI.B #'|', 1(A0)",
            "BEQ.W bitOrOk",
            "bitXor",
            "BSR.W bitAnd",
            "CMPI.B #'^', 1(A0)",
            "BEQ.W bitXorOk",
            "bitAnd",
            "BSR.W compare",
            "CMPI.B #'&', 1(A0)",
            "BEQ.W bitAndOk",
        ]
    ));

    let runtime = normalize_tkpkg_fragment(
        &fs::read_to_string(root.join("native/motorola68000/amigaos/exprvm/exprvm_runtime.asm"))
            .expect("read native ExprVM runtime"),
    );
    assert!(source_contains_in_order(
        &runtime,
        &[
            "applyBinaryBitAnd",
            "AND.L D2, D3",
            "applyBinaryBitOr",
            "OR.L D2, D3",
            "applyBinaryBitXor",
            "EOR.L D2, D3",
        ]
    ));
}

#[test]
fn native_expression_bitwise_fs_uae() {
    // Proof level D. This proves the real native CLI matches live Rust for all
    // bitwise operators and their precedence ladder. It does not prove logical
    // AND/XOR or ternary semantics.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("bitwise FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "expression-bitwise",
        cpu_id: "65c02",
        source: BITWISE_SOURCE,
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bitwise_bytes()),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("bitwise FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native bitwise fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bitwise_bytes(), "native bitwise bytes differ");
        }
    }
}
