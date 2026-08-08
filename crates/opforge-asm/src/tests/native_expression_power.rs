//! Native scalar exponentiation parity proofs.

use super::*;

const POWER_SOURCE: &[u8] = b"pow1 .const 2 ** 3\
\npow2 .const 3 ** 2 ** 2\
\npow3 .const 2 * 3 ** 2\
\npow4 .const 2 ** 3 * 2\
\npow5 .const 0 ** 0\
\n        .byte pow1,pow2,pow3,pow4,pow5\n";

fn rust_power_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(POWER_SOURCE).expect("power fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust power authority");
    assert!(
        diagnostics.is_empty(),
        "Rust power diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_power_rust_oracle() {
    // Proof level A. This proves the live Rust expression implementation's
    // right associativity, precedence, and zero-exponent behavior. It does not
    // prove native parser or evaluator execution.
    assert_eq!(rust_power_bytes(), [8, 81, 18, 16, 1]);

    let (_, diagnostics) = assemble_source_entries_with_runtime_mode(
        &[".cpu 65c02", "bad .const 2 ** -1", "        .byte bad"],
        true,
    )
    .expect("negative power source should report diagnostics without panicking");
    assert!(
        !diagnostics.is_empty(),
        "Rust must reject a negative integer exponent"
    );
}

#[test]
fn native_expression_power_parser_runtime_contract() {
    // Proof level B. This proves the native parser owns a right-recursive power
    // tier above multiplication and the runtime evaluates nonnegative powers
    // by squaring while rejecting negative exponents. It does not prove real
    // 68020 execution or final CLI bytes.
    let root = workspace_root();
    let compiler = format_tokvm_asm_fragment(
        &fs::read_to_string(
            root.join("native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm"),
        )
        .expect("read native expression bridge"),
    );
    assert!(source_contains_in_order(
        &compiler,
        &[
            "compileMultiplicative:",
            "BSR.W power",
            "power:",
            "BSR.W compileSingleTerm",
            "CMPI.B #'*', 1(A0)",
            "BSR.W power",
            "MOVEQ #runtime.EXPRVM_BINARY_POWER, D6",
        ]
    ));

    let runtime = normalize_tkpkg_fragment(
        &fs::read_to_string(root.join("native/motorola68000/amigaos/exprvm/exprvm_runtime.asm"))
            .expect("read native ExprVM runtime"),
    );
    assert!(source_contains_in_order(
        &runtime,
        &[
            "EXPRVM_BINARY_POWER = 3",
            "CMPI.B #EXPRVM_BINARY_POWER, D6",
            "BEQ.W applyBinaryPower",
            "applyBinaryPower:",
            "TST.L D2",
            "BMI.W fail",
            "MOVEQ #1, D3",
            "BTST #0, D2",
            "MULU.L D1, D3",
            "LSR.L #1, D2",
            "MULU.L D1, D1",
        ]
    ));
}

#[test]
fn native_expression_power_fs_uae() {
    // Proof level D. This proves the real native CLI evaluates exponentiation
    // with the same values, precedence, and right associativity as live Rust.
    // It does not prove later comparison, bitwise, logical, or ternary tiers.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("power FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "expression-power",
        cpu_id: "65c02",
        source: POWER_SOURCE,
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_power_bytes()),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("power FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native power fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_power_bytes(), "native power bytes differ");
        }
    }
}
