//! Native scalar ternary-selection parity proofs.

use super::*;

const TERNARY_SOURCE: &[u8] = b".byte 0 ? $11 : $22\
\n.byte 7 ? $33 : $44\
\n.byte 0 ? 1 : 0 ? 2 : 3\
\n.byte 1 ? 0 ? 4 : 5 : 6\
\n.byte 0 || 1 ? 7 : 8\n";

fn rust_ternary_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(TERNARY_SOURCE).expect("ternary fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust ternary authority");
    assert!(
        diagnostics.is_empty(),
        "Rust ternary diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_ternary_rust_oracle() {
    // Proof level A. This proves live Rust true/false arm selection, nested
    // right associativity, and condition precedence. It does not prove native
    // runtime execution.
    assert_eq!(rust_ternary_bytes(), [0x22, 0x33, 0x03, 0x05, 0x07]);
}

#[test]
fn native_expression_ternary_runtime_contract() {
    // Proof level B. This proves the native runtime preserves the false arm
    // outside D2 before popD3 clobbers D2 to retrieve the condition and branches
    // before an arm restore changes condition codes. It does not prove real
    // 68020 execution or final CLI bytes.
    let runtime = normalize_tkpkg_fragment(
        &fs::read_to_string(
            workspace_root().join("native/motorola68000/amigaos/exprvm/exprvm_runtime.asm"),
        )
        .expect("read native ExprVM runtime"),
    );
    assert!(source_contains_in_order(
        &runtime,
        &[
            "applyTernarySelect",
            "MOVE.L D2, D1",
            "MOVE.L D3, -(SP)",
            "BSR.W popD3",
            "TST.L D3",
            "BEQ.S applyTernaryFalse",
            "MOVE.L (SP)+, D3",
            "BRA.W applyBinaryDone",
            "applyTernaryFalse",
            "ADDQ.L #4, SP",
            "MOVE.L D1, D3",
        ]
    ));
}

#[test]
fn native_expression_ternary_fs_uae() {
    // Proof level D. This proves all five complete ternary cases execute in the
    // real native CLI and match live Rust bytes. It does not prove unary
    // operators within ternary arms.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("ternary FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "expression-ternary",
        cpu_id: "65c02",
        source: TERNARY_SOURCE,
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_ternary_bytes()),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("ternary FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native ternary fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_ternary_bytes(), "native ternary bytes differ");
        }
    }
}
