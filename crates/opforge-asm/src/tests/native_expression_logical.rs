//! Native scalar logical-expression parity proofs.

use super::*;

const LOGICAL_SOURCE: &[u8] = b"and_true .const 2 && 3\
\nand_false .const 2 && 0\
\nor_true .const 0 || 3\
\nor_false .const 0 || 0\
\nxor_true .const 0 ^^ 3\
\nxor_false .const 2 ^^ 3\
\nand_precedence .const 1 || 0 && 0\
\nor_xor_left .const 1 || 1 ^^ 1\
\nbitwise_first .const 2 && 1 | 2\
\n        .byte and_true,and_false,or_true\
\n        .byte or_false,xor_true,xor_false\
\n        .byte and_precedence,or_xor_left,bitwise_first\n";

fn rust_logical_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(LOGICAL_SOURCE).expect("logical fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust logical authority");
    assert!(
        diagnostics.is_empty(),
        "Rust logical diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_logical_rust_oracle() {
    // Proof level A. This proves live Rust truthiness, logical-AND precedence,
    // shared left-associative OR/XOR precedence, and bitwise-before-logical
    // behavior. It does not prove native parser or evaluator execution.
    assert_eq!(rust_logical_bytes(), [1, 0, 1, 0, 1, 0, 1, 0, 1]);
}

#[test]
fn native_expression_logical_parser_runtime_contract() {
    // Proof level B. This proves native logical AND delegates to bitwise OR and
    // the outer tier recognizes both OR and XOR, while the runtime normalizes
    // nonzero operands. It does not prove real 68020 execution.
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
            "BSR.W logicalAnd",
            "logicalOr",
            "MOVEQ #runtime.EXPRVM_BINARY_LOGIC_OR, D6",
            "logicalXor",
            "MOVEQ #runtime.EXPRVM_BINARY_LOGIC_XOR, D6",
            "BSR.W logicalAnd",
            "logicalAnd",
            "BSR.W bitOr",
            "CMPI.B #'&', 1(A0)",
            "MOVEQ #runtime.EXPRVM_BINARY_LOGIC_AND, D6",
        ]
    ));

    let runtime = normalize_tkpkg_fragment(
        &fs::read_to_string(root.join("native/motorola68000/amigaos/exprvm/exprvm_runtime.asm"))
            .expect("read native ExprVM runtime"),
    );
    assert!(source_contains_in_order(
        &runtime,
        &[
            "applyBinaryLogicOr",
            "OR.L D2, D3",
            "applyBinaryLogicAnd",
            "TST.L D3",
            "TST.L D2",
            "MOVEQ #1, D3",
            "applyBinaryLogicFalse",
            "MOVEQ #0, D3",
            "applyBinaryLogicXor",
            "SNE D3",
            "SNE D2",
            "EOR.L D2, D3",
        ]
    ));
}

#[test]
fn native_expression_logical_fs_uae() {
    // Proof level D. This proves the real native CLI matches live Rust for all
    // logical operators, zero/nonzero truthiness, and discriminating precedence
    // cases. It does not prove ternary semantics.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("logical FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "expression-logical",
        cpu_id: "65c02",
        source: LOGICAL_SOURCE,
        package_bytes: package.as_slice(),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("logical FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native logical fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = fs::read(
                run.artifact_dir
                    .join("Work")
                    .join(crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE),
            )
            .expect("read native logical output");
            assert_eq!(native, rust_logical_bytes(), "native logical bytes differ");
        }
    }
}
