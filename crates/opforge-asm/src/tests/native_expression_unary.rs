//! Native scalar unary-expression parity proofs.

use super::*;

const UNARY_SOURCE: &[u8] = b"base .const $1234\
\nstart lda #>base\
\n      lda #<base\
\n      lda #!0\
\n      lda #!7\
\n      lda #(~$f0)&$ff\
\n      lda #(--5)\
\n      lda #(+1+2)\
\n      lda #>$1200 + $34\
\n      lda #<$12ff + 2\
\n      lda #-2 ** 2\
\n      lda #~2 ** 2\
\n      lda #!2 ** 0\
\n      lda #(<$1201)\
\n      lda #1 ? >$1234 : <$1234\
\n      lda #0 ? >$1234 : <$1234\
\n      rts\n";

fn rust_unary_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(UNARY_SOURCE).expect("unary fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust unary authority");
    assert!(
        diagnostics.is_empty(),
        "Rust unary diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_unary_rust_oracle() {
    // Proof level A. This proves live Rust recursive unary behavior, outer
    // high/low precedence, unary-before-power precedence, ternary-arm entry,
    // and the complete instruction byte sequence. It does not prove native
    // execution.
    assert_eq!(
        rust_unary_bytes(),
        [
            0xa9, 0x12, 0xa9, 0x34, 0xa9, 0x01, 0xa9, 0x00, 0xa9, 0x0f, 0xa9, 0x05, 0xa9, 0x03,
            0xa9, 0x12, 0xa9, 0x01, 0xa9, 0x04, 0xa9, 0x09, 0xa9, 0x01, 0xa9, 0x01, 0xa9, 0x12,
            0xa9, 0x34, 0x60,
        ]
    );
}

#[test]
fn native_expression_unary_parser_runtime_contract() {
    // Proof level B. This proves valid unary-leading selected operands reach
    // the compiler, recursive unary operators compile at the power operand
    // tier, and high/low compile around a complete expression. It does not
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
            "CMPI.B #'(', D7",
            "CMPI.B #'~', D7",
            "CMPI.B #'!', D7",
            "CMPI.B #'<', D7",
            "CMPI.B #'>', D7",
            "textOk",
            "JSR expr_bridge.opcoreExvmEvalOperandV1",
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
            "compileExpression .block",
            "BSR.W compileHighLow",
            "compileHighLow",
            "MOVEQ #runtime.EXPRVM_UNARY_LOW, D6",
            "MOVEQ #runtime.EXPRVM_UNARY_HIGH, D6",
            "BSR.W compileHighLow",
            "BSR.W emitApplyUnaryD6",
            "compileTernary .block",
            "CMPI.B #'?', (A0)",
            "BSR.W compileHighLow",
            "CMPI.B #':', (A0)",
            "BSR.W compileHighLow",
            "compileSingleTerm .block",
            "CMPI.B #'~', (A0)",
            "CMPI.B #'!', (A0)",
            "BSR.W compileSingleTerm",
            "BSR.W emitApplyUnaryD6",
        ]
    ));
}

#[test]
fn native_expression_unary_fs_uae() {
    // Proof level D. This proves the real native CLI matches live Rust for all
    // fifteen unary/grouping instructions and discriminating precedence cases,
    // including both high/low ternary arms.
    // It does not prove string-literal expression semantics.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("unary FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "expression-unary",
        cpu_id: "65c02",
        source: UNARY_SOURCE,
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_unary_bytes()),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("unary FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native unary fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_unary_bytes(), "native unary bytes differ");
        }
    }
}
