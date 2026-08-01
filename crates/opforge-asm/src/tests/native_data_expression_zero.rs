//! Native zero-valued data-expression parity proofs.

use super::*;

const ZERO_DATA_SOURCE: &[u8] = b".org 0\
\nBASE .const $2000\
\n.byte (BASE & $ff), $7f, (0 || 0), (BASE >> 8)\
\n.byte $11, (BASE & $ff)\
\n        rts\n";

fn rust_zero_data_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(ZERO_DATA_SOURCE).expect("zero-data fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust zero-data authority");
    assert!(
        diagnostics.is_empty(),
        "Rust zero-data diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_data_expression_zero_rust_oracle() {
    // Proof level A. This proves live Rust accepts zero-valued expressions in
    // the first, middle, and later positions of a numeric byte list and fixes
    // their exact output. It does not prove native evaluation or guest output.
    assert_eq!(
        rust_zero_data_bytes(),
        [0x00, 0x7f, 0x00, 0x20, 0x11, 0x00, 0x60]
    );

    let (_, diagnostics) = assemble_source_entries_with_runtime_mode(
        &[".cpu 65c02", ".byte (MISSING & $ff), 1"],
        true,
    )
    .expect("unresolved Rust data expression should report diagnostics");
    assert!(
        !diagnostics.is_empty(),
        "Rust must reject an unresolved numeric data expression"
    );
}

#[test]
fn native_data_expression_zero_dispatch_contract() {
    // Proof level B. This proves the comma-operand path branches on expression
    // service status and accepts the returned value, including zero. It does
    // not execute the native code or prove final bytes.
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm assembly driver");
    let start = driver
        .find("readCommaOperandValueForStatement\t.block")
        .expect("comma operand evaluator block");
    let block = &driver[start..];
    let end = block
        .find("\t.bend  ; readCommaOperandValueForStatement")
        .expect("comma operand evaluator end");
    let block = &block[..end];
    assert!(source_contains_in_order(
        block,
        &[
            "jsr tkpkg.dispatchEvaluateExpressionV1",
            "bne.s evalPartFallback",
            "bsr.w readEvaluateExpressionValue",
            "bra.w evalPartOk",
        ]
    ));
    assert!(
        !block.contains("bsr.w readEvaluateExpressionValue\n\ttst.l d3"),
        "a successful zero expression must not be reclassified by value truthiness"
    );
}

#[test]
fn native_data_expression_zero_fs_uae() {
    // Proof level D. This proves the real native CLI emits the live Rust bytes
    // for zero/nonzero numeric expressions in every relevant list position.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("zero-data FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "data-expression-zero",
        cpu_id: "65c02",
        source: ZERO_DATA_SOURCE,
        package_bytes: package.as_slice(),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("zero-data FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native zero-data fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = fs::read(
                run.artifact_dir
                    .join("Work")
                    .join(crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE),
            )
            .expect("read native zero-data output");
            assert_eq!(
                native,
                rust_zero_data_bytes(),
                "native zero-data bytes differ"
            );
        }
    }
}

#[test]
fn native_data_expression_unresolved_fs_uae() {
    // Proof level D. This proves accepting a successful zero result does not
    // turn an unresolved expression into success. Guest completion and exit
    // status distinguish this rejection from an emulator crash.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("unresolved data-expression FS-UAE lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let source = b".org 0\n.byte (MISSING & $ff), 1\n";
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "data-expression-unresolved",
        cpu_id: "65c02",
        source,
        package_bytes: package.as_slice(),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("unresolved data-expression FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            let guest_exit_code = run.exit_code.unwrap_or_else(|| {
                panic!(
                    "unresolved native data expression produced no guest exit code\nstdout:\n{}\nstderr:\n{}",
                    run.stdout, run.stderr
                )
            });
            assert_ne!(guest_exit_code, 0, "unresolved expression must fail");
            let completion_marker = run
                .source_path
                .parent()
                .and_then(|work_dir| work_dir.parent())
                .expect("native CLI case source must be inside its case Work directory")
                .join("opforge_fsuae_smoke.done");
            assert!(
                completion_marker.is_file(),
                "unresolved expression must produce a guest completion marker"
            );
            assert!(
                !run.stderr.trim().is_empty(),
                "unresolved expression must produce diagnostics"
            );
        }
    }
}
