//! Native pass-one direct forward-reference stability proofs.

use super::*;

struct ForwardRefOracleDir(PathBuf);

impl Drop for ForwardRefOracleDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[test]
fn native_mos_forward_ref_stability_pass_one_contract() {
    // Proof level B. This proves the native expression boundary marks an
    // absent pass-one symbol unstable and the selector defers an
    // unstable-widen candidate while retaining it as a no-wider fallback.
    // It does not prove real 68020 execution or the final emitted bytes.
    let root = workspace_root();
    let expression = format_tokvm_asm_fragment(
        &fs::read_to_string(
            root.join("native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm"),
        )
        .expect("read native expression bridge"),
    );
    assert!(source_contains_in_order(
        &expression,
        &[
            "CMPI.W #1, D3",
            "MOVE.B #1, OpcoreExvmSawUnresolvedSymbol",
            "BSR.W emitPushLiteralD3",
        ]
    ));
    assert!(source_contains_in_order(
        &expression,
        &[
            "BSR.W runEvalProgram",
            "TST.B OpcoreExvmSawUnresolvedSymbol",
            "MOVEQ #1, D4",
            "MOVEQ #1, D5",
        ]
    ));

    let selection = format_tokvm_asm_fragment(
        &fs::read_to_string(
            root.join("native/motorola68000/amigaos/tkpkg/tkpkg_selection_service.asm"),
        )
        .expect("read native selection service"),
    );
    assert!(source_contains_in_order(
        &selection,
        &[
            "MOVE.B 2(A2), D4",
            "JSR operand.tkpkgMselTryBuildCandidateV1",
            "TST.B D4",
            "TST.B state.EncodeSelectedMselUnstable",
            "BSET #1, state.EncodeSelectedMselMatchFlags",
            "MOVE.W D1, state.EncodeSelectedMselFallbackLen",
        ]
    ));
    assert!(source_contains_in_order(
        &selection,
        &[
            "noOutput",
            "BTST #1, state.EncodeSelectedMselMatchFlags",
            "MOVE.W state.EncodeSelectedMselFallbackLen, D1",
            "MOVEQ #TKPKG_SELECTED_STATUS_OK, D0",
        ]
    ));

    let engine = format_tokvm_asm_fragment(
        &fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read native opasm engine"),
    );
    let comma_scan_start = engine
        .find("checkTopLevelComma")
        .expect("find selected-shape comma scan");
    let comma_scan_end = engine[comma_scan_start..]
        .find("\nready")
        .map(|offset| comma_scan_start + offset)
        .expect("find selected-shape ready label");
    let comma_scan = &engine[comma_scan_start..comma_scan_end];
    assert!(
        !comma_scan.contains("BEQ.W none"),
        "a top-level indexed suffix comma must reach suffix-shape inference"
    );
    assert!(source_contains_in_order(
        &engine,
        &[
            "ready",
            "BSR.W inferSelectedShapeSuffix",
            "CMPI.B #'y', D0",
            "BEQ.W directY",
        ]
    ));
}

#[test]
fn native_mos_forward_ref_stability_fs_uae() {
    // Proof level D. This proves the exact canonical mixed 6502/65C02 source
    // produces the same BIN artifact in the real Amiga-native CLI and live Rust
    // CLI, including both forward references across page boundaries. It does not
    // prove unrelated forward-reference forms or any additive fixture.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("forward-reference FS-UAE lock poisoned");
    let root = workspace_root();
    let source_path = root.join("examples/mos6502/mos_forward_ref_stability.asm");
    let source = fs::read(&source_path).expect("read canonical forward-reference source");
    let temp = create_temp_dir("native-mos-forward-ref-stability");
    let _temp_guard = ForwardRefOracleDir(temp.clone());
    let rust_hex = temp.join("mos_forward_ref_stability.hex");
    let rust_bin = temp.join("mos_forward_ref_stability.bin");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        source_path.to_string_lossy().into_owned(),
        "--hex".to_string(),
        rust_hex.to_string_lossy().into_owned(),
        "--bin".to_string(),
        rust_bin.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "6502".to_string(),
    ]);
    run_with_cli_with_context(&cli).expect("live Rust forward-reference CLI authority");
    assert_eq!(
        fs::read(&rust_hex).expect("read live Rust forward-reference HEX"),
        fs::read(root.join("examples/reference/mos6502/mos_forward_ref_stability.hex"))
            .expect("read checked-in forward-reference HEX"),
        "live Rust HEX must match the checked-in canonical reference"
    );
    let rust_bytes = fs::read(&rust_bin).expect("read live Rust forward-reference BIN");
    assert_eq!(
        rust_bytes.len(),
        0x105,
        "Rust BIN spans $00FD through $0201"
    );
    assert_eq!(
        &rust_bytes[..5],
        &[0xad, 0x01, 0x01, 0xea, 0x60],
        "Rust m6502 bytes must agree with the canonical source comment"
    );
    assert_eq!(
        &rust_bytes[0x100..],
        &[0x9c, 0x01, 0x02, 0xea, 0x60],
        "Rust 65C02 bytes must agree with the canonical source comment"
    );
    assert!(
        rust_bytes[5..0x100].iter().all(|byte| *byte == 0),
        "Rust BIN must materialize the forward .org gap"
    );

    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "mos_forward_ref_stability",
        cpu_id: "m6502",
        source: source.as_slice(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bytes),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("forward-reference FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one canonical forward-reference run");
            let run = &runs[0];
            assert!(
                run.success,
                "native canonical forward-reference source failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bytes, "native forward-reference bytes differ");
        }
    }
}

#[test]
fn native_mos_unstable_widen_no_wider_fallback_fs_uae() {
    // Proof level D. This proves an unstable-widen selector remains usable when
    // no wider selector exists for the same operand shape. It does not prove
    // the canonical page-crossing case, which has its own standalone test.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("no-wider fallback FS-UAE lock poisoned");
    let root = workspace_root();
    let source = b".org $0010\nstart\n        stx target,y\n        nop\ntarget\n        rts\n";
    let mut rust_lines = vec![".cpu 6502"];
    rust_lines.extend(
        std::str::from_utf8(source)
            .expect("fallback source UTF-8")
            .lines(),
    );
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_lines, true)
        .expect("live Rust no-wider fallback authority");
    assert!(
        diagnostics.is_empty(),
        "Rust no-wider fallback diagnostics: {diagnostics:?}"
    );
    let rust_bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(rust_bytes, [0x96, 0x13, 0xea, 0x60]);

    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "mos_unstable_widen_no_wider_fallback",
        cpu_id: "m6502",
        source,
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bytes),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("no-wider fallback FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one no-wider fallback run");
            let run = &runs[0];
            assert!(
                run.success,
                "native no-wider fallback source failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bytes, "native no-wider fallback bytes differ");
        }
    }
}
