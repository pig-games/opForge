//! Native label-table and symbol-snapshot capacity parity proofs.

use super::*;

#[test]
fn native_label_capacity_tracks_complete_source_record_domain() {
    // Proof level B. This proves native label storage and both read-only symbol
    // snapshot paths cover the existing 512-record source domain. It does not
    // prove real 68020 allocation or execution.
    let root = workspace_root();
    let engine =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read native opasm engine");
    assert!(engine.lines().any(|line| {
        line.split_whitespace().collect::<Vec<_>>()
            == [
                "NATIVE_LABEL_TABLE_CAPACITY",
                "=",
                "NATIVE_SOURCE_RECORD_CAPACITY",
            ]
    }));
    assert!(engine.contains("move.w #NATIVE_LABEL_TABLE_CAPACITY - 1, d0"));

    let context = fs::read_to_string(
        root.join("native/motorola68000/amigaos/tkpkg/tkpkg_runtime_context.asm"),
    )
    .expect("read native runtime context");
    assert!(context.lines().any(|line| {
        line.split_whitespace().collect::<Vec<_>>()
            == ["RUNTIME_CONTEXT_STABILITY_CAPACITY", "=", "512"]
    }));

    let operand =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_operand_eval.asm"))
            .expect("read native operand evaluator");
    assert!(operand.lines().any(|line| {
        line.split_whitespace().collect::<Vec<_>>()
            == ["SCOPED_SNAPSHOT_SOURCE_CAPACITY", "=", "512"]
    }));
    assert!(operand.lines().any(|line| {
        line.split_whitespace().collect::<Vec<_>>() == ["SCOPED_SNAPSHOT_CAPACITY", "=", "1024"]
    }));
}

#[test]
fn native_label_capacity_over_16_fs_uae() {
    // Proof level D. This proves the real native CLI stores more than sixteen
    // labels and exposes the last one to selected-instruction expression
    // evaluation. It does not prove the full 512-record overflow boundary.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("label-capacity FS-UAE lock poisoned");
    let root = workspace_root();
    let mut source = String::from("start\n");
    for index in 0..20 {
        source.push_str(&format!("value{index:02} .const {index}\n"));
    }
    source.push_str("        lda #value19\n        rts\n");

    let mut rust_lines = vec![".cpu 65c02"];
    rust_lines.extend(source.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_lines, true)
        .expect("live Rust label-capacity authority");
    assert!(
        diagnostics.is_empty(),
        "Rust label-capacity diagnostics: {diagnostics:?}"
    );
    let rust_bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(rust_bytes, [0xa9, 0x13, 0x60]);

    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "label-capacity-over-16",
        cpu_id: "65c02",
        source: source.as_bytes(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bytes),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("label-capacity FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one label-capacity run");
            let run = &runs[0];
            assert!(
                run.success,
                "native label-capacity source failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bytes, "native label-capacity bytes differ");
        }
    }
}
