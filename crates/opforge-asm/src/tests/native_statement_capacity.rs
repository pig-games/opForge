//! Native statement-record capacity boundary proofs.

use super::*;

#[test]
fn native_statement_capacity_tracks_bounded_source_record_capacity() {
    // Proof level B. This proves both native owners use one bounded 512-record
    // domain and that opasm rejects a statement before writing beyond it. It
    // does not prove a real Amiga-native run crosses the former 160-row limit.
    let root = workspace_root();
    let cli_constants =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/constants.asm"))
            .expect("read native CLI constants");
    let engine =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read native opasm engine");

    for (owner, source) in [("CLI", &cli_constants), ("opasm", &engine)] {
        assert!(
            source.contains("NATIVE_SOURCE_RECORD_CAPACITY   = 512"),
            "{owner} source capacity must stay explicitly bounded"
        );
        assert!(
            source.contains("NATIVE_STATEMENT_TABLE_CAPACITY = NATIVE_SOURCE_RECORD_CAPACITY"),
            "{owner} statement capacity must track the bounded source domain"
        );
    }
    assert!(source_contains_in_order(
        &engine,
        &[
            "cmpi.w #NATIVE_STATEMENT_TABLE_CAPACITY, d0",
            "bhs.w fail",
            "bsr.w storeStatementRecord",
        ]
    ));
}

#[test]
fn native_statement_capacity_over_160_fs_uae() {
    // Proof level D. This proves the real Amiga-native CLI stores and emits 161
    // ordinary statements exactly like Rust. It does not prove unrelated label,
    // expression, source-line, or image capacity behavior.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("statement-capacity FS-UAE lock poisoned");
    let root = workspace_root();
    let mut source = String::from("        .org $1000\n");
    for _ in 0..161 {
        source.push_str("        nop\n");
    }
    let mut rust_lines = vec![".cpu 65c02"];
    rust_lines.extend(source.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_lines, true)
        .expect("Rust statement-capacity authority");
    assert!(diagnostics.is_empty(), "Rust diagnostics: {diagnostics:?}");
    let rust_bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(rust_bytes.len(), 161, "one emitted byte per NOP");
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "statement_capacity_161",
        cpu_id: "65c02",
        source: source.as_bytes(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bytes),
    };

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("statement-capacity FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one native statement-capacity run");
            let run = &runs[0];
            assert!(
                run.success,
                "native 161-statement source failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bytes, "native 161-statement bytes differ");
        }
    }
}
