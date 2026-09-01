//! Native FS-UAE harness evidence tests.

use super::*;

struct NativeHarnessOracleDir(PathBuf);

impl Drop for NativeHarnessOracleDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn native_harness_live_rust_cli_oracle(label: &str, source: &str) -> Vec<u8> {
    let case_dir = create_temp_dir(label);
    let _case_dir_guard = NativeHarnessOracleDir(case_dir.clone());
    let input_path = case_dir.join("input.asm");
    let bin_path = case_dir.join("oracle.bin");
    fs::write(&input_path, source).expect("write native-harness Rust oracle source");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run native-harness live Rust CLI oracle");
    fs::read(&bin_path).expect("read native-harness live Rust CLI oracle")
}

#[test]
fn native_debug_contract_cli_header_fs_uae_proves_real_site_behavior() {
    match crate::fs_uae_smoke::run_native_cli_debug_event_from_env(&workspace_root())
        .expect("native CLI debug-event FS-UAE harness should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native CLI debug-event harness failed under FS-UAE\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
        }
    }
}

#[test]
fn external_fs_uae_native_cli_directive_router_emits_org_and_data_fixture() {
    // Proof level D. This proves the real native directive router emits bytes
    // identical to the live Rust CLI for the exact source in this case. It does
    // not prove other directives or source CPU families.
    let rust_oracle = native_harness_live_rust_cli_oracle(
        "native-directive-router-live-rust-cli",
        crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT,
    );
    assert_eq!(
        rust_oracle,
        vec![
            0xa9, 0x42, 0x99, 0x34, 0x12, 0x78, 0x56, 0x04, 0x03, 0x02, 0x01, b'O', b'K', b'A',
            0x00, 0x02, b'B', b'C',
        ]
    );
    match crate::fs_uae_smoke::run_native_cli_directive_router_from_env(
        &workspace_root(),
        &rust_oracle,
    )
    .expect("native directive-router FS-UAE fixture should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native directive-router fixture failed under FS-UAE\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            assert_eq!(run.verified_output.as_deref(), Some(rust_oracle.as_slice()));
        }
    }
}

#[test]
fn external_fs_uae_native_progress_cli_preserves_exact_artifact_and_exit() {
    // Proof level D. This runs the real CLI with counters enabled and heartbeat
    // and diagnostic abort disabled, requires fresh protocol completion and
    // explicit exit zero, and compares the exact artifact to the live Rust
    // oracle. It does not prove a full self-host run or runtime overhead.
    let rust_oracle = native_harness_live_rust_cli_oracle(
        "native-progress-cli-live-rust-cli",
        crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT,
    );
    match crate::fs_uae_smoke::run_native_progress_cli_parity_from_env(
        &workspace_root(),
        &rust_oracle,
    )
    .expect("native progress CLI parity should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native progress CLI parity failed under FS-UAE\nprotocol completed: {}\nguest exit: {:?}\nstdout:\n{}\nstderr:\n{}",
                run.protocol_completed, run.exit_code, run.stdout, run.stderr
            );
            assert_eq!(run.verified_output.as_deref(), Some(rust_oracle.as_slice()));
        }
    }
}

#[test]
fn external_fs_uae_native_cli_flow_navigation_preserves_nested_structural_skips() {
    // Proof level D. This runs the native CLI through false `.if`, zero `.for`,
    // `.match` default selection, and zero `.while` navigation and compares the
    // output with the live Rust CLI. It does not add or prove support for a new
    // source CPU.
    let rust_oracle = native_harness_live_rust_cli_oracle(
        "native-flow-navigation-live-rust-cli",
        crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_FLOW_NAVIGATION_INPUT_TEXT,
    );
    assert_eq!(rust_oracle, vec![0x42]);
    match crate::fs_uae_smoke::run_native_cli_flow_navigation_from_env(
        &workspace_root(),
        &rust_oracle,
    )
    .expect("native flow-navigation FS-UAE fixture should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native flow-navigation fixture failed under FS-UAE\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            assert_eq!(run.verified_output.as_deref(), Some(rust_oracle.as_slice()));
        }
    }
}

#[test]
fn native_macro_preprocessor_harness_fs_uae_proves_capture_lookup_and_nested_frame_rejection() {
    // Proof level D. The guest harness captures COPY/PAIR/TEXT/LOCAL, validates
    // bounded substitution, and proves a nested invocation frame can be pushed
    // and popped without overwriting its caller or restored source line.
    match crate::fs_uae_smoke::run_native_macro_preprocessor_harness_from_env(&workspace_root())
        .expect("native macro-preprocessor FS-UAE harness should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            assert!(
                runs[0].success,
                "native macro-preprocessor harness failed: guest exit {:?}, protocol_completed={}\nstdout:\n{}\nstderr:\n{}",
                runs[0].exit_code,
                runs[0].protocol_completed,
                runs[0].stdout,
                runs[0].stderr
            );
        }
    }
}

#[test]
fn native_pipeline_select_harness_fs_uae_proves_embedded_65c02_selection() {
    // Proof level D. This isolates the embedded package service selection path;
    // it does not prove the source CPU directive or macro expansion paths.
    match crate::fs_uae_smoke::run_native_pipeline_select_harness_from_env(&workspace_root())
        .expect("native pipeline-selection FS-UAE harness should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            assert!(
                runs[0].success,
                "native pipeline-selection harness failed: guest exit {:?}, protocol_completed={}\nstdout:\n{}\nstderr:\n{}",
                runs[0].exit_code,
                runs[0].protocol_completed,
                runs[0].stdout,
                runs[0].stderr
            );
        }
    }
}

#[test]
fn native_macro_cli_debug_event_harness_proves_complete_macro_fixture_image() {
    // Proof level E diagnostic. This localizes whether the full guest CLI
    // reached an 11-byte image; only native_macro_invocation_fixture_fs_uae is
    // macro artifact parity authority.
    match crate::fs_uae_smoke::run_native_macro_cli_debug_event_harness_from_env(&workspace_root())
        .expect("native macro CLI debug-event harness should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            assert!(
                runs[0].success,
                "diagnostic harness should emit the complete macro fixture image: guest exit {:?}, protocol_completed={}\nstdout:\n{}\nstderr:\n{}",
                runs[0].exit_code,
                runs[0].protocol_completed,
                runs[0].stdout,
                runs[0].stderr
            );
        }
    }
}
