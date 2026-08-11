//! Native FS-UAE harness evidence tests.

use super::*;

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
    match crate::fs_uae_smoke::run_native_cli_directive_router_from_env(&workspace_root())
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
            let bytes = fs::read(
                run.artifact_dir
                    .join("Work")
                    .join(crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE),
            )
            .expect("read native directive-router output");
            assert_eq!(
                bytes,
                vec![
                    0xa9, 0x42, 0x99, 0x34, 0x12, 0x78, 0x56, 0x04, 0x03, 0x02, 0x01, b'O', b'K',
                    b'A', 0x00, 0x02, b'B', b'C',
                ]
            );
        }
    }
}

#[test]
fn external_fs_uae_native_cli_flow_navigation_preserves_nested_structural_skips() {
    // Proof level D. This runs the native CLI through false `.if`, zero `.for`,
    // `.match` default selection, and zero `.while` navigation. It does not
    // add or prove support for a new source CPU.
    match crate::fs_uae_smoke::run_native_cli_flow_navigation_from_env(&workspace_root())
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
            let bytes = fs::read(
                run.artifact_dir
                    .join("Work")
                    .join(crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE),
            )
            .expect("read native flow-navigation output");
            assert_eq!(bytes, vec![0x42]);
        }
    }
}

#[test]
fn native_macro_preprocessor_harness_fs_uae_proves_capture_lookup_and_nested_frame_rejection() {
    // Proof level D. The guest harness captures COPY/PAIR/TEXT/LOCAL, validates
    // bounded substitution, and proves a nested macro call fails without
    // overwriting the active caller frame or its restored source line.
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
                "native pipeline-selection harness failed: {}",
                runs[0].stdout
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
                "diagnostic harness should emit the complete macro fixture image: {}",
                runs[0].stdout
            );
        }
    }
}
