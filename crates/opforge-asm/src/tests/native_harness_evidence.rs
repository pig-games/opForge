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

fn native_harness_decode_exported_profile(
    run: &crate::fs_uae_smoke::FsUaeSmokeRun,
    require_complete: bool,
) -> serde_json::Value {
    // Inputs are captured from this already-completed fresh guest, never a
    // stored report/oracle. All decoder scratch files are ephemeral as well.
    let case_dir = create_temp_dir("native-profile-decode");
    let _case_dir_guard = NativeHarnessOracleDir(case_dir.clone());
    let mut command = std::process::Command::new("python3");
    command.arg(workspace_root().join("scripts/performance/decode_native_progress.py"));
    for (extension, flag, expected_size) in [
        ("ofpr", None, 128),
        ("ofwk", Some("--work-record"), 128),
        ("ofse", Some("--symbol-expression-record"), 256),
        ("ofvm", Some("--runtime-record"), 192),
        ("ofio", Some("--platform-record"), 528),
    ] {
        let name = format!("opforge-profile.{extension}");
        let bytes = captured_fs_uae_artifact(run, &format!("Work/{name}"));
        assert_eq!(bytes.len(), expected_size, "invalid exported {name}");
        let path = case_dir.join(&name);
        fs::write(&path, bytes).expect("write ephemeral native profile decoder input");
        if let Some(flag) = flag {
            command.arg(flag);
        }
        command.arg(path);
    }
    if require_complete {
        command.arg("--require-complete");
    }
    let result = command.output().expect("run strict native profile decoder");
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    serde_json::from_slice(&result.stdout).expect("decode native profile JSON")
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
    // Proof level D. This runs the real CLI with progress, work-multiplication,
    // and detailed symbol/expression counters enabled while heartbeat and
    // diagnostic abort remain disabled. It requires fresh protocol completion,
    // explicit exit zero, and exact equality to the live Rust oracle. It does
    // not prove corpus-wide counts or runtime overhead.
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
fn external_fs_uae_native_platform_profile_cli_preserves_exact_artifact_and_exit() {
    // Proof level D: the real CLI with platform counters must complete its
    // fresh guest protocol and match the actual source's live Rust oracle.
    // This does not prove counter coverage, corpus attribution, or speed.
    let rust_oracle = native_harness_live_rust_cli_oracle(
        "native-platform-profile-cli-live-rust-cli",
        crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT,
    );
    match crate::fs_uae_smoke::run_native_platform_profile_cli_parity_from_env(
        &workspace_root(),
        &rust_oracle,
    )
    .expect("platform profile CLI parity should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.protocol_completed,
                "guest protocol did not complete: {}",
                run.stderr
            );
            assert_eq!(run.exit_code, Some(0), "{}\n{}", run.stdout, run.stderr);
            assert!(run.success, "platform profile CLI failed: {}", run.stderr);
            assert_eq!(run.verified_output.as_deref(), Some(rust_oracle.as_slice()));
            let profile = native_harness_decode_exported_profile(run, true);
            let mode =
                std::env::var("OPFORGE_NATIVE_PROFILE_PLATFORM_MODE").unwrap_or("all".into());
            let io_enabled = matches!(mode.as_str(), "all" | "io");
            let bulk_enabled = matches!(mode.as_str(), "all" | "bulk");
            assert_eq!(profile["platform_io"]["enabled_groups"]["io"], io_enabled);
            assert_eq!(
                profile["platform_io"]["enabled_groups"]["bulk"],
                bulk_enabled
            );
            assert_eq!(
                profile["platform_io"]["logical_lines"].as_u64().unwrap() > 0,
                io_enabled
            );
            if io_enabled {
                let source =
                    crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT;
                assert_eq!(profile["platform_io"]["source_bytes"], source.len());
                assert_eq!(profile["platform_io"]["reads"]["source"], source.len() + 1);
                assert_eq!(
                    profile["platform_io"]["logical_lines"],
                    source.lines().count()
                );
                assert_eq!(profile["platform_io"]["short_reads"], 1);
                assert_eq!(profile["platform_io"]["writes"]["artifact"], 1);
                assert_eq!(
                    profile["platform_io"]["write_bytes"]["artifact"],
                    rust_oracle.len()
                );
            }
            if bulk_enabled {
                let resets = 1
                    + profile["work_multiplication"]["layout_rounds"]
                        .as_u64()
                        .unwrap()
                    + profile["work_multiplication"]["final_emissions"]
                        .as_u64()
                        .unwrap();
                let presence = &profile["platform_io"]["bulk_by_range"]["presence"]["clears"];
                assert_eq!(presence["calls"], resets);
                assert_eq!(presence["completed_bytes"], resets * 1_048_576);
            }
            assert!(
                (profile["platform_io"]["clears"]["completed_bytes"]
                    .as_u64()
                    .unwrap()
                    > 0)
                    == bulk_enabled
            );
            eprintln!(
                "PLATFORM_PROFILE {}",
                serde_json::to_string(&profile).unwrap()
            );
            eprintln!(
                "PLATFORM_EXECUTABLE_BYTES {}",
                captured_fs_uae_artifact(run, "Work/build/opforge_cli").len()
            );
        }
    }
}

#[test]
fn external_fs_uae_native_platform_profile_output_open_failure() {
    // Proof level D for a real DOS error with profiling enabled: exact fresh
    // completion, explicit nonzero guest exit, and the required diagnostic.
    // This does not prove arbitrary short writes or corpus-wide counter values.
    use crate::fs_uae_smoke::{
        OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let case = OpforgeNativeCliParityCase {
        name: "native-platform-output-open-failure",
        cpu_override: "68020",
        extra_assembly_defines: &[
            crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_DEFINE,
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_PROGRESS_PLATFORM_COUNTERS",
            "OPFORGE_PROGRESS_EXPORT_RECORDS",
            "OPFORGE_PROGRESS_WORK_COUNTERS",
            "OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS",
            "OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL",
            "OPFORGE_PROGRESS_RUNTIME_COUNTERS",
        ],
        source_override: Some(
            crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT.as_bytes(),
        ),
        command_template: Some("{input} --bin Work:absent-platform-dir/out.bin --cpu m6502"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExpectedFailureContaining(
            "ERROR OPC-NCLI043: native output file open failed",
        ),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &[case],
    )
    .expect("profiled output failure must complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(run.protocol_completed, "{}", run.stderr);
            assert!(
                run.exit_code.is_some_and(|code| code != 0),
                "{}\n{}",
                run.stdout,
                run.stderr
            );
            // `success` means guest exit zero, not expected-negative proof.
            // The runner already validated the fresh failure protocol/diagnostic.
            assert!(!run.success);
            assert!(format!("{}\n{}", run.stdout, run.stderr)
                .contains("ERROR OPC-NCLI043: native output file open failed"));
            let profile = native_harness_decode_exported_profile(run, false);
            assert_eq!(profile["state"], "incomplete");
            assert_eq!(
                profile["exit_status"].as_i64(),
                run.exit_code.map(i64::from)
            );
            assert_eq!(profile["platform_io"]["state"], "incomplete");
            assert_eq!(profile["platform_io"]["overflow_bits"], 0);
        }
    }
}

#[test]
fn external_fs_uae_native_progress_only_cli_preserves_exact_artifact_and_exit() {
    // Proof level D. This is the progress-only control for bounded Item 0b
    // perturbation measurements. It proves exact output/exit for this fixture,
    // not a performance improvement or terminal self-hosting.
    let rust_oracle = native_harness_live_rust_cli_oracle(
        "native-progress-only-cli-live-rust-cli",
        crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT,
    );
    match crate::fs_uae_smoke::run_native_progress_only_cli_parity_from_env(
        &workspace_root(),
        &rust_oracle,
    )
    .expect("native progress-only CLI parity should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native progress-only CLI parity failed under FS-UAE\nprotocol completed: {}\nguest exit: {:?}\nstdout:\n{}\nstderr:\n{}",
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
