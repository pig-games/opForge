//! Bounded real-CLI performance corpus: fresh live Rust authority, never reports.

use super::*;
use crate::fs_uae_smoke::{
    FsUaeSmokeOutcome, OpforgeNativeCliExpectedArtifact, OpforgeNativeCliGuestFile,
    OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase, OpforgeNativeCliProof,
};
use std::collections::BTreeMap;

struct CorpusInput {
    corpus_sha256: String,
    package_bytes: Vec<u8>,
    package_sha256: String,
    cases: Vec<CorpusCase>,
}

struct CorpusCase {
    id: String,
    sha256: String,
    source: String,
    files: BTreeMap<String, String>,
    argv: Vec<String>,
    live_rust: CorpusOracle,
}

struct CorpusOracle {
    oracle_bytes: BTreeMap<String, Vec<u8>>,
}

impl CorpusInput {
    fn from_json(bytes: &[u8]) -> Self {
        let value: serde_json::Value = serde_json::from_slice(bytes).expect("live corpus JSON");
        let text = |row: &serde_json::Value, field: &str| {
            row[field].as_str().expect("corpus text field").to_string()
        };
        Self {
            corpus_sha256: text(&value, "corpus_sha256"),
            package_sha256: text(&value["package"], "sha256"),
            package_bytes: serde_json::from_value(value["package_bytes"].clone())
                .expect("live Rust package bytes"),
            cases: value["cases"]
                .as_array()
                .expect("corpus case array")
                .iter()
                .map(|row| CorpusCase {
                    id: text(row, "id"),
                    sha256: text(row, "sha256"),
                    source: text(row, "source"),
                    files: serde_json::from_value(row["files"].clone())
                        .expect("corpus source files"),
                    argv: serde_json::from_value(row["argv"].clone()).expect("corpus command"),
                    live_rust: CorpusOracle {
                        oracle_bytes: serde_json::from_value(
                            row["live_rust"]["oracle_bytes"].clone(),
                        )
                        .expect("live Rust artifact bytes"),
                    },
                })
                .collect(),
        }
    }
}

#[test]
fn external_fs_uae_native_production_corpus_parity() {
    run_native_production_corpus(None);
}

#[test]
fn external_fs_uae_native_production_corpus_diagnostic() {
    // Level E only: a controlled incomplete assembly is never corpus parity.
    if std::env::var("OPFORGE_NATIVE_CORPUS_DIAGNOSTIC").as_deref() != Ok("1") {
        eprintln!("SKIP: bounded corpus diagnosis requires explicit opt-in");
        return;
    }
    let visits: u32 = std::env::var("OPFORGE_NATIVE_CORPUS_ABORT_VISITS")
        .expect("diagnostic visit limit required")
        .parse()
        .expect("positive integer visit limit");
    assert!((1..=100_000).contains(&visits));
    assert_eq!(
        std::env::var("OPFORGE_PERFORMANCE_CORPUS").as_deref(),
        Ok("1")
    );
    assert!(matches!(
        std::env::var("OPFORGE_NATIVE_CORPUS_PROFILE").as_deref(),
        Ok("all" | "all-no-io")
    ));
    run_native_production_corpus(Some(visits));
}

fn native_corpus_profile_defines(profile_mode: &str, diagnostic: bool) -> Vec<String> {
    assert!(
        matches!(profile_mode, "off" | "all") || (diagnostic && profile_mode == "all-no-io"),
        "unknown or non-diagnostic corpus profile mode"
    );
    if profile_mode == "off" {
        assert!(
            !diagnostic,
            "diagnosis requires progress and abort counters"
        );
        return Vec::new();
    }
    let mut defines: Vec<String> = [
        "OPFORGE_DEBUG_CONTRACTS",
        "OPFORGE_PROGRESS_WORK_COUNTERS",
        "OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS",
        "OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL",
        "OPFORGE_PROGRESS_RUNTIME_COUNTERS",
        "OPFORGE_PROGRESS_PLATFORM_COUNTERS",
        "OPFORGE_PROGRESS_EXPORT_RECORDS",
    ]
    .into_iter()
    .map(String::from)
    .collect();
    if profile_mode == "all-no-io" {
        defines.push("OPFORGE_PROGRESS_PLATFORM_NO_IO".into());
    }
    defines
}

#[test]
fn native_corpus_io_kill_switch_is_diagnostic_only_and_preserves_other_defines() {
    // Level B host build contract, not native execution or timing evidence.
    let all = native_corpus_profile_defines("all", true);
    let disabled = native_corpus_profile_defines("all-no-io", true);
    assert_eq!(&disabled[..all.len()], all.as_slice());
    assert_eq!(&disabled[all.len()..], ["OPFORGE_PROGRESS_PLATFORM_NO_IO"]);
    assert_eq!(native_corpus_profile_defines("all", false), all);
    assert!(native_corpus_profile_defines("off", false).is_empty());
    for (mode, diagnostic) in [("all-no-io", false), ("off", true), ("unknown", true)] {
        assert!(
            std::panic::catch_unwind(|| native_corpus_profile_defines(mode, diagnostic)).is_err()
        );
    }
}

fn run_native_production_corpus(abort_visits: Option<u32>) {
    // Level D only for each successful complete fresh guest. This is bounded
    // corpus parity, not terminal self-host proof or physical A6000 timing.
    if std::env::var("OPFORGE_PERFORMANCE_CORPUS").as_deref() != Ok("1") {
        eprintln!("SKIP: set OPFORGE_PERFORMANCE_CORPUS=1 for bounded corpus confirmation");
        return;
    }
    let root = workspace_root();
    let template = std::env::var("OPFORGE_FS_UAE_CONFIG_TEMPLATE").expect("corpus config required");
    let config = fs::read_to_string(&template).expect("read measured emulator config");
    let sections: Vec<_> = config
        .lines()
        .map(str::trim)
        .filter(|line| line.starts_with('['))
        .map(str::to_ascii_lowercase)
        .collect();
    assert_eq!(
        sections,
        ["[fs-uae]"],
        "corpus requires one unambiguous config section"
    );
    assert!(
        !config
            .lines()
            .filter_map(|line| line.split_once('='))
            .any(|(key, _)| key.trim().to_ascii_lowercase().replace('-', "_") == "uae_cpu_model"),
        "uae_cpu_model would override the pinned CPU"
    );
    let cpus: Vec<_> = config
        .lines()
        .filter_map(|line| line.split_once('='))
        .filter(|(key, _)| key.trim() == "cpu")
        .map(|(_, value)| value.trim())
        .collect();
    assert_eq!(
        cpus,
        ["68020"],
        "corpus measurement must pin the actual emulator CPU"
    );
    for (key, expected) in [("uae_cpu_speed", "max"), ("jit_compiler", "0")] {
        let values: Vec<_> = config
            .lines()
            .filter_map(|line| line.split_once('='))
            .filter(|(name, _)| name.trim() == key)
            .map(|(_, value)| value.trim())
            .collect();
        assert_eq!(values, [expected], "corpus config must pin {key}");
    }
    assert_eq!(
        std::env::var("OPFORGE_FS_UAE_ARGS").as_deref(),
        Ok("{fsuae_config}"),
        "additional launcher arguments could override the recorded corpus config"
    );
    assert_eq!(
        std::env::var("OPFORGE_FS_UAE_POST_START_TIMEOUT_MS").as_deref(),
        Ok("120000")
    );
    let poll_ms = std::env::var("OPFORGE_FS_UAE_POLL_MS").unwrap_or("250".into());
    assert_eq!(
        poll_ms, "250",
        "matched corpus runs require the default 250ms polling interval"
    );
    let profile_mode = std::env::var("OPFORGE_NATIVE_CORPUS_PROFILE").unwrap_or("off".into());
    let mut owned_defines = native_corpus_profile_defines(&profile_mode, abort_visits.is_some());
    if let Some(visits) = abort_visits {
        owned_defines.push(format!("OPFORGE_PROGRESS_ABORT_VISITS={visits}"));
    }
    let clear_mode = std::env::var("OPFORGE_NATIVE_CORPUS_CLEAR").unwrap_or("longword".into());
    match clear_mode.as_str() {
        "longword" => {}
        "byte" => owned_defines.push("OPFORGE_SESSION_CLEAR_BYTE_REFERENCE".into()),
        _ => panic!("OPFORGE_NATIVE_CORPUS_CLEAR must be longword or byte"),
    }
    let statement_clear_mode =
        std::env::var("OPFORGE_NATIVE_CORPUS_STATEMENT_CLEAR").unwrap_or("live".into());
    match statement_clear_mode.as_str() {
        "live" => {}
        "full" => owned_defines.push("OPFORGE_SESSION_CLEAR_ALL_STATEMENTS".into()),
        _ => panic!("OPFORGE_NATIVE_CORPUS_STATEMENT_CLEAR must be live or full"),
    }
    let module_read_mode = std::env::var("OPFORGE_MODULE_SCAN_READ").unwrap_or("buffered".into());
    match module_read_mode.as_str() {
        "buffered" => {}
        "byte" => owned_defines.push("OPFORGE_MODULE_SCAN_BYTE_READ_REFERENCE".into()),
        _ => panic!("OPFORGE_MODULE_SCAN_READ must be buffered or byte"),
    }
    let defines: Vec<&str> = owned_defines.iter().map(String::as_str).collect();
    let mut command = std::process::Command::new("python3");
    command
        .arg(root.join("scripts/performance/production_corpus.py"))
        .arg("native-input");
    if let Ok(selection) = std::env::var("OPFORGE_NATIVE_CORPUS_CASES") {
        for name in selection.split(',') {
            command.arg("--case").arg(name);
        }
    }
    // The generator builds the locked release executable and runs the exact
    // source/command/package now; no result file is accepted as an oracle.
    let output = command.output().expect("generate live corpus Rust oracles");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let input = CorpusInput::from_json(&output.stdout);
    let package = input.package_bytes;
    eprintln!(
        "CORPUS_CONFIG {}",
        serde_json::json!({"template": config, "profile_mode": profile_mode,
        "corpus_sha256": input.corpus_sha256, "package_sha256": input.package_sha256,
        "package_mode": "explicit", "post_start_timeout_ms": 120000})
    );
    let mut failures = Vec::new();
    for case in input.cases {
        let source = case.files.get(&case.source).expect("corpus source present");
        let extra: Vec<_> = case
            .files
            .iter()
            .filter(|(name, _)| **name != case.source)
            .map(|(name, source)| OpforgeNativeCliGuestFile {
                relative_path: name,
                bytes: source.as_bytes(),
            })
            .collect();
        let artifact_paths: Vec<_> = case
            .live_rust
            .oracle_bytes
            .keys()
            .map(|path| format!("Work/{path}"))
            .collect();
        let expected: Vec<_> = artifact_paths
            .iter()
            .zip(case.live_rust.oracle_bytes.values())
            .map(|(path, bytes)| OpforgeNativeCliExpectedArtifact {
                relative_path: path,
                rust_oracle: bytes,
            })
            .collect();
        let mut args = case.argv.clone();
        args[0] = format!("Work:{}", case.source);
        for i in 0..args.len() - 1 {
            if matches!(args[i].as_str(), "--bin" | "--srec" | "-M" | "-I") {
                args[i + 1] = format!("Work:{}", args[i + 1]);
            }
        }
        assert!(
            args.iter().all(|arg| !arg.contains([' ', '\n', '"'])),
            "corpus arguments need explicit Amiga quoting"
        );
        let command = format!("{} --opasm-package {{package}}", args.join(" "));
        let actual = OpforgeNativeCliParityCase {
            name: &case.id,
            cpu_override: "68020",
            extra_assembly_defines: &defines,
            source_override: Some(source.as_bytes()),
            command_template: Some(&command),
            package_mode: OpforgeNativeCliPackageMode::Explicit(&package),
            extra_guest_files: &extra,
            proof: if abort_visits.is_some() {
                OpforgeNativeCliProof::ExpectedFailureContaining(
                    "ERROR OPC-NCLI020: native pass engine failed",
                )
            } else {
                OpforgeNativeCliProof::ExactArtifacts(&expected)
            },
        };
        eprintln!("CORPUS_START {}", case.id);
        let checked = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
                &root,
                &[actual],
            ) {
                Ok(FsUaeSmokeOutcome::Completed { runs }) => {
                    assert_eq!(runs.len(), 1);
                    let run = &runs[0];
                    assert!(run.protocol_completed);
                    assert!(
                        !run.artifact_dir.exists(),
                        "case evidence must be ephemeral"
                    );
                    if let Some(visits) = abort_visits {
                        assert!(!run.success && run.exit_code.is_some_and(|code| code != 0));
                        let profile =
                            super::native_harness_evidence::native_harness_decode_exported_profile(
                                run, false,
                            );
                        assert_eq!(profile["state"], "incomplete");
                        assert_eq!(profile["abort_requested"], true);
                        assert_eq!(profile["abort_after_visits"], visits);
                        assert_eq!(profile["statement_visits"], visits);
                        assert_eq!(
                            profile["exit_status"].as_i64(),
                            run.exit_code.map(i64::from)
                        );
                        for group in [
                            "work_multiplication",
                            "symbol_expression_work",
                            "runtime_execution",
                            "platform_io",
                        ] {
                            assert_eq!(profile[group]["overflow_bits"], 0);
                        }
                        assert_eq!(profile["overflow_bits"], 0);
                        assert_eq!(
                            profile["platform_io"]["enabled_groups"],
                            serde_json::json!({"io": profile_mode != "all-no-io", "bulk": true})
                        );
                        let raw_records: BTreeMap<_, _> = ["ofpr", "ofwk", "ofse", "ofvm", "ofio"]
                            .into_iter()
                            .map(|extension| {
                                (
                                    extension,
                                    captured_fs_uae_artifact(
                                        run,
                                        &format!("Work/opforge-profile.{extension}"),
                                    )
                                    .to_vec(),
                                )
                            })
                            .collect();
                        let protocol: BTreeMap<_, _> = ["started", "done", "exitcode", "stdout", "stderr"]
                            .into_iter().map(|extension| (extension, String::from_utf8_lossy(captured_fs_uae_artifact(run,
                                &format!("Work/case_artifacts/case_0000/opforge_fsuae_smoke.{extension}"))).into_owned())).collect();
                        eprintln!(
                            "CORPUS_DIAGNOSTIC {}",
                            serde_json::json!({
                                "id": case.id, "case_sha256": case.sha256, "corpus_sha256": input.corpus_sha256,
                                "package_sha256": input.package_sha256, "command_template": command,
                                "instrumentation_defines": owned_defines, "proof_level": "E",
                                "complete": false, "parity_passed": false, "protocol_completed": true,
                                "exit_status": run.exit_code, "profile": profile, "raw_records": raw_records,
                                "guest_protocol": protocol,
                            })
                        );
                        return;
                    }
                    assert!(run.exit_code == Some(0) && run.success);
                    for artifact in &expected {
                        assert_eq!(
                            captured_fs_uae_artifact(run, artifact.relative_path),
                            artifact.rust_oracle
                        );
                    }
                    for stream in ["stdout", "stderr"] {
                        let bytes = captured_fs_uae_artifact(
                            run,
                            &format!("Work/case_artifacts/case_0000/opforge_fsuae_smoke.{stream}"),
                        );
                        assert!(
                            bytes.is_empty(),
                            "native corpus emitted unexpected guest {stream}: {}",
                            String::from_utf8_lossy(bytes)
                        );
                    }
                    let profile = if profile_mode == "all" {
                        Some(
                            super::native_harness_evidence::native_harness_decode_exported_profile(
                                run, true,
                            ),
                        )
                    } else {
                        None
                    };
                    eprintln!(
                        "CORPUS_RESULT {}",
                        serde_json::json!({"id": case.id, "case_sha256": case.sha256,
                    "complete": true, "exit_status": 0, "exact_artifacts": artifact_paths,
                    "command_template": command, "package_sha256": input.package_sha256,
                    "profile_mode": profile_mode, "profile": profile,
                    "clear_mode": clear_mode, "statement_clear_mode": statement_clear_mode, "module_read_mode": module_read_mode,
                    "native_image_digest": run.native_image_digest,
                    "start_to_done_host_seconds": run.start_to_done_host_seconds,
                    "timing_poll_interval_ms": 250,
                    "timing_boundary": "host-observed case START to DONE; polling uncertainty applies"})
                    );
                }
                Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                    failures.push(format!("{}: no proof: {reason}", case.id))
                }
                Err(error) => {
                    eprintln!("CORPUS_FAILURE {}: {error}", case.id);
                    failures.push(format!("{}: {error}", case.id));
                }
            }
        }));
        if checked.is_err() {
            failures.push(format!(
                "{}: post-run validation panicked; no corpus result",
                case.id
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "{} corpus cases failed after all were attempted:\n{}",
        failures.len(),
        failures.join("\n")
    );
}
