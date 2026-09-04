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
    let profile_mode = std::env::var("OPFORGE_NATIVE_CORPUS_PROFILE").unwrap_or("off".into());
    assert!(
        matches!(profile_mode.as_str(), "off" | "all"),
        "unknown corpus profile mode"
    );
    let profile_defines = [
        "OPFORGE_DEBUG_CONTRACTS",
        "OPFORGE_PROGRESS_WORK_COUNTERS",
        "OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS",
        "OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL",
        "OPFORGE_PROGRESS_RUNTIME_COUNTERS",
        "OPFORGE_PROGRESS_PLATFORM_COUNTERS",
        "OPFORGE_PROGRESS_EXPORT_RECORDS",
    ];
    let defines: &[&str] = if profile_mode == "all" {
        &profile_defines
    } else {
        &[]
    };
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
            extra_assembly_defines: defines,
            source_override: Some(source.as_bytes()),
            command_template: Some(&command),
            package_mode: OpforgeNativeCliPackageMode::Explicit(&package),
            extra_guest_files: &extra,
            proof: OpforgeNativeCliProof::ExactArtifacts(&expected),
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
                    assert!(run.protocol_completed && run.exit_code == Some(0) && run.success);
                    assert!(
                        !run.artifact_dir.exists(),
                        "case evidence must be ephemeral"
                    );
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
                    "profile_mode": profile_mode, "profile": profile})
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
