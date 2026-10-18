//! Full frozen-input comparisons using exported images and a freshly assembled Rust oracle.
//! Case JSON optionally sets capture_mode to disk or resident (the default).
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use serde_json::{json, Value};

// Optional case `capture_mode` and preparation environment use the same explicit
// values. Unset preserves the resident reference path.
pub(super) fn capture_mode(value: Option<&str>) -> &'static str {
    match value {
        None | Some("resident") => "resident",
        Some("disk") => "disk",
        Some(other) => panic!("capture mode must be resident or disk, got {other:?}"),
    }
}

pub(super) fn capture_command(command: &str, mode: &str) -> String {
    if mode == "disk" {
        format!("{command} --capture-dir Work:")
    } else {
        command.to_owned()
    }
}

// The existing runner captures files, not empty directories. This checks leaked
// scratch payloads; the component probe separately checks private dir removal.
pub(super) fn capture_scratch_files(run: &FsUaeSmokeRun) -> Vec<String> {
    run.captured_artifacts
        .keys()
        .filter_map(|path| {
            let relative = path.strip_prefix("Work").ok()?;
            let directory = relative.components().next()?.as_os_str().to_str()?;
            let suffix = directory.strip_prefix("opfg")?;
            (suffix.len() == 16 && suffix.bytes().all(|byte| byte.is_ascii_hexdigit()))
                .then(|| path.to_string_lossy().into_owned())
        })
        .collect()
}

#[test]
fn native_capture_configuration_is_explicit_and_defaults_to_resident() {
    assert_eq!(capture_mode(None), "resident");
    assert_eq!(capture_mode(Some("resident")), "resident");
    assert_eq!(capture_mode(Some("disk")), "disk");
    assert_eq!(
        capture_command("-i source.asm", "resident"),
        "-i source.asm"
    );
    assert_eq!(
        capture_command("-i source.asm", "disk"),
        "-i source.asm --capture-dir Work:"
    );
    assert!(std::panic::catch_unwind(|| capture_mode(Some("typo"))).is_err());
}

fn checked_file(case: &Path, row: &Value) -> Vec<u8> {
    let path = row["path"].as_str().expect("case file path");
    let relative = Path::new(path);
    assert!(
        !relative.is_absolute()
            && relative
                .components()
                .all(|part| matches!(part, std::path::Component::Normal(_))),
        "relative case file"
    );
    let bytes = fs::read(case.join(relative)).expect("read case file");
    assert_eq!(
        opforge_self_host_package_digest(&bytes),
        row["digest"].as_str().unwrap(),
        "case file changed: {path}"
    );
    bytes
}

#[test]
#[ignore = "requires prepared full comparison case and configured FS-UAE"]
fn native_full_selfhost_comparison_fs_uae() {
    full_selfhost_comparison(false);
}

// Retain existing memory/progress receipts for a known failed constrained run.
// A passing diagnostic test is NEVER an assembly success or a fit qualification.
#[test]
#[ignore = "requires a known failing full case and the 8 MiB FS-UAE profile"]
fn native_full_selfhost_budget_failure_fs_uae() {
    assert_eq!(
        std::env::var("OPFORGE_FS_UAE_MEMORY_PROFILE").as_deref(),
        Ok("68020-8m")
    );
    full_selfhost_comparison(true);
}

fn full_selfhost_comparison(budget_failure_diagnostic: bool) {
    let specification =
        PathBuf::from(std::env::var_os("OPFORGE_FULL_COMPARISON_CASE").expect("case JSON path"));
    let directory = specification.parent().unwrap();
    let input: Value = serde_json::from_slice(&fs::read(&specification).unwrap()).unwrap();
    assert_eq!(input["schema"], 1);
    assert!(input["capture_mode"].is_null() || input["capture_mode"].is_string());
    let capture_mode = capture_mode(input["capture_mode"].as_str());
    let output = directory.join("result");
    fs::create_dir(&output).expect("fresh result directory required");
    let image = checked_file(directory, &input["image"]);
    let exported_oracle = checked_file(directory, &input["oracle"]);
    let rows = input["files"].as_array().unwrap();
    let owned = rows
        .iter()
        .map(|row| {
            let name = row["guest_path"].as_str().unwrap().to_owned();
            assert!(
                name.starts_with("src/"),
                "only source workload files expected"
            );
            (name, checked_file(directory, row))
        })
        .collect::<Vec<_>>();
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let source = directory.join("inputs/src");
    // Keep the source's explicit sections on .output, as the exporter does.
    // A CLI Hunk override creates another request without those sections.
    let live_path = output.join("build/opforge_compact");
    let cli = Cli::parse_from([
        "opForge".into(),
        "-i".into(),
        source
            .join("experimental/opforge_compact_cli.asm")
            .to_string_lossy()
            .into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        source.to_string_lossy().into_owned(),
        "-I".into(),
        source.join("debug").to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).expect("validate live full workload oracle");
    config.out_dir = Some(output.clone());
    assert!(config.defines.is_empty(), "release live oracle");
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| {
        let message = match error {
            cli_core::CliRunError::Assembler { error, .. } => {
                let details = error
                    .diagnostics()
                    .iter()
                    .take(8)
                    .map(|diagnostic| {
                        format!(
                            "{}:{}: {}",
                            diagnostic.file.as_deref().unwrap_or("<unknown>"),
                            diagnostic.line,
                            diagnostic
                                .error
                                .message()
                                .chars()
                                .take(240)
                                .collect::<String>()
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(" | ");
                format!(
                    "{}; {details}",
                    error.summary().chars().take(500).collect::<String>()
                )
            }
            cli_core::CliRunError::Workflow { error, .. } => {
                error.to_string().chars().take(500).collect()
            }
            _ => "assembly terminated with warnings".into(),
        };
        fs::write(output.join("failure.txt"), &message).unwrap();
        panic!("live full Rust oracle assembly failed: {message}");
    });
    let oracle = fs::read(&live_path).expect("fresh live Rust Hunk");
    assert!(
        oracle == exported_oracle,
        "validated export must match entire live Rust Hunk: live={} ({} bytes), exported={} ({} bytes)",
        opforge_self_host_package_digest(&oracle), oracle.len(),
        opforge_self_host_package_digest(&exported_oracle), exported_oracle.len()
    );
    let files = owned
        .iter()
        .map(|(path, bytes)| OpforgeNativeCliGuestFile {
            relative_path: path,
            bytes,
        })
        .collect::<Vec<_>>();
    let define_strings = input["bootstrap_defines"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_str().unwrap())
        .collect::<Vec<_>>();
    let command = capture_command(
        "-i src/experimental/opforge_compact_cli.asm --hunk Work:output.hunk -M src -I src/debug",
        capture_mode,
    );
    let case = OpforgeNativeCliParityCase {
        name: "full-exported-selfhost-comparison",
        cpu_override: "68020",
        extra_assembly_defines: &define_strings,
        source_override: Some(b""),
        command_template: Some(&command),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: if budget_failure_diagnostic {
            OpforgeNativeCliProof::ExpectedFailureWithDiagnostic
        } else {
            OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.hunk",
                rust_oracle: &oracle,
            }
        },
    };
    let outcome = match run_prebuilt_compact_cli_case_from_env(&workspace, &case, &image) {
        Ok(outcome) => outcome,
        Err(error) => {
            fs::write(output.join("failure.txt"), &error).unwrap();
            panic!("fresh full comparison failed: {error}");
        }
    };
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("actual native execution required")
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    fs::write(output.join("assembly.stdout"), &run.stdout).unwrap();
    fs::write(output.join("assembly.stderr"), &run.stderr).unwrap();
    for (path, bytes) in &run.captured_artifacts {
        let relative = path.strip_prefix("Work").expect("captured Work artifact");
        assert!(relative
            .components()
            .all(|part| matches!(part, std::path::Component::Normal(_))));
        let destination = output.join(relative);
        fs::create_dir_all(destination.parent().unwrap()).unwrap();
        fs::write(destination, bytes).unwrap();
    }
    // Source-directed self-host output also overwrites build/opforge_compact.
    // The recorded bootstrap identity must remain the actual input executable.
    let bootstrap_digest = opforge_self_host_package_digest(&image);
    assert_eq!(
        run.native_image_digest.as_deref(),
        Some(bootstrap_digest.as_str())
    );
    let static_bytes = native_package_loading_performance::linked_static_bytes(&image);
    let scratch_files = capture_scratch_files(run);
    let result = json!({
        "capture_mode": capture_mode, "capture_scratch_files_remaining": scratch_files,
        "budget_failure_diagnostic": budget_failure_diagnostic,
        "success": run.success && run.protocol_completed && run.exit_code == Some(0) && scratch_files.is_empty(),
        "guest_protocol_completed": run.protocol_completed, "assembler_exit_code": run.exit_code,
        "exact_rust_match": run.verified_output.as_deref() == Some(oracle.as_slice()),
        "start_to_done_host_seconds": run.start_to_done_host_seconds,
        "native_image_digest": run.native_image_digest, "image_bytes": image.len(),
        "linked_static_reserved_bytes": static_bytes, "live_rust_oracle_digest": opforge_self_host_package_digest(&oracle),
        "output_bytes": run.verified_output.as_ref().map(Vec::len), "oracle_bytes": oracle.len(),
        "case_sha256": input["case_sha256"], "metadata": input["metadata"],
    });
    fs::write(
        output.join("native-result.json"),
        serde_json::to_vec_pretty(&result).unwrap(),
    )
    .unwrap();
    eprintln!("FULL_COMPARISON_RESULT {result}");
    assert!(
        scratch_files.is_empty(),
        "capture scratch files survived cleanup: {scratch_files:?}"
    );
    if budget_failure_diagnostic {
        assert!(run.protocol_completed && !run.success && run.exit_code == Some(20));
        assert!(run.verified_output.is_none());
    } else {
        assert!(run.protocol_completed && run.success && run.exit_code == Some(0));
        assert_eq!(run.verified_output.as_deref(), Some(oracle.as_slice()));
    }
}
