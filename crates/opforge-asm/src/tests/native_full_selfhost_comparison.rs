//! Full frozen-input comparisons using exported images and a freshly assembled Rust oracle.
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use serde_json::{json, Value};

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
    let specification =
        PathBuf::from(std::env::var_os("OPFORGE_FULL_COMPARISON_CASE").expect("case JSON path"));
    let directory = specification.parent().unwrap();
    let input: Value = serde_json::from_slice(&fs::read(&specification).unwrap()).unwrap();
    assert_eq!(input["schema"], 1);
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
    let case = OpforgeNativeCliParityCase {
        name: "full-exported-selfhost-comparison", cpu_override: "68020", extra_assembly_defines: &define_strings,
        source_override: Some(b""),
        command_template: Some("-i src/experimental/opforge_compact_cli.asm --hunk Work:output.hunk -M src -I src/debug"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault, extra_guest_files: &files,
        proof: OpforgeNativeCliProof::ExactArtifact { relative_path: "Work/output.hunk", rust_oracle: &oracle },
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
    let result = json!({
        "success": run.success && run.protocol_completed && run.exit_code == Some(0),
        "guest_protocol_completed": run.protocol_completed, "assembler_exit_code": run.exit_code,
        "exact_rust_match": run.verified_output.as_deref() == Some(oracle.as_slice()),
        "start_to_done_host_seconds": run.start_to_done_host_seconds,
        "native_image_digest": run.native_image_digest, "image_bytes": image.len(),
        "linked_static_reserved_bytes": static_bytes, "live_rust_oracle_digest": opforge_self_host_package_digest(&oracle),
        "output_bytes": oracle.len(), "case_sha256": input["case_sha256"], "metadata": input["metadata"],
    });
    fs::write(
        output.join("native-result.json"),
        serde_json::to_vec_pretty(&result).unwrap(),
    )
    .unwrap();
    eprintln!("FULL_COMPARISON_RESULT {result}");
    assert!(run.protocol_completed && run.success && run.exit_code == Some(0));
    assert_eq!(run.verified_output.as_deref(), Some(oracle.as_slice()));
}
