//! Frozen maintained preparation workloads through the existing single-case runner.
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use serde_json::json;

fn normal_relative(path: &Path) -> bool {
    !path.as_os_str().is_empty()
        && path
            .components()
            .all(|part| matches!(part, std::path::Component::Normal(_)))
}

#[test]
fn preparation_artifact_paths_are_bounded() {
    for path in ["main.asm", "build/out.bin", "memory.bin"] {
        assert!(normal_relative(Path::new(path)));
    }
    for path in ["", "/tmp/out", "../out", "build/../out"] {
        assert!(!normal_relative(Path::new(path)));
    }
}

#[test]
#[ignore = "requires explicit preparation image, workload, fresh result directory and FS-UAE"]
fn native_preparation_benchmark_fs_uae() {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let workload = std::env::var("OPFORGE_PREPARATION_WORKLOAD").expect("modules or macros");
    assert!(matches!(workload.as_str(), "modules" | "macros"));
    let requested = PathBuf::from(
        std::env::var_os("OPFORGE_PREPARATION_RESULT_DIR").expect("fresh result directory"),
    );
    let parent = requested
        .parent()
        .expect("result parent")
        .canonicalize()
        .unwrap();
    let output = parent.join(requested.file_name().expect("result directory name"));
    assert!(
        !output.starts_with(workspace.join("target")),
        "results must be outside target"
    );
    fs::create_dir(&output).expect("fresh result directory required");
    let image_path = PathBuf::from(
        std::env::var_os("OPFORGE_PREPARATION_IMAGE").expect("prebuilt native image"),
    );
    let image = fs::read(&image_path).expect("read prebuilt native image");
    fs::write(output.join("input.hunk"), &image).unwrap();
    let inputs = output.join("inputs");
    fs::create_dir(&inputs).unwrap();
    let source = workspace
        .join("benchmarks/native-preparation")
        .join(&workload);
    let mut paths = fs::read_dir(source)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| {
            matches!(
                path.extension().and_then(|s| s.to_str()),
                Some("asm" | "inc")
            )
        })
        .collect::<Vec<_>>();
    paths.sort();
    let owned = paths
        .iter()
        .map(|path| {
            assert!(fs::symlink_metadata(path).unwrap().file_type().is_file());
            let name = path.file_name().unwrap().to_str().unwrap().to_owned();
            assert!(normal_relative(Path::new(&name)));
            let bytes = fs::read(path).unwrap();
            fs::write(inputs.join(&name), &bytes).unwrap();
            (name, bytes)
        })
        .collect::<Vec<_>>();
    assert!(owned.iter().any(|(name, _)| name == "main.asm"));
    let cache_mode = match std::env::var("OPFORGE_PREPARATION_LEAF_CACHE").as_deref() {
        Ok("cold") => Some("cold"),
        Ok("warm") => Some("warm"),
        Ok(other) => panic!("unsupported OPFORGE_PREPARATION_LEAF_CACHE value: {other}"),
        Err(std::env::VarError::NotPresent) => None,
        Err(error) => panic!("read OPFORGE_PREPARATION_LEAF_CACHE: {error}"),
    };
    let cache_entries = if cache_mode == Some("warm") {
        let directory = PathBuf::from(
            std::env::var_os("OPFORGE_PREPARATION_LEAF_ENTRIES")
                .expect("warm mode requires a local leaf-entry directory"),
        );
        let mut entries = fs::read_dir(directory)
            .expect("read warm leaf-entry directory")
            .filter_map(|entry| {
                let entry = entry.expect("leaf-entry directory item");
                let name = entry.file_name().into_string().ok()?;
                let relative = format!("cache/{name}");
                if !normal_relative(Path::new(&relative))
                    || Path::new(&name)
                        .extension()
                        .and_then(|value| value.to_str())
                        != Some("ofl")
                    || !fs::symlink_metadata(entry.path())
                        .ok()?
                        .file_type()
                        .is_file()
                {
                    return None;
                }
                Some((relative, fs::read(entry.path()).ok()?))
            })
            .collect::<Vec<_>>();
        entries.sort_by(|left, right| left.0.cmp(&right.0));
        entries
    } else {
        Vec::new()
    };
    let oracle_dir = output.join("oracle");
    fs::create_dir(&oracle_dir).unwrap();
    let cli = Cli::parse_from([
        "opforge".into(),
        "-i".into(),
        inputs.join("main.asm").to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        inputs.to_string_lossy().into_owned(),
        "-I".into(),
        inputs.to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).expect("validate live Rust oracle");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("assemble live Rust oracle");
    let oracle = fs::read(oracle_dir.join("build/out.bin")).expect("fresh Rust BIN");
    let hunk = fs::read(oracle_dir.join("build/out.hunk")).expect("fresh Rust Hunk");
    let expected = [
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/out.bin",
            rust_oracle: &oracle,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/out.hunk",
            rust_oracle: &hunk,
        },
    ];
    let mut files = owned
        .iter()
        .map(|(name, bytes)| OpforgeNativeCliGuestFile {
            relative_path: name,
            bytes,
        })
        .collect::<Vec<_>>();
    if cache_mode.is_some() {
        files.push(OpforgeNativeCliGuestFile {
            relative_path: "cache/keep",
            bytes: &[],
        });
        files.extend(
            cache_entries
                .iter()
                .map(|(name, bytes)| OpforgeNativeCliGuestFile {
                    relative_path: name,
                    bytes,
                }),
        );
    }
    let command = if cache_mode.is_some() {
        "-i Work:main.asm -M Work: -I Work: --leaf-cache Work:cache".to_owned()
    } else {
        "-i Work:main.asm -M Work: -I Work:".to_owned()
    };
    let case = OpforgeNativeCliParityCase {
        name: "native-preparation-benchmark",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(b""),
        command_template: Some(&command),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: OpforgeNativeCliProof::ExactArtifacts(&expected),
    };
    let outcome = match run_prebuilt_compact_cli_case_from_env(&workspace, &case, &image) {
        Ok(outcome) => outcome,
        Err(error) => {
            fs::write(output.join("failure.txt"), &error).unwrap();
            panic!("fresh preparation benchmark failed: {error}");
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
        assert!(normal_relative(relative));
        let destination = output.join("guest").join(relative);
        fs::create_dir_all(destination.parent().unwrap()).unwrap();
        fs::write(destination, bytes).unwrap();
    }
    let digest = opforge_self_host_package_digest(&image);
    assert_eq!(run.native_image_digest.as_deref(), Some(digest.as_str()));
    let artifacts = expected
        .iter()
        .map(|artifact| {
            let actual = run
                .captured_artifacts
                .get(Path::new(artifact.relative_path));
            json!({
                "path": artifact.relative_path,
                "exact_rust_match": actual.map(Vec::as_slice) == Some(artifact.rust_oracle),
                "captured_bytes": actual.map(Vec::len),
                "rust_bytes": artifact.rust_oracle.len(),
                "rust_digest": opforge_self_host_package_digest(artifact.rust_oracle),
                "captured_digest": actual.map(|bytes| opforge_self_host_package_digest(bytes)),
            })
        })
        .collect::<Vec<_>>();
    let exact = artifacts
        .iter()
        .all(|artifact| artifact["exact_rust_match"] == true);
    let success = run.success && run.protocol_completed && run.exit_code == Some(0) && exact;
    let sources = owned.iter().map(|(path, bytes)| json!({
        "path": path, "bytes": bytes.len(), "digest": opforge_self_host_package_digest(bytes),
    })).collect::<Vec<_>>();
    let result = json!({
        "success": success, "workload": workload, "sources": sources, "artifacts": artifacts,
        "leaf_cache_mode": cache_mode.unwrap_or("reference"),
        "leaf_cache_work": native_leaf_cache::cache_work(&run.stdout),
        // The single-case runner validates both challenge-bound START and DONE.
        "fresh_start_and_done_verified": run.protocol_completed,
        "guest_protocol_completed": run.protocol_completed, "assembler_exit_code": run.exit_code,
        "exact_rust_match": exact, "start_to_done_host_seconds": run.start_to_done_host_seconds,
        "native_image_digest": run.native_image_digest, "image_bytes": image.len(),
        "linked_static_reserved_bytes": native_package_loading_performance::linked_static_bytes(&image),
        "live_rust_oracle_digest": opforge_self_host_package_digest(&oracle),
        "output_bytes": oracle.len(),
    });
    fs::write(
        output.join("native-result.json"),
        serde_json::to_vec_pretty(&result).unwrap(),
    )
    .unwrap();
    eprintln!("PREPARATION_BENCHMARK_RESULT {result}");
    assert!(
        success,
        "all fresh native artifacts must match live Rust oracles"
    );
}
