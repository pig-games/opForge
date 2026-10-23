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

// One execution policy selects the preserved reference, without rebuilding or
// modifying the source workload. Older frozen cases keep their original command.
fn module_command(command: &str, mode: Option<&str>) -> String {
    match mode {
        None | Some("private") => command.to_owned(),
        Some("reference") => format!("{command} --reference-modules"),
        Some(other) => panic!("module mode must be private or reference, got {other:?}"),
    }
}

// Frozen cases retain their recorded explicit roots. Current self-host cases
// exercise the public directory/file shorthand, with no hidden -M/-I arguments.
fn input_command(value: Option<&str>) -> &str {
    match value {
        None => "-i src/main.asm -M src -I src/debug",
        Some("src") => "src",
        Some("src/main.asm") => "src/main.asm",
        Some(other) => panic!("unsupported full self-host input command: {other:?}"),
    }
}

// New exports exercise the CLI's bounded default without overriding it. Older
// frozen images retain their explicitly selected capture policy.
fn selfhost_capture_command(command: &str, mode: &str, default_capture: bool) -> String {
    if default_capture {
        assert_eq!(mode, "disk");
        assert_eq!(command, "src");
        command.to_owned()
    } else {
        capture_command(command, mode).replace("--capture-dir Work:", "--capture-dir scratch")
    }
}

#[test]
fn selfhost_input_defaults_are_explicit() {
    assert_eq!(input_command(Some("src")), "src");
    assert_eq!(input_command(Some("src/main.asm")), "src/main.asm");
    assert!(input_command(None).contains("-M src"));
    assert!(std::panic::catch_unwind(|| input_command(Some("src -I other"))).is_err());
    assert_eq!(selfhost_capture_command("src", "disk", true), "src");
    assert_eq!(
        selfhost_capture_command("src", "disk", false),
        "src --capture-dir scratch"
    );
    assert!(
        std::panic::catch_unwind(|| selfhost_capture_command("src", "resident", true)).is_err()
    );
}

#[test]
fn module_comparison_policy_is_explicit() {
    assert_eq!(module_command("-i source.asm", None), "-i source.asm");
    assert_eq!(
        module_command("-i source.asm", Some("private")),
        "-i source.asm"
    );
    assert_eq!(
        module_command("-i source.asm", Some("reference")),
        "-i source.asm --reference-modules"
    );
    assert!(std::panic::catch_unwind(|| module_command("", Some("typo"))).is_err());
}

// The existing runner captures files, not empty directories. This checks leaked
// scratch payloads; the component probe separately checks private dir removal.
fn is_capture_scratch_path(path: &Path) -> bool {
    path.strip_prefix("Work")
        .ok()
        .and_then(Path::parent)
        .is_some_and(|parent| {
            parent.components().any(|component| {
                component.as_os_str().to_str().is_some_and(|name| {
                    name.strip_prefix("opfg").is_some_and(|suffix| {
                        suffix.len() == 16 && suffix.bytes().all(|byte| byte.is_ascii_hexdigit())
                    })
                })
            })
        })
}

pub(super) fn capture_scratch_files(run: &FsUaeSmokeRun) -> Vec<String> {
    run.captured_artifacts
        .keys()
        .filter(|path| is_capture_scratch_path(path))
        .map(|path| path.to_string_lossy().into_owned())
        .collect()
}

#[test]
fn capture_cleanup_checks_nested_scratch_directories() {
    for path in [
        "Work/opfg0123456789abcdef/capture",
        "Work/scratch/opfg0123456789abcdef/capture",
    ] {
        assert!(is_capture_scratch_path(Path::new(path)), "{path}");
    }
    for path in [
        "Other/opfg0123456789abcdef/capture",
        "Work/scratch/opfg0123456789abcdef.uaem",
        "Work/scratch/opfg0123456789abcdef",
        "Work/scratch/opfg0123/capture",
        "Work/build/ofm/module.ofm",
    ] {
        assert!(!is_capture_scratch_path(Path::new(path)), "{path}");
    }
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

// AmigaDOS Date emits a whole-second HH:MM:SS field alongside its date.
// Each measured phase must finish within one day; midnight wraps naturally.
fn guest_seconds(bytes: &[u8]) -> u32 {
    std::str::from_utf8(bytes)
        .expect("guest Date output is text")
        .split_whitespace()
        .find_map(|field| {
            let parts = field.split(':').collect::<Vec<_>>();
            if parts.len() != 3 || parts.iter().any(|part| part.len() != 2) {
                return None;
            }
            let hour = parts[0].parse::<u32>().ok()?;
            let minute = parts[1].parse::<u32>().ok()?;
            let second = parts[2].parse::<u32>().ok()?;
            (hour < 24 && minute < 60 && second < 60).then_some(hour * 3600 + minute * 60 + second)
        })
        .expect("guest Date must contain HH:MM:SS")
}

#[test]
fn guest_phase_clock_parses_date_and_midnight() {
    assert_eq!(guest_seconds(b"Thursday 22-Oct-26 12:34:56\n"), 45296);
    let start = guest_seconds(b"23:59:59\n");
    let end = guest_seconds(b"00:00:02\n");
    assert_eq!((end + 86400 - start) % 86400, 3);
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
    assert!(input["module_mode"].is_null() || input["module_mode"].is_string());
    let capture_mode = capture_mode(input["capture_mode"].as_str());
    assert!(input["default_capture"].is_null() || input["default_capture"].is_boolean());
    let default_capture = input["default_capture"].as_bool().unwrap_or(false);
    assert!(input["incremental"].is_null() || input["incremental"].is_boolean());
    let incremental = input["incremental"].as_bool().unwrap_or(false);
    assert!(
        !incremental || !budget_failure_diagnostic,
        "incremental comparison requires two successful phases"
    );
    assert!(
        !incremental || input["module_mode"].as_str() != Some("reference"),
        "incremental comparison requires private modules"
    );
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
                name.starts_with("src/") || name.starts_with("packages/"),
                "only source workload and runtime package files expected"
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
    let live_path = output.join("build/opforge");
    let command = input_command(input["input_command"].as_str());
    let mut args = vec!["opForge".to_owned()];
    if input["input_command"].is_null() {
        args.extend([
            "-i".into(),
            source.join("main.asm").to_string_lossy().into_owned(),
            "--cpu".into(),
            "68020".into(),
            "-M".into(),
            source.to_string_lossy().into_owned(),
            "-I".into(),
            source.join("debug").to_string_lossy().into_owned(),
        ]);
    } else {
        args.push(
            if command == "src" {
                source.clone()
            } else {
                source.join("main.asm")
            }
            .to_string_lossy()
            .into_owned(),
        );
    }
    let cli = Cli::parse_from(args);
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
    let command = module_command(
        &selfhost_capture_command(command, capture_mode, default_capture),
        input["module_mode"].as_str(),
    );
    let assembly_command = command.clone();
    let instrumented = define_strings.contains(&"OPFORGE_MEMORY_TELEMETRY");
    let scratch_setup = if default_capture {
        ""
    } else {
        "MakeDir Work:scratch\n"
    };
    let command = if incremental {
        let cold_memory = if instrumented {
            "Copy Work:memory.bin Work:cold.memory.bin\n"
        } else {
            ""
        };
        let warm_memory = if instrumented {
            "Copy Work:memory.bin Work:warm.memory.bin\n"
        } else {
            ""
        };
        format!(
            "--version\n{scratch_setup}Copy Work:build/opforge Work:incremental_bootstrap\n\
             C:Date >Work:cold.start\n\
             Delete Work:build/opforge QUIET\n\
             Work:incremental_bootstrap {command} --rebuild >Work:cold.stdout\n\
             Echo $RC >Work:cold.rc\nC:Date >Work:cold.end\n\
             Copy Work:build/opforge Work:cold.hunk\n{cold_memory}\
             Delete Work:build/opforge QUIET\n\
             C:Date >Work:warm.start\n\
             Work:incremental_bootstrap {command} >Work:warm.stdout\n\
             Echo $RC >Work:warm.rc\nC:Date >Work:warm.end\n{warm_memory}\
             C:Type Work:cold.stdout\nC:Type Work:warm.stdout"
        )
    } else {
        // The source output shares the launcher's path. Remove it only after
        // copying the bootstrap, so stale input bytes can never satisfy proof.
        format!(
            "--version\n{scratch_setup}\
             Copy Work:build/opforge Work:selfhost_bootstrap\n\
             Delete Work:build/opforge QUIET\n\
             Work:selfhost_bootstrap {command}"
        )
    };
    let incremental_proof = [
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/cold.hunk",
            rust_oracle: &oracle,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/opforge",
            rust_oracle: &oracle,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/cold.rc",
            rust_oracle: b"0\n",
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/warm.rc",
            rust_oracle: b"0\n",
        },
    ];
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
        } else if incremental {
            OpforgeNativeCliProof::ExactArtifacts(&incremental_proof)
        } else {
            OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/build/opforge",
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
    // Source-directed self-host output also overwrites build/opforge.
    // The recorded bootstrap identity must remain the actual input executable.
    let bootstrap_digest = opforge_self_host_package_digest(&image);
    assert_eq!(
        run.native_image_digest.as_deref(),
        Some(bootstrap_digest.as_str())
    );
    let static_bytes = native_package_loading_performance::linked_static_bytes(&image);
    let scratch_files = capture_scratch_files(run);
    let memory = run
        .captured_artifacts
        .get(&PathBuf::from("Work/memory.bin"))
        .map(|bytes| native_package_loading_performance::memory(bytes));
    if !budget_failure_diagnostic {
        assert_eq!(memory.is_some(), instrumented, "measurement build identity");
    }
    let phase_artifact = |name: &str| {
        run.captured_artifacts
            .get(&PathBuf::from(format!("Work/{name}")))
            .unwrap_or_else(|| panic!("missing incremental phase artifact {name}"))
    };
    let phase_seconds = |phase: &str| {
        let start = guest_seconds(phase_artifact(&format!("{phase}.start")));
        let end = guest_seconds(phase_artifact(&format!("{phase}.end")));
        (end + 86400 - start) % 86400
    };
    let cold_seconds = incremental.then(|| phase_seconds("cold"));
    let warm_seconds = incremental.then(|| phase_seconds("warm"));
    let cold_memory = (incremental && instrumented)
        .then(|| native_package_loading_performance::memory(phase_artifact("cold.memory.bin")));
    let warm_memory = (incremental && instrumented)
        .then(|| native_package_loading_performance::memory(phase_artifact("warm.memory.bin")));
    let cache_counts = incremental.then(|| {
        let cold = super::native_incremental::counters(std::str::from_utf8(phase_artifact("cold.stdout")).unwrap());
        let warm = super::native_incremental::counters(std::str::from_utf8(phase_artifact("warm.stdout")).unwrap());
        json!({"cold_reused": cold.0, "cold_rebuilt": cold.1, "warm_reused": warm.0, "warm_rebuilt": warm.1})
    });
    let cache_reuse_verified = cache_counts.as_ref().is_none_or(|counts| {
        counts["cold_reused"] == 0
            && counts["cold_rebuilt"].as_u64().unwrap() > 0
            && counts["warm_reused"] == counts["cold_rebuilt"]
            && counts["warm_rebuilt"] == 0
    });
    if incremental {
        assert_eq!(phase_artifact("cold.rc").as_slice(), b"0\n");
        assert_eq!(phase_artifact("warm.rc").as_slice(), b"0\n");
    }
    // Inventory deliverables already captured by the runner; no guest observer
    // or additional file reads are added to the measured execution interval.
    let objects: Vec<_> = run
        .captured_artifacts
        .iter()
        .filter_map(|(path, bytes)| {
            let relative = path.strip_prefix("Work").ok()?;
            let directory = relative.parent()?.file_name()?.to_str()?;
            ((relative.parent() == Some(Path::new("build/ofm"))
                || directory == "objects"
                || directory.strip_prefix("objects-").is_some_and(|suffix| {
                    !suffix.is_empty() && suffix.bytes().all(|b| b.is_ascii_digit())
                }))
                && relative
                    .extension()
                    .is_some_and(|extension| extension == "ofm"))
            .then_some((relative, bytes.len()))
        })
        .collect();
    let object_sizes = objects.iter().map(|(_, bytes)| *bytes).collect::<Vec<_>>();
    // Retain single-run comparisons of older frozen executables. Incremental
    // comparisons additionally qualify the current shared-store contract.
    let legacy_objects = objects
        .iter()
        .filter(|(path, _)| path.parent() != Some(Path::new("build/ofm")))
        .count();
    let shared_markers = run
        .captured_artifacts
        .keys()
        .filter(|path| {
            path.parent() == Some(Path::new("Work/build/ofm"))
                && path.extension().is_some_and(|extension| extension == "ofi")
        })
        .count();
    if incremental {
        assert_eq!(legacy_objects, 0, "incremental builds use only build/ofm");
        let count = cache_counts.as_ref().unwrap()["warm_reused"]
            .as_u64()
            .unwrap() as usize;
        assert_eq!(
            object_sizes.len(),
            count,
            "cold and warm builds share one object set"
        );
        assert_eq!(
            shared_markers, count,
            "one validity marker per shared cached image"
        );
    }
    let private_modules = native_cli_frozen_comparison::private_module_count(&run.stdout);
    let store_io = native_cli_frozen_comparison::module_store_io(&run.stdout);
    // Heap-only images keep allocation accounting but omit progress diagnostics.
    // The progress profile additionally qualifies exact module I/O counters.
    if instrumented
        && define_strings.contains(&"OPFORGE_PREPARATION_PROGRESS")
        && !budget_failure_diagnostic
        && !input["module_mode"].is_null()
    {
        let count = private_modules.expect("module publication receipt");
        if input["module_mode"] == "reference" {
            assert_eq!(count, 0);
        }
        if capture_mode == "disk" {
            let io = store_io.expect("persisted module I/O receipt");
            if legacy_objects != 0 {
                assert_eq!(io[1], count, "legacy registry saves each admitted image");
                assert_eq!(
                    legacy_objects,
                    count as usize * if incremental { 2 } else { 1 }
                );
            } else {
                assert_eq!(
                    io[1] as usize + shared_markers,
                    count as usize,
                    "cached images are adopted; only uncached images need a registry write"
                );
                assert_eq!(
                    object_sizes.len(),
                    count as usize,
                    "one persistent object per admitted module"
                );
            }
            assert!(
                io[0] <= count,
                "each stored module is read at most once at final emission"
            );
        }
    }
    let result = json!({
        "assembly_command": assembly_command, "default_capture": default_capture,
        "capture_mode": capture_mode, "capture_scratch_files_remaining": scratch_files,
        "module_mode": input["module_mode"],
        "incremental": incremental, "cache_counts": cache_counts,
        "cache_reuse_verified": cache_reuse_verified,
        "cold_guest_seconds": cold_seconds, "warm_guest_seconds": warm_seconds,
        "guest_clock_resolution_seconds": if incremental { Some(1) } else { None },
        "cold_memory": cold_memory, "warm_memory": warm_memory,
        "cold_assembler_exit_code": if incremental { Some(0) } else { None },
        "warm_assembler_exit_code": if incremental { Some(0) } else { None },
        "exit_code_scope": if incremental { "whole guest wrapper; phase return codes proved separately" } else { "assembler" },
        "host_time_scope": if incremental { "setup and both assembly phases" } else { "single assembly" },
        "budget_failure_diagnostic": budget_failure_diagnostic,
        "success": run.success && run.protocol_completed && run.exit_code == Some(0) && scratch_files.is_empty() && cache_reuse_verified,
        "guest_protocol_completed": run.protocol_completed, "assembler_exit_code": run.exit_code,
        "exact_rust_match": run.verified_output.as_deref() == Some(oracle.as_slice()),
        "self_host": !budget_failure_diagnostic && define_strings.is_empty()
            && image == oracle && run.success && run.protocol_completed
            && run.exit_code == Some(0)
            && run.verified_output.as_deref() == Some(image.as_slice()),
        "start_to_done_host_seconds": run.start_to_done_host_seconds,
        "instrumented": instrumented,
        "release_start_to_done_host_seconds": if instrumented { None } else { run.start_to_done_host_seconds },
        "memory": memory, "private_modules": private_modules,
        "module_store_reads_writes_hits": store_io,
        "module_object_files": object_sizes.len(),
        "module_object_bytes": object_sizes.iter().sum::<usize>(),
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
    // Preserve completed timings/outputs even if a cache policy check fails.
    assert!(
        cache_reuse_verified,
        "full warm build must prove real module reuse and forced cold build must rebuild"
    );
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
