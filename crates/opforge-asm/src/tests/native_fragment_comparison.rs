//! Frozen full CLI comparison; only the supplied bootstrap changes between roles.
//! Exports are prepared separately with export_compact_self_host_bundle. This test
//! reassembles manifest-listed frozen inputs with Rust before any native launch.
//! Set OPFORGE_FRAGMENT_FROZEN_BUNDLE, OPFORGE_FRAGMENT_BASELINE_RELEASE,
//! OPFORGE_FRAGMENT_CANDIDATE_RELEASE, OPFORGE_FRAGMENT_BASELINE_MEMORY and
//! OPFORGE_FRAGMENT_CANDIDATE_MEMORY to independent embedded export directories.
//! Run native_fragment_frozen_comparison with --ignored --nocapture
//! --test-threads=1 and the normal FS-UAE environment. Each workload runs three
//! releases per role plus one separately instrumented memory run per role.
//! A full release run is a self-build only when the live oracle equals its
//! bootstrap and the frozen/export source manifest digests are identical.
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use serde_json::{json, Value};

fn bundle(variable: &str) -> (PathBuf, Value, Vec<u8>, Vec<u8>) {
    let root = PathBuf::from(std::env::var_os(variable).expect(variable));
    let manifest: Value =
        serde_json::from_slice(&fs::read(root.join("manifest.json")).unwrap()).unwrap();
    assert_eq!(manifest["kind"], "compact-self-host-local-export");
    assert_eq!(manifest["manifest_version"], 1);
    assert_eq!(manifest["release_defines"], json!([]));
    assert_eq!(manifest["bootstrap_package_storage"], "embedded");
    assert_eq!(manifest["output_package_storage"], "embedded");
    let image = fs::read(root.join("opforge")).unwrap();
    let package = fs::read(root.join(manifest["runtime_package_file"].as_str().unwrap())).unwrap();
    assert_eq!(
        opforge_self_host_package_digest(&image),
        manifest["bootstrap_hunk_digest"]
    );
    assert_eq!(
        opforge_self_host_package_digest(&package),
        manifest["runtime_package_digest"]
    );
    assert_eq!(
        image
            .windows(package.len())
            .filter(|bytes| *bytes == package)
            .count(),
        1,
        "bootstrap must contain the exact frozen 68020 package once"
    );
    (root, manifest, image, package)
}

fn oracle(root: &Path, entry: &str, output: &Path, binary: bool) -> Vec<u8> {
    let mut args = vec![
        "opForge".to_owned(),
        root.join(entry).to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        root.join("src").to_string_lossy().into_owned(),
        "-I".into(),
        root.join("src/debug").to_string_lossy().into_owned(),
    ];
    if binary {
        args.extend([
            "--bin".into(),
            output.join("output.bin").to_string_lossy().into_owned(),
        ]);
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(output.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).expect("live relocated Rust oracle");
    fs::read(output.join(if binary {
        "output.bin"
    } else {
        "build/opforge_compact"
    }))
    .unwrap()
}

// Existing gated progress row: converted modules, old bytes, replacement bytes.
fn conversion_stats(stdout: &str) -> Option<Value> {
    let line = stdout.lines().find(|line| {
        line.to_ascii_lowercase()
            .starts_with("progress p=000001c2 ")
    })?;
    let fields = line
        .split_whitespace()
        .skip(2)
        .take(3)
        .map(|field| u64::from_str_radix(field.split_once('=').unwrap().1, 16).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(fields.len(), 3);
    Some(
        json!({"converted_modules": fields[0], "before_bytes": fields[1], "after_bytes": fields[2]}),
    )
}

#[test]
#[ignore = "four prepared embedded bundles and configured serial FS-UAE required"]
fn native_fragment_frozen_comparison() {
    let (frozen_root, frozen, _, package) = bundle("OPFORGE_FRAGMENT_FROZEN_BUNDLE");
    let baseline = bundle("OPFORGE_FRAGMENT_BASELINE_RELEASE");
    let candidate = bundle("OPFORGE_FRAGMENT_CANDIDATE_RELEASE");
    let baseline_memory = bundle("OPFORGE_FRAGMENT_BASELINE_MEMORY");
    let candidate_memory = bundle("OPFORGE_FRAGMENT_CANDIDATE_MEMORY");
    for release in [&baseline, &candidate] {
        assert_eq!(release.1["bootstrap_defines"], json!([]));
        assert_eq!(
            release.2,
            fs::read(release.0.join("oracle.hunk")).unwrap(),
            "release bootstrap is its own exported Rust oracle"
        );
        assert_eq!(release.3, package, "identical embedded runtime package");
    }
    let memory_defines = [
        "OPFORGE_DEBUG_CONTRACTS",
        "OPFORGE_MEMORY_TELEMETRY",
        "OPFORGE_PREPARATION_PROGRESS",
        "OPFORGE_CAPTURE_TELEMETRY",
        "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
    ];
    for (release, memory) in [
        (&baseline, &baseline_memory),
        (&candidate, &candidate_memory),
    ] {
        let flags = memory.1["bootstrap_defines"].as_array().unwrap();
        assert_eq!(flags.len(), memory_defines.len());
        assert_eq!(
            flags
                .iter()
                .map(|v| v.as_str().unwrap())
                .collect::<BTreeSet<_>>(),
            memory_defines.iter().copied().collect::<BTreeSet<_>>()
        );
        assert_eq!(
            memory.1["source_manifest_digest"],
            release.1["source_manifest_digest"]
        );
        assert_eq!(fs::read(memory.0.join("oracle.hunk")).unwrap(), release.2);
        assert_eq!(memory.3, package);
        assert_ne!(memory.2, release.2, "actual instrumented image required");
    }
    let scratch = std::env::temp_dir().join(format!(
        "opforge-fragment-comparison-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&scratch).unwrap();
    let _cleanup = EphemeralArtifactDir(scratch.clone());
    let staged = scratch.join("frozen");
    let mut files = Vec::<(String, Vec<u8>)>::new();
    let mut identity = Vec::new();
    let mut names = BTreeSet::new();
    for row in frozen["source_mapping"].as_array().unwrap() {
        let name = row["staged_path"].as_str().unwrap();
        assert!(Path::new(name)
            .components()
            .all(|p| matches!(p, std::path::Component::Normal(_))));
        assert!(name.starts_with("src/") && names.insert(name.to_ascii_lowercase()));
        let bytes = fs::read(frozen_root.join(name)).unwrap();
        assert_eq!(bytes.len() as u64, row["bytes"].as_u64().unwrap());
        assert_eq!(opforge_self_host_package_digest(&bytes), row["digest"]);
        identity.extend_from_slice(row["logical_path"].as_str().unwrap().as_bytes());
        identity.push(0);
        identity.extend_from_slice(&bytes);
        identity.push(0);
        let path = staged.join(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, &bytes).unwrap();
        files.push((name.to_owned(), bytes));
    }
    assert_eq!(
        opforge_self_host_package_digest(&identity),
        frozen["source_manifest_digest"]
    );
    let package_asset = format!(
        "src/experimental/packages/{}",
        frozen["runtime_package_file"].as_str().unwrap()
    );
    assert_eq!(
        files
            .iter()
            .find(|(name, _)| name == &package_asset)
            .expect("frozen embedded package input")
            .1,
        package
    );

    assert_eq!(files.len() as u64, frozen["source_files"].as_u64().unwrap());
    let entry = frozen["entry"].as_str().unwrap();
    assert_eq!(entry, "src/experimental/opforge_compact_cli.asm");
    assert_eq!(frozen["module_roots"], json!(["src"]));
    assert_eq!(frozen["include_roots"], json!(["src/debug"]));
    let full_command = format!("-i {entry} --hunk output.hunk -M src -I src/debug");
    assert_eq!(frozen["command"], format!("opforge {full_command}"));
    let full_oracle = oracle(&staged, entry, &scratch.join("full-oracle"), false);
    assert_eq!(
        full_oracle,
        fs::read(frozen_root.join("oracle.hunk")).unwrap()
    );
    // Real file discovery, alias import, shared dependency and instruction/data
    // emission provide a bounded control without synthetic giant module counts.
    let small = small_fixture("converted");
    let small_root = scratch.join("small");
    for (name, bytes) in &small {
        let path = small_root.join(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
    }
    let small_oracle = oracle(
        &small_root,
        "src/main.asm",
        &scratch.join("small-oracle"),
        true,
    );
    assert!(!small_oracle.is_empty());
    let macro_inputs = small_fixture("macro");
    let macro_root = scratch.join("macro");
    for (name, bytes) in &macro_inputs {
        let path = macro_root.join(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
    }
    let macro_oracle = oracle(
        &macro_root,
        "src/main.asm",
        &scratch.join("macro-oracle"),
        true,
    );
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mut failures = Vec::new();
    for (workload, inputs, expected, command, output) in [
        (
            "full",
            &files,
            &full_oracle,
            full_command.as_str(),
            "Work/output.hunk",
        ),
        (
            "small",
            &small,
            &small_oracle,
            "-i src/main.asm --bin output.bin -M src",
            "Work/output.bin",
        ),
        (
            "macro",
            &macro_inputs,
            &macro_oracle,
            "-i src/main.asm --bin output.bin -M src",
            "Work/output.bin",
        ),
    ] {
        let guest_files = inputs
            .iter()
            .map(|(name, bytes)| OpforgeNativeCliGuestFile {
                relative_path: name,
                bytes,
            })
            .collect::<Vec<_>>();
        let case_identity = inputs
            .iter()
            .flat_map(|(name, bytes)| name.as_bytes().iter().chain(bytes))
            .copied()
            .collect::<Vec<_>>();
        for (role, image, instrumented, rounds) in [
            // Resource counters and full correctness first; release timings
            // follow only after the candidate's real whole-build path ran.
            ("candidate_memory", &candidate_memory.2, true, 1),
            ("baseline_memory", &baseline_memory.2, true, 1),
            ("baseline", &baseline.2, false, 3),
            ("candidate", &candidate.2, false, 3),
        ] {
            for round in 1..=rounds {
                let case = OpforgeNativeCliParityCase {
                    name: workload,
                    cpu_override: "68020",
                    extra_assembly_defines: if instrumented { &memory_defines } else { &[] },
                    source_override: Some(b""),
                    command_template: Some(command),
                    package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
                    extra_guest_files: &guest_files,
                    proof: OpforgeNativeCliProof::ExactArtifact {
                        relative_path: output,
                        rust_oracle: expected,
                    },
                };
                match run_prebuilt_compact_cli_case_from_env(&workspace, &case, image) {
                    Ok(FsUaeSmokeOutcome::Completed { runs }) => {
                        let Some(run) = runs.first() else {
                            failures.push(format!("{workload}/{role}/{round}: no completed run"));
                            continue;
                        };
                        let valid = runs.len() == 1
                            && run.success
                            && run.protocol_completed
                            && run.exit_code == Some(0)
                            && run.verified_output.as_deref() == Some(expected.as_slice());
                        if !valid {
                            failures
                                .push(format!("{workload}/{role}/{round}: incomplete exact proof"));
                        }
                        let allocation = if instrumented && valid {
                            Some(super::native_package_loading_performance::memory(
                                run.captured_artifacts
                                    .get(&PathBuf::from("Work/memory.bin"))
                                    .expect("fresh memory record"),
                            ))
                        } else {
                            None
                        };
                        let bootstrap_manifest = if role.starts_with("baseline") {
                            &baseline.1
                        } else {
                            &candidate.1
                        };
                        let self_host = workload == "full"
                            && !instrumented
                            && valid
                            && frozen["source_manifest_digest"]
                                == bootstrap_manifest["source_manifest_digest"]
                            && expected == image;
                        eprintln!(
                            "NATIVE_FRAGMENT_COMPARISON {}",
                            json!({
                                "workload": workload, "role": role, "round": round, "instrumented": instrumented,
                                "fresh_completion_exact": valid, "exit_code": run.exit_code,
                                "self_host": self_host,
                                "bootstrap_source_manifest_digest": bootstrap_manifest["source_manifest_digest"],
                                "frozen_source_manifest_digest": frozen["source_manifest_digest"],
                                "release_start_to_done_host_seconds": if instrumented { None } else { run.start_to_done_host_seconds },
                                "source_digest": opforge_self_host_package_digest(&case_identity),
                                "source_files": inputs.len(), "source_bytes": inputs.iter().map(|(_, b)| b.len()).sum::<usize>(),
                                "package_digest": opforge_self_host_package_digest(&package),
                                "oracle_digest": opforge_self_host_package_digest(expected),
                                "image_digest": opforge_self_host_package_digest(image), "image_bytes": image.len(),
                                "linked_static_reserved_bytes": super::native_package_loading_performance::linked_static_bytes(image),
                                "allocation": allocation, "conversion": conversion_stats(&run.stdout), "command": command,
                                "memory_profile": std::env::var("OPFORGE_FS_UAE_MEMORY_PROFILE").ok(),
                                "config_template": std::env::var("OPFORGE_FS_UAE_CONFIG_TEMPLATE").ok(),
                                "metric_scope": "host-observed START/DONE interval, not hardware timing; tracked heap and linked reservations exclude OS/loader/untracked storage"
                            })
                        );
                    }
                    Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                        failures.push(format!("{workload}/{role}/{round}: skipped: {reason}"))
                    }
                    Err(error) => failures.push(format!("{workload}/{role}/{round}: {error}")),
                }
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[path = "native_fragment_fixtures.rs"]
mod fixtures;
use fixtures::small_fixture;

#[test]
#[ignore = "candidate release/memory embedded exports and serial configured FS-UAE required"]
fn native_fragment_cli_smoke() {
    run_cli_smoke(&[
        ("converted", false),
        ("source_pc", false),
        ("shifted", false),
        ("conditional", false),
        ("macro", false),
        ("converted", true),
        ("macro", true),
        ("disk", true),
        ("oversized", true),
        ("oversized_resident", false),
    ]);
}

#[test]
#[ignore = "prepared release/memory exports; focused retained-origin reference check"]
fn native_fragment_cli_shifted() {
    run_cli_smoke(&[("shifted", false)]);
}

#[test]
#[ignore = "candidate release/memory exports; focused conversion and MEMD qualification"]
fn native_fragment_cli_memory() {
    run_cli_smoke(&[("converted", true)]);
}

#[test]
#[ignore = "candidate exports; macro expansion exact output and conversion/MEMD proof"]
fn native_fragment_cli_macro() {
    run_cli_smoke(&[("macro", false), ("macro", true)]);
}

#[test]
#[ignore = "candidate exports; disk capture exact output and conversion/MEMD proof"]
fn native_fragment_cli_disk() {
    run_cli_smoke(&[("disk", true), ("oversized", true)]);
}

fn run_cli_smoke(cases: &[(&str, bool)]) {
    let release = bundle("OPFORGE_FRAGMENT_CANDIDATE_RELEASE");
    let memory = bundle("OPFORGE_FRAGMENT_CANDIDATE_MEMORY");
    assert_eq!(release.1["bootstrap_defines"], json!([]));
    assert_eq!(release.3, memory.3);
    assert_eq!(
        release.1["source_manifest_digest"],
        memory.1["source_manifest_digest"]
    );
    let scratch = std::env::temp_dir().join(format!(
        "opforge-fragment-smoke-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&scratch).unwrap();
    let _cleanup = EphemeralArtifactDir(scratch.clone());
    let defines = [
        "OPFORGE_DEBUG_CONTRACTS",
        "OPFORGE_MEMORY_TELEMETRY",
        "OPFORGE_PREPARATION_PROGRESS",
        "OPFORGE_CAPTURE_TELEMETRY",
        "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
    ];
    assert_eq!(
        memory.1["bootstrap_defines"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect::<BTreeSet<_>>(),
        defines.iter().copied().collect::<BTreeSet<_>>()
    );
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mut failures = Vec::new();
    for &(scenario, instrumented) in cases {
        let mut inputs = small_fixture(if scenario == "oversized_resident" {
            "oversized"
        } else {
            scenario
        });
        if matches!(scenario, "disk" | "oversized") {
            inputs.push(("capture/keep".into(), Vec::new()));
        }
        let staged = scratch.join(format!("{scenario}-{instrumented}"));
        for (name, bytes) in &inputs {
            let path = staged.join(name);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, bytes).unwrap();
        }
        let expected = oracle(&staged, "src/main.asm", &staged.join("oracle"), true);
        let guest_files = inputs
            .iter()
            .map(|(name, bytes)| OpforgeNativeCliGuestFile {
                relative_path: name,
                bytes,
            })
            .collect::<Vec<_>>();
        let case = OpforgeNativeCliParityCase {
            name: scenario,
            cpu_override: "68020",
            source_override: Some(b""),
            extra_assembly_defines: if instrumented { &defines } else { &[] },
            command_template: Some(if matches!(scenario, "disk" | "oversized") {
                "-i src/main.asm --bin output.bin -M src --capture-dir Work:capture"
            } else {
                "-i src/main.asm --bin output.bin -M src"
            }),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &guest_files,
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.bin",
                rust_oracle: &expected,
            },
        };
        let image = if instrumented { &memory.2 } else { &release.2 };
        match run_prebuilt_compact_cli_case_from_env(&workspace, &case, image) {
            Ok(FsUaeSmokeOutcome::Completed { runs }) => {
                let Some(run) = runs.first() else {
                    failures.push(format!("{scenario}: no native result"));
                    continue;
                };
                if runs.len() != 1
                    || !run.success
                    || !run.protocol_completed
                    || run.exit_code != Some(0)
                    || run.verified_output.as_deref() != Some(expected.as_slice())
                {
                    failures.push(format!("{scenario}: incomplete exact proof"));
                    continue;
                }
                if instrumented {
                    for line in run
                        .stdout
                        .lines()
                        .filter(|line| line.contains("p=000001C2 "))
                    {
                        eprintln!("NATIVE_FRAGMENT_DETAIL {line}");
                    }
                }
                let allocation = instrumented.then(|| {
                    super::native_package_loading_performance::memory(
                        run.captured_artifacts
                            .get(&PathBuf::from("Work/memory.bin"))
                            .expect("fresh MEMD"),
                    )
                });
                let stats = if instrumented {
                    let stats =
                        conversion_stats(&run.stdout).expect("fragment conversion progress row");
                    if matches!(scenario, "converted" | "disk")
                        && !(stats["converted_modules"].as_u64().unwrap() > 0
                            && stats["before_bytes"].as_u64().unwrap()
                                > stats["after_bytes"].as_u64().unwrap())
                    {
                        failures.push(format!(
                            "{scenario}: missing expected conversion/reduction: {stats}"
                        ));
                    }
                    Some(stats)
                } else {
                    None
                };
                eprintln!(
                    "NATIVE_FRAGMENT_CLI_SMOKE {}",
                    json!({"scenario": scenario,
                    "instrumented": instrumented, "fresh_completion_exact": true,
                    "output_bytes": expected.len(), "allocation": allocation, "conversion": stats})
                );
            }
            Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                failures.push(format!("{scenario}: skipped {reason}"))
            }
            Err(error) => {
                eprintln!("NATIVE_FRAGMENT_CLI_FAILURE {scenario}: {error}");
                failures.push(format!("{scenario}: {error}"));
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

// Development probe builds the current CLI once through the maintained native
// runner. Frozen exports remain mandatory for resource comparisons/self-hosting.
#[test]
#[ignore = "configured FS-UAE with OPFORGE_COMPARE_MEMORY=1 OPFORGE_PHASE_ONLY=1 OPFORGE_PREPARATION_PROGRESS=1"]
fn native_fragment_cli_current_probe() {
    assert_eq!(std::env::var("OPFORGE_COMPARE_MEMORY").as_deref(), Ok("1"));
    assert_eq!(
        std::env::var("OPFORGE_PREPARATION_PROGRESS").as_deref(),
        Ok("1")
    );
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let core = vm::runtime_model_core::RuntimeModelCore::from_registry(
        &engine::build_default_asm_registry(),
    )
    .unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = crate::binary_source_experiment::prepare_package(&core, &resolved).unwrap();
    let root =
        std::env::temp_dir().join(format!("opforge-fragment-current-{}", std::process::id()));
    fs::create_dir_all(&root).unwrap();
    let _cleanup = EphemeralArtifactDir(root.clone());
    let inputs = small_fixture("converted");
    for (name, bytes) in &inputs {
        let path = root.join(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
    }
    let expected = oracle(&root, "src/main.asm", &root.join("oracle"), true);
    let sources = inputs
        .iter()
        .map(|(name, bytes)| (name.as_str(), bytes.as_slice()))
        .collect::<Vec<_>>();
    let FsUaeSmokeOutcome::Completed { runs } = run_compact_cli_files_from_env(
        &workspace,
        &package,
        &sources,
        &["src"],
        &[],
        Some(&expected),
        false,
    )
    .expect("current native fragment probe") else {
        panic!("fresh native completion required");
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed && run.exit_code == Some(0));
    for line in run
        .stdout
        .lines()
        .filter(|line| line.contains("p=000001C2 "))
    {
        eprintln!("NATIVE_FRAGMENT_DETAIL {line}");
    }
    let stats = run
        .stdout
        .lines()
        .find(|line| line.contains("p=000001C2 "))
        .expect("fragment counters");
    assert!(!stats.contains("f=00000000"), "actual conversion: {stats}");
    eprintln!("NATIVE_FRAGMENT_CURRENT_EXACT bytes={}", expected.len());
}
