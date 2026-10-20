//! Frozen full CLI comparison; only the supplied bootstrap changes between roles.
//! Exports are prepared separately with export_compact_self_host_bundle. This test
//! reassembles manifest-listed frozen inputs with Rust before any native launch.
//! Set OPFORGE_CLI_FROZEN_BUNDLE, OPFORGE_CLI_BASELINE_RELEASE,
//! OPFORGE_CLI_CANDIDATE_RELEASE, OPFORGE_CLI_BASELINE_MEMORY and
//! OPFORGE_CLI_CANDIDATE_MEMORY to independent embedded export directories.
//! Run native_cli_frozen_comparison with --ignored --nocapture
//! --test-threads=1 and the normal FS-UAE environment. Each workload runs three
//! releases per role plus one separately instrumented memory run per role.
//! native_cli_frozen_candidate_comparison reuses a previously measured frozen
//! reference and runs only the fresh candidate roles; input identities must match
//! that earlier receipt before comparing results.
//! OPFORGE_CLI_COMPARISON_WORKLOAD optionally selects all, full or controls.
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

// The existing final assembly-work packet carries the accepted module count.
// Read it only for policy comparisons; older images assign that word differently.
pub(super) fn private_module_count(stdout: &str) -> Option<u32> {
    stdout.lines().find_map(|line| {
        let fields = line
            .strip_prefix("progress ")?
            .split_whitespace()
            .filter_map(|field| field.split_once('='))
            .collect::<BTreeMap<_, _>>();
        (u32::from_str_radix(fields.get("p")?, 16).ok()? == 300)
            .then(|| u32::from_str_radix(fields.get("r")?, 16).ok())?
    })
}

pub(super) fn module_store_io(stdout: &str) -> Option<[u32; 3]> {
    stdout.lines().find_map(|line| {
        let fields = line
            .strip_prefix("progress ")?
            .split_whitespace()
            .filter_map(|field| field.split_once('='))
            .collect::<BTreeMap<_, _>>();
        if u32::from_str_radix(fields.get("p")?, 16).ok()? != 451 {
            return None;
        }
        Some([
            u32::from_str_radix(fields.get("f")?, 16).ok()?,
            u32::from_str_radix(fields.get("l")?, 16).ok()?,
            u32::from_str_radix(fields.get("r")?, 16).ok()?,
        ])
    })
}

#[test]
#[ignore = "four prepared embedded bundles and configured serial FS-UAE required"]
fn native_cli_frozen_comparison() {
    compare(false, false, Measurement::Both);
}

#[test]
#[ignore = "fresh candidate against an already measured identical frozen input; serial FS-UAE required"]
fn native_cli_frozen_candidate_comparison() {
    compare(true, false, Measurement::Both);
}

// Compare execution policies in the same executable, package and source tree.
// The reference flag preserves the old schedule as well as module execution.
#[test]
#[ignore = "prepared embedded bundles and configured serial FS-UAE required"]
fn native_cli_frozen_module_policy_memory() {
    compare(false, true, Measurement::Memory);
}

#[test]
#[ignore = "qualified module-policy memory comparison and serial FS-UAE required"]
fn native_cli_frozen_module_policy_release() {
    compare(false, true, Measurement::Release);
}

#[derive(PartialEq)]
enum Measurement {
    Both,
    Memory,
    Release,
}

fn compare(candidate_only: bool, module_policy_comparison: bool, measurement: Measurement) {
    let workloads =
        std::env::var("OPFORGE_CLI_COMPARISON_WORKLOAD").unwrap_or_else(|_| "all".into());
    assert!(matches!(workloads.as_str(), "all" | "full" | "controls"));
    let disk = match std::env::var("OPFORGE_CLI_COMPARISON_CAPTURE") {
        Err(std::env::VarError::NotPresent) => false,
        Ok(value) if value == "resident" => false,
        Ok(value) if value == "disk" => true,
        value => panic!("invalid comparison capture policy: {value:?}"),
    };
    let (frozen_root, frozen, _, package) = bundle("OPFORGE_CLI_FROZEN_BUNDLE");
    let baseline = bundle("OPFORGE_CLI_BASELINE_RELEASE");
    let candidate = bundle("OPFORGE_CLI_CANDIDATE_RELEASE");
    let baseline_memory = bundle("OPFORGE_CLI_BASELINE_MEMORY");
    let candidate_memory = bundle("OPFORGE_CLI_CANDIDATE_MEMORY");
    if module_policy_comparison {
        assert_eq!(baseline.2, candidate.2, "identical release executable");
        assert_eq!(
            baseline_memory.2, candidate_memory.2,
            "identical memory executable"
        );
        assert_eq!(
            baseline.1["source_manifest_digest"],
            candidate.1["source_manifest_digest"]
        );
    }
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
        "opforge-cli-comparison-{}-{}",
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
    let small = small_fixture("small");
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
        if (workloads == "full" && workload != "full")
            || (workloads == "controls" && workload == "full")
        {
            continue;
        }
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
            if candidate_only && role.starts_with("baseline") {
                continue;
            }
            if (measurement == Measurement::Memory && !instrumented)
                || (measurement == Measurement::Release && instrumented)
            {
                continue;
            }
            let reference_modules = module_policy_comparison && role.starts_with("baseline");
            let command = format!(
                "{command}{}{}",
                if disk { " --capture-dir Work:" } else { "" },
                if reference_modules {
                    " --reference-modules"
                } else {
                    ""
                }
            );
            for round in 1..=rounds {
                let case = OpforgeNativeCliParityCase {
                    name: workload,
                    cpu_override: "68020",
                    extra_assembly_defines: if instrumented { &memory_defines } else { &[] },
                    source_override: Some(b""),
                    command_template: Some(&command),
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
                        // A bad resource receipt must not suppress later fresh cases.
                        let allocation = if instrumented && valid {
                            let decoded = run
                                .captured_artifacts
                                .get(&PathBuf::from("Work/memory.bin"))
                                .and_then(|bytes| {
                                    std::panic::catch_unwind(|| {
                                        super::native_package_loading_performance::memory(bytes)
                                    })
                                    .ok()
                                });
                            if decoded.is_none() {
                                failures.push(format!("{workload}/{role}/{round}: missing or invalid MEMD/cleanup receipt"));
                            }
                            decoded
                        } else {
                            None
                        };
                        let private_modules = (module_policy_comparison && instrumented)
                            .then(|| private_module_count(&run.stdout))
                            .flatten();
                        let store_io = (disk && instrumented)
                            .then(|| module_store_io(&run.stdout))
                            .flatten();
                        if disk
                            && module_policy_comparison
                            && instrumented
                            && valid
                            && !matches!((store_io, private_modules), (Some(io), Some(count)) if io[1] == count && io[0] >= count)
                        {
                            failures.push(format!("{workload}/{role}: missing or inconsistent persisted-module receipt {store_io:?}"));
                        }
                        if module_policy_comparison
                            && instrumented
                            && valid
                            && (private_modules.is_none()
                                || (reference_modules && private_modules != Some(0))
                                || (!reference_modules
                                    && workload == "full"
                                    && private_modules == Some(0)))
                        {
                            failures.push(format!(
                                "{workload}/{role}: invalid private publication receipt {private_modules:?}"
                            ));
                        }
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
                            "NATIVE_CLI_COMPARISON {}",
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
                                "allocation": allocation, "command": command,
                                "module_policy_comparison": module_policy_comparison,
                                "capture_mode": if disk { "disk" } else { "resident" },
                                "reference_modules": reference_modules,
                                "privately_compiled_modules": private_modules,
                                "module_store_reads_writes_hits": store_io,
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

#[path = "native_cli_comparison_fixtures.rs"]
mod fixtures;
use fixtures::small_fixture;
