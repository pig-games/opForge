//! Portable export for a physical Amiga self-host run, with optional FS-UAE proof.
//! OPFORGE_COMPACT_EXPORT_DIR=/absolute/new/bundle cargo test -p asm
//! export_compact_self_host_bundle -- --ignored --nocapture --test-threads=1
//! The default loads the named 68020 package from disk. Embedding is opt-in. Set
//! OPFORGE_COMPACT_EXPORT_NATIVE=1 for fresh full native qualification.
use super::*;
use crate::fs_uae_smoke::{
    run_prebuilt_compact_cli_case_from_env, OpforgeNativeCliExpectedArtifact,
    OpforgeNativeCliGuestFile, OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase,
    OpforgeNativeCliProof,
};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use registry::registry::ModuleRegistry;
use vm::runtime_model_core::RuntimeModelCore;

const ENTRY: &str = "main.asm";

// Output embedding also supplies the bootstrap package. A bootstrap-only embed
// remains an explicit reference configuration; neither option is a default.
fn export_package_selection(
    bootstrap: Option<&str>,
    output: Option<&str>,
) -> Result<(bool, bool), &'static str> {
    let selected = |value| match value {
        None => Ok(false),
        Some("68020") => Ok(true),
        Some(_) => Err("unsupported embedded self-host target"),
    };
    let output = selected(output)?;
    Ok((selected(bootstrap)? || output, output))
}

#[test]
fn self_host_packages_default_to_disk_with_explicit_embedding() {
    assert_eq!(export_package_selection(None, None), Ok((false, false)));
    assert_eq!(
        export_package_selection(Some("68020"), None),
        Ok((true, false))
    );
    assert_eq!(
        export_package_selection(None, Some("68020")),
        Ok((true, true))
    );
    assert!(export_package_selection(Some("other"), None).is_err());
    assert!(export_package_selection(None, Some("other")).is_err());
}
const INSTRUMENTED_DEFINES: &[&str] = &[
    "OPFORGE_DEBUG_CONTRACTS",
    "OPFORGE_MEMORY_TELEMETRY",
    "OPFORGE_TOKEN_DETAIL_TELEMETRY",
    "OPFORGE_PREPARATION_PROGRESS",
    "OPFORGE_BINDING_DETAIL_TELEMETRY",
    "OPFORGE_TEMPLATE_WORK_TELEMETRY",
    "OPFORGE_INPUT_TELEMETRY",
    "OPFORGE_CAPTURE_TELEMETRY",
    "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
];

fn bounded_text(value: &str, limit: usize) -> String {
    let mut chars = value.chars();
    let text: String = chars.by_ref().take(limit).collect();
    if chars.next().is_some() {
        format!("{text}…")
    } else {
        text
    }
}

fn concise_assembly_failure(context: &str, error: cli_core::CliRunError) -> String {
    match error {
        cli_core::CliRunError::Assembler { error, .. } => {
            let diagnostics = error.diagnostics();
            let details = diagnostics
                .iter()
                .take(8)
                .map(|diagnostic| {
                    format!(
                        "{}:{}: {}",
                        diagnostic.file.as_deref().unwrap_or("<unknown>"),
                        diagnostic.line,
                        bounded_text(diagnostic.error.message(), 240)
                    )
                })
                .collect::<Vec<_>>();
            format!(
                "{context}: {}; diagnostics: {} total, showing {}: {}",
                bounded_text(error.summary(), 500),
                diagnostics.len(),
                details.len(),
                details.join(" | ")
            )
        }
        cli_core::CliRunError::Workflow { error, .. } => {
            format!(
                "{context}: workflow error: {}",
                bounded_text(&error.to_string(), 500)
            )
        }
        cli_core::CliRunError::WarningsAsErrors { reports } => format!(
            "{context}: warnings treated as errors ({} reports)",
            reports.len()
        ),
    }
}

#[test]
#[ignore = "host-only export; requires a new OPFORGE_COMPACT_EXPORT_DIR"]
fn export_compact_self_host_bundle() {
    export_bundle(false);
}

#[test]
#[ignore = "diagnostic failure capture only; requires instrumented full native export"]
fn capture_compact_self_host_failure() {
    assert_eq!(
        std::env::var("OPFORGE_COMPACT_EXPORT_NATIVE").as_deref(),
        Ok("1")
    );
    assert_eq!(
        std::env::var("OPFORGE_COMPACT_EXPORT_INSTRUMENTED").as_deref(),
        Ok("1")
    );
    export_bundle(true);
}

fn export_bundle(failure_capture: bool) {
    let output = PathBuf::from(
        std::env::var_os("OPFORGE_COMPACT_EXPORT_DIR")
            .expect("set OPFORGE_COMPACT_EXPORT_DIR to a new local directory"),
    );
    assert!(output.is_absolute(), "export directory must be absolute");
    assert!(!output.exists(), "export directory must not already exist");
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    let source_root = fs::canonicalize(
        std::env::var_os("OPFORGE_SELF_HOST_SOURCE_ROOT")
            .map(PathBuf::from)
            .unwrap_or_else(|| workspace.join("native/motorola68000/amigaos")),
    )
    .expect("canonical source root");
    let scratch = create_artifact_dir(&workspace, "compact-self-host-export").unwrap();
    let cleanup = EphemeralArtifactDir(scratch.clone());
    let dependencies = scratch.join("dependencies.d");
    let instrumented = std::env::var("OPFORGE_COMPACT_EXPORT_INSTRUMENTED").as_deref() == Ok("1");
    let selection = |name| match std::env::var(name) {
        Err(std::env::VarError::NotPresent) => None,
        Ok(value) => Some(value),
        Err(error) => panic!("invalid {name}: {error}"),
    };
    let (embedded, output_embedded) = export_package_selection(
        selection("OPFORGE_COMPACT_EXPORT_EMBED").as_deref(),
        selection("OPFORGE_COMPACT_EXPORT_OUTPUT_EMBED").as_deref(),
    )
    .expect("self-host embed selections currently accept only 68020");
    let package_build = embedded.then(|| {
        use crate::native_package_build::{build_native_packages, EmbedSelection};
        let registry = engine::build_default_asm_registry();
        build_native_packages(
            &registry,
            &scratch.join("embedded"),
            &source_root.join(ENTRY),
            &EmbedSelection::Targets(vec!["68020".into()]),
        )
        .expect("generate portable single-CPU embedded catalog")
    });
    let mut args = vec![
        "opForge".to_string(),
        source_root.to_string_lossy().into_owned(),
        "--dependencies".to_string(),
        dependencies.to_string_lossy().into_owned(),
    ];
    if output_embedded {
        args[1] = package_build
            .as_ref()
            .unwrap()
            .cli_source_path
            .to_string_lossy()
            .into_owned();
        // The configured entry lives outside the native source tree.
        args.extend([
            "-M".to_string(),
            source_root.to_string_lossy().into_owned(),
            "-I".to_string(),
            source_root.to_string_lossy().into_owned(),
        ]);
    }
    let cli = Cli::parse_from(args.clone());
    let mut config = validate_cli(&cli).expect("validate live release self-build");
    assert!(
        config.defines.is_empty(),
        "release oracle requires no defines"
    );
    config.out_dir = Some(scratch.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| {
        panic!(
            "{}",
            concise_assembly_failure("fresh release self-build", error)
        )
    });
    let oracle = fs::read(scratch.join("build/opforge")).unwrap();
    let allocation = hunk::allocation(&oracle).expect("strict release Hunk allocation");
    let dependency_text = fs::read_to_string(&dependencies).unwrap();
    let phase_only = std::env::var("OPFORGE_PHASE_ONLY").as_deref() == Ok("1");
    // Retain the established completion-failure snapshot without the dense work
    // counters. This diagnostic bootstrap still assembles the release inputs.
    let binding_failure_only = std::env::var("OPFORGE_BINDING_FAILURE_ONLY").as_deref() == Ok("1");
    // Focused owner/copy accounting avoids the hot nested timing probes.
    let capture_only = std::env::var("OPFORGE_CAPTURE_ONLY").as_deref() == Ok("1");
    let hotspot_only = std::env::var("OPFORGE_HOTSPOT_ONLY").as_deref() == Ok("1");
    assert!(
        !binding_failure_only || instrumented,
        "binding failure observation requires an instrumented bootstrap"
    );
    assert!(
        !capture_only || (instrumented && !binding_failure_only),
        "capture accounting requires an instrumented bootstrap and its own profile"
    );
    assert!(
        !hotspot_only || (instrumented && !phase_only && !capture_only && !binding_failure_only),
        "sampled hotspots require their own instrumented profile"
    );
    // Explicit measurement configuration reuses existing instrumentation gates.
    // In particular, heap accounting does not require dense progress/failure
    // snapshots. Do not combine a define list with a preset profile.
    let configured_defines = std::env::var("OPFORGE_COMPACT_EXPORT_DEFINES").ok();
    let bootstrap_defines: Vec<String> = if let Some(defines) = configured_defines {
        assert!(
            instrumented && !hotspot_only && !capture_only && !phase_only && !binding_failure_only,
            "explicit defines require an instrumented export without a preset profile"
        );
        let defines: Vec<String> = defines.split_whitespace().map(str::to_owned).collect();
        assert!(
            defines
                .iter()
                .any(|value| value == "OPFORGE_DEBUG_CONTRACTS")
                && defines
                    .iter()
                    .any(|value| value == "OPFORGE_MEMORY_TELEMETRY"),
            "instrumented exports require both allocation-accounting gates"
        );
        defines
    } else if hotspot_only {
        let period = std::env::var("OPFORGE_HOTSPOT_PERIOD").unwrap_or_else(|_| "128".into());
        assert!(matches!(period.as_str(), "64" | "128" | "256"));
        let seed = std::env::var("OPFORGE_HOTSPOT_SEED").unwrap_or_else(|_| "0".into());
        let seed: u8 = seed.parse().expect("hotspot phase seed must be 0..255");
        let mut defines = [
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_MEMORY_TELEMETRY",
            "OPFORGE_PREPARATION_PROGRESS",
            "OPFORGE_CAPTURE_TELEMETRY",
            "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
            "OPFORGE_HOTSPOT_TELEMETRY",
        ]
        .map(str::to_owned)
        .to_vec();
        defines.push(format!("OPFORGE_HOTSPOT_SAMPLE_PERIOD={period}"));
        defines.push(format!("OPFORGE_HOTSPOT_PHASE_SEED={seed}"));
        defines
    } else if instrumented {
        INSTRUMENTED_DEFINES
            .iter()
            .copied()
            .filter(|define| !phase_only || *define != "OPFORGE_TOKEN_DETAIL_TELEMETRY")
            .filter(|define| {
                !capture_only
                    || matches!(
                        *define,
                        "OPFORGE_DEBUG_CONTRACTS"
                            | "OPFORGE_MEMORY_TELEMETRY"
                            | "OPFORGE_PREPARATION_PROGRESS"
                            | "OPFORGE_CAPTURE_TELEMETRY"
                            | "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT"
                    )
            })
            .filter(|define| {
                !binding_failure_only
                    || matches!(
                        *define,
                        "OPFORGE_DEBUG_CONTRACTS"
                            | "OPFORGE_MEMORY_TELEMETRY"
                            | "OPFORGE_PREPARATION_PROGRESS"
                            | "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT"
                    )
            })
            .map(str::to_owned)
            .collect()
    } else {
        Vec::new()
    };
    let embedded_files = if let Some(build) = &package_build {
        assert_eq!(build.embedded_files.len(), 1);
        args[1] = build.cli_source_path.to_string_lossy().into_owned();
        if !output_embedded {
            // Only the bootstrap entry is generated outside the source tree.
            args.extend([
                "-M".into(),
                source_root.to_string_lossy().into_owned(),
                "-I".into(),
                source_root.to_string_lossy().into_owned(),
            ]);
        }
        build.embedded_files.iter().cloned().collect::<Vec<_>>()
    } else {
        Vec::new()
    };
    let bootstrap = if instrumented || (embedded && !output_embedded) {
        let bootstrap_cli = Cli::parse_from(args);
        config = validate_cli(&bootstrap_cli).expect("validate configured bootstrap");
        config.out_dir = Some(scratch.clone());
        config.defines = bootstrap_defines.clone();
        run_with_validated_cli_with_context(&bootstrap_cli, &config).unwrap_or_else(|error| {
            panic!(
                "{}",
                concise_assembly_failure("fresh configured bootstrap build", error)
            )
        });
        fs::read(scratch.join("build/opforge")).unwrap()
    } else {
        oracle.clone()
    };
    let bootstrap_allocation =
        hunk::allocation(&bootstrap).expect("strict bootstrap Hunk allocation");
    let (_, prerequisites) = dependency_text
        .split_once(": ")
        .expect("Makefile dependency prerequisites");
    let mut sources =
        prerequisites
            .split_whitespace()
            .map(|name| {
                let path = PathBuf::from(name);
                let (logical, origin) =
                    if let Some(build) = package_build.as_ref().filter(|_| output_embedded) {
                        if path == build.cli_source_path {
                            (PathBuf::from(ENTRY), "configured_entry")
                        } else if path == build.catalog_path {
                            (PathBuf::from("catalog.i"), "generated_catalog")
                        } else if path.starts_with(build.output_dir.join("packages")) {
                            (
                                path.strip_prefix(&build.output_dir).unwrap().to_path_buf(),
                                "package_asset",
                            )
                        } else {
                            (
                                path.strip_prefix(&source_root)
                                    .expect("native source dependency")
                                    .to_path_buf(),
                                "native",
                            )
                        }
                    } else {
                        (
                            path.strip_prefix(&source_root)
                                .expect("native source dependency")
                                .to_path_buf(),
                            "native",
                        )
                    };
                assert!(logical
                    .components()
                    .all(|part| { matches!(part, std::path::Component::Normal(_)) }));
                assert!(logical.to_str().unwrap().bytes().all(|byte| {
                    byte > 32 && byte < 127 && !matches!(byte, b':' | b'\\' | b'"')
                }));
                // Preserve include literals and the exact successful compact input tree.
                let staged = logical.clone();
                (logical, staged, fs::read(path).unwrap(), origin)
            })
            .collect::<Vec<_>>();
    sources.sort_by(|left, right| left.0.cmp(&right.0));
    assert!(sources
        .iter()
        .any(|(logical, _, _, _)| logical == Path::new(ENTRY)));
    let mut staged_names = BTreeSet::new();
    let mut manifest_bytes = Vec::new();
    for (logical, staged, bytes, _) in &sources {
        assert!(staged_names.insert(staged.to_string_lossy().to_ascii_lowercase()));
        manifest_bytes.extend_from_slice(logical.to_str().unwrap().as_bytes());
        manifest_bytes.push(0);
        manifest_bytes.extend_from_slice(bytes);
        manifest_bytes.push(0);
    }
    // Match the registry used by the compact self-host parity test.
    let mut registry = ModuleRegistry::new();
    families::register_intel8080_family_stack(&mut registry);
    families::register_mos6502_family_stack(&mut registry);
    families::register_motorola6800_family_stack(&mut registry);
    families::register_motorola68000_family_stack(&mut registry);
    let core = RuntimeModelCore::from_registry(&registry).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = crate::native_runtime_package::prepare_package(&core, &resolved).unwrap();
    if embedded {
        assert_eq!(
            fs::read(scratch.join("embedded/packages").join(&embedded_files[0])).unwrap(),
            package,
            "embedded payload must match the self-host package"
        );
        assert_eq!(
            bootstrap
                .windows(package.len())
                .filter(|w| *w == package)
                .count(),
            1
        );
    }
    let module_roots = ["src"];
    let entry = Path::new(ENTRY);
    let package_file = embedded_files
        .first()
        .map(String::as_str)
        .unwrap_or("packages/m68020--motorola68k.bin");
    let native_output_file = "build/opforge";
    let command = "opforge src";
    assert!(
        command.len() <= 255,
        "classic Shell command exceeds 255 bytes"
    );
    let files = sources
        .iter()
        .map(|(logical, staged, bytes, origin)| {
            json!({
                "logical_path": logical,
                "staged_path": Path::new("src").join(staged),
                "bytes": bytes.len(),
                "digest": opforge_self_host_package_digest(bytes),
                "origin": origin,
            })
        })
        .collect::<Vec<_>>();
    let long_components = sources
        .iter()
        .flat_map(|(_, staged, _, _)| staged.components())
        .map(|part| part.as_os_str().to_str().unwrap())
        .filter(|name| name.len() > AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX)
        .collect::<BTreeSet<_>>();
    assert!(
        long_components.is_empty(),
        "physical Amiga export requires filenames of at most {} bytes: {:?}",
        AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX,
        long_components
    );
    let mut manifest = json!({
        "manifest_version": 1,
        "kind": "compact-self-host-local-export",
        "source_root": source_root,
        "source_manifest_digest": opforge_self_host_package_digest(&manifest_bytes),
        "source_files": sources.len(),
        "source_bytes": sources.iter().map(|(_, _, bytes, _)| bytes.len()).sum::<usize>(),
        "source_mapping": files,
        "filename_mapping": if output_embedded {
            "identity; generated inputs have explicit origins"
        } else {
            "identity; source include literals are unchanged"
        },
        "classic_filename_component_limit": AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX,
        "classic_filename_compatible": long_components.is_empty(),
        "over_classic_limit_components": long_components,
        "release_defines": [],
        "release_hunk_bytes": oracle.len(),
        "release_hunk_digest": opforge_self_host_package_digest(&oracle),
        "release_hunk_allocation_bytes": allocation.total(),
        "bootstrap_defines": bootstrap_defines,
        "bootstrap_package_storage": if embedded { "embedded" } else { "external" },
        "embedded_packages": embedded_files,
        "output_package_storage": if output_embedded { "embedded" } else { "external" },
        "output_embedded_packages": if output_embedded { embedded_files.clone() } else { Vec::new() },
        "bootstrap_hunk_bytes": bootstrap.len(),
        "bootstrap_hunk_digest": opforge_self_host_package_digest(&bootstrap),
        "bootstrap_hunk_allocation_bytes": bootstrap_allocation.total(),
        "telemetry_file": instrumented.then_some("memory.bin"),
        "runtime_package_magic": "BS38",
        "runtime_package_file": package_file,
        "runtime_package_bytes": package.len(),
        "runtime_package_digest": opforge_self_host_package_digest(&package),
        "entry": format!("src/{}", entry.display()),
        "module_roots": module_roots,
        "include_roots": ["src"],
        "command": command,
        "default_capture_directory": "build/scratch",
        "native_validation": "not_run",
        "native_output_file": native_output_file,
        "capture_mode": "disk",
    });
    fs::create_dir(&output).expect("create new export directory");
    for (_, staged, bytes, _) in &sources {
        let path = output.join("src").join(staged);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
    }
    // Reassemble the relocated inputs, rather than trusting their manifest or
    // assuming the generator's host layout is equivalent to the guest layout.
    let relocated_args = vec![
        "opForge".to_string(),
        output.join("src").to_string_lossy().into_owned(),
    ];
    let relocated_cli = Cli::parse_from(relocated_args);
    let mut relocated_config = validate_cli(&relocated_cli).unwrap();
    relocated_config.out_dir = Some(scratch.join("relocated"));
    run_with_validated_cli_with_context(&relocated_cli, &relocated_config)
        .expect("fresh release assembly of relocated bundle inputs");
    assert_eq!(
        fs::read(scratch.join("relocated/build/opforge")).unwrap(),
        oracle,
        "portable bundle must produce the same complete Hunk"
    );
    fs::write(output.join("opforge"), &bootstrap).unwrap();
    fs::write(output.join("oracle.hunk"), &oracle).unwrap();
    let package_path = output.join(package_file);
    fs::create_dir_all(package_path.parent().unwrap()).unwrap();
    fs::write(package_path, &package).unwrap();
    if embedded && !output_embedded {
        // The reference bootstrap embeds its target, while its newly built
        // executable uses normal on-disk package discovery after deployment.
        let external = output.join("packages/m68020--motorola68k.bin");
        fs::create_dir_all(external.parent().unwrap()).unwrap();
        fs::write(external, &package).unwrap();
    }
    fs::write(output.join("command.txt"), format!("{command}\n")).unwrap();
    fs::write(
        output.join("manifest.json"),
        serde_json::to_vec_pretty(&manifest).unwrap(),
    )
    .unwrap();
    fs::write(
        output.join("README.txt"),
        "Local self-host export. Consult manifest.json for validation status.\nRun command.txt from this directory on AmigaOS. Module/include roots default to src.\nObjects persist in build/ofm; temporary capture storage defaults to build/scratch.\nSet an executable protection\nbit on opforge after transfer if needed. Time the whole command externally.\nRequire exit 0 and compare the manifest native_output_file exactly with the release oracle.hunk.\nThe manifest records bootstrap instrumentation and both package-storage choices.\nThe default bootstrap loads packages/m68020--motorola68k.bin through the\nsource .cpu 68020 selection and its catalog. Keep packages beside opforge.\nAn embedded bootstrap uses the same source selection; its named verification\npackage is retained locally and is not a runtime fallback. Embedded-\noutput cases also stage that package under src/packages as a binary\nassembly input. An instrumented bootstrap writes memory.bin in its current directory.\nNative source bytes are unchanged except the explicitly recorded configured entry\ninclude. Generated origins are recorded in manifest.json. Catalog paths are relative; filenames are preserved.\nFNV digests identify inputs; exact artifact bytes remain the parity authority.\n",
    )
    .unwrap();
    if std::env::var("OPFORGE_COMPACT_EXPORT_NATIVE").as_deref() == Ok("1") {
        let mut guest_files = sources
            .iter()
            .map(|(_, staged, bytes, _)| (format!("src/{}", staged.display()), bytes))
            .collect::<Vec<_>>();
        if !output_embedded {
            guest_files.push(("packages/m68020--motorola68k.bin".into(), &package));
        }
        let files = guest_files
            .iter()
            .map(|(path, bytes)| OpforgeNativeCliGuestFile {
                relative_path: path,
                bytes,
            })
            .collect::<Vec<_>>();
        let guest_output = format!("Work/{native_output_file}");
        let artifacts = [OpforgeNativeCliExpectedArtifact {
            relative_path: &guest_output,
            rust_oracle: &oracle,
        }];
        let bootstrap_define_refs = bootstrap_defines
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>();
        // The runner stages its launcher at the product output path. Remove it
        // after copying so only a freshly written assembly can satisfy proof.
        let assembly_command = command.strip_prefix("opforge ").unwrap();
        let native_command = format!(
            "--version\nCopy Work:build/opforge Work:selfhost_bootstrap\n\
             Delete Work:build/opforge\nWork:selfhost_bootstrap {assembly_command}"
        );
        let case = OpforgeNativeCliParityCase {
            name: "compact-configured-self-host",
            cpu_override: "68020",
            extra_assembly_defines: &bootstrap_define_refs,
            source_override: Some(b""),
            command_template: Some(&native_command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof: if failure_capture {
                OpforgeNativeCliProof::ExpectedFailureWithDiagnostic
            } else {
                OpforgeNativeCliProof::ExactArtifacts(&artifacts)
            },
        };
        eprintln!(
            "COMPACT_CONFIGURED_SELF_HOST_INPUT {}",
            json!({
                "command": command,
                "source_files": manifest["source_files"],
                "source_bytes": manifest["source_bytes"],
                "source_manifest_digest": manifest["source_manifest_digest"],
                "runtime_package_digest": manifest["runtime_package_digest"],
                "bootstrap_hunk_digest": manifest["bootstrap_hunk_digest"],
                "release_hunk_digest": manifest["release_hunk_digest"],
                "output_package_storage": manifest["output_package_storage"],
            })
        );
        let result = run_prebuilt_compact_cli_case_from_env(&workspace, &case, &bootstrap)
            .expect("fresh configured native self-host execution");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real native execution is required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].protocol_completed);
        assert_eq!(runs[0].success, !failure_capture);
        assert_eq!(
            runs[0].exit_code,
            Some(if failure_capture { 20 } else { 0 })
        );
        if failure_capture || instrumented {
            fs::write(output.join("native-stdout.txt"), &runs[0].stdout).unwrap();
            fs::write(output.join("native-stderr.txt"), &runs[0].stderr).unwrap();
        }
        if instrumented {
            let memory = runs[0]
                .captured_artifacts
                .get(&PathBuf::from("Work/memory.bin"))
                .expect("fresh instrumented self-host memory record");
            let words = memory
                .chunks_exact(4)
                .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
                .collect::<Vec<_>>();
            assert_eq!(memory.len(), 2280, "current MEMD layout");
            assert_eq!(words[0], 0x4d454d44, "MEMD magic");
            assert_eq!(words[1], 0, "no live owned storage after cleanup");
            assert_eq!(words[3], words[4], "all allocated capacity is freed");
            if !failure_capture {
                assert_eq!(words[29], 0, "no profiling errors");
            }
            fs::write(output.join("memory.bin"), memory).unwrap();
            manifest["native_peak_owned_bytes"] = json!(words[2]);
        }
        manifest["native_validation"] = json!(if failure_capture {
            "fresh_fs_uae_failure_capture"
        } else {
            "fresh_fs_uae_exact_hunk"
        });
        manifest["native_assembler_exit_code"] = json!(runs[0].exit_code);
        manifest["native_protocol_completed"] = json!(runs[0].protocol_completed);
        manifest["native_start_to_done_host_seconds"] = json!(runs[0].start_to_done_host_seconds);
        fs::write(
            output.join("manifest.json"),
            serde_json::to_vec_pretty(&manifest).unwrap(),
        )
        .unwrap();
        eprintln!(
            "COMPACT_CONFIGURED_SELF_HOST_RESULT {}",
            json!({
                "output_package_storage": manifest["output_package_storage"],
                "source_files": manifest["source_files"],
                "source_bytes": manifest["source_bytes"],
                "source_manifest_digest": manifest["source_manifest_digest"],
                "release_hunk_bytes": oracle.len(),
                "bootstrap_hunk_bytes": bootstrap.len(),
                "linked_reserved_bytes": allocation.total(),
                "native_start_to_done_host_seconds": runs[0].start_to_done_host_seconds,
                "exact_rust_match": !failure_capture,
                "failure_capture_only": failure_capture,
            })
        );
    }
    drop(cleanup);
    assert!(!scratch.exists(), "host build scratch was removed");
    eprintln!(
        "COMPACT_SELF_HOST_EXPORT {}",
        json!({
            "directory": output,
            "source_files": manifest["source_files"],
            "source_bytes": manifest["source_bytes"],
            "source_manifest_digest": manifest["source_manifest_digest"],
            "release_hunk_bytes": oracle.len(),
            "bootstrap_hunk_bytes": bootstrap.len(),
            "bootstrap_defines": bootstrap_defines,
            "bootstrap_package_storage": manifest["bootstrap_package_storage"],
            "embedded_packages": manifest["embedded_packages"],
            "output_package_storage": manifest["output_package_storage"],
            "native_validation": manifest["native_validation"],
            "over_classic_limit_components": manifest["over_classic_limit_components"],
        })
    );
}
