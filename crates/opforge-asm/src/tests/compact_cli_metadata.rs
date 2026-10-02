// SPDX-License-Identifier: GPL-3.0-or-later
//! Inline literal metadata checkpoint, not metadata-block or CPU-specific parity.
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

struct MetadataCase {
    name: &'static str,
    metadata: &'static str,
    flags: &'static str,
    outputs: &'static [&'static str],
    input_base: bool,
}

fn cases() -> Vec<MetadataCase> {
    vec![
        MetadataCase { name: "metadata-nested-name-only", metadata: ".meta.output.name \"@ROOT@nested/base\"", flags: "", outputs: &[], input_base: false },
        MetadataCase { name: "metadata-source-hex-relative-parent", metadata: ".meta.output.name \"@ROOT@nested/base\"\n.meta.output.hex \"records\"", flags: "", outputs: &["nested/records.hex"], input_base: false },
        MetadataCase { name: "metadata-source-hex-relative-components", metadata: ".meta.output.name \"@ROOT@nested/base\"\n.meta.output.hex \"./child/../records\"", flags: "", outputs: &["nested/records.hex"], input_base: false },
        MetadataCase { name: "metadata-cli-hex-relative-parent", metadata: ".meta.output.name \"@ROOT@nested/base\"", flags: "--hex records", outputs: &["nested/records.hex"], input_base: false },
        MetadataCase { name: "metadata-source-hex-hidden-basename", metadata: ".meta.output.name \"@ROOT@base\"\n.meta.output.hex \".records\"", flags: "", outputs: &[".records.hex"], input_base: false },
        MetadataCase { name: "metadata-name-only", metadata: ".meta.output.name \"@ROOT@base\"", flags: "", outputs: &[], input_base: false },
        MetadataCase { name: "metadata-description-only", metadata: ".meta.name \"Description\"\n.meta.version \"1.2\"", flags: "", outputs: &[], input_base: false },
        MetadataCase { name: "metadata-source-hex-input-base", metadata: ".meta.output.hex", flags: "", outputs: &["entry.hex"], input_base: true },
        MetadataCase { name: "metadata-source-hex-empty-input-base", metadata: ".meta.output.hex \"\"", flags: "", outputs: &["entry.hex"], input_base: true },
        MetadataCase { name: "metadata-source-hex-name", metadata: ".meta.output.hex \"@ROOT@source\"", flags: "", outputs: &["source.hex"], input_base: false },
        MetadataCase { name: "metadata-source-hex-omitted", metadata: ".meta.output.name \"@ROOT@base\"\n.meta.output.hex", flags: "", outputs: &["base.hex"], input_base: false },
        MetadataCase { name: "metadata-source-hex-empty", metadata: ".meta.output.name \"@ROOT@base\"\n.meta.output.hex \"\"", flags: "", outputs: &["base.hex"], input_base: false },
        MetadataCase { name: "metadata-source-hex-cli-bin", metadata: ".meta.output.hex \"@ROOT@source\"", flags: "--bin @ROOT@explicit.bin", outputs: &["explicit.bin", "source.hex"], input_base: false },
        MetadataCase { name: "metadata-source-hex-go", metadata: ".meta.output.hex \"@ROOT@source\"", flags: "--go 1234", outputs: &["source.hex"], input_base: false },
        MetadataCase { name: "metadata-source-hex-cli-srec", metadata: ".meta.output.hex \"@ROOT@source\"", flags: "--srec @ROOT@explicit.srec", outputs: &["explicit.srec", "source.hex"], input_base: false },
        MetadataCase { name: "metadata-cli-hex-overrides-source", metadata: ".meta.output.hex \"@ROOT@source\"", flags: "--hex @ROOT@explicit.hex", outputs: &["explicit.hex"], input_base: false },
        MetadataCase { name: "metadata-cli-hex-base", metadata: ".meta.output.name \"@ROOT@base\"", flags: "--hex", outputs: &["base.hex"], input_base: false },
        MetadataCase { name: "metadata-cli-bin-base", metadata: ".meta.output.name \"@ROOT@base\"", flags: "--bin", outputs: &["base.bin"], input_base: false },
        MetadataCase { name: "metadata-cli-srec-base", metadata: ".meta.output.name \"@ROOT@base\"", flags: "--srec", outputs: &["base.srec"], input_base: false },
        MetadataCase { name: "metadata-active-conditional", metadata: ".if 1\n.meta.output.hex \"@ROOT@active\"\n.endif\n.if 0\n.meta.output.hex \"@ROOT@inactive\"\n.endif", flags: "", outputs: &["active.hex"], input_base: false },
        MetadataCase { name: "metadata-inactive-unsupported", metadata: ".if 0\n.meta.output.fill \"ff\"\n.meta.output.hex unquoted\n.endif", flags: "", outputs: &[], input_base: false },
    ]
}

fn source(metadata: &str, root: &str) -> String {
    format!(
        ".module main\n.cpu 68020\n{}\n.org $1000\n.byte $12,$34\n.endmodule\n",
        metadata.replace("@ROOT@", root)
    )
}

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "opforge-cli-metadata-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&dir).unwrap();
    dir
}

fn rust_oracle(base: &Path, case: &MetadataCase) -> Vec<Vec<u8>> {
    let dir = base.join(case.name);
    // Each oracle starts from a completely empty tree; no prior output can pass.
    if dir.exists() {
        fs::remove_dir_all(&dir).unwrap();
    }
    fs::create_dir(&dir).unwrap();
    let input = dir.join("entry.asm");
    let root = format!("{}/", dir.display());
    fs::write(&input, source(case.metadata, &root)).unwrap();
    let mut args = vec![
        "opForge".to_string(),
        input.display().to_string(),
        "--cpu".into(),
        "68020".into(),
    ];
    args.extend(
        case.flags
            .split_whitespace()
            .map(|arg| arg.replace("@ROOT@", &root)),
    );
    // A metadata base must retain precedence over the input-derived default.
    // Absolute path literals provide an isolated host counterpart of Work:.
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    if case.input_base {
        // Root only the input-derived default, never replace a metadata base.
        config.out_dir = Some(dir.clone());
    }
    run_with_validated_cli_with_context(&cli, &config).expect("fresh metadata Rust oracle");
    fn files(dir: &Path, root: &Path, inventory: &mut Vec<String>) {
        for entry in fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                files(&path, root, inventory);
            } else if path.is_file() {
                inventory.push(
                    path.strip_prefix(root)
                        .unwrap()
                        .to_string_lossy()
                        .into_owned(),
                );
            }
        }
    }
    let mut inventory = Vec::new();
    files(&dir, &dir, &mut inventory);
    inventory.sort();
    let mut expected: Vec<_> = case
        .outputs
        .iter()
        .map(|name| name.to_string())
        .chain(std::iter::once("entry.asm".into()))
        .collect();
    expected.sort();
    assert_eq!(inventory, expected, "{}: Rust output inventory", case.name);
    case.outputs
        .iter()
        .map(|name| {
            let bytes = fs::read(dir.join(name)).unwrap();
            if name.ends_with(".bin") {
                assert_eq!(bytes, [0x12, 0x34]);
            } else {
                assert!(!bytes.is_empty());
            }
            bytes
        })
        .collect()
}

#[test]
fn compact_cli_metadata_live_rust_oracles() {
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    for case in cases() {
        rust_oracle(&dir, &case);
    }
}

fn negative_cases() -> Vec<(&'static str, String, Option<&'static str>)> {
    let mut result = Vec::new();
    for (name, metadata) in [
        ("metadata-reject-unquoted-name", ".meta.output.name base"),
        ("metadata-reject-unquoted-hex", ".meta.output.hex base"),
        (
            "metadata-reject-unquoted-description",
            ".meta.name description",
        ),
        ("metadata-reject-unquoted-version", ".meta.version 1"),
        (
            "metadata-reject-block",
            ".meta\nname = \"description\"\n.endmeta",
        ),
        (
            "metadata-reject-cpu-specific",
            ".meta.output.68020.name \"Work:base\"",
        ),
        ("metadata-reject-list", ".meta.output.list \"Work:list\""),
        ("metadata-reject-bin", ".meta.output.bin \"Work:image\""),
        ("metadata-reject-fill", ".meta.output.fill \"ff\""),
        (
            "metadata-reject-nested",
            ".namespace nested\n.meta.output.hex \"Work:nested\"\n.endnamespace",
        ),
    ] {
        result.push((name, source(metadata, "Work:"), None));
    }
    result.push(("metadata-reject-imported", ".module main\n.cpu 68020\n.use imported as i\n.byte i.value\n.endmodule\n".into(), Some(".module imported\n.cpu 68020\n.meta.output.hex \"Work:imported\"\n.pub\nvalue = $12\n.endmodule\n")));
    result.push(("metadata-reject-second-root-module", ".module main\n.cpu 68020\n.byte $12\n.endmodule\n.module other\n.meta.output.hex \"Work:other\"\n.endmodule\n".into(), None));
    result
}

#[test]
#[ignore = "fresh real-native inline metadata checkpoint; requires configured FS-UAE"]
fn compact_cli_inline_source_metadata() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.join("native"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into()]),
    )
    .unwrap();
    let telemetry = std::env::var_os("OPFORGE_CLI_METADATA_TELEMETRY").is_some();
    let defines = if telemetry {
        vec![
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_MEMORY_TELEMETRY",
            "OPFORGE_PROGRESS_RUNTIME_COUNTERS",
        ]
    } else {
        vec![]
    };
    let image = super::compact_cli_input::assemble_cli_with_defines(&root, &build, &defines);
    let selected = std::env::var("OPFORGE_CLI_METADATA_CASES").ok();
    let positive = cases();
    let negatives = negative_cases();
    if let Some(names) = &selected {
        for name in names.split(',') {
            assert!(
                positive.iter().any(|case| case.name == name)
                    || negatives.iter().any(|case| case.0 == name)
                    || name == "metadata-reject-go-without-record-output",
                "unknown metadata case {name}"
            );
        }
    }
    let wanted = |name: &str| {
        selected
            .as_ref()
            .is_none_or(|names| names.split(',').any(|item| item == name))
    };
    let mut failures = Vec::new();
    let mut oracle_failures = Vec::new();
    let mut attempted = 0;
    let mut run = |name: &str,
                   source: &str,
                   flags: &str,
                   imported: Option<&str>,
                   proof: OpforgeNativeCliProof<'_>,
                   exit,
                   outputs: Option<&[&str]>| {
        attempted += 1;
        let command = format!(
            "--cpu 68020 Work:entry.asm {}",
            flags.replace("@ROOT@", "Work:")
        );
        let mut files = vec![OpforgeNativeCliGuestFile {
            relative_path: "entry.asm",
            bytes: source.as_bytes(),
        }];
        if let Some(imported) = imported {
            files.push(OpforgeNativeCliGuestFile {
                relative_path: "imported.asm",
                bytes: imported.as_bytes(),
            });
        }
        let case = OpforgeNativeCliParityCase {
            name: name,
            cpu_override: "68020",
            extra_assembly_defines: &defines,
            source_override: Some(source.as_bytes()),
            command_template: Some(&command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof: proof,
        };
        match run_prebuilt_compact_cli_case_from_env(&root, &case, &image) {
            Ok(FsUaeSmokeOutcome::Completed { runs })
                if runs.len() == 1
                    && runs[0].protocol_completed
                    && runs[0].exit_code == Some(exit) =>
            {
                if telemetry && exit == 0 {
                    let Some(memory) = runs[0]
                        .captured_artifacts
                        .get(&PathBuf::from("Work/memory.bin"))
                    else {
                        failures.push(format!("{name}: missing memory telemetry"));
                        return;
                    };
                    if memory.len() != 2280 || &memory[..4] != b"MEMD" {
                        failures.push(format!("{name}: invalid memory telemetry record"));
                        return;
                    }
                    let word = |index: usize| {
                        u32::from_be_bytes(memory[index * 4..index * 4 + 4].try_into().unwrap())
                    };
                    if word(1) != 0
                        || word(3) != word(4)
                        || word(11) != 0
                        || word(29) != 0
                        || word(524) != 0
                    {
                        failures.push(format!(
                            "{name}: telemetry ownership, allocation or profiling failure"
                        ));
                        return;
                    }
                }
                if let Some(outputs) = outputs {
                    // The runner stages input.bin for CompactCli and the startup
                    // alias named by FS_UAE_STARTUP_HUNK_ALIAS. Exclude only those
                    // exact inputs; assembler output must never collide with them.
                    let staged = [
                        PathBuf::from("Work/input.bin"),
                        PathBuf::from("Work").join(FS_UAE_STARTUP_HUNK_ALIAS),
                    ];
                    assert!(outputs
                        .iter()
                        .all(|name| !staged.contains(&PathBuf::from("Work").join(name))));
                    // Include every emitted output class, including unwanted defaults.
                    let actual: Vec<_> = runs[0]
                        .captured_artifacts
                        .keys()
                        .filter(|path| {
                            !staged.contains(path)
                                && matches!(
                                    path.extension().and_then(|ext| ext.to_str()),
                                    Some("hex" | "srec" | "lst" | "bin" | "hunk")
                                )
                        })
                        .filter(|path| !telemetry || **path != PathBuf::from("Work/memory.bin"))
                        .map(|path| path.to_string_lossy().into_owned())
                        .collect();
                    let mut expected: Vec<_> =
                        outputs.iter().map(|name| format!("Work/{name}")).collect();
                    expected.sort();
                    if actual != expected {
                        failures.push(format!(
                            "{name}: guest output inventory {actual:?}, expected {expected:?}"
                        ));
                    }
                }
                eprintln!("{name}: fresh native completion, exit {exit}");
            }
            Ok(FsUaeSmokeOutcome::Completed { runs }) => failures.push(format!(
                "{name}: invalid completion/exit ({} runs)",
                runs.len()
            )),
            Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                failures.push(format!("{name}: skipped: {reason}"))
            }
            Err(error) => failures.push(format!("{name}: {error}")),
        }
    };
    for case in positive.iter().filter(|case| wanted(case.name)) {
        let oracle = std::panic::catch_unwind(|| rust_oracle(&dir, case));
        let Ok(oracle) = oracle else {
            oracle_failures.push(format!("{}: live Rust oracle failed", case.name));
            continue;
        };
        let paths: Vec<_> = case
            .outputs
            .iter()
            .map(|name| format!("Work/{name}"))
            .collect();
        let artifacts: Vec<_> = paths
            .iter()
            .zip(&oracle)
            .map(|(path, bytes)| OpforgeNativeCliExpectedArtifact {
                relative_path: path,
                rust_oracle: bytes,
            })
            .collect();
        let absent = [
            "Work/entry.hex",
            "Work/entry.bin",
            "Work/entry.srec",
            "Work/entry.lst",
            "Work/base.hex",
            "Work/base.bin",
            "Work/base.srec",
        ];
        let proof = if artifacts.is_empty() {
            OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(&absent)
        } else {
            OpforgeNativeCliProof::ExactArtifacts(&artifacts)
        };
        run(
            case.name,
            &source(case.metadata, "Work:"),
            case.flags,
            None,
            proof,
            0,
            Some(case.outputs),
        );
    }
    for (name, source, imported) in negatives.iter().filter(|case| wanted(case.0)) {
        run(
            name,
            source,
            "",
            *imported,
            OpforgeNativeCliProof::ExpectedFailureContaining(
                "binary source: unsupported or invalid input",
            ),
            20,
            None,
        );
    }
    if wanted("metadata-reject-go-without-record-output") {
        run(
            "metadata-reject-go-without-record-output",
            &source("", "Work:"),
            "--go 1234",
            None,
            OpforgeNativeCliProof::ExpectedFailureContaining("compact CLI:"),
            20,
            None,
        );
    }
    drop(run);
    failures.extend(oracle_failures);
    let expected = positive.iter().filter(|case| wanted(case.name)).count()
        + negatives.iter().filter(|case| wanted(case.0)).count()
        + usize::from(wanted("metadata-reject-go-without-record-output"));
    assert_eq!(
        attempted, expected,
        "selected Rust oracle failed before native attempt"
    );
    assert!(attempted > 0);
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
