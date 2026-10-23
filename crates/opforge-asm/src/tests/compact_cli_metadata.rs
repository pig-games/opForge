// SPDX-License-Identifier: GPL-3.0-or-later
//! Literal metadata controls with exact output and inventory comparisons.
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

#[path = "compact_cli_metadata_blocks.rs"]
mod blocks;

struct MetadataCase {
    name: &'static str,
    metadata: &'static str,
    flags: &'static str,
    outputs: &'static [&'static str],
    binary_outputs: &'static [(&'static str, &'static [u8])],
    input_base: bool,
    empty_image: bool,
}

fn case(
    name: &'static str,
    metadata: &'static str,
    flags: &'static str,
    outputs: &'static [&'static str],
    input_base: bool,
) -> MetadataCase {
    MetadataCase {
        name,
        metadata,
        flags,
        outputs,
        binary_outputs: &[],
        input_base,
        empty_image: false,
    }
}

fn set_binary_outputs(
    cases: &mut [MetadataCase],
    case_name: &str,
    binary_outputs: &'static [(&'static str, &'static [u8])],
) {
    let case = cases
        .iter_mut()
        .find(|case| case.name == case_name)
        .unwrap_or_else(|| panic!("missing metadata case {case_name}"));
    case.binary_outputs = binary_outputs;
}

fn set_empty_image(cases: &mut [MetadataCase], case_name: &str) {
    let case = cases
        .iter_mut()
        .find(|case| case.name == case_name)
        .unwrap_or_else(|| panic!("missing metadata case {case_name}"));
    case.empty_image = true;
}

fn cases() -> Vec<MetadataCase> {
    let mut cases = vec![
        case("metadata-nested-name-only", ".meta.output.name \"@ROOT@nested/base\"", "", &[], false),
        case("metadata-source-hex-relative-parent", ".meta.output.name \"@ROOT@nested/base\"\n.meta.output.hex \"records\"", "", &["nested/records.hex"], false),
        case("metadata-source-hex-relative-components", ".meta.output.name \"@ROOT@nested/base\"\n.meta.output.hex \"./child/../records\"", "", &["nested/records.hex"], false),
        case("metadata-cli-hex-relative-parent", ".meta.output.name \"@ROOT@nested/base\"", "--hex records", &["nested/records.hex"], false),
        case("metadata-source-hex-hidden-basename", ".meta.output.name \"@ROOT@base\"\n.meta.output.hex \".records\"", "", &[".records.hex"], false),
        case("metadata-name-only", ".meta.output.name \"@ROOT@base\"", "", &[], false),
        case("metadata-description-only", ".meta.name \"Description\"\n.meta.version \"1.2\"", "", &[], false),
        case("metadata-source-hex-input-base", ".meta.output.hex", "", &["entry.hex"], true),
        case("metadata-source-hex-empty-input-base", ".meta.output.hex \"\"", "", &["entry.hex"], true),
        case("metadata-source-hex-name", ".meta.output.hex \"@ROOT@source\"", "", &["source.hex"], false),
        case("metadata-source-hex-omitted", ".meta.output.name \"@ROOT@base\"\n.meta.output.hex", "", &["base.hex"], false),
        case("metadata-source-hex-empty", ".meta.output.name \"@ROOT@base\"\n.meta.output.hex \"\"", "", &["base.hex"], false),
        case("metadata-source-hex-cli-bin", ".meta.output.hex \"@ROOT@source\"", "--bin @ROOT@explicit.bin", &["explicit.bin", "source.hex"], false),
        case("metadata-source-hex-go", ".meta.output.hex \"@ROOT@source\"", "--go 1234", &["source.hex"], false),
        case("metadata-source-hex-cli-srec", ".meta.output.hex \"@ROOT@source\"", "--srec @ROOT@explicit.srec", &["explicit.srec", "source.hex"], false),
        case("metadata-cli-hex-overrides-source", ".meta.output.hex \"@ROOT@source\"", "--hex @ROOT@explicit.hex", &["explicit.hex"], false),
        case("metadata-cli-hex-base", ".meta.output.name \"@ROOT@base\"", "--hex", &["base.hex"], false),
        case("metadata-cli-bin-base", ".meta.output.name \"@ROOT@base\"", "--bin", &["base.bin"], false),
        case("metadata-cli-srec-base", ".meta.output.name \"@ROOT@base\"", "--srec", &["base.srec"], false),
        case("metadata-active-conditional", ".if 1\n.meta.output.hex \"@ROOT@active\"\n.meta.output.bin \"@BIN_ROOT@active\"\n.endif\n.if 0\n.meta.output.hex \"@ROOT@inactive\"\n.meta.output.bin \"@BIN_ROOT@inactive\"\n.endif", "", &["active.hex", "active.bin"], false),
        case("metadata-inactive-unsupported", ".if 0\n.meta.output.fill \"ff\"\n.meta.output.bin unquoted\n.meta.output.hex unquoted\n.endif", "", &[], false),
    ];
    cases[12].binary_outputs = &[("explicit.bin", &[0x12, 0x34])];
    cases[17].binary_outputs = &[("base.bin", &[0x12, 0x34])];
    cases[19].binary_outputs = &[("active.bin", &[0x12, 0x34])];

    cases.extend([
        case("metadata-source-bin-name", ".meta.output.bin \"@BIN_ROOT@source\"", "", &["source.bin"], false),
        case("metadata-source-bin-omitted-metadata-base", ".meta.output.name \"@ROOT@base\"\n.meta.output.bin", "", &["base.bin"], false),
        case("metadata-source-bin-empty-metadata-base", ".meta.output.name \"@ROOT@base\"\n.meta.output.bin \"\"", "", &["base.bin"], false),
        case("metadata-source-bin-custom-extension", ".meta.output.bin \"@BIN_ROOT@source.image\"", "", &["source.image"], false),
        case("metadata-source-bin-range-padded", ".meta.output.bin \"@BIN_ROOT@range.bin:0ffe:1003\"\n.meta.output.fill \"aa\"", "", &["range.bin"], false),
        case("metadata-source-bin-range-empty-intersection", ".meta.output.bin \"@BIN_ROOT@empty.bin:2000:2001\"", "", &["empty.bin"], false),
        case("metadata-source-bin-multiple-unnamed-ranges", ".meta.output.name \"@ROOT@base\"\n.meta.output.bin \"1000:1001\"\n.meta.output.bin \"0000:0000\"", "", &["base-1000.bin", "base-0000.bin"], false),
        case("metadata-source-bin-multiple-unnamed-no-ranges", ".meta.output.name \"@ROOT@base\"\n.meta.output.bin\n.meta.output.bin \"\"", "", &["base-1.bin", "base-2.bin"], false),
        case("metadata-source-bin-multiple-unnamed-no-ranges-nonempty", ".meta.output.name \"@ROOT@base\"\n.meta.output.bin\n.meta.output.bin \"\"", "", &["base-1000.bin"], false),
        case("metadata-source-bin-repeated-fill-last-wins", ".meta.output.bin \"@BIN_ROOT@repeat.bin:0ffe:1003\"\n.meta.output.fill \"aa\"\n.meta.output.fill \"bb\"", "", &["repeat.bin"], false),
        case("metadata-source-bin-cli-bin-suppresses-vector", ".meta.output.name \"@ROOT@base\"\n.meta.output.hex \"@ROOT@source\"\n.meta.output.bin \"@BIN_ROOT@source\"", "--bin", &["base.bin", "source.hex"], false),
    ]);
    set_empty_image(&mut cases, "metadata-source-bin-multiple-unnamed-no-ranges");
    set_binary_outputs(
        &mut cases,
        "metadata-source-hex-cli-bin",
        &[("explicit.bin", &[0x12, 0x34])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-cli-bin-base",
        &[("base.bin", &[0x12, 0x34])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-active-conditional",
        &[("active.bin", &[0x12, 0x34])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-name",
        &[("source.bin", &[0x12, 0x34])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-omitted-metadata-base",
        &[("base.bin", &[0x12, 0x34])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-empty-metadata-base",
        &[("base.bin", &[0x12, 0x34])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-custom-extension",
        &[("source.image", &[0x12, 0x34])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-range-padded",
        &[("range.bin", &[0xaa, 0xaa, 0x12, 0x34, 0xaa, 0xaa])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-range-empty-intersection",
        &[("empty.bin", &[0, 0])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-multiple-unnamed-ranges",
        &[("base-1000.bin", &[0x12, 0x34]), ("base-0000.bin", &[0])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-multiple-unnamed-no-ranges",
        &[("base-1.bin", &[]), ("base-2.bin", &[])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-multiple-unnamed-no-ranges-nonempty",
        &[("base-1000.bin", &[0x12, 0x34])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-repeated-fill-last-wins",
        &[("repeat.bin", &[0xbb, 0xbb, 0x12, 0x34, 0xbb, 0xbb])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-source-bin-cli-bin-suppresses-vector",
        &[("base.bin", &[0x12, 0x34])],
    );
    cases.extend(blocks::cases());
    cases
}

fn source(metadata: &str, root: &str, empty_image: bool) -> String {
    // BIN's shared colon grammar reserves ':' for a range. Relative guest BIN
    // names are rooted by Work:, while host counterparts need an isolated path.
    // NAME/HEX operands are ordinary paths and retain their volume prefix.
    let bin_root = if root == "Work:" { "" } else { root };
    format!(
        ".module main\n.cpu 68020\n{}\n.org $1000\n{}.endmodule\n",
        metadata
            .replace("@BIN_ROOT@", bin_root)
            .replace("@ROOT@", root),
        if empty_image { "" } else { ".byte $12,$34\n" }
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
    fs::write(&input, source(case.metadata, &root, case.empty_image)).unwrap();
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
                let expected = case
                    .binary_outputs
                    .iter()
                    .find_map(|(path, expected)| (*path == *name).then_some(*expected))
                    .unwrap_or_else(|| {
                        panic!(
                            "{}: missing independent binary oracle for {name}",
                            case.name
                        )
                    });
                assert_eq!(bytes, expected, "{}: {name}", case.name);
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

struct NegativeMetadataCase {
    name: &'static str,
    source: String,
    imported: Option<&'static str>,
    diagnostic: &'static str,
}

fn rejection(
    name: &'static str,
    source: String,
    imported: Option<&'static str>,
) -> NegativeMetadataCase {
    NegativeMetadataCase {
        name,
        source,
        imported,
        diagnostic: "binary source: unsupported or invalid input",
    }
}

fn negative_cases() -> Vec<NegativeMetadataCase> {
    let mut result = Vec::new();
    for (name, metadata) in [
        ("metadata-reject-unquoted-name", ".meta.output.name base"),
        ("metadata-reject-unquoted-hex", ".meta.output.hex base"),
        (
            "metadata-reject-unquoted-description",
            ".meta.name description",
        ),
        ("metadata-reject-unquoted-version", ".meta.version 1"),
        ("metadata-reject-list", ".meta.output.list \"Work:list\""),
        (
            "metadata-reject-bin-range",
            ".meta.output.bin \"1000:0fff\"",
        ),
        ("metadata-reject-fill-byte", ".meta.output.fill \"g0\""),
        ("metadata-reject-nul-path", ".meta.output.hex \"bad\0name\""),
        (
            "metadata-reject-nested",
            ".namespace nested\n.meta.output.hex \"Work:nested\"\n.endnamespace",
        ),
    ] {
        result.push(rejection(name, source(metadata, "Work:", false), None));
    }
    result.push(rejection("metadata-reject-imported", ".module main\n.cpu 68020\n.use imported as i\n.byte i.value\n.endmodule\n".into(), Some(".module imported\n.cpu 68020\n.meta.output.hex \"Work:imported\"\n.pub\nvalue = $12\n.endmodule\n")));
    result.push(rejection("metadata-reject-second-root-module", ".module main\n.cpu 68020\n.byte $12\n.endmodule\n.module other\n.meta.output.hex \"Work:other\"\n.endmodule\n".into(), None));
    let mut late = rejection(
        "metadata-reject-late-bin-path",
        source(
            ".meta.output.bin \"first\"\n.meta.output.bin \"../escape\"",
            "Work:",
            false,
        ),
        None,
    );
    late.diagnostic = "opforge: output failed";
    result.push(late);
    result.extend(blocks::negative_cases());
    result
}

struct OwnedMetadataRun {
    name: String,
    source: String,
    command: String,
    imported: Option<String>,
    artifacts: Vec<(String, Vec<u8>)>,
    absent: Vec<String>,
    diagnostic: Option<String>,
    exit: i32,
    outputs: Option<Vec<String>>,
}

fn check_metadata_run(
    name: &str,
    run: &FsUaeSmokeRun,
    exit: i32,
    outputs: Option<&[&str]>,
    telemetry: bool,
    failures: &mut Vec<String>,
) {
    if telemetry && exit == 0 {
        let Some(memory) = run
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
        let word =
            |index: usize| u32::from_be_bytes(memory[index * 4..index * 4 + 4].try_into().unwrap());
        if word(1) != 0 || word(3) != word(4) || word(11) != 0 || word(29) != 0 || word(524) != 0 {
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
        let actual: Vec<_> = run
            .captured_artifacts
            .keys()
            .filter(|path| {
                !staged.contains(path)
                    && (outputs
                        .iter()
                        .any(|name| PathBuf::from("Work").join(name) == **path)
                        || matches!(
                            path.extension().and_then(|ext| ext.to_str()),
                            Some("hex" | "srec" | "lst" | "bin" | "hunk")
                        ))
            })
            .filter(|path| !telemetry || path.as_path() != Path::new("Work/memory.bin"))
            .map(|path| path.to_string_lossy().into_owned())
            .collect();
        let mut expected: Vec<_> = outputs.iter().map(|name| format!("Work/{name}")).collect();
        expected.sort();
        if actual != expected {
            failures.push(format!(
                "{name}: guest output inventory {actual:?}, expected {expected:?}"
            ));
        }
    }
    eprintln!("{name}: fresh native completion, exit {exit}");
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
    let image = if let Some(path) = std::env::var_os("OPFORGE_CLI_METADATA_IMAGE") {
        fs::read(path).expect("read prebuilt compact CLI metadata image")
    } else {
        let build = build_native_packages(
            &engine::build_default_asm_registry(),
            &dir.join("native"),
            &root.join("native/motorola68000/amigaos/main.asm"),
            &EmbedSelection::Targets(vec!["68020".into()]),
        )
        .unwrap();
        super::compact_cli_input::assemble_cli_with_defines(&root, &build, &defines)
    };
    let selected = std::env::var("OPFORGE_CLI_METADATA_CASES").ok();
    let positive = cases();
    let negatives = negative_cases();
    if let Some(names) = &selected {
        for name in names.split(',') {
            assert!(
                positive.iter().any(|case| case.name == name)
                    || negatives.iter().any(|case| case.name == name)
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
    let batch_size = std::env::var("OPFORGE_CLI_METADATA_BATCH_SIZE")
        .ok()
        .map(|value| {
            let size: usize = value
                .parse()
                .expect("metadata batch size must be in 1..=64");
            assert!(
                (1..=64).contains(&size),
                "metadata batch size must be in 1..=64"
            );
            size
        });
    let mut pending = Vec::new();
    let mut failures = Vec::new();
    let mut oracle_failures = Vec::new();
    let mut attempted = 0;
    {
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
            if batch_size.is_some() {
                let mut owned = OwnedMetadataRun {
                    name: name.into(),
                    source: source.into(),
                    command,
                    imported: imported.map(str::to_owned),
                    artifacts: vec![],
                    absent: vec![],
                    diagnostic: None,
                    exit,
                    outputs: outputs.map(|names| names.iter().map(|name| (*name).into()).collect()),
                };
                match proof {
                    OpforgeNativeCliProof::ExactArtifacts(artifacts) => {
                        owned.artifacts = artifacts
                            .iter()
                            .map(|artifact| {
                                (artifact.relative_path.into(), artifact.rust_oracle.to_vec())
                            })
                            .collect()
                    }
                    OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(paths) => {
                        owned.absent = paths.iter().map(|path| (*path).into()).collect()
                    }
                    OpforgeNativeCliProof::ExpectedFailureContaining(diagnostic) => {
                        owned.diagnostic = Some(diagnostic.into())
                    }
                    _ => panic!("unsupported metadata proof"),
                }
                pending.push(owned);
                return;
            }
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
                name,
                cpu_override: "68020",
                extra_assembly_defines: &defines,
                source_override: Some(source.as_bytes()),
                command_template: Some(&command),
                package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &files,
                proof,
            };
            match run_prebuilt_compact_cli_case_from_env(&root, &case, &image) {
                Ok(FsUaeSmokeOutcome::Completed { runs })
                    if runs.len() == 1
                        && runs[0].protocol_completed
                        && runs[0].exit_code == Some(exit) =>
                {
                    check_metadata_run(name, &runs[0], exit, outputs, telemetry, &mut failures);
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
                &source(case.metadata, "Work:", case.empty_image),
                case.flags,
                None,
                proof,
                0,
                Some(case.outputs),
            );
        }
        for case in negatives.iter().filter(|case| wanted(case.name)) {
            run(
                case.name,
                &case.source,
                "",
                case.imported,
                OpforgeNativeCliProof::ExpectedFailureContaining(case.diagnostic),
                20,
                Some(&[]),
            );
        }
        if wanted("metadata-reject-go-without-record-output") {
            run(
                "metadata-reject-go-without-record-output",
                &source("", "Work:", false),
                "--go 1234",
                None,
                OpforgeNativeCliProof::ExpectedFailureContaining("opforge:"),
                20,
                None,
            );
        }
    }
    if let Some(batch_size) = batch_size {
        let files: Vec<Vec<_>> = pending
            .iter()
            .map(|owned| {
                let mut files = vec![OpforgeNativeCliGuestFile {
                    relative_path: "entry.asm",
                    bytes: owned.source.as_bytes(),
                }];
                if let Some(imported) = &owned.imported {
                    files.push(OpforgeNativeCliGuestFile {
                        relative_path: "imported.asm",
                        bytes: imported.as_bytes(),
                    });
                }
                files
            })
            .collect();
        let artifacts: Vec<Vec<_>> = pending
            .iter()
            .map(|owned| {
                owned
                    .artifacts
                    .iter()
                    .map(|(path, bytes)| OpforgeNativeCliExpectedArtifact {
                        relative_path: path,
                        rust_oracle: bytes,
                    })
                    .collect()
            })
            .collect();
        let absent: Vec<Vec<_>> = pending
            .iter()
            .map(|owned| owned.absent.iter().map(String::as_str).collect())
            .collect();
        let cases: Vec<_> = pending
            .iter()
            .enumerate()
            .map(|(index, owned)| OpforgeNativeCliParityCase {
                name: &owned.name,
                cpu_override: "68020",
                extra_assembly_defines: &[],
                source_override: Some(owned.source.as_bytes()),
                command_template: Some(&owned.command),
                package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &files[index],
                proof: if let Some(diagnostic) = &owned.diagnostic {
                    OpforgeNativeCliProof::ExpectedFailureContaining(diagnostic)
                } else if artifacts[index].is_empty() {
                    OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(&absent[index])
                } else {
                    OpforgeNativeCliProof::ExactArtifacts(&artifacts[index])
                },
            })
            .collect();
        // The frozen image already contains the selected instrumentation defines.
        // Breadth is correctness-only: per-case timing is deliberately absent.
        if !cases.is_empty() {
            match run_prebuilt_compact_cli_breadth_from_env(&root, &cases, &image, batch_size) {
                Ok(results) => {
                    assert_eq!(results.len(), pending.len());
                    for (owned, result) in pending.iter().zip(results) {
                        if let Some(error) = result.error {
                            failures.push(format!("{}: {error}", owned.name));
                            continue;
                        }
                        let Some(run) = result.run else {
                            failures.push(format!("{}: missing completion", owned.name));
                            continue;
                        };
                        if !run.protocol_completed || run.exit_code != Some(owned.exit) {
                            failures.push(format!("{}: invalid completion/exit", owned.name));
                            continue;
                        }
                        assert!(run.start_to_done_host_seconds.is_none());
                        let outputs = owned
                            .outputs
                            .as_ref()
                            .map(|names| names.iter().map(String::as_str).collect::<Vec<_>>());
                        check_metadata_run(
                            &owned.name,
                            &run,
                            owned.exit,
                            outputs.as_deref(),
                            telemetry,
                            &mut failures,
                        );
                    }
                }
                Err(error) => failures.push(format!("metadata breadth: {error}")),
            }
        }
    }
    failures.extend(oracle_failures);
    let expected = positive.iter().filter(|case| wanted(case.name)).count()
        + negatives.iter().filter(|case| wanted(case.name)).count()
        + usize::from(wanted("metadata-reject-go-without-record-output"));
    assert_eq!(
        attempted, expected,
        "selected Rust oracle failed before native attempt"
    );
    assert!(attempted > 0);
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
