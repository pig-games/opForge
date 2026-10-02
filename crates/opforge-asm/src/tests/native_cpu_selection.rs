//! Source-selected initial packages and base 6502 breadth use live Rust oracles.
use super::*;
use crate::fs_uae_smoke::{
    run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliGuestFile,
    OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase, OpforgeNativeCliProof,
};
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

struct Case {
    name: &'static str,
    source: &'static str,
    command: &'static str,
    rejection: Option<&'static str>,
}
const CASES: &[Case] = &[
    Case {
        name: "numeric-source-cpu",
        source: ";comment\n.cpu 6502;target\n.org $1000\n lda #$42;instruction\n sta $1234\n",
        command: "Work:main.asm --bin Work:output.bin -P Work:packages",
        rejection: None,
    },
    Case {
        name: "qualified-module-quoted-alias",
        source: ".module app.main\n.CPU \"m68020\"\n moveq #42,d0\n rts\n.endmodule\n",
        command: "Work:main.asm --bin Work:output.bin",
        rejection: None,
    },
    Case {
        name: "directory-default",
        source: ".cpu M6502\n lda #1\n",
        command: "Work: --bin Work:output.bin -P Work:packages",
        rejection: None,
    },
    Case {
        name: "omitted-input",
        source: ".cpu 6502\n nop\n",
        command: "--bin Work:output.bin -P Work:packages",
        rejection: None,
    },
    Case {
        name: "engine-default-without-declaration",
        source: ".byte $12,$34\n",
        command: "Work:main.asm --bin Work:output.bin -P Work:packages",
        rejection: None,
    },
    Case {
        name: "explicit-cpu-bypasses-preamble",
        source: ".org $1000\n.cpu m6502\n lda #2\n",
        command: "--cpu 6502 Work:main.asm --bin Work:output.bin -P Work:packages",
        rejection: None,
    },
    Case {
        name: "malformed-cpu",
        source: ".cpu\n",
        command: "Work:main.asm --bin Work:output.bin",
        rejection: Some("compact CLI: cannot select initial target from root preamble"),
    },
    Case {
        name: "unknown-cpu",
        source: ".cpu nonexistent\n.byte 1\n",
        command: "Work:main.asm --bin Work:output.bin",
        rejection: Some("package: missing, invalid or incompatible runtime package"),
    },
];

const ALL_MODES: &str = include_str!("../../../../examples/mos6502/6502_allmodes.asm");
const FORWARD: &str = ".cpu 6502\n.org $00fd\nstart\n lda target\n nop\ntarget\n rts\n";

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "opforge-source-cpu-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&dir).unwrap();
    dir
}

fn oracle(dir: &Path, name: &str, source: &str) -> Result<Vec<u8>, String> {
    let source_path = dir.join(format!("{}.asm", name.replace('-', "_")));
    let output = dir.join(format!("{name}.bin"));
    fs::write(&source_path, source).unwrap();
    let cli = Cli::parse_from([
        "opForge".into(),
        source_path.to_string_lossy().into_owned(),
        "--bin".into(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).map_err(|e| e.to_string())?;
    run_with_validated_cli_with_context(&cli, &config).map_err(|e| match e {
        cli_core::CliRunError::Assembler { error, .. } => format!("{:?}", error.diagnostics()),
        cli_core::CliRunError::Workflow { error, .. } => error.to_string(),
        cli_core::CliRunError::WarningsAsErrors { .. } => "warnings as errors".into(),
    })?;
    fs::read(output).map_err(|e| e.to_string())
}

#[test]
fn initial_cpu_and_6502_live_rust_oracles() {
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    for case in CASES {
        let result = oracle(&dir, case.name, case.source);
        if case.rejection.is_some() {
            assert!(result.is_err(), "{}", case.name);
        } else {
            result.unwrap_or_else(|e| panic!("{}: {e}", case.name));
        }
    }
    assert!(!oracle(&dir, "all-modes", ALL_MODES).unwrap().is_empty());
    assert_eq!(
        oracle(&dir, "forward", FORWARD).unwrap(),
        [0xad, 1, 1, 0xea, 0x60]
    );
}

#[test]
#[ignore = "fresh source-selected compact CLI proof with generated runtime packages"]
fn native_initial_cpu_selection_fs_uae() {
    native(false);
}

#[test]
#[ignore = "base6502 addressing matrix and forward-sizing artifact proof"]
fn native_base6502_parity_fs_uae() {
    native(true);
}

fn native(breadth: bool) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.join("build"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into()]),
    )
    .unwrap();
    let image = crate::fs_uae_smoke::compact_cli_input::assemble_cli(&root, &build);
    let package = fs::read(build.output_dir.join("packages/m6502--transparent.bin")).unwrap();
    let default_cpu = engine::default_cpu();
    let registry = engine::build_default_asm_registry();
    let default_name = format!(
        "{}--{}.bin",
        default_cpu.as_str(),
        registry.cpu_default_dialect(default_cpu).unwrap()
    );
    let default_package = fs::read(build.output_dir.join("packages").join(&default_name)).unwrap();
    let default_path = format!("packages/{default_name}");
    let breadth_cases = [
        Case {
            name: "all-modes",
            source: ALL_MODES,
            command: "Work:main.asm --bin Work:output.bin -P Work:packages",
            rejection: None,
        },
        Case {
            name: "forward-sizing",
            source: FORWARD,
            command: "Work:main.asm --bin Work:output.bin -P Work:packages",
            rejection: None,
        },
    ];
    let selected = std::env::var("OPFORGE_CPU_SELECTION_CASES").ok();
    let mut count = 0;
    for case in if breadth { &breadth_cases[..] } else { CASES } {
        if selected
            .as_ref()
            .is_some_and(|s| !s.split(',').any(|n| n == case.name))
        {
            continue;
        }
        count += 1;
        let expected = oracle(&dir, case.name, case.source);
        let files = [
            OpforgeNativeCliGuestFile {
                relative_path: "main.asm",
                bytes: case.source.as_bytes(),
            },
            OpforgeNativeCliGuestFile {
                relative_path: "packages/m6502--transparent.bin",
                bytes: &package,
            },
            OpforgeNativeCliGuestFile {
                relative_path: &default_path,
                bytes: &default_package,
            },
        ];
        let proof = if let Some(message) = case.rejection {
            OpforgeNativeCliProof::ExpectedFailureContaining(message)
        } else {
            OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.bin",
                rust_oracle: expected.as_ref().unwrap(),
            }
        };
        let native_case = OpforgeNativeCliParityCase {
            name: case.name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(case.source.as_bytes()),
            command_template: Some(case.command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof,
        };
        let result = run_prebuilt_compact_cli_case_from_env(&root, &native_case, &image)
            .unwrap_or_else(|e| panic!("{}: {e}", case.name));
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("fresh native execution required")
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].protocol_completed, "{}", case.name);
        assert_eq!(
            runs[0].exit_code,
            Some(if case.rejection.is_some() { 20 } else { 0 }),
            "{}: {}",
            case.name,
            runs[0].stdout
        );
        eprintln!(
            "SOURCE_CPU case={} exact={} seconds={:?} cli_bytes={} linked_reserved_bytes={} package_bytes={}",
            case.name,
            case.rejection.is_none(),
            runs[0].start_to_done_host_seconds,
            image.len(),
            hunk::allocation(&image).unwrap().total(),
            package.len()
        );
    }
    assert!(count > 0, "selected no CPU cases");
    if let Some(export) = std::env::var_os("OPFORGE_CPU_PARITY_EXPORT") {
        assert!(
            breadth && selected.is_none(),
            "export requires the complete breadth proof"
        );
        let export = PathBuf::from(export);
        assert!(export.is_absolute() && !export.exists());
        fs::create_dir_all(export.join("packages")).unwrap();
        fs::write(export.join("opforge"), &image).unwrap();
        fs::write(export.join("packages/m6502--transparent.bin"), &package).unwrap();
        fs::write(export.join("main.asm"), ALL_MODES).unwrap();
        fs::write(
            export.join("oracle.bin"),
            oracle(&dir, "export-oracle", ALL_MODES).unwrap(),
        )
        .unwrap();
        fs::write(export.join("README.txt"),"Base m6502 parity bundle. From this directory on AmigaOS:\nopforge main.asm --bin output.bin -P packages\nCompare output.bin exactly with oracle.bin. Package is independent of source. CLI embeds m68020; m6502 is external.\n").unwrap();
    }
}
