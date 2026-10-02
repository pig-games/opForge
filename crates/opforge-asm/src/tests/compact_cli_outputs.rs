// SPDX-License-Identifier: GPL-3.0-or-later
//! C2 output checkpoint: each native artifact carries its fresh Rust CLI oracle.
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

const HUNK_BODY: &str = ".module main\n.cpu 68020\n.section code,kind=code\n move.l #payload,d0\n lea buffer,a0\n rts\n.endsection\n.section data,kind=data\npayload: .byte $12,$34,$56,$78\n.endsection\n.section bss,kind=bss\nbuffer: .res byte,12\n.endsection\n";
const PLACED_BODY: &str = ".module main\n.cpu 68020\n.region first,$1000,$1001\n.region second,$1002,$10ff\n.section code,kind=code\n.byte $11,$22\n.endsection\n.section data,kind=data\n.byte $33,$44\n.endsection\n.place code in first\n.place data in second\n";

struct OutputCase {
    name: &'static str,
    source: String,
    explicit_bin: bool,
    artifacts: Vec<(&'static str, Option<Vec<u8>>)>,
}

fn output_cases() -> Vec<OutputCase> {
    vec![
        OutputCase {
            name: "source-hunk-minimal",
            source: HUNK_BODY.replace(" move.l #payload,d0\n lea buffer,a0", " moveq #42,d0") + ".output \"minimal\",format=hunk,sections=code,data,bss\n.endmodule\n",
            explicit_bin: false,
            artifacts: vec![("minimal", None)],
        },
        OutputCase {
            name: "source-mapped-flat",
            source: ".module main\n.cpu 68020\n.region rom,$1000,$10ff\n.use dep (entry) as d map { code -> app_code }\n.section app_code\n.byte $22\n.word d.entry\n.byte $33\n.endsection\n.place app_code in rom\n.output \"mapped\",format=bin,sections=app_code\n.endmodule\n.module dep\n.cpu 68020\n.pub\n.section code,logical\n.byte $b0\nentry .block\n.byte $11\n.bend\nunused .block\n.byte $99\n.bend\n.byte $b1\n.endsection\n.endmodule\n".into(),
            explicit_bin: false,
            artifacts: vec![("mapped", Some(vec![0x22,0x10,0x05,0x33,0xb0,0x11,0xb1]))],
        },
        OutputCase {
            name: "source-hunk-relocations-bss",
            source: format!("{HUNK_BODY}.output \"literal-image\",format=hunk,sections=code,data,bss\n.endmodule\n"),
            explicit_bin: false,
            artifacts: vec![("literal-image", None)],
        },
        OutputCase {
            name: "source-multiple-hunks-literal-names",
            source: format!("{HUNK_BODY}.output \"first.image\",format=hunk,sections=code,data,bss\n.output \"second\",format=hunk,sections=code,data,bss\n.endmodule\n"),
            explicit_bin: false,
            artifacts: vec![("first.image", None), ("second", None)],
        },
        OutputCase {
            name: "source-placed-bin-literal-name",
            source: format!("{PLACED_BODY}.output \"plain-image\",format=bin,sections=code,data\n.endmodule\n"),
            explicit_bin: false,
            artifacts: vec![("plain-image", Some(vec![0x11, 0x22, 0x33, 0x44]))],
        },
        OutputCase {
            name: "source-nested-output-path",
            source: format!("{PLACED_BODY}.output \"nested/outputs/image\",format=bin,sections=code,data\n.endmodule\n"),
            explicit_bin: false,
            artifacts: vec![("nested/outputs/image", Some(vec![0x11, 0x22, 0x33, 0x44]))],
        },
        OutputCase {
            name: "source-placed-prg-literal-name",
            source: format!("{PLACED_BODY}.output \"program-image\",format=prg,sections=code,data\n.endmodule\n"),
            explicit_bin: false,
            artifacts: vec![("program-image", Some(vec![0x00, 0x10, 0x11, 0x22, 0x33, 0x44]))],
        },
        OutputCase {
            name: "explicit-bin-adds-source-bin",
            source: format!("{PLACED_BODY}.output \"source-image\",format=bin,sections=code,data\n.endmodule\n"),
            explicit_bin: true,
            artifacts: vec![("explicit.bin", Some(vec![0x11, 0x22, 0x33, 0x44])), ("source-image", Some(vec![0x11, 0x22, 0x33, 0x44]))],
        },
        OutputCase {
            name: "data-output-before-declarations",
            source: PLACED_BODY.replace(".region first", ".output \"data-only\",format=bin,sections=data\n.region first") + ".endmodule\n",
            explicit_bin: false,
            artifacts: vec![("data-only", Some(vec![0x33, 0x44]))],
        },
        OutputCase {
            name: "inactive-invalid-output",
            source: ".module main\n.cpu 68020\n.if 0\n.output \"inactive\",format=invalid,sections=missing\n.endif\n.byte $12\n.endmodule\n".into(),
            explicit_bin: false,
            artifacts: vec![],
        },
        OutputCase {
            name: "output-free-validation",
            source: ".module main\n.cpu 68020\n.byte $12,$34\n.endmodule\n".into(),
            explicit_bin: false,
            artifacts: vec![],
        },
    ]
}

fn scratch() -> PathBuf {
    let path = std::env::temp_dir().join(format!(
        "opforge-compact-cli-outputs-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&path).unwrap();
    path
}

fn rust_oracle(base: &Path, case: &OutputCase) -> Vec<Vec<u8>> {
    let dir = base.join(case.name);
    fs::create_dir(&dir).unwrap();
    let input = dir.join("entry.asm");
    fs::write(&input, &case.source).unwrap();
    let mut argv = vec![
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
    ];
    if case.explicit_bin {
        argv.extend([
            "--bin".into(),
            dir.join("explicit.bin").to_string_lossy().into_owned(),
        ]);
    }
    let cli = Cli::parse_from(argv);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.clone());
    run_with_validated_cli_with_context(&cli, &config)
        .expect("assemble actual source with Rust CLI");
    let outputs: Vec<_> = case
        .artifacts
        .iter()
        .map(|(name, expected)| {
            let bytes =
                fs::read(dir.join(name)).expect("read live Rust artifact with literal filename");
            if let Some(expected) = expected {
                assert_eq!(&bytes, expected, "{}", case.name);
            } else {
                assert_eq!(hunk::allocation(&bytes).unwrap().segments, 3);
            }
            bytes
        })
        .collect();
    fn files(dir: &Path, root: &Path, names: &mut Vec<String>) {
        for entry in fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                files(&path, root, names);
            } else {
                names.push(
                    path.strip_prefix(root)
                        .unwrap()
                        .to_string_lossy()
                        .into_owned(),
                );
            }
        }
    }
    let mut filenames = Vec::new();
    files(&dir, &dir, &mut filenames);
    filenames.sort();
    let mut expected: Vec<_> = case
        .artifacts
        .iter()
        .map(|(name, _)| name.to_string())
        .chain(std::iter::once("entry.asm".into()))
        .collect();
    expected.sort();
    assert_eq!(
        filenames, expected,
        "{}: exact Rust output inventory",
        case.name
    );
    outputs
}

#[test]
fn compact_cli_outputs_live_rust_oracles() {
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    for case in output_cases() {
        rust_oracle(&dir, &case);
    }
    // Output-free mode still parses and validates the actual source.
    let invalid = dir.join("invalid.asm");
    fs::write(
        &invalid,
        ".module main\n.cpu 68020\nunknown_instruction d0\n.endmodule\n",
    )
    .unwrap();
    let cli = Cli::parse_from(["opForge", invalid.to_str().unwrap(), "--cpu", "68020"]);
    let config = validate_cli(&cli).unwrap();
    assert!(run_with_validated_cli_with_context(&cli, &config).is_err());
}

#[test]
#[ignore = "fresh real-native C2 output proof; requires configured FS-UAE"]
fn compact_cli_source_outputs_and_validation() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let registry = engine::build_default_asm_registry();
    let build = build_native_packages(
        &registry,
        &dir.join("native"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into()]),
    )
    .unwrap();
    let image = super::compact_cli_input::assemble_cli(&root, &build);
    let selected = std::env::var("OPFORGE_CLI_OUTPUT_CASES").ok();
    let wanted = |name: &str| {
        selected
            .as_ref()
            .is_none_or(|names| names.split(',').any(|item| item == name))
    };
    let mut attempted = 0;
    let mut failures = Vec::new();
    let mut run = |name: &str,
                   source: &[u8],
                   explicit_bin: bool,
                   proof: OpforgeNativeCliProof<'_>,
                   exit: i32| {
        if !wanted(name) {
            return;
        }
        attempted += 1;
        let files = [OpforgeNativeCliGuestFile {
            relative_path: "entry.asm",
            bytes: source,
        }];
        let case = OpforgeNativeCliParityCase {
            name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source),
            command_template: Some(if explicit_bin {
                "--cpu 68020 Work:entry.asm --bin Work:explicit.bin"
            } else {
                "--cpu 68020 Work:entry.asm"
            }),
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
                if let Some(seconds) = runs[0].start_to_done_host_seconds {
                    eprintln!("{name}: fresh native completion, exit {exit}, host START/DONE observation {seconds:.3}s");
                } else {
                    eprintln!("{name}: fresh native completion, exit {exit}");
                }
            }
            Ok(FsUaeSmokeOutcome::Completed { runs }) => failures.push(format!(
                "{name}: invalid completion/exit; {} runs",
                runs.len()
            )),
            Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                failures.push(format!("{name}: skipped: {reason}"))
            }
            Err(error) => failures.push(format!("{name}: {error}")),
        }
    };
    for case in output_cases() {
        let oracle = rust_oracle(&dir, &case);
        let paths: Vec<_> = case
            .artifacts
            .iter()
            .map(|(name, _)| format!("Work/{name}"))
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
            "Work/entry.bin",
            "Work/entry.hunk",
            "Work/entry.lst",
            "Work/entry.hex",
            "Work/entry.srec",
            "Work/inactive",
        ];
        let proof = if artifacts.is_empty() {
            OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(&absent)
        } else {
            OpforgeNativeCliProof::ExactArtifacts(&artifacts)
        };
        run(
            case.name,
            case.source.as_bytes(),
            case.explicit_bin,
            proof,
            0,
        );
    }
    run(
        "output-free-invalid-source",
        b".module main\n.cpu 68020\nunknown_instruction d0\n.endmodule\n",
        false,
        OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        20,
    );
    for (name, source) in [
        ("reject-output-option", format!("{PLACED_BODY}.output \"unsupported\",format=bin,fill=$ff,sections=code,data\n.endmodule\n")),
        ("reject-unplaced-bin", format!("{HUNK_BODY}.output \"unplaced\",format=bin,sections=code,data\n.endmodule\n")),
        ("reject-different-hunk-selection", format!("{HUNK_BODY}.output \"first\",format=hunk,sections=code,data,bss\n.output \"second\",format=hunk,sections=code\n.endmodule\n")),
        ("source-output-write-failure", format!("{PLACED_BODY}.output \"entry.asm/image\",format=bin,sections=code,data\n.endmodule\n")),
    ] {
        let diagnostic = if name != "source-output-write-failure" { "binary source: unsupported or invalid input" } else { "compact CLI: output failed" };
        run(name, source.as_bytes(), false, OpforgeNativeCliProof::ExpectedFailureContaining(diagnostic), 20);
    }
    assert!(attempted > 0, "output selector must match a real case");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
