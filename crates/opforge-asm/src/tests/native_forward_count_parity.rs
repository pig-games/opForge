// SPDX-License-Identifier: GPL-3.0-or-later
//! Immutable counted-loop dependencies share no CPU-specific semantics.
use super::*;
use crate::binary_source_experiment::prepare_package;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli, CliRunError};
use vm::runtime_model_core::RuntimeModelCore;

struct CountCase<'a> {
    name: &'static str,
    cpu: &'static str,
    source: &'static str,
    expected: Option<&'a [u8]>,
}

fn cases() -> Vec<CountCase<'static>> {
    vec![
        CountCase {
            name: "forward-m68k",
            cpu: "68020",
            source: ".for target\n.byte 1\n.endfor\ntarget = 2\n",
            expected: Some(&[1, 1]),
        },
        CountCase {
            name: "forward-mos",
            cpu: "6502",
            source: ".for target\n.byte 1\n.endfor\ntarget = 2\n",
            expected: Some(&[1, 1]),
        },
        CountCase {
            name: "forward-graph",
            cpu: "68020",
            source:
                ".for total+1\n.byte $12\n.endfor\ntotal = base+1\nbase .const seed+1\nseed = 0\n",
            expected: Some(&[0x12; 3]),
        },
        CountCase {
            name: "forward-nested",
            cpu: "68020",
            source: ".for outer\n.for inner\n.byte $34\n.endfor\n.endfor\nouter = 2\ninner = 3\n",
            expected: Some(&[0x34; 6]),
        },
        CountCase {
            name: "mutable-snapshot",
            cpu: "68020",
            source:
                "variable := 2\nsaved = variable\nvariable := 4\n.for saved\n.byte $56\n.endfor\n",
            expected: Some(&[0x56; 2]),
        },
        CountCase {
            name: "mutable-m68k",
            cpu: "68020",
            source: "n:=1\n moveq #n,d0\nn := -17\n.long n/7\nsaved = n\nn:=4\n.long saved,n\n",
            expected: Some(&[
                0x70, 1, 0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xef, 0, 0, 0, 4,
            ]),
        },
        CountCase {
            name: "mutable-mos",
            cpu: "6502",
            source: "n:=1\n lda #n\nn := 2\n lda #n\nn .set 3\n.byte n\nn: .var 4\n.byte n\n",
            expected: Some(&[0xa9, 1, 0xa9, 2, 3, 4]),
        },
        CountCase {
            name: "readonly-assignment",
            cpu: "68020",
            source: "n = 1\nn := 2\n.byte n\n",
            expected: None,
        },
        CountCase {
            name: "inactive-definition",
            cpu: "68020",
            source:
                ".if 0\ntarget = missing\n.endif\n.for target\n.byte $78\n.endfor\ntarget = 2\n",
            expected: Some(&[0x78; 2]),
        },
        CountCase {
            name: "contextual-count",
            cpu: "68020",
            source: ".for target\n.byte 1\n.endfor\ntarget = $+2\n",
            expected: None,
        },
    ]
}

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "opforge-forward-count-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&dir).unwrap();
    dir
}

fn oracle(dir: &Path, case: &CountCase<'_>) -> Option<Vec<u8>> {
    let input = dir.join("entry.asm");
    let output = dir.join("image.bin");
    fs::write(&input, case.source).unwrap();
    let cli = Cli::parse_from([
        "opForge".into(),
        input.to_string_lossy().into_owned(),
        "--cpu".into(),
        case.cpu.into(),
        "--bin".into(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).unwrap();
    let result = run_with_validated_cli_with_context(&cli, &config);
    if let Some(expected) = case.expected {
        result.unwrap();
        let bytes = fs::read(output).unwrap();
        assert_eq!(bytes, expected, "{}", case.name);
        Some(bytes)
    } else {
        let Err(CliRunError::Assembler { error, .. }) = result else {
            panic!("{} must reject during assembly", case.name);
        };
        if case.name == "contextual-count" {
            assert!(format!("{:?}", error.diagnostics())
                .contains("loop iteration count changed between passes"));
        }
        if case.name == "readonly-assignment" {
            assert!(
                format!("{:?}", error.diagnostics()).contains("symbol has already been defined")
            );
        }
        None
    }
}

#[test]
fn native_forward_count_rust_oracles() {
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    for case in cases() {
        oracle(&dir, &case);
    }
}

#[test]
#[ignore = "comparative host timing on an unchanged literal-count workload"]
fn native_forward_count_host_timing() {
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let expected = [0x12, 0x34].repeat(4096);
    let case = CountCase {
        name: "literal-nested-control",
        cpu: "68020",
        source: ".for 4096\n.for 1\n.byte $12,$34\n.endfor\n.endfor\n",
        expected: Some(&expected),
    };
    // Warm registry/package caches before five identical CLI assemblies.
    oracle(&dir, &case);
    for sample in 0..5 {
        let start = Instant::now();
        oracle(&dir, &case);
        eprintln!(
            "COUNT_HOST_TIMING sample={sample} seconds={:.9} source_fnv={:016x} output_bytes={}",
            start.elapsed().as_secs_f64(),
            fnv1a64_update(0xcbf29ce484222325, case.source.as_bytes()),
            expected.len()
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; live immutable-loop differential cases"]
fn native_forward_count_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let image = if let Some(path) = std::env::var_os("OPFORGE_COUNT_IMAGE") {
        fs::read(path).unwrap()
    } else {
        let build = build_native_packages(
            &engine::build_default_asm_registry(),
            &dir.join("native"),
            &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
            &EmbedSelection::Targets(vec!["68020".into()]),
        )
        .unwrap();
        super::compact_cli_input::assemble_cli(&root, &build)
    };
    if let Some(path) = std::env::var_os("OPFORGE_COUNT_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
    let core = RuntimeModelCore::from_registry(&engine::build_default_asm_registry()).unwrap();
    let mos = prepare_package(&core, &core.resolve_pipeline("6502", None).unwrap()).unwrap();
    let selected = std::env::var("OPFORGE_COUNT_CASES").ok();
    let cases = cases();
    if let Some(names) = &selected {
        assert!(names
            .split(',')
            .all(|name| cases.iter().any(|case| case.name == name)));
    }
    let mut failures = Vec::new();
    for case in cases.iter().filter(|case| {
        selected
            .as_ref()
            .is_none_or(|names| names.split(',').any(|name| name == case.name))
    }) {
        let oracle = oracle(&dir, &case);
        let mut files = vec![OpforgeNativeCliGuestFile {
            relative_path: "entry.asm",
            bytes: case.source.as_bytes(),
        }];
        let command = if case.cpu == "6502" {
            files.push(OpforgeNativeCliGuestFile {
                relative_path: "runtime.bin",
                bytes: &mos,
            });
            "Work:entry.asm --runtime-package Work:runtime.bin --bin Work:image.bin".into()
        } else {
            format!("--cpu {} Work:entry.asm --bin Work:image.bin", case.cpu)
        };
        let artifacts = oracle.as_ref().map(|bytes| {
            [OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/image.bin",
                rust_oracle: bytes,
            }]
        });
        let native = OpforgeNativeCliParityCase {
            name: case.name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(case.source.as_bytes()),
            command_template: Some(&command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof: artifacts.as_ref().map_or(
                OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
                |artifacts| OpforgeNativeCliProof::ExactArtifacts(artifacts),
            ),
        };
        match run_prebuilt_compact_cli_case_from_env(&root, &native, &image) {
            Ok(FsUaeSmokeOutcome::Completed { runs })
                if runs.len() == 1
                    && runs[0].protocol_completed
                    && runs[0].exit_code == Some(if oracle.is_some() { 0 } else { 20 }) =>
            {
                eprintln!("COUNT_RESULT name={} source_fnv={:016x} image_fnv={:016x} exit={:?} seconds={:?} proof_verified=true", case.name,
                    fnv1a64_update(0xcbf29ce484222325, case.source.as_bytes()),
                    fnv1a64_update(0xcbf29ce484222325, &image), runs[0].exit_code, runs[0].start_to_done_host_seconds);
            }
            Ok(FsUaeSmokeOutcome::Completed { runs }) => failures.push(format!(
                "{}: invalid completion ({} runs)",
                case.name,
                runs.len()
            )),
            Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                failures.push(format!("{}: skipped: {reason}", case.name))
            }
            Err(error) => failures.push(format!("{}: {error}", case.name)),
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
