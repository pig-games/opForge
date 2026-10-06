// SPDX-License-Identifier: GPL-3.0-or-later
//! Deferred region validation and initialized-byte provenance share no CPU rules.
use super::*;
use crate::binary_source_experiment::prepare_package;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli, CliRunError};
use vm::runtime_model_core::RuntimeModelCore;

struct LayoutCase {
    name: String,
    source: String,
    outputs: &'static [&'static str],
    unknown_region: bool,
}

fn cases() -> Vec<LayoutCase> {
    let mut cases = vec![LayoutCase {
        name: "unknown-region".into(),
        source: include_str!(
            "../../../../examples/opcore/linker_regions_phase6_unknown_region.asm"
        )
        .into(),
        outputs: &["image.bin"],
        unknown_region: true,
    }];
    for (name, source, outputs) in [
        ("align-original-hex", include_str!("../../../../examples/opcore/align_simple.asm").to_string(), &["image.hex"][..]),
        ("align-flat-records", ".module main\n.cpu 68020\n.org $1000\n.byte 1\n.align 16\n.align 16\n.byte 2\n.endmodule\n".into(), &["image.bin", "image.hex", "image.srec"][..]),
        ("align-mos-records", ".module main\n.cpu 6502\n.org $1000\n.byte 1\n.align 4\n.byte 2\n.endmodule\n".into(), &["image.bin", "image.hex", "image.srec"][..]),
        ("forward-region-placed-records", ".module main\n.cpu 68020\n.section code,kind=code\n.byte 1\n.align 4\n.byte 2\n.endsection\n.place code in ram\n.region ram,$1000,$10ff\n.endmodule\n".into(), &["image.bin", "image.hex", "image.srec"][..]),
        ("zero-align-outside-section", ".module main\n.cpu 68020\n.region ram,$1000,$10ff\n.align 4\n.section code,kind=code\n.byte 1,2,3,4\n.endsection\n.align 4\n.place code in ram\n.endmodule\n".into(), &["image.bin"][..]),
        ("inactive-unknown-region", ".module main\n.cpu 68020\n.if 0\n.place code in nowhere\n.endif\n.byte 1\n.endmodule\n".into(), &["image.bin"][..]),
        ("align-hunk-bss", ".module main\n.cpu 68020\n.section code,kind=code\n.byte 1\n.align 4\n.byte 2\n.endsection\n.section data,kind=data\n.byte 3\n.align 8\n.byte 4\n.endsection\n.section storage,kind=bss\n.res byte,1\n.align 16\n.res byte,2\n.endsection\n.output \"image.hunk\",format=hunk,sections=code,data,storage\n.endmodule\n".into(), &["image.hunk"][..]),
    ] {
        cases.push(LayoutCase { name: name.into(), source, outputs, unknown_region: false });
    }
    // Same valid Bin output before and after both fixes. Exercise preparation,
    // placement and repeated padding with real instructions; no record writer.
    let body = ".byte 1\n.align 16\n moveq #42,d0\n addq.l #1,d0\n.byte $11,$22\n".repeat(256);
    cases.push(LayoutCase {
        name: "aligned-control".into(),
        source: format!(".module main\n.cpu 68020\n.region ram,$1000,$ffff\n.section code,kind=code\n{body}.endsection\n.place code in ram\n.endmodule\n"),
        outputs: &["image.bin"],
        unknown_region: false,
    });
    // Native currently accepts one CLI output kind per invocation.
    cases
        .into_iter()
        .flat_map(|case| {
            if case.outputs.len() != 3 {
                return vec![case];
            }
            [
                (&["image.bin"][..], "bin"),
                (&["image.hex"][..], "hex"),
                (&["image.srec"][..], "srec"),
            ]
            .into_iter()
            .map(|(outputs, suffix)| LayoutCase {
                name: format!("{}-{suffix}", case.name),
                source: case.source.clone(),
                outputs,
                unknown_region: false,
            })
            .collect()
        })
        .collect()
}

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "opforge-layout-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&dir).unwrap();
    dir
}

fn oracle(dir: &Path, case: &LayoutCase) -> Vec<Vec<u8>> {
    let dir = dir.join(&case.name);
    fs::create_dir(&dir).unwrap();
    let input = dir.join("entry.asm");
    fs::write(&input, &case.source).unwrap();
    let mut args = vec!["opForge".into(), input.to_string_lossy().into_owned()];
    // The original fixtures deliberately omit .cpu; choose the same package
    // for both executors. Explicit CPU directives remain authoritative.
    args.extend(["--cpu".into(), "68020".into()]);
    for output in case.outputs.iter().filter(|name| !name.ends_with("hunk")) {
        let flag = match output.rsplit('.').next().unwrap() {
            "bin" => "--bin",
            "hex" => "--hex",
            "srec" => "--srec",
            _ => unreachable!(),
        };
        args.extend([flag.into(), dir.join(output).to_string_lossy().into_owned()]);
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.clone());
    let result = run_with_validated_cli_with_context(&cli, &config);
    if case.unknown_region {
        let Err(CliRunError::Assembler { error, .. }) = result else {
            panic!("unknown region must fail in the live Rust oracle");
        };
        assert!(format!("{:?}", error.diagnostics())
            .contains("Unknown region in placement directive: nowhere"));
        return vec![];
    }
    result.unwrap();
    case.outputs
        .iter()
        .map(|name| fs::read(dir.join(name)).unwrap())
        .collect()
}

#[test]
fn native_layout_rust_oracles() {
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    for case in cases() {
        let outputs = oracle(&dir, &case);
        if case.name == "align-flat-records-bin" {
            assert_eq!(outputs[0], [&[1][..], &[0; 15][..], &[2][..]].concat());
        }
        if case.name == "align-flat-records-hex" {
            let hex = String::from_utf8(outputs[0].clone()).unwrap();
            assert!(hex.contains(":0110000001EE\n:0110100002DD\n"));
        }
    }
}

#[test]
#[ignore = "fresh native layout and comparative timing; requires configured FS-UAE"]
fn native_layout_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let image = if let Some(path) = std::env::var_os("OPFORGE_LAYOUT_IMAGE") {
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
    if let Some(path) = std::env::var_os("OPFORGE_LAYOUT_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
    let selected = std::env::var("OPFORGE_LAYOUT_CASES").ok();
    let cases = cases();
    if let Some(names) = &selected {
        assert!(names
            .split(',')
            .all(|name| cases.iter().any(|case| case.name == name)));
    }
    let core = RuntimeModelCore::from_registry(&engine::build_default_asm_registry()).unwrap();
    let mos = prepare_package(&core, &core.resolve_pipeline("6502", None).unwrap()).unwrap();
    let mut failures = Vec::new();
    for case in cases.iter().filter(|case| {
        selected
            .as_ref()
            .is_none_or(|names| names.split(',').any(|name| name == case.name))
    }) {
        let oracle = oracle(&dir, case);
        let mut files = vec![OpforgeNativeCliGuestFile {
            relative_path: "entry.asm",
            bytes: case.source.as_bytes(),
        }];
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
        let cpu = if case.name.starts_with("align-mos") {
            "6502"
        } else {
            "68020"
        };
        let mut command = format!("--cpu {cpu} Work:entry.asm");
        if cpu == "6502" {
            files.push(OpforgeNativeCliGuestFile {
                relative_path: "runtime.bin",
                bytes: &mos,
            });
            // An explicit runtime package supplies the initial target; --cpu
            // conflicts with it. The source still selects its declared CPU.
            command = "Work:entry.asm --runtime-package Work:runtime.bin".into();
        }
        for output in case.outputs.iter().filter(|name| !name.ends_with("hunk")) {
            command.push_str(&format!(
                " --{} Work:{output}",
                output.rsplit('.').next().unwrap()
            ));
        }
        let native = OpforgeNativeCliParityCase {
            name: &case.name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(case.source.as_bytes()),
            command_template: Some(&command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof: if case.unknown_region {
                OpforgeNativeCliProof::ExpectedFailureWithDiagnostic
            } else {
                OpforgeNativeCliProof::ExactArtifacts(&artifacts)
            },
        };
        let result = run_prebuilt_compact_cli_case_from_env(&root, &native, &image);
        match result {
            Ok(FsUaeSmokeOutcome::Completed { runs })
                if runs.len() == 1
                    && runs[0].protocol_completed
                    && runs[0].exit_code == Some(if case.unknown_region { 20 } else { 0 }) =>
            {
                eprintln!("LAYOUT_RESULT name={} source_fnv={:016x} source_bytes={} image_fnv={:016x} image_bytes={} exit={:?} seconds={:?} proof_verified=true", case.name, fnv1a64(case.source.as_bytes()), case.source.len(), fnv1a64(&image), image.len(), runs[0].exit_code, runs[0].start_to_done_host_seconds);
            }
            Ok(FsUaeSmokeOutcome::Completed { runs }) => failures.push(format!(
                "{}: invalid completion, {} runs",
                case.name,
                runs.len()
            )),
            Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                failures.push(format!("{}: skipped: {reason}", case.name))
            }
            Err(error) => {
                eprintln!("LAYOUT_FAILURE {}: {error}", case.name);
                failures.push(format!("{}: {error}", case.name));
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

fn fnv1a64(bytes: &[u8]) -> u64 {
    fnv1a64_update(0xcbf29ce484222325, bytes)
}
