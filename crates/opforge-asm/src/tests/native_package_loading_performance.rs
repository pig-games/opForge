// SPDX-License-Identifier: GPL-3.0-or-later
//! Same-input P2 comparison. START/DONE host timing includes command execution;
//! it is not isolated package-load time. Optional instrumentation changes costs.
//! Borrowed embedded packages still copy their runtime prefix before assembly;
//! the embedded executable image contains the whole selected package.
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection, NativePackageBuild};
use clap::Parser;
use cli_core::{run_with_cli_with_context, run_with_validated_cli_with_context, validate_cli, Cli};
use serde_json::{json, Value};

fn assemble(root: &Path, build: &NativePackageBuild, instrumented: bool) -> Vec<u8> {
    let mut args = vec![
        "opForge".into(),
        build.cli_source_path.to_string_lossy().into_owned(),
    ];
    let mut roots: Vec<_> = fs::read_dir(root.join("native"))
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.is_dir())
        .collect();
    roots.sort();
    for path in roots {
        args.extend(["-M".into(), path.to_string_lossy().into_owned()]);
    }
    args.extend([
        "-I".into(),
        root.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    if instrumented {
        for define in ["OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY"] {
            args.extend(["--define".into(), define.into()]);
        }
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(build.output_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "build measured native CLI: {}; diagnostics: {:?}",
            error.summary(),
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("build measured native CLI: {error}")
        }
        cli_core::CliRunError::WarningsAsErrors { .. } => {
            panic!("build measured native CLI: warnings treated as errors")
        }
    });
    fs::read(build.output_dir.join("build/opforge_compact")).unwrap()
}

// HUNK_HEADER reservations measure linked static segments, not allocator overhead.
pub(crate) fn linked_static_bytes(image: &[u8]) -> u64 {
    let word = |offset: usize| u32::from_be_bytes(image[offset..offset + 4].try_into().unwrap());
    assert!(image.len() >= 20);
    assert_eq!(word(0), 0x3f3);
    assert_eq!(word(4), 0, "no resident names in measured load file");
    let count = word(8) as usize;
    assert!(count > 0 && count <= (image.len() - 20) / 4);
    assert_eq!(word(12), 0);
    assert_eq!(word(16) as usize, count - 1);
    (0..count)
        .map(|index| u64::from(word(20 + index * 4) & 0x3fff_ffff) * 4)
        .sum()
}

fn memory(bytes: &[u8]) -> Value {
    assert_eq!(bytes.len(), 2280, "current MEMD record size");
    let words: Vec<_> = bytes
        .chunks_exact(4)
        .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
        .collect();
    assert_eq!(words[0], 0x4d454d44);
    assert_eq!(words[1], 0, "terminal live allocations");
    assert_eq!(words[11], 0, "terminal cleanup allocations");
    assert_eq!(words[3], words[4], "allocation/free balance");
    assert_eq!(words[29], 0, "profiling errors");
    let seconds = |index| {
        let ticks = (u64::from(words[index]) << 32) | u64::from(words[index + 1]);
        (words[28] != 0).then(|| ticks as f64 / f64::from(words[28]))
    };
    let stages: Vec<_> = ["source_io_and_other", "package_setup", "tokenization", "binding_and_raw_records", "expression_preparation", "runtime_finalization", "module_discovery"]
        .iter().enumerate().map(|(index, name)| json!({"name": name, "seconds": seconds(30 + 2 * index), "calls": words[44 + index]})).collect();
    json!({"live_owned_bytes": words[1], "peak_owned_bytes": words[2],
        "total_allocated_bytes": words[3], "total_freed_bytes": words[4],
        "runtime_prefix_bytes": words[13], "packed_source_bytes": words[14],
        "source_bytes_read": words[15], "eclock_hz": words[28], "preparation_stages": stages})
}

pub(crate) fn workload() -> String {
    // Arithmetic, conditional branches, modules/imports and data share one input
    // and live Rust oracle across every storage mode and baseline executable.
    let mut source = String::from(".module constants\n.cpu m68020\n.org $1000\n.pub\nmask = $ff\nstep = 3\n.endmodule\n.module main\n.use constants as c\n");
    for index in 0..64 {
        source.push_str(&format!("block{index} .block\n move.l #({index}+1)*c.step,d0\n andi.l #c.mask,d0\n cmpi.l #127,d0\n bne.w done\n eori.l #$55,d0\ndone\n .word {index},({index}+2)*c.step\n .byte {index},c.mask\n .bend\n"));
    }
    source.push_str(" rts\n");
    // Keep each packed record within the existing native byte-sized line limit.
    // Every block is still referenced and the emitted pointer bytes are unchanged.
    for start in (0..64).step_by(8) {
        let names = (start..start + 8)
            .map(|index| format!("block{index}"))
            .collect::<Vec<_>>()
            .join(",");
        source.push_str(&format!(" .long {names}\n"));
    }
    source.push_str(".endmodule\n.end\n");
    source
}

#[test]
#[ignore = "same-input native performance comparison; requires configured FS-UAE"]
fn native_package_loading_performance() {
    let root = std::env::var_os("OPFORGE_COMPARE_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| Path::new(env!("CARGO_MANIFEST_DIR")).join("../.."))
        .canonicalize()
        .unwrap();
    let scratch = std::env::temp_dir().join(format!(
        "opforge-package-perf-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&scratch).unwrap();
    let _cleanup = EphemeralArtifactDir(scratch.clone());
    let registry = engine::build_default_asm_registry();
    let template = root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm");
    let external = build_native_packages(
        &registry,
        &scratch.join("external"),
        &template,
        &EmbedSelection::ExternalOnly,
    )
    .unwrap();
    let embedded = build_native_packages(
        &registry,
        &scratch.join("embedded"),
        &template,
        &EmbedSelection::Targets(vec!["m68020".into()]),
    )
    .unwrap();
    let external_cli = assemble(&root, &external, false);
    let embedded_cli = assemble(&root, &embedded, false);
    let target_name = crate::native_package_build::resolve_embeds(
        &registry,
        &EmbedSelection::Targets(vec!["m68020".into()]),
    )
    .unwrap()
    .into_iter()
    .next()
    .unwrap();
    // A frozen native tree carries its own capsule contract for before/after
    // measurements. This changes the test input, never production dispatch.
    let package_path = std::env::var_os("OPFORGE_COMPARE_PACKAGE")
        .map(PathBuf::from)
        .unwrap_or_else(|| external.output_dir.join("packages").join(&target_name));
    let package = fs::read(package_path).unwrap();
    let named_path = format!("build/packages/{target_name}");
    let source = workload();
    let input = scratch.join("source.asm");
    let output = scratch.join("oracle.bin");
    fs::write(&input, &source).unwrap();
    let cli = Cli::parse_from([
        "opForge",
        input.to_str().unwrap(),
        "--cpu",
        "m68020",
        "--bin",
        output.to_str().unwrap(),
    ]);
    run_with_cli_with_context(&cli).expect("fresh shared Rust oracle");
    let expected = fs::read(output).unwrap();
    assert!(
        expected.len() >= 1500,
        "benchmark blocks must survive emission"
    );
    let baseline = match (
        std::env::var_os("OPFORGE_PACKAGE_BASELINE_CLI"),
        std::env::var_os("OPFORGE_PACKAGE_BASELINE_PACKAGE"),
    ) {
        (Some(cli), Some(package)) => Some((
            fs::read(cli).expect("baseline CLI"),
            fs::read(package).expect("baseline package"),
        )),
        (None, None) => {
            eprintln!(
                "{}",
                json!({"comparison": "old_explicit", "status": "skipped", "reason": "set OPFORGE_PACKAGE_BASELINE_CLI and OPFORGE_PACKAGE_BASELINE_PACKAGE"})
            );
            None
        }
        _ => panic!("both baseline paths are required together"),
    };
    if let Some((_, package)) = &baseline {
        assert!(
            [
                b"BS16".as_slice(),
                b"BS17".as_slice(),
                b"BS18".as_slice(),
                b"BS19".as_slice(),
                b"BS24".as_slice(),
                b"BS25".as_slice(),
                b"BS26".as_slice(),
                b"BS30".as_slice()
            ]
            .contains(&package.get(..4).unwrap_or(&[])),
            "baseline must carry its own known frozen native contract"
        );
    }
    let rounds = std::env::var("OPFORGE_PACKAGE_PERF_ROUNDS")
        .map(|value| value.parse::<usize>().expect("integer rounds"))
        .unwrap_or(2);
    assert!((1..=8).contains(&rounds), "rounds must be 1..=8");
    let args = std::env::var(FS_UAE_ARGS_ENV).expect("OPFORGE_FS_UAE_ARGS required");
    let binary = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".into());
    let selected = std::env::var("OPFORGE_PACKAGE_PERF_CASE").ok();
    let mut attempted = 0;
    let mut failures = Vec::new();
    let mut measure = |name: &str,
                       round: usize,
                       image: &[u8],
                       package: &[u8],
                       command: &str,
                       package_path: Option<&str>,
                       instrumented: bool| {
        if selected.as_deref().is_some_and(|selected| selected != name) {
            return;
        }
        attempted += 1;
        let mut files = vec![OpforgeNativeCliGuestFile {
            relative_path: "source.asm",
            bytes: source.as_bytes(),
        }];
        if let Some(path) = package_path {
            files.push(OpforgeNativeCliGuestFile {
                relative_path: path,
                bytes: package,
            });
        }
        let defines: &[&str] = if instrumented {
            &["OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY"]
        } else {
            &[]
        };
        let case = OpforgeNativeCliParityCase {
            name,
            cpu_override: "68020",
            extra_assembly_defines: defines,
            source_override: Some(source.as_bytes()),
            command_template: Some(command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.bin",
                rust_oracle: &expected,
            },
        };
        match run_native_cli_parity_batch_cases(
            &root,
            &binary,
            &args,
            &[case],
            NativeCliParityExecutable::CompactCli,
            Some(image),
        ) {
            Ok(FsUaeSmokeOutcome::Completed { runs }) => {
                assert_eq!(runs.len(), 1);
                let run = &runs[0];
                assert!(run.protocol_completed && run.success);
                assert_eq!(run.exit_code, Some(0));
                assert_eq!(run.verified_output.as_deref(), Some(expected.as_slice()));
                let allocation = if instrumented {
                    Some(memory(
                        run.captured_artifacts
                            .get(&PathBuf::from("Work/memory.bin"))
                            .expect("fresh instrumented memory.bin"),
                    ))
                } else {
                    None
                };
                let static_bytes = linked_static_bytes(image);
                let accounted_peak = allocation
                    .as_ref()
                    .map(|record| static_bytes + record["peak_owned_bytes"].as_u64().unwrap());
                eprintln!(
                    "{}",
                    json!({"comparison": name, "round": round, "start_to_done_host_seconds": run.start_to_done_host_seconds,
                    "native_image_digest": run.native_image_digest, "image_bytes": image.len(), "linked_static_reserved_bytes": static_bytes, "linked_static_plus_peak_owned_bytes": accounted_peak, "package_bytes": package.len(), "output_bytes": expected.len(), "source_bytes": source.len(),
                    "instrumented": instrumented, "allocation": allocation, "fresh_completion_exact": true})
                );
            }
            Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                failures.push(format!("{name}: skipped: {reason}"))
            }
            Err(error) => {
                eprintln!(
                    "{}",
                    json!({"comparison": name, "round": round, "error": error.to_string()})
                );
                failures.push(format!("{name}: {error}"));
            }
        }
    };
    for round in 1..=rounds {
        if let Some((image, package)) = &baseline {
            measure(
                "old_explicit",
                round,
                image,
                package,
                "Work:package.bin Work:source.asm Work:output.bin",
                Some("package.bin"),
                false,
            );
        }
        measure(
            "new_explicit",
            round,
            &external_cli,
            &package,
            "--runtime-package Work:package.bin -i Work:source.asm --bin Work:output.bin",
            Some("package.bin"),
            false,
        );
        measure(
            "new_named_external",
            round,
            &external_cli,
            &package,
            "--cpu m68020 -i Work:source.asm --bin Work:output.bin",
            Some(&named_path),
            false,
        );
        measure(
            "new_named_embedded",
            round,
            &embedded_cli,
            &package,
            "--cpu m68020 -i Work:source.asm --bin Work:output.bin",
            None,
            false,
        );
    }
    if std::env::var("OPFORGE_PACKAGE_PERF_MEMORY").as_deref() == Ok("1") {
        let image = assemble(&root, &external, true);
        measure(
            "new_named_external_memory",
            1,
            &image,
            &package,
            "--cpu m68020 -i Work:source.asm --bin Work:output.bin",
            Some(&named_path),
            true,
        );
        let image = assemble(&root, &embedded, true);
        measure(
            "new_named_embedded_memory",
            1,
            &image,
            &package,
            "--cpu m68020 -i Work:source.asm --bin Work:output.bin",
            None,
            true,
        );
    }
    assert!(attempted > 0, "selected performance case must exist");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    eprintln!(
        "{}",
        json!({"metric_scope": "same source and fresh oracle; host-observed START/DONE command interval; emulator startup is not separately measured", "embedded_scope": "whole package remains in executable image; runtime prefix still copied", "memory_scope": "Hunk segment reservations plus tracked owned peak excludes OS, loader overhead and untracked allocations; instrumented runs measured separately", "rounds": rounds})
    );
}
