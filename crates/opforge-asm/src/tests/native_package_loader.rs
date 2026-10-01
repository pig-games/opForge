// SPDX-License-Identifier: GPL-3.0-or-later
//! Fresh native package selection proof; the host builds every input for this run.
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection, NativePackageBuild};
use clap::Parser;
use cli_core::{run_with_cli_with_context, run_with_validated_cli_with_context, validate_cli, Cli};

fn assemble_cli(root: &Path, build: &NativePackageBuild) -> Vec<u8> {
    let mut args = vec![
        "opForge".to_string(),
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
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(build.output_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("assemble configured CLI");
    fs::read(build.output_dir.join("build/opforge_compact")).unwrap()
}
fn oracle(scratch: &Path, cpu: &str, source: &[u8]) -> Vec<u8> {
    let input = scratch.join(format!("{cpu}.asm"));
    let output = scratch.join(format!("{cpu}.bin"));
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from([
        "opForge",
        input.to_str().unwrap(),
        "--cpu",
        cpu,
        "--bin",
        output.to_str().unwrap(),
    ]);
    run_with_cli_with_context(&cli).expect("fresh Rust oracle");
    fs::read(output).unwrap()
}

#[test]
#[ignore = "real FS-UAE proof; requires OPFORGE_FS_UAE_ARGS and configured emulator"]
fn native_package_loader_selection_and_rejections() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let scratch = std::env::temp_dir().join(format!(
        "opforge-package-loader-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
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
        &EmbedSelection::Targets(vec!["m6502".into()]),
    )
    .unwrap();
    let external_cli = assemble_cli(&root, &external);
    let embedded_cli = assemble_cli(&root, &embedded);
    let source = b".org $1000\nstart\n lda #$2a\n sta $2000\n rts\n";
    let other_source = b".org $1000\nstart\n moveq #42,d0\n add.l d0,d0\n bra.w done\ndone\n rts\n";
    let expected = oracle(&scratch, "m6502", source);
    let other_expected = oracle(&scratch, "m68020", other_source);
    let target_name = crate::native_package_build::resolve_embeds(
        &registry,
        &EmbedSelection::Targets(vec!["m6502".into()]),
    )
    .unwrap()
    .into_iter()
    .next()
    .unwrap();
    let other_name = crate::native_package_build::resolve_embeds(
        &registry,
        &EmbedSelection::Targets(vec!["m68020".into()]),
    )
    .unwrap()
    .into_iter()
    .next()
    .unwrap();
    let target = fs::read(external.output_dir.join("packages").join(&target_name)).unwrap();
    let other = fs::read(external.output_dir.join("packages").join(&other_name)).unwrap();
    let target_path = format!("build/packages/{target_name}");
    let other_path = format!("build/packages/{other_name}");
    let mut bad_magic = target.clone();
    bad_magic[..4].copy_from_slice(b"BS11");
    let mut malformed_offset = target.clone();
    malformed_offset[124..128].copy_from_slice(&((target.len() as u32) + 2).to_be_bytes());
    let truncated = &target[..target.len() - 1];
    // Corrupt only the embedded payload, then offer a valid external replacement.
    // A matching embedded package must fail closed rather than fall back.
    let mut invalid_embedded_cli = embedded_cli.clone();
    let payload = invalid_embedded_cli
        .windows(target.len())
        .position(|bytes| bytes == target)
        .unwrap();
    invalid_embedded_cli[payload..payload + 4].copy_from_slice(b"BS11");
    let args = std::env::var(FS_UAE_ARGS_ENV).expect("OPFORGE_FS_UAE_ARGS required");
    let binary = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".into());
    // The runner mounts the executable in Work:build: its default PROGDIR:packages
    // therefore exercises the same sibling layout as the distributed executable.
    let cases: [(
        &str,
        &[u8],
        &[u8],
        &str,
        Option<(&str, &[u8])>,
        Option<&[u8]>,
    ); 10] = [
        (
            "named-alias-default",
            &external_cli,
            source,
            "--cpu 6502 Work:source.asm Work:output.bin",
            Some((&target_path, &target)),
            Some(&expected),
        ),
        (
            "embedded-without-file",
            &embedded_cli,
            source,
            "--cpu m6502 Work:source.asm Work:output.bin",
            None,
            Some(&expected),
        ),
        (
            "embedded-external-fallback",
            &embedded_cli,
            other_source,
            "--cpu 68020 Work:source.asm Work:output.bin -d motorola68k -P Work:build/packages",
            Some((&other_path, &other)),
            Some(&other_expected),
        ),
        (
            "embedded-precedes-invalid-file",
            &embedded_cli,
            source,
            "--cpu m6502 Work:source.asm Work:output.bin",
            Some((&target_path, &bad_magic)),
            Some(&expected),
        ),
        (
            "invalid-embedded-does-not-fall-back",
            &invalid_embedded_cli,
            source,
            "--cpu m6502 Work:source.asm Work:output.bin",
            Some((&target_path, &target)),
            None,
        ),
        (
            "wrong-target-payload",
            &external_cli,
            source,
            "--cpu m6502 Work:source.asm Work:output.bin",
            Some((&target_path, &other)),
            None,
        ),
        (
            "superseded-BS11-contract",
            &external_cli,
            source,
            "--cpu m6502 Work:source.asm Work:output.bin",
            Some((&target_path, &bad_magic)),
            None,
        ),
        (
            "malformed-target-offset",
            &external_cli,
            source,
            "--cpu m6502 Work:source.asm Work:output.bin",
            Some((&target_path, &malformed_offset)),
            None,
        ),
        (
            "truncated-package",
            &external_cli,
            source,
            "--cpu m6502 Work:source.asm Work:output.bin",
            Some((&target_path, truncated)),
            None,
        ),
        (
            "missing-package",
            &external_cli,
            source,
            "--cpu m6502 Work:source.asm Work:output.bin",
            None,
            None,
        ),
    ];
    let selected = std::env::var("OPFORGE_PACKAGE_LOADER_CASE").ok();
    let mut attempted = 0;
    let mut failures = Vec::new();
    for (name, executable, source, command, package, expected) in cases {
        if selected.as_deref().is_some_and(|selected| selected != name) {
            continue;
        }
        attempted += 1;
        let mut files = vec![OpforgeNativeCliGuestFile {
            relative_path: "source.asm",
            bytes: source,
        }];
        if let Some((relative_path, bytes)) = package {
            files.push(OpforgeNativeCliGuestFile {
                relative_path,
                bytes,
            });
        }
        let case = OpforgeNativeCliParityCase {
            name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source),
            command_template: Some(command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof: expected.map_or(
                OpforgeNativeCliProof::ExpectedFailureContaining(
                    "package: missing, invalid or incompatible runtime package",
                ),
                |rust_oracle| OpforgeNativeCliProof::ExactArtifact {
                    relative_path: "Work/output.bin",
                    rust_oracle,
                },
            ),
        };
        match run_native_cli_parity_batch_cases(
            &root,
            &binary,
            &args,
            &[case],
            NativeCliParityExecutable::CompactCli,
            Some(executable),
        ) {
            Ok(FsUaeSmokeOutcome::Completed { runs }) => {
                assert_eq!(runs.len(), 1, "one fresh case result");
                for run in runs {
                    assert!(run.protocol_completed, "{name}: fresh completion required");
                    if expected.is_some() {
                        assert!(run.success, "{name}: positive assembly failed");
                        assert_eq!(run.exit_code, Some(0), "{name}: positive exit");
                    } else {
                        assert!(
                            run.exit_code.is_some_and(|code| code != 0),
                            "{name}: negative exit"
                        );
                    }
                    println!(
                        "PACKAGE_LOADER_METRIC {}",
                        serde_json::json!({
                            "case": name,
                            "seconds": run.start_to_done_host_seconds,
                            "image_bytes": executable.len(),
                            "hunk_allocated_bytes": hunk::allocation(executable).expect("valid configured Hunk").total(),
                            "source_bytes": source.len(),
                            "output_bytes": expected.map(|bytes| bytes.len()),
                            "external_package_bytes": package.map(|(_,bytes)|bytes.len()).unwrap_or(0),
                            "selected_package_bytes": if source == other_source {other.len()} else {target.len()},
                            "protocol_completed": run.protocol_completed,
                            "exit_code": run.exit_code,
                            "scope": "single fresh release observation; no broad performance claim",
                        })
                    );
                }
            }
            Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                failures.push(format!("{name}: skipped: {reason}"))
            }
            Err(error) => failures.push(format!("{name}: {error}")),
        }
    }
    assert!(attempted > 0, "selected package case must exist");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
