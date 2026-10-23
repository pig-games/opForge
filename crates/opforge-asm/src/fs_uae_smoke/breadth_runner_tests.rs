// SPDX-License-Identifier: GPL-3.0-or-later
use super::*;

#[test]
fn package_preparation_uses_fresh_output_under_existing_scratch() {
    use crate::native_package_build::{build_native_packages, EmbedSelection};
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    let scratch = scratch();
    let output = scratch.0.join("native");
    assert!(scratch.0.is_dir());
    assert!(!output.exists());
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &output,
        &root.join("native/motorola68000/amigaos/main.asm"),
        &EmbedSelection::Targets(vec!["68020".into()]),
    )
    .expect("generate current packages under existing scratch parent");
    assert!(!capture_artifact_files(&build.output_dir.join("packages"))
        .unwrap()
        .is_empty());
    assert!(build.output_dir.join("catalog.i").is_file());
}

#[test]
#[ignore = "requires a frozen compact image and real FS-UAE; correctness only"]
fn native_compact_breadth_correctness() {
    use crate::native_package_build::{build_native_packages, EmbedSelection};
    use clap::Parser;
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    let scratch = scratch();
    let image = fs::read(
        std::env::var_os("OPFORGE_CORPUS_IMAGE")
            .expect("OPFORGE_CORPUS_IMAGE must identify the frozen executable"),
    )
    .unwrap();
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &scratch.0.join("native"),
        &root.join("native/motorola68000/amigaos/main.asm"),
        &EmbedSelection::Targets(vec!["68020".into()]),
    )
    .unwrap();
    let package_files = capture_artifact_files(&build.output_dir.join("packages")).unwrap();
    let sources = [
        ".module main\n.cpu 68020\n.org 0\n.byte 17\n.endmodule\n",
        ".module main\n.cpu 68020\n.org 0\n.word $1234\n.byte 34\n.endmodule\n",
    ];
    let mut oracles = Vec::new();
    for (index, source) in sources.iter().enumerate() {
        let dir = scratch.0.join(format!("oracle-{index}"));
        stage_guest_input_bytes(&dir, "entry.asm", source.as_bytes()).unwrap();
        let cli = cli_core::Cli::parse_from([
            "opForge".to_owned(),
            dir.join("entry.asm").to_string_lossy().into_owned(),
            "--hex".to_owned(),
            dir.join("audit.hex").to_string_lossy().into_owned(),
        ]);
        cli_core::run_with_cli_with_context(&cli).expect("live Rust oracle");
        oracles.push(fs::read(dir.join("audit.hex")).unwrap());
    }
    let owned_files: Vec<Vec<(String, Vec<u8>)>> = sources
        .iter()
        .map(|source| {
            let mut files = vec![("entry.asm".into(), source.as_bytes().to_vec())];
            files.extend(
                package_files
                    .iter()
                    .map(|(path, bytes)| (format!("packages/{}", path.display()), bytes.clone())),
            );
            files
        })
        .collect();
    let guest_files: Vec<Vec<_>> = owned_files
        .iter()
        .map(|files| {
            files
                .iter()
                .map(|(path, bytes)| OpforgeNativeCliGuestFile {
                    relative_path: path,
                    bytes,
                })
                .collect()
        })
        .collect();
    let cases: Vec<_> = sources
        .iter()
        .enumerate()
        .map(|(index, source)| OpforgeNativeCliParityCase {
            name: "shared-core-byte-word",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source.as_bytes()),
            command_template: Some(
                "--cpu 68020 -P Work:packages Work:entry.asm --hex Work:audit.hex",
            ),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &guest_files[index],
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/audit.hex",
                rust_oracle: &oracles[index],
            },
        })
        .collect();
    let results = run_prebuilt_compact_cli_breadth_from_env(root, &cases, &image, 2).unwrap();
    assert_eq!(results.len(), 2);
    for result in results {
        assert!(result.error.is_none(), "{:?}", result.error);
        let run = result.run.expect("fresh completed case");
        assert!(run.protocol_completed && run.success);
        assert_eq!(
            run.native_image_digest,
            Some(opforge_self_host_package_digest(&image))
        );
        assert!(run.start_to_done_host_seconds.is_none());
    }
}

#[test]
#[should_panic(expected = "batch size must be positive")]
fn recovery_rejects_zero_batch_size() {
    recover_cases::<()>(1, 0, |_| unreachable!(), |_| ());
}

#[test]
fn staging_removes_input_supplied_output_evidence() {
    let scratch = scratch();
    let files = [OpforgeNativeCliGuestFile {
        relative_path: "audit.hex",
        bytes: b"oracle",
    }];
    let case = case(&files, b"oracle");
    let paths = stage_case(&scratch.0, 0, "fresh", &case, b"image").unwrap();
    assert!(!paths.artifact_dir.join("audit.hex").exists());
    assert!(
        collect_case(&case, &paths, &scratch.0, Path::new("image"), b"image")
            .unwrap()
            .is_none()
    );
}

#[test]
fn interruption_retains_completed_prefix_and_retries_only_unfinished_once() {
    let mut invocations = vec![];
    let results = recover_cases(
        5,
        4,
        |indices| {
            invocations.push(indices.to_vec());
            match invocations.len() {
                1 => Attempt {
                    completed: vec![(0, Ok(0)), (1, Err("proof mismatch".into()))],
                    interruption: "crash at 2".into(),
                },
                2 => Attempt {
                    completed: vec![],
                    interruption: "crash at 2 again".into(),
                },
                3 => Attempt {
                    completed: indices.iter().map(|&i| (i, Ok(i))).collect(),
                    interruption: String::new(),
                },
                _ => panic!("completed cases must not run again"),
            }
        },
        Err::<usize, String>,
    );
    assert_eq!(invocations, vec![vec![0, 1, 2, 3], vec![2], vec![3, 4]]);
    assert_eq!(results[0], Ok(0));
    assert_eq!(results[1], Err("proof mismatch".into()));
    assert!(results[2].as_ref().unwrap_err().contains("isolated retry"));
    assert_eq!(results[3..], [Ok(3), Ok(4)]);
}

#[test]
fn interruption_before_start_is_bounded_and_every_later_case_is_attempted() {
    let mut invocations = vec![];
    let results = recover_cases(
        3,
        3,
        |indices| {
            invocations.push(indices.to_vec());
            Attempt {
                completed: vec![],
                interruption: "boot timeout".into(),
            }
        },
        |error| error,
    );
    assert_eq!(
        invocations,
        vec![
            vec![0, 1, 2],
            vec![0],
            vec![1, 2],
            vec![1],
            vec![2],
            vec![2]
        ]
    );
    assert_eq!(results.len(), 3);
    assert!(results.iter().all(|error| error.contains("boot timeout")));
}

#[test]
fn successful_isolated_retry_resumes_at_first_unstarted_case() {
    let mut invocations = vec![];
    let results = recover_cases(
        4,
        4,
        |indices| {
            invocations.push(indices.to_vec());
            let completed = if invocations.len() == 1 {
                vec![(0, 0)]
            } else {
                indices.iter().map(|&i| (i, i)).collect()
            };
            Attempt {
                completed,
                interruption: "timeout".into(),
            }
        },
        |_| usize::MAX,
    );
    assert_eq!(results, vec![0, 1, 2, 3]);
    assert_eq!(invocations, vec![vec![0, 1, 2, 3], vec![1], vec![2, 3]]);
}

fn scratch() -> EphemeralArtifactDir {
    scratch_with_challenge(&opforge_native_cli_run_challenge())
}

fn scratch_with_challenge(challenge: &str) -> EphemeralArtifactDir {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT_SCRATCH: AtomicU64 = AtomicU64::new(0);
    loop {
        let path = std::env::temp_dir().join(format!(
            "opforge-breadth-host-{}-{}",
            challenge,
            NEXT_SCRATCH.fetch_add(1, Ordering::Relaxed)
        ));
        // Cleanup owns only a directory this invocation created exclusively.
        // Clock resolution cannot make parallel tests share an owned tree.
        match fs::create_dir(&path) {
            Ok(()) => return EphemeralArtifactDir(path),
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(error) => {
                panic!("create breadth scratch parent before package generation: {error}")
            }
        }
    }
}

#[test]
fn parallel_scratch_cleanup_preserves_other_owned_trees() {
    // Force identical clock-derived challenges to reproduce the former alias.
    let challenge = opforge_native_cli_run_challenge();
    let owners = std::thread::scope(|scope| {
        let workers = (0..16)
            .map(|_| scope.spawn(|| scratch_with_challenge(&challenge)))
            .collect::<Vec<_>>();
        workers
            .into_iter()
            .map(|worker| worker.join().unwrap())
            .collect::<Vec<_>>()
    });
    let mut paths = owners
        .iter()
        .map(|owner| owner.0.clone())
        .collect::<Vec<_>>();
    paths.sort();
    paths.dedup();
    assert_eq!(paths.len(), owners.len());
    for owner in &owners {
        fs::write(owner.0.join("owned"), b"live").unwrap();
    }
    let mut owners = owners;
    while let Some(owner) = owners.pop() {
        let removed = owner.0.clone();
        drop(owner);
        assert!(!removed.exists());
        for survivor in &owners {
            assert_eq!(fs::read(survivor.0.join("owned")).unwrap(), b"live");
        }
    }
}

fn case<'a>(
    files: &'a [OpforgeNativeCliGuestFile<'a>],
    oracle: &'a [u8],
) -> OpforgeNativeCliParityCase<'a> {
    OpforgeNativeCliParityCase {
        name: "actual-source",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(b".byte 7"),
        command_template: Some("Work:entry.asm --hex Work:audit.hex"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: files,
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/audit.hex",
            rust_oracle: oracle,
        },
    }
}

fn complete(paths: &OpforgeNativeCliBatchCasePaths, output: &[u8]) {
    fs::write(&paths.started_path, &paths.expected_started).unwrap();
    fs::write(&paths.done_path, &paths.expected_done).unwrap();
    fs::write(&paths.exit_code_path, b"0").unwrap();
    fs::write(paths.artifact_dir.join("audit.hex"), output).unwrap();
}

#[test]
fn per_case_work_roots_isolate_inputs_absolute_outputs_and_capture() {
    let scratch = scratch();
    let first_files = [OpforgeNativeCliGuestFile {
        relative_path: "entry.asm",
        bytes: b".byte 1",
    }];
    let second_files = [OpforgeNativeCliGuestFile {
        relative_path: "entry.asm",
        bytes: b".byte 2",
    }];
    let first_case = case(&first_files, b"first");
    let second_case = case(&second_files, b"second");
    let first = stage_case(&scratch.0, 7, "fresh", &first_case, b"image").unwrap();
    let second = stage_case(&scratch.0, 8, "fresh", &second_case, b"image").unwrap();
    assert_ne!(first.expected_started, second.expected_started);
    let duplicate = stage_case(&scratch.0, 9, "fresh", &first_case, b"image").unwrap();
    assert_ne!(first.expected_started, duplicate.expected_started);
    assert_eq!(
        fs::read(first.artifact_dir.join("entry.asm")).unwrap(),
        b".byte 1"
    );
    assert_eq!(
        fs::read(second.artifact_dir.join("entry.asm")).unwrap(),
        b".byte 2"
    );
    complete(&first, b"first");
    complete(&second, b"second");
    let result = collect_case(
        &first_case,
        &first,
        &scratch.0,
        Path::new("image"),
        b"image",
    )
    .unwrap()
    .unwrap();
    assert!(result.error.is_none());
    let run = result.run.unwrap();
    assert_eq!(
        run.captured_artifacts[Path::new("Work/audit.hex")],
        b"first"
    );
    assert!(run.start_to_done_host_seconds.is_none());
    let mut script = String::from("FailAt 999\n");
    append_case_script(&mut script, 7, &first, first_case.command_template.unwrap());
    append_case_script(
        &mut script,
        8,
        &second,
        second_case.command_template.unwrap(),
    );
    assert!(script.contains(
        "C:Assign Work: OPForgeBatch:cases/case_0007\nIf WARN\nQuit 20\nEndIf\nCD Work:"
    ));
    assert!(script.contains(
        "C:Assign Work: OPForgeBatch:cases/case_0008\nIf WARN\nQuit 20\nEndIf\nCD Work:"
    ));
    assert_eq!(script.matches("OPForgeBatch:build/opforge").count(), 2);
}

#[test]
fn stale_challenge_and_missing_exit_never_complete_and_mismatch_keeps_run() {
    let scratch = scratch();
    let case = case(&[], b"oracle");
    let paths = stage_case(&scratch.0, 0, "fresh", &case, b"image").unwrap();
    complete(&paths, b"wrong");
    fs::write(
        &paths.started_path,
        "OPFORGE-FS-UAE-PROOF-V1 START stale identity",
    )
    .unwrap();
    assert!(
        collect_case(&case, &paths, &scratch.0, Path::new("image"), b"image")
            .unwrap()
            .is_none()
    );
    fs::write(&paths.started_path, &paths.expected_started).unwrap();
    fs::remove_file(&paths.exit_code_path).unwrap();
    assert!(
        collect_case(&case, &paths, &scratch.0, Path::new("image"), b"image")
            .unwrap()
            .is_none()
    );
    fs::write(&paths.exit_code_path, "0").unwrap();
    let result = collect_case(&case, &paths, &scratch.0, Path::new("image"), b"image")
        .unwrap()
        .unwrap();
    assert!(result.error.is_some());
    let run = result.run.unwrap();
    assert!(run.protocol_completed);
    assert!(!run.success);
    assert_eq!(
        run.captured_artifacts[Path::new("Work/audit.hex")],
        b"wrong"
    );
}

#[test]
fn launcher_interruption_snapshots_completed_prefix_after_confirmed_shutdown() {
    let scratch = scratch();
    let cases = [case(&[], b"first"), case(&[], b"second")];
    let first = stage_case(&scratch.0, 0, "fresh", &cases[0], b"image").unwrap();
    let second = stage_case(&scratch.0, 1, "fresh", &cases[1], b"image").unwrap();
    complete(&first, b"first");
    fs::write(&second.started_path, &second.expected_started).unwrap();
    let staged = [(0, first), (1, second)];
    let image_path = scratch.0.join("image");
    fs::write(&image_path, b"image").unwrap();
    let captured = snapshot_cases(
        &cases,
        &staged,
        &scratch.0,
        &image_path,
        b"image",
        vec![],
        LaunchOutcome {
            interruption: "launcher timeout".into(),
            shutdown_confirmed: true,
        },
    );
    assert_eq!(captured.completed.len(), 1);
    assert_eq!(captured.completed[0].0, 0);
    assert!(captured.completed[0].1.error.is_none());
    assert!(
        captured.completed[0]
            .1
            .run
            .as_ref()
            .unwrap()
            .protocol_completed
    );
    assert_eq!(captured.interruption, "launcher timeout");
    fs::write(&image_path, b"changed executable").unwrap();
    let changed = snapshot_cases(
        &cases,
        &staged,
        &scratch.0,
        &image_path,
        b"image",
        vec![],
        LaunchOutcome {
            interruption: String::new(),
            shutdown_confirmed: true,
        },
    );
    assert!(changed.completed[0]
        .1
        .error
        .as_ref()
        .unwrap()
        .contains("frozen executable changed"));
    assert!(!changed.completed[0].1.run.as_ref().unwrap().success);

    let quarantined = snapshot_cases(
        &cases,
        &staged,
        &scratch.0,
        &image_path,
        b"image",
        vec![],
        LaunchOutcome {
            interruption: "shutdown uncertain".into(),
            shutdown_confirmed: false,
        },
    );
    assert!(quarantined.completed.is_empty());
}

#[test]
fn negative_proof_retains_completed_nonzero_exit_and_fresh_diagnostic() {
    let scratch = scratch();
    let mut case = case(&[], b"unused");
    case.proof = OpforgeNativeCliProof::ExpectedFailureContaining("invalid source");
    let paths = stage_case(&scratch.0, 0, "fresh", &case, b"image").unwrap();
    complete(&paths, b"");
    fs::write(&paths.exit_code_path, "20").unwrap();
    fs::write(&paths.stdout_path, "invalid source").unwrap();
    fs::write(&paths.stderr_path, "").unwrap();
    let result = collect_case(&case, &paths, &scratch.0, Path::new("image"), b"image")
        .unwrap()
        .unwrap();
    assert!(result.error.is_none());
    let run = result.run.unwrap();
    assert!(run.protocol_completed);
    assert_eq!(run.exit_code, Some(20));
    assert!(
        !run.success,
        "negative proof must not claim successful assembly"
    );
}

#[test]
fn breadth_config_bootstraps_work_assign_without_changing_saved_system_or_commands() {
    let scratch = scratch();
    let saved_system = scratch.0.join("saved-system");
    let original = b"; saved startup\nIf EXISTS Work:build/tkpkg_debug_cli.hunk\n Work:build/tkpkg_debug_cli.hunk\nEndIf\n";
    stage_guest_input_bytes(&saved_system, "S/user-startup", original).unwrap();
    stage_guest_input_bytes(
        &saved_system,
        "S/Startup-sequence",
        b"Execute S:User-Startup\n",
    )
    .unwrap();
    stage_guest_input_bytes(&saved_system, "C/Assign", b"assign-command").unwrap();
    let template = format!("[fs-uae]\ncpu = 68020\nhard_drive_0 = {}\nhard_drive_1 = /saved/work\nhard_drive_1_label = Work\nkickstart_file = /saved/rom\n", saved_system.display());
    let attempt = scratch.0.join("attempt");
    let work = attempt.join("Work");
    stage_guest_script(&work, "FailAt 999\n").unwrap();
    let config_path =
        prepare_breadth_config(&template, &attempt, &work, FsUaeMemoryProfile::Existing).unwrap();
    let config = fs::read_to_string(config_path).unwrap();
    assert!(config.contains(&format!(
        "hard_drive_0 = {}\n",
        attempt.join("System").display()
    )));
    assert!(config.contains(&format!("hard_drive_1 = {}\n", work.display())));
    assert!(config.contains("hard_drive_1_label = OPForgeBatch\n"));
    assert!(config.contains("hard_drive_0_label = OPForgeSystem\n"));
    assert!(config.contains("hard_drive_1_device = DH1\n"));
    assert!(!config.contains("hard_drive_1_label = Work\n"));
    assert_eq!(
        config
            .lines()
            .filter(|line| line.starts_with("hard_drive_1 ="))
            .count(),
        1
    );
    assert!(config.contains("kickstart_file = /saved/rom\n"));
    assert_eq!(
        fs::read(saved_system.join("S/user-startup")).unwrap(),
        original
    );
    let generated_startup = fs::read_to_string(attempt.join("System/S/user-startup")).unwrap();
    let assign = generated_startup
        .find("C:Assign Work: OPForgeBatch:")
        .unwrap();
    let driver = generated_startup
        .find("Work:build/tkpkg_debug_cli.hunk")
        .unwrap();
    assert!(assign < driver);
    assert!(generated_startup.as_bytes().ends_with(original));
    assert_eq!(
        fs::read(attempt.join("System/C/Assign")).unwrap(),
        b"assign-command"
    );
    assert_eq!(
        fs::read(work.join(FS_UAE_STARTUP_HUNK_ALIAS)).unwrap(),
        b"FailAt 999\n"
    );
    // Saved single-case configuration still selects its ordinary Work mount.
    let ordinary =
        rewrite_fs_uae_config_work_mount(&template, "/ordinary/work", FsUaeMemoryProfile::Existing);
    assert!(ordinary.contains("hard_drive_1 = /ordinary/work\n"));
    assert!(!ordinary.contains("OPForgeBatch"));
}

#[test]
fn breadth_config_rejects_non_directory_system_templates() {
    let scratch = scratch();
    let hdf = scratch.0.join("system.hdf");
    fs::write(&hdf, b"disk-image").unwrap();
    let template = format!("hard_drive_0 = {}\n", hdf.display());
    let error = prepare_breadth_config(
        &template,
        &scratch.0.join("attempt"),
        &scratch.0.join("attempt/Work"),
        FsUaeMemoryProfile::Existing,
    )
    .unwrap_err();
    assert!(error.contains("absolute directory system drive"));
}
