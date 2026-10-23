// SPDX-License-Identifier: GPL-3.0-or-later
//! Correctness-only batching of a frozen compact CLI. No timing qualification.
use super::*;

pub(crate) struct BreadthCaseResult {
    pub(crate) run: Option<FsUaeSmokeRun>,
    pub(crate) error: Option<String>,
}

struct Attempt<T> {
    completed: Vec<(usize, T)>,
    interruption: String,
}

// Completed means fresh protocol completion, even when artifact proof fails.
// Only unfinished cases may be retried; successful prefixes remain in memory.
fn recover_cases<T>(
    count: usize,
    batch_size: usize,
    mut execute: impl FnMut(&[usize]) -> Attempt<T>,
    mut failed: impl FnMut(String) -> T,
) -> Vec<T> {
    assert!(batch_size > 0, "batch size must be positive");
    let mut results: Vec<Option<T>> = (0..count).map(|_| None).collect();
    let mut interrupted = vec![false; count];
    while let Some(first) = results.iter().position(Option::is_none) {
        let retry = interrupted[first];
        let pending: Vec<_> = (first..count)
            .filter(|&index| results[index].is_none())
            .take(if retry { 1 } else { batch_size })
            .collect();
        let attempt = execute(&pending);
        for (index, result) in attempt.completed {
            assert!(
                pending.contains(&index),
                "backend returned an unrequested case"
            );
            assert!(
                results[index].is_none(),
                "backend repeated a completed case"
            );
            results[index] = Some(result);
        }
        if let Some(&unfinished) = pending.iter().find(|&&index| results[index].is_none()) {
            if retry {
                results[unfinished] = Some(failed(format!(
                    "case interrupted again during isolated retry: {}",
                    attempt.interruption
                )));
            } else {
                interrupted[unfinished] = true;
            }
        }
    }
    results.into_iter().map(Option::unwrap).collect()
}

pub(crate) fn run_from_env(
    workspace_root: &Path,
    cases: &[OpforgeNativeCliParityCase<'_>],
    image: &[u8],
    batch_size: usize,
) -> Result<Vec<BreadthCaseResult>, String> {
    if image.is_empty() || cases.is_empty() || !(1..=64).contains(&batch_size) {
        return Err("breadth audit requires an image, cases and batch_size in 1..=64".into());
    }
    if std::env::var("OPFORGE_NATIVE_CORPUS_LIVE_CAPTURE").as_deref() == Ok("1") {
        return Err("OPFORGE_NATIVE_CORPUS_LIVE_CAPTURE uses the retired OFPR progress protocol; use the current console-debugger tools for inspection".into());
    }
    if std::env::var("OPFORGE_FS_UAE_SMOKE").as_deref() != Ok("1") {
        return Err("breadth native execution requires OPFORGE_FS_UAE_SMOKE=1".into());
    }
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let args = std::env::var(FS_UAE_ARGS_ENV).map_err(|err| err.to_string())?;
    // Batch volume aliases and startup script require the canonical mounted config.
    if args.trim() != "{fsuae_config}" {
        return Err("breadth correctness batching requires canonical {fsuae_config} args".into());
    }
    let binary = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".into());
    Ok(recover_cases(
        cases.len(),
        batch_size,
        |indices| match execute_batch(workspace_root, &binary, cases, image, indices) {
            Ok(attempt) => attempt,
            Err(error) => Attempt {
                completed: vec![],
                interruption: error,
            },
        },
        |error| BreadthCaseResult {
            run: None,
            error: Some(error),
        },
    ))
}

fn append_case_script(
    script: &mut String,
    index: usize,
    paths: &OpforgeNativeCliBatchCasePaths,
    command: &str,
) {
    // Work: absolute outputs and relative outputs both resolve inside this case.
    script.push_str(&format!(
        "C:Assign Work: OPForgeBatch:cases/case_{index:04}\nIf WARN\nQuit 20\nEndIf\nCD Work:\nIf WARN\nQuit 20\nEndIf\n\
         Echo \"{}\" >{}/{}\n\
         OPForgeBatch:build/opforge {} >{}/{} *>{}/{}\n\
         Echo $RC >{}/{}\nEcho \"{}\" >{}/{}\n",
        paths.expected_started,
        paths.guest_artifact_dir,
        FS_UAE_OPFORGE_NATIVE_CLI_CASE_STARTED_FILE,
        command,
        paths.guest_artifact_dir,
        FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDOUT_FILE,
        paths.guest_artifact_dir,
        FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDERR_FILE,
        paths.guest_artifact_dir,
        FS_UAE_OPFORGE_NATIVE_CLI_CASE_EXITCODE_FILE,
        paths.expected_done,
        paths.guest_artifact_dir,
        FS_UAE_OPFORGE_NATIVE_CLI_CASE_DONE_FILE,
    ));
}

fn clear_declared_outputs(root: &Path, proof: OpforgeNativeCliProof<'_>) -> Result<(), String> {
    let outputs: Vec<&str> = match proof {
        OpforgeNativeCliProof::ExactArtifact { relative_path, .. } => vec![relative_path],
        OpforgeNativeCliProof::ExactArtifacts(artifacts) => artifacts
            .iter()
            .map(|artifact| artifact.relative_path)
            .collect(),
        OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(paths) => paths.to_vec(),
        _ => vec![],
    };
    for output in outputs {
        let relative = Path::new(output)
            .strip_prefix(FS_UAE_MOUNTED_WORK_DIR_NAME)
            .map_err(|_| format!("breadth output must be inside Work/: {output}"))?;
        if relative.as_os_str().is_empty()
            || relative
                .components()
                .any(|part| !matches!(part, std::path::Component::Normal(_)))
        {
            return Err(format!("invalid breadth output path: {output}"));
        }
        match fs::remove_file(root.join(relative)) {
            Ok(()) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(error.to_string()),
        }
    }
    Ok(())
}

fn stage_case(
    work: &Path,
    index: usize,
    challenge: &str,
    case: &OpforgeNativeCliParityCase<'_>,
    image: &[u8],
) -> Result<OpforgeNativeCliBatchCasePaths, String> {
    if case.cpu_override != "68020" || !case.extra_assembly_defines.is_empty() {
        return Err("prebuilt breadth cases require 68020 host and no assembly defines".into());
    }
    if !matches!(
        case.package_mode,
        OpforgeNativeCliPackageMode::EmbeddedDefault
    ) {
        return Err("breadth packages must be carried explicitly as guest files".into());
    }
    let root = work.join(format!("cases/case_{index:04}"));
    let identity = opforge_native_cli_case_identity(
        case,
        None,
        NativeCliParityExecutable::CompactCli,
        Some(image),
    );
    // Each isolated tree keeps canonical case_0000 protocol paths for the verifier.
    let case_challenge = format!("{challenge}-{index}");
    let paths = opforge_native_cli_batch_case_paths(&root, 0, &case_challenge, &identity);
    fs::create_dir_all(&paths.protocol_dir).map_err(|err| err.to_string())?;
    stage_guest_input_bytes(
        &root,
        "input.bin",
        case.source_override
            .ok_or("breadth case requires source bytes")?,
    )?;
    for file in case.extra_guest_files {
        stage_guest_input_bytes(&root, file.relative_path, file.bytes)?;
    }
    clear_native_cli_output_artifacts(&root)?;
    clear_declared_outputs(&root, case.proof)?;
    // Guest inputs must not prepopulate protocol evidence.
    clear_capture_files(&batch_capture_config_from_env(std::slice::from_ref(
        &paths,
    ))?)?;
    Ok(paths)
}

fn execute_batch(
    workspace_root: &Path,
    binary: &str,
    cases: &[OpforgeNativeCliParityCase<'_>],
    image: &[u8],
    indices: &[usize],
) -> Result<Attempt<BreadthCaseResult>, String> {
    let artifact_dir = create_artifact_dir(workspace_root, "fs-uae-compact-breadth")?;
    let ephemeral_artifact_dir = EphemeralArtifactDir(artifact_dir.clone());
    let work = artifact_dir.join(FS_UAE_MOUNTED_WORK_DIR_NAME);
    stage_guest_input_bytes(&work, "build/opforge", image)?;
    let challenge = opforge_native_cli_run_challenge();
    let mut script = String::from("FailAt 999\n");
    let mut staged = Vec::new();
    let mut completed = Vec::new();
    for &index in indices {
        let case = &cases[index];
        let prepared = (|| {
            let command = case
                .command_template
                .ok_or("breadth case requires source command")?;
            let paths = stage_case(&work, index, &challenge, case, image)?;
            append_case_script(&mut script, index, &paths, command);
            Ok::<_, String>(paths)
        })();
        match prepared {
            Ok(paths) => staged.push((index, paths)),
            Err(error) => completed.push((
                index,
                BreadthCaseResult {
                    run: None,
                    error: Some(error),
                },
            )),
        }
    }
    let launch = if staged.is_empty() {
        LaunchOutcome {
            interruption: String::new(),
            shutdown_confirmed: true,
        }
    } else {
        let prepared = (|| {
            stage_guest_script(&work, &script)?;
            materialize_breadth_config(&artifact_dir, &work)
        })();
        match prepared {
            Ok(config) => launch_batch(binary, &artifact_dir, &config, &staged),
            Err(error) => LaunchOutcome {
                interruption: error,
                shutdown_confirmed: true,
            },
        }
    };
    let attempt = snapshot_cases(
        cases,
        &staged,
        &artifact_dir,
        &work.join("build/opforge"),
        image,
        completed,
        launch,
    );
    drop(ephemeral_artifact_dir);
    Ok(attempt)
}

// Directory-system setup is copied into this attempt so the saved boot scripts
// remain untouched. Work: must be an assign before the standard smoke hook runs.
fn copy_system_directory(source: &Path, destination: &Path) -> Result<(), String> {
    fs::create_dir_all(destination).map_err(|error| error.to_string())?;
    for entry in fs::read_dir(source).map_err(|error| error.to_string())? {
        let entry = entry.map_err(|error| error.to_string())?;
        let target = destination.join(entry.file_name());
        let kind = entry.file_type().map_err(|error| error.to_string())?;
        if kind.is_dir() {
            copy_system_directory(&entry.path(), &target)?;
        } else if kind.is_file() {
            fs::copy(entry.path(), target).map_err(|error| error.to_string())?;
        } else {
            return Err(format!(
                "unsupported system directory entry: {}",
                entry.path().display()
            ));
        }
    }
    Ok(())
}

fn rewrite_breadth_config(
    template: &str,
    work: &Path,
    system: &Path,
    profile: FsUaeMemoryProfile,
) -> String {
    let rewritten = rewrite_fs_uae_config_work_mount(template, &work.to_string_lossy(), profile);
    let mut lines: Vec<_> = rewritten
        .lines()
        .filter(|line| {
            let key = line.split_once('=').map(|(key, _)| key.trim());
            !matches!(
                key,
                Some(
                    "hard_drive_0"
                        | "hard_drive_0_label"
                        | "hard_drive_1"
                        | "hard_drive_1_label"
                        | "hard_drive_1_device"
                        | "hard_drive_1_read_only"
                )
            )
        })
        .map(str::to_owned)
        .collect();
    lines.extend([
        format!("hard_drive_0 = {}", system.display()),
        "hard_drive_0_label = OPForgeSystem".into(),
        format!("hard_drive_1 = {}", work.display()),
        "hard_drive_1_label = OPForgeBatch".into(),
        "hard_drive_1_device = DH1".into(),
        "hard_drive_1_read_only = 0".into(),
    ]);
    lines.join("\n") + "\n"
}

fn prepare_breadth_config(
    template: &str,
    artifact_dir: &Path,
    work: &Path,
    profile: FsUaeMemoryProfile,
) -> Result<PathBuf, String> {
    let system_source = template
        .lines()
        .find_map(|line| {
            let (key, value) = line.split_once('=')?;
            (key.trim() == "hard_drive_0").then(|| PathBuf::from(value.trim()))
        })
        .ok_or("breadth boot requires hard_drive_0 directory in template")?;
    if !system_source.is_absolute() || !system_source.is_dir() {
        return Err("breadth boot requires an absolute directory system drive".into());
    }
    let system = artifact_dir.join("System");
    copy_system_directory(&system_source, &system)?;
    let startup = system.join("S/user-startup");
    let original = fs::read(&startup)
        .map_err(|error| format!("read copied breadth S/user-startup: {error}"))?;
    let mut bootstrap = b"; opForge breadth-only Work assign before the standard smoke hook\nFailAt 999\nC:Assign Work: OPForgeBatch:\nIf WARN\nQuit 20\nEndIf\n".to_vec();
    bootstrap.extend(original);
    fs::write(&startup, bootstrap).map_err(|error| error.to_string())?;
    let config = artifact_dir.join(FS_UAE_CONFIG_FILE_NAME);
    fs::write(
        &config,
        rewrite_breadth_config(template, work, &system, profile),
    )
    .map_err(|error| error.to_string())?;
    Ok(config)
}

fn materialize_breadth_config(artifact_dir: &Path, work: &Path) -> Result<PathBuf, String> {
    let template = std::env::var(FS_UAE_CONFIG_TEMPLATE_ENV)
        .map_err(|error| format!("breadth boot requires config template: {error}"))?;
    let text = fs::read_to_string(template).map_err(|error| error.to_string())?;
    prepare_breadth_config(&text, artifact_dir, work, fs_uae_memory_profile_from_env()?)
}

fn snapshot_cases(
    cases: &[OpforgeNativeCliParityCase<'_>],
    staged: &[(usize, OpforgeNativeCliBatchCasePaths)],
    artifact_dir: &Path,
    image_path: &Path,
    image: &[u8],
    mut completed: Vec<(usize, BreadthCaseResult)>,
    launch: LaunchOutcome,
) -> Attempt<BreadthCaseResult> {
    let image_error = if launch.shutdown_confirmed {
        match fs::read(image_path) {
            Ok(actual) if actual == image => None,
            Ok(_) => Some("frozen executable changed during breadth batch".to_owned()),
            Err(error) => Some(format!(
                "read frozen executable after breadth batch: {error}"
            )),
        }
    } else {
        None
    };
    // Snapshot after shutdown on success AND interruption, before ephemeral cleanup.
    for (index, paths) in staged.iter().filter(|_| launch.shutdown_confirmed) {
        let case = &cases[*index];
        match collect_case(case, paths, artifact_dir, image_path, image) {
            Ok(Some(mut result)) => {
                if let Some(error) = &image_error {
                    result.error = Some(match result.error {
                        Some(proof_error) => format!("{proof_error}\n{error}"),
                        None => error.clone(),
                    });
                    if let Some(run) = &mut result.run {
                        run.success = false;
                    }
                }
                completed.push((*index, result));
            }
            Ok(None) => {} // Incomplete protocol: coordinator owns the bounded retry.
            Err(error) => completed.push((
                *index,
                BreadthCaseResult {
                    run: None,
                    error: Some(error),
                },
            )),
        }
    }
    Attempt {
        completed,
        interruption: launch.interruption,
    }
}

fn collect_case(
    case: &OpforgeNativeCliParityCase<'_>,
    paths: &OpforgeNativeCliBatchCasePaths,
    artifact_dir: &Path,
    image_path: &Path,
    image: &[u8],
) -> Result<Option<BreadthCaseResult>, String> {
    let started = read_optional_text(&paths.started_path)?
        .is_some_and(|s| s.trim() == paths.expected_started);
    let done =
        read_optional_text(&paths.done_path)?.is_some_and(|s| s.trim() == paths.expected_done);
    let exit_code = read_optional_exit_code(&paths.exit_code_path)?;
    if !(started && done && exit_code.is_some()) {
        return Ok(None);
    }
    let captured_artifacts = capture_artifact_files(&paths.artifact_dir)?
        .into_iter()
        .map(|(path, bytes)| {
            (
                PathBuf::from(FS_UAE_MOUNTED_WORK_DIR_NAME).join(path),
                bytes,
            )
        })
        .collect();
    let mut run = FsUaeSmokeRun {
        example_name: "opforge_breadth",
        source_path: paths.artifact_dir.join("input.bin"),
        artifact_dir: artifact_dir.into(),
        hunk_path: image_path.into(),
        stdout: read_optional_text(&paths.stdout_path)?.unwrap_or_default(),
        stderr: read_optional_text(&paths.stderr_path)?.unwrap_or_default(),
        exit_code,
        protocol_completed: true,
        start_to_done_host_seconds: None,
        native_image_digest: Some(opforge_self_host_package_digest(image)),
        success: exit_code == Some(0),
        verified_output: None,
        captured_artifacts,
    };
    let error = verify_native_cli_case_proof(case, &mut run).err();
    if error.is_some() {
        run.success = false;
    }
    Ok(Some(BreadthCaseResult {
        run: Some(run),
        error,
    }))
}

struct LaunchOutcome {
    interruption: String,
    shutdown_confirmed: bool,
}

fn launch_batch(
    binary: &str,
    artifact_dir: &Path,
    config: &Path,
    staged: &[(usize, OpforgeNativeCliBatchCasePaths)],
) -> LaunchOutcome {
    let setup = (|| {
        terminate_preexisting_fs_uae_processes()?;
        let baseline = snapshot_fs_uae_process_ids()?;
        let stdout = fs::File::create(artifact_dir.join(FS_UAE_LAUNCHER_STDOUT_FILE))
            .map_err(|err| err.to_string())?;
        let stderr = fs::File::create(artifact_dir.join(FS_UAE_LAUNCHER_STDERR_FILE))
            .map_err(|err| err.to_string())?;
        let child = fs_uae_launch_command(binary, &[config.to_string_lossy().into_owned()])
            .current_dir(artifact_dir)
            .stdout(Stdio::from(stdout))
            .stderr(Stdio::from(stderr))
            .spawn()
            .map_err(|err| format!("launch breadth FS-UAE: {err}"))?;
        Ok::<_, String>((baseline, child))
    })();
    let (baseline, mut child) = match setup {
        Ok(value) => value,
        Err(error) => {
            return LaunchOutcome {
                interruption: error,
                shutdown_confirmed: true,
            }
        }
    };
    let mut errors = Vec::new();
    match wait_for_batch(&mut child, staged, &baseline) {
        Ok(()) => {
            if let Err(error) = wait_for_process_exit_after_capture(&mut child, "opforge_breadth") {
                errors.push(error);
            }
        }
        Err(error) => errors.push(error),
    }
    // Cleanup executes on every wait outcome. Even cleanup errors must not erase
    // independently completed cases if shutdown can still be confirmed.
    let _ = child.kill();
    if let Err(error) = cleanup_spawned_fs_uae_processes(&baseline) {
        errors.push(error);
    }
    let shutdown_confirmed = match wait_for_spawned_fs_uae_processes_to_exit(&baseline) {
        Ok(()) => true,
        Err(error) => {
            errors.push(error);
            false
        }
    };
    if shutdown_confirmed {
        if let Err(error) = child.wait() {
            errors.push(error.to_string());
        }
    }
    LaunchOutcome {
        interruption: errors.join("\n"),
        shutdown_confirmed,
    }
}

fn wait_for_batch(
    child: &mut std::process::Child,
    staged: &[(usize, OpforgeNativeCliBatchCasePaths)],
    baseline: &BTreeSet<u32>,
) -> Result<(), String> {
    let capture = capture_config_from_env(&staged[0].1.artifact_dir, None)?;
    let mut current = 0;
    let mut deadline = Instant::now() + capture.timeout;
    let mut started_at = None;
    let mut launcher_exited_at = None;
    loop {
        let paths = &staged[current].1;
        let started = read_optional_text(&paths.started_path)?
            .is_some_and(|s| s.trim() == paths.expected_started);
        let done =
            read_optional_text(&paths.done_path)?.is_some_and(|s| s.trim() == paths.expected_done);
        if started && done && read_optional_exit_code(&paths.exit_code_path)?.is_some() {
            current += 1;
            if current == staged.len() {
                return Ok(());
            }
            deadline = Instant::now() + capture.timeout;
            started_at = None;
            continue;
        }
        if started && started_at.is_none() {
            started_at = Some(Instant::now());
        }
        if child.try_wait().map_err(|err| err.to_string())?.is_some() {
            let now = Instant::now();
            let exited_at = *launcher_exited_at.get_or_insert(now);
            let present = snapshot_fs_uae_process_ids()?
                .difference(baseline)
                .next()
                .is_some();
            if launcher_exit_is_terminal(exited_at, now, present) {
                return Err(format!(
                    "breadth emulator exited before case {} completed",
                    staged[current].0
                ));
            }
        }
        if Instant::now() >= deadline
            || started_at.is_some_and(|at: Instant| at.elapsed() >= capture.post_start_timeout)
        {
            return Err(format!(
                "breadth case {} timed out without fresh completion",
                staged[current].0
            ));
        }
        thread::sleep(capture.poll_interval);
    }
}

#[cfg(test)]
#[path = "breadth_runner_tests.rs"]
mod tests;
