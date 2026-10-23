use cli_core::LabelOutputFormat as CliLabelOutputFormat;
use engine::OutputFormat as EngineOutputFormat;
use engine::{default_cpu, run_assembly, AssemblyExecutionRequest, ExecutionMode};
use serde_json::json;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitStatus;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use types::lockstep::ContinuationHead;
use vm::output_model::BinOutputSpec;

#[path = "fs_uae_smoke/config.rs"]
mod config;
use config::{
    fs_uae_memory_profile_from_env, rewrite_fs_uae_config_work_mount, validate_budget_launch,
    FsUaeMemoryProfile,
};

#[path = "fs_uae_smoke/breadth_runner.rs"]
mod breadth_runner;
pub(crate) use breadth_runner::BreadthCaseResult;

#[path = "tests/binary_source_hunk.rs"]
pub(crate) mod hunk;

#[cfg(test)]
#[path = "tests/compact_self_host_export.rs"]
mod compact_self_host_export;
#[cfg(test)]
#[path = "tests/native_cli_frozen_comparison.rs"]
mod native_cli_frozen_comparison;

pub(crate) struct FsUaeNativeCliSmokeLock(std::sync::Mutex<()>);

impl FsUaeNativeCliSmokeLock {
    pub(crate) fn lock(&self) -> Result<std::sync::MutexGuard<'_, ()>, std::convert::Infallible> {
        Ok(match self.0.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        })
    }
}

pub(crate) fn fs_uae_native_cli_smoke_lock() -> &'static FsUaeNativeCliSmokeLock {
    static LOCK: std::sync::OnceLock<FsUaeNativeCliSmokeLock> = std::sync::OnceLock::new();
    LOCK.get_or_init(|| FsUaeNativeCliSmokeLock(std::sync::Mutex::new(())))
}

const FS_UAE_BIN_ENV: &str = "OPFORGE_FS_UAE_BIN";
const FS_UAE_ARGS_ENV: &str = "OPFORGE_FS_UAE_ARGS";
const FS_UAE_CONFIG_TEMPLATE_ENV: &str = "OPFORGE_FS_UAE_CONFIG_TEMPLATE";
const FS_UAE_START_FILE_ENV: &str = "OPFORGE_FS_UAE_START_FILE";
const FS_UAE_READY_FILE_ENV: &str = "OPFORGE_FS_UAE_READY_FILE";
const FS_UAE_STDOUT_FILE_ENV: &str = "OPFORGE_FS_UAE_STDOUT_FILE";
const FS_UAE_STDERR_FILE_ENV: &str = "OPFORGE_FS_UAE_STDERR_FILE";
const FS_UAE_EXIT_CODE_FILE_ENV: &str = "OPFORGE_FS_UAE_EXIT_CODE_FILE";
const FS_UAE_TIMEOUT_MS_ENV: &str = "OPFORGE_FS_UAE_TIMEOUT_MS";
const FS_UAE_POLL_MS_ENV: &str = "OPFORGE_FS_UAE_POLL_MS";
const FS_UAE_POST_START_TIMEOUT_MS_ENV: &str = "OPFORGE_FS_UAE_POST_START_TIMEOUT_MS";
const FS_UAE_DEFAULT_START_FILE: &str = "opforge_fsuae_smoke.start";
const FS_UAE_DEFAULT_READY_FILE: &str = "opforge_fsuae_smoke.done";
const FS_UAE_DEFAULT_STDOUT_FILE: &str = "opforge_fsuae_smoke.stdout";
const FS_UAE_DEFAULT_STDERR_FILE: &str = "opforge_fsuae_smoke.stderr";
const FS_UAE_DEFAULT_EXIT_CODE_FILE: &str = "opforge_fsuae_smoke.exitcode";
const FS_UAE_DEFAULT_TIMEOUT_MS: u64 = 300_000;
const FS_UAE_DEFAULT_POLL_MS: u64 = 250;
const FS_UAE_DEFAULT_POST_START_TIMEOUT_MS: u64 = 300_000;
const FS_UAE_LAUNCHER_HANDOFF_GRACE_MS: u64 = 5_000;
const FS_UAE_LAUNCHER_STDOUT_FILE: &str = "fs_uae_launcher.stdout.log";
const FS_UAE_LAUNCHER_STDERR_FILE: &str = "fs_uae_launcher.stderr.log";
const FS_UAE_CONFIG_FILE_NAME: &str = "fs-uae-smoke.fs-uae";
const FS_UAE_MOUNTED_WORK_DIR_NAME: &str = "Work";
const FS_UAE_MOUNTED_HUNK_ALIAS: &str = "build/opforge_fsuae_smoke.hunk";
const FS_UAE_STARTUP_HUNK_ALIAS: &str = "build/tkpkg_debug_cli.hunk";
const FS_UAE_STARTUP_HUNK_ALIAS_UAEM: &str = "build/tkpkg_debug_cli.hunk.uaem";

pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE: &str = "opforge_native_out.bin";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_PRG_OUTPUT_FILE: &str = "opforge_native_out.prg";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_HEX_OUTPUT_FILE: &str = "opforge_native_out.hex";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_LST_OUTPUT_FILE: &str = "opforge_native_out.lst";

const FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME: &str = "debug_contract_harness";
const FS_UAE_DEBUG_CONTRACT_SOURCE_PATH: &str =
    "native/motorola68000/amigaos/tests/debug/debug_contract_harness.asm";

const FS_UAE_OPFORGE_NATIVE_CLI_CASE_ARTIFACTS_DIR: &str = "case_artifacts";
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDOUT_FILE: &str = "opforge_fsuae_smoke.stdout";
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDERR_FILE: &str = "opforge_fsuae_smoke.stderr";
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_EXITCODE_FILE: &str = "opforge_fsuae_smoke.exitcode";
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_STARTED_FILE: &str = "opforge_fsuae_smoke.started";
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_DONE_FILE: &str = "opforge_fsuae_smoke.done";
const AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX: usize = 30;
const FS_UAE_SCRIPT_UAEM_TEXT: &str = "-s--rw-d 2021-04-13 02:43:19.40 \n";

const FS_UAE_EXAMPLES: &[(&str, &str, &str)] = &[
    (
        "helloworld",
        "examples/motorola68000/amigaos/helloworld.asm",
        "68000",
    ),
    (
        "writefile",
        "examples/motorola68000/amigaos/writefile.asm",
        "68000",
    ),
    (
        "prvm_smoke",
        "native/motorola68000/amigaos/tests/prvm/prvm_smoke.asm",
        "68020",
    ),
    (
        "prvm_line_iterator_smoke",
        "native/motorola68000/amigaos/tests/prvm/prvm_line_iterator_smoke.asm",
        "68020",
    ),
];

pub(crate) struct FsUaeSmokeRun {
    pub(crate) example_name: &'static str,
    pub(crate) source_path: PathBuf,
    pub(crate) artifact_dir: PathBuf,
    pub(crate) hunk_path: PathBuf,
    pub(crate) stdout: String,
    pub(crate) stderr: String,
    pub(crate) exit_code: Option<i32>,
    pub(crate) protocol_completed: bool,
    // Single-case host observation only; parity checks remain authoritative.
    pub(crate) start_to_done_host_seconds: Option<f64>,
    pub(crate) native_image_digest: Option<String>,
    pub(crate) success: bool,
    pub(crate) verified_output: Option<Vec<u8>>,
    pub(crate) captured_artifacts: BTreeMap<PathBuf, Vec<u8>>,
}

pub(crate) struct FsUaeConsoleLaunch {
    pub(crate) artifact_dir: PathBuf,
    pub(crate) config_path: PathBuf,
    pub(crate) hunk_path: PathBuf,
    pub(crate) descriptor_path: PathBuf,
}

pub(crate) enum FsUaeSmokeOutcome {
    Skipped(String),
    Completed { runs: Vec<FsUaeSmokeRun> },
}

#[derive(Clone, Copy)]
pub(crate) enum OpforgeNativeCliPackageMode {
    EmbeddedDefault,
}

#[derive(Clone, Copy)]
pub(crate) struct OpforgeNativeCliGuestFile<'a> {
    pub(crate) relative_path: &'a str,
    pub(crate) bytes: &'a [u8],
}

#[derive(Clone, Copy)]
pub(crate) struct OpforgeNativeCliParityCase<'a> {
    pub(crate) name: &'a str,
    pub(crate) cpu_override: &'a str,
    pub(crate) extra_assembly_defines: &'a [&'a str],
    pub(crate) source_override: Option<&'a [u8]>,
    pub(crate) command_template: Option<&'a str>,
    pub(crate) package_mode: OpforgeNativeCliPackageMode,
    pub(crate) extra_guest_files: &'a [OpforgeNativeCliGuestFile<'a>],
    pub(crate) proof: OpforgeNativeCliProof<'a>,
}

#[derive(Clone, Copy)]
pub(crate) struct OpforgeNativeCliExpectedArtifact<'a> {
    pub(crate) relative_path: &'a str,
    pub(crate) rust_oracle: &'a [u8],
}

pub(crate) fn opforge_self_host_package_digest(bytes: &[u8]) -> String {
    format!(
        "fnv1a64:{:016x}",
        fnv1a64_update(0xcbf2_9ce4_8422_2325, bytes)
    )
}

#[derive(Clone, Copy)]
pub(crate) enum OpforgeNativeCliProof<'a> {
    ExactArtifact {
        relative_path: &'a str,
        rust_oracle: &'a [u8],
    },
    ExactArtifacts(&'a [OpforgeNativeCliExpectedArtifact<'a>]),
    ExactStdoutLines {
        prefix: &'a str,
        rust_oracle: &'a [u8],
    },
    /// Informational commands require a fresh exit-zero protocol and live text.
    SuccessfulExitContaining(&'a str),
    /// Validation requires fresh successful completion and absence of named artifacts.
    SuccessfulExitWithoutArtifacts(&'a [&'a str]),
    ExpectedFailureWithDiagnostic,
    ExpectedFailureContaining(&'a str),
}

#[derive(Clone, Copy)]
enum NativeCliParityExecutable {
    ExprvmI64Harness,
    TkvmBranchHarness,
    PrvmMacroHarness,
    BinarySourceHarness,
    CompactCli,
}

pub(crate) fn run_hunk_smoke_from_env(workspace_root: &Path) -> Result<FsUaeSmokeOutcome, String> {
    run_hunk_smoke_cases_from_env(workspace_root, FS_UAE_EXAMPLES)
}

pub(crate) fn run_prvm_hunk_smoke_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    let examples = FS_UAE_EXAMPLES
        .iter()
        .copied()
        .filter(|(name, _, _)| matches!(*name, "prvm_smoke" | "prvm_line_iterator_smoke"))
        .collect::<Vec<_>>();
    run_hunk_smoke_cases_from_env(workspace_root, &examples)
}

fn run_hunk_smoke_cases_from_env(
    workspace_root: &Path,
    examples: &[(&'static str, &'static str, &'static str)],
) -> Result<FsUaeSmokeOutcome, String> {
    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let mut runs = Vec::with_capacity(examples.len());
    for &(example_name, relative_source_path, cpu_override) in examples {
        match run_example_smoke(
            workspace_root,
            &fs_uae_bin,
            &args_text,
            example_name,
            relative_source_path,
            cpu_override,
        )? {
            ExampleSmokeResult::Run(run) => runs.push(*run),
            ExampleSmokeResult::Skipped(reason) => return Ok(FsUaeSmokeOutcome::Skipped(reason)),
        }
    }

    Ok(FsUaeSmokeOutcome::Completed { runs })
}

pub(crate) fn run_native_debug_contract_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; configure FS-UAE to execute the native debug-contract harness"
            )))
        }
    };
    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    match run_example_smoke_with_extra_defines(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME,
        FS_UAE_DEBUG_CONTRACT_SOURCE_PATH,
        "68020",
        &["OPFORGE_DEBUG_CONTRACTS"],
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![*run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

/// Fresh challenged allocator probe; guest zero exit requires every ownership,
/// capacity, payload and public-register assertion in the harness to pass.
pub(crate) fn run_native_memory_budget_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    let args = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set"
            )))
        }
    };
    let binary = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".into());
    match run_example_smoke(
        workspace_root,
        &binary,
        &args,
        "binary_memory_budget_harness",
        "native/motorola68000/amigaos/tests/assembler/memory_budget_harness.asm",
        "68020",
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![*run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

/// Assemble and mount the debug-contract harness without launching FS-UAE.
/// The returned config is consumed only by the separately opt-in PTY runner.
pub(crate) fn prepare_native_debug_contract_console_from_env(
    workspace_root: &Path,
) -> Result<Option<FsUaeConsoleLaunch>, String> {
    if std::env::var("OPFORGE_FS_UAE_CONSOLE_DEBUGGER").as_deref() != Ok("1") {
        return Ok(None);
    }
    if std::env::var(FS_UAE_CONFIG_TEMPLATE_ENV)
        .ok()
        .filter(|value| !value.trim().is_empty())
        .is_none()
    {
        return Err(format!(
            "{FS_UAE_CONFIG_TEMPLATE_ENV} must name the normal FS-UAE template before preparing a console capture"
        ));
    }

    let source_path = workspace_root.join(FS_UAE_DEBUG_CONTRACT_SOURCE_PATH);
    let artifact_dir = create_artifact_dir(workspace_root, "fs-uae-console-debug-contract")?;
    let mounted_work_dir = artifact_dir.join(FS_UAE_MOUNTED_WORK_DIR_NAME);
    fs::create_dir_all(mounted_work_dir.join("build")).map_err(|err| {
        format!(
            "create console harness Work directory {}: {err}",
            mounted_work_dir.display()
        )
    })?;
    let defines = [
        "OPFORGE_DEBUG_CONTRACTS".to_string(),
        "OPFORGE_FS_UAE_CONSOLE_DEBUGGER_HARNESS".to_string(),
    ];
    let include_paths = example_include_paths(workspace_root, FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME);
    let module_paths = example_module_paths(workspace_root, FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME);
    run_assembly(AssemblyExecutionRequest {
        root_path: &source_path,
        input_base: FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME,
        defines: &defines,
        include_paths: &include_paths,
        module_paths: &module_paths,
        pp_macro_depth: 64,
        cpu_override: Some("68020"),
        default_cpu: default_cpu(),
        max_loop_iterations: 1000,
        opasm_package_path: None,
        out_dir: Some(&artifact_dir),
        debug_conditionals: false,
        tab_size: None,
        output_format: EngineOutputFormat::Text,
        go_addr: None,
        bin_specs: &[] as &[BinOutputSpec],
        fill_byte: 0xff,
        fill_byte_set: false,
        default_outputs: false,
        labels_file: None,
        label_output_format: CliLabelOutputFormat::Default,
        dependency_output: None,
        outfile_override: None,
        list_name_override: None,
        hex_name_override: None,
        srec_name_override: None,
        hunk_name_override: None,
        header_title: "opForge FS-UAE console debugger harness",
        output_sink: None,
        source_provider: None,
        execution_mode: ExecutionMode::Lockstep {
            continuation_head: ContinuationHead::Vm,
        },
        collect_runtime_traces: true,
        suppress_outputs: false,
    })
    .map_err(|err| format!("assemble console debug-contract harness: {}", err.summary()))?;
    let hunk_path = generated_hunk_artifact_path(&artifact_dir, FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME);
    if !hunk_path.is_file() {
        return Err(format!(
            "expected console harness Hunk at {}",
            hunk_path.display()
        ));
    }
    let mounted_hunk_path = mounted_work_dir.join(FS_UAE_MOUNTED_HUNK_ALIAS);
    fs::copy(&hunk_path, &mounted_hunk_path).map_err(|err| {
        format!(
            "mount console harness {} at {}: {err}",
            hunk_path.display(),
            mounted_hunk_path.display()
        )
    })?;
    stage_guest_script(
        &mounted_work_dir,
        format!("FailAt 999\nWork:{FS_UAE_MOUNTED_HUNK_ALIAS}\n").as_str(),
    )?;
    let config_path =
        maybe_materialize_fs_uae_config(&artifact_dir, &mounted_work_dir, "{fsuae_config}")?
            .ok_or_else(|| "console harness requires generated FS-UAE config".to_string())?;
    let descriptor_path = artifact_dir.join("console-debugger-launch.json");
    let descriptor = serde_json::to_vec_pretty(&json!({
        "schema_version": 1,
        "mode": "fs-uae-console-debug-contract",
        "proof_level": "E",
        "fs_uae_binary": std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string()),
        "config": config_path,
        "hunk": hunk_path,
        "work_mount": mounted_work_dir,
    }))
    .map_err(|err| format!("serialize console launch descriptor: {err}"))?;
    fs::write(&descriptor_path, descriptor).map_err(|err| {
        format!(
            "write console launch descriptor {}: {err}",
            descriptor_path.display()
        )
    })?;
    Ok(Some(FsUaeConsoleLaunch {
        artifact_dir,
        config_path,
        hunk_path,
        descriptor_path,
    }))
}

/// Execute actual caller-supplied scalar cases against their live Rust oracle.
/// The existing runner supplies a fresh challenge, guest start/completion/exit
/// evidence, exact artifact comparison, and ephemeral cleanup.
pub(crate) fn run_exprvm_i64_harness_from_env(
    workspace_root: &Path,
    case_bytes: &[u8],
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    run_exact_harness_from_env(
        workspace_root,
        case_bytes,
        rust_oracle,
        "Work/build/exprvm-i64-values.bin",
        NativeCliParityExecutable::ExprvmI64Harness,
    )
}

pub(crate) fn run_tkvm_branch_harness_from_env(
    workspace_root: &Path,
    case_bytes: &[u8],
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    run_exact_harness_from_env(
        workspace_root,
        case_bytes,
        rust_oracle,
        "Work/build/tkvm-branch-values.bin",
        NativeCliParityExecutable::TkvmBranchHarness,
    )
}

pub(crate) fn run_prvm_macro_harness_from_env(
    workspace_root: &Path,
    case_bytes: &[u8],
    rust_oracle: &[u8],
    profiled: bool,
) -> Result<FsUaeSmokeOutcome, String> {
    let defines = if profiled {
        &[
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_PROGRESS_RUNTIME_COUNTERS",
        ][..]
    } else {
        &[][..]
    };
    run_exact_harness_with_defines_from_env(
        workspace_root,
        case_bytes,
        rust_oracle,
        "Work/build/prvm-macro-results.bin",
        NativeCliParityExecutable::PrvmMacroHarness,
        defines,
    )
}

/// Compare explicitly ordered source files against the caller's live oracle.
/// The guest reads each actual file; the manifest carries only bounded paths.
/// File contents participate in the shared fresh-run challenge and cleanup.
pub(crate) fn run_binary_source_harness_from_env(
    workspace_root: &Path,
    package: &[u8],
    sources: &[(&str, &[u8])],
    expected: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    run_binary_source_files_from_env(
        workspace_root,
        package,
        sources,
        Some(expected),
        None,
        BinarySourceSearch::ordered(),
    )
}

/// Exercise an explicitly supplied compact executable through the normal fresh
/// challenge and exact-artifact proof path. The bootstrap is part of case identity.
pub(crate) fn run_prebuilt_compact_cli_case_from_env(
    workspace_root: &Path,
    case: &OpforgeNativeCliParityCase<'_>,
    bootstrap: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    if bootstrap.is_empty() {
        return Err("compact CLI bootstrap must not be empty".into());
    }
    let args = std::env::var(FS_UAE_ARGS_ENV).map_err(|err| err.to_string())?;
    let binary = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".into());
    run_native_cli_parity_batch_cases(
        workspace_root,
        &binary,
        &args,
        std::slice::from_ref(case),
        NativeCliParityExecutable::CompactCli,
        Some(bootstrap),
    )
}

/// Correctness-only frozen-image breadth audit. Each indexed result retains
/// its completed run and independent proof error; this API reports no timings.
pub(crate) fn run_prebuilt_compact_cli_breadth_from_env(
    workspace_root: &Path,
    cases: &[OpforgeNativeCliParityCase<'_>],
    bootstrap: &[u8],
    batch_size: usize,
) -> Result<Vec<BreadthCaseResult>, String> {
    breadth_runner::run_from_env(workspace_root, cases, bootstrap, batch_size)
}

/// Exercise the standalone Shell entry with a plain BSP3 package and one source.
pub(crate) fn run_compact_cli_from_env(
    workspace_root: &Path,
    package: &[u8],
    source: &[u8],
    expected: Option<&[u8]>,
) -> Result<FsUaeSmokeOutcome, String> {
    run_compact_cli_files_from_env(
        workspace_root,
        package,
        &[("input.asm", source)],
        &[],
        &[],
        expected,
        false,
    )
}

/// Run the compact Shell command with real source paths and explicit search roots.
pub(crate) fn run_compact_cli_files_from_env(
    workspace_root: &Path,
    package: &[u8],
    sources: &[(&str, &[u8])],
    module_roots: &[&str],
    include_roots: &[&str],
    expected: Option<&[u8]>,
    bare_entry: bool,
) -> Result<FsUaeSmokeOutcome, String> {
    let args = std::env::var(FS_UAE_ARGS_ENV).map_err(|err| err.to_string())?;
    let binary = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".into());
    if sources.is_empty() || module_roots.len() > 8 || include_roots.len() > 15 {
        return Err("compact CLI requires an entry and bounded search roots".into());
    }
    let valid = |path: &str| {
        !path.is_empty()
            && path.is_ascii()
            && !path.bytes().any(|byte| {
                byte < 32 || byte == 127 || byte == b':' || byte == b'\\' || byte == b'"'
            })
            && Path::new(path)
                .components()
                .all(|part| matches!(part, std::path::Component::Normal(_)))
    };
    if (bare_entry && sources[0].0.contains('/'))
        || sources.iter().any(|(name, _)| !valid(name))
        || module_roots.iter().any(|root| !valid(root))
        || include_roots.iter().any(|root| !valid(root))
    {
        return Err("compact CLI source and search paths must be safe relative paths".into());
    }
    let entry_path = if bare_entry {
        sources[0].0.to_string()
    } else {
        format!("Work:sources/{}", sources[0].0)
    };
    // Existing Hunk probes declare their format in source; the CLI option must
    // state that format rather than sending Hunk bytes through --bin.
    let hunk_output = sources.iter().any(|(_, bytes)| {
        let text = String::from_utf8_lossy(bytes);
        text.lines().any(|line| {
            let line = line.to_ascii_lowercase().replace(' ', "");
            line.contains(".output") && line.contains("format=hunk")
        })
    });
    let quote = |value: &str| format!("\"{}\"", value.replace('*', "**"));
    let mut command = format!(
        "--runtime-package Work:input.bin -i {} {} Work:output.bin",
        quote(&entry_path),
        if hunk_output { "--hunk" } else { "--bin" }
    );
    for root in module_roots {
        command.push_str(&format!(" -M {}", quote(&format!("Work:sources/{root}"))));
    }
    for root in include_roots {
        command.push_str(&format!(" -I {}", quote(&format!("Work:sources/{root}"))));
    }
    let paths = sources
        .iter()
        .enumerate()
        .map(|(index, (name, bytes))| {
            (
                if bare_entry && index == 0 {
                    (*name).to_string()
                } else {
                    format!("sources/{name}")
                },
                *bytes,
            )
        })
        .collect::<Vec<_>>();
    let files = paths
        .iter()
        .map(|(path, bytes)| OpforgeNativeCliGuestFile {
            relative_path: path.as_str(),
            bytes,
        })
        .collect::<Vec<_>>();
    let artifacts = expected.map(|rust_oracle| {
        [OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/output.bin",
            rust_oracle,
        }]
    });
    let memory_telemetry = std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1");
    let progress = std::env::var("OPFORGE_PREPARATION_PROGRESS").as_deref() == Ok("1");
    let mut extra_assembly_defines = exact_harness_assembly_defines(
        NativeCliParityExecutable::CompactCli,
        memory_telemetry,
        progress,
        std::env::var("OPFORGE_PHASE_ONLY").as_deref() != Ok("1"),
    );
    if memory_telemetry && std::env::var("OPFORGE_BINDING_DETAIL").as_deref() == Ok("1") {
        extra_assembly_defines.push("OPFORGE_BINDING_DETAIL_TELEMETRY");
    }
    if memory_telemetry && std::env::var("OPFORGE_TEMPLATE_WORK").as_deref() == Ok("1") {
        extra_assembly_defines.push("OPFORGE_TEMPLATE_WORK_TELEMETRY");
    }
    if memory_telemetry && std::env::var("OPFORGE_INPUT_DETAIL").as_deref() == Ok("1") {
        extra_assembly_defines.push("OPFORGE_INPUT_TELEMETRY");
    }
    if memory_telemetry && std::env::var("OPFORGE_CAPTURE_DETAIL").as_deref() == Ok("1") {
        extra_assembly_defines.push("OPFORGE_CAPTURE_TELEMETRY");
    }
    let case = OpforgeNativeCliParityCase {
        name: "compact-cli-source-set",
        cpu_override: "68020",
        extra_assembly_defines: &extra_assembly_defines,
        source_override: Some(package),
        command_template: Some(&command),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: match &artifacts {
            Some(artifacts) => OpforgeNativeCliProof::ExactArtifacts(artifacts),
            None => OpforgeNativeCliProof::ExpectedFailureContaining(
                "binary source: unsupported or invalid input",
            ),
        },
    };
    run_native_cli_parity_batch_cases(
        workspace_root,
        &binary,
        &args,
        &[case],
        NativeCliParityExecutable::CompactCli,
        None,
    )
}

/// Expected failures retain fresh completion, nonzero exit and diagnostic proof.
pub(crate) fn run_binary_source_rejection_from_env(
    workspace_root: &Path,
    package: &[u8],
    sources: &[(&str, &[u8])],
    diagnostic: Option<&str>,
) -> Result<FsUaeSmokeOutcome, String> {
    run_binary_source_files_from_env(
        workspace_root,
        package,
        sources,
        None,
        diagnostic,
        BinarySourceSearch::ordered(),
    )
}

/// Numeric module graph execution: first file is the entry, remaining files are candidates.
/// Candidates remain in supplied order; the guest builds the dependency order.
pub(crate) fn run_binary_graph_from_env(
    workspace_root: &Path,
    package: &[u8],
    sources: &[(&str, &[u8])],
    expected: Option<&[u8]>,
    diagnostic: Option<&str>,
) -> Result<FsUaeSmokeOutcome, String> {
    run_binary_source_files_from_env(
        workspace_root,
        package,
        sources,
        expected,
        diagnostic,
        BinarySourceSearch {
            graph: true,
            ..BinarySourceSearch::ordered()
        },
    )
}

/// Search for modules from the entry directory and configured module roots;
/// selected files resolve includes from their own directory and include roots.
pub(crate) fn run_binary_discovery_with_includes_from_env(
    workspace_root: &Path,
    package: &[u8],
    sources: &[(&str, &[u8])],
    expected: Option<&[u8]>,
    diagnostic: Option<&str>,
    module_roots: &[&str],
    include_roots: &[&str],
) -> Result<FsUaeSmokeOutcome, String> {
    run_binary_source_files_from_env(
        workspace_root,
        package,
        sources,
        expected,
        diagnostic,
        BinarySourceSearch {
            graph: true,
            module_roots: Some(module_roots),
            include_roots,
        },
    )
}

/// Acquisition settings stay together so ordered, graph and discovery callers
/// cannot accidentally exchange the two search root sets.
struct BinarySourceSearch<'a> {
    graph: bool,
    module_roots: Option<&'a [&'a str]>,
    include_roots: &'a [&'a str],
}

impl BinarySourceSearch<'_> {
    fn ordered() -> Self {
        Self {
            graph: false,
            module_roots: None,
            include_roots: &[],
        }
    }
}

fn run_binary_source_files_from_env(
    workspace_root: &Path,
    package: &[u8],
    sources: &[(&str, &[u8])],
    expected: Option<&[u8]>,
    diagnostic: Option<&str>,
    search: BinarySourceSearch<'_>,
) -> Result<FsUaeSmokeOutcome, String> {
    let BinarySourceSearch {
        graph,
        module_roots,
        include_roots,
    } = search;
    let args = std::env::var(FS_UAE_ARGS_ENV).map_err(|err| err.to_string())?;
    let binary = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".into());
    let count = u16::try_from(sources.len()).map_err(|_| "too many source files")?;
    if count == 0 || count > 0x7fff {
        return Err("source file count must be 1..32767".into());
    }
    let mut input = package.to_vec();
    let discovery = module_roots.is_some();
    let manifest_entries = if let Some(roots) = module_roots {
        u16::try_from(2 + roots.len()).map_err(|_| "too many module search roots")?
    } else {
        count
    };
    if manifest_entries > 0x3fff {
        return Err("too many manifest entries".into());
    }
    let manifest_count =
        manifest_entries | if graph { 0x8000 } else { 0 } | if discovery { 0x4000 } else { 0 };
    input.extend_from_slice(&manifest_count.to_be_bytes());
    let mut paths = Vec::with_capacity(sources.len());
    for (name, _) in sources {
        if name.is_empty()
            || !name.is_ascii()
            || name
                .bytes()
                .any(|byte| byte < 32 || byte == 127 || byte == b':' || byte == b'\\')
            || Path::new(name)
                .components()
                .any(|part| !matches!(part, std::path::Component::Normal(_)))
        {
            return Err(format!("invalid source filename: {name}"));
        }
        let path = format!("sources/{name}");
        if paths.contains(&path) {
            return Err(format!("duplicate source filename: {name}"));
        }
        let guest_path = format!("Work:sources/{name}");
        if guest_path.len() > 255 {
            return Err("source path exceeds 255 bytes".into());
        }
        paths.push(path);
    }
    let manifest_paths = if let Some(roots) = module_roots {
        let entry = format!("Work:{}", paths[0]);
        let parent = Path::new(&paths[0])
            .parent()
            .ok_or("entry has no search directory")?;
        let mut paths = vec![entry, format!("Work:{}", parent.display())];
        for root in roots {
            let relative = Path::new(root);
            if relative.as_os_str().is_empty()
                || relative
                    .components()
                    .any(|part| !matches!(part, std::path::Component::Normal(_)))
                || !root.is_ascii()
                || root
                    .bytes()
                    .any(|byte| byte < 32 || byte == 127 || byte == b':' || byte == b'\\')
            {
                return Err(format!("invalid module search root: {root}"));
            }
            paths.push(format!("Work:sources/{root}"));
        }
        paths
    } else {
        paths.iter().map(|path| format!("Work:{path}")).collect()
    };
    for guest_path in &manifest_paths {
        if guest_path.len() > 255 {
            return Err("manifest path exceeds 255 bytes".into());
        }
        input.extend_from_slice(&(guest_path.len() as u16).to_be_bytes());
        input.extend_from_slice(guest_path.as_bytes());
    }
    if discovery {
        let count = u16::try_from(include_roots.len()).map_err(|_| "too many include roots")?;
        if count > 16 {
            return Err("include roots exceed native limit of 16".into());
        }
        input.extend_from_slice(&count.to_be_bytes());
        for root in include_roots {
            let relative = Path::new(root);
            if relative.as_os_str().is_empty()
                || relative
                    .components()
                    .any(|part| !matches!(part, std::path::Component::Normal(_)))
                || !root.is_ascii()
                || root
                    .bytes()
                    .any(|byte| byte < 32 || byte == 127 || byte == b':' || byte == b'\\')
            {
                return Err(format!("invalid include root: {root}"));
            }
            let guest_path = format!("Work:sources/{root}");
            if guest_path.len() > 255 {
                return Err("include root path exceeds 255 bytes".into());
            }
            input.extend_from_slice(&(guest_path.len() as u16).to_be_bytes());
            input.extend_from_slice(guest_path.as_bytes());
        }
    } else if !include_roots.is_empty() {
        return Err("include roots require discovery mode".into());
    }
    let files = paths
        .iter()
        .zip(sources)
        .map(|(path, (_, bytes))| OpforgeNativeCliGuestFile {
            relative_path: path,
            bytes,
        })
        .collect::<Vec<_>>();
    let expected_artifacts = expected.map(|rust_oracle| {
        [OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/output.bin",
            rust_oracle,
        }]
    });
    let memory_telemetry = std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1");
    let mut extra_assembly_defines = exact_harness_assembly_defines(
        NativeCliParityExecutable::BinarySourceHarness,
        memory_telemetry,
        false,
        std::env::var("OPFORGE_PHASE_ONLY").as_deref() != Ok("1"),
    );
    if std::env::var("OPFORGE_CAPTURE_REPLAY_TEST").as_deref() == Ok("1") {
        extra_assembly_defines.push("OPFORGE_CAPTURE_REPLAY_TEST");
    }
    let case = OpforgeNativeCliParityCase {
        name: "binary-source-files",
        cpu_override: "68020",
        extra_assembly_defines: &extra_assembly_defines,
        source_override: Some(&input),
        command_template: None,
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: match &expected_artifacts {
            Some(artifacts) => OpforgeNativeCliProof::ExactArtifacts(artifacts),
            None => OpforgeNativeCliProof::ExpectedFailureContaining(
                diagnostic.unwrap_or("binary source: unsupported or invalid input"),
            ),
        },
    };
    run_native_cli_parity_batch_cases(
        workspace_root,
        &binary,
        &args,
        &[case],
        NativeCliParityExecutable::BinarySourceHarness,
        None,
    )
}

fn run_exact_harness_from_env(
    workspace_root: &Path,
    case_bytes: &[u8],
    rust_oracle: &[u8],
    artifact_path: &'static str,
    executable: NativeCliParityExecutable,
) -> Result<FsUaeSmokeOutcome, String> {
    run_exact_harness_with_defines_from_env(
        workspace_root,
        case_bytes,
        rust_oracle,
        artifact_path,
        executable,
        &[],
    )
}

fn run_exact_harness_with_defines_from_env(
    workspace_root: &Path,
    case_bytes: &[u8],
    rust_oracle: &[u8],
    artifact_path: &'static str,
    executable: NativeCliParityExecutable,
    extra_defines: &[&'static str],
) -> Result<FsUaeSmokeOutcome, String> {
    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; configure FS-UAE for the native harness"
            )))
        }
    };
    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let expected = [OpforgeNativeCliExpectedArtifact {
        relative_path: artifact_path,
        rust_oracle,
    }];
    let memory_telemetry = std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1");
    let mut extra_assembly_defines = exact_harness_assembly_defines(
        executable,
        memory_telemetry,
        false,
        std::env::var("OPFORGE_PHASE_ONLY").as_deref() != Ok("1"),
    );
    extra_assembly_defines.extend_from_slice(extra_defines);
    let case = OpforgeNativeCliParityCase {
        name: "native-harness-live-oracle",
        cpu_override: "68020",
        extra_assembly_defines: &extra_assembly_defines,
        source_override: Some(case_bytes),
        command_template: None,
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifacts(&expected),
    };
    run_native_cli_parity_batch_cases(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        &[case],
        executable,
        None,
    )
}

fn exact_harness_assembly_defines(
    executable: NativeCliParityExecutable,
    memory_telemetry: bool,
    preparation_progress: bool,
    token_detail: bool,
) -> Vec<&'static str> {
    if memory_telemetry
        && matches!(
            executable,
            NativeCliParityExecutable::BinarySourceHarness | NativeCliParityExecutable::CompactCli
        )
    {
        let mut defines = vec!["OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY"];
        // Phase-only profiling keeps the coarse clocks without per-token probes.
        // Detailed accounting remains the default for existing memory tests.
        if token_detail {
            defines.push("OPFORGE_TOKEN_DETAIL_TELEMETRY");
        }
        if preparation_progress && matches!(executable, NativeCliParityExecutable::CompactCli) {
            defines.push("OPFORGE_PREPARATION_PROGRESS");
        }
        defines
    } else {
        Vec::new()
    }
}

fn verify_native_cli_case_proof(
    case: &OpforgeNativeCliParityCase<'_>,
    run: &mut FsUaeSmokeRun,
) -> Result<(), String> {
    verify_native_cli_case_proof_inner(case, run).map_err(|error| {
        // Reuse the existing opt-in mismatch directory for fresh object
        // diagnostics when failure occurs before an output can be compared.
        // This retains captured bytes only; proof and ephemeral cleanup stay
        // unchanged.
        if let Some(directory) = std::env::var_os("OPFORGE_NATIVE_MISMATCH_DIR") {
            let directory = PathBuf::from(directory).join(case.name).join("objects");
            for (path, bytes) in &run.captured_artifacts {
                if path.extension().is_some_and(|extension| extension == "ofm") {
                    if let Err(error) = fs::create_dir_all(&directory).and_then(|()| {
                        fs::write(directory.join(path.file_name().unwrap()), bytes)
                    }) {
                        eprintln!("could not retain fresh module diagnostic: {error}");
                    }
                }
            }
        }
        // Failed budget runs must retain their environment reading too. The
        // ephemeral Work tree is removed before the caller receives this error.
        let error = if let Some(memory) = run.captured_artifacts.get(&PathBuf::from("Work/guest-memory.txt")) {
            format!("{error}\nguest memory before launch:\n{}", String::from_utf8_lossy(memory))
        } else {
            error
        };
        match run.start_to_done_host_seconds.filter(|_| run.protocol_completed) {
            Some(seconds) => format!(
                "{error}\ncase timing: start_to_done_host_seconds={seconds:.9}; guest_exit={:?}; proof_verified=false (host observation of fresh START/DONE)",
                run.exit_code
            ),
            None => error,
        }
    })
}

fn verify_native_cli_case_proof_inner(
    case: &OpforgeNativeCliParityCase<'_>,
    run: &mut FsUaeSmokeRun,
) -> Result<(), String> {
    fn protocol_artifact<'a>(
        case_name: &str,
        run: &'a FsUaeSmokeRun,
        file_name: &str,
    ) -> Result<&'a [u8], String> {
        let path = PathBuf::from(FS_UAE_MOUNTED_WORK_DIR_NAME)
            .join(FS_UAE_OPFORGE_NATIVE_CLI_CASE_ARTIFACTS_DIR)
            .join(opforge_native_cli_batch_case_name(0))
            .join(file_name);
        run.captured_artifacts
            .get(&path)
            .map(Vec::as_slice)
            .ok_or_else(|| {
                format!(
                    "FS-UAE proof for {} is invalid: isolated guest artifact {} is missing",
                    case_name,
                    path.display()
                )
            })
    }

    if !run.protocol_completed {
        return Err(format!(
            "FS-UAE proof for {} is invalid: the exact fresh start/done challenge and guest exit evidence were not all present\nstdout:\n{}\nstderr:\n{}",
            case.name, run.stdout, run.stderr
        ));
    }

    match case.proof {
        OpforgeNativeCliProof::ExactArtifact {
            relative_path,
            rust_oracle,
        } => verify_exact_native_cli_artifacts(
            case,
            run,
            &[OpforgeNativeCliExpectedArtifact {
                relative_path,
                rust_oracle,
            }],
        ),
        OpforgeNativeCliProof::ExactArtifacts(artifacts) => {
            verify_exact_native_cli_artifacts(case, run, artifacts)
        }
        OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(paths) => {
            if paths.is_empty() || !run.success || run.exit_code != Some(0) {
                return Err(format!(
                    "FS-UAE absence proof for {} requires named paths and fresh exit zero",
                    case.name
                ));
            }
            for path in paths {
                if run.captured_artifacts.contains_key(&PathBuf::from(path)) {
                    return Err(format!(
                        "FS-UAE absence proof for {} unexpectedly produced {path}",
                        case.name
                    ));
                }
            }
            Ok(())
        }
        OpforgeNativeCliProof::ExactStdoutLines {
            prefix,
            rust_oracle,
        } => {
            if !run.success || run.exit_code != Some(0) {
                return Err(format!(
                    "FS-UAE proof for {} is invalid: guest exit was {:?}, expected exactly 0\nstdout:\n{}\nstderr:\n{}",
                    case.name, run.exit_code, run.stdout, run.stderr
                ));
            }
            let guest_stdout = std::str::from_utf8(protocol_artifact(
                case.name,
                run,
                FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDOUT_FILE,
            )?)
            .map_err(|error| {
                format!(
                    "FS-UAE proof for {} has non-UTF-8 isolated guest stdout: {error}",
                    case.name
                )
            })?;
            let actual = guest_stdout
                .lines()
                .filter(|line| line.starts_with(prefix))
                .collect::<Vec<_>>()
                .join("\n")
                .into_bytes();
            if actual != rust_oracle {
                return Err(format!(
                    "FS-UAE stdout proof for {} differs from the in-memory Rust oracle: {}",
                    case.name,
                    describe_first_byte_mismatch(&actual, rust_oracle)
                ));
            }
            run.verified_output = Some(actual);
            Ok(())
        }
        OpforgeNativeCliProof::SuccessfulExitContaining(text) => {
            if !run.success || run.exit_code != Some(0) {
                return Err(format!(
                    "FS-UAE informational proof for {} requires exactly exit 0, got {:?}",
                    case.name, run.exit_code
                ));
            }
            let stdout =
                protocol_artifact(case.name, run, FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDOUT_FILE)?;
            if !String::from_utf8_lossy(stdout).contains(text) {
                return Err(format!(
                    "FS-UAE informational proof for {} is missing required live stdout {text:?}",
                    case.name
                ));
            }
            run.verified_output = Some(stdout.to_vec());
            Ok(())
        }
        OpforgeNativeCliProof::ExpectedFailureWithDiagnostic
        | OpforgeNativeCliProof::ExpectedFailureContaining(_) => {
            if run.exit_code == Some(0) || run.exit_code.is_none() {
                return Err(format!(
                    "FS-UAE negative proof for {} is invalid: guest exit was {:?}, expected a completed nonzero exit",
                    case.name, run.exit_code
                ));
            }
            let guest_stdout = String::from_utf8_lossy(protocol_artifact(
                case.name,
                run,
                FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDOUT_FILE,
            )?);
            let guest_stderr = String::from_utf8_lossy(protocol_artifact(
                case.name,
                run,
                FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDERR_FILE,
            )?);
            let combined = format!("{guest_stdout}\n{guest_stderr}");
            if combined.trim().is_empty() {
                return Err(format!(
                    "FS-UAE negative proof for {} is invalid: completed guest failure produced no diagnostic output",
                    case.name
                ));
            }
            if let OpforgeNativeCliProof::ExpectedFailureContaining(diagnostic) = case.proof {
                if !combined.contains(diagnostic) {
                    return Err(format!(
                        "FS-UAE negative proof for {} is invalid: completed guest output did not contain required diagnostic {diagnostic:?}\nstdout:\n{}\nstderr:\n{}",
                        case.name, run.stdout, run.stderr
                    ));
                }
            }
            Ok(())
        }
    }
}

fn verify_exact_native_cli_artifacts(
    case: &OpforgeNativeCliParityCase<'_>,
    run: &mut FsUaeSmokeRun,
    artifacts: &[OpforgeNativeCliExpectedArtifact<'_>],
) -> Result<(), String> {
    if artifacts.is_empty() {
        return Err(format!(
            "FS-UAE proof for {} is invalid: exact artifact proof declared no artifacts",
            case.name
        ));
    }
    if !run.success || run.exit_code != Some(0) {
        let captured = run
            .captured_artifacts
            .keys()
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>()
            .join(", ");
        return Err(format!(
            "FS-UAE proof for {} is invalid: guest exit was {:?}, expected exactly 0; fresh captured paths: [{}]\nstdout:\n{}\nstderr:\n{}",
            case.name, run.exit_code, captured, run.stdout, run.stderr
        ));
    }

    let mut errors = Vec::new();
    let mut first_verified = None;
    for artifact in artifacts {
        let relative_output_path = PathBuf::from(artifact.relative_path);
        let Some(actual) = run.captured_artifacts.get(&relative_output_path).cloned() else {
            errors.push(format!(
                "did not produce required output {}",
                relative_output_path.display()
            ));
            continue;
        };
        if actual != artifact.rust_oracle {
            #[cfg(test)]
            if let Err(error) = save_mismatch_diagnostic(
                case.name,
                &relative_output_path,
                &actual,
                artifact.rust_oracle,
            ) {
                errors.push(error);
            }
            errors.push(format!(
                "{}: native output ({} bytes) differs from the in-memory Rust oracle ({} bytes); {}",
                relative_output_path.display(),
                actual.len(),
                artifact.rust_oracle.len(),
                describe_artifact_mismatch(&actual, artifact.rust_oracle)
            ));
            continue;
        }
        if first_verified.is_none() {
            first_verified = Some(actual);
        }
    }
    if !errors.is_empty() {
        let captured = run
            .captured_artifacts
            .keys()
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>()
            .join(", ");
        return Err(format!(
            "FS-UAE exact artifact proof failed for {} after checking all {} declared artifacts:\n{}\nfresh captured paths: [{}]\nstdout:\n{}\nstderr:\n{}",
            case.name,
            artifacts.len(),
            errors.join("\n"),
            captured,
            run.stdout,
            run.stderr,
        ));
    }
    run.verified_output = first_verified;
    Ok(())
}

// Optional test diagnostics copy only the freshly captured mismatched bytes.
// The normal run tree is still removed and exact proof still fails unchanged.
#[cfg(test)]
fn save_mismatch_diagnostic(
    case_name: &str,
    relative_path: &Path,
    actual: &[u8],
    expected: &[u8],
) -> Result<(), String> {
    let Some(directory) = std::env::var_os("OPFORGE_NATIVE_MISMATCH_DIR") else {
        return Ok(());
    };
    let path = PathBuf::from(directory).join(case_name).join(relative_path);
    let parent = path.parent().ok_or("mismatch diagnostic has no parent")?;
    fs::create_dir_all(parent)
        .map_err(|error| format!("create mismatch diagnostic {}: {error}", parent.display()))?;
    for (suffix, bytes) in [("native", actual), ("rust", expected)] {
        let output = path.with_extension(suffix);
        fs::write(&output, bytes)
            .map_err(|error| format!("write mismatch diagnostic {}: {error}", output.display()))?;
    }
    eprintln!("fresh mismatch diagnostics: {}", path.display());
    Ok(())
}

fn describe_first_byte_mismatch(actual: &[u8], expected: &[u8]) -> String {
    let shared_len = actual.len().min(expected.len());
    if let Some(offset) = (0..shared_len).find(|&offset| actual[offset] != expected[offset]) {
        let last_offset = (0..shared_len)
            .rfind(|&candidate| actual[candidate] != expected[candidate])
            .expect("a first mismatch implies a last mismatch");
        let difference_count = actual
            .iter()
            .zip(expected.iter())
            .filter(|(native, rust)| native != rust)
            .count();
        let count_text = if actual.len() == expected.len() {
            format!("{difference_count} differing byte(s); ")
        } else {
            String::new()
        };
        let last_text = if last_offset == offset {
            String::new()
        } else {
            format!(
                "; last mismatch at offset {last_offset}: native={:#04x}, Rust={:#04x}",
                actual[last_offset], expected[last_offset]
            )
        };
        return format!(
            "{count_text}first mismatch at offset {offset}: native={:#04x}, Rust={:#04x}{last_text}",
            actual[offset], expected[offset],
        );
    }
    if actual.len() < expected.len() {
        return format!(
            "native output ends at offset {shared_len}; next Rust byte is {:#04x}",
            expected[shared_len]
        );
    }
    if expected.len() < actual.len() {
        return format!(
            "Rust output ends at offset {shared_len}; next native byte is {:#04x}",
            actual[shared_len]
        );
    }
    "outputs are equal".to_string()
}

fn first_byte_difference(actual: &[u8], expected: &[u8]) -> usize {
    actual
        .iter()
        .zip(expected)
        .position(|(native, rust)| native != rust)
        .unwrap_or(actual.len().min(expected.len()))
}

fn describe_byte_window(bytes: &[u8], offset: usize) -> String {
    let start = offset.saturating_sub(16).min(bytes.len());
    let end = offset.saturating_add(17).min(bytes.len());
    let hex = bytes[start..end]
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect::<Vec<_>>()
        .join(" ");
    format!("[{start}..{end}): {hex}")
}

fn describe_hunk_segments(segments: &[hunk::Segment<'_>]) -> String {
    let mut summaries = segments
        .iter()
        .enumerate()
        .take(16)
        .map(|(index, segment)| {
            let kind = match segment.kind {
                0x3e9 => "CODE",
                0x3ea => "DATA",
                0x3eb => "BSS",
                _ => unreachable!(),
            };
            format!(
                "segment {index} {kind} allocation={} body={} reloc32={}",
                segment.reserved_bytes,
                segment.payload.len(),
                segment.relocations.len()
            )
        })
        .collect::<Vec<_>>();
    if segments.len() > 16 {
        summaries.push(format!("{} further segments omitted", segments.len() - 16));
    }
    format!("{} segments; {}", segments.len(), summaries.join("; "))
}

fn describe_artifact_mismatch(actual: &[u8], expected: &[u8]) -> String {
    let raw = describe_first_byte_mismatch(actual, expected);
    let (Ok(native), Ok(rust)) = (hunk::segments(actual), hunk::segments(expected)) else {
        let offset = first_byte_difference(actual, expected);
        return format!(
            "{raw}\nnative byte window {}\nRust byte window {}",
            describe_byte_window(actual, offset),
            describe_byte_window(expected, offset)
        );
    };
    let mut details = format!(
        "{raw}\nHunk native: {}\nHunk Rust: {}",
        describe_hunk_segments(&native),
        describe_hunk_segments(&rust)
    );
    for (index, (native, rust)) in native.iter().zip(&rust).enumerate().take(16) {
        if native.payload != rust.payload {
            let offset = first_byte_difference(native.payload, rust.payload);
            details.push_str(&format!(
                "\nHunk payload difference: segment={index}; payload_offset={offset}; native_file_offset={}; Rust_file_offset={}; {}\nnative payload window {}\nRust payload window {}",
                native.payload_offset + offset,
                rust.payload_offset + offset,
                describe_first_byte_mismatch(native.payload, rust.payload),
                describe_byte_window(native.payload, offset),
                describe_byte_window(rust.payload, offset)
            ));
        }
        let mut native_relocations = native.relocations.clone();
        let mut rust_relocations = rust.relocations.clone();
        native_relocations.sort_unstable();
        rust_relocations.sort_unstable();
        if native_relocations != rust_relocations {
            let record = native_relocations
                .iter()
                .zip(&rust_relocations)
                .position(|(native, rust)| native != rust)
                .unwrap_or(native_relocations.len().min(rust_relocations.len()));
            details.push_str(&format!(
                "\nHunk relocation identities differ: segment={index}; first_sorted_record={record}; native={:?}; Rust={:?} (payload_offset, target_segment)",
                native_relocations.get(record), rust_relocations.get(record)
            ));
        }
    }
    details
}

#[derive(Debug, Clone)]
struct OpforgeNativeCliBatchCasePaths {
    artifact_dir: PathBuf,
    protocol_dir: PathBuf,
    captured_relative_prefix: PathBuf,
    stdout_path: PathBuf,
    stderr_path: PathBuf,
    exit_code_path: PathBuf,
    started_path: PathBuf,
    done_path: PathBuf,
    expected_started: String,
    expected_done: String,
    guest_artifact_dir: String,
    command_guest_work_dir: String,
}

fn opforge_native_cli_batch_case_name(index: usize) -> String {
    format!("case_{index:04}")
}

fn opforge_native_cli_batch_case_paths(
    mounted_work_dir: &Path,
    index: usize,
    run_challenge: &str,
    case_identity: &str,
) -> OpforgeNativeCliBatchCasePaths {
    let case_name = opforge_native_cli_batch_case_name(index);
    // The coordinator invokes this function with exactly one case per fresh
    // artifact tree. Treat that mounted Work volume as the case boundary so
    // absolute AmigaDOS Work: outputs and relative outputs are equally
    // isolated from every other parity case.
    let captured_relative_prefix = PathBuf::from(FS_UAE_MOUNTED_WORK_DIR_NAME);
    let artifact_dir = mounted_work_dir.to_path_buf();
    let protocol_dir = artifact_dir
        .join(FS_UAE_OPFORGE_NATIVE_CLI_CASE_ARTIFACTS_DIR)
        .join(case_name.as_str());
    let stdout_path = protocol_dir.join(FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDOUT_FILE);
    let stderr_path = protocol_dir.join(FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDERR_FILE);
    let exit_code_path = protocol_dir.join(FS_UAE_OPFORGE_NATIVE_CLI_CASE_EXITCODE_FILE);
    let started_path = protocol_dir.join(FS_UAE_OPFORGE_NATIVE_CLI_CASE_STARTED_FILE);
    let done_path = protocol_dir.join(FS_UAE_OPFORGE_NATIVE_CLI_CASE_DONE_FILE);
    let guest_artifact_dir =
        format!("Work:{FS_UAE_OPFORGE_NATIVE_CLI_CASE_ARTIFACTS_DIR}/{case_name}");
    let command_guest_work_dir = "Work:".to_string();
    let expected_started = format!("OPFORGE-FS-UAE-PROOF-V1 START {run_challenge} {case_identity}");
    let expected_done = format!("OPFORGE-FS-UAE-PROOF-V1 DONE {run_challenge} {case_identity}");
    OpforgeNativeCliBatchCasePaths {
        artifact_dir,
        protocol_dir,
        captured_relative_prefix,
        stdout_path,
        stderr_path,
        exit_code_path,
        started_path,
        done_path,
        expected_started,
        expected_done,
        guest_artifact_dir,
        command_guest_work_dir,
    }
}

fn opforge_native_cli_case_captured_artifacts(
    captured_artifacts: &BTreeMap<PathBuf, Vec<u8>>,
    case_paths: &OpforgeNativeCliBatchCasePaths,
) -> BTreeMap<PathBuf, Vec<u8>> {
    let mut selected = captured_artifacts
        .iter()
        .filter_map(|(path, bytes)| {
            path.strip_prefix(&case_paths.captured_relative_prefix)
                .ok()
                .map(|relative| {
                    (
                        PathBuf::from(FS_UAE_MOUNTED_WORK_DIR_NAME).join(relative),
                        bytes.clone(),
                    )
                })
        })
        .collect::<BTreeMap<_, _>>();
    // Bounded component accounting is diagnostic data, never an artifact oracle.
    for file_name in ["memory.bin", "guest-memory.txt", "metrics.bin"] {
        let path = PathBuf::from(FS_UAE_MOUNTED_WORK_DIR_NAME).join(file_name);
        if let Some(bytes) = captured_artifacts.get(&path) {
            selected.insert(path, bytes.clone());
        }
    }
    selected
}

fn fnv1a64_update(mut state: u64, bytes: &[u8]) -> u64 {
    for byte in bytes {
        state ^= u64::from(*byte);
        state = state.wrapping_mul(0x0000_0100_0000_01b3);
    }
    state
}

fn opforge_native_cli_case_identity(
    case: &OpforgeNativeCliParityCase<'_>,
    resolved_package_bytes: Option<&[u8]>,
    executable: NativeCliParityExecutable,
    bootstrap_executable: Option<&[u8]>,
) -> String {
    let mut state = 0xcbf2_9ce4_8422_2325;
    state = fnv1a64_update(
        state,
        match executable {
            NativeCliParityExecutable::ExprvmI64Harness => b"exprvm-i64-harness",
            NativeCliParityExecutable::TkvmBranchHarness => b"tkvm-branch-harness",
            NativeCliParityExecutable::PrvmMacroHarness => b"prvm-macro-harness",

            NativeCliParityExecutable::BinarySourceHarness => b"binary-source-harness",
            NativeCliParityExecutable::CompactCli => b"compact-cli",
        },
    );
    state = fnv1a64_update(state, &[0]);
    state = fnv1a64_update(state, case.cpu_override.as_bytes());
    state = fnv1a64_update(state, &[0]);
    state = fnv1a64_update(state, case.source_override.unwrap_or_default());
    state = fnv1a64_update(state, &[0]);
    if let Some(command_template) = case.command_template {
        state = fnv1a64_update(state, command_template.as_bytes());
    }
    for define in case.extra_assembly_defines {
        state = fnv1a64_update(state, &[0]);
        state = fnv1a64_update(state, define.as_bytes());
    }
    state = fnv1a64_update(state, &[0]);
    state = fnv1a64_update(state, resolved_package_bytes.unwrap_or_default());
    state = fnv1a64_update(state, &[0]);
    state = fnv1a64_update(state, bootstrap_executable.unwrap_or_default());
    for file in case.extra_guest_files {
        state = fnv1a64_update(state, &[0]);
        state = fnv1a64_update(state, file.relative_path.as_bytes());
        state = fnv1a64_update(state, &[0]);
        state = fnv1a64_update(state, file.bytes);
    }
    state = fnv1a64_update(state, &[0]);
    match case.proof {
        OpforgeNativeCliProof::ExactArtifact {
            relative_path,
            rust_oracle,
        } => {
            state = fnv1a64_update(state, b"exact-artifact");
            state = fnv1a64_update(state, &[0]);
            state = fnv1a64_update(state, relative_path.as_bytes());
            state = fnv1a64_update(state, &[0]);
            state = fnv1a64_update(state, rust_oracle);
        }
        OpforgeNativeCliProof::ExactArtifacts(artifacts) => {
            state = fnv1a64_update(state, b"exact-artifacts");
            for artifact in artifacts {
                state = fnv1a64_update(state, &[0]);
                state = fnv1a64_update(state, artifact.relative_path.as_bytes());
                state = fnv1a64_update(state, &[0]);
                state = fnv1a64_update(state, artifact.rust_oracle);
            }
        }
        OpforgeNativeCliProof::ExactStdoutLines {
            prefix,
            rust_oracle,
        } => {
            state = fnv1a64_update(state, b"exact-stdout-lines");
            state = fnv1a64_update(state, &[0]);
            state = fnv1a64_update(state, prefix.as_bytes());
            state = fnv1a64_update(state, &[0]);
            state = fnv1a64_update(state, rust_oracle);
        }
        OpforgeNativeCliProof::ExpectedFailureWithDiagnostic => {
            state = fnv1a64_update(state, b"failure-with-diagnostic");
        }
        OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(paths) => {
            state = fnv1a64_update(state, b"successful-exit-without-artifacts");
            for path in paths {
                state = fnv1a64_update(state, &[0]);
                state = fnv1a64_update(state, path.as_bytes());
            }
        }
        OpforgeNativeCliProof::SuccessfulExitContaining(text) => {
            state = fnv1a64_update(state, b"successful-exit-containing");
            state = fnv1a64_update(state, &[0]);
            state = fnv1a64_update(state, text.as_bytes());
        }
        OpforgeNativeCliProof::ExpectedFailureContaining(diagnostic) => {
            state = fnv1a64_update(state, b"failure-containing");
            state = fnv1a64_update(state, &[0]);
            state = fnv1a64_update(state, diagnostic.as_bytes());
        }
    }
    format!("{state:016x}")
}

fn opforge_native_cli_run_challenge() -> String {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!("{:x}-{nanos:x}", std::process::id())
}

fn stage_guest_script(mounted_work_dir: &Path, script_text: &str) -> Result<(), String> {
    stage_guest_input_bytes(
        mounted_work_dir,
        FS_UAE_STARTUP_HUNK_ALIAS,
        script_text.as_bytes(),
    )?;
    stage_guest_input_bytes(
        mounted_work_dir,
        FS_UAE_STARTUP_HUNK_ALIAS_UAEM,
        FS_UAE_SCRIPT_UAEM_TEXT.as_bytes(),
    )
}

fn native_cli_guest_batch_prefix(stack_bytes: Option<&str>) -> Result<String, String> {
    let mut script = String::from("FailAt 999\n");
    if let Some(value) = stack_bytes {
        let bytes = value.parse::<u32>().ok().filter(|bytes| {
            !value.is_empty()
                && value.bytes().all(|byte| byte.is_ascii_digit())
                && (4096..=1_048_576).contains(bytes)
                && bytes % 16 == 0
        }).ok_or_else(|| {
            "OPFORGE_FS_UAE_GUEST_STACK_BYTES must be decimal bytes in 4096..1048576, aligned to 16".to_owned()
        })?;
        script.push_str(&format!("Stack {bytes}\n"));
    }
    script.push_str("CD Work:\n");
    Ok(script)
}

fn native_cli_guest_batch_prefix_from_env() -> Result<String, String> {
    let stack_bytes = std::env::var("OPFORGE_FS_UAE_GUEST_STACK_BYTES")
        .map(Some)
        .or_else(|error| match error {
            std::env::VarError::NotPresent => Ok(None),
            _ => Err("OPFORGE_FS_UAE_GUEST_STACK_BYTES must be UTF-8".to_owned()),
        })?;
    native_cli_guest_batch_prefix(stack_bytes.as_deref())
}

fn run_native_cli_parity_batch_cases(
    workspace_root: &Path,
    fs_uae_bin: &str,
    args_text: &str,
    cases: &[OpforgeNativeCliParityCase<'_>],
    executable: NativeCliParityExecutable,
    bootstrap_executable: Option<&[u8]>,
) -> Result<FsUaeSmokeOutcome, String> {
    let batch_prefix = native_cli_guest_batch_prefix_from_env()?;
    if std::env::var("OPFORGE_NATIVE_CORPUS_LIVE_CAPTURE").as_deref() == Ok("1") {
        return Err("OPFORGE_NATIVE_CORPUS_LIVE_CAPTURE uses the retired OFPR progress protocol; use the current console-debugger tools for inspection".into());
    }
    let prebuilt_compact_cli = if matches!(executable, NativeCliParityExecutable::CompactCli) {
        bootstrap_executable
    } else {
        None
    };
    if cases.len() > 1 {
        let mut runs = Vec::with_capacity(cases.len());
        let mut proof_errors = Vec::new();
        for case in cases {
            match run_native_cli_parity_batch_cases(
                workspace_root,
                fs_uae_bin,
                args_text,
                std::slice::from_ref(case),
                executable,
                bootstrap_executable,
            ) {
                Err(error) => proof_errors.push(format!("{}: {error}", case.name)),
                Ok(FsUaeSmokeOutcome::Completed { runs: case_runs }) => runs.extend(case_runs),
                Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                    return Ok(FsUaeSmokeOutcome::Skipped(reason));
                }
            }
        }
        if !proof_errors.is_empty() {
            return Err(format!(
                "{} of {} FS-UAE cases failed their proof contract after every case was attempted:\n{}",
                proof_errors.len(),
                cases.len(),
                proof_errors.join("\n")
            ));
        }
        return Ok(FsUaeSmokeOutcome::Completed { runs });
    }

    let example_name = match executable {
        NativeCliParityExecutable::ExprvmI64Harness => "exprvm_i64_harness",
        NativeCliParityExecutable::TkvmBranchHarness => "tkvm_branch_harness",
        NativeCliParityExecutable::PrvmMacroHarness => "prvm_macro_harness",

        NativeCliParityExecutable::BinarySourceHarness => "binary_source_harness",
        NativeCliParityExecutable::CompactCli => "opforge",
    };
    let source_path = workspace_root.join(match executable {
        NativeCliParityExecutable::TkvmBranchHarness => {
            "native/motorola68000/amigaos/tests/tkvm/tkvm_branch_harness.asm"
        }
        NativeCliParityExecutable::PrvmMacroHarness => {
            "native/motorola68000/amigaos/tests/prvm/prvm_macro_harness.asm"
        }
        NativeCliParityExecutable::ExprvmI64Harness => {
            "native/motorola68000/amigaos/tests/exprvm/exprvm_i64_harness.asm"
        }

        NativeCliParityExecutable::BinarySourceHarness => {
            "native/motorola68000/amigaos/tests/assembler/source_harness.asm"
        }
        NativeCliParityExecutable::CompactCli => "native/motorola68000/amigaos/main.asm",
    });
    if !source_path.is_file() {
        return Err(format!(
            "expected FS-UAE smoke example source at {}",
            source_path.display()
        ));
    }

    let artifact_dir = create_artifact_dir(workspace_root, "fs-uae-hunk-smoke-native")?;
    let ephemeral_artifact_dir = EphemeralArtifactDir(artifact_dir.clone());
    let mounted_work_dir = artifact_dir.join(FS_UAE_MOUNTED_WORK_DIR_NAME);
    fs::create_dir_all(mounted_work_dir.join("build")).map_err(|err| {
        format!(
            "create mounted Work directory {}: {err}",
            mounted_work_dir.display(),
        )
    })?;

    // Rust resolves source-owned relative output paths from the invocation
    // directory.  Make the mounted Work volume that same authority on AmigaOS
    // before invoking any absolute `Work:` executable path.
    let mut batch_script = batch_prefix;
    let mut batch_paths = Vec::with_capacity(cases.len());
    let run_challenge = opforge_native_cli_run_challenge();
    for (index, case) in cases.iter().enumerate() {
        if case.cpu_override != "68020" {
            return Err(format!(
                "native opForge CLI FS-UAE batch runner currently supports only cpu_override=68020, got {} for case {index}",
                case.cpu_override
            ));
        }
        let case_identity =
            opforge_native_cli_case_identity(case, None, executable, bootstrap_executable);
        let case_paths = opforge_native_cli_batch_case_paths(
            &mounted_work_dir,
            index,
            run_challenge.as_str(),
            case_identity.as_str(),
        );
        fs::create_dir_all(&case_paths.protocol_dir).map_err(|err| {
            format!(
                "create native CLI batch case protocol directory {}: {err}",
                case_paths.protocol_dir.display()
            )
        })?;
        match executable {
            NativeCliParityExecutable::ExprvmI64Harness => {
                if cases.len() != 1 {
                    return Err(
                        "ExprVM scalar harness requires one dynamically generated batch".into(),
                    );
                }
                stage_guest_input_bytes(
                    &mounted_work_dir,
                    "exprvm-i64-cases.bin",
                    case.source_override
                        .ok_or("ExprVM scalar harness requires case bytes")?,
                )?;
            }
            NativeCliParityExecutable::PrvmMacroHarness => {
                if cases.len() != 1 {
                    return Err("macro descriptor harness requires one live batch".into());
                }
                stage_guest_input_bytes(
                    &mounted_work_dir,
                    "prvm-macro-cases.bin",
                    case.source_override
                        .ok_or("macro descriptor harness requires case bytes")?,
                )?;
            }
            NativeCliParityExecutable::TkvmBranchHarness => {
                if cases.len() != 1 {
                    return Err("tokenizer branch harness requires one batch".into());
                }
                stage_guest_input_bytes(
                    &mounted_work_dir,
                    "tkvm-branch-cases.bin",
                    case.source_override
                        .ok_or("tokenizer branch harness requires case bytes")?,
                )?;
            }

            NativeCliParityExecutable::BinarySourceHarness
            | NativeCliParityExecutable::CompactCli => {
                if cases.len() != 1 {
                    return Err("binary source harness requires one live input".into());
                }
                stage_guest_input_bytes(
                    &mounted_work_dir,
                    "input.bin",
                    case.source_override
                        .ok_or("binary source harness requires input bytes")?,
                )?;
                for file in case.extra_guest_files {
                    stage_guest_input_bytes(&mounted_work_dir, file.relative_path, file.bytes)?;
                }
            }
        }
        let command = match executable {
            NativeCliParityExecutable::TkvmBranchHarness => {
                "Work:build/tkvm_branch_harness".to_string()
            }
            NativeCliParityExecutable::PrvmMacroHarness => {
                "Work:build/prvm_macro_harness".to_string()
            }
            NativeCliParityExecutable::ExprvmI64Harness => {
                "Work:build/exprvm_i64_harness".to_string()
            }

            NativeCliParityExecutable::BinarySourceHarness => {
                "Work:build/binary_source_harness".to_string()
            }
            NativeCliParityExecutable::CompactCli => {
                format!(
                    "Work:build/opforge {}",
                    case.command_template
                        .ok_or("compact CLI requires an explicit source command")?
                )
            }
        };
        if matches!(
            executable,
            NativeCliParityExecutable::BinarySourceHarness | NativeCliParityExecutable::CompactCli
        ) && (case
            .extra_assembly_defines
            .contains(&"OPFORGE_MEMORY_TELEMETRY")
            || (matches!(executable, NativeCliParityExecutable::CompactCli)
                && fs_uae_memory_profile_from_env()? == FsUaeMemoryProfile::Budget8MiB68020))
        {
            batch_script.push_str("C:Version >Work:guest-memory.txt\n");
            batch_script.push_str("C:CPU >>Work:guest-memory.txt\n");
            batch_script.push_str("Stack >>Work:guest-memory.txt\n");
            batch_script.push_str("C:Avail >>Work:guest-memory.txt\n");
        }
        batch_script.push_str("Echo \"");
        batch_script.push_str(case_paths.expected_started.as_str());
        batch_script.push_str("\" >");
        batch_script.push_str(
            format!(
                "{}/{}",
                case_paths.guest_artifact_dir, FS_UAE_OPFORGE_NATIVE_CLI_CASE_STARTED_FILE
            )
            .as_str(),
        );
        batch_script.push('\n');
        batch_script.push_str(command.as_str());
        batch_script.push_str(" >");
        batch_script.push_str(
            format!(
                "{}/{}",
                case_paths.guest_artifact_dir, FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDOUT_FILE
            )
            .as_str(),
        );
        batch_script.push_str(" *>");
        batch_script.push_str(
            format!(
                "{}/{}",
                case_paths.guest_artifact_dir, FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDERR_FILE
            )
            .as_str(),
        );
        batch_script.push('\n');
        batch_script.push_str("Echo $RC >");
        batch_script.push_str(
            format!(
                "{}/{}",
                case_paths.guest_artifact_dir, FS_UAE_OPFORGE_NATIVE_CLI_CASE_EXITCODE_FILE
            )
            .as_str(),
        );
        batch_script.push('\n');
        batch_script.push_str("Echo \"");
        batch_script.push_str(case_paths.expected_done.as_str());
        batch_script.push_str("\" >");
        batch_script.push_str(
            format!(
                "{}/{}",
                case_paths.guest_artifact_dir, FS_UAE_OPFORGE_NATIVE_CLI_CASE_DONE_FILE
            )
            .as_str(),
        );
        batch_script.push('\n');
        batch_paths.push(case_paths);
    }
    let assembly_defines = opforge_native_cli_case_assembly_defines(&cases[0]);
    let include_paths = example_include_paths(workspace_root, example_name);
    let module_paths = example_module_paths(workspace_root, example_name);
    let hunk_path = if let Some(bytes) = prebuilt_compact_cli {
        let hunk_path = generated_hunk_artifact_path(&artifact_dir, example_name);
        if let Some(parent) = hunk_path.parent() {
            fs::create_dir_all(parent).map_err(|err| {
                format!(
                    "create canonical prebuilt Hunk directory {}: {err}",
                    parent.display()
                )
            })?;
        }
        fs::write(&hunk_path, bytes).map_err(|err| {
            format!(
                "stage prebuilt compact CLI as canonical Hunk at {}: {err}",
                hunk_path.display()
            )
        })?;
        hunk_path
    } else {
        run_assembly(AssemblyExecutionRequest {
            root_path: &source_path,
            input_base: example_name,
            defines: &assembly_defines,
            include_paths: &include_paths,
            module_paths: &module_paths,
            pp_macro_depth: 64,
            cpu_override: Some("68020"),
            default_cpu: default_cpu(),
            max_loop_iterations: 1000,
            opasm_package_path: None,
            out_dir: Some(&artifact_dir),
            debug_conditionals: false,
            tab_size: None,
            output_format: EngineOutputFormat::Text,
            go_addr: None,
            bin_specs: &[] as &[BinOutputSpec],
            fill_byte: 0xff,
            fill_byte_set: false,
            default_outputs: false,
            labels_file: None,
            label_output_format: CliLabelOutputFormat::Default,
            dependency_output: None,
            outfile_override: None,
            list_name_override: None,
            hex_name_override: None,
            srec_name_override: None,
            hunk_name_override: None,
            header_title: "opForge Assembler FS-UAE smoke",
            output_sink: None,
            source_provider: None,
            execution_mode: ExecutionMode::Lockstep {
                continuation_head: ContinuationHead::Vm,
            },
            collect_runtime_traces: true,
            suppress_outputs: false,
        })
        .map_err(|err| {
            format!(
                "assemble FS-UAE smoke example {} from {}: {}; diagnostics: {:#?}",
                example_name,
                source_path.display(),
                err.summary(),
                err.diagnostics()
            )
        })?;

        // Source-directed output may use a literal extensionless filename.
        // Resolve it after assembly; before then neither candidate exists.
        let hunk_path = generated_hunk_artifact_path(&artifact_dir, example_name);
        if !hunk_path.is_file() {
            return Err(format!(
                "expected generated Hunk artifact at {}",
                hunk_path.display()
            ));
        }
        hunk_path
    };
    let mounted_hunk_alias_path = match executable {
        NativeCliParityExecutable::TkvmBranchHarness => {
            mounted_work_dir.join("build/tkvm_branch_harness")
        }
        NativeCliParityExecutable::PrvmMacroHarness => {
            mounted_work_dir.join("build/prvm_macro_harness")
        }
        NativeCliParityExecutable::ExprvmI64Harness => {
            mounted_work_dir.join("build/exprvm_i64_harness")
        }

        NativeCliParityExecutable::BinarySourceHarness => {
            mounted_work_dir.join("build/binary_source_harness")
        }
        NativeCliParityExecutable::CompactCli => mounted_work_dir.join("build/opforge"),
    };
    if let Some(bytes) = bootstrap_executable {
        fs::write(&mounted_hunk_alias_path, bytes).map_err(|err| {
            format!(
                "stage captured self-host executable at {}: {err}",
                mounted_hunk_alias_path.display()
            )
        })?;
    } else if mounted_hunk_alias_path != hunk_path {
        fs::copy(&hunk_path, &mounted_hunk_alias_path).map_err(|err| {
            format!(
                "copy {} to mounted Hunk alias {}: {err}",
                hunk_path.display(),
                mounted_hunk_alias_path.display()
            )
        })?;
    }
    stage_guest_script(&mounted_work_dir, batch_script.as_str())?;
    let capture = batch_capture_config_from_env(&batch_paths)?;
    clear_capture_files(&capture)?;
    clear_native_cli_output_artifacts(&mounted_work_dir)?;
    // Self-host output can replace the loaded executable's pathname. Record the
    // staged bootstrap before execution, rather than hashing that output later.
    let native_image_digest = opforge_self_host_package_digest(
        &fs::read(&mounted_hunk_alias_path)
            .map_err(|err| format!("read staged native image: {err}"))?,
    );
    let generated_config_path =
        maybe_materialize_fs_uae_config(&artifact_dir, &mounted_work_dir, args_text)?;

    let args = args_text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.replace("{hunk}", &hunk_path.to_string_lossy())
                .replace("{artifact_dir}", &artifact_dir.to_string_lossy())
                .replace("{example}", example_name)
                .replace(
                    "{start_file}",
                    &capture.start_paths.primary.to_string_lossy(),
                )
                .replace(
                    "{ready_file}",
                    &capture.ready_paths.primary.to_string_lossy(),
                )
                .replace(
                    "{stdout_file}",
                    &capture.stdout_paths.primary.to_string_lossy(),
                )
                .replace(
                    "{stderr_file}",
                    &capture.stderr_paths.primary.to_string_lossy(),
                )
                .replace(
                    "{exit_code_file}",
                    &capture.exit_code_paths.primary.to_string_lossy(),
                )
                .replace(
                    "{fsuae_config}",
                    &generated_config_path
                        .as_ref()
                        .map(|path| path.to_string_lossy().into_owned())
                        .unwrap_or_default(),
                )
        })
        .collect::<Vec<_>>();

    let mut launch_command = fs_uae_launch_command(fs_uae_bin, &args);
    terminate_preexisting_fs_uae_processes()?;
    let baseline_process_ids = snapshot_fs_uae_process_ids()?;
    let launcher_stdout_path = artifact_dir.join(FS_UAE_LAUNCHER_STDOUT_FILE);
    let launcher_stderr_path = artifact_dir.join(FS_UAE_LAUNCHER_STDERR_FILE);
    let launcher_stdout = fs::File::create(&launcher_stdout_path)
        .map_err(|err| format!("create {}: {err}", launcher_stdout_path.display()))?;
    let launcher_stderr = fs::File::create(&launcher_stderr_path)
        .map_err(|err| format!("create {}: {err}", launcher_stderr_path.display()))?;

    let mut child = match launch_command
        .current_dir(&artifact_dir)
        .stdout(Stdio::from(launcher_stdout))
        .stderr(Stdio::from(launcher_stderr))
        .spawn()
    {
        Ok(child) => child,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "FS-UAE binary '{fs_uae_bin}' was not found; install FS-UAE or set {FS_UAE_BIN_ENV}"
            )))
        }
        Err(err) => {
            return Err(format!(
                "launch FS-UAE binary '{fs_uae_bin}' for {} example {}: {err}",
                example_name,
                hunk_path.display()
            ))
        }
    };

    let wait_outcome = match wait_for_capture_or_exit(
        &mut child,
        &capture,
        example_name,
        &baseline_process_ids,
    ) {
        Ok(wait_outcome) => wait_outcome,
        Err(err) => {
            let _ = cleanup_spawned_fs_uae_processes(&baseline_process_ids);
            let _ = wait_for_spawned_fs_uae_processes_to_exit(&baseline_process_ids);

            let partial = batch_paths
                .iter()
                .enumerate()
                .map(|(index, paths)| {
                    let stdout = read_optional_text(&paths.stdout_path)
                        .ok()
                        .flatten()
                        .unwrap_or_default();
                    let stderr = read_optional_text(&paths.stderr_path)
                        .ok()
                        .flatten()
                        .unwrap_or_default();
                    format!(
                        "case {index} partial stdout:\n{stdout}\ncase {index} partial stderr:\n{stderr}"
                    )
                })
                .collect::<Vec<_>>()
                .join("\n");
            return Err(format!("{err}\n{partial}"));
        }
    };
    if matches!(wait_outcome, FsUaeWaitOutcome::Captured(_)) {
        wait_for_process_exit_after_capture(&mut child, example_name)?;
    }
    let launcher_status = child
        .wait()
        .map_err(|err| format!("wait for FS-UAE process for {}: {err}", example_name))?;
    let _ = cleanup_spawned_fs_uae_processes(&baseline_process_ids);
    wait_for_spawned_fs_uae_processes_to_exit(&baseline_process_ids)?;

    let launcher_stdout = read_optional_text(&launcher_stdout_path)?;
    let launcher_stderr = read_optional_text(&launcher_stderr_path)?;

    let launcher_status_text = fs_uae_launcher_status_text(launcher_status);
    let launcher_success = launcher_status.success();
    let common_stderr = merge_output(
        Some(launcher_status_text),
        launcher_stderr,
        "FS-UAE launcher stderr",
    );
    let common_stdout = launcher_stdout.unwrap_or_default();

    let captured_artifacts = capture_artifact_files(&artifact_dir)?;
    let mut runs = Vec::with_capacity(cases.len());
    let mut proof_errors = Vec::new();
    for (case, case_paths) in cases.iter().zip(batch_paths.iter()) {
        let exit_code = match read_optional_exit_code(&case_paths.exit_code_path) {
            Ok(value) => value,
            Err(error) => {
                proof_errors.push(format!("{} exit evidence: {error}", case.name));
                None
            }
        };
        let stdout = match read_optional_text(&case_paths.stdout_path) {
            Ok(value) => value.unwrap_or_default(),
            Err(error) => {
                proof_errors.push(format!("{} stdout evidence: {error}", case.name));
                String::new()
            }
        };
        let stderr = match read_optional_text(&case_paths.stderr_path) {
            Ok(value) => value.unwrap_or_default(),
            Err(error) => {
                proof_errors.push(format!("{} stderr evidence: {error}", case.name));
                String::new()
            }
        };
        let started_matches = match read_optional_text(&case_paths.started_path) {
            Ok(value) => value.is_some_and(|text| text.trim() == case_paths.expected_started),
            Err(error) => {
                proof_errors.push(format!("{} start evidence: {error}", case.name));
                false
            }
        };
        let done_matches = match read_optional_text(&case_paths.done_path) {
            Ok(value) => value.is_some_and(|text| text.trim() == case_paths.expected_done),
            Err(error) => {
                proof_errors.push(format!("{} completion evidence: {error}", case.name));
                false
            }
        };
        let success = determine_batch_case_success(
            started_matches,
            done_matches,
            exit_code,
            launcher_success,
        );
        let protocol_completed = started_matches && done_matches && exit_code.is_some();
        let mut run = FsUaeSmokeRun {
            example_name,
            source_path: mounted_work_dir.join(match executable {
                NativeCliParityExecutable::ExprvmI64Harness => "exprvm-i64-cases.bin",
                NativeCliParityExecutable::PrvmMacroHarness => "prvm-macro-cases.bin",
                NativeCliParityExecutable::TkvmBranchHarness => "tkvm-branch-cases.bin",
                NativeCliParityExecutable::BinarySourceHarness
                | NativeCliParityExecutable::CompactCli => "input.bin",
            }),
            artifact_dir: artifact_dir.clone(),
            hunk_path: mounted_hunk_alias_path.clone(),
            stdout: merge_output(
                Some(stdout),
                Some(common_stdout.clone()),
                "FS-UAE launcher stdout",
            ),
            stderr: merge_output(
                Some(stderr),
                Some(common_stderr.clone()),
                "FS-UAE launcher stderr",
            ),
            exit_code,
            protocol_completed,
            start_to_done_host_seconds: wait_outcome
                .single_case_seconds(cases.len(), protocol_completed),
            native_image_digest: Some(native_image_digest.clone()),
            success,
            verified_output: None,
            captured_artifacts: opforge_native_cli_case_captured_artifacts(
                &captured_artifacts,
                case_paths,
            ),
        };
        if let Err(error) = verify_native_cli_case_proof(case, &mut run) {
            proof_errors.push(error);
        }
        runs.push(run);
    }

    drop(ephemeral_artifact_dir);
    if artifact_dir.exists() {
        return Err(format!(
            "FS-UAE proof artifact directory still exists after cleanup: {}",
            artifact_dir.display()
        ));
    }
    if !proof_errors.is_empty() {
        return Err(format!(
            "{} FS-UAE case proof error(s) after all {} cases were evaluated:\n{}",
            proof_errors.len(),
            cases.len(),
            proof_errors.join("\n")
        ));
    }
    Ok(FsUaeSmokeOutcome::Completed { runs })
}

fn opforge_native_cli_case_assembly_defines(case: &OpforgeNativeCliParityCase<'_>) -> Vec<String> {
    case.extra_assembly_defines
        .iter()
        .map(|define| (*define).to_string())
        .collect()
}

fn create_artifact_dir(workspace_root: &Path, label: &str) -> Result<PathBuf, String> {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let dir = workspace_root
        .join("target")
        .join(format!("{label}-{}-{nanos}", std::process::id()));
    fs::create_dir_all(&dir)
        .map_err(|err| format!("create artifact directory {}: {err}", dir.display()))?;
    Ok(dir)
}

struct EphemeralArtifactDir(PathBuf);

impl Drop for EphemeralArtifactDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn capture_artifact_files(root: &Path) -> Result<BTreeMap<PathBuf, Vec<u8>>, String> {
    fn visit(
        root: &Path,
        dir: &Path,
        captured: &mut BTreeMap<PathBuf, Vec<u8>>,
    ) -> Result<(), String> {
        for entry in fs::read_dir(dir)
            .map_err(|err| format!("read ephemeral artifact directory {}: {err}", dir.display()))?
        {
            let entry = entry.map_err(|err| {
                format!(
                    "read entry in ephemeral artifact directory {}: {err}",
                    dir.display()
                )
            })?;
            let path = entry.path();
            if path.is_dir() {
                visit(root, &path, captured)?;
            } else if path.is_file() {
                let relative = path.strip_prefix(root).map_err(|err| {
                    format!(
                        "make ephemeral artifact {} relative to {}: {err}",
                        path.display(),
                        root.display()
                    )
                })?;
                let bytes = fs::read(&path)
                    .map_err(|err| format!("read ephemeral artifact {}: {err}", path.display()))?;
                captured.insert(relative.to_path_buf(), bytes);
            }
        }
        Ok(())
    }

    let mut captured = BTreeMap::new();
    visit(root, root, &mut captured)?;
    Ok(captured)
}

fn example_module_paths(workspace_root: &Path, example_name: &str) -> Vec<PathBuf> {
    // Module identifiers resolve recursively from the native composition root.
    // Listing implementation directories can omit moved modules or admit stale
    // compositions from a different namespace layout.
    if matches!(
        example_name,
        FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME
            | "exprvm_i64_harness"
            | "tkvm_branch_harness"
            | "prvm_macro_harness"
            | "prvm_smoke"
            | "prvm_line_iterator_smoke"
            | "binary_source_harness"
            | "binary_memory_budget_harness"
            | "opforge"
    ) {
        vec![workspace_root.join("native/motorola68000/amigaos")]
    } else {
        Vec::new()
    }
}

fn example_include_paths(workspace_root: &Path, example_name: &str) -> Vec<PathBuf> {
    if !example_module_paths(workspace_root, example_name).is_empty()
        || example_name == "tokvm_interpreter"
    {
        vec![workspace_root.join("native/motorola68000/amigaos/debug")]
    } else {
        Vec::new()
    }
}

fn stage_guest_input_bytes(
    mounted_work_dir: &Path,
    relative_path: &str,
    bytes: &[u8],
) -> Result<(), String> {
    let relative_path = Path::new(relative_path);
    if relative_path.as_os_str().is_empty()
        || relative_path
            .components()
            .any(|component| !matches!(component, std::path::Component::Normal(_)))
    {
        return Err(format!(
            "guest input path must be a non-empty relative path without traversal: {}",
            relative_path.display()
        ));
    }
    let target_path = mounted_work_dir.join(relative_path);
    if let Some(parent_dir) = target_path.parent() {
        fs::create_dir_all(parent_dir).map_err(|err| {
            format!(
                "create guest input parent directory {}: {err}",
                parent_dir.display()
            )
        })?;
    }
    fs::write(&target_path, bytes)
        .map_err(|err| format!("write guest input file {}: {err}", target_path.display()))
}

enum ExampleSmokeResult {
    Run(Box<FsUaeSmokeRun>),
    Skipped(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FsUaeCapturePathSet {
    primary: PathBuf,
    fallback: Option<PathBuf>,
}

impl FsUaeCapturePathSet {
    fn from_primary_and_optional_fallback(primary: PathBuf, fallback: Option<PathBuf>) -> Self {
        Self { primary, fallback }
    }

    fn candidates(&self) -> impl Iterator<Item = &Path> {
        std::iter::once(self.primary.as_path()).chain(self.fallback.as_deref())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FsUaeCaptureConfig {
    start_paths: FsUaeCapturePathSet,
    ready_paths: FsUaeCapturePathSet,
    stdout_paths: FsUaeCapturePathSet,
    stderr_paths: FsUaeCapturePathSet,
    exit_code_paths: FsUaeCapturePathSet,
    timeout: Duration,
    post_start_timeout: Duration,
    poll_interval: Duration,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FsUaeWaitOutcome {
    Exited,
    Captured(Option<Duration>),
}

impl FsUaeWaitOutcome {
    fn single_case_seconds(self, case_count: usize, protocol_completed: bool) -> Option<f64> {
        match self {
            Self::Captured(Some(duration)) if case_count == 1 && protocol_completed => {
                Some(duration.as_secs_f64())
            }
            _ => None,
        }
    }
}

fn capture_config_from_env(
    artifact_dir: &Path,
    fallback_artifact_dir: Option<&Path>,
) -> Result<FsUaeCaptureConfig, String> {
    let start_name = std::env::var(FS_UAE_START_FILE_ENV)
        .ok()
        .filter(|value| !value.trim().is_empty())
        .unwrap_or_else(|| FS_UAE_DEFAULT_START_FILE.to_string());
    let ready_name = std::env::var(FS_UAE_READY_FILE_ENV)
        .ok()
        .filter(|value| !value.trim().is_empty())
        .unwrap_or_else(|| FS_UAE_DEFAULT_READY_FILE.to_string());
    let stdout_name = std::env::var(FS_UAE_STDOUT_FILE_ENV)
        .ok()
        .filter(|value| !value.trim().is_empty())
        .unwrap_or_else(|| FS_UAE_DEFAULT_STDOUT_FILE.to_string());
    let stderr_name = std::env::var(FS_UAE_STDERR_FILE_ENV)
        .ok()
        .filter(|value| !value.trim().is_empty())
        .unwrap_or_else(|| FS_UAE_DEFAULT_STDERR_FILE.to_string());
    let exit_code_name = std::env::var(FS_UAE_EXIT_CODE_FILE_ENV)
        .ok()
        .filter(|value| !value.trim().is_empty())
        .unwrap_or_else(|| FS_UAE_DEFAULT_EXIT_CODE_FILE.to_string());

    Ok(FsUaeCaptureConfig {
        start_paths: capture_path_set(artifact_dir, fallback_artifact_dir, &start_name),
        ready_paths: capture_path_set(artifact_dir, fallback_artifact_dir, &ready_name),
        stdout_paths: capture_path_set(artifact_dir, fallback_artifact_dir, &stdout_name),
        stderr_paths: capture_path_set(artifact_dir, fallback_artifact_dir, &stderr_name),
        exit_code_paths: capture_path_set(artifact_dir, fallback_artifact_dir, &exit_code_name),
        timeout: Duration::from_millis(parse_env_u64(
            FS_UAE_TIMEOUT_MS_ENV,
            FS_UAE_DEFAULT_TIMEOUT_MS,
        )?),
        post_start_timeout: Duration::from_millis(parse_env_u64(
            FS_UAE_POST_START_TIMEOUT_MS_ENV,
            FS_UAE_DEFAULT_POST_START_TIMEOUT_MS,
        )?),
        poll_interval: Duration::from_millis(parse_env_u64(
            FS_UAE_POLL_MS_ENV,
            FS_UAE_DEFAULT_POLL_MS,
        )?),
    })
}

/// The mounted base image emits generic smoke markers while booting. Native
/// CLI batch runs must instead synchronize on their own case markers, otherwise
/// the host can tear down FS-UAE before the batch command has produced output.
fn batch_capture_config_from_env(
    batch_paths: &[OpforgeNativeCliBatchCasePaths],
) -> Result<FsUaeCaptureConfig, String> {
    let first = batch_paths
        .first()
        .ok_or_else(|| "native CLI batch capture requires at least one case".to_string())?;
    let last = batch_paths
        .last()
        .ok_or_else(|| "native CLI batch capture requires at least one case".to_string())?;
    let mut capture = capture_config_from_env(&first.artifact_dir, None)?;
    capture.start_paths =
        FsUaeCapturePathSet::from_primary_and_optional_fallback(first.started_path.clone(), None);
    capture.ready_paths =
        FsUaeCapturePathSet::from_primary_and_optional_fallback(last.done_path.clone(), None);
    capture.stdout_paths =
        FsUaeCapturePathSet::from_primary_and_optional_fallback(last.stdout_path.clone(), None);
    capture.stderr_paths =
        FsUaeCapturePathSet::from_primary_and_optional_fallback(last.stderr_path.clone(), None);
    capture.exit_code_paths =
        FsUaeCapturePathSet::from_primary_and_optional_fallback(last.exit_code_path.clone(), None);
    Ok(capture)
}

fn capture_path_set(
    artifact_dir: &Path,
    fallback_artifact_dir: Option<&Path>,
    value: &str,
) -> FsUaeCapturePathSet {
    FsUaeCapturePathSet::from_primary_and_optional_fallback(
        resolve_capture_path(artifact_dir, value),
        fallback_artifact_dir.map(|dir| resolve_capture_path(dir, value)),
    )
}

fn resolve_capture_path(artifact_dir: &Path, value: &str) -> PathBuf {
    let candidate = Path::new(value);
    if candidate.is_absolute() {
        candidate.to_path_buf()
    } else {
        artifact_dir.join(candidate)
    }
}

fn parse_env_u64(name: &str, default_value: u64) -> Result<u64, String> {
    match std::env::var(name) {
        Ok(value) if !value.trim().is_empty() => value
            .trim()
            .parse::<u64>()
            .map_err(|err| format!("parse {name}='{value}' as u64: {err}")),
        _ => Ok(default_value),
    }
}

fn clear_capture_files(capture: &FsUaeCaptureConfig) -> Result<(), String> {
    for path in capture
        .start_paths
        .candidates()
        .chain(capture.ready_paths.candidates())
        .chain(capture.stdout_paths.candidates())
        .chain(capture.stderr_paths.candidates())
        .chain(capture.exit_code_paths.candidates())
    {
        match fs::remove_file(path) {
            Ok(()) => {}
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
            Err(err) => {
                return Err(format!(
                    "remove stale capture file {}: {err}",
                    path.display()
                ));
            }
        }
    }
    Ok(())
}

fn clear_native_cli_output_artifacts(mounted_work_dir: &Path) -> Result<(), String> {
    for path in [
        mounted_work_dir.join(FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE),
        mounted_work_dir.join(FS_UAE_OPFORGE_NATIVE_CLI_PRG_OUTPUT_FILE),
        mounted_work_dir
            .join("build")
            .join(FS_UAE_OPFORGE_NATIVE_CLI_PRG_OUTPUT_FILE),
        mounted_work_dir
            .join("build")
            .join(FS_UAE_OPFORGE_NATIVE_CLI_HEX_OUTPUT_FILE),
        mounted_work_dir
            .join("build")
            .join(FS_UAE_OPFORGE_NATIVE_CLI_LST_OUTPUT_FILE),
    ] {
        match fs::remove_file(&path) {
            Ok(()) => {}
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
            Err(err) => {
                return Err(format!(
                    "remove stale native CLI output {}: {err}",
                    path.display()
                ))
            }
        }
    }
    Ok(())
}

fn capture_path_exists(paths: &FsUaeCapturePathSet) -> bool {
    paths.candidates().any(Path::is_file)
}

fn snapshot_fs_uae_process_ids() -> Result<BTreeSet<u32>, String> {
    let mut process_ids = BTreeSet::new();
    for process_name in ["fs-uae", "fs-uae-launcher"] {
        let output = match Command::new("pgrep").args(["-x", process_name]).output() {
            Ok(output) => output,
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(process_ids),
            Err(err) => return Err(format!("run pgrep for {process_name}: {err}")),
        };

        if output.status.success() {
            for line in String::from_utf8_lossy(&output.stdout).lines() {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                let process_id = trimmed.parse::<u32>().map_err(|err| {
                    format!("parse pgrep pid '{trimmed}' for {process_name}: {err}")
                })?;
                process_ids.insert(process_id);
            }
        }
    }
    Ok(process_ids)
}

fn cleanup_spawned_fs_uae_processes(baseline_process_ids: &BTreeSet<u32>) -> Result<(), String> {
    let current_process_ids = snapshot_fs_uae_process_ids()?;
    for process_id in current_process_ids.difference(baseline_process_ids) {
        terminate_process_id(*process_id)?;
    }
    Ok(())
}

fn terminate_preexisting_fs_uae_processes() -> Result<(), String> {
    let existing_process_ids = snapshot_fs_uae_process_ids()?;
    if existing_process_ids.is_empty() {
        return Ok(());
    }

    for process_id in &existing_process_ids {
        terminate_process_id(*process_id)?;
    }

    let deadline = Instant::now() + Duration::from_secs(30);
    loop {
        if snapshot_fs_uae_process_ids()?.is_empty() {
            return Ok(());
        }
        if Instant::now() >= deadline {
            return Err(
                "FS-UAE preflight cleanup timed out waiting for existing emulator processes to exit"
                    .to_string(),
            );
        }
        thread::sleep(Duration::from_millis(250));
    }
}

fn wait_for_spawned_fs_uae_processes_to_exit(
    baseline_process_ids: &BTreeSet<u32>,
) -> Result<(), String> {
    let deadline = Instant::now() + Duration::from_secs(30);
    loop {
        let current_process_ids = snapshot_fs_uae_process_ids()?;
        if current_process_ids
            .difference(baseline_process_ids)
            .next()
            .is_none()
        {
            return Ok(());
        }
        if Instant::now() >= deadline {
            return Err(
                "FS-UAE helper cleanup timed out waiting for spawned emulator processes to exit"
                    .to_string(),
            );
        }
        thread::sleep(Duration::from_millis(250));
    }
}

fn terminate_process_id(process_id: u32) -> Result<(), String> {
    let pid_text = process_id.to_string();
    let term_status = Command::new("kill")
        .args(["-TERM", &pid_text])
        .status()
        .map_err(|err| format!("send SIGTERM to pid {process_id}: {err}"))?;
    if !term_status.success() {
        return Ok(());
    }

    thread::sleep(Duration::from_millis(500));

    let kill_status = Command::new("kill")
        .args(["-KILL", &pid_text])
        .status()
        .map_err(|err| format!("send SIGKILL to pid {process_id}: {err}"))?;
    if !kill_status.success() {
        return Ok(());
    }

    Ok(())
}

fn maybe_materialize_fs_uae_config(
    artifact_dir: &Path,
    work_mount_dir: &Path,
    args_text: &str,
) -> Result<Option<PathBuf>, String> {
    let template_path = std::env::var(FS_UAE_CONFIG_TEMPLATE_ENV)
        .ok()
        .filter(|value| !value.trim().is_empty());
    let memory_profile = fs_uae_memory_profile_from_env()?;
    validate_budget_launch(memory_profile, args_text, template_path.as_deref())?;
    let Some(template_path) = template_path else {
        return Ok(None);
    };

    let template_path = PathBuf::from(template_path);
    let template_text = fs::read_to_string(&template_path).map_err(|err| {
        format!(
            "read FS-UAE config template {}: {err}",
            template_path.display()
        )
    })?;
    let config_text = rewrite_fs_uae_config_work_mount(
        &template_text,
        &work_mount_dir.to_string_lossy(),
        memory_profile,
    );
    let config_path = artifact_dir.join(FS_UAE_CONFIG_FILE_NAME);
    fs::write(&config_path, config_text).map_err(|err| {
        format!(
            "write generated FS-UAE config {}: {err}",
            config_path.display()
        )
    })?;
    Ok(Some(config_path))
}

fn read_optional_text(path: &Path) -> Result<Option<String>, String> {
    if !path.is_file() {
        return Ok(None);
    }

    fs::read(path)
        .map(|bytes| Some(String::from_utf8_lossy(&bytes).to_string()))
        .map_err(|err| format!("read {}: {err}", path.display()))
}

fn read_optional_text_from_paths(paths: &FsUaeCapturePathSet) -> Result<Option<String>, String> {
    for path in paths.candidates() {
        if let Some(text) = read_optional_text(path)? {
            return Ok(Some(text));
        }
    }
    Ok(None)
}

fn read_optional_exit_code(path: &Path) -> Result<Option<i32>, String> {
    let Some(text) = read_optional_text(path)? else {
        return Ok(None);
    };
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return Ok(None);
    }

    trimmed
        .parse::<i32>()
        .map(Some)
        .map_err(|err| format!("parse guest exit code from {}: {err}", path.display()))
}

fn read_optional_exit_code_from_paths(paths: &FsUaeCapturePathSet) -> Result<Option<i32>, String> {
    for path in paths.candidates() {
        if let Some(code) = read_optional_exit_code(path)? {
            return Ok(Some(code));
        }
    }
    Ok(None)
}

fn merge_output(
    captured: Option<String>,
    launcher: Option<String>,
    launcher_label: &str,
) -> String {
    match (
        captured.filter(|value| !value.is_empty()),
        launcher.filter(|value| !value.is_empty()),
    ) {
        (Some(captured), Some(launcher)) => {
            format!("{captured}\n\n[{launcher_label}]\n{launcher}")
        }
        (Some(captured), None) => captured,
        (None, Some(launcher)) => launcher,
        (None, None) => String::new(),
    }
}

fn determine_smoke_success(guest_exit_code: Option<i32>, _launcher_success: bool) -> bool {
    guest_exit_code == Some(0)
}

fn require_completed_guest_protocol(
    example_name: &str,
    protocol_completed: bool,
    guest_exit_code: Option<i32>,
    stdout: &str,
    stderr: &str,
) -> Result<(), String> {
    if protocol_completed && guest_exit_code.is_some() {
        return Ok(());
    }
    Err(format!(
        "FS-UAE guest proof did not complete for {example_name}; no test result is valid without the fresh completion marker and explicit guest exit code\nstdout:\n{stdout}\nstderr:\n{stderr}"
    ))
}

fn determine_batch_case_success(
    case_started: bool,
    case_done: bool,
    guest_exit_code: Option<i32>,
    launcher_success: bool,
) -> bool {
    case_started
        && case_done
        && guest_exit_code.is_some()
        && determine_smoke_success(guest_exit_code, launcher_success)
}

fn fs_uae_launcher_status_text(status: ExitStatus) -> String {
    if let Some(code) = status.code() {
        return format!("FS-UAE launcher exit code: {code}\n");
    }
    format!("FS-UAE launcher exit status: {status}\n")
}

/// Start the macOS application bundle through Launch Services when the caller
/// supplied its inner executable. Directly invoking that binary exits after
/// initialization on this host, while opening the bundle keeps the emulator
/// process alive for the mounted guest startup hook and debugger.
fn fs_uae_launch_command(fs_uae_bin: &str, args: &[String]) -> Command {
    #[cfg(target_os = "macos")]
    {
        let executable = Path::new(fs_uae_bin);
        if let Some(app_bundle) = executable
            .ancestors()
            .find(|path| path.extension().and_then(|value| value.to_str()) == Some("app"))
        {
            let mut command = Command::new("/usr/bin/open");
            command
                .args(["-W", "-n"])
                .arg(app_bundle)
                .arg("--args")
                .args(args);
            return command;
        }
    }

    let mut command = Command::new(fs_uae_bin);
    command.args(args);
    command
}

fn launcher_exit_is_terminal(
    launcher_exited_at: Instant,
    now: Instant,
    spawned_emulator_present: bool,
) -> bool {
    !spawned_emulator_present
        && now.duration_since(launcher_exited_at)
            >= Duration::from_millis(FS_UAE_LAUNCHER_HANDOFF_GRACE_MS)
}

fn wait_for_capture_or_exit(
    child: &mut std::process::Child,
    capture: &FsUaeCaptureConfig,
    example_name: &str,
    baseline_process_ids: &BTreeSet<u32>,
) -> Result<FsUaeWaitOutcome, String> {
    let deadline = Instant::now() + capture.timeout;
    let mut smoke_started_at = None;
    let mut launcher_exited_at = None;
    loop {
        if capture_path_exists(&capture.ready_paths) {
            return Ok(FsUaeWaitOutcome::Captured(
                smoke_started_at.map(|started: Instant| started.elapsed()),
            ));
        }

        if smoke_started_at.is_none() && capture_path_exists(&capture.start_paths) {
            smoke_started_at = Some(Instant::now());
        }

        if child
            .try_wait()
            .map_err(|err| format!("poll FS-UAE process for {example_name}: {err}"))?
            .is_some()
        {
            let now = Instant::now();
            let exited_at = *launcher_exited_at.get_or_insert(now);
            let spawned_emulator_present = snapshot_fs_uae_process_ids()?
                .difference(baseline_process_ids)
                .next()
                .is_some();
            if launcher_exit_is_terminal(exited_at, now, spawned_emulator_present) {
                return Ok(FsUaeWaitOutcome::Exited);
            }
        }

        if Instant::now() >= deadline {
            let _ = child.kill();
            let _ = child.wait();
            return Err(format!(
                "FS-UAE smoke for {example_name} timed out after {} ms waiting for {} or process exit",
                capture.timeout.as_millis(),
                capture.ready_paths.primary.display(),
            ));
        }

        if let Some(smoke_started_at) = smoke_started_at {
            if Instant::now().duration_since(smoke_started_at) >= capture.post_start_timeout {
                let _ = child.kill();
                let _ = child.wait();
                return Err(format!(
                    "FS-UAE smoke for {example_name} exceeded the post-start timeout of {} ms after {} appeared without producing {}",
                    capture.post_start_timeout.as_millis(),
                    capture.start_paths.primary.display(),
                    capture.ready_paths.primary.display(),
                ));
            }
        }

        thread::sleep(capture.poll_interval);
    }
}

fn wait_for_process_exit_after_capture(
    child: &mut std::process::Child,
    example_name: &str,
) -> Result<(), String> {
    if child
        .try_wait()
        .map_err(|err| format!("poll FS-UAE process for {example_name}: {err}"))?
        .is_none()
    {
        // The generated disposable configuration does not request FS-UAE to
        // exit after the guest writes its completion marker.  The marker is
        // the protocol's terminal event, so retaining the emulator here only
        // turns every successful native probe into a fixed host-side delay.
        let _ = terminate_process_id(child.id());
    }
    Ok(())
}

fn run_example_smoke(
    workspace_root: &Path,
    fs_uae_bin: &str,
    args_text: &str,
    example_name: &'static str,
    relative_source_path: &str,
    cpu_override: &str,
) -> Result<ExampleSmokeResult, String> {
    run_example_smoke_with_extra_defines(
        workspace_root,
        fs_uae_bin,
        args_text,
        example_name,
        relative_source_path,
        cpu_override,
        &[],
    )
}

fn run_example_smoke_with_extra_defines(
    workspace_root: &Path,
    fs_uae_bin: &str,
    args_text: &str,
    example_name: &'static str,
    relative_source_path: &str,
    cpu_override: &str,
    extra_assembly_defines: &[&str],
) -> Result<ExampleSmokeResult, String> {
    run_example_smoke_with_request(&RunExampleSmokeRequest {
        workspace_root,
        fs_uae_bin,
        args_text,
        example_name,
        relative_source_path,
        cpu_override,
        extra_assembly_defines,
    })
}

struct RunExampleSmokeRequest<'a> {
    workspace_root: &'a Path,
    fs_uae_bin: &'a str,
    args_text: &'a str,
    example_name: &'static str,
    relative_source_path: &'a str,
    cpu_override: &'a str,
    extra_assembly_defines: &'a [&'a str],
}

fn run_example_smoke_with_request(
    req: &RunExampleSmokeRequest<'_>,
) -> Result<ExampleSmokeResult, String> {
    let workspace_root = req.workspace_root;
    let fs_uae_bin = req.fs_uae_bin;
    let args_text = req.args_text;
    let example_name = req.example_name;
    let relative_source_path = req.relative_source_path;
    let cpu_override = req.cpu_override;
    let extra_assembly_defines = req.extra_assembly_defines;
    let source_path = workspace_root.join(relative_source_path);
    if !source_path.is_file() {
        return Err(format!(
            "expected FS-UAE smoke example source at {}",
            source_path.display()
        ));
    }

    let artifact_dir =
        create_artifact_dir(workspace_root, &format!("fs-uae-hunk-smoke-{example_name}"))?;
    let ephemeral_artifact_dir = EphemeralArtifactDir(artifact_dir.clone());
    let mounted_work_dir = artifact_dir.join(FS_UAE_MOUNTED_WORK_DIR_NAME);
    fs::create_dir_all(mounted_work_dir.join("build")).map_err(|err| {
        format!(
            "create mounted Work directory {}: {err}",
            mounted_work_dir.display(),
        )
    })?;
    let hunk_name_override = if example_name == "helloworld" {
        Some(format!("build/{example_name}.hunk"))
    } else {
        None
    };
    let assembly_defines = extra_assembly_defines
        .iter()
        .map(|define| (*define).to_string())
        .collect::<Vec<_>>();
    let include_paths = example_include_paths(workspace_root, example_name);
    let module_paths = example_module_paths(workspace_root, example_name);
    run_assembly(AssemblyExecutionRequest {
        root_path: &source_path,
        input_base: example_name,
        defines: &assembly_defines,
        include_paths: &include_paths,
        module_paths: &module_paths,
        pp_macro_depth: 64,
        cpu_override: Some(cpu_override),
        default_cpu: default_cpu(),
        max_loop_iterations: 1000,
        opasm_package_path: None,
        out_dir: Some(&artifact_dir),
        debug_conditionals: false,
        tab_size: None,
        output_format: EngineOutputFormat::Text,
        go_addr: None,
        bin_specs: &[] as &[BinOutputSpec],
        fill_byte: 0xff,
        fill_byte_set: false,
        default_outputs: false,
        labels_file: None,
        label_output_format: CliLabelOutputFormat::Default,
        dependency_output: None,
        outfile_override: None,
        list_name_override: None,
        hex_name_override: None,
        srec_name_override: None,
        hunk_name_override: hunk_name_override.as_deref(),
        header_title: "opForge Assembler FS-UAE smoke",
        output_sink: None,
        source_provider: None,
        execution_mode: ExecutionMode::Lockstep {
            continuation_head: ContinuationHead::Vm,
        },
        collect_runtime_traces: true,
        suppress_outputs: false,
    })
    .map_err(|err| {
        format!(
            "assemble FS-UAE smoke example {} from {}: {}; diagnostics: {:#?}",
            example_name,
            source_path.display(),
            err.summary(),
            err.diagnostics()
        )
    })?;

    let hunk_path = generated_hunk_artifact_path(&artifact_dir, example_name);
    if !hunk_path.is_file() {
        return Err(format!(
            "expected generated Hunk artifact at {}",
            hunk_path.display()
        ));
    }
    let mounted_hunk_alias_path = mounted_work_dir.join(FS_UAE_MOUNTED_HUNK_ALIAS);
    if mounted_hunk_alias_path != hunk_path {
        fs::copy(&hunk_path, &mounted_hunk_alias_path).map_err(|err| {
            format!(
                "copy {} to mounted Hunk alias {}: {err}",
                hunk_path.display(),
                mounted_hunk_alias_path.display(),
            )
        })?;
    }
    let startup_hunk_alias_path = mounted_work_dir.join(FS_UAE_STARTUP_HUNK_ALIAS);
    if startup_hunk_alias_path != hunk_path {
        fs::copy(&hunk_path, &startup_hunk_alias_path).map_err(|err| {
            format!(
                "copy {} to startup Hunk alias {}: {err}",
                hunk_path.display(),
                startup_hunk_alias_path.display(),
            )
        })?;
    }

    let capture = capture_config_from_env(&mounted_work_dir, None)?;
    clear_capture_files(&capture)?;
    let generated_config_path =
        maybe_materialize_fs_uae_config(&artifact_dir, &mounted_work_dir, args_text)?;

    let args = args_text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.replace("{hunk}", &hunk_path.to_string_lossy())
                .replace("{artifact_dir}", &artifact_dir.to_string_lossy())
                .replace("{example}", example_name)
                .replace(
                    "{start_file}",
                    &capture.start_paths.primary.to_string_lossy(),
                )
                .replace(
                    "{ready_file}",
                    &capture.ready_paths.primary.to_string_lossy(),
                )
                .replace(
                    "{stdout_file}",
                    &capture.stdout_paths.primary.to_string_lossy(),
                )
                .replace(
                    "{stderr_file}",
                    &capture.stderr_paths.primary.to_string_lossy(),
                )
                .replace(
                    "{exit_code_file}",
                    &capture.exit_code_paths.primary.to_string_lossy(),
                )
                .replace(
                    "{fsuae_config}",
                    &generated_config_path
                        .as_ref()
                        .map(|path| path.to_string_lossy().to_string())
                        .unwrap_or_default(),
                )
        })
        .collect::<Vec<_>>();
    terminate_preexisting_fs_uae_processes()?;
    let baseline_process_ids = snapshot_fs_uae_process_ids()?;
    let launcher_stdout_path = artifact_dir.join(FS_UAE_LAUNCHER_STDOUT_FILE);
    let launcher_stderr_path = artifact_dir.join(FS_UAE_LAUNCHER_STDERR_FILE);
    let launcher_stdout = fs::File::create(&launcher_stdout_path)
        .map_err(|err| format!("create {}: {err}", launcher_stdout_path.display()))?;
    let launcher_stderr = fs::File::create(&launcher_stderr_path)
        .map_err(|err| format!("create {}: {err}", launcher_stderr_path.display()))?;

    let mut child = match fs_uae_launch_command(fs_uae_bin, &args)
        .current_dir(&artifact_dir)
        .stdout(Stdio::from(launcher_stdout))
        .stderr(Stdio::from(launcher_stderr))
        .spawn()
    {
        Ok(child) => child,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Ok(ExampleSmokeResult::Skipped(format!(
                "FS-UAE binary '{fs_uae_bin}' was not found; install FS-UAE or set {FS_UAE_BIN_ENV}"
            )))
        }
        Err(err) => {
            return Err(format!(
                "launch FS-UAE binary '{fs_uae_bin}' for {} example {}: {err}",
                example_name,
                hunk_path.display()
            ))
        }
    };

    let wait_outcome =
        match wait_for_capture_or_exit(&mut child, &capture, example_name, &baseline_process_ids) {
            Ok(wait_outcome) => wait_outcome,
            Err(err) => {
                let _ = cleanup_spawned_fs_uae_processes(&baseline_process_ids);
                let _ = wait_for_spawned_fs_uae_processes_to_exit(&baseline_process_ids);
                return Err(err);
            }
        };
    if matches!(wait_outcome, FsUaeWaitOutcome::Captured(_)) {
        wait_for_process_exit_after_capture(&mut child, example_name)?;
    }
    let launcher_status = child
        .wait()
        .map_err(|err| format!("wait for FS-UAE process for {example_name}: {err}"))?;
    let _ = cleanup_spawned_fs_uae_processes(&baseline_process_ids);
    wait_for_spawned_fs_uae_processes_to_exit(&baseline_process_ids)?;

    let guest_exit_code = read_optional_exit_code_from_paths(&capture.exit_code_paths)?;
    let launcher_stdout = read_optional_text(&launcher_stdout_path)?;
    let launcher_stderr = read_optional_text(&launcher_stderr_path)?;
    let launcher_status_text = fs_uae_launcher_status_text(launcher_status);
    let captured_stdout = read_optional_text_from_paths(&capture.stdout_paths)?;
    let captured_stderr = read_optional_text_from_paths(&capture.stderr_paths)?;

    let protocol_completed =
        capture.ready_paths.candidates().any(Path::is_file) && guest_exit_code.is_some();
    let captured_artifacts = capture_artifact_files(&artifact_dir)?;
    let run = FsUaeSmokeRun {
        example_name,
        source_path,
        artifact_dir: artifact_dir.clone(),
        hunk_path,
        stdout: merge_output(captured_stdout, launcher_stdout, "FS-UAE launcher stdout"),
        stderr: merge_output(
            captured_stderr,
            Some(merge_output(
                Some(launcher_status_text),
                launcher_stderr,
                "FS-UAE launcher stderr",
            )),
            "FS-UAE launcher",
        ),
        exit_code: guest_exit_code,
        protocol_completed,
        start_to_done_host_seconds: None,
        native_image_digest: None,
        success: protocol_completed && guest_exit_code == Some(0),
        verified_output: None,
        captured_artifacts,
    };
    drop(ephemeral_artifact_dir);
    if artifact_dir.exists() {
        return Err(format!(
            "FS-UAE smoke artifact directory still exists after cleanup: {}",
            artifact_dir.display()
        ));
    }
    require_completed_guest_protocol(
        example_name,
        run.protocol_completed,
        run.exit_code,
        &run.stdout,
        &run.stderr,
    )?;
    Ok(ExampleSmokeResult::Run(Box::new(run)))
}

fn generated_hunk_artifact_path(artifact_dir: &Path, example_name: &str) -> PathBuf {
    let build_dir = artifact_dir.join("build");
    let extension_path = build_dir.join(format!("{example_name}.hunk"));
    if extension_path.exists() {
        return extension_path;
    }

    let stem_path = build_dir.join(example_name);
    if stem_path.exists() {
        return stem_path;
    }

    extension_path
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_examples_resolve_from_current_composition_root() {
        let workspace = Path::new("/workspace");
        let native = workspace.join("native/motorola68000/amigaos");
        for example in [
            "opforge",
            "binary_source_harness",
            "prvm_smoke",
            FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME,
        ] {
            assert_eq!(
                example_module_paths(workspace, example),
                vec![native.clone()]
            );
            assert_eq!(
                example_include_paths(workspace, example),
                vec![native.join("debug")]
            );
        }
        assert!(example_module_paths(workspace, "unknown_example").is_empty());
        assert!(example_include_paths(workspace, "unknown_example").is_empty());
    }

    #[test]
    fn completed_timing_requires_single_case_protocol_evidence() {
        // Level B receipt contract; not native parity or a timing calibration.
        let timed = FsUaeWaitOutcome::Captured(Some(Duration::from_millis(1250)));
        assert_eq!(timed.single_case_seconds(1, true), Some(1.25));
        for outcome in [FsUaeWaitOutcome::Exited, FsUaeWaitOutcome::Captured(None)] {
            assert_eq!(outcome.single_case_seconds(1, true), None);
        }
        for count in [0, 2, 10] {
            assert_eq!(timed.single_case_seconds(count, true), None);
        }
        assert_eq!(timed.single_case_seconds(1, false), None);
    }

    #[test]
    fn launcher_handoff_waits_for_a_spawned_emulator() {
        let launcher_exited_at = Instant::now();
        let after_grace =
            launcher_exited_at + Duration::from_millis(FS_UAE_LAUNCHER_HANDOFF_GRACE_MS + 1);

        assert!(!launcher_exit_is_terminal(
            launcher_exited_at,
            after_grace,
            true
        ));
        assert!(!launcher_exit_is_terminal(
            launcher_exited_at,
            launcher_exited_at,
            false
        ));
        assert!(launcher_exit_is_terminal(
            launcher_exited_at,
            after_grace,
            false
        ));
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn macos_inner_binary_launches_through_the_application_bundle() {
        let args = vec!["case.fs-uae".to_string(), "--headless=1".to_string()];
        let command =
            fs_uae_launch_command("/Applications/FS-UAE.app/Contents/MacOS/fs-uae", &args);

        assert_eq!(command.get_program(), "/usr/bin/open");
        assert_eq!(
            command
                .get_args()
                .map(|arg| arg.to_string_lossy().into_owned())
                .collect::<Vec<_>>(),
            [
                "-W",
                "-n",
                "/Applications/FS-UAE.app",
                "--args",
                "case.fs-uae",
                "--headless=1",
            ]
        );
    }

    #[test]
    fn capture_config_defaults_to_standard_smoke_files() {
        let artifact_dir = Path::new("/tmp/opforge-fsuae-smoke");
        let capture = FsUaeCaptureConfig {
            start_paths: capture_path_set(artifact_dir, None, FS_UAE_DEFAULT_START_FILE),
            ready_paths: capture_path_set(artifact_dir, None, FS_UAE_DEFAULT_READY_FILE),
            stdout_paths: capture_path_set(artifact_dir, None, FS_UAE_DEFAULT_STDOUT_FILE),
            stderr_paths: capture_path_set(artifact_dir, None, FS_UAE_DEFAULT_STDERR_FILE),
            exit_code_paths: capture_path_set(artifact_dir, None, FS_UAE_DEFAULT_EXIT_CODE_FILE),
            timeout: Duration::from_millis(FS_UAE_DEFAULT_TIMEOUT_MS),
            post_start_timeout: Duration::from_millis(FS_UAE_DEFAULT_POST_START_TIMEOUT_MS),
            poll_interval: Duration::from_millis(FS_UAE_DEFAULT_POLL_MS),
        };

        assert_eq!(
            capture.start_paths.primary,
            artifact_dir.join(FS_UAE_DEFAULT_START_FILE)
        );
        assert_eq!(
            capture.ready_paths.primary,
            artifact_dir.join(FS_UAE_DEFAULT_READY_FILE)
        );
        assert_eq!(
            capture.stdout_paths.primary,
            artifact_dir.join(FS_UAE_DEFAULT_STDOUT_FILE)
        );
        assert_eq!(
            capture.stderr_paths.primary,
            artifact_dir.join(FS_UAE_DEFAULT_STDERR_FILE)
        );
        assert_eq!(
            capture.exit_code_paths.primary,
            artifact_dir.join(FS_UAE_DEFAULT_EXIT_CODE_FILE)
        );
        assert_eq!(
            capture.timeout,
            Duration::from_millis(FS_UAE_DEFAULT_TIMEOUT_MS)
        );
        assert_eq!(
            capture.post_start_timeout,
            Duration::from_millis(FS_UAE_DEFAULT_POST_START_TIMEOUT_MS)
        );
        assert_eq!(
            capture.poll_interval,
            Duration::from_millis(FS_UAE_DEFAULT_POLL_MS)
        );
    }

    #[test]
    fn resolve_capture_path_keeps_absolute_overrides() {
        let artifact_dir = Path::new("/tmp/opforge-fsuae-smoke");
        let absolute = Path::new("/var/tmp/opforge-smoke.ready");

        assert_eq!(
            resolve_capture_path(artifact_dir, absolute.to_str().expect("absolute path text")),
            absolute
        );
        assert_eq!(
            resolve_capture_path(artifact_dir, "relative/output.txt"),
            artifact_dir.join("relative/output.txt")
        );
    }

    #[test]
    fn guest_exit_code_takes_precedence_over_launcher_status() {
        assert!(determine_smoke_success(Some(0), false));
        assert!(!determine_smoke_success(Some(5), true));
        assert!(!determine_smoke_success(None, true));
        assert!(!determine_smoke_success(None, false));
    }

    #[test]
    fn every_generic_run_requires_fresh_guest_completion_even_for_expected_failure() {
        assert!(require_completed_guest_protocol("positive", true, Some(0), "", "").is_ok());
        assert!(
            require_completed_guest_protocol("expected-failure", true, Some(7), "", "").is_ok()
        );
        let missing_marker =
            require_completed_guest_protocol("missing-marker", false, Some(7), "out", "err")
                .expect_err("missing marker must fail closed");
        assert!(missing_marker.contains("stdout:\nout\nstderr:\nerr"));
        assert!(require_completed_guest_protocol("missing-exit", true, None, "", "").is_err());
    }

    #[test]
    fn byte_mismatch_description_reports_the_exact_first_divergence() {
        assert_eq!(
            describe_first_byte_mismatch(&[0x10, 0x22], &[0x10, 0x33]),
            "1 differing byte(s); first mismatch at offset 1: native=0x22, Rust=0x33"
        );
        assert_eq!(
            describe_first_byte_mismatch(&[0x10, 0x22, 0x30, 0x44], &[0x10, 0x33, 0x30, 0x55]),
            "2 differing byte(s); first mismatch at offset 1: native=0x22, Rust=0x33; last mismatch at offset 3: native=0x44, Rust=0x55"
        );
        assert_eq!(
            describe_first_byte_mismatch(&[0x10], &[0x10, 0x33]),
            "native output ends at offset 1; next Rust byte is 0x33"
        );
        assert_eq!(
            describe_first_byte_mismatch(&[0x10, 0x22], &[0x10]),
            "Rust output ends at offset 1; next native byte is 0x22"
        );
    }

    #[test]
    fn artifact_diagnostics_localize_hunk_payload_after_header_difference() {
        let image = |payload: &[u32]| {
            let mut words = vec![
                0x3f3,
                0,
                3,
                0,
                2,
                payload.len() as u32,
                1,
                5,
                0x3e9,
                payload.len() as u32,
            ];
            words.extend_from_slice(payload);
            words.extend_from_slice(&[
                0x3ec, 1, 1, 0, 0, 0x3f2, 0x3ea, 1, 0x11223344, 0x3f2, 0x3eb, 5, 0x3f2,
            ]);
            words
                .into_iter()
                .flat_map(u32::to_be_bytes)
                .collect::<Vec<_>>()
        };
        let native = image(&[0x11223344, 0x55667788, 0x99aabbcc]);
        let rust = image(&[0x11223344, 0xdeadbeef, 0x99aabbcc, 0]);
        let detail = describe_artifact_mismatch(&native, &rust);
        assert!(detail.contains("first mismatch at offset 23"), "{detail}");
        assert!(
            detail.contains("segment 0 CODE allocation=12 body=12 reloc32=1"),
            "{detail}"
        );
        assert!(
            detail.contains("segment 0 CODE allocation=16 body=16 reloc32=1"),
            "{detail}"
        );
        assert!(
            detail.contains("segment 1 DATA allocation=4 body=4 reloc32=0"),
            "{detail}"
        );
        assert!(
            detail.contains("segment 2 BSS allocation=20 body=0 reloc32=0"),
            "{detail}"
        );
        assert!(
            detail.contains(
                "segment=0; payload_offset=4; native_file_offset=44; Rust_file_offset=44"
            ),
            "{detail}"
        );
        assert!(
            detail.contains("native payload window [0..12): 11 22 33 44 55 66 77 88"),
            "{detail}"
        );
        assert!(
            detail.contains("Rust payload window [0..16): 11 22 33 44 de ad be ef"),
            "{detail}"
        );

        let truncated = &native[..native.len() - 1];
        let raw = describe_artifact_mismatch(truncated, &rust);
        assert!(!raw.contains("Hunk native"));
        assert!(raw.contains("native byte window"));
        assert!(raw.contains("Rust byte window"));
    }

    #[test]
    fn mismatch_windows_are_bounded_and_cover_missing_payload_tails() {
        let bytes = [0xaa; 100];
        let window = describe_byte_window(&bytes, 50);
        assert!(window.starts_with("[34..67): "));
        assert_eq!(
            window
                .split(": ")
                .nth(1)
                .unwrap()
                .split_whitespace()
                .count(),
            33
        );
        assert_eq!(describe_byte_window(&[], 0), "[0..0): ");
        assert!(describe_byte_window(&bytes[..40], 40).starts_with("[24..40): "));
    }

    #[test]
    fn hunk_diagnostics_report_later_segment_gaps_and_relocation_identity_changes() {
        let image = |address, data: &[u32], target| {
            let mut words = vec![
                0x3f3,
                0,
                2,
                0,
                1,
                1,
                data.len() as u32,
                0x3e9,
                1,
                address,
                0x3ec,
                1,
                target,
                0,
                0,
                0x3f2,
                0x3ea,
                data.len() as u32,
            ];
            words.extend_from_slice(data);
            words.push(0x3f2);
            words
                .into_iter()
                .flat_map(u32::to_be_bytes)
                .collect::<Vec<_>>()
        };
        let rust = image(20, &[0x11223344, 0x55667788], 1);
        let address_only =
            describe_artifact_mismatch(&image(16, &[0x11223344, 0x55667788], 1), &rust);
        assert!(
            address_only.contains("Hunk payload difference: segment=0; payload_offset=3"),
            "{address_only}"
        );
        assert!(!address_only.contains("Hunk payload difference: segment=1"));
        assert!(!address_only.contains("relocation identities differ"));

        let gap = describe_artifact_mismatch(&image(16, &[0x11223344], 1), &rust);
        assert!(gap.contains("Hunk payload difference: segment=0"), "{gap}");
        assert!(
            gap.contains("Hunk payload difference: segment=1; payload_offset=4"),
            "{gap}"
        );
        assert!(gap.contains("native output ends at offset 4"), "{gap}");

        let identities =
            describe_artifact_mismatch(&image(20, &[0x11223344, 0x55667788], 0), &rust);
        assert!(identities.contains("Hunk relocation identities differ: segment=0; first_sorted_record=0; native=Some((0, 0)); Rust=Some((0, 1))"), "{identities}");
        assert!(!identities.contains("Hunk payload difference:"));
        assert!(!identities.starts_with("outputs are equal"));
    }

    #[test]
    fn batch_case_success_requires_done_marker_and_zero_exit() {
        assert!(determine_batch_case_success(true, true, Some(0), false));
        assert!(!determine_batch_case_success(false, true, Some(0), true));
        assert!(!determine_batch_case_success(true, false, Some(0), true));
        assert!(!determine_batch_case_success(true, true, None, true));
        assert!(!determine_batch_case_success(true, true, Some(7), true));
    }

    fn proof_test_run(
        protocol_completed: bool,
        exit_code: Option<i32>,
        output: Option<&[u8]>,
    ) -> FsUaeSmokeRun {
        let mut captured_artifacts = BTreeMap::new();
        if let Some(output) = output {
            captured_artifacts.insert(
                PathBuf::from(FS_UAE_MOUNTED_WORK_DIR_NAME)
                    .join(FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE),
                output.to_vec(),
            );
        }
        FsUaeSmokeRun {
            example_name: "opforge",
            source_path: PathBuf::from("/ephemeral/Work/input.asm"),
            artifact_dir: PathBuf::from("/ephemeral"),
            hunk_path: PathBuf::from("/ephemeral/opforge"),
            stdout: String::new(),
            stderr: "diagnostic".to_string(),
            exit_code,
            protocol_completed,
            start_to_done_host_seconds: None,
            native_image_digest: None,
            success: protocol_completed && exit_code == Some(0),
            verified_output: None,
            captured_artifacts,
        }
    }

    #[test]
    fn absence_proof_requires_fresh_zero_exit_and_missing_artifacts() {
        let case = OpforgeNativeCliParityCase {
            name: "absence-proof",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b".byte 1"),
            command_template: Some("--cpu 68020 source.asm"),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(&["Work/unrequested.lst"]),
        };
        for (completed, exit) in [(false, Some(0)), (true, None), (true, Some(20))] {
            assert!(verify_native_cli_case_proof(
                &case,
                &mut proof_test_run(completed, exit, None)
            )
            .is_err());
        }
        let mut run = proof_test_run(true, Some(0), None);
        verify_native_cli_case_proof(&case, &mut run).unwrap();
        run.captured_artifacts
            .insert(PathBuf::from("Work/unrequested.lst"), Vec::new());
        assert!(verify_native_cli_case_proof(&case, &mut run).is_err());
        let empty_case = OpforgeNativeCliParityCase {
            name: "empty-absence-proof",
            proof: OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(&[]),
            ..case
        };
        assert!(verify_native_cli_case_proof(
            &empty_case,
            &mut proof_test_run(true, Some(0), None)
        )
        .is_err());
    }

    #[test]
    fn informational_proof_requires_fresh_zero_exit_and_isolated_stdout() {
        let case = OpforgeNativeCliParityCase {
            name: "informational-proof",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: None,
            command_template: Some("--help"),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::SuccessfulExitContaining("Usage:"),
        };
        let path = PathBuf::from(FS_UAE_MOUNTED_WORK_DIR_NAME)
            .join(FS_UAE_OPFORGE_NATIVE_CLI_CASE_ARTIFACTS_DIR)
            .join(opforge_native_cli_batch_case_name(0))
            .join(FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDOUT_FILE);
        for (completed, exit, text) in [
            (false, Some(0), "Usage:"),
            (true, Some(20), "Usage:"),
            (true, None, "Usage:"),
            (true, Some(0), "unrelated text"),
        ] {
            let mut run = proof_test_run(completed, exit, None);
            run.stdout = "Usage: stale launcher text".into();
            run.captured_artifacts
                .insert(path.clone(), text.as_bytes().to_vec());
            assert!(verify_native_cli_case_proof(&case, &mut run).is_err());
        }
        let mut run = proof_test_run(true, Some(0), None);
        run.captured_artifacts
            .insert(path, b"Usage: fresh help".to_vec());
        verify_native_cli_case_proof(&case, &mut run).unwrap();
        assert_eq!(
            run.verified_output.as_deref(),
            Some(b"Usage: fresh help".as_slice())
        );
    }

    #[test]
    fn failed_proofs_retain_fresh_completed_case_timing_without_verification() {
        let case = OpforgeNativeCliParityCase {
            name: "timed-failure",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b"input"),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &[0xa9, 0x42],
            },
        };
        for exit in [0, 7] {
            let mut run = proof_test_run(true, Some(exit), Some(&[0xa9, 0x43]));
            run.captured_artifacts.insert(
                PathBuf::from("Work/guest-memory.txt"),
                b"total 7800856 571368 8372224 5768176\n".to_vec(),
            );
            run.start_to_done_host_seconds =
                FsUaeWaitOutcome::Captured(Some(Duration::from_millis(1250)))
                    .single_case_seconds(1, run.protocol_completed);
            let error = verify_native_cli_case_proof(&case, &mut run).unwrap_err();
            assert!(error.contains("guest memory before launch:\ntotal 7800856"));
            assert!(
                error.contains("start_to_done_host_seconds=1.250000000"),
                "{error}"
            );
            assert!(
                error.contains(&format!("guest_exit=Some({exit}); proof_verified=false")),
                "{error}"
            );
            assert!(run.verified_output.is_none());
        }
        let mut negative = case;
        negative.proof = OpforgeNativeCliProof::ExpectedFailureWithDiagnostic;
        let mut run = proof_test_run(true, Some(0), None);
        run.start_to_done_host_seconds = Some(1.25);
        assert!(verify_native_cli_case_proof(&negative, &mut run)
            .unwrap_err()
            .contains("proof_verified=false"));

        run.protocol_completed = false;
        assert!(!verify_native_cli_case_proof(&case, &mut run)
            .unwrap_err()
            .contains("case timing:"));
        assert!(run.verified_output.is_none());
    }

    #[test]
    fn exact_artifact_set_requires_every_same_case_path_and_byte() {
        let primary = [0xa9, 0x42];
        let map = b"Regions\n";
        let artifacts = [
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &primary,
            },
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/build/case.map",
                rust_oracle: map,
            },
        ];
        let case = OpforgeNativeCliParityCase {
            name: "complete-artifact-set",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b".mapfile \"build/case.map\"\n"),
            command_template: Some("{input} --bin {bin} --cpu 65c02"),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExactArtifacts(&artifacts),
        };

        let mut missing = proof_test_run(true, Some(0), Some(&primary));
        assert!(verify_native_cli_case_proof(&case, &mut missing).is_err());

        let mut mismatch = proof_test_run(true, Some(0), Some(&primary));
        mismatch
            .captured_artifacts
            .insert(PathBuf::from("Work/build/case.map"), b"wrong".to_vec());
        assert!(verify_native_cli_case_proof(&case, &mut mismatch).is_err());

        let mut exact = proof_test_run(true, Some(0), Some(&primary));
        exact
            .captured_artifacts
            .insert(PathBuf::from("Work/build/case.map"), map.to_vec());
        verify_native_cli_case_proof(&case, &mut exact)
            .expect("every declared artifact is fresh and exact");
    }

    #[test]
    fn native_cli_case_identity_is_derived_from_actual_cpu_source_and_command() {
        let base = OpforgeNativeCliParityCase {
            name: "identity",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b"lda #1\n"),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };
        let mut changed_source = base;
        changed_source.source_override = Some(b"lda #2\n");
        let mut changed_command = base;
        changed_command.command_template = Some("{input} --bin {bin} --cpu 65c02");
        let mut changed_oracle = base;
        changed_oracle.proof = OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: b"different-rust-oracle",
        };
        let mut renamed = base;
        renamed.name = "display-name-must-not-select-oracle";

        assert_ne!(
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::CompactCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &changed_source,
                None,
                NativeCliParityExecutable::CompactCli,
                None,
            )
        );
        assert_ne!(
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::CompactCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &changed_command,
                None,
                NativeCliParityExecutable::CompactCli,
                None,
            )
        );
        assert_ne!(
            opforge_native_cli_case_identity(
                &base,
                Some(b"package-a"),
                NativeCliParityExecutable::CompactCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &base,
                Some(b"package-b"),
                NativeCliParityExecutable::CompactCli,
                None,
            )
        );
        assert_ne!(
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::CompactCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &changed_oracle,
                None,
                NativeCliParityExecutable::CompactCli,
                None,
            )
        );
        assert_eq!(
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::CompactCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &renamed,
                None,
                NativeCliParityExecutable::CompactCli,
                None,
            )
        );
        assert_ne!(
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::CompactCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::CompactCli,
                Some(b"captured-bootstrap"),
            ),
            "captured self-host executable bytes must bind the fresh case identity"
        );
    }

    #[test]
    fn fresh_marker_protocol_rejects_stale_or_wrong_case_text() {
        let dir = std::env::temp_dir().join(format!(
            "opforge-fsuae-proof-marker-{}",
            opforge_native_cli_run_challenge()
        ));
        let paths = opforge_native_cli_batch_case_paths(&dir, 0, "fresh", "actual-case");
        fs::create_dir_all(&paths.protocol_dir).expect("create marker test directory");
        fs::write(
            &paths.started_path,
            "OPFORGE-FS-UAE-PROOF-V1 START stale actual-case",
        )
        .expect("write stale start marker");
        fs::write(
            &paths.done_path,
            "OPFORGE-FS-UAE-PROOF-V1 DONE fresh wrong-case",
        )
        .expect("write wrong-case done marker");

        assert_ne!(
            read_optional_text(&paths.started_path)
                .expect("read start marker")
                .as_deref()
                .map(str::trim),
            Some(paths.expected_started.as_str())
        );
        assert_ne!(
            read_optional_text(&paths.done_path)
                .expect("read done marker")
                .as_deref()
                .map(str::trim),
            Some(paths.expected_done.as_str())
        );
        fs::remove_dir_all(&dir).expect("remove marker test directory");
    }

    #[test]
    fn ephemeral_artifact_guard_removes_all_case_evidence_on_drop() {
        let dir = std::env::temp_dir().join(format!(
            "opforge-fsuae-proof-cleanup-{}",
            opforge_native_cli_run_challenge()
        ));
        fs::create_dir_all(dir.join("Work/case_artifacts/case_0000"))
            .expect("create ephemeral proof tree");
        fs::write(
            dir.join("Work/case_artifacts/case_0000/opforge_fsuae_smoke.done"),
            "stale",
        )
        .expect("write ephemeral proof marker");
        {
            let _guard = EphemeralArtifactDir(dir.clone());
        }
        assert!(!dir.exists(), "all stored case evidence must be removed");
    }

    #[test]
    fn proof_failure_still_removes_all_case_evidence() {
        let expected = [0xa9, 0x42];
        let case = OpforgeNativeCliParityCase {
            name: "cleanup-on-proof-failure",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b"lda #$42\n"),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &expected,
            },
        };
        let dir = std::env::temp_dir().join(format!(
            "opforge-fsuae-proof-failure-cleanup-{}",
            opforge_native_cli_run_challenge()
        ));
        fs::create_dir_all(&dir).expect("create proof-failure directory");
        fs::write(dir.join("stale-output.bin"), b"wrong").expect("write wrong proof output");
        let mut run = proof_test_run(true, Some(0), Some(b"wrong"));

        let result = {
            let _guard = EphemeralArtifactDir(dir.clone());
            verify_native_cli_case_proof(&case, &mut run)
        };

        assert!(result.is_err());
        assert!(!dir.exists(), "failed proof evidence must be removed");
    }

    #[test]
    fn native_cli_guest_stack_prefix_preserves_default_and_sets_explicit_stack() {
        assert_eq!(
            native_cli_guest_batch_prefix(None).unwrap(),
            "FailAt 999\nCD Work:\n"
        );
        for bytes in [4096, 65536, 1_048_576] {
            assert_eq!(
                native_cli_guest_batch_prefix(Some(&bytes.to_string())).unwrap(),
                format!("FailAt 999\nStack {bytes}\nCD Work:\n")
            );
        }
    }

    #[test]
    fn native_cli_guest_stack_prefix_rejects_invalid_values() {
        for value in [
            "",
            "0",
            "4080",
            "4097",
            "1048592",
            "4294967296",
            "-4096",
            "+4096",
            " 4096",
            "4096\n",
            "64k",
        ] {
            assert!(
                native_cli_guest_batch_prefix(Some(value)).is_err(),
                "accepted {value:?}"
            );
        }
    }

    #[test]
    fn cleanup_only_targets_new_process_ids() {
        let baseline = BTreeSet::from([10_u32, 20_u32]);
        let current = BTreeSet::from([10_u32, 20_u32, 30_u32, 40_u32]);
        let difference = current.difference(&baseline).copied().collect::<Vec<_>>();

        assert_eq!(difference, vec![30_u32, 40_u32]);
    }

    #[test]
    fn guest_input_staging_rejects_absolute_and_parent_traversal_paths() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time after unix epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!("opforge-fsuae-safe-staging-{unique}"));
        let ephemeral_root = EphemeralArtifactDir(root.clone());
        fs::create_dir_all(&root).expect("create safe staging root");

        for unsafe_path in [
            "../escape.asm",
            "/tmp/escape.asm",
            "nested/../../escape.asm",
        ] {
            let error = stage_guest_input_bytes(&root, unsafe_path, b"unsafe")
                .expect_err("unsafe guest path must be rejected");
            assert!(
                error.contains("without traversal"),
                "unexpected error: {error}"
            );
        }

        drop(ephemeral_root);
        assert!(!root.exists(), "safe staging test tree must be ephemeral");
    }

    #[test]
    fn native_cli_batch_cases_isolate_guest_inputs_outputs_and_captured_artifacts() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time after unix epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!("opforge-fsuae-case-isolation-{unique}"));
        let ephemeral_root = EphemeralArtifactDir(root.clone());
        let first_work_dir = root.join("first/Work");
        let second_work_dir = root.join("second/Work");
        fs::create_dir_all(&first_work_dir).expect("create first mounted Work directory");
        fs::create_dir_all(&second_work_dir).expect("create second mounted Work directory");
        let first = opforge_native_cli_batch_case_paths(&first_work_dir, 0, "challenge", "first");
        let second =
            opforge_native_cli_batch_case_paths(&second_work_dir, 0, "challenge", "second");
        assert_ne!(first.artifact_dir, second.artifact_dir);
        assert_eq!(first.command_guest_work_dir, "Work:");
        assert_eq!(second.command_guest_work_dir, "Work:");
        stage_guest_input_bytes(&first.artifact_dir, "source.asm", b"lda #$11\n").unwrap();
        stage_guest_input_bytes(&second.artifact_dir, "source.asm", b"lda #$22\n").unwrap();
        assert_eq!(
            fs::read(first.artifact_dir.join("source.asm")).unwrap(),
            b"lda #$11\n"
        );
        assert_eq!(
            fs::read(second.artifact_dir.join("source.asm")).unwrap(),
            b"lda #$22\n"
        );

        let first_captured =
            BTreeMap::from([(PathBuf::from("Work/opforge_native_out.bin"), vec![0x11])]);
        let second_captured =
            BTreeMap::from([(PathBuf::from("Work/opforge_native_out.bin"), vec![0x22])]);
        assert_eq!(
            opforge_native_cli_case_captured_artifacts(&first_captured, &first),
            BTreeMap::from([(PathBuf::from("Work/opforge_native_out.bin"), vec![0x11])])
        );
        assert_eq!(
            opforge_native_cli_case_captured_artifacts(&second_captured, &second),
            BTreeMap::from([(PathBuf::from("Work/opforge_native_out.bin"), vec![0x22])])
        );
        drop(ephemeral_root);
        assert!(!root.exists(), "isolated case trees must be ephemeral");
    }

    #[test]
    fn binary_source_memory_comparison_enables_only_gated_telemetry_defines() {
        assert_eq!(
            exact_harness_assembly_defines(
                NativeCliParityExecutable::BinarySourceHarness,
                true,
                false,
                true,
            ),
            vec![
                "OPFORGE_DEBUG_CONTRACTS",
                "OPFORGE_MEMORY_TELEMETRY",
                "OPFORGE_TOKEN_DETAIL_TELEMETRY",
            ]
        );
        assert_eq!(
            exact_harness_assembly_defines(
                NativeCliParityExecutable::CompactCli,
                true,
                true,
                false,
            ),
            vec![
                "OPFORGE_DEBUG_CONTRACTS",
                "OPFORGE_MEMORY_TELEMETRY",
                "OPFORGE_PREPARATION_PROGRESS",
            ]
        );
        assert!(exact_harness_assembly_defines(
            NativeCliParityExecutable::ExprvmI64Harness,
            true,
            true,
            true,
        )
        .is_empty());
        assert!(exact_harness_assembly_defines(
            NativeCliParityExecutable::BinarySourceHarness,
            false,
            true,
            true,
        )
        .is_empty());
        assert_eq!(
            exact_harness_assembly_defines(NativeCliParityExecutable::CompactCli, true, true, true),
            vec![
                "OPFORGE_DEBUG_CONTRACTS",
                "OPFORGE_MEMORY_TELEMETRY",
                "OPFORGE_TOKEN_DETAIL_TELEMETRY",
                "OPFORGE_PREPARATION_PROGRESS",
            ]
        );
    }

    #[test]
    fn binary_source_memory_receipt_is_retained_with_case_artifacts() {
        let paths = opforge_native_cli_batch_case_paths(
            Path::new("/tmp/opforge-fsuae/Work"),
            0,
            "challenge",
            "case",
        );
        let captured = BTreeMap::from([
            (PathBuf::from("Work/memory.bin"), vec![0x4d, 0x45, 0x4d]),
            (
                PathBuf::from("Work/guest-memory.txt"),
                b"Kickstart 47.102\nMaximum 123456\n".to_vec(),
            ),
            (
                paths.captured_relative_prefix.join("output.bin"),
                vec![0x42],
            ),
        ]);

        assert_eq!(
            opforge_native_cli_case_captured_artifacts(&captured, &paths),
            BTreeMap::from([
                (PathBuf::from("Work/memory.bin"), vec![0x4d, 0x45, 0x4d]),
                (
                    PathBuf::from("Work/guest-memory.txt"),
                    b"Kickstart 47.102\nMaximum 123456\n".to_vec(),
                ),
                (PathBuf::from("Work/output.bin"), vec![0x42]),
            ])
        );
    }
}

#[cfg(test)]
#[path = "tests/native_package_loader.rs"]
mod native_package_loader;

#[cfg(test)]
#[path = "tests/native_package_loading_performance.rs"]
pub(crate) mod native_package_loading_performance;

#[cfg(test)]
#[path = "tests/compact_cli_input.rs"]
pub(crate) mod compact_cli_input;

#[cfg(test)]
#[path = "tests/native_cpu_selection.rs"]
mod native_cpu_selection;

#[cfg(test)]
#[path = "tests/compact_cli_diagnostics.rs"]
mod compact_cli_diagnostics;

#[path = "tests/compact_cli_outputs.rs"]
mod compact_cli_outputs;

#[path = "tests/compact_cli_records.rs"]
mod compact_cli_records;

#[cfg(test)]
#[path = "tests/native_expression_lists.rs"]
mod native_expression_lists;
#[cfg(test)]
#[path = "tests/native_expression_ranges.rs"]
mod native_expression_ranges;
#[cfg(test)]
#[path = "tests/native_layout_parity.rs"]
mod native_layout_parity;
#[cfg(test)]
#[path = "tests/native_preparation_values.rs"]
mod native_preparation_values;

#[cfg(test)]
#[path = "tests/native_forward_count_parity.rs"]
mod native_forward_count_parity;

#[path = "tests/compact_cli_metadata.rs"]
mod compact_cli_metadata;

#[cfg(test)]
#[path = "tests/native_active_includes.rs"]
mod native_active_includes;
#[cfg(test)]
#[path = "tests/native_assembly_session_probe.rs"]
mod native_assembly_session_probe;
#[cfg(test)]
#[path = "tests/native_binding_owner_states.rs"]
mod native_binding_owner_states;
#[cfg(test)]
#[path = "tests/native_binding_plan_grow.rs"]
mod native_binding_plan_grow;
#[cfg(test)]
#[path = "tests/native_compact_reservation.rs"]
mod native_compact_reservation;
#[cfg(test)]
#[path = "tests/native_composed_include_binding.rs"]
mod native_composed_include_binding;
#[path = "tests/native_data_narrowing.rs"]
mod native_data_narrowing;
#[path = "tests/native_forward_branches.rs"]
mod native_forward_branches;
#[cfg(test)]
#[path = "tests/native_guard_reuse.rs"]
mod native_guard_reuse;
#[path = "tests/native_guarded_iterables.rs"]
mod native_guarded_iterables;
#[cfg(test)]
#[path = "tests/native_include_collections.rs"]
mod native_include_collections;
#[cfg(test)]
#[path = "tests/native_iterable_loops.rs"]
mod native_iterable_loops;
#[cfg(test)]
#[path = "tests/native_macro_parity.rs"]
mod native_macro_parity;
#[path = "tests/native_macro_plan_clone.rs"]
mod native_macro_plan_clone;
#[path = "tests/native_macro_plan_limits.rs"]
mod native_macro_plan_limits;
#[path = "tests/native_match_controls.rs"]
mod native_match_controls;
#[cfg(test)]
#[path = "tests/native_member_owner_controls.rs"]
mod native_member_owner_controls;
#[cfg(test)]
#[path = "tests/native_name_store.rs"]
mod native_name_store;
#[cfg(test)]
#[path = "tests/native_package_heads.rs"]
mod native_package_heads;
#[cfg(test)]
#[path = "tests/native_scope_identity_grow.rs"]
mod native_scope_identity_grow;
#[cfg(test)]
#[path = "tests/native_scoped_layouts.rs"]
mod native_scoped_layouts;
#[cfg(test)]
#[path = "tests/native_scoped_loops.rs"]
mod native_scoped_loops;
#[cfg(test)]
#[path = "tests/native_source_cases.rs"]
mod native_source_cases;
#[path = "tests/native_static_include_graph.rs"]
mod native_static_include_graph;
#[path = "tests/native_struct_literal_values.rs"]
mod native_struct_literal_values;
#[cfg(test)]
#[path = "tests/native_struct_name_edges.rs"]
mod native_struct_name_edges;
#[path = "tests/native_struct_parameters.rs"]
mod native_struct_parameters;
#[path = "tests/native_struct_vm.rs"]
mod native_struct_vm;
#[cfg(test)]
#[path = "tests/native_template_imports.rs"]
mod native_template_imports;
#[path = "tests/native_ternary_values.rs"]
mod native_ternary_values;
#[cfg(test)]
#[path = "tests/native_text_encoding.rs"]
mod native_text_encoding;
#[path = "tests/native_typed_list_values.rs"]
mod native_typed_list_values;
#[path = "tests/native_wide_iterables.rs"]
mod native_wide_iterables;

#[cfg(test)]
#[path = "tests/native_compound_updates.rs"]
mod native_compound_updates;
#[cfg(test)]
#[path = "tests/native_expression_records.rs"]
mod native_expression_records;
#[cfg(test)]
#[path = "tests/native_named_scoped_loops.rs"]
mod native_named_scoped_loops;
#[cfg(test)]
#[path = "tests/native_scoped_while.rs"]
mod native_scoped_while;
#[cfg(test)]
#[path = "tests/native_while_layouts.rs"]
mod native_while_layouts;
#[cfg(test)]
#[path = "tests/native_while_loops.rs"]
mod native_while_loops;
#[cfg(test)]
#[path = "tests/native_wide_literals.rs"]
mod native_wide_literals;

#[cfg(test)]
#[path = "tests/native_labeled_conditionals.rs"]
mod native_labeled_conditionals;

#[cfg(test)]
#[path = "tests/native_statement_storage.rs"]
mod native_statement_storage;

#[cfg(test)]
#[path = "tests/native_statement_calls.rs"]
mod native_statement_calls;

#[cfg(test)]
#[path = "tests/native_origin_outputs.rs"]
mod native_origin_outputs;

#[cfg(test)]
#[path = "tests/native_statement_templates.rs"]
mod native_statement_templates;

#[cfg(test)]
#[path = "tests/native_statement_exposure.rs"]
mod native_statement_exposure;

#[cfg(test)]
#[path = "tests/native_statement_tail.rs"]
mod native_statement_tail;

#[cfg(test)]
#[path = "tests/native_output_spans.rs"]
mod native_output_spans;

#[cfg(test)]
#[path = "tests/native_flat_output.rs"]
mod native_flat_output;

#[cfg(test)]
#[path = "tests/native_binding_remap.rs"]
mod native_binding_remap;

#[cfg(test)]
#[path = "tests/native_package_members.rs"]
mod native_package_members;

#[cfg(test)]
#[path = "tests/native_capture_telemetry.rs"]
mod native_capture_telemetry;

#[cfg(test)]
#[path = "tests/native_capture_initialization.rs"]
mod native_capture_initialization;

#[cfg(test)]
#[path = "tests/native_hotspot_telemetry.rs"]
mod native_hotspot_telemetry;
#[path = "tests/native_symbol_buckets.rs"]
mod native_symbol_buckets;

#[cfg(test)]
#[path = "tests/native_full_selfhost_comparison.rs"]
mod native_full_selfhost_comparison;

#[cfg(test)]
#[path = "tests/native_preparation_benchmarks.rs"]
mod native_preparation_benchmarks;

#[cfg(test)]
#[path = "tests/native_canonical_bindings.rs"]
mod native_canonical_bindings;
#[cfg(test)]
#[path = "tests/native_fragment_controls.rs"]
mod native_fragment_controls;
#[cfg(test)]
#[path = "tests/native_source_controls.rs"]
mod native_source_controls;
#[cfg(test)]
#[path = "tests/native_store_probes.rs"]
mod native_store_probes;

#[cfg(test)]
#[path = "tests/native_incremental.rs"]
mod native_incremental;
#[cfg(test)]
#[path = "tests/native_module_compile.rs"]
mod native_module_compile;
#[cfg(test)]
#[path = "tests/native_module_images.rs"]
mod native_module_images;
#[cfg(test)]
#[path = "tests/native_module_pipeline.rs"]
mod native_module_pipeline;
#[cfg(test)]
#[path = "tests/native_module_shared_sections.rs"]
mod native_module_shared_sections;

#[cfg(test)]
#[path = "tests/native_instruction_fixup_identity.rs"]
mod native_instruction_fixup_identity;
#[cfg(test)]
#[path = "tests/native_module_geometry.rs"]
mod native_module_geometry;
