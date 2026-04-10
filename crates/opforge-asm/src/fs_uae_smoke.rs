use cli_core::LabelOutputFormat as CliLabelOutputFormat;
use engine::OutputFormat as EngineOutputFormat;
use engine::{default_cpu, run_assembly, AssemblyExecutionRequest, ExecutionMode};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};
use types::lockstep::ContinuationHead;
use vm::output_model::BinOutputSpec;

const FS_UAE_OPT_IN_ENV: &str = "OPFORGE_FS_UAE_SMOKE";
const FS_UAE_BIN_ENV: &str = "OPFORGE_FS_UAE_BIN";
const FS_UAE_ARGS_ENV: &str = "OPFORGE_FS_UAE_ARGS";
const FS_UAE_EXAMPLES: &[(&str, &str)] = &[
    (
        "helloworld",
        "examples/motorola68000/amigaos/helloworld.asm",
    ),
    ("writefile", "examples/motorola68000/amigaos/writefile.asm"),
];

pub(crate) struct FsUaeSmokeRun {
    pub(crate) example_name: &'static str,
    pub(crate) source_path: PathBuf,
    pub(crate) artifact_dir: PathBuf,
    pub(crate) hunk_path: PathBuf,
    pub(crate) stdout: String,
    pub(crate) stderr: String,
    pub(crate) success: bool,
}

pub(crate) enum FsUaeSmokeOutcome {
    Skipped(String),
    Completed { runs: Vec<FsUaeSmokeRun> },
}

pub(crate) fn run_hunk_smoke_from_env(workspace_root: &Path) -> Result<FsUaeSmokeOutcome, String> {
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}} and {{artifact_dir}} placeholders"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let mut runs = Vec::with_capacity(FS_UAE_EXAMPLES.len());
    for &(example_name, relative_source_path) in FS_UAE_EXAMPLES {
        match run_example_smoke(
            workspace_root,
            &fs_uae_bin,
            &args_text,
            example_name,
            relative_source_path,
        )? {
            ExampleSmokeResult::Run(run) => runs.push(run),
            ExampleSmokeResult::Skipped(reason) => return Ok(FsUaeSmokeOutcome::Skipped(reason)),
        }
    }

    Ok(FsUaeSmokeOutcome::Completed { runs })
}

fn create_artifact_dir(workspace_root: &Path, label: &str) -> Result<PathBuf, String> {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let dir = workspace_root
        .join("target")
        .join(format!("{label}-{nanos}"));
    fs::create_dir_all(&dir)
        .map_err(|err| format!("create artifact directory {}: {err}", dir.display()))?;
    Ok(dir)
}

enum ExampleSmokeResult {
    Run(FsUaeSmokeRun),
    Skipped(String),
}

fn run_example_smoke(
    workspace_root: &Path,
    fs_uae_bin: &str,
    args_text: &str,
    example_name: &'static str,
    relative_source_path: &str,
) -> Result<ExampleSmokeResult, String> {
    let source_path = workspace_root.join(relative_source_path);
    if !source_path.is_file() {
        return Err(format!(
            "expected FS-UAE smoke example source at {}",
            source_path.display()
        ));
    }

    let artifact_dir =
        create_artifact_dir(workspace_root, &format!("fs-uae-hunk-smoke-{example_name}"))?;
    let hunk_name_override = if example_name == "helloworld" {
        Some(format!("build/{example_name}.hunk"))
    } else {
        None
    };
    run_assembly(AssemblyExecutionRequest {
        root_path: &source_path,
        input_base: example_name,
        defines: &[],
        include_paths: &[],
        module_paths: &[],
        pp_macro_depth: 64,
        cpu_override: Some("68000"),
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
        suppress_outputs: false,
    })
    .map_err(|err| {
        format!(
            "assemble FS-UAE smoke example {} from {}: {}",
            example_name,
            source_path.display(),
            err.summary()
        )
    })?;

    let hunk_path = artifact_dir
        .join("build")
        .join(format!("{example_name}.hunk"));
    if !hunk_path.is_file() {
        return Err(format!(
            "expected generated Hunk artifact at {}",
            hunk_path.display()
        ));
    }

    let args = args_text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.replace("{hunk}", &hunk_path.to_string_lossy())
                .replace("{artifact_dir}", &artifact_dir.to_string_lossy())
                .replace("{example}", example_name)
        })
        .collect::<Vec<_>>();

    let output = match Command::new(fs_uae_bin)
        .args(&args)
        .current_dir(&artifact_dir)
        .output()
    {
        Ok(output) => output,
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

    Ok(ExampleSmokeResult::Run(FsUaeSmokeRun {
        example_name,
        source_path,
        artifact_dir,
        hunk_path,
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        success: output.status.success(),
    }))
}
