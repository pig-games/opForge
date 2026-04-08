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

pub(crate) enum FsUaeSmokeOutcome {
    Skipped(String),
    Completed {
        artifact_dir: PathBuf,
        hunk_path: PathBuf,
        stdout: String,
        stderr: String,
        success: bool,
    },
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

    let artifact_dir = create_artifact_dir(workspace_root, "fs-uae-hunk-smoke")?;
    let source_path = artifact_dir.join("smoke.asm");
    fs::write(&source_path, smoke_source()).map_err(|err| {
        format!(
            "write smoke assembly source {}: {err}",
            source_path.display()
        )
    })?;

    run_assembly(AssemblyExecutionRequest {
        root_path: &source_path,
        input_base: "fs-uae-hunk-smoke",
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
        header_title: "opForge Assembler FS-UAE smoke",
        output_sink: None,
        source_provider: None,
        execution_mode: ExecutionMode::Lockstep {
            continuation_head: ContinuationHead::Vm,
        },
        suppress_outputs: false,
    })
    .map_err(|err| format!("assemble FS-UAE smoke Hunk artifact: {}", err.summary()))?;

    let hunk_path = artifact_dir.join("build/out.hunk");
    if !hunk_path.is_file() {
        return Err(format!(
            "expected generated Hunk artifact at {}",
            hunk_path.display()
        ));
    }

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let args = args_text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.replace("{hunk}", &hunk_path.to_string_lossy())
                .replace("{artifact_dir}", &artifact_dir.to_string_lossy())
        })
        .collect::<Vec<_>>();

    let output = match Command::new(&fs_uae_bin)
        .args(&args)
        .current_dir(&artifact_dir)
        .output()
    {
        Ok(output) => output,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "FS-UAE binary '{fs_uae_bin}' was not found; install FS-UAE or set {FS_UAE_BIN_ENV}"
            )))
        }
        Err(err) => {
            return Err(format!(
                "launch FS-UAE binary '{fs_uae_bin}' for {}: {err}",
                hunk_path.display()
            ))
        }
    };

    Ok(FsUaeSmokeOutcome::Completed {
        artifact_dir,
        hunk_path,
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        success: output.status.success(),
    })
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

fn smoke_source() -> &'static str {
    ".cpu 68000\n.module main\n.region ram, $2000, $20ff\n.section code, kind=code\n.byte $4e, $75\n.endsection\n.place code in ram\n.output \"build/out.hunk\", format=hunk, sections=code\n.endmodule\n"
}
