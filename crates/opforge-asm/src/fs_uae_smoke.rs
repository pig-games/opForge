use cli_core::LabelOutputFormat as CliLabelOutputFormat;
use engine::OutputFormat as EngineOutputFormat;
use engine::{default_cpu, run_assembly, AssemblyExecutionRequest, ExecutionMode};
use package::encode_hierarchy_chunks_from_chunks;
use registry::registry::ModuleRegistry;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitStatus;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use types::lockstep::ContinuationHead;
use vm::builder::build_hierarchy_chunks_from_registry;
use vm::output_model::BinOutputSpec;

const FS_UAE_OPT_IN_ENV: &str = "OPFORGE_FS_UAE_SMOKE";
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
const FS_UAE_DEFAULT_TIMEOUT_MS: u64 = 120_000;
const FS_UAE_DEFAULT_POLL_MS: u64 = 250;
const FS_UAE_DEFAULT_POST_START_TIMEOUT_MS: u64 = 20_000;
const FS_UAE_LAUNCHER_STDOUT_FILE: &str = "fs_uae_launcher.stdout.log";
const FS_UAE_LAUNCHER_STDERR_FILE: &str = "fs_uae_launcher.stderr.log";
const FS_UAE_CONFIG_FILE_NAME: &str = "fs-uae-smoke.fs-uae";
const FS_UAE_LAST_GREEN_DIR_NAME: &str = "fs-uae-last-green";
const FS_UAE_LAST_GREEN_FILE_NAME: &str = "last_green.txt";
const FS_UAE_MOUNTED_WORK_DIR_NAME: &str = "Work";
const FS_UAE_MOUNTED_HUNK_ALIAS: &str = "build/opforge_fsuae_smoke.hunk";
const FS_UAE_STARTUP_HUNK_ALIAS: &str = "build/tkpkg_debug_cli.hunk";
const FS_UAE_TKPKG_SMOKE_INPUT_FILE: &str = "opforge_fsuae_smoke_input.asm";
const FS_UAE_TKPKG_SMOKE_INPUT_TEXT: &str = "move.b d0,d1\nmove.w d2,d3\n";
const FS_UAE_OPFORGE_NATIVE_CLI_INPUT_TEXT: &str =
    ".module main\n.use math\n.use math as m\n.endmodule\n";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE: &str = "OPFORGE_FS_UAE_NATIVE_CLI_6502_OUTPUT";
const FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_65C02_OUTPUT";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM10_INCLUDE_OUTPUT";
const FS_UAE_OPFORGE_NATIVE_CLI_MISSING_INCLUDE_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_MISSING_INCLUDE";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM13_OUTPUT_DIRECTIVE_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM13_OUTPUT_DIRECTIVE";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM14_OUTPUT_DIRECTIVE_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM14_OUTPUT_DIRECTIVE";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM15_OUTPUT_DIRECTIVE_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM15_OUTPUT_DIRECTIVE";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM16_LIST_OUTPUT_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM16_LIST_OUTPUT";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_ARTIFACT_MATRIX_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM17_ARTIFACT_MATRIX";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE: &str = "opforge_6502_native_cli_smoke.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_TEXT: &str =
    "start   lda #$42\n        sta $20\n        lda $20,x\n        sta $0200\n        lda $0200,x\n        lda $0200,y\ndone    jmp done\n";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE: &str = "opforge_native_out.bin";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_PRG_OUTPUT_FILE: &str = "opforge_native_out.prg";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_HEX_OUTPUT_FILE: &str = "opforge_native_out.hex";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_LST_OUTPUT_FILE: &str = "opforge_native_out.lst";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC_FILE: &str =
    "opforge_6502_unknown_mnemonic.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC_TEXT: &str = "start   wat #$42\n";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING_FILE: &str =
    "opforge_6502_unsupported_addressing.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING_TEXT: &str = "start   jmp $20,x\n";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_UNRESOLVED_LABEL_FILE: &str =
    "opforge_6502_unresolved_label.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_UNRESOLVED_LABEL_TEXT: &str = "start   jmp missing\n";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_BAD_ORG_FILE: &str = "opforge_6502_bad_org.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_BAD_ORG_TEXT: &str =
    "        .org missing\n        lda #$42\n";
const FS_UAE_OPFORGE_NATIVE_CLI_MODULE_FILE: &str = "math.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_MODULE_TEXT: &str =
    ".module math\n.use helper\nfoo     sta $0200\n.endmodule\n";
const FS_UAE_OPFORGE_NATIVE_CLI_NESTED_MODULE_FILE: &str = "helper.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_NESTED_MODULE_TEXT: &str =
    ".module helper\n        lda #$00\n.endmodule\n";
const FS_UAE_OPFORGE_NATIVE_CLI_INCLUDE_FILE: &str = "opforge_fsuae_include.inc";
const FS_UAE_OPFORGE_NATIVE_CLI_INCLUDE_TEXT: &str = "        lda #$01\n";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_A_FILE: &str = "opforge_include_root_a/defs.inc";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_A_TEXT: &str = "        .byte $11\n";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_B_FILE: &str = "opforge_include_root_b/defs.inc";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_B_TEXT: &str = "        .byte $22\n";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM12_VALUES_FILE: &str = "values.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_ITEM12_VALUES_TEXT: &str =
    ".module values\n.pub\nVALUE .const $37\n.endmodule\n";
const FS_UAE_OPFORGE_NATIVE_CLI_UNMATCHED_ENDMODULE_FILE: &str =
    "opforge_fsuae_unmatched_endmodule.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_UNMATCHED_ENDMODULE_TEXT: &str = ".endmodule\n";
const FS_UAE_OPFORGE_NATIVE_CLI_UNTERMINATED_MODULE_FILE: &str =
    "opforge_fsuae_unterminated_module.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_UNTERMINATED_MODULE_TEXT: &str =
    ".module main\n        move.b d0,d1\n";
const FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_FILE: &str = "opforge_fsuae_bad_use.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_TEXT: &str = ".module main\n.use math ()\n.endmodule\n";
const FS_UAE_OPFORGE_NATIVE_CLI_MISSING_MODULE_FILE: &str = "opforge_fsuae_missing_module.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_MISSING_MODULE_TEXT: &str =
    ".module main\n.use missing\n.endmodule\n";
const FS_UAE_TKPKG_MANIFEST_FILE: &str = "opforge_fsuae_tkpkg_manifest.txt";
const FS_UAE_TKPKG_MANIFEST_INPUT_DIR: &str = "opforge_fsuae_tkpkg_inputs";
const FS_UAE_TKPKG_DEBUG_CLI_EXAMPLE_NAME: &str = "tkpkg_debug_cli";
const FS_UAE_TKPKG_DEBUG_CLI_SOURCE_PATH: &str =
    "native/motorola68000/amigaos/test-harnesses/tkpkg/tkpkg_debug_cli.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME: &str = "opforge_cli";
const FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH: &str = "native/motorola68000/amigaos/main.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_PACKAGE_PATH: &str =
    "native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm";
const FS_UAE_OPFORGE_NATIVE_CLI_PACKAGE_GUEST_FILE: &str = "opforge_cli_package.opasm";
const FS_UAE_OPFORGE_NATIVE_CLI_OVERSIZED_PACKAGE_GUEST_FILE: &str =
    "opforge_cli_package_oversized.opasm";
const FS_UAE_TKPKG_DEBUG_CLI_PACKAGE_NAME: &str = "tkpkg_debug_cli_package.opasm";
const FS_UAE_TKPKG_DEBUG_CLI_PACKAGE_OVERRIDE_NAME: &str = "tkpkg_debug_cli_package_override.opasm";
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
        "tkpkg_debug_cli",
        "native/motorola68000/amigaos/test-harnesses/tkpkg/tkpkg_debug_cli.asm",
        "68020",
    ),
    (
        "prvm_smoke",
        "native/motorola68000/amigaos/test-harnesses/prvm/prvm_smoke.asm",
        "68020",
    ),
    (
        "prvm_line_iterator_smoke",
        "native/motorola68000/amigaos/test-harnesses/prvm/prvm_line_iterator_smoke.asm",
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
    pub(crate) success: bool,
}

pub(crate) enum FsUaeSmokeOutcome {
    Skipped(String),
    Completed { runs: Vec<FsUaeSmokeRun> },
}

pub(crate) struct TkpkgDebugCliManifestCase<'a> {
    pub(crate) name: &'a str,
    pub(crate) cpu_id: &'a str,
    pub(crate) source: &'a [u8],
}

pub(crate) struct OpforgeNativeCliFailureCase<'a> {
    pub(crate) name: &'a str,
    pub(crate) define: &'a str,
    pub(crate) expected_diagnostic: &'a str,
}

pub(crate) struct OpforgeNativeCliMosFixtureCase<'a> {
    pub(crate) name: &'a str,
    pub(crate) cpu_id: &'a str,
    pub(crate) source: &'a [u8],
    pub(crate) package_bytes: &'a [u8],
}

struct OpforgeNativeCliInputOverride<'a> {
    source: &'a [u8],
    package_bytes: &'a [u8],
}

struct GitHeadProvenance {
    commit: String,
    commit_unix_seconds: String,
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
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let mut runs = Vec::with_capacity(FS_UAE_EXAMPLES.len());
    for &(example_name, relative_source_path, cpu_override) in FS_UAE_EXAMPLES {
        match run_example_smoke(
            workspace_root,
            &fs_uae_bin,
            &args_text,
            example_name,
            relative_source_path,
            cpu_override,
        )? {
            ExampleSmokeResult::Run(run) => runs.push(run),
            ExampleSmokeResult::Skipped(reason) => return Ok(FsUaeSmokeOutcome::Skipped(reason)),
        }
    }

    Ok(FsUaeSmokeOutcome::Completed { runs })
}

pub(crate) fn run_opforge_native_cli_stub_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    match run_example_smoke(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
        FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH,
        "68020",
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

pub(crate) fn run_opforge_native_cli_6502_output_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    match run_example_smoke_with_extra_defines(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
        FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH,
        "68020",
        &[FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE],
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

pub(crate) fn run_opforge_native_cli_mos_fixture_outputs_from_env(
    workspace_root: &Path,
    cases: &[OpforgeNativeCliMosFixtureCase<'_>],
) -> Result<FsUaeSmokeOutcome, String> {
    if cases.is_empty() {
        return Err(
            "native opForge CLI MOS fixture FS-UAE mode requires at least one case".to_string(),
        );
    }
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let mut runs = Vec::with_capacity(cases.len());
    for case in cases {
        let define = match case.cpu_id {
            "m6502" => FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE,
            "65c02" => FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE,
            other => {
                return Err(format!(
                    "unsupported native opForge CLI MOS fixture CPU id '{other}' for {}",
                    case.name
                ))
            }
        };
        let input_override = OpforgeNativeCliInputOverride {
            source: case.source,
            package_bytes: case.package_bytes,
        };
        let run = run_example_smoke_with_extra_defines_and_native_cli_input(
            workspace_root,
            &fs_uae_bin,
            &args_text,
            FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
            FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH,
            "68020",
            &[define],
            Some(&input_override),
        )?;
        match run {
            ExampleSmokeResult::Run(run) => runs.push(run),
            ExampleSmokeResult::Skipped(reason) => return Ok(FsUaeSmokeOutcome::Skipped(reason)),
        }
    }

    Ok(FsUaeSmokeOutcome::Completed { runs })
}

pub(crate) fn run_opforge_native_cli_item10_include_from_env(
    workspace_root: &Path,
    package_bytes: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let include_source = "        .include \"defs.inc\"\n        lda #$44\n";
    let missing_include_source = "        .include \"missing.inc\"\n        lda #$44\n";
    let cases = [
        (
            FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE,
            include_source.as_bytes(),
        ),
        (
            FS_UAE_OPFORGE_NATIVE_CLI_MISSING_INCLUDE_DEFINE,
            missing_include_source.as_bytes(),
        ),
    ];
    let mut runs = Vec::with_capacity(cases.len());
    for (define, source) in cases {
        let input_override = OpforgeNativeCliInputOverride {
            source,
            package_bytes,
        };
        let run = run_example_smoke_with_extra_defines_and_native_cli_input(
            workspace_root,
            &fs_uae_bin,
            &args_text,
            FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
            FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH,
            "68020",
            &[define],
            Some(&input_override),
        )?;
        match run {
            ExampleSmokeResult::Run(run) => runs.push(run),
            ExampleSmokeResult::Skipped(reason) => return Ok(FsUaeSmokeOutcome::Skipped(reason)),
        }
    }

    Ok(FsUaeSmokeOutcome::Completed { runs })
}

pub(crate) fn run_opforge_native_cli_item13_output_directive_from_env(
    workspace_root: &Path,
    source: &[u8],
    package_bytes: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let input_override = OpforgeNativeCliInputOverride {
        source,
        package_bytes,
    };
    match run_example_smoke_with_extra_defines_and_native_cli_input(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
        FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH,
        "68020",
        &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM13_OUTPUT_DIRECTIVE_DEFINE],
        Some(&input_override),
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

pub(crate) fn run_opforge_native_cli_item14_prg_output_from_env(
    workspace_root: &Path,
    success_source: &[u8],
    wide_loadaddr_source: &[u8],
    package_bytes: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let cases = [success_source, wide_loadaddr_source];
    let mut runs = Vec::with_capacity(cases.len());
    for source in cases {
        let input_override = OpforgeNativeCliInputOverride {
            source,
            package_bytes,
        };
        let run = run_example_smoke_with_extra_defines_and_native_cli_input(
            workspace_root,
            &fs_uae_bin,
            &args_text,
            FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
            FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH,
            "68020",
            &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM14_OUTPUT_DIRECTIVE_DEFINE],
            Some(&input_override),
        )?;
        match run {
            ExampleSmokeResult::Run(run) => runs.push(run),
            ExampleSmokeResult::Skipped(reason) => return Ok(FsUaeSmokeOutcome::Skipped(reason)),
        }
    }

    Ok(FsUaeSmokeOutcome::Completed { runs })
}

pub(crate) fn run_opforge_native_cli_item15_hex_output_from_env(
    workspace_root: &Path,
    source: &[u8],
    package_bytes: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let input_override = OpforgeNativeCliInputOverride {
        source,
        package_bytes,
    };
    match run_example_smoke_with_extra_defines_and_native_cli_input(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
        FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH,
        "68020",
        &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM15_OUTPUT_DIRECTIVE_DEFINE],
        Some(&input_override),
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

pub(crate) fn run_opforge_native_cli_item16_listing_output_from_env(
    workspace_root: &Path,
    source: &[u8],
    package_bytes: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let input_override = OpforgeNativeCliInputOverride {
        source,
        package_bytes,
    };
    match run_example_smoke_with_extra_defines_and_native_cli_input(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
        FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH,
        "68020",
        &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM16_LIST_OUTPUT_DEFINE],
        Some(&input_override),
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

pub(crate) fn run_opforge_native_cli_item17_artifact_matrix_from_env(
    workspace_root: &Path,
    sources: [&[u8]; 4],
    package_bytes: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let mut runs = Vec::with_capacity(sources.len());
    for source in sources {
        let input_override = OpforgeNativeCliInputOverride {
            source,
            package_bytes,
        };
        let run = run_example_smoke_with_extra_defines_and_native_cli_input(
            workspace_root,
            &fs_uae_bin,
            &args_text,
            FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
            FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH,
            "68020",
            &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_ARTIFACT_MATRIX_DEFINE],
            Some(&input_override),
        )?;
        match run {
            ExampleSmokeResult::Run(run) => runs.push(run),
            ExampleSmokeResult::Skipped(reason) => return Ok(FsUaeSmokeOutcome::Skipped(reason)),
        }
    }

    Ok(FsUaeSmokeOutcome::Completed { runs })
}

fn mos6502_native_cli_single_cpu_package_bytes() -> Result<Vec<u8>, String> {
    let mut registry = ModuleRegistry::new();
    registry.register_family(Box::new(
        families::families::mos6502::module::MOS6502FamilyModule,
    ));
    registry.register_cpu(Box::new(
        families::families::mos6502::module::M6502CpuModule,
    ));
    let chunks = build_hierarchy_chunks_from_registry(&registry)
        .map_err(|err| format!("build MOS 6502 native CLI chunks: {err}"))?;
    encode_hierarchy_chunks_from_chunks(&chunks)
        .map_err(|err| format!("encode MOS 6502 native CLI package: {err}"))
}

pub(crate) fn run_opforge_native_cli_failure_cases_from_env(
    workspace_root: &Path,
    cases: &[OpforgeNativeCliFailureCase<'_>],
) -> Result<FsUaeSmokeOutcome, String> {
    if cases.is_empty() {
        return Err("native opForge CLI failure-path mode requires at least one case".to_string());
    }
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let mut runs = Vec::with_capacity(cases.len());
    for case in cases {
        let run = run_example_smoke_with_extra_defines(
            workspace_root,
            &fs_uae_bin,
            &args_text,
            FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
            FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH,
            "68020",
            &[case.define],
        )?;
        match run {
            ExampleSmokeResult::Run(run) => runs.push(run),
            ExampleSmokeResult::Skipped(reason) => return Ok(FsUaeSmokeOutcome::Skipped(reason)),
        }
    }

    Ok(FsUaeSmokeOutcome::Completed { runs })
}

pub(crate) fn run_tkpkg_debug_cli_file_mode_from_env(
    workspace_root: &Path,
    guest_source: &[u8],
    cpu_id: &str,
) -> Result<FsUaeSmokeOutcome, String> {
    run_tkpkg_debug_cli_file_mode_with_optional_package_from_env(
        workspace_root,
        guest_source,
        cpu_id,
        None,
    )
}

pub(crate) fn run_tkpkg_debug_cli_file_mode_with_package_from_env(
    workspace_root: &Path,
    guest_source: &[u8],
    cpu_id: &str,
    package_bytes: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    run_tkpkg_debug_cli_file_mode_with_optional_package_from_env(
        workspace_root,
        guest_source,
        cpu_id,
        Some(package_bytes),
    )
}

fn run_tkpkg_debug_cli_file_mode_with_optional_package_from_env(
    workspace_root: &Path,
    guest_source: &[u8],
    cpu_id: &str,
    package_bytes: Option<&[u8]>,
) -> Result<FsUaeSmokeOutcome, String> {
    run_tkpkg_debug_cli_input_mode_with_optional_package_from_env(
        workspace_root,
        TkpkgDebugCliInputMode::SingleFile(guest_source),
        cpu_id,
        package_bytes,
    )
}

pub(crate) fn run_tkpkg_debug_cli_manifest_mode_with_package_from_env<'a>(
    workspace_root: &Path,
    cases: &'a [TkpkgDebugCliManifestCase<'a>],
    package_bytes: &'a [u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let cpu_id = cases
        .first()
        .map(|case| case.cpu_id)
        .ok_or_else(|| "tkpkg debug-cli manifest mode requires at least one case".to_string())?;
    run_tkpkg_debug_cli_input_mode_with_optional_package_from_env(
        workspace_root,
        TkpkgDebugCliInputMode::Manifest(cases),
        cpu_id,
        Some(package_bytes),
    )
}

fn run_tkpkg_debug_cli_input_mode_with_optional_package_from_env<'a>(
    workspace_root: &Path,
    input_mode: TkpkgDebugCliInputMode<'a>,
    cpu_id: &str,
    package_bytes: Option<&'a [u8]>,
) -> Result<FsUaeSmokeOutcome, String> {
    if std::env::var(FS_UAE_OPT_IN_ENV).is_err() {
        return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "set {FS_UAE_OPT_IN_ENV}=1 to enable the opt-in FS-UAE smoke test"
        )));
    }

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments with {{hunk}}, {{artifact_dir}}, {{example}}, {{ready_file}}, {{stdout_file}}, {{stderr_file}}, and {{exit_code_file}} placeholders as needed"
            )))
        }
    };

    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let spec = GuestInputSmokeSpec {
        example_name: FS_UAE_TKPKG_DEBUG_CLI_EXAMPLE_NAME,
        relative_source_path: FS_UAE_TKPKG_DEBUG_CLI_SOURCE_PATH,
        cpu_override: "68020",
        input_mode,
        pipeline_define: tkpkg_pipeline_define_for_cpu(cpu_id)?,
        package_bytes,
    };
    let run = run_example_smoke_with_guest_input(workspace_root, &fs_uae_bin, &args_text, &spec)?;
    match run {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
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

pub(crate) fn record_last_green_fs_uae_test_run(
    workspace_root: &Path,
    test_name: &str,
    artifact_dir: &Path,
) -> Result<PathBuf, String> {
    let provenance = read_git_head_provenance(workspace_root)?;
    let green_run_unix_seconds = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
        .to_string();
    let record = format_last_green_record(
        test_name,
        artifact_dir,
        provenance.commit.as_str(),
        provenance.commit_unix_seconds.as_str(),
        green_run_unix_seconds.as_str(),
    );

    let target_dir = workspace_root
        .join("target")
        .join(FS_UAE_LAST_GREEN_DIR_NAME);
    fs::create_dir_all(&target_dir).map_err(|err| {
        format!(
            "create last-green directory {}: {err}",
            target_dir.display()
        )
    })?;

    let stable_marker_path = target_dir.join(format!("{test_name}.txt"));
    fs::write(&stable_marker_path, record.as_bytes()).map_err(|err| {
        format!(
            "write stable last-green marker {}: {err}",
            stable_marker_path.display()
        )
    })?;

    let artifact_marker_path = artifact_dir.join(FS_UAE_LAST_GREEN_FILE_NAME);
    fs::write(&artifact_marker_path, record.as_bytes()).map_err(|err| {
        format!(
            "write artifact last-green marker {}: {err}",
            artifact_marker_path.display()
        )
    })?;

    Ok(stable_marker_path)
}

fn format_last_green_record(
    test_name: &str,
    artifact_dir: &Path,
    git_head_commit: &str,
    git_head_commit_unix_seconds: &str,
    green_run_unix_seconds: &str,
) -> String {
    format!(
        "test={test_name}\n\
git_head_commit={git_head_commit}\n\
git_head_commit_unix_seconds={git_head_commit_unix_seconds}\n\
green_run_unix_seconds={green_run_unix_seconds}\n\
artifact_dir={}\n",
        artifact_dir.display()
    )
}

fn read_git_head_provenance(workspace_root: &Path) -> Result<GitHeadProvenance, String> {
    let commit = run_git_stdout(workspace_root, &["rev-parse", "HEAD"])?;
    let commit_unix_seconds =
        run_git_stdout(workspace_root, &["show", "-s", "--format=%ct", "HEAD"])?;
    Ok(GitHeadProvenance {
        commit,
        commit_unix_seconds,
    })
}

fn run_git_stdout(workspace_root: &Path, args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(workspace_root)
        .output()
        .map_err(|err| {
            format!(
                "run git {} in {}: {err}",
                args.join(" "),
                workspace_root.display()
            )
        })?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "git {} failed in {} with status {}: {}",
            args.join(" "),
            workspace_root.display(),
            output.status,
            stderr.trim()
        ));
    }

    let stdout = String::from_utf8(output.stdout)
        .map_err(|err| format!("decode git {} stdout as UTF-8: {err}", args.join(" ")))?;
    Ok(stdout.trim().to_string())
}

fn example_guest_input(example_name: &str) -> Option<(&'static str, &'static [u8])> {
    match example_name {
        "tkpkg_debug_cli" => Some((
            FS_UAE_TKPKG_SMOKE_INPUT_FILE,
            FS_UAE_TKPKG_SMOKE_INPUT_TEXT.as_bytes(),
        )),
        "opforge_cli" => Some((
            FS_UAE_TKPKG_SMOKE_INPUT_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_INPUT_TEXT.as_bytes(),
        )),
        _ => None,
    }
}

fn example_assembly_defines(example_name: &str) -> Vec<String> {
    match example_name {
        "tkpkg_debug_cli" | "opforge_cli" => vec!["OPFORGE_FS_UAE_SMOKE".to_string()],
        _ => Vec::new(),
    }
}

fn example_module_paths(workspace_root: &Path, example_name: &str) -> Vec<PathBuf> {
    if example_name == FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME {
        let amigaos_dir = workspace_root
            .join("native")
            .join("motorola68000")
            .join("amigaos");
        return vec![
            amigaos_dir.join("tkpkg"),
            amigaos_dir.join("tkvm"),
            amigaos_dir.join("prvm"),
            amigaos_dir.join("exprvm"),
            amigaos_dir.join("opcore"),
            amigaos_dir.join("opasm"),
        ];
    }

    if example_name == "tkpkg_debug_cli" {
        let amigaos_dir = workspace_root
            .join("native")
            .join("motorola68000")
            .join("amigaos");
        return vec![
            amigaos_dir.join("tkpkg"),
            amigaos_dir.join("tkvm"),
            amigaos_dir.join("prvm"),
            amigaos_dir.join("exprvm"),
            amigaos_dir.join("opcore"),
            amigaos_dir.join("opasm"),
        ];
    }

    if matches!(example_name, "prvm_smoke" | "prvm_line_iterator_smoke") {
        let amigaos_dir = workspace_root
            .join("native")
            .join("motorola68000")
            .join("amigaos");
        return vec![amigaos_dir.join("prvm")];
    }

    Vec::new()
}

fn example_include_paths(workspace_root: &Path, example_name: &str) -> Vec<PathBuf> {
    if example_name == "tkpkg_debug_cli" {
        let amigaos_dir = workspace_root
            .join("native")
            .join("motorola68000")
            .join("amigaos");
        return vec![amigaos_dir.join("tkpkg"), amigaos_dir.join("tkvm")];
    }

    Vec::new()
}

fn stage_example_guest_inputs(
    workspace_root: &Path,
    example_name: &str,
    mounted_work_dir: &Path,
    _extra_assembly_defines: &[&str],
    native_cli_input_override: Option<&OpforgeNativeCliInputOverride<'_>>,
) -> Result<(), String> {
    let Some((relative_path, bytes)) = example_guest_input(example_name) else {
        return Ok(());
    };

    stage_guest_input_bytes(mounted_work_dir, relative_path, bytes)?;
    if example_name == FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME {
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE,
            native_cli_input_override
                .map(|input| input.source)
                .unwrap_or_else(|| FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_TEXT.as_bytes()),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_INCLUDE_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_INCLUDE_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_A_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_A_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_B_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_B_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_ITEM12_VALUES_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_ITEM12_VALUES_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_MODULE_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_MODULE_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_NESTED_MODULE_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_NESTED_MODULE_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_UNRESOLVED_LABEL_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_UNRESOLVED_LABEL_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_BAD_ORG_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_BAD_ORG_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_UNMATCHED_ENDMODULE_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_UNMATCHED_ENDMODULE_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_UNTERMINATED_MODULE_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_UNTERMINATED_MODULE_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_TEXT.as_bytes(),
        )?;
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_MISSING_MODULE_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_MISSING_MODULE_TEXT.as_bytes(),
        )?;
        let package_bytes = if let Some(input) = native_cli_input_override {
            input.package_bytes.to_vec()
        } else if _extra_assembly_defines.is_empty()
            || _extra_assembly_defines.iter().any(|define| {
                matches!(
                    *define,
                    FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE
                        | FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE
                        | "OPFORGE_FS_UAE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC"
                        | "OPFORGE_FS_UAE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING"
                        | "OPFORGE_FS_UAE_NATIVE_CLI_6502_UNRESOLVED_LABEL"
                        | "OPFORGE_FS_UAE_NATIVE_CLI_6502_BAD_ORG"
                        | "OPFORGE_FS_UAE_NATIVE_CLI_UNSUPPORTED_OUTPUT"
                )
            })
        {
            mos6502_native_cli_single_cpu_package_bytes()?
        } else {
            let package_path = workspace_root.join(FS_UAE_OPFORGE_NATIVE_CLI_PACKAGE_PATH);
            fs::read(&package_path)
                .map_err(|err| format!("read package fixture {}: {err}", package_path.display()))?
        };
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_PACKAGE_GUEST_FILE,
            package_bytes.as_slice(),
        )?;
        let oversized_package = vec![0u8; 32_769];
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_OVERSIZED_PACKAGE_GUEST_FILE,
            &oversized_package,
        )?;
    }
    Ok(())
}

fn stage_guest_input_bytes(
    mounted_work_dir: &Path,
    relative_path: &str,
    bytes: &[u8],
) -> Result<(), String> {
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

fn stage_tkpkg_manifest_inputs(
    mounted_work_dir: &Path,
    cases: &[TkpkgDebugCliManifestCase<'_>],
) -> Result<(), String> {
    if cases.is_empty() {
        return Err("tkpkg debug-cli manifest mode requires at least one case".to_string());
    }

    let mut manifest = String::from("# opforge-tkpkg-manifest-v1\n");
    for (index, case) in cases.iter().enumerate() {
        tkpkg_pipeline_define_for_cpu(case.cpu_id)?;
        let relative_path = tkpkg_manifest_case_relative_path(index, case.name);
        stage_guest_input_bytes(mounted_work_dir, relative_path.as_str(), case.source)?;
        let guest_path = format!("Work:{relative_path}");
        manifest.push_str("# [[case]]\n");
        manifest.push_str("# name = \"");
        manifest.push_str(&case.name.replace(['\r', '\n'], " "));
        manifest.push_str("\"\n");
        manifest.push_str("# cpu = \"");
        manifest.push_str(&case.cpu_id.replace(['\r', '\n', '\t'], " "));
        manifest.push_str("\"\n");
        manifest.push_str(case.cpu_id);
        manifest.push('\t');
        manifest.push_str(guest_path.as_str());
        manifest.push('\n');
    }

    stage_guest_input_bytes(
        mounted_work_dir,
        FS_UAE_TKPKG_MANIFEST_FILE,
        manifest.as_bytes(),
    )
}

fn tkpkg_manifest_case_relative_path(index: usize, name: &str) -> String {
    let sanitized = sanitize_tkpkg_manifest_case_name(name);
    let stem = sanitized.strip_suffix(".asm").unwrap_or(sanitized.as_str());
    format!("{FS_UAE_TKPKG_MANIFEST_INPUT_DIR}/case_{index:04}_{stem}.asm")
}

fn sanitize_tkpkg_manifest_case_name(name: &str) -> String {
    let mut sanitized = String::new();
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || matches!(ch, '.' | '_' | '-') {
            sanitized.push(ch);
        } else {
            sanitized.push('_');
        }
    }
    let trimmed = sanitized.trim_matches('_');
    if trimmed.is_empty() {
        "case".to_string()
    } else {
        trimmed.to_string()
    }
}

fn tkpkg_pipeline_define_for_cpu(cpu_id: &str) -> Result<&'static str, String> {
    match cpu_id {
        "m6502" => Ok("TKPKG_DEBUG_PIPELINE_M6502"),
        "65c02" => Ok("TKPKG_DEBUG_PIPELINE_65C02"),
        "65816" => Ok("TKPKG_DEBUG_PIPELINE_65816"),
        "45gs02" => Ok("TKPKG_DEBUG_PIPELINE_45GS02"),
        "8085" => Ok("TKPKG_DEBUG_PIPELINE_8085"),
        "z80" => Ok("TKPKG_DEBUG_PIPELINE_Z80"),
        "m6809" => Ok("TKPKG_DEBUG_PIPELINE_M6809"),
        "hd6309" => Ok("TKPKG_DEBUG_PIPELINE_HD6309"),
        "m68000" => Ok("TKPKG_DEBUG_PIPELINE_M68000"),
        "m68010" => Ok("TKPKG_DEBUG_PIPELINE_M68010"),
        "m68020" => Ok("TKPKG_DEBUG_PIPELINE_M68020"),
        "m68030" => Ok("TKPKG_DEBUG_PIPELINE_M68030"),
        "m68040" => Ok("TKPKG_DEBUG_PIPELINE_M68040"),
        "m68080" => Ok("TKPKG_DEBUG_PIPELINE_M68080"),
        other => Err(format!("unsupported tkpkg debug-cli CPU id '{other}'")),
    }
}

enum ExampleSmokeResult {
    Run(FsUaeSmokeRun),
    Skipped(String),
}

struct GuestInputSmokeSpec<'a> {
    example_name: &'static str,
    relative_source_path: &'a str,
    cpu_override: &'a str,
    input_mode: TkpkgDebugCliInputMode<'a>,
    pipeline_define: &'a str,
    package_bytes: Option<&'a [u8]>,
}

#[derive(Clone, Copy)]
enum TkpkgDebugCliInputMode<'a> {
    SingleFile(&'a [u8]),
    Manifest(&'a [TkpkgDebugCliManifestCase<'a>]),
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
    Captured,
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

fn resolve_fs_uae_boot_work_dir_from_template() -> Result<Option<PathBuf>, String> {
    let Some(template_path) = std::env::var(FS_UAE_CONFIG_TEMPLATE_ENV)
        .ok()
        .filter(|value| !value.trim().is_empty())
    else {
        return Ok(None);
    };

    let template_path = PathBuf::from(template_path);
    let template_text = fs::read_to_string(&template_path).map_err(|err| {
        format!(
            "read FS-UAE config template {}: {err}",
            template_path.display()
        )
    })?;
    Ok(parse_fs_uae_hard_drive_path(&template_text, 0))
}

fn parse_fs_uae_hard_drive_path(config_text: &str, drive_index: usize) -> Option<PathBuf> {
    let expected_key = format!("hard_drive_{drive_index}");
    for line in config_text.lines() {
        let trimmed = line.trim();
        if !trimmed.starts_with(&expected_key) {
            continue;
        }
        let (_, value) = trimmed.split_once('=')?;
        let path_text = value.trim();
        if path_text.is_empty() {
            continue;
        }
        return Some(PathBuf::from(path_text));
    }
    None
}

fn mirror_smoke_work_payloads(source_root: &Path, target_root: &Path) -> Result<(), String> {
    if source_root == target_root {
        return Ok(());
    }

    fs::create_dir_all(target_root).map_err(|err| {
        format!(
            "create fallback Work directory {}: {err}",
            target_root.display()
        )
    })?;

    let mut pending_dirs = vec![source_root.to_path_buf()];
    while let Some(current_dir) = pending_dirs.pop() {
        for entry in fs::read_dir(&current_dir)
            .map_err(|err| format!("read directory {}: {err}", current_dir.display()))?
        {
            let entry = entry.map_err(|err| {
                format!("read directory entry in {}: {err}", current_dir.display())
            })?;
            let source_path = entry.path();
            let relative = source_path.strip_prefix(source_root).map_err(|err| {
                format!(
                    "strip prefix {} from {}: {err}",
                    source_root.display(),
                    source_path.display()
                )
            })?;
            let target_path = target_root.join(relative);
            let file_type = entry
                .file_type()
                .map_err(|err| format!("query file type for {}: {err}", source_path.display()))?;
            if file_type.is_dir() {
                fs::create_dir_all(&target_path).map_err(|err| {
                    format!("create mirror directory {}: {err}", target_path.display())
                })?;
                pending_dirs.push(source_path);
            } else if file_type.is_file() {
                if let Some(parent) = target_path.parent() {
                    fs::create_dir_all(parent).map_err(|err| {
                        format!("create mirror parent {}: {err}", parent.display())
                    })?;
                }
                fs::copy(&source_path, &target_path).map_err(|err| {
                    format!(
                        "copy smoke payload {} to {}: {err}",
                        source_path.display(),
                        target_path.display()
                    )
                })?;
            }
        }
    }

    Ok(())
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
) -> Result<Option<PathBuf>, String> {
    let Some(template_path) = std::env::var(FS_UAE_CONFIG_TEMPLATE_ENV)
        .ok()
        .filter(|value| !value.trim().is_empty())
    else {
        return Ok(None);
    };

    let template_path = PathBuf::from(template_path);
    let template_text = fs::read_to_string(&template_path).map_err(|err| {
        format!(
            "read FS-UAE config template {}: {err}",
            template_path.display()
        )
    })?;
    let config_text =
        rewrite_fs_uae_config_work_mount(&template_text, &work_mount_dir.to_string_lossy());
    let config_path = artifact_dir.join(FS_UAE_CONFIG_FILE_NAME);
    fs::write(&config_path, config_text).map_err(|err| {
        format!(
            "write generated FS-UAE config {}: {err}",
            config_path.display()
        )
    })?;
    Ok(Some(config_path))
}

fn rewrite_fs_uae_config_work_mount(template_text: &str, work_mount_path: &str) -> String {
    let mut lines = Vec::new();
    let mut replaced_work_mount = false;
    for line in template_text.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("hard_drive_1") {
            lines.push(format!("hard_drive_1 = {work_mount_path}"));
            replaced_work_mount = true;
        } else {
            lines.push(line.to_string());
        }
    }
    if !replaced_work_mount {
        lines.push(format!("hard_drive_1 = {work_mount_path}"));
    }
    let mut rewritten = lines.join("\n");
    rewritten.push('\n');
    rewritten
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

fn determine_smoke_success(guest_exit_code: Option<i32>, launcher_success: bool) -> bool {
    guest_exit_code
        .map(|code| code == 0)
        .unwrap_or(launcher_success)
}

fn fs_uae_launcher_status_text(status: ExitStatus) -> String {
    if let Some(code) = status.code() {
        return format!("FS-UAE launcher exit code: {code}\n");
    }
    format!("FS-UAE launcher exit status: {status}\n")
}

fn wait_for_capture_or_exit(
    child: &mut std::process::Child,
    capture: &FsUaeCaptureConfig,
    example_name: &str,
) -> Result<FsUaeWaitOutcome, String> {
    let deadline = Instant::now() + capture.timeout;
    let mut smoke_started_at = None;
    loop {
        if capture_path_exists(&capture.ready_paths) {
            return Ok(FsUaeWaitOutcome::Captured);
        }

        if smoke_started_at.is_none() && capture_path_exists(&capture.start_paths) {
            smoke_started_at = Some(Instant::now());
        }

        if child
            .try_wait()
            .map_err(|err| format!("poll FS-UAE process for {example_name}: {err}"))?
            .is_some()
        {
            return Ok(FsUaeWaitOutcome::Exited);
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
    run_example_smoke_with_extra_defines_and_native_cli_input(
        workspace_root,
        fs_uae_bin,
        args_text,
        example_name,
        relative_source_path,
        cpu_override,
        extra_assembly_defines,
        None,
    )
}

#[allow(clippy::too_many_arguments)]
fn run_example_smoke_with_extra_defines_and_native_cli_input(
    workspace_root: &Path,
    fs_uae_bin: &str,
    args_text: &str,
    example_name: &'static str,
    relative_source_path: &str,
    cpu_override: &str,
    extra_assembly_defines: &[&str],
    native_cli_input_override: Option<&OpforgeNativeCliInputOverride<'_>>,
) -> Result<ExampleSmokeResult, String> {
    let req = RunExampleSmokeRequest {
        workspace_root,
        fs_uae_bin,
        args_text,
        example_name,
        relative_source_path,
        cpu_override,
        extra_assembly_defines,
        native_cli_input_override,
    };
    run_example_smoke_with_request(&req)
}

struct RunExampleSmokeRequest<'a> {
    workspace_root: &'a Path,
    fs_uae_bin: &'a str,
    args_text: &'a str,
    example_name: &'static str,
    relative_source_path: &'a str,
    cpu_override: &'a str,
    extra_assembly_defines: &'a [&'a str],
    native_cli_input_override: Option<&'a OpforgeNativeCliInputOverride<'a>>,
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
    let native_cli_input_override = req.native_cli_input_override;
    let source_path = workspace_root.join(relative_source_path);
    if !source_path.is_file() {
        return Err(format!(
            "expected FS-UAE smoke example source at {}",
            source_path.display()
        ));
    }

    let artifact_dir =
        create_artifact_dir(workspace_root, &format!("fs-uae-hunk-smoke-{example_name}"))?;
    let mounted_work_dir = artifact_dir.join(FS_UAE_MOUNTED_WORK_DIR_NAME);
    fs::create_dir_all(mounted_work_dir.join("build")).map_err(|err| {
        format!(
            "create mounted Work directory {}: {err}",
            mounted_work_dir.display(),
        )
    })?;
    stage_example_guest_inputs(
        workspace_root,
        example_name,
        &mounted_work_dir,
        extra_assembly_defines,
        native_cli_input_override,
    )?;
    let hunk_name_override = if example_name == "helloworld" {
        Some(format!("build/{example_name}.hunk"))
    } else {
        None
    };
    let mut assembly_defines = example_assembly_defines(example_name);
    assembly_defines.extend(
        extra_assembly_defines
            .iter()
            .map(|define| (*define).to_string()),
    );
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
            "assemble FS-UAE smoke example {} from {}: {}",
            example_name,
            source_path.display(),
            err.summary()
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
    let generated_config_path = maybe_materialize_fs_uae_config(&artifact_dir, &mounted_work_dir)?;

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
    let baseline_process_ids = snapshot_fs_uae_process_ids()?;
    let launcher_stdout_path = artifact_dir.join(FS_UAE_LAUNCHER_STDOUT_FILE);
    let launcher_stderr_path = artifact_dir.join(FS_UAE_LAUNCHER_STDERR_FILE);
    let launcher_stdout = fs::File::create(&launcher_stdout_path)
        .map_err(|err| format!("create {}: {err}", launcher_stdout_path.display()))?;
    let launcher_stderr = fs::File::create(&launcher_stderr_path)
        .map_err(|err| format!("create {}: {err}", launcher_stderr_path.display()))?;

    let mut child = match Command::new(fs_uae_bin)
        .args(&args)
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

    let wait_outcome = match wait_for_capture_or_exit(&mut child, &capture, example_name) {
        Ok(wait_outcome) => wait_outcome,
        Err(err) => {
            let _ = cleanup_spawned_fs_uae_processes(&baseline_process_ids);
            return Err(err);
        }
    };
    if wait_outcome == FsUaeWaitOutcome::Captured {
        let _ = child.kill();
    }
    let launcher_status = child
        .wait()
        .map_err(|err| format!("wait for FS-UAE process for {example_name}: {err}"))?;
    let _ = cleanup_spawned_fs_uae_processes(&baseline_process_ids);

    let guest_exit_code = read_optional_exit_code_from_paths(&capture.exit_code_paths)?;
    let launcher_stdout = read_optional_text(&launcher_stdout_path)?;
    let launcher_stderr = read_optional_text(&launcher_stderr_path)?;
    let launcher_status_text = fs_uae_launcher_status_text(launcher_status);
    let captured_stdout = read_optional_text_from_paths(&capture.stdout_paths)?;
    let captured_stderr = read_optional_text_from_paths(&capture.stderr_paths)?;

    Ok(ExampleSmokeResult::Run(FsUaeSmokeRun {
        example_name,
        source_path,
        artifact_dir,
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
        success: determine_smoke_success(guest_exit_code, launcher_status.success()),
    }))
}

fn run_example_smoke_with_guest_input(
    workspace_root: &Path,
    fs_uae_bin: &str,
    args_text: &str,
    spec: &GuestInputSmokeSpec<'_>,
) -> Result<ExampleSmokeResult, String> {
    let mut source_path = workspace_root.join(spec.relative_source_path);
    if !source_path.is_file() {
        return Err(format!(
            "expected FS-UAE smoke example source at {}",
            source_path.display()
        ));
    }

    let artifact_dir = create_artifact_dir(
        workspace_root,
        &format!("fs-uae-hunk-smoke-{}", spec.example_name),
    )?;
    let mounted_work_dir = artifact_dir.join(FS_UAE_MOUNTED_WORK_DIR_NAME);
    fs::create_dir_all(mounted_work_dir.join("build")).map_err(|err| {
        format!(
            "create mounted Work directory {}: {err}",
            mounted_work_dir.display(),
        )
    })?;
    match spec.input_mode {
        TkpkgDebugCliInputMode::SingleFile(guest_source) => {
            stage_guest_input_bytes(
                &mounted_work_dir,
                FS_UAE_TKPKG_SMOKE_INPUT_FILE,
                guest_source,
            )?;
        }
        TkpkgDebugCliInputMode::Manifest(cases) => {
            stage_tkpkg_manifest_inputs(&mounted_work_dir, cases)?;
        }
    }
    let module_paths = example_module_paths(workspace_root, spec.example_name);
    if let Some(package_bytes) = spec.package_bytes {
        source_path = materialize_tkpkg_debug_cli_package_override_source(
            &source_path,
            &artifact_dir,
            package_bytes,
        )?;
    }

    let mut assembly_defines = example_assembly_defines(spec.example_name);
    if matches!(spec.input_mode, TkpkgDebugCliInputMode::Manifest(_)) {
        assembly_defines.push("OPFORGE_FS_UAE_TKPKG_MANIFEST".to_string());
    }
    assembly_defines.push(spec.pipeline_define.to_string());
    let include_paths = example_include_paths(workspace_root, spec.example_name);
    run_assembly(AssemblyExecutionRequest {
        root_path: &source_path,
        input_base: spec.example_name,
        defines: &assembly_defines,
        include_paths: &include_paths,
        module_paths: &module_paths,
        pp_macro_depth: 64,
        cpu_override: Some(spec.cpu_override),
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
            "assemble FS-UAE smoke example {} from {}: {}",
            spec.example_name,
            source_path.display(),
            err.summary()
        )
    })?;

    let hunk_path = generated_hunk_artifact_path(&artifact_dir, spec.example_name);
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

    let fallback_work_dir =
        resolve_fs_uae_boot_work_dir_from_template()?.map(|root| root.join("Work"));
    if let Some(fallback_work_dir) = fallback_work_dir.as_deref() {
        mirror_smoke_work_payloads(&mounted_work_dir, fallback_work_dir)?;
    }
    let capture = capture_config_from_env(&mounted_work_dir, fallback_work_dir.as_deref())?;
    clear_capture_files(&capture)?;
    let generated_config_path = maybe_materialize_fs_uae_config(&artifact_dir, &mounted_work_dir)?;

    let args = args_text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.replace("{hunk}", &hunk_path.to_string_lossy())
                .replace("{artifact_dir}", &artifact_dir.to_string_lossy())
                .replace("{example}", spec.example_name)
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
    let baseline_process_ids = snapshot_fs_uae_process_ids()?;
    let launcher_stdout_path = artifact_dir.join(FS_UAE_LAUNCHER_STDOUT_FILE);
    let launcher_stderr_path = artifact_dir.join(FS_UAE_LAUNCHER_STDERR_FILE);
    let launcher_stdout = fs::File::create(&launcher_stdout_path)
        .map_err(|err| format!("create {}: {err}", launcher_stdout_path.display()))?;
    let launcher_stderr = fs::File::create(&launcher_stderr_path)
        .map_err(|err| format!("create {}: {err}", launcher_stderr_path.display()))?;

    let mut child = match Command::new(fs_uae_bin)
        .args(&args)
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
                spec.example_name,
                hunk_path.display()
            ))
        }
    };

    let wait_outcome = match wait_for_capture_or_exit(&mut child, &capture, spec.example_name) {
        Ok(wait_outcome) => wait_outcome,
        Err(err) => {
            let _ = cleanup_spawned_fs_uae_processes(&baseline_process_ids);
            return Err(err);
        }
    };
    if wait_outcome == FsUaeWaitOutcome::Captured {
        let _ = child.kill();
    }
    let launcher_status = child
        .wait()
        .map_err(|err| format!("wait for FS-UAE process for {}: {err}", spec.example_name))?;
    let _ = cleanup_spawned_fs_uae_processes(&baseline_process_ids);

    let guest_exit_code = read_optional_exit_code_from_paths(&capture.exit_code_paths)?;
    let launcher_stdout = read_optional_text(&launcher_stdout_path)?;
    let launcher_stderr = read_optional_text(&launcher_stderr_path)?;
    let launcher_status_text = fs_uae_launcher_status_text(launcher_status);
    let captured_stdout = read_optional_text_from_paths(&capture.stdout_paths)?;
    let captured_stderr = read_optional_text_from_paths(&capture.stderr_paths)?;

    Ok(ExampleSmokeResult::Run(FsUaeSmokeRun {
        example_name: spec.example_name,
        source_path,
        artifact_dir,
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
        success: determine_smoke_success(guest_exit_code, launcher_status.success()),
    }))
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

fn materialize_tkpkg_debug_cli_package_override_source(
    source_path: &Path,
    artifact_dir: &Path,
    package_bytes: &[u8],
) -> Result<PathBuf, String> {
    let package_path = artifact_dir.join(FS_UAE_TKPKG_DEBUG_CLI_PACKAGE_OVERRIDE_NAME);
    fs::write(&package_path, package_bytes)
        .map_err(|err| format!("write package override {}: {err}", package_path.display()))?;

    let source = fs::read_to_string(source_path)
        .map_err(|err| format!("read source {}: {err}", source_path.display()))?;
    let default_incbin = format!(".incbin \"{FS_UAE_TKPKG_DEBUG_CLI_PACKAGE_NAME}\"");
    let formatted_relative_incbin =
        format!(".incbin \"../../tkpkg/{FS_UAE_TKPKG_DEBUG_CLI_PACKAGE_NAME}\"");
    let override_incbin = format!(".incbin \"{}\"", package_path.display());
    let overridden = source
        .replace(default_incbin.as_str(), override_incbin.as_str())
        .replace(formatted_relative_incbin.as_str(), override_incbin.as_str());
    if overridden == source {
        return Err(format!(
            "source {} does not contain expected package incbin '{}' or '{}'",
            source_path.display(),
            default_incbin,
            formatted_relative_incbin
        ));
    }

    let override_source_path = artifact_dir.join("tkpkg_debug_cli_package_override.asm");
    fs::write(&override_source_path, overridden).map_err(|err| {
        format!(
            "write package override source {}: {err}",
            override_source_path.display()
        )
    })?;
    Ok(override_source_path)
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert!(determine_smoke_success(None, true));
        assert!(!determine_smoke_success(None, false));
    }

    #[test]
    fn cleanup_only_targets_new_process_ids() {
        let baseline = BTreeSet::from([10_u32, 20_u32]);
        let current = BTreeSet::from([10_u32, 20_u32, 30_u32, 40_u32]);
        let difference = current.difference(&baseline).copied().collect::<Vec<_>>();

        assert_eq!(difference, vec![30_u32, 40_u32]);
    }

    #[test]
    fn rewrite_fs_uae_config_work_mount_replaces_hard_drive_1() {
        let template = "[fs-uae]\nhard_drive_0 = /sys\nhard_drive_1 = /old/work\nsave_disk = 0\n";
        let rewritten = rewrite_fs_uae_config_work_mount(template, "/new/work");

        assert!(rewritten.contains("hard_drive_0 = /sys"));
        assert!(rewritten.contains("hard_drive_1 = /new/work"));
        assert!(!rewritten.contains("hard_drive_1 = /old/work"));
    }

    #[test]
    fn rewrite_fs_uae_config_work_mount_appends_missing_hard_drive_1() {
        let template = "[fs-uae]\nhard_drive_0 = /sys\n";
        let rewritten = rewrite_fs_uae_config_work_mount(template, "/new/work");

        assert!(rewritten.contains("hard_drive_0 = /sys"));
        assert!(rewritten.contains("hard_drive_1 = /new/work"));
    }

    #[test]
    fn example_guest_input_exposes_smoke_source_files() {
        let (relative_path, bytes) =
            example_guest_input("tkpkg_debug_cli").expect("tkpkg smoke input mapping");

        assert_eq!(relative_path, FS_UAE_TKPKG_SMOKE_INPUT_FILE);
        assert_eq!(bytes, FS_UAE_TKPKG_SMOKE_INPUT_TEXT.as_bytes());

        let (relative_path, bytes) =
            example_guest_input("opforge_cli").expect("opforge native CLI smoke input mapping");
        assert_eq!(relative_path, FS_UAE_TKPKG_SMOKE_INPUT_FILE);
        assert_eq!(bytes, FS_UAE_OPFORGE_NATIVE_CLI_INPUT_TEXT.as_bytes());
        assert!(example_guest_input("helloworld").is_none());
    }

    #[test]
    fn example_assembly_defines_enable_fs_uae_mode_for_native_cli_examples() {
        assert_eq!(
            example_assembly_defines("tkpkg_debug_cli"),
            vec!["OPFORGE_FS_UAE_SMOKE".to_string()]
        );
        assert_eq!(
            example_assembly_defines("opforge_cli"),
            vec!["OPFORGE_FS_UAE_SMOKE".to_string()]
        );
        assert!(example_assembly_defines("helloworld").is_empty());
    }

    #[test]
    fn format_last_green_record_captures_commit_timestamp_and_artifact_dir() {
        let record = format_last_green_record(
            "external_fs_uae_opforge_native_cli_6502_writes_rust_matching_bin",
            Path::new("/tmp/opforge-fsuae-smoke"),
            "abc123def456",
            "1717152000",
            "1717152600",
        );

        assert!(record
            .contains("test=external_fs_uae_opforge_native_cli_6502_writes_rust_matching_bin"));
        assert!(record.contains("git_head_commit=abc123def456"));
        assert!(record.contains("git_head_commit_unix_seconds=1717152000"));
        assert!(record.contains("green_run_unix_seconds=1717152600"));
        assert!(record.contains("artifact_dir=/tmp/opforge-fsuae-smoke"));
    }

    #[test]
    fn tkpkg_manifest_case_paths_are_stable_and_guest_relative() {
        assert_eq!(
            tkpkg_manifest_case_relative_path(3, "examples/mos6502/foo bar.asm"),
            "opforge_fsuae_tkpkg_inputs/case_0003_examples_mos6502_foo_bar.asm"
        );
        assert_eq!(
            tkpkg_manifest_case_relative_path(4, ":::"),
            "opforge_fsuae_tkpkg_inputs/case_0004_case.asm"
        );
    }

    #[test]
    fn tkpkg_pipeline_define_for_cpu_maps_supported_ids() {
        for (cpu_id, define) in [
            ("m6502", "TKPKG_DEBUG_PIPELINE_M6502"),
            ("65c02", "TKPKG_DEBUG_PIPELINE_65C02"),
            ("65816", "TKPKG_DEBUG_PIPELINE_65816"),
            ("45gs02", "TKPKG_DEBUG_PIPELINE_45GS02"),
            ("8085", "TKPKG_DEBUG_PIPELINE_8085"),
            ("z80", "TKPKG_DEBUG_PIPELINE_Z80"),
            ("m6809", "TKPKG_DEBUG_PIPELINE_M6809"),
            ("hd6309", "TKPKG_DEBUG_PIPELINE_HD6309"),
            ("m68000", "TKPKG_DEBUG_PIPELINE_M68000"),
            ("m68010", "TKPKG_DEBUG_PIPELINE_M68010"),
            ("m68020", "TKPKG_DEBUG_PIPELINE_M68020"),
            ("m68030", "TKPKG_DEBUG_PIPELINE_M68030"),
            ("m68040", "TKPKG_DEBUG_PIPELINE_M68040"),
            ("m68080", "TKPKG_DEBUG_PIPELINE_M68080"),
        ] {
            assert_eq!(
                tkpkg_pipeline_define_for_cpu(cpu_id).expect("supported CPU id"),
                define
            );
        }

        assert_eq!(
            tkpkg_pipeline_define_for_cpu("bogus").expect_err("unsupported CPU id"),
            "unsupported tkpkg debug-cli CPU id 'bogus'"
        );
    }
}
