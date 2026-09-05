use cli_core::LabelOutputFormat as CliLabelOutputFormat;
use engine::OutputFormat as EngineOutputFormat;
use engine::{default_cpu, run_assembly, AssemblyExecutionRequest, ExecutionMode};
use package::encode_hierarchy_chunks_from_chunks;
use registry::registry::ModuleRegistry;
use serde_json::json;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitStatus;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use types::lockstep::ContinuationHead;
use vm::builder::{build_hierarchy_chunks_from_registry, build_hierarchy_package_from_registry};
use vm::output_model::BinOutputSpec;

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
const FS_UAE_NATIVE_ZORRO_III_MEMORY_KIB: u32 = 65_536;
const FS_UAE_MOUNTED_WORK_DIR_NAME: &str = "Work";
const FS_UAE_MOUNTED_HUNK_ALIAS: &str = "build/opforge_fsuae_smoke.hunk";
const FS_UAE_STARTUP_HUNK_ALIAS: &str = "build/tkpkg_debug_cli.hunk";
const FS_UAE_STARTUP_HUNK_ALIAS_UAEM: &str = "build/tkpkg_debug_cli.hunk.uaem";
const FS_UAE_TKPKG_SMOKE_INPUT_FILE: &str = "opforge_fsuae_smoke_input.asm";
const FS_UAE_TKPKG_OPERAND_RECORD_BATCH_FILE: &str = "opforge_fsuae_operand_records.bin";
const FS_UAE_TKPKG_SMOKE_INPUT_TEXT: &str = "move.b d0,d1\nmove.w d2,d3\n";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_INPUT_TEXT: &str =
    ".module main\n.use math\n.use math as m\n.endmodule\n";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT: &str =
    ".org $0800\nstart   lda #$42\n.byte $99\n.word $1234, $5678\n.long $01020304\n.text \"OK\"\n.null \"A\"\n.ptext \"BC\"\n";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM5_DIRECTIVE_ROUTER";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_FLOW_NAVIGATION_INPUT_TEXT: &str =
    ".org $0800\n.if 0\n.byte $11\n.else\n.byte $42\n.endif\n";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_FLOW_NAVIGATION_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM5_FLOW_NAVIGATION";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_6502_OUTPUT";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_65C02_OUTPUT";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_MACRO_DEBUG_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_MACRO_DEBUG";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM10_INCLUDE_OUTPUT";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_MISSING_INCLUDE_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_MISSING_INCLUDE";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_ITEM13_OUTPUT_DIRECTIVE_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM13_OUTPUT_DIRECTIVE";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_ITEM14_OUTPUT_DIRECTIVE_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM14_OUTPUT_DIRECTIVE";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_ITEM15_OUTPUT_DIRECTIVE_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM15_OUTPUT_DIRECTIVE";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_ITEM16_LIST_OUTPUT_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM16_LIST_OUTPUT";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_ARTIFACT_MATRIX_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM17_ARTIFACT_MATRIX";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_SOURCE_CPU_ONLY_DEFINE: &str =
    "OPFORGE_FS_UAE_NATIVE_CLI_ITEM17_SOURCE_CPU_ONLY";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE: &str = "opforge_6502_native_cli_smoke.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_TEXT: &str =
    ".org $0800\nstart   lda #$42\n        sta $20\n        lda $20,x\n        sta $0200\n        lda $0200,x\n        lda $0200,y\ndone    jmp done\n";
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
const FS_UAE_OPFORGE_NATIVE_CLI_6502_UNRESOLVED_LABEL_TEXT: &str = "start   lda #missing\n";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_BAD_ORG_FILE: &str = "opforge_6502_bad_org.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_6502_BAD_ORG_TEXT: &str =
    "        .org missing\n        lda #$42\n";
const FS_UAE_OPFORGE_NATIVE_CLI_MODULE_FILE: &str = "math.asm";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_MODULE_TEXT: &str =
    ".module math\n.use helper\nfoo     sta $0200\n.endmodule\n";
const FS_UAE_OPFORGE_NATIVE_CLI_NESTED_MODULE_FILE: &str = "helper.asm";
pub(crate) const FS_UAE_OPFORGE_NATIVE_CLI_NESTED_MODULE_TEXT: &str =
    ".module helper\n.endmodule\n";
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
const FS_UAE_OPFORGE_NATIVE_CLI_UNTERMINATED_MODULE_TEXT: &str = ".module main\n";
const FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_FILE: &str = "opforge_fsuae_bad_use.asm";
const FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_TEXT: &str = ".module target\n.cpu 65c02\n.endmodule\n.module main\n.cpu 65c02\n.use target map { code -> app_code data -> app_data }\n.endmodule\n";
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
const FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME: &str = "debug_contract_harness";
const FS_UAE_DEBUG_CONTRACT_SOURCE_PATH: &str =
    "native/motorola68000/amigaos/test-harnesses/debug/debug_contract_harness.asm";
const FS_UAE_PROGRESS_HARNESS_NAME: &str = "opasm_progress_harness";
const FS_UAE_PROGRESS_HARNESS_SOURCE_PATH: &str =
    "native/motorola68000/amigaos/test-harnesses/debug/opasm_progress_harness.asm";
const FS_UAE_CLI_DEBUG_EVENT_EXAMPLE_NAME: &str = "cli_debug_event_harness";
const FS_UAE_CLI_DEBUG_EVENT_SOURCE_PATH: &str =
    "native/motorola68000/amigaos/test-harnesses/debug/cli_debug_event_harness.asm";
const FS_UAE_MACRO_CLI_DEBUG_EVENT_HARNESS_NAME: &str = "macro_cli_debug_event_harness";
const FS_UAE_MACRO_CLI_DEBUG_EVENT_HARNESS_SOURCE_PATH: &str =
    "native/motorola68000/amigaos/test-harnesses/debug/macro_cli_debug_event_harness.asm";
const FS_UAE_MACRO_PREPROCESSOR_HARNESS_NAME: &str = "macro_preprocessor_harness";
const FS_UAE_MACRO_PREPROCESSOR_HARNESS_SOURCE_PATH: &str =
    "native/motorola68000/amigaos/test-harnesses/debug/macro_preprocessor_harness.asm";
const FS_UAE_PIPELINE_SELECT_HARNESS_NAME: &str = "pipeline_select_harness";
const FS_UAE_PIPELINE_SELECT_HARNESS_SOURCE_PATH: &str =
    "native/motorola68000/amigaos/test-harnesses/debug/pipeline_select_harness.asm";
// Keep explicit-package parity commands below the classic AmigaShell command
// tail limit. The filename is runner-private and every byte is still supplied
// by the authoritative case package.
const FS_UAE_OPFORGE_NATIVE_CLI_PACKAGE_GUEST_FILE: &str = "p.opasm";
const FS_UAE_OPFORGE_NATIVE_CLI_OVERSIZED_PACKAGE_GUEST_FILE: &str =
    "opforge_cli_package_oversized.opasm";
const FS_UAE_OPFORGE_NATIVE_CLI_OVERSIZED_PACKAGE_BYTES: usize = 393_217;
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_ARTIFACTS_DIR: &str = "case_artifacts";
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDOUT_FILE: &str = "opforge_fsuae_smoke.stdout";
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDERR_FILE: &str = "opforge_fsuae_smoke.stderr";
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_EXITCODE_FILE: &str = "opforge_fsuae_smoke.exitcode";
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_STARTED_FILE: &str = "opforge_fsuae_smoke.started";
const FS_UAE_OPFORGE_NATIVE_CLI_CASE_DONE_FILE: &str = "opforge_fsuae_smoke.done";
const AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX: usize = 30;
const OPFORGE_SELF_HOST_PRODUCT_DIR: &str = "native/motorola68000/amigaos";
const OPFORGE_SELF_HOST_SOURCE_DIRS: &[&str] = &[
    "opforge-cli",
    "tkpkg",
    "tkvm",
    "prvm",
    "exprvm",
    "opcore",
    "opasm",
    "debug",
];
const OPFORGE_SELF_HOST_SHORT_COMPONENT_MAP: &[(&str, &str)] = &[
    (
        "opforge_symbol_expr_profile.asm",
        "opforge_symexpr_profile.asm",
    ),
    (
        "tkpkg_engine_context_adapter.asm",
        "tkpkg_engine_ctx_adapter.asm",
    ),
    (
        "tkpkg_operand_record_service.asm",
        "tkpkg_operand_rec_service.asm",
    ),
];
const FS_UAE_SCRIPT_UAEM_TEXT: &str = "-s--rw-d 2021-04-13 02:43:19.40 \n";
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

pub(crate) struct TkpkgDebugCliManifestCase<'a> {
    pub(crate) name: &'a str,
    pub(crate) cpu_id: &'a str,
    pub(crate) source: &'a [u8],
}

pub(crate) struct TkpkgDebugCliOperandRecordParityCase<'a> {
    pub(crate) name: &'a str,
    pub(crate) batch: &'a [u8],
    pub(crate) package_bytes: &'a [u8],
    pub(crate) proof: TkpkgDebugCliOperandRecordProof<'a>,
}

#[derive(Clone, Copy)]
pub(crate) enum TkpkgDebugCliOperandRecordProof<'a> {
    ExactRows(&'a [u8]),
    ExpectedFailureContaining(&'a str),
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
    pub(crate) proof: OpforgeNativeCliMosProof<'a>,
}

pub(crate) enum OpforgeNativeCliMosProof<'a> {
    ExactRustBytes(&'a [u8]),
    ExpectedFailureWithDiagnostic,
    ExpectedFailureContaining(&'a str),
}

#[derive(Clone, Copy)]
#[allow(dead_code)]
pub(crate) enum OpforgeNativeCliPackageMode<'a> {
    EmbeddedDefault,
    Explicit(&'a [u8]),
    Mos6502FocusedPair,
    M68020SinglePipeline,
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
    pub(crate) package_mode: OpforgeNativeCliPackageMode<'a>,
    pub(crate) extra_guest_files: &'a [OpforgeNativeCliGuestFile<'a>],
    pub(crate) proof: OpforgeNativeCliProof<'a>,
}

#[derive(Clone, Copy)]
pub(crate) struct OpforgeNativeCliExpectedArtifact<'a> {
    pub(crate) relative_path: &'a str,
    pub(crate) rust_oracle: &'a [u8],
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct OpforgeSelfHostPathRendering<'a> {
    pub(crate) executable: &'a str,
    pub(crate) product_dir: &'a str,
    pub(crate) listing: &'a str,
    pub(crate) srec: &'a str,
}

#[derive(Debug, Clone)]
pub(crate) struct OpforgeSelfHostTreeFile {
    pub(crate) logical_relative_path: PathBuf,
    pub(crate) staged_relative_path: PathBuf,
    pub(crate) bytes: Vec<u8>,
}

pub(crate) struct OpforgeNativeSelfHostGenerationOneCase<'a> {
    pub(crate) name: &'a str,
    pub(crate) amiga_command: &'a [String],
    pub(crate) amiga_paths: OpforgeSelfHostPathRendering<'a>,
    pub(crate) guest_files: &'a [OpforgeNativeCliGuestFile<'a>],
    pub(crate) expected_artifacts: &'a [OpforgeNativeCliExpectedArtifact<'a>],
}

pub(crate) fn render_opforge_self_host_command(
    paths: OpforgeSelfHostPathRendering<'_>,
) -> Result<Vec<String>, String> {
    let executable_name = paths
        .executable
        .rsplit([':', '/'])
        .next()
        .unwrap_or_default();
    if executable_name != "opforge" {
        return Err(format!(
            "self-host executable must retain basename 'opforge', got '{}'",
            paths.executable
        ));
    }
    for (role, value) in [
        ("product directory", paths.product_dir),
        ("listing", paths.listing),
        ("S-record", paths.srec),
    ] {
        if value.is_empty() {
            return Err(format!("self-host {role} path must not be empty"));
        }
    }
    Ok(vec![
        paths.executable.to_string(),
        paths.product_dir.to_string(),
        "-l".to_string(),
        paths.listing.to_string(),
        "-s".to_string(),
        paths.srec.to_string(),
    ])
}

pub(crate) fn verify_opforge_self_host_command_rendering(
    actual: &[String],
    paths: OpforgeSelfHostPathRendering<'_>,
) -> Result<(), String> {
    let expected = render_opforge_self_host_command(paths)?;
    if actual.len() != expected.len() {
        return Err(format!(
            "self-host command has {} tokens; canonical command has {}",
            actual.len(),
            expected.len()
        ));
    }
    for (index, (actual, expected)) in actual.iter().zip(expected.iter()).enumerate() {
        if actual != expected {
            return Err(format!(
                "self-host command token {index} differs: actual='{actual}', expected='{expected}'"
            ));
        }
    }
    Ok(())
}

pub(crate) fn verify_opforge_self_host_same_logical_command(
    unix_command: &[String],
    unix_paths: OpforgeSelfHostPathRendering<'_>,
    amiga_command: &[String],
    amiga_paths: OpforgeSelfHostPathRendering<'_>,
) -> Result<(), String> {
    verify_opforge_self_host_command_rendering(unix_command, unix_paths)
        .map_err(|error| format!("Unix rendering: {error}"))?;
    verify_opforge_self_host_command_rendering(amiga_command, amiga_paths)
        .map_err(|error| format!("AmigaDOS rendering: {error}"))?;
    Ok(())
}

fn opforge_self_host_staged_relative_path(logical: &Path) -> Result<PathBuf, String> {
    let mut staged = PathBuf::new();
    for component in logical.components() {
        let std::path::Component::Normal(component) = component else {
            return Err(format!(
                "self-host staging path must be relative without traversal: {}",
                logical.display()
            ));
        };
        let component = component
            .to_str()
            .ok_or_else(|| format!("self-host staging path is not UTF-8: {}", logical.display()))?;
        let staged_component = OPFORGE_SELF_HOST_SHORT_COMPONENT_MAP
            .iter()
            .find_map(|(source, target)| (*source == component).then_some(*target))
            .unwrap_or(component);
        if staged_component.len() > AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX {
            return Err(format!(
                "self-host staging component '{}' is {} bytes and has no reviewed <= {} byte mapping",
                component,
                component.len(),
                AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX
            ));
        }
        staged.push(staged_component);
    }
    if staged.as_os_str().is_empty() {
        return Err("self-host staging path must not be empty".to_string());
    }
    Ok(staged)
}

pub(crate) fn collect_opforge_self_host_product_tree(
    workspace_root: &Path,
) -> Result<Vec<OpforgeSelfHostTreeFile>, String> {
    let product_dir = workspace_root.join(OPFORGE_SELF_HOST_PRODUCT_DIR);
    let mut logical_paths = vec![PathBuf::from(OPFORGE_SELF_HOST_PRODUCT_DIR).join("main.asm")];
    for source_dir in OPFORGE_SELF_HOST_SOURCE_DIRS {
        let absolute_dir = product_dir.join(source_dir);
        let mut entries = fs::read_dir(&absolute_dir)
            .map_err(|error| {
                format!(
                    "read self-host source directory {}: {error}",
                    absolute_dir.display()
                )
            })?
            .collect::<Result<Vec<_>, _>>()
            .map_err(|error| {
                format!(
                    "collect self-host source directory {}: {error}",
                    absolute_dir.display()
                )
            })?;
        entries.sort_by_key(|entry| entry.file_name());
        for entry in entries {
            let path = entry.path();
            if !path.is_file() {
                continue;
            }
            let extension = path.extension().and_then(|value| value.to_str());
            if !matches!(extension, Some("asm" | "i")) {
                continue;
            }
            logical_paths.push(
                path.strip_prefix(workspace_root)
                    .map_err(|error| {
                        format!(
                            "make self-host source {} relative to {}: {error}",
                            path.display(),
                            workspace_root.display()
                        )
                    })?
                    .to_path_buf(),
            );
        }
    }
    logical_paths.push(
        PathBuf::from(OPFORGE_SELF_HOST_PRODUCT_DIR).join("opforge-cli/opforge_cli_package.opasm"),
    );
    logical_paths.sort();
    logical_paths.dedup();

    let mut staged_paths = BTreeSet::new();
    let mut files = Vec::with_capacity(logical_paths.len());
    for logical_relative_path in logical_paths {
        let staged_relative_path = opforge_self_host_staged_relative_path(&logical_relative_path)?;
        if !staged_paths.insert(staged_relative_path.clone()) {
            return Err(format!(
                "self-host short-name mapping collides at {}",
                staged_relative_path.display()
            ));
        }
        let absolute_path = workspace_root.join(&logical_relative_path);
        let bytes = fs::read(&absolute_path).map_err(|error| {
            format!(
                "read self-host product input {}: {error}",
                absolute_path.display()
            )
        })?;
        files.push(OpforgeSelfHostTreeFile {
            logical_relative_path,
            staged_relative_path,
            bytes,
        });
    }
    Ok(files)
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
    ExpectedFailureWithDiagnostic,
    ExpectedFailureContaining(&'a str),
}

#[derive(Clone, Copy)]
enum NativeCliParityExecutable {
    OpforgeCli,
    OpforgeSelfHostGenerationOne,
    TkpkgDebugCliOperandRecord,
}

struct OpforgeNativeCliStagedInputs<'a> {
    source: Option<&'a [u8]>,
    package_bytes: Option<&'a [u8]>,
    extra_guest_files: &'a [OpforgeNativeCliGuestFile<'a>],
}

pub(crate) fn run_hunk_smoke_from_env(workspace_root: &Path) -> Result<FsUaeSmokeOutcome, String> {
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
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

pub(crate) fn run_native_progress_harness_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    run_native_progress_harness_with_platform_from_env(workspace_root, false, &[])
}

pub(crate) fn run_native_platform_profile_harness_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    let mut runs = Vec::new();
    for disabled in [
        &[][..],
        &["OPFORGE_PROGRESS_PLATFORM_NO_IO"][..],
        &["OPFORGE_PROGRESS_PLATFORM_NO_BULK"][..],
        &[
            "OPFORGE_PROGRESS_PLATFORM_NO_IO",
            "OPFORGE_PROGRESS_PLATFORM_NO_BULK",
        ][..],
    ] {
        match run_native_progress_harness_with_platform_from_env(workspace_root, true, disabled)? {
            FsUaeSmokeOutcome::Skipped(reason) => return Ok(FsUaeSmokeOutcome::Skipped(reason)),
            FsUaeSmokeOutcome::Completed {
                runs: mut mode_runs,
            } => runs.append(&mut mode_runs),
        }
    }
    Ok(FsUaeSmokeOutcome::Completed { runs })
}

fn run_native_progress_harness_with_platform_from_env(
    workspace_root: &Path,
    platform: bool,
    disabled_groups: &[&str],
) -> Result<FsUaeSmokeOutcome, String> {
    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "{FS_UAE_ARGS_ENV} is not set; configure FS-UAE to execute the native progress harness"
        )))
        }
    };
    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let mut defines = vec![
        "OPFORGE_DEBUG_CONTRACTS",
        "OPFORGE_PROGRESS_WORK_COUNTERS",
        "OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS",
        "OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL",
        "OPFORGE_PROGRESS_RUNTIME_COUNTERS",
    ];
    if platform {
        defines.push("OPFORGE_PROGRESS_PLATFORM_COUNTERS");
    }
    defines.extend_from_slice(disabled_groups);
    match run_example_smoke_with_extra_defines(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_PROGRESS_HARNESS_NAME,
        FS_UAE_PROGRESS_HARNESS_SOURCE_PATH,
        "68020",
        &defines,
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
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
    let config_path = maybe_materialize_fs_uae_config(&artifact_dir, &mounted_work_dir)?
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

pub(crate) fn run_native_cli_debug_event_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; configure FS-UAE to execute the native CLI debug-event harness"
            )))
        }
    };
    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    match run_example_smoke_with_extra_defines(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_CLI_DEBUG_EVENT_EXAMPLE_NAME,
        FS_UAE_CLI_DEBUG_EVENT_SOURCE_PATH,
        "68020",
        &[
            "OPFORGE_FS_UAE_SMOKE",
            "OPFORGE_FS_UAE_NATIVE_CLI_DEBUG_EVENT",
            "OPFORGE_DEBUG_CONTRACTS",
        ],
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

pub(crate) fn run_native_cli_directive_router_from_env(
    workspace_root: &Path,
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let cases = [OpforgeNativeCliParityCase {
        name: "native-cli-directive-router",
        cpu_override: "68020",
        extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_DEFINE],
        source_override: Some(FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT.as_bytes()),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle,
        },
    }];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

pub(crate) fn run_native_progress_cli_parity_from_env(
    workspace_root: &Path,
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    run_native_progress_cli_parity_with_platform_from_env(workspace_root, rust_oracle, false)
}

pub(crate) fn run_native_platform_profile_cli_parity_from_env(
    workspace_root: &Path,
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    run_native_progress_cli_parity_with_platform_from_env(workspace_root, rust_oracle, true)
}

fn run_native_progress_cli_parity_with_platform_from_env(
    workspace_root: &Path,
    rust_oracle: &[u8],
    platform: bool,
) -> Result<FsUaeSmokeOutcome, String> {
    let mut defines = vec![
        FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_DEFINE,
        "OPFORGE_DEBUG_CONTRACTS",
        "OPFORGE_PROGRESS_WORK_COUNTERS",
        "OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS",
        "OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL",
        "OPFORGE_PROGRESS_RUNTIME_COUNTERS",
    ];
    if platform {
        defines.push("OPFORGE_PROGRESS_PLATFORM_COUNTERS");
        defines.push("OPFORGE_PROGRESS_EXPORT_RECORDS");
        match std::env::var("OPFORGE_NATIVE_PROFILE_PLATFORM_MODE")
            .as_deref()
            .unwrap_or("all")
        {
            "all" => {}
            "io" => defines.push("OPFORGE_PROGRESS_PLATFORM_NO_BULK"),
            "bulk" => defines.push("OPFORGE_PROGRESS_PLATFORM_NO_IO"),
            "neither" => {
                defines.push("OPFORGE_PROGRESS_PLATFORM_NO_IO");
                defines.push("OPFORGE_PROGRESS_PLATFORM_NO_BULK");
            }
            mode => {
                return Err(format!(
                    "unknown OPFORGE_NATIVE_PROFILE_PLATFORM_MODE: {mode}"
                ))
            }
        }
    }
    let cases = [OpforgeNativeCliParityCase {
        name: "native-progress-cli-parity",
        cpu_override: "68020",
        extra_assembly_defines: &defines,
        source_override: Some(FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT.as_bytes()),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle,
        },
    }];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

pub(crate) fn run_native_progress_only_cli_parity_from_env(
    workspace_root: &Path,
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let cases = [OpforgeNativeCliParityCase {
        name: "native-progress-only-cli-parity",
        cpu_override: "68020",
        extra_assembly_defines: &[
            FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_DEFINE,
            "OPFORGE_DEBUG_CONTRACTS",
        ],
        source_override: Some(FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT.as_bytes()),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle,
        },
    }];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

pub(crate) fn run_native_cli_flow_navigation_from_env(
    workspace_root: &Path,
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let cases = [OpforgeNativeCliParityCase {
        name: "native-cli-flow-navigation",
        cpu_override: "68020",
        extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_FLOW_NAVIGATION_DEFINE],
        source_override: Some(FS_UAE_OPFORGE_NATIVE_CLI_FLOW_NAVIGATION_INPUT_TEXT.as_bytes()),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle,
        },
    }];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

// @opforge-evidence: level=D; role=focused-negative; authority=focused-contract; lifecycle=permanent
pub(crate) fn run_native_macro_preprocessor_harness_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "{FS_UAE_ARGS_ENV} is not set; configure FS-UAE to execute the native macro-preprocessor harness"
        ))),
    };
    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    match run_example_smoke(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_MACRO_PREPROCESSOR_HARNESS_NAME,
        FS_UAE_MACRO_PREPROCESSOR_HARNESS_SOURCE_PATH,
        "68020",
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

// @opforge-evidence: level=D; role=focused-contract; authority=focused-contract; lifecycle=permanent
pub(crate) fn run_native_pipeline_select_harness_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "{FS_UAE_ARGS_ENV} is not set; configure FS-UAE to execute the native pipeline-selection harness"
        ))),
    };
    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    match run_example_smoke_with_extra_defines(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_PIPELINE_SELECT_HARNESS_NAME,
        FS_UAE_PIPELINE_SELECT_HARNESS_SOURCE_PATH,
        "68020",
        &["OPFORGE_DEBUG_CONTRACTS"],
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

// @opforge-evidence: level=E; role=diagnostic; authority=none; lifecycle=permanent
pub(crate) fn run_native_macro_cli_debug_event_harness_from_env(
    workspace_root: &Path,
) -> Result<FsUaeSmokeOutcome, String> {
    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => return Ok(FsUaeSmokeOutcome::Skipped(format!(
            "{FS_UAE_ARGS_ENV} is not set; configure FS-UAE to execute the native macro CLI debug-event harness"
        ))),
    };
    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    match run_example_smoke_with_extra_defines(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        FS_UAE_MACRO_CLI_DEBUG_EVENT_HARNESS_NAME,
        FS_UAE_MACRO_CLI_DEBUG_EVENT_HARNESS_SOURCE_PATH,
        "68020",
        &[
            "OPFORGE_FS_UAE_SMOKE",
            FS_UAE_OPFORGE_NATIVE_CLI_MACRO_DEBUG_DEFINE,
        ],
    )? {
        ExampleSmokeResult::Run(run) => Ok(FsUaeSmokeOutcome::Completed { runs: vec![run] }),
        ExampleSmokeResult::Skipped(reason) => Ok(FsUaeSmokeOutcome::Skipped(reason)),
    }
}

pub(crate) fn run_opforge_native_cli_6502_output_from_env(
    workspace_root: &Path,
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let cases = [OpforgeNativeCliParityCase {
        name: "native-cli-6502-output",
        cpu_override: "68020",
        extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE],
        source_override: None,
        command_template: None,
        package_mode: OpforgeNativeCliPackageMode::Mos6502FocusedPair,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle,
        },
    }];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

pub(crate) fn run_opforge_native_cli_parity_cases_from_env(
    workspace_root: &Path,
    cases: &[OpforgeNativeCliParityCase<'_>],
) -> Result<FsUaeSmokeOutcome, String> {
    if cases.is_empty() {
        return Err("native opForge CLI parity mode requires at least one case".to_string());
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
    run_native_cli_parity_batch_cases(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        cases,
        NativeCliParityExecutable::OpforgeCli,
        None,
    )
}

pub(crate) fn run_opforge_native_self_host_generation_one_from_env(
    workspace_root: &Path,
    case: &OpforgeNativeSelfHostGenerationOneCase<'_>,
) -> Result<FsUaeSmokeOutcome, String> {
    run_opforge_native_self_host_generation_from_env(workspace_root, case, None)
}

pub(crate) fn run_opforge_native_self_host_generation_from_env(
    workspace_root: &Path,
    case: &OpforgeNativeSelfHostGenerationOneCase<'_>,
    bootstrap_executable: Option<&[u8]>,
) -> Result<FsUaeSmokeOutcome, String> {
    verify_opforge_self_host_command_rendering(case.amiga_command, case.amiga_paths)?;
    if bootstrap_executable.is_some_and(|bytes| bytes.is_empty()) {
        return Err("self-host bootstrap executable override must not be empty".to_string());
    }
    let command_tail = case.amiga_command[1..].join(" ");
    let parity_case = OpforgeNativeCliParityCase {
        name: case.name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(b""),
        command_template: Some(command_tail.as_str()),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: case.guest_files,
        proof: OpforgeNativeCliProof::ExactArtifacts(case.expected_artifacts),
    };

    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments"
            )))
        }
    };
    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    run_native_cli_parity_batch_cases(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        std::slice::from_ref(&parity_case),
        NativeCliParityExecutable::OpforgeSelfHostGenerationOne,
        bootstrap_executable,
    )
}

pub(crate) fn run_tkpkg_debug_cli_operand_record_parity_cases_from_env(
    workspace_root: &Path,
    cases: &[TkpkgDebugCliOperandRecordParityCase<'_>],
) -> Result<FsUaeSmokeOutcome, String> {
    const DEFINES: &[&str] = &[
        "OPFORGE_FS_UAE_SMOKE",
        "OPFORGE_FS_UAE_TKPKG_OPERAND_RECORD",
    ];
    if cases.is_empty() {
        return Err(
            "native tkpkg operand-record parity mode requires at least one case".to_string(),
        );
    }
    let args_text = match std::env::var(FS_UAE_ARGS_ENV) {
        Ok(value) if !value.trim().is_empty() => value,
        _ => {
            return Ok(FsUaeSmokeOutcome::Skipped(format!(
                "{FS_UAE_ARGS_ENV} is not set; provide newline-delimited FS-UAE arguments"
            )))
        }
    };
    let fs_uae_bin = std::env::var(FS_UAE_BIN_ENV).unwrap_or_else(|_| "fs-uae".to_string());
    let parity_cases = cases
        .iter()
        .map(|case| OpforgeNativeCliParityCase {
            name: case.name,
            cpu_override: "68020",
            extra_assembly_defines: DEFINES,
            source_override: Some(case.batch),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::Explicit(case.package_bytes),
            extra_guest_files: &[],
            proof: match case.proof {
                TkpkgDebugCliOperandRecordProof::ExactRows(rust_oracle) => {
                    OpforgeNativeCliProof::ExactStdoutLines {
                        prefix: "TKPKG OPRD ",
                        rust_oracle,
                    }
                }
                TkpkgDebugCliOperandRecordProof::ExpectedFailureContaining(diagnostic) => {
                    OpforgeNativeCliProof::ExpectedFailureContaining(diagnostic)
                }
            },
        })
        .collect::<Vec<_>>();
    run_native_cli_parity_batch_cases(
        workspace_root,
        &fs_uae_bin,
        &args_text,
        &parity_cases,
        NativeCliParityExecutable::TkpkgDebugCliOperandRecord,
        None,
    )
}

fn native_cli_output_define_for_cpu(cpu_id: &str, case_name: &str) -> Result<&'static str, String> {
    match cpu_id {
        "m6502" => Ok(FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE),
        "65c02" => Ok(FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE),
        other => Err(format!(
            "unsupported native opForge CLI MOS fixture CPU id '{other}' for {case_name}"
        )),
    }
}

pub(crate) fn run_opforge_native_cli_mos_fixture_outputs_from_env(
    workspace_root: &Path,
    cases: &[OpforgeNativeCliMosFixtureCase<'_>],
) -> Result<FsUaeSmokeOutcome, String> {
    if cases.is_empty() {
        return Err("native opForge CLI MOS proof requires at least one case".to_string());
    }

    let mut verified_runs = Vec::with_capacity(cases.len());
    let mut proof_errors = Vec::new();
    for case in cases {
        let defines = [native_cli_output_define_for_cpu(case.cpu_id, case.name)?];
        let parity_case = OpforgeNativeCliParityCase {
            name: case.name,
            cpu_override: "68020",
            extra_assembly_defines: defines.as_slice(),
            source_override: Some(case.source),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::Explicit(case.package_bytes),
            extra_guest_files: &[],
            proof: match case.proof {
                OpforgeNativeCliMosProof::ExactRustBytes(rust_oracle) => {
                    OpforgeNativeCliProof::ExactArtifact {
                        relative_path: "Work/opforge_native_out.bin",
                        rust_oracle,
                    }
                }
                OpforgeNativeCliMosProof::ExpectedFailureWithDiagnostic => {
                    OpforgeNativeCliProof::ExpectedFailureWithDiagnostic
                }
                OpforgeNativeCliMosProof::ExpectedFailureContaining(diagnostic) => {
                    OpforgeNativeCliProof::ExpectedFailureContaining(diagnostic)
                }
            },
        };
        let outcome = match run_opforge_native_cli_parity_cases_from_env(
            workspace_root,
            std::slice::from_ref(&parity_case),
        ) {
            Ok(outcome) => outcome,
            Err(error) => {
                proof_errors.push(format!("{}: {error}", case.name));
                continue;
            }
        };
        let mut runs = match outcome {
            FsUaeSmokeOutcome::Skipped(reason) => return Ok(FsUaeSmokeOutcome::Skipped(reason)),
            FsUaeSmokeOutcome::Completed { runs } => runs,
        };
        if runs.len() != 1 {
            return Err(format!(
                "FS-UAE MOS proof for {} returned {} runs instead of exactly one",
                case.name,
                runs.len()
            ));
        }
        verified_runs.push(runs.remove(0));
    }

    if !proof_errors.is_empty() {
        return Err(format!(
            "{} of {} MOS FS-UAE cases failed their proof contract after every case was attempted:\n{}",
            proof_errors.len(),
            cases.len(),
            proof_errors.join("\n")
        ));
    }

    Ok(FsUaeSmokeOutcome::Completed {
        runs: verified_runs,
    })
}

fn verify_native_cli_case_proof(
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
            errors.push(format!(
                "{}: native output ({} bytes, {:02x?}) differs from the in-memory Rust oracle ({} bytes); {}",
                relative_output_path.display(),
                actual.len(),
                actual,
                artifact.rust_oracle.len(),
                describe_first_byte_mismatch(&actual, artifact.rust_oracle)
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

pub(crate) fn run_opforge_native_cli_item10_include_from_env(
    workspace_root: &Path,
    _package_bytes: &[u8],
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let include_source = "        .include \"defs.inc\"\n        lda #$44\n";
    let missing_include_source = "        .include \"missing.inc\"\n        lda #$44\n";
    let include_support = [
        OpforgeNativeCliGuestFile {
            relative_path: FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_A_FILE,
            bytes: FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_A_TEXT.as_bytes(),
        },
        OpforgeNativeCliGuestFile {
            relative_path: FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_B_FILE,
            bytes: FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_B_TEXT.as_bytes(),
        },
    ];
    let missing_include_support = [OpforgeNativeCliGuestFile {
        relative_path: FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_A_FILE,
        bytes: FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_A_TEXT.as_bytes(),
    }];
    let cases = [
        OpforgeNativeCliParityCase {
            name: "item10-include-success",
            cpu_override: "68020",
            extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE],
            source_override: Some(include_source.as_bytes()),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &include_support,
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle,
            },
        },
        OpforgeNativeCliParityCase {
            name: "item10-missing-include",
            cpu_override: "68020",
            extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_MISSING_INCLUDE_DEFINE],
            source_override: Some(missing_include_source.as_bytes()),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &missing_include_support,
            proof: OpforgeNativeCliProof::ExpectedFailureContaining(
                "ERROR OPC-NCLI014: native include expansion failed",
            ),
        },
    ];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

pub(crate) fn run_opforge_native_cli_item13_output_directive_from_env(
    workspace_root: &Path,
    source: &[u8],
    _package_bytes: &[u8],
    proof_relative_path: &str,
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let cases = [OpforgeNativeCliParityCase {
        name: "item13-output-directive",
        cpu_override: "68020",
        extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM13_OUTPUT_DIRECTIVE_DEFINE],
        source_override: Some(source),
        command_template: None,
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: proof_relative_path,
            rust_oracle,
        },
    }];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

pub(crate) fn run_opforge_native_cli_item14_prg_output_from_env(
    workspace_root: &Path,
    success_source: &[u8],
    wide_loadaddr_source: &[u8],
    _package_bytes: &[u8],
    success_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let cases = [
        OpforgeNativeCliParityCase {
            name: "item14-prg-success",
            cpu_override: "68020",
            extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM14_OUTPUT_DIRECTIVE_DEFINE],
            source_override: Some(success_source),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.prg",
                rust_oracle: success_oracle,
            },
        },
        OpforgeNativeCliParityCase {
            name: "item14-wide-load-address",
            cpu_override: "68020",
            extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM14_OUTPUT_DIRECTIVE_DEFINE],
            source_override: Some(wide_loadaddr_source),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        },
    ];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

pub(crate) fn run_opforge_native_cli_item15_hex_output_from_env(
    workspace_root: &Path,
    source: &[u8],
    _package_bytes: &[u8],
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let cases = [OpforgeNativeCliParityCase {
        name: "item15-hex-output",
        cpu_override: "68020",
        extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM15_OUTPUT_DIRECTIVE_DEFINE],
        source_override: Some(source),
        command_template: None,
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.hex",
            rust_oracle,
        },
    }];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

pub(crate) fn run_opforge_native_cli_item16_listing_output_from_env(
    workspace_root: &Path,
    source: &[u8],
    _package_bytes: &[u8],
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let cases = [OpforgeNativeCliParityCase {
        name: "item16-listing-output",
        cpu_override: "68020",
        extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM16_LIST_OUTPUT_DEFINE],
        source_override: Some(source),
        command_template: None,
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.lst",
            rust_oracle,
        },
    }];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

pub(crate) fn run_opforge_native_cli_item17_artifact_matrix_from_env(
    workspace_root: &Path,
    sources: [&[u8]; 4],
    _package_bytes: &[u8],
    rust_oracles: [&[u8]; 4],
) -> Result<FsUaeSmokeOutcome, String> {
    let cases = sources
        .iter()
        .zip(rust_oracles)
        .enumerate()
        .map(
            |(index, (source, rust_oracle))| OpforgeNativeCliParityCase {
                name: ["item17-bin", "item17-prg", "item17-hex", "item17-lst"][index],
                cpu_override: "68020",
                extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_ARTIFACT_MATRIX_DEFINE],
                source_override: Some(*source),
                command_template: None,
                package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &[],
                proof: OpforgeNativeCliProof::ExactArtifact {
                    relative_path: [
                        "Work/opforge_native_out.bin",
                        "Work/opforge_native_out.prg",
                        "Work/opforge_native_out.hex",
                        "Work/opforge_native_out.lst",
                    ][index],
                    rust_oracle,
                },
            },
        )
        .collect::<Vec<_>>();
    run_opforge_native_cli_parity_cases_from_env(workspace_root, cases.as_slice())
}

pub(crate) fn run_opforge_native_cli_item17_source_cpu_output_from_env(
    workspace_root: &Path,
    source: &[u8],
    package_bytes: &[u8],
    proof_relative_path: &str,
    rust_oracle: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    let cases = [OpforgeNativeCliParityCase {
        name: "item17-source-cpu-output",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source),
        command_template: Some("{input}"),
        package_mode: OpforgeNativeCliPackageMode::Explicit(package_bytes),
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: proof_relative_path,
            rust_oracle,
        },
    }];
    run_opforge_native_cli_parity_cases_from_env(workspace_root, &cases)
}

fn mos6502_native_cli_focused_pair_package_bytes() -> Result<Vec<u8>, String> {
    let mut registry = ModuleRegistry::new();
    registry.register_family(Box::new(
        families::families::mos6502::module::MOS6502FamilyModule,
    ));
    registry.register_cpu(Box::new(
        families::families::mos6502::module::M6502CpuModule,
    ));
    registry.register_cpu(Box::new(families::m65c02::module::M65C02CpuModule));
    build_hierarchy_package_from_registry(&registry)
        .map_err(|err| format!("build focused MOS native CLI package: {err}"))
}

fn m68020_native_cli_single_pipeline_package_bytes() -> Result<Vec<u8>, String> {
    let family_owner = types::hierarchy::ScopedOwner::Family("motorola68000".to_string());
    let mut registry = ModuleRegistry::new();
    families::register_motorola68000_family_stack(&mut registry);
    let mut chunks = build_hierarchy_chunks_from_registry(&registry)
        .map_err(|err| format!("build m68020 native CLI chunks: {err}"))?;

    chunks
        .cpus
        .retain(|cpu| cpu.id == families::m68020::module::CPU_ID.as_str());
    chunks
        .dialects
        .retain(|dialect| dialect.id == "motorola68k" && dialect.family_id == "motorola68000");
    for dialect in &mut chunks.dialects {
        dialect.cpu_allow_list = Some(vec![families::m68020::module::CPU_ID.as_str().to_string()]);
    }
    chunks
        .token_policies
        .retain(|policy| policy.owner == family_owner);
    chunks
        .tokenizer_vm_programs
        .retain(|program| program.owner == family_owner);

    encode_hierarchy_chunks_from_chunks(&chunks)
        .map_err(|err| format!("encode m68020 native CLI package: {err}"))
}

fn resolve_opforge_native_cli_package_bytes(
    _workspace_root: &Path,
    case: &OpforgeNativeCliParityCase<'_>,
) -> Result<Option<Vec<u8>>, String> {
    match case.package_mode {
        OpforgeNativeCliPackageMode::EmbeddedDefault => Ok(None),
        OpforgeNativeCliPackageMode::Explicit(bytes) => Ok(Some(bytes.to_vec())),
        OpforgeNativeCliPackageMode::Mos6502FocusedPair => {
            mos6502_native_cli_focused_pair_package_bytes().map(Some)
        }
        OpforgeNativeCliPackageMode::M68020SinglePipeline => {
            m68020_native_cli_single_pipeline_package_bytes().map(Some)
        }
    }
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
    captured_artifacts
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
        .collect()
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
            NativeCliParityExecutable::OpforgeCli => b"opforge-cli",
            NativeCliParityExecutable::OpforgeSelfHostGenerationOne => {
                b"opforge-self-host-generation-one"
            }
            NativeCliParityExecutable::TkpkgDebugCliOperandRecord => {
                b"tkpkg-debug-cli-operand-record"
            }
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

fn opforge_native_cli_case_define<'a>(case: &'a OpforgeNativeCliParityCase<'a>) -> Option<&'a str> {
    // Instrumentation is not a fixture selector: it must not add a second
    // discoverable source alias or change command-template interpolation.
    case.extra_assembly_defines.iter().copied().find(|define| {
        matches!(
            *define,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_FLOW_NAVIGATION_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_MACRO_DEBUG_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_MISSING_INCLUDE_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_ITEM13_OUTPUT_DIRECTIVE_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_ITEM14_OUTPUT_DIRECTIVE_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_ITEM15_OUTPUT_DIRECTIVE_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_ITEM16_LIST_OUTPUT_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_ARTIFACT_MATRIX_DEFINE
                | FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_SOURCE_CPU_ONLY_DEFINE
                | "OPFORGE_FS_UAE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC"
                | "OPFORGE_FS_UAE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING"
                | "OPFORGE_FS_UAE_NATIVE_CLI_6502_UNRESOLVED_LABEL"
                | "OPFORGE_FS_UAE_NATIVE_CLI_6502_BAD_ORG"
                | "OPFORGE_FS_UAE_NATIVE_CLI_UNMATCHED_ENDMODULE"
                | "OPFORGE_FS_UAE_NATIVE_CLI_UNTERMINATED_MODULE"
                | "OPFORGE_FS_UAE_NATIVE_CLI_BAD_USE"
                | "OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE"
                | "OPFORGE_FS_UAE_NATIVE_CLI_MISSING_INPUT"
                | "OPFORGE_FS_UAE_NATIVE_CLI_HUNK_OUTPUT"
                | "OPFORGE_FS_UAE_NATIVE_CLI_MIXED_INPUT"
                | "OPFORGE_FS_UAE_NATIVE_CLI_BAD_PACKAGE"
                | "OPFORGE_FS_UAE_NATIVE_CLI_PACKAGE_TOO_LARGE"
                | "OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE_PATH"
                | "OPFORGE_FS_UAE_NATIVE_CLI_MODULE_PATH_OVERFLOW"
                | "OPFORGE_FS_UAE_NATIVE_CLI_UNSUPPORTED_OUTPUT"
                | "OPFORGE_FS_UAE_NATIVE_CLI_MISSING_HUNK"
        )
    })
}

fn opforge_native_cli_case_source_relative_path(
    case: &OpforgeNativeCliParityCase<'_>,
) -> &'static str {
    if case.source_override.is_some() && opforge_native_cli_case_define(case).is_none() {
        return FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE;
    }

    match opforge_native_cli_case_define(case) {
        Some(FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE) => {
            FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC") => {
            FS_UAE_OPFORGE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC_FILE
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING") => {
            FS_UAE_OPFORGE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING_FILE
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_6502_UNRESOLVED_LABEL") => {
            FS_UAE_OPFORGE_NATIVE_CLI_6502_UNRESOLVED_LABEL_FILE
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_6502_BAD_ORG") => {
            FS_UAE_OPFORGE_NATIVE_CLI_6502_BAD_ORG_FILE
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_UNMATCHED_ENDMODULE") => {
            FS_UAE_OPFORGE_NATIVE_CLI_UNMATCHED_ENDMODULE_FILE
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_UNTERMINATED_MODULE") => {
            FS_UAE_OPFORGE_NATIVE_CLI_UNTERMINATED_MODULE_FILE
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_BAD_USE") => FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_FILE,
        Some("OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE") => {
            FS_UAE_OPFORGE_NATIVE_CLI_MISSING_MODULE_FILE
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_MISSING_INPUT") => "opforge_missing_input.asm",
        Some("OPFORGE_FS_UAE_NATIVE_CLI_HUNK_OUTPUT")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_MIXED_INPUT")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_BAD_PACKAGE")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_PACKAGE_TOO_LARGE")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE_PATH")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_MODULE_PATH_OVERFLOW") => FS_UAE_TKPKG_SMOKE_INPUT_FILE,
        Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_MISSING_INCLUDE_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM13_OUTPUT_DIRECTIVE_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM14_OUTPUT_DIRECTIVE_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM15_OUTPUT_DIRECTIVE_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM16_LIST_OUTPUT_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_ARTIFACT_MATRIX_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_SOURCE_CPU_ONLY_DEFINE) => {
            FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE
        }
        _ => FS_UAE_TKPKG_SMOKE_INPUT_FILE,
    }
}

fn opforge_native_cli_case_command(
    case: &OpforgeNativeCliParityCase<'_>,
    paths: &OpforgeNativeCliBatchCasePaths,
) -> String {
    let guest_work_dir = paths.command_guest_work_dir.as_str();
    let guest_path = |relative: &str| format!("{guest_work_dir}{relative}");
    let source_path = guest_path(opforge_native_cli_case_source_relative_path(case));
    let bin_path = guest_path(FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE);
    let _prg_path =
        guest_path(format!("build/{FS_UAE_OPFORGE_NATIVE_CLI_PRG_OUTPUT_FILE}").as_str());
    let _hex_path =
        guest_path(format!("build/{FS_UAE_OPFORGE_NATIVE_CLI_HEX_OUTPUT_FILE}").as_str());
    let _list_path =
        guest_path(format!("build/{FS_UAE_OPFORGE_NATIVE_CLI_LST_OUTPUT_FILE}").as_str());
    let hunk_path = guest_path("build/opforge_native_out.hunk");
    let package_path = guest_path(FS_UAE_OPFORGE_NATIVE_CLI_PACKAGE_GUEST_FILE);
    let oversized_package_path = guest_path(FS_UAE_OPFORGE_NATIVE_CLI_OVERSIZED_PACKAGE_GUEST_FILE);
    let include_a =
        guest_path(FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_A_FILE.trim_end_matches("/defs.inc"));
    let include_b =
        guest_path(FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_B_FILE.trim_end_matches("/defs.inc"));
    let with_case_package = |args: String| match case.package_mode {
        OpforgeNativeCliPackageMode::EmbeddedDefault => args,
        _ if args.contains("--opasm-package") => args,
        _ => format!("{args} --opasm-package {package_path}"),
    };
    let default_package_args = |args: &str| with_case_package(args.to_string());

    if let Some(template) = case.command_template {
        return with_case_package(
            template
                .replace("{input}", &source_path)
                .replace("{bin}", &bin_path)
                .replace("{prg}", &_prg_path)
                .replace("{hex}", &_hex_path)
                .replace("{list}", &_list_path)
                .replace("{hunk}", &hunk_path)
                .replace("{package}", &package_path)
                .replace("{guest_work_dir}", guest_work_dir)
                .replace("{include_a}", &include_a)
                .replace("{include_b}", &include_b),
        );
    }

    if case.source_override.is_some() && opforge_native_cli_case_define(case).is_none() {
        return default_package_args(
            format!("{source_path} --bin {bin_path} --cpu m6502").as_str(),
        );
    }

    match opforge_native_cli_case_define(case) {
        Some(FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE) => {
            default_package_args(format!("{source_path} --bin {bin_path} --cpu m6502").as_str())
        }
        Some(FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE) => {
            default_package_args(format!("{source_path} --bin {bin_path} --cpu 65c02").as_str())
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_6502_UNRESOLVED_LABEL")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_6502_BAD_ORG") => {
            default_package_args(format!("{source_path} --bin {bin_path} --cpu m6502").as_str())
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_UNSUPPORTED_OUTPUT") => {
            default_package_args(
                format!(
                    "{source_path} --hex {} --cpu m6502",
                    guest_path("build/opforge_native_out.hex")
                )
                .as_str(),
            )
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_MISSING_INPUT") => {
            default_package_args(
                format!("{source_path} --bin {bin_path} --cpu m68020").as_str(),
            )
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_MISSING_HUNK") => {
            default_package_args(format!("{source_path} --cpu m6502").as_str())
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_HUNK_OUTPUT") => {
            default_package_args(format!("{source_path} --hunk {hunk_path} --cpu m68020").as_str())
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_MIXED_INPUT") => {
            default_package_args(
                format!("{source_path} --infile {source_path} --bin {bin_path} --cpu m68020").as_str(),
            )
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_BAD_PACKAGE") => {
            format!("{source_path} --bin {bin_path} --cpu m68020 --opasm-package {}", guest_path("opforge_missing_package.opasm"))
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_PACKAGE_TOO_LARGE") => {
            format!("{source_path} --bin {bin_path} --cpu m68020 --opasm-package {oversized_package_path}")
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_UNMATCHED_ENDMODULE")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_UNTERMINATED_MODULE")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_BAD_USE")
        | Some("OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE") => {
            default_package_args(format!("{source_path} --bin {bin_path} --cpu m68020").as_str())
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE_PATH") => {
            default_package_args(format!("{source_path} --bin {bin_path} --cpu m68020 -M").as_str())
        }
        Some("OPFORGE_FS_UAE_NATIVE_CLI_MODULE_PATH_OVERFLOW") => {
            default_package_args(format!("{source_path} --bin {bin_path} --cpu m68020 -M {} -M {} -M {} -M {} -M {} -M {} -M {} -M {}",
                guest_path("mod1"),
                guest_path("mod2"),
                guest_path("mod3"),
                guest_path("mod4"),
                guest_path("mod5"),
                guest_path("mod6"),
                guest_path("mod7"),
                guest_path("mod8"),
            ).as_str())
        }
        Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE) => {
            default_package_args(
                format!("{source_path} --bin {bin_path} --cpu 6502 -I {include_b} -I {include_a}").as_str(),
            )
        }
        Some(FS_UAE_OPFORGE_NATIVE_CLI_MISSING_INCLUDE_DEFINE) => {
            default_package_args(
                format!("{source_path} --bin {bin_path} --cpu 6502 -I {include_a}").as_str(),
            )
        }
        Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM13_OUTPUT_DIRECTIVE_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM14_OUTPUT_DIRECTIVE_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM15_OUTPUT_DIRECTIVE_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM16_LIST_OUTPUT_DEFINE)
        | Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_ARTIFACT_MATRIX_DEFINE) => {
            default_package_args(format!("{source_path} --cpu m6502").as_str())
        }
        Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM17_SOURCE_CPU_ONLY_DEFINE) => {
            default_package_args(source_path.as_str())
        }
        _ => format!(
            "{source_path} --bin {bin_path} --cpu m6502 -M {} --module-path {}",
            guest_path("opforge_module_a"),
            guest_path("opforge_module_b")
        ),
    }
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

fn stage_opforge_native_cli_case_guest_inputs(
    case_paths: &OpforgeNativeCliBatchCasePaths,
    case: &OpforgeNativeCliParityCase<'_>,
    package_bytes: Option<&[u8]>,
) -> Result<(), String> {
    if case.source_override.is_none() {
        stage_opforge_native_cli_common_guest_inputs(
            &case_paths.artifact_dir,
            None,
            package_bytes,
        )?;
        for guest_file in case.extra_guest_files {
            stage_guest_input_bytes(
                &case_paths.artifact_dir,
                guest_file.relative_path,
                guest_file.bytes,
            )?;
        }
        return Ok(());
    }

    let input_override = OpforgeNativeCliStagedInputs {
        source: case.source_override,
        package_bytes,
        extra_guest_files: case.extra_guest_files,
    };
    stage_opforge_native_cli_common_guest_inputs(
        &case_paths.artifact_dir,
        Some(&input_override),
        package_bytes,
    )?;
    if let Some(source) = case.source_override {
        stage_guest_input_bytes(
            &case_paths.artifact_dir,
            opforge_native_cli_case_source_relative_path(case),
            source,
        )?;
    }
    Ok(())
}

fn run_native_cli_parity_batch_cases(
    workspace_root: &Path,
    fs_uae_bin: &str,
    args_text: &str,
    cases: &[OpforgeNativeCliParityCase<'_>],
    executable: NativeCliParityExecutable,
    bootstrap_executable: Option<&[u8]>,
) -> Result<FsUaeSmokeOutcome, String> {
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
        NativeCliParityExecutable::OpforgeCli
        | NativeCliParityExecutable::OpforgeSelfHostGenerationOne => {
            FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME
        }
        NativeCliParityExecutable::TkpkgDebugCliOperandRecord => {
            FS_UAE_TKPKG_DEBUG_CLI_EXAMPLE_NAME
        }
    };
    let source_path = workspace_root.join(match executable {
        NativeCliParityExecutable::OpforgeCli
        | NativeCliParityExecutable::OpforgeSelfHostGenerationOne => {
            FS_UAE_OPFORGE_NATIVE_CLI_SOURCE_PATH
        }
        NativeCliParityExecutable::TkpkgDebugCliOperandRecord => FS_UAE_TKPKG_DEBUG_CLI_SOURCE_PATH,
    });
    if !source_path.is_file() {
        return Err(format!(
            "expected FS-UAE smoke example source at {}",
            source_path.display()
        ));
    }

    let artifact_dir = create_artifact_dir(workspace_root, "fs-uae-hunk-smoke-opforge_cli")?;
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
    let mut batch_script = String::from("FailAt 999\nCD Work:\n");
    let mut batch_paths = Vec::with_capacity(cases.len());
    let run_challenge = opforge_native_cli_run_challenge();
    for (index, case) in cases.iter().enumerate() {
        if case.cpu_override != "68020" {
            return Err(format!(
                "native opForge CLI FS-UAE batch runner currently supports only cpu_override=68020, got {} for case {index}",
                case.cpu_override
            ));
        }
        let package_bytes = resolve_opforge_native_cli_package_bytes(workspace_root, case)?;
        let case_identity = opforge_native_cli_case_identity(
            case,
            package_bytes.as_deref(),
            executable,
            bootstrap_executable,
        );
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
            NativeCliParityExecutable::OpforgeCli
            | NativeCliParityExecutable::OpforgeSelfHostGenerationOne => {
                stage_opforge_native_cli_case_guest_inputs(
                    &case_paths,
                    case,
                    package_bytes.as_deref(),
                )?;
            }
            NativeCliParityExecutable::TkpkgDebugCliOperandRecord => {
                stage_guest_input_bytes(
                    &case_paths.artifact_dir,
                    FS_UAE_TKPKG_OPERAND_RECORD_BATCH_FILE,
                    case.source_override.ok_or_else(|| {
                        format!("tkpkg operand-record case {} has no batch bytes", case.name)
                    })?,
                )?;
            }
        }
        let command = match executable {
            NativeCliParityExecutable::OpforgeCli => format!(
                "Work:build/opforge_cli {}",
                opforge_native_cli_case_command(case, &case_paths)
            ),
            NativeCliParityExecutable::OpforgeSelfHostGenerationOne => format!(
                "Work:opforge {}",
                opforge_native_cli_case_command(case, &case_paths)
            ),
            NativeCliParityExecutable::TkpkgDebugCliOperandRecord => {
                "Work:build/tkpkg_debug_cli_bin".to_string()
            }
        };
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
    let assembly_defines = match executable {
        NativeCliParityExecutable::OpforgeCli
        | NativeCliParityExecutable::OpforgeSelfHostGenerationOne => {
            opforge_native_cli_case_assembly_defines(&cases[0])
        }
        NativeCliParityExecutable::TkpkgDebugCliOperandRecord => vec![
            "OPFORGE_FS_UAE_SMOKE".to_string(),
            "OPFORGE_FS_UAE_TKPKG_OPERAND_RECORD".to_string(),
        ],
    };
    let include_paths = example_include_paths(workspace_root, example_name);
    let module_paths = example_module_paths(workspace_root, example_name);
    let source_path = match executable {
        NativeCliParityExecutable::OpforgeCli
        | NativeCliParityExecutable::OpforgeSelfHostGenerationOne => source_path,
        NativeCliParityExecutable::TkpkgDebugCliOperandRecord => {
            let package_bytes =
                resolve_opforge_native_cli_package_bytes(workspace_root, &cases[0])?.ok_or_else(
                    || "tkpkg operand-record parity requires explicit package bytes".to_string(),
                )?;
            materialize_tkpkg_debug_cli_package_override_source(
                &source_path,
                &artifact_dir,
                &package_bytes,
            )?
        }
    };
    let live_capture = std::env::var("OPFORGE_NATIVE_CORPUS_LIVE_CAPTURE").as_deref() == Ok("1");
    let live_labels_path = artifact_dir.join("live-labels.txt");
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
        labels_file: live_capture.then_some(live_labels_path.as_path()),
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

    let hunk_path = generated_hunk_artifact_path(&artifact_dir, example_name);
    if !hunk_path.is_file() {
        return Err(format!(
            "expected generated Hunk artifact at {}",
            hunk_path.display()
        ));
    }
    let mounted_hunk_alias_path = match executable {
        NativeCliParityExecutable::OpforgeCli => mounted_work_dir.join("build/opforge_cli"),
        NativeCliParityExecutable::OpforgeSelfHostGenerationOne => mounted_work_dir.join("opforge"),
        NativeCliParityExecutable::TkpkgDebugCliOperandRecord => {
            mounted_work_dir.join("build/tkpkg_debug_cli_bin")
        }
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
                        .map(|path| path.to_string_lossy().into_owned())
                        .unwrap_or_default(),
                )
        })
        .collect::<Vec<_>>();

    let mut launch_command = if live_capture {
        let control_mode =
            std::env::var("OPFORGE_NATIVE_CORPUS_CONTROL_MODE").unwrap_or_else(|_| "sample".into());
        if std::env::var("OPFORGE_NATIVE_CORPUS_DIAGNOSTIC").as_deref() != Ok("1")
            || std::env::var("OPFORGE_PERFORMANCE_CORPUS").as_deref() != Ok("1")
            || (control_mode == "sample"
                && std::env::var("OPFORGE_FS_UAE_CONSOLE_DEBUGGER_AUTOMATE").as_deref() != Ok("1"))
            || cases.len() != 1
            || args_text.trim() != "{fsuae_config}"
        {
            return Err(
                "live capture requires one opt-in diagnostic corpus case and canonical args".into(),
            );
        }
        let config = generated_config_path
            .as_ref()
            .ok_or("live capture requires a generated config")?;
        let mut command = Command::new("python3");
        command
            .arg(workspace_root.join("scripts/performance/capture_native_live.py"))
            .arg("--binary")
            .arg(fs_uae_bin)
            .arg("--config")
            .arg(config)
            .arg("--hunk")
            .arg(&mounted_hunk_alias_path)
            .arg("--labels")
            .arg(&live_labels_path)
            .arg("--start-file")
            .arg(&batch_paths[0].started_path)
            .arg("--expected-start")
            .arg(&batch_paths[0].expected_started)
            .arg("--done-file")
            .arg(&batch_paths[0].done_path)
            .arg("--expected-done")
            .arg(&batch_paths[0].expected_done)
            .arg("--exit-file")
            .arg(&batch_paths[0].exit_code_path)
            .arg("--control-mode")
            .arg(control_mode)
            .arg("--after-start-seconds")
            .arg(std::env::var("OPFORGE_NATIVE_CORPUS_SAMPLE_AFTER_SECONDS").unwrap_or_default());
        command
    } else {
        fs_uae_launch_command(fs_uae_bin, &args)
    };
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
            if live_capture {
                eprintln!(
                    "{}",
                    read_optional_text(&launcher_stdout_path)?.unwrap_or_default()
                );
            }
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
        if live_capture {
            // The opt-in observer records DONE before cleaning its child and
            // printing the receipt. The normal 500ms force-kill grace is too
            // short for that bounded cleanup. Never relax guest proof checks.
            let _ = wait_for_diagnostic_observer_exit(&mut child, Duration::from_secs(8))?;
        }
        wait_for_process_exit_after_capture(&mut child, example_name)?;
    }
    let launcher_status = child
        .wait()
        .map_err(|err| format!("wait for FS-UAE process for {}: {err}", example_name))?;
    let _ = cleanup_spawned_fs_uae_processes(&baseline_process_ids);
    wait_for_spawned_fs_uae_processes_to_exit(&baseline_process_ids)?;

    let launcher_stdout = read_optional_text(&launcher_stdout_path)?;
    let launcher_stderr = read_optional_text(&launcher_stderr_path)?;
    if live_capture {
        // Retain Level E observations before the ordinary fail-closed protocol
        // check and ephemeral-tree cleanup. This cannot supply guest evidence.
        eprintln!("{}", launcher_stdout.as_deref().unwrap_or_default());
    }
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
            source_path: case_paths
                .artifact_dir
                .join(opforge_native_cli_case_source_relative_path(case)),
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
            start_to_done_host_seconds: wait_outcome.single_case_seconds(cases.len(), success),
            native_image_digest: Some(opforge_self_host_package_digest(
                &fs::read(&mounted_hunk_alias_path)
                    .map_err(|err| format!("read measured native image: {err}"))?,
            )),
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

fn opforge_native_cli_fixture_assembly_defines() -> Vec<String> {
    // Fixture-backed native CLI parity must build the real CLI entrypoint without
    // FS-UAE-specific test defines. The guest startup script provides the actual
    // CLI arguments for every case.
    Vec::new()
}

fn opforge_native_cli_case_assembly_defines(case: &OpforgeNativeCliParityCase<'_>) -> Vec<String> {
    let mut defines = opforge_native_cli_fixture_assembly_defines();
    defines.extend(
        case.extra_assembly_defines
            .iter()
            .map(|define| (*define).to_string()),
    );
    defines
}

pub(crate) fn run_opforge_native_cli_failure_cases_from_env(
    workspace_root: &Path,
    cases: &[OpforgeNativeCliFailureCase<'_>],
) -> Result<FsUaeSmokeOutcome, String> {
    if cases.is_empty() {
        return Err("native opForge CLI failure-path mode requires at least one case".to_string());
    }
    let define_slices = cases
        .iter()
        .map(|case| vec![case.define])
        .collect::<Vec<_>>();
    let mut parity_cases = Vec::with_capacity(cases.len());
    for (case, defines) in cases.iter().zip(define_slices.iter()) {
        parity_cases.push(OpforgeNativeCliParityCase {
            name: case.name,
            cpu_override: "68020",
            extra_assembly_defines: defines.as_slice(),
            source_override: None,
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureContaining(case.expected_diagnostic),
        });
    }
    run_opforge_native_cli_parity_cases_from_env(workspace_root, parity_cases.as_slice())
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
        &[],
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
        &[],
    )
}

pub(crate) fn run_tkpkg_debug_cli_fixed_opcode_with_package_from_env(
    workspace_root: &Path,
    cpu_id: &str,
    package_bytes: &[u8],
) -> Result<FsUaeSmokeOutcome, String> {
    run_tkpkg_debug_cli_file_mode_with_optional_package_from_env(
        workspace_root,
        b"",
        cpu_id,
        Some(package_bytes),
        &["OPFORGE_FS_UAE_TKPKG_FIXED_OPCODE"],
    )
}

fn run_tkpkg_debug_cli_file_mode_with_optional_package_from_env(
    workspace_root: &Path,
    guest_source: &[u8],
    cpu_id: &str,
    package_bytes: Option<&[u8]>,
    extra_assembly_defines: &[&str],
) -> Result<FsUaeSmokeOutcome, String> {
    run_tkpkg_debug_cli_input_mode_with_optional_package_from_env(
        workspace_root,
        TkpkgDebugCliInputMode::SingleFile(guest_source),
        cpu_id,
        package_bytes,
        extra_assembly_defines,
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
        &[],
    )
}

fn run_tkpkg_debug_cli_input_mode_with_optional_package_from_env<'a>(
    workspace_root: &Path,
    input_mode: TkpkgDebugCliInputMode<'a>,
    cpu_id: &str,
    package_bytes: Option<&'a [u8]>,
    extra_assembly_defines: &'a [&'a str],
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
    let spec = GuestInputSmokeSpec {
        example_name: FS_UAE_TKPKG_DEBUG_CLI_EXAMPLE_NAME,
        relative_source_path: FS_UAE_TKPKG_DEBUG_CLI_SOURCE_PATH,
        cpu_override: "68020",
        input_mode,
        pipeline_define: tkpkg_pipeline_define_for_cpu(cpu_id)?,
        package_bytes,
        extra_assembly_defines,
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

fn example_guest_input(example_name: &str) -> Option<(&'static str, &'static [u8])> {
    match example_name {
        "tkpkg_debug_cli" => Some((
            FS_UAE_TKPKG_SMOKE_INPUT_FILE,
            FS_UAE_TKPKG_SMOKE_INPUT_TEXT.as_bytes(),
        )),
        "opforge_cli" | FS_UAE_CLI_DEBUG_EVENT_EXAMPLE_NAME => Some((
            FS_UAE_TKPKG_SMOKE_INPUT_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_INPUT_TEXT.as_bytes(),
        )),
        FS_UAE_MACRO_CLI_DEBUG_EVENT_HARNESS_NAME => Some((
            FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE,
            include_bytes!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../../examples/opcore/macro_invocation_native.asm"
            )),
        )),
        _ => None,
    }
}

fn example_assembly_defines(example_name: &str) -> Vec<String> {
    match example_name {
        "tkpkg_debug_cli" | "opforge_cli" | FS_UAE_MACRO_CLI_DEBUG_EVENT_HARNESS_NAME => {
            vec!["OPFORGE_FS_UAE_SMOKE".to_string()]
        }
        _ => Vec::new(),
    }
}

fn example_module_paths(workspace_root: &Path, example_name: &str) -> Vec<PathBuf> {
    if example_name == FS_UAE_CLI_DEBUG_EVENT_EXAMPLE_NAME
        || example_name == FS_UAE_PROGRESS_HARNESS_NAME
        || example_name == FS_UAE_MACRO_PREPROCESSOR_HARNESS_NAME
        || example_name == FS_UAE_PIPELINE_SELECT_HARNESS_NAME
        || example_name == FS_UAE_MACRO_CLI_DEBUG_EVENT_HARNESS_NAME
    {
        let amigaos_dir = workspace_root
            .join("native")
            .join("motorola68000")
            .join("amigaos");
        return vec![
            amigaos_dir.join("opforge-cli"),
            amigaos_dir.join("tkpkg"),
            amigaos_dir.join("tkvm"),
            amigaos_dir.join("prvm"),
            amigaos_dir.join("exprvm"),
            amigaos_dir.join("opcore"),
            amigaos_dir.join("opasm"),
            amigaos_dir.join("debug"),
        ];
    }

    if example_name == FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME {
        let amigaos_dir = workspace_root
            .join("native")
            .join("motorola68000")
            .join("amigaos");
        return vec![amigaos_dir.join("debug")];
    }

    if example_name == FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME
        || example_name == FS_UAE_CLI_DEBUG_EVENT_EXAMPLE_NAME
    {
        // Resolve the whole native CLI composition from the worktree. Omitting
        // this directory can mix a stale CLI composition with current tkpkg
        // and opasm modules, which is not valid native parity evidence.
        let amigaos_dir = workspace_root
            .join("native")
            .join("motorola68000")
            .join("amigaos");
        return vec![
            amigaos_dir.join("opforge-cli"),
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
            amigaos_dir.join("opforge-cli"),
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
    if example_name == FS_UAE_DEBUG_CONTRACT_EXAMPLE_NAME
        || example_name == FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME
        || example_name == FS_UAE_PROGRESS_HARNESS_NAME
        || example_name == FS_UAE_CLI_DEBUG_EVENT_EXAMPLE_NAME
        || example_name == FS_UAE_MACRO_PREPROCESSOR_HARNESS_NAME
        || example_name == FS_UAE_PIPELINE_SELECT_HARNESS_NAME
        || example_name == FS_UAE_MACRO_CLI_DEBUG_EVENT_HARNESS_NAME
    {
        let amigaos_dir = workspace_root
            .join("native")
            .join("motorola68000")
            .join("amigaos");
        return vec![amigaos_dir.join("debug")];
    }

    if example_name == "tkpkg_debug_cli" {
        let amigaos_dir = workspace_root
            .join("native")
            .join("motorola68000")
            .join("amigaos");
        return vec![amigaos_dir.join("tkpkg"), amigaos_dir.join("tkvm")];
    }

    Vec::new()
}

fn stage_opforge_native_cli_common_guest_inputs(
    mounted_work_dir: &Path,
    native_cli_input_override: Option<&OpforgeNativeCliStagedInputs<'_>>,
    package_bytes: Option<&[u8]>,
) -> Result<(), String> {
    if let Some(input) = native_cli_input_override {
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE,
            input
                .source
                .unwrap_or_else(|| FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_TEXT.as_bytes()),
        )?;
        for guest_file in input.extra_guest_files {
            stage_guest_input_bytes(mounted_work_dir, guest_file.relative_path, guest_file.bytes)?;
        }
        if let Some(package_bytes) = package_bytes {
            stage_guest_input_bytes(
                mounted_work_dir,
                FS_UAE_OPFORGE_NATIVE_CLI_PACKAGE_GUEST_FILE,
                package_bytes,
            )?;
        }
        return Ok(());
    }

    stage_guest_input_bytes(
        mounted_work_dir,
        FS_UAE_TKPKG_SMOKE_INPUT_FILE,
        FS_UAE_OPFORGE_NATIVE_CLI_INPUT_TEXT.as_bytes(),
    )?;
    stage_guest_input_bytes(
        mounted_work_dir,
        FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE,
        native_cli_input_override
            .and_then(|input| input.source)
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
    if let Some(input) = native_cli_input_override {
        for guest_file in input.extra_guest_files {
            stage_guest_input_bytes(mounted_work_dir, guest_file.relative_path, guest_file.bytes)?;
        }
    }
    if let Some(package_bytes) = package_bytes {
        stage_guest_input_bytes(
            mounted_work_dir,
            FS_UAE_OPFORGE_NATIVE_CLI_PACKAGE_GUEST_FILE,
            package_bytes,
        )?;
    }
    // This must exceed native tkpkg PACKAGE_STORAGE_CAPACITY (393216 bytes).
    let oversized_package = vec![0u8; FS_UAE_OPFORGE_NATIVE_CLI_OVERSIZED_PACKAGE_BYTES];
    stage_guest_input_bytes(
        mounted_work_dir,
        FS_UAE_OPFORGE_NATIVE_CLI_OVERSIZED_PACKAGE_GUEST_FILE,
        &oversized_package,
    )?;
    Ok(())
}

fn stage_example_guest_inputs(
    _workspace_root: &Path,
    example_name: &str,
    mounted_work_dir: &Path,
    _extra_assembly_defines: &[&str],
    native_cli_input_override: Option<&OpforgeNativeCliStagedInputs<'_>>,
) -> Result<(), String> {
    if example_name == FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME && native_cli_input_override.is_some()
    {
        return stage_opforge_native_cli_common_guest_inputs(
            mounted_work_dir,
            native_cli_input_override,
            native_cli_input_override.and_then(|input| input.package_bytes),
        );
    }

    let Some((relative_path, bytes)) = example_guest_input(example_name) else {
        return Ok(());
    };

    stage_guest_input_bytes(mounted_work_dir, relative_path, bytes)?;
    if example_name == FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME {
        stage_opforge_native_cli_common_guest_inputs(
            mounted_work_dir,
            native_cli_input_override,
            native_cli_input_override.and_then(|input| input.package_bytes),
        )?;
    }
    Ok(())
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
    extra_assembly_defines: &'a [&'a str],
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
    Captured(Option<Duration>),
}

impl FsUaeWaitOutcome {
    fn single_case_seconds(self, case_count: usize, success: bool) -> Option<f64> {
        match self {
            Self::Captured(Some(duration)) if case_count == 1 && success => {
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
    let mut replaced_zorro_memory = false;
    for line in template_text.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("hard_drive_1") {
            lines.push(format!("hard_drive_1 = {work_mount_path}"));
            replaced_work_mount = true;
        } else if trimmed.starts_with("zorro_iii_memory") {
            lines.push(format!(
                "zorro_iii_memory = {FS_UAE_NATIVE_ZORRO_III_MEMORY_KIB}"
            ));
            replaced_zorro_memory = true;
        } else {
            lines.push(line.to_string());
        }
    }
    if !replaced_work_mount {
        lines.push(format!("hard_drive_1 = {work_mount_path}"));
    }
    if !replaced_zorro_memory {
        lines.push(format!(
            "zorro_iii_memory = {FS_UAE_NATIVE_ZORRO_III_MEMORY_KIB}"
        ));
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

fn wait_for_diagnostic_observer_exit(
    child: &mut std::process::Child,
    max_wait: Duration,
) -> Result<bool, String> {
    let deadline = Instant::now() + max_wait;
    loop {
        if child
            .try_wait()
            .map_err(|err| format!("poll diagnostic observer: {err}"))?
            .is_some()
        {
            return Ok(true);
        }
        if Instant::now() >= deadline {
            return Ok(false);
        }
        thread::sleep(Duration::from_millis(50));
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
    native_cli_input_override: Option<&OpforgeNativeCliStagedInputs<'_>>,
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
    native_cli_input_override: Option<&'a OpforgeNativeCliStagedInputs<'a>>,
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
    let ephemeral_artifact_dir = EphemeralArtifactDir(artifact_dir.clone());
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
    Ok(ExampleSmokeResult::Run(run))
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
    let ephemeral_artifact_dir = EphemeralArtifactDir(artifact_dir.clone());
    let mounted_work_dir = artifact_dir.join(FS_UAE_MOUNTED_WORK_DIR_NAME);
    fs::create_dir_all(mounted_work_dir.join("build")).map_err(|err| {
        format!(
            "create mounted Work directory {}: {err}",
            mounted_work_dir.display(),
        )
    })?;
    match spec.input_mode {
        TkpkgDebugCliInputMode::SingleFile(guest_source) => {
            let guest_file = if spec
                .extra_assembly_defines
                .contains(&"OPFORGE_FS_UAE_TKPKG_OPERAND_RECORD")
            {
                FS_UAE_TKPKG_OPERAND_RECORD_BATCH_FILE
            } else {
                FS_UAE_TKPKG_SMOKE_INPUT_FILE
            };
            stage_guest_input_bytes(&mounted_work_dir, guest_file, guest_source)?;
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
    assembly_defines.extend(
        spec.extra_assembly_defines
            .iter()
            .map(|define| (*define).to_string()),
    );
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
            "assemble FS-UAE smoke example {} from {}: {}; diagnostics: {:#?}; source notes: {:#?}",
            spec.example_name,
            source_path.display(),
            err.summary(),
            err.diagnostics(),
            err.source_lines()
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
                spec.example_name,
                hunk_path.display()
            ))
        }
    };

    let wait_outcome = match wait_for_capture_or_exit(
        &mut child,
        &capture,
        spec.example_name,
        &baseline_process_ids,
    ) {
        Ok(wait_outcome) => wait_outcome,
        Err(err) => {
            let _ = cleanup_spawned_fs_uae_processes(&baseline_process_ids);
            let _ = wait_for_spawned_fs_uae_processes_to_exit(&baseline_process_ids);
            return Err(err);
        }
    };
    if matches!(wait_outcome, FsUaeWaitOutcome::Captured(_)) {
        wait_for_process_exit_after_capture(&mut child, spec.example_name)?;
    }
    let launcher_status = child
        .wait()
        .map_err(|err| format!("wait for FS-UAE process for {}: {err}", spec.example_name))?;
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
        example_name: spec.example_name,
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
        spec.example_name,
        run.protocol_completed,
        run.exit_code,
        &run.stdout,
        &run.stderr,
    )?;
    Ok(ExampleSmokeResult::Run(run))
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

    fn test_workspace_root() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("canonical test workspace root")
    }

    #[test]
    fn completed_timing_never_promotes_missing_failed_or_batch_evidence() {
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
    fn self_host_command_is_exact_and_allows_only_reviewed_path_rendering() {
        let unix_paths = OpforgeSelfHostPathRendering {
            executable: "opforge",
            product_dir: "native/motorola68000/amigaos",
            listing: "build/opforge.lst",
            srec: "build/opforge.srec",
        };
        let amiga_paths = OpforgeSelfHostPathRendering {
            executable: "Work:opforge",
            product_dir: "Work:native/motorola68000/amigaos",
            listing: "Work:build/opforge.lst",
            srec: "Work:build/opforge.srec",
        };
        let unix = render_opforge_self_host_command(unix_paths).expect("render Unix command");
        let amiga = render_opforge_self_host_command(amiga_paths).expect("render Amiga command");
        assert_eq!(
            unix,
            [
                "opforge",
                "native/motorola68000/amigaos",
                "-l",
                "build/opforge.lst",
                "-s",
                "build/opforge.srec",
            ]
        );
        assert_eq!(
            amiga,
            [
                "Work:opforge",
                "Work:native/motorola68000/amigaos",
                "-l",
                "Work:build/opforge.lst",
                "-s",
                "Work:build/opforge.srec",
            ]
        );
        verify_opforge_self_host_same_logical_command(&unix, unix_paths, &amiga, amiga_paths)
            .expect("reviewed path-only translation");

        let mut changed_flag = amiga.clone();
        changed_flag[2] = "--list".to_string();
        assert!(verify_opforge_self_host_same_logical_command(
            &unix,
            unix_paths,
            &changed_flag,
            amiga_paths,
        )
        .is_err());
        let mut changed_value = amiga.clone();
        changed_value[5] = "Work:build/other.srec".to_string();
        assert!(verify_opforge_self_host_same_logical_command(
            &unix,
            unix_paths,
            &changed_value,
            amiga_paths,
        )
        .is_err());
        let mut reordered = amiga.clone();
        reordered.swap(2, 4);
        assert!(verify_opforge_self_host_same_logical_command(
            &unix,
            unix_paths,
            &reordered,
            amiga_paths,
        )
        .is_err());
        let mut added = amiga.clone();
        added.extend(["--cpu".to_string(), "68020".to_string()]);
        assert!(verify_opforge_self_host_same_logical_command(
            &unix,
            unix_paths,
            &added,
            amiga_paths,
        )
        .is_err());
    }

    #[test]
    fn self_host_product_tree_has_a_complete_classic_amigaos_name_map() {
        let root = test_workspace_root();
        let files = collect_opforge_self_host_product_tree(&root)
            .expect("collect complete self-host product tree");
        assert!(files.len() >= 95, "complete tree unexpectedly shrank");
        assert!(files.iter().any(|file| {
            file.logical_relative_path.ends_with("main.asm")
                && file.staged_relative_path.ends_with("main.asm")
        }));
        assert!(files.iter().any(|file| {
            file.logical_relative_path
                .ends_with("opforge_cli_package.opasm")
                && file
                    .staged_relative_path
                    .ends_with("opforge_cli_package.opasm")
        }));

        let mapped = files
            .iter()
            .filter(|file| file.logical_relative_path != file.staged_relative_path)
            .map(|file| {
                (
                    file.logical_relative_path
                        .file_name()
                        .expect("logical filename")
                        .to_string_lossy()
                        .into_owned(),
                    file.staged_relative_path
                        .file_name()
                        .expect("staged filename")
                        .to_string_lossy()
                        .into_owned(),
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(
            mapped,
            [
                (
                    "opforge_symbol_expr_profile.asm".to_string(),
                    "opforge_symexpr_profile.asm".to_string(),
                ),
                (
                    "tkpkg_engine_context_adapter.asm".to_string(),
                    "tkpkg_engine_ctx_adapter.asm".to_string(),
                ),
                (
                    "tkpkg_operand_record_service.asm".to_string(),
                    "tkpkg_operand_rec_service.asm".to_string(),
                ),
            ]
        );
        for file in &files {
            for component in file.staged_relative_path.components() {
                let std::path::Component::Normal(component) = component else {
                    panic!(
                        "staged path is not normal: {}",
                        file.staged_relative_path.display()
                    );
                };
                assert!(
                    component.as_encoded_bytes().len() <= AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX,
                    "overlong staged component in {}",
                    file.staged_relative_path.display()
                );
            }
            assert_eq!(
                file.bytes,
                fs::read(root.join(&file.logical_relative_path)).expect("reread logical input"),
                "staging changed bytes for {}",
                file.logical_relative_path.display()
            );
        }

        let unmapped = opforge_self_host_staged_relative_path(Path::new(
            "native/motorola68000/amigaos/this_component_is_definitely_too_long.asm",
        ))
        .expect_err("unreviewed long component must fail closed");
        assert!(unmapped.contains("has no reviewed"));
    }

    #[test]
    fn malformed_use_map_fixture_is_65c02_and_omits_required_comma() {
        assert!(FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_TEXT.contains(".cpu 65c02"));
        assert!(FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_TEXT
            .contains("map { code -> app_code data -> app_data }"));
        assert!(!FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_TEXT.contains("app_code,"));
    }

    #[test]
    fn diagnostic_observer_gets_bounded_receipt_cleanup_time() {
        let mut child = Command::new("python3")
            .args(["-c", "import time; time.sleep(0.7)"])
            .spawn()
            .expect("host observer stand-in");
        assert!(!wait_for_diagnostic_observer_exit(&mut child, Duration::ZERO).unwrap());
        let finished =
            wait_for_diagnostic_observer_exit(&mut child, Duration::from_secs(4)).unwrap();
        if !finished {
            let _ = child.kill();
        }
        let status = child.wait().unwrap();
        assert!(
            finished && status.success(),
            "observer should finish after the old 500ms grace"
        );
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
            example_name: FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
            source_path: PathBuf::from("/ephemeral/Work/input.asm"),
            artifact_dir: PathBuf::from("/ephemeral"),
            hunk_path: PathBuf::from("/ephemeral/opforge_cli"),
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
    fn mos_byte_proof_rejects_incomplete_nonzero_missing_and_mismatched_runs() {
        let expected = [0xa9, 0x42];
        let case = OpforgeNativeCliParityCase {
            name: "proof-case",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b"lda #$42\n"),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: OpforgeNativeCliPackageMode::Explicit(b"package"),
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &expected,
            },
        };

        for mut run in [
            proof_test_run(false, Some(0), Some(&expected)),
            proof_test_run(true, Some(5), Some(&expected)),
            proof_test_run(true, Some(0), None),
            proof_test_run(true, Some(0), Some(&[0xa9, 0x43])),
        ] {
            assert!(verify_native_cli_case_proof(&case, &mut run).is_err());
        }

        let mut run = proof_test_run(true, Some(0), Some(&expected));
        verify_native_cli_case_proof(&case, &mut run).expect("exact fresh byte proof");
        assert_eq!(run.verified_output.as_deref(), Some(expected.as_slice()));
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
                NativeCliParityExecutable::OpforgeCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &changed_source,
                None,
                NativeCliParityExecutable::OpforgeCli,
                None,
            )
        );
        assert_ne!(
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::OpforgeCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &changed_command,
                None,
                NativeCliParityExecutable::OpforgeCli,
                None,
            )
        );
        assert_ne!(
            opforge_native_cli_case_identity(
                &base,
                Some(b"package-a"),
                NativeCliParityExecutable::OpforgeCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &base,
                Some(b"package-b"),
                NativeCliParityExecutable::OpforgeCli,
                None,
            )
        );
        assert_ne!(
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::OpforgeCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &changed_oracle,
                None,
                NativeCliParityExecutable::OpforgeCli,
                None,
            )
        );
        assert_eq!(
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::OpforgeCli,
                None,
            ),
            opforge_native_cli_case_identity(
                &renamed,
                None,
                NativeCliParityExecutable::OpforgeCli,
                None,
            )
        );
        assert_ne!(
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::OpforgeSelfHostGenerationOne,
                None,
            ),
            opforge_native_cli_case_identity(
                &base,
                None,
                NativeCliParityExecutable::OpforgeSelfHostGenerationOne,
                Some(b"captured-generation-one"),
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
    fn native_cli_batch_commands_use_fresh_case_mounted_work_volume() {
        let mounted_work_dir = Path::new("/tmp/opforge-fsuae-smoke/Work");
        let paths = opforge_native_cli_batch_case_paths(mounted_work_dir, 0, "challenge", "case");
        let case = OpforgeNativeCliParityCase {
            name: "bounded-guest-alias",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b"lda #1\n"),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };

        let command = opforge_native_cli_case_command(&case, &paths);

        assert!(command.contains("Work:opforge_6502_native_cli_smoke.asm"));
        assert!(command.contains("Work:opforge_native_out.bin"));
        assert_eq!(paths.artifact_dir, mounted_work_dir);
        assert!(!opforge_native_cli_case_assembly_defines(&case)
            .iter()
            .any(|define| define == "OPFORGE_FS_UAE_SMOKE"));
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
        let template = "[fs-uae]\nhard_drive_0 = /sys\nhard_drive_1 = /old/work\nzorro_iii_memory = 16384\nsave_disk = 0\n";
        let rewritten = rewrite_fs_uae_config_work_mount(template, "/new/work");

        assert!(rewritten.contains("hard_drive_0 = /sys"));
        assert!(rewritten.contains("hard_drive_1 = /new/work"));
        assert!(!rewritten.contains("hard_drive_1 = /old/work"));
        assert!(rewritten.contains("zorro_iii_memory = 65536"));
        assert!(!rewritten.contains("zorro_iii_memory = 16384"));
    }

    #[test]
    fn rewrite_fs_uae_config_work_mount_appends_missing_hard_drive_1() {
        let template = "[fs-uae]\nhard_drive_0 = /sys\n";
        let rewritten = rewrite_fs_uae_config_work_mount(template, "/new/work");

        assert!(rewritten.contains("hard_drive_0 = /sys"));
        assert!(rewritten.contains("hard_drive_1 = /new/work"));
        assert!(rewritten.contains("zorro_iii_memory = 65536"));
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
    fn native_cli_fixture_batch_assembles_without_fs_uae_smoke_define() {
        assert!(
            opforge_native_cli_fixture_assembly_defines().is_empty(),
            "fixture-backed native CLI parity must not compile OPFORGE_FS_UAE_SMOKE into opforge_cli"
        );
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

    #[test]
    fn native_cli_guest_staging_writes_extra_guest_files() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time after unix epoch")
            .as_nanos();
        let mounted_work_dir = std::env::temp_dir().join(format!(
            "opforge-fsuae-native-cli-extra-guest-files-{unique}"
        ));
        fs::create_dir_all(&mounted_work_dir).expect("create mounted Work directory");

        let extra_guest_files = [OpforgeNativeCliGuestFile {
            relative_path: "custom/modules/math.asm",
            bytes: b".module math\n.endmodule\n",
        }];
        let input_override = OpforgeNativeCliStagedInputs {
            source: Some(b"        lda #$42\n"),
            package_bytes: Some(b"pkg"),
            extra_guest_files: &extra_guest_files,
        };

        stage_example_guest_inputs(
            Path::new("/unused"),
            FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
            &mounted_work_dir,
            &[],
            Some(&input_override),
        )
        .expect("stage native CLI guest inputs with extra files");

        let extra_path = mounted_work_dir.join("custom/modules/math.asm");
        assert_eq!(
            fs::read(&extra_path).expect("read staged extra guest file"),
            b".module math\n.endmodule\n"
        );
        let staged_source = mounted_work_dir.join(FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE);
        assert_eq!(
            fs::read(&staged_source).expect("read staged native CLI source"),
            b"        lda #$42\n"
        );
        for unrelated in [
            FS_UAE_TKPKG_SMOKE_INPUT_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_BAD_USE_FILE,
            FS_UAE_OPFORGE_NATIVE_CLI_OVERSIZED_PACKAGE_GUEST_FILE,
        ] {
            assert!(
                !mounted_work_dir.join(unrelated).exists(),
                "case-specific support staging must exclude unrelated fixture {unrelated}"
            );
        }

        fs::remove_dir_all(&mounted_work_dir).expect("remove mounted Work directory");
    }

    #[test]
    fn native_cli_case_staging_uses_the_command_selected_source_path() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time after unix epoch")
            .as_nanos();
        let root =
            std::env::temp_dir().join(format!("opforge-fsuae-native-cli-selected-source-{unique}"));
        let ephemeral_root = EphemeralArtifactDir(root.clone());
        fs::create_dir_all(&root).expect("create case Work directory");
        let paths = opforge_native_cli_batch_case_paths(&root, 0, "challenge", "case");
        let source = b"        definitely_not_6502\n";
        let case = OpforgeNativeCliParityCase {
            name: "selected-source-staging",
            cpu_override: "68020",
            extra_assembly_defines: &["OPFORGE_FS_UAE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC"],
            source_override: Some(source),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };

        stage_opforge_native_cli_case_guest_inputs(&paths, &case, None)
            .expect("stage selected source path");

        let selected = root.join(FS_UAE_OPFORGE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC_FILE);
        assert_eq!(
            fs::read(selected).expect("read selected staged source"),
            source
        );
        assert!(opforge_native_cli_case_command(&case, &paths).starts_with(
            format!("Work:{FS_UAE_OPFORGE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC_FILE}").as_str()
        ));
        drop(ephemeral_root);
        assert!(!root.exists(), "selected source evidence must be ephemeral");
    }

    #[test]
    fn native_cli_no_override_cases_stage_specialized_and_fallback_builtin_sources() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time after unix epoch")
            .as_nanos();
        let root =
            std::env::temp_dir().join(format!("opforge-fsuae-native-cli-builtin-sources-{unique}"));
        let ephemeral_root = EphemeralArtifactDir(root.clone());

        let specialized_root = root.join("specialized");
        fs::create_dir_all(&specialized_root).expect("create specialized Work directory");
        let specialized_paths =
            opforge_native_cli_batch_case_paths(&specialized_root, 0, "challenge", "specialized");
        let specialized = OpforgeNativeCliParityCase {
            name: "builtin-specialized-source",
            cpu_override: "68020",
            extra_assembly_defines: &["OPFORGE_FS_UAE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC"],
            source_override: None,
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };
        stage_opforge_native_cli_case_guest_inputs(&specialized_paths, &specialized, None)
            .expect("stage specialized built-in source");
        assert_eq!(
            fs::read(specialized_root.join(FS_UAE_OPFORGE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC_FILE))
                .expect("read specialized built-in source"),
            FS_UAE_OPFORGE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC_TEXT.as_bytes()
        );

        let fallback_root = root.join("fallback");
        fs::create_dir_all(&fallback_root).expect("create fallback Work directory");
        let fallback_paths =
            opforge_native_cli_batch_case_paths(&fallback_root, 0, "challenge", "fallback");
        let fallback = OpforgeNativeCliParityCase {
            name: "builtin-fallback-source",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: None,
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };
        stage_opforge_native_cli_case_guest_inputs(&fallback_paths, &fallback, None)
            .expect("stage fallback built-in source");
        assert_eq!(
            fs::read(fallback_root.join(FS_UAE_TKPKG_SMOKE_INPUT_FILE))
                .expect("read fallback built-in source"),
            FS_UAE_OPFORGE_NATIVE_CLI_INPUT_TEXT.as_bytes()
        );

        drop(ephemeral_root);
        assert!(!root.exists(), "built-in source evidence must be ephemeral");
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
    fn native_cli_guest_staging_uses_embedded_default_package_when_no_override_is_present() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time after unix epoch")
            .as_nanos();
        let mounted_work_dir = std::env::temp_dir().join(format!(
            "opforge-fsuae-native-cli-embedded-default-{unique}"
        ));
        fs::create_dir_all(&mounted_work_dir).expect("create mounted Work directory");

        stage_example_guest_inputs(
            Path::new("/unused"),
            FS_UAE_OPFORGE_NATIVE_CLI_EXAMPLE_NAME,
            &mounted_work_dir,
            &[],
            None,
        )
        .expect("stage native CLI guest inputs without package override");

        assert!(
            !mounted_work_dir
                .join(FS_UAE_OPFORGE_NATIVE_CLI_PACKAGE_GUEST_FILE)
                .exists(),
            "embedded-default native CLI staging must not materialize an external package file"
        );

        fs::remove_dir_all(&mounted_work_dir).expect("remove mounted Work directory");
    }

    #[test]
    fn item10_native_cli_cases_route_through_6502_override_source_path() {
        for define in [
            FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE,
            FS_UAE_OPFORGE_NATIVE_CLI_MISSING_INCLUDE_DEFINE,
        ] {
            let case = OpforgeNativeCliParityCase {
                name: "item10-source-routing",
                cpu_override: "68020",
                extra_assembly_defines: &[define],
                source_override: Some(b"        .include \"defs.inc\"\n"),
                command_template: None,
                package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &[],
                proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
            };

            assert_eq!(
                opforge_native_cli_case_source_relative_path(&case),
                FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE,
                "expected Item 10 define {define} to use the 6502 override source file"
            );
        }
    }

    #[test]
    fn item10_native_cli_command_uses_include_roots_and_6502_override_source() {
        let case = OpforgeNativeCliParityCase {
            name: "item10-command",
            cpu_override: "68020",
            extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE],
            source_override: Some(b"        .include \"defs.inc\"\n"),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };
        let paths = opforge_native_cli_batch_case_paths(
            Path::new("/tmp/opforge-fsuae"),
            0,
            "challenge",
            "case",
        );

        assert_eq!(
            opforge_native_cli_case_command(&case, &paths),
            "Work:opforge_6502_native_cli_smoke.asm --bin Work:opforge_native_out.bin --cpu 6502 -I Work:opforge_include_root_b -I Work:opforge_include_root_a"
        );
    }

    #[test]
    fn profiling_defines_preserve_guest_source_inventory_and_legacy_selectors() {
        // Level B staging/command contract only; no native execution proof.
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = std::env::temp_dir().join(format!("opforge-profile-inventory-{unique}"));
        let ephemeral_root = EphemeralArtifactDir(root.clone());
        let mut baseline = None;
        for (index, defines) in [
            &[][..],
            &["OPFORGE_SESSION_CLEAR_BYTE_REFERENCE"][..],
            &["OPFORGE_SESSION_CLEAR_ALL_STATEMENTS"][..],
            &["OPFORGE_MODULE_SCAN_BYTE_READ_REFERENCE"][..],
            &[
                "OPFORGE_DEBUG_CONTRACTS",
                "OPFORGE_PROGRESS_PLATFORM_COUNTERS",
            ][..],
            &[
                "OPFORGE_DEBUG_CONTRACTS",
                "OPFORGE_PROGRESS_PLATFORM_COUNTERS",
                "OPFORGE_PROGRESS_PLATFORM_NO_IO",
            ][..],
        ]
        .into_iter()
        .enumerate()
        {
            let work = root.join(index.to_string());
            fs::create_dir_all(&work).unwrap();
            let paths = opforge_native_cli_batch_case_paths(&work, 0, "challenge", "case");
            let mut case = OpforgeNativeCliParityCase {
                name: "profile-inventory",
                cpu_override: "68020",
                extra_assembly_defines: defines,
                source_override: Some(b".module main\n.endmodule\n"),
                command_template: Some("{input} --cpu m6502 --bin {bin}"),
                package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &[],
                proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
            };
            stage_opforge_native_cli_case_guest_inputs(&paths, &case, None).unwrap();
            let mut inventory: Vec<_> = fs::read_dir(&work)
                .unwrap()
                .map(|entry| {
                    let entry = entry.unwrap();
                    (entry.file_name(), fs::read(entry.path()).unwrap())
                })
                .collect();
            inventory.sort();
            assert_eq!(inventory.len(), 1);
            assert_eq!(inventory[0].0, FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE);
            let evidence = (inventory, opforge_native_cli_case_command(&case, &paths));
            if let Some(expected) = &baseline {
                assert_eq!(&evidence, expected);
            } else {
                baseline = Some(evidence);
            }
            let legacy_defines = [
                "OPFORGE_DEBUG_CONTRACTS",
                FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE,
            ];
            case.extra_assembly_defines = &legacy_defines;
            case.command_template = None;
            assert_eq!(
                opforge_native_cli_case_define(&case),
                Some(FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE)
            );
            assert!(opforge_native_cli_case_command(&case, &paths)
                .contains(" -I Work:opforge_include_root_b -I Work:opforge_include_root_a"));
        }
        drop(ephemeral_root);
        assert!(!root.exists());
    }

    #[test]
    fn source_override_without_define_uses_case_specific_6502_input_and_command() {
        let case = OpforgeNativeCliParityCase {
            name: "source-override-command",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b"        .module app\n"),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };
        let paths = opforge_native_cli_batch_case_paths(
            Path::new("/tmp/opforge-fsuae"),
            0,
            "challenge",
            "case",
        );

        assert_eq!(
            opforge_native_cli_case_source_relative_path(&case),
            FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE
        );
        assert_eq!(
            opforge_native_cli_case_command(&case, &paths),
            "Work:opforge_6502_native_cli_smoke.asm --bin Work:opforge_native_out.bin --cpu m6502"
        );
    }

    #[test]
    fn source_cpu_only_command_template_runs_input_only() {
        let case = OpforgeNativeCliParityCase {
            name: "source-cpu-only-command",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b"        .cpu 6502\n"),
            command_template: Some("{input}"),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };
        let paths = opforge_native_cli_batch_case_paths(
            Path::new("/tmp/opforge-fsuae"),
            0,
            "challenge",
            "case",
        );

        assert_eq!(
            opforge_native_cli_case_command(&case, &paths),
            "Work:opforge_6502_native_cli_smoke.asm"
        );
    }

    #[test]
    fn explicit_command_template_interpolates_guest_paths() {
        let case = OpforgeNativeCliParityCase {
            name: "explicit-command-template",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b"        lda #$42\n"),
            command_template: Some("{input} --list {list} --cpu m6502 -I {include_a}"),
            package_mode: OpforgeNativeCliPackageMode::Mos6502FocusedPair,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };
        let paths = opforge_native_cli_batch_case_paths(
            Path::new("/tmp/opforge-fsuae"),
            0,
            "challenge",
            "case",
        );

        assert_eq!(
            opforge_native_cli_case_command(&case, &paths),
            "Work:opforge_6502_native_cli_smoke.asm --list Work:build/opforge_native_out.lst --cpu m6502 -I Work:opforge_include_root_a --opasm-package Work:p.opasm"
        );
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
        let first_input = OpforgeNativeCliStagedInputs {
            source: Some(b"lda #$11\n"),
            package_bytes: None,
            extra_guest_files: &[],
        };
        let second_input = OpforgeNativeCliStagedInputs {
            source: Some(b"lda #$22\n"),
            package_bytes: None,
            extra_guest_files: &[],
        };
        stage_opforge_native_cli_common_guest_inputs(&first.artifact_dir, Some(&first_input), None)
            .expect("stage first isolated case");
        stage_opforge_native_cli_common_guest_inputs(
            &second.artifact_dir,
            Some(&second_input),
            None,
        )
        .expect("stage second isolated case");
        assert_eq!(
            fs::read(
                first
                    .artifact_dir
                    .join(FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE)
            )
            .expect("read first isolated source"),
            b"lda #$11\n"
        );
        assert_eq!(
            fs::read(
                second
                    .artifact_dir
                    .join(FS_UAE_OPFORGE_NATIVE_CLI_6502_INPUT_FILE)
            )
            .expect("read second isolated source"),
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

    fn normalize_cli_surface_tokens(tokens: &[String]) -> Vec<String> {
        tokens
            .iter()
            .map(|token| {
                if token.ends_with(".asm") {
                    "<input>".to_string()
                } else if token.ends_with(".bin") {
                    "<bin>".to_string()
                } else if token.ends_with("defs.inc") {
                    "<defs.inc>".to_string()
                } else if token.contains("include_root_a") {
                    "<include-root-a>".to_string()
                } else if token.contains("include_root_b") {
                    "<include-root-b>".to_string()
                } else {
                    token.clone()
                }
            })
            .collect()
    }

    #[test]
    fn default_bin_native_cli_command_matches_rust_cli_arg_surface() {
        use clap::Parser;

        let case = OpforgeNativeCliParityCase {
            name: "default-bin-command",
            cpu_override: "68020",
            extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_DEFINE],
            source_override: Some(b"        lda #$42\n"),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };
        let paths = opforge_native_cli_batch_case_paths(
            Path::new("/tmp/opforge-fsuae"),
            0,
            "challenge",
            "case",
        );
        let native_tokens = opforge_native_cli_case_command(&case, &paths)
            .split_whitespace()
            .map(str::to_string)
            .collect::<Vec<_>>();
        let rust_cli = cli_core::Cli::parse_from([
            "opforge",
            "/tmp/opforge-fsuae/case_0000/input.asm",
            "--bin",
            "/tmp/opforge-fsuae/case_0000/out.bin",
            "--cpu",
            "m6502",
        ]);
        assert!(rust_cli.opasm_package.is_none());
        let rust_tokens = vec![
            "/tmp/opforge-fsuae/case_0000/input.asm".to_string(),
            "--bin".to_string(),
            "/tmp/opforge-fsuae/case_0000/out.bin".to_string(),
            "--cpu".to_string(),
            "m6502".to_string(),
        ];
        assert_eq!(
            normalize_cli_surface_tokens(&native_tokens),
            normalize_cli_surface_tokens(&rust_tokens),
        );
    }

    #[test]
    fn include_root_native_cli_command_matches_rust_cli_arg_surface() {
        use clap::Parser;

        let case = OpforgeNativeCliParityCase {
            name: "include-root-cli-surface",
            cpu_override: "68020",
            extra_assembly_defines: &[FS_UAE_OPFORGE_NATIVE_CLI_ITEM10_INCLUDE_DEFINE],
            source_override: Some(b"        .include \"defs.inc\"\n"),
            command_template: None,
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };
        let paths = opforge_native_cli_batch_case_paths(
            Path::new("/tmp/opforge-fsuae"),
            0,
            "challenge",
            "case",
        );
        let native_tokens = opforge_native_cli_case_command(&case, &paths)
            .split_whitespace()
            .map(str::to_string)
            .collect::<Vec<_>>();
        let rust_cli = cli_core::Cli::parse_from([
            "opforge",
            "/tmp/opforge-fsuae/case_0000/input.asm",
            "--bin",
            "/tmp/opforge-fsuae/case_0000/out.bin",
            "--cpu",
            "6502",
            "-I",
            "/tmp/opforge-fsuae/case_0000/include_root_b",
            "-I",
            "/tmp/opforge-fsuae/case_0000/include_root_a",
        ]);
        assert!(rust_cli.opasm_package.is_none());
        let rust_tokens = vec![
            "/tmp/opforge-fsuae/case_0000/input.asm".to_string(),
            "--bin".to_string(),
            "/tmp/opforge-fsuae/case_0000/out.bin".to_string(),
            "--cpu".to_string(),
            "6502".to_string(),
            "-I".to_string(),
            "/tmp/opforge-fsuae/case_0000/include_root_b".to_string(),
            "-I".to_string(),
            "/tmp/opforge-fsuae/case_0000/include_root_a".to_string(),
        ];
        assert_eq!(
            normalize_cli_surface_tokens(&native_tokens),
            normalize_cli_surface_tokens(&rust_tokens),
        );
    }
}
