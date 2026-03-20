#[path = "common/release_ffi_support.rs"]
mod release_ffi_support;

use ffi::{
    opforge_asm_assemble_file_with_request, opforge_asm_report_error_count,
    opforge_asm_report_free, opforge_asm_report_status, opforge_asm_session_assemble,
    opforge_asm_session_create_with_request, opforge_asm_session_create_with_request_report,
    opforge_asm_session_free, opforge_diag_code_from_asm_report,
    opforge_diag_fixit_applicability_from_asm_report, opforge_diag_fixit_count_from_asm_report,
    opforge_diag_fixit_replacement_from_asm_report, opforge_diag_help_count_from_asm_report,
    opforge_diag_help_from_asm_report, OpforgeAsmDiagnosticsOptions, OpforgeAsmExecutionOptions,
    OpforgeAsmOutputOptions, OpforgeAsmRequest, OpforgeAsmSourceOptions, OpforgeProcessorStatus,
    OpforgeStatus, OpforgeStringList, OPFORGE_DEFAULT_OUTPUTS_DISABLE,
    OPFORGE_DEFAULT_OUTPUTS_ENABLE, OPFORGE_EXECUTION_MODE_LOCKSTEP_RUST,
    OPFORGE_EXECUTION_MODE_LOCKSTEP_VM, OPFORGE_EXECUTION_MODE_RUST, OPFORGE_EXECUTION_MODE_VM,
    OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT, OPFORGE_LABEL_OUTPUT_FORMAT_VICE,
    OPFORGE_OUTPUT_FORMAT_TEXT,
};
use libloading::Library;
use opforge as ffi;
use release_ffi_support::{
    build_release_ffi_cdylib, header_function_names_from_shipped_header, release_ffi_library_path,
};
use std::ffi::{CStr, CString};
use std::fs;
use std::os::raw::c_char;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn make_temp_dir(name: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock before epoch")
        .as_nanos();
    let path = std::env::temp_dir().join(format!(
        "libopforge-ffi-it-{name}-{}-{nanos}",
        std::process::id()
    ));
    fs::create_dir_all(&path).expect("create temp dir");
    path
}

fn invalid_utf8_ptr() -> *const c_char {
    c"\xFF".as_ptr()
}

fn empty_string_list() -> OpforgeStringList {
    OpforgeStringList {
        items: std::ptr::null(),
        count: 0,
    }
}

fn basic_request(
    root_path: *const c_char,
    output_base: *const c_char,
    out_dir: *const c_char,
    execution_mode: u32,
    emit_outputs: u8,
) -> OpforgeAsmRequest {
    let emit_outputs = match emit_outputs {
        0 => OPFORGE_DEFAULT_OUTPUTS_DISABLE,
        1 => OPFORGE_DEFAULT_OUTPUTS_ENABLE,
        value => value,
    };
    OpforgeAsmRequest {
        source: OpforgeAsmSourceOptions {
            root_path,
            output_base,
            defines: empty_string_list(),
            include_paths: empty_string_list(),
            module_paths: empty_string_list(),
            pp_macro_depth: 0,
        },
        execution: OpforgeAsmExecutionOptions {
            execution_mode,
            cpu_override: std::ptr::null(),
            max_loop_iterations: 0,
            opasm_package_path: std::ptr::null(),
        },
        output: OpforgeAsmOutputOptions {
            out_dir,
            emit_outputs,
            output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
            go_addr: std::ptr::null(),
            bin_specs: empty_string_list(),
            fill_byte: 0,
            fill_byte_set: 0,
            labels_file: std::ptr::null(),
            label_output_format: OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT,
            dependency_output_path: std::ptr::null(),
            dependency_append: 0,
            dependency_make_phony: 0,
            outfile_override: std::ptr::null(),
            list_name_override: std::ptr::null(),
            hex_name_override: std::ptr::null(),
            header_title: std::ptr::null(),
            no_outputs: 0,
        },
        diagnostics: OpforgeAsmDiagnosticsOptions {
            debug_conditionals: 0,
            tab_size: 0,
        },
    }
}

fn rustc_host_target() -> String {
    let rustc = std::env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());
    let output = Command::new(rustc)
        .arg("-vV")
        .output()
        .expect("query rustc host target");
    assert!(
        output.status.success(),
        "failed to query rustc host target\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    String::from_utf8_lossy(&output.stdout)
        .lines()
        .find_map(|line| line.strip_prefix("host: "))
        .map(str::to_string)
        .expect("rustc -vV missing host triple")
}

fn resolve_c_compiler() -> cc::Tool {
    let target = rustc_host_target();
    cc::Build::new()
        .cargo_metadata(false)
        .opt_level(0)
        .host(&target)
        .target(&target)
        .try_get_compiler()
        .unwrap_or_else(|error| {
            panic!("no supported C compiler available for ABI contract test: {error}")
        })
}

fn compile_header_abi_check(
    compiler: &cc::Tool,
    source_path: &PathBuf,
    object_path: &PathBuf,
    header_dir: &PathBuf,
) -> std::process::Output {
    let mut command = Command::new(compiler.path());
    command.args(compiler.args());

    if compiler.is_like_msvc() {
        command
            .arg("/nologo")
            .arg("/std:c11")
            .arg(format!("/I{}", header_dir.display()))
            .arg("/c")
            .arg(source_path)
            .arg(format!("/Fo{}", object_path.display()));
    } else {
        command
            .arg("-std=c11")
            .arg("-I")
            .arg(header_dir)
            .arg("-c")
            .arg(source_path)
            .arg("-o")
            .arg(object_path);
    }

    command
        .output()
        .expect("run C compiler for header ABI check")
}

#[test]
fn exported_header_matches_rust_abi_contract() {
    let compiler = resolve_c_compiler();

    let work_dir = make_temp_dir("header-abi");
    let source_path = work_dir.join("header_check.c");
    let object_path = work_dir.join("header_check.o");
    let header_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    let c_source = format!(
        "#include <stddef.h>\n\
         #include \"opforge.h\"\n\
         _Static_assert(OPFORGE_EXECUTION_MODE_RUST == {mode_rust}, \"mode rust mismatch\");\n\
         _Static_assert(OPFORGE_EXECUTION_MODE_VM == {mode_vm}, \"mode vm mismatch\");\n\
         _Static_assert(OPFORGE_EXECUTION_MODE_LOCKSTEP_RUST == {mode_lockstep_rust}, \"mode lockstep rust mismatch\");\n\
         _Static_assert(OPFORGE_EXECUTION_MODE_LOCKSTEP_VM == {mode_lockstep_vm}, \"mode lockstep vm mismatch\");\n\
         _Static_assert(OPFORGE_STATUS_OK == {status_ok}, \"status ok mismatch\");\n\
         _Static_assert(OPFORGE_STATUS_INVALID_REQUEST == {status_invalid}, \"status invalid mismatch\");\n\
         _Static_assert(OPFORGE_STATUS_ASSEMBLE_ERROR == {status_assemble_error}, \"status assemble error mismatch\");\n\
         _Static_assert(OPFORGE_OUTPUT_FORMAT_TEXT == {output_format_text}, \"output format text mismatch\");\n\
         _Static_assert(OPFORGE_OUTPUT_FORMAT_JSON == {output_format_json}, \"output format json mismatch\");\n\
         _Static_assert(OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT == {label_format_default}, \"label format default mismatch\");\n\
         _Static_assert(OPFORGE_LABEL_OUTPUT_FORMAT_VICE == {label_format_vice}, \"label format vice mismatch\");\n\
         _Static_assert(OPFORGE_LABEL_OUTPUT_FORMAT_CTAGS == {label_format_ctags}, \"label format ctags mismatch\");\n\
         _Static_assert(OPFORGE_PROCESSOR_STATUS_OK == {processor_status_ok}, \"processor status ok mismatch\");\n\
         _Static_assert(OPFORGE_PROCESSOR_STATUS_RETURNED == {processor_status_returned}, \"processor status returned mismatch\");\n\
         _Static_assert(OPFORGE_EXPR_BINARY == {expr_binary_kind}, \"expr binary kind mismatch\");\n\
         _Static_assert(OPFORGE_LINE_USE == {line_use_kind}, \"line use kind mismatch\");\n\
         _Static_assert(sizeof(opforge_asm_request) == {asm_request_size}, \"asm request size mismatch\");\n\
         _Static_assert(_Alignof(opforge_asm_request) == {asm_request_align}, \"asm request align mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_request, source) == {asm_request_source_offset}, \"asm request source offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_request, execution) == {asm_request_execution_offset}, \"asm request execution offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_request, output) == {asm_request_output_offset}, \"asm request output offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_request, diagnostics) == {asm_request_diagnostics_offset}, \"asm request diagnostics offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_source_options, output_base) == {asm_source_options_output_base_offset}, \"asm source options output_base offset mismatch\");\n\
         _Static_assert(sizeof(opforge_asm_execution_options) == {asm_execution_options_size}, \"asm execution options size mismatch\");\n\
         _Static_assert(_Alignof(opforge_asm_execution_options) == {asm_execution_options_align}, \"asm execution options align mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_execution_options, execution_mode) == {asm_execution_options_execution_mode_offset}, \"asm execution options execution_mode offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_execution_options, cpu_override) == {asm_execution_options_cpu_override_offset}, \"asm execution options cpu_override offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_execution_options, max_loop_iterations) == {asm_execution_options_max_loop_iterations_offset}, \"asm execution options max_loop_iterations offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_execution_options, opasm_package_path) == {asm_execution_options_opasm_package_path_offset}, \"asm execution options opasm_package_path offset mismatch\");\n\
         _Static_assert(sizeof(opforge_asm_output_options) == {asm_output_options_size}, \"asm output options size mismatch\");\n\
         _Static_assert(_Alignof(opforge_asm_output_options) == {asm_output_options_align}, \"asm output options align mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_output_options, out_dir) == {asm_output_options_out_dir_offset}, \"asm output options out_dir offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_output_options, output_format) == {asm_output_options_output_format_offset}, \"asm output options output_format offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_output_options, go_addr) == {asm_output_options_go_addr_offset}, \"asm output options go_addr offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_output_options, bin_specs) == {asm_output_options_bin_specs_offset}, \"asm output options bin_specs offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_output_options, fill_byte) == {asm_output_options_fill_byte_offset}, \"asm output options fill_byte offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_output_options, fill_byte_set) == {asm_output_options_fill_byte_set_offset}, \"asm output options fill_byte_set offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_output_options, labels_file) == {asm_output_options_labels_file_offset}, \"asm output options labels_file offset mismatch\");\n\
         _Static_assert(offsetof(opforge_asm_output_options, no_outputs) == {asm_output_options_no_outputs_offset}, \"asm output options no_outputs offset mismatch\");\n\
         /* Public high-level assembler entrypoints: keep this block aligned with opforge.h. */\n\
         static void (*asm_request_init_fn)(opforge_asm_request *) = opforge_asm_request_init;\n\
         static opforge_asm_report *(*assemble_with_request_fn)(const opforge_asm_request *) = opforge_asm_assemble_file_with_request;\n\
         static opforge_asm_report *(*assemble_memory_with_request_fn)(const opforge_asm_request *, const char *, const opforge_output_callbacks *) = opforge_asm_assemble_memory_with_request;\n\
         static opforge_asm_report *(*check_memory_with_request_fn)(const opforge_asm_request *, const char *, const opforge_output_callbacks *) = opforge_asm_check_memory_with_request;\n\
         static opforge_asm_session *(*asm_session_create_with_request_fn)(const opforge_asm_request *) = opforge_asm_session_create_with_request;\n\
         static opforge_asm_report *(*asm_session_create_with_request_report_fn)(const opforge_asm_request *, opforge_asm_session **) = opforge_asm_session_create_with_request_report;\n\
         static opforge_prepared_asm_session *(*asm_session_prepare_fn)(const opforge_asm_session *) = opforge_asm_session_prepare;\n\
         static opforge_asm_report *(*asm_session_assemble_fn)(const opforge_asm_session *) = opforge_asm_session_assemble;\n\
         static opforge_asm_report *(*asm_session_check_fn)(const opforge_asm_session *) = opforge_asm_session_check;\n\
         static opforge_asm_report *(*prepared_asm_session_assemble_fn)(const opforge_prepared_asm_session *) = opforge_prepared_asm_session_assemble;\n\
         static opforge_asm_report *(*prepared_asm_session_check_fn)(const opforge_prepared_asm_session *) = opforge_prepared_asm_session_check;\n\
         static void (*asm_session_free_fn)(opforge_asm_session *) = opforge_asm_session_free;\n\
         static void (*prepared_asm_session_free_fn)(opforge_prepared_asm_session *) = opforge_prepared_asm_session_free;\n\
         static size_t (*diag_count_fn)(const opforge_asm_report *) = opforge_diag_count_from_asm_report;\n\
         static opforge_diagnostic_severity (*diag_severity_fn)(const opforge_asm_report *, size_t) = opforge_diag_severity_from_asm_report;\n\
         static uint32_t (*diag_line_fn)(const opforge_asm_report *, size_t) = opforge_diag_line_from_asm_report;\n\
         static size_t (*diag_col_start_fn)(const opforge_asm_report *, size_t) = opforge_diag_column_from_asm_report;\n\
         static size_t (*diag_col_end_fn)(const opforge_asm_report *, size_t) = opforge_diag_col_end_from_asm_report;\n\
         static const char *(*diag_message_fn)(const opforge_asm_report *, size_t) = opforge_diag_message_from_asm_report;\n\
         static const char *(*diag_code_fn)(const opforge_asm_report *, size_t) = opforge_diag_code_from_asm_report;\n\
         static const char *(*diag_file_fn)(const opforge_asm_report *, size_t) = opforge_diag_file_from_asm_report;\n\
         static size_t (*diag_related_span_count_fn)(const opforge_asm_report *, size_t) = opforge_diag_related_span_count_from_asm_report;\n\
         static const char *(*diag_related_span_file_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_related_span_file_from_asm_report;\n\
         static uint32_t (*diag_related_span_line_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_related_span_line_from_asm_report;\n\
         static size_t (*diag_related_span_col_start_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_related_span_col_start_from_asm_report;\n\
         static size_t (*diag_related_span_col_end_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_related_span_col_end_from_asm_report;\n\
         static const char *(*diag_related_span_label_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_related_span_label_from_asm_report;\n\
         static uint8_t (*diag_related_span_is_primary_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_related_span_is_primary_from_asm_report;\n\
         static size_t (*diag_note_count_fn)(const opforge_asm_report *, size_t) = opforge_diag_note_count_from_asm_report;\n\
         static const char *(*diag_note_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_note_from_asm_report;\n\
         static size_t (*diag_help_count_fn)(const opforge_asm_report *, size_t) = opforge_diag_help_count_from_asm_report;\n\
         static const char *(*diag_help_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_help_from_asm_report;\n\
         static size_t (*diag_fixit_count_fn)(const opforge_asm_report *, size_t) = opforge_diag_fixit_count_from_asm_report;\n\
         static const char *(*diag_fixit_file_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_fixit_file_from_asm_report;\n\
         static uint32_t (*diag_fixit_line_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_fixit_line_from_asm_report;\n\
         static size_t (*diag_fixit_col_start_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_fixit_col_start_from_asm_report;\n\
         static size_t (*diag_fixit_col_end_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_fixit_col_end_from_asm_report;\n\
         static const char *(*diag_fixit_replacement_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_fixit_replacement_from_asm_report;\n\
         static const char *(*diag_fixit_applicability_fn)(const opforge_asm_report *, size_t, size_t) = opforge_diag_fixit_applicability_from_asm_report;\n\
         static opforge_registry *(*registry_default_fn)(void) = opforge_registry_default;\n\
         static size_t (*registry_cpu_count_fn)(const opforge_registry *) = opforge_registry_cpu_count;\n\
         static const char *(*registry_cpu_id_fn)(const opforge_registry *, size_t) = opforge_registry_cpu_id;\n\
         static opforge_registry_cpu_view *(*registry_cpu_view_fn)(const opforge_registry *, const char *) = opforge_registry_cpu_view_lookup;\n\
         static const char *(*registry_cpu_view_family_fn)(const opforge_registry_cpu_view *) = opforge_registry_cpu_view_family_id;\n\
         static size_t (*registry_cpu_view_mnemonic_count_fn)(const opforge_registry_cpu_view *) = opforge_registry_cpu_view_mnemonic_count;\n\
         static const char *(*registry_cpu_view_mnemonic_fn)(const opforge_registry_cpu_view *, size_t) = opforge_registry_cpu_view_mnemonic;\n\
         static void (*registry_free_fn)(opforge_registry *) = opforge_registry_free;\n\
         static void (*registry_cpu_view_free_fn)(opforge_registry_cpu_view *) = opforge_registry_cpu_view_free;\n\
         static opforge_opcore_tokenize_report *(*opcore_tokenize_fn)(const char *, uint32_t) = opforge_opcore_tokenize_line;\n\
         static opforge_processor_status (*opcore_tokenize_status_fn)(const opforge_opcore_tokenize_report *) = opforge_opcore_tokenize_report_status;\n\
         static size_t (*opcore_tokenize_count_fn)(const opforge_opcore_tokenize_report *) = opforge_opcore_tokenize_report_token_count;\n\
         static opforge_token_kind (*opcore_tokenize_kind_fn)(const opforge_opcore_tokenize_report *, size_t) = opforge_opcore_tokenize_report_token_kind;\n\
         static const char *(*opcore_tokenize_text_fn)(const opforge_opcore_tokenize_report *, size_t) = opforge_opcore_tokenize_report_token_text;\n\
         static uint32_t (*opcore_tokenize_line_fn)(const opforge_opcore_tokenize_report *, size_t) = opforge_opcore_tokenize_report_token_line;\n\
         static size_t (*opcore_tokenize_col_start_fn)(const opforge_opcore_tokenize_report *, size_t) = opforge_opcore_tokenize_report_token_col_start;\n\
         static size_t (*opcore_tokenize_col_end_fn)(const opforge_opcore_tokenize_report *, size_t) = opforge_opcore_tokenize_report_token_col_end;\n\
         static const char *(*opcore_tokenize_error_message_fn)(const opforge_opcore_tokenize_report *) = opforge_opcore_tokenize_report_error_message;\n\
         static uint32_t (*opcore_tokenize_error_line_fn)(const opforge_opcore_tokenize_report *) = opforge_opcore_tokenize_report_error_line;\n\
         static size_t (*opcore_tokenize_error_col_start_fn)(const opforge_opcore_tokenize_report *) = opforge_opcore_tokenize_report_error_col_start;\n\
         static size_t (*opcore_tokenize_error_col_end_fn)(const opforge_opcore_tokenize_report *) = opforge_opcore_tokenize_report_error_col_end;\n\
         static void (*opcore_tokenize_free_fn)(opforge_opcore_tokenize_report *) = opforge_opcore_tokenize_report_free;\n\
         static opforge_opasm_tokenize_report *(*opasm_tokenize_fn)(const char *, uint32_t) = opforge_opasm_tokenize_statement;\n\
         static opforge_processor_status (*opasm_tokenize_status_fn)(const opforge_opasm_tokenize_report *) = opforge_opasm_tokenize_report_status;\n\
         static size_t (*opasm_tokenize_count_fn)(const opforge_opasm_tokenize_report *) = opforge_opasm_tokenize_report_token_count;\n\
         static opforge_token_kind (*opasm_tokenize_kind_fn)(const opforge_opasm_tokenize_report *, size_t) = opforge_opasm_tokenize_report_token_kind;\n\
         static const char *(*opasm_tokenize_text_fn)(const opforge_opasm_tokenize_report *, size_t) = opforge_opasm_tokenize_report_token_text;\n\
         static uint32_t (*opasm_tokenize_line_fn)(const opforge_opasm_tokenize_report *, size_t) = opforge_opasm_tokenize_report_token_line;\n\
         static size_t (*opasm_tokenize_col_start_fn)(const opforge_opasm_tokenize_report *, size_t) = opforge_opasm_tokenize_report_token_col_start;\n\
         static size_t (*opasm_tokenize_col_end_fn)(const opforge_opasm_tokenize_report *, size_t) = opforge_opasm_tokenize_report_token_col_end;\n\
         static const char *(*opasm_tokenize_error_message_fn)(const opforge_opasm_tokenize_report *) = opforge_opasm_tokenize_report_error_message;\n\
         static uint32_t (*opasm_tokenize_error_line_fn)(const opforge_opasm_tokenize_report *) = opforge_opasm_tokenize_report_error_line;\n\
         static size_t (*opasm_tokenize_error_col_start_fn)(const opforge_opasm_tokenize_report *) = opforge_opasm_tokenize_report_error_col_start;\n\
         static size_t (*opasm_tokenize_error_col_end_fn)(const opforge_opasm_tokenize_report *) = opforge_opasm_tokenize_report_error_col_end;\n\
         static void (*opasm_tokenize_free_fn)(opforge_opasm_tokenize_report *) = opforge_opasm_tokenize_report_free;\n\
         static opforge_opasm_parse_report *(*opasm_parse_fn)(const char *, uint32_t) = opforge_opasm_parse_statement;\n\
         static void (*opasm_parse_free_fn)(opforge_opasm_parse_report *) = opforge_opasm_parse_report_free;\n\
         static opforge_opasm_process_report *(*opasm_process_fn)(const opforge_opasm_process_config *) = opforge_opasm_process_statement;\n\
         static opforge_processing_trace *(*opasm_process_trace_fn)(const opforge_opasm_process_report *) = opforge_opasm_process_report_processing_trace;\n\
         static size_t (*processing_trace_count_fn)(const opforge_processing_trace *) = opforge_processing_trace_request_count;\n\
         static const char *(*processing_trace_text_fn)(const opforge_processing_trace *, size_t) = opforge_processing_trace_request_text;\n\
         static void (*processing_trace_free_fn)(opforge_processing_trace *) = opforge_processing_trace_free;\n\
         static opforge_lockstep_report *(*opasm_process_lockstep_fn)(const opforge_opasm_process_report *) = opforge_opasm_process_report_lockstep_report;\n\
         static size_t (*lockstep_match_count_fn)(const opforge_lockstep_report *) = opforge_lockstep_report_match_count;\n\
         static size_t (*lockstep_divergence_count_fn)(const opforge_lockstep_report *) = opforge_lockstep_report_divergence_count;\n\
         static const char *(*lockstep_match_stage_fn)(const opforge_lockstep_report *, size_t) = opforge_lockstep_report_match_stage_text;\n\
         static const char *(*lockstep_match_request_fn)(const opforge_lockstep_report *, size_t) = opforge_lockstep_report_match_request_text;\n\
         static const char *(*lockstep_divergence_reason_fn)(const opforge_lockstep_report *, size_t) = opforge_lockstep_report_divergence_reason_code;\n\
         static void (*lockstep_report_free_fn)(opforge_lockstep_report *) = opforge_lockstep_report_free;\n\
         static void (*opasm_process_free_fn)(opforge_opasm_process_report *) = opforge_opasm_process_report_free;\n\
         static opforge_opcore_expr_report *(*opcore_expr_fn)(const char *, uint32_t) = opforge_opcore_parse_expression;\n\
         static void (*opcore_expr_free_fn)(opforge_opcore_expr_report *) = opforge_opcore_expr_report_free;\n\
         static opforge_opcore_module_item_report *(*opcore_module_item_fn)(const char *, uint32_t) = opforge_opcore_process_module_item;\n\
         static void (*opcore_module_item_free_fn)(opforge_opcore_module_item_report *) = opforge_opcore_module_item_report_free;\n\
         int main(void) {{ return asm_request_init_fn != 0 && assemble_with_request_fn != 0 && assemble_memory_with_request_fn != 0 && check_memory_with_request_fn != 0 && asm_session_create_with_request_fn != 0 && asm_session_create_with_request_report_fn != 0 && asm_session_prepare_fn != 0 && asm_session_assemble_fn != 0 && asm_session_check_fn != 0 && prepared_asm_session_assemble_fn != 0 && prepared_asm_session_check_fn != 0 && asm_session_free_fn != 0 && prepared_asm_session_free_fn != 0 && diag_count_fn != 0 && diag_severity_fn != 0 && diag_line_fn != 0 && diag_col_start_fn != 0 && diag_col_end_fn != 0 && diag_message_fn != 0 && diag_code_fn != 0 && diag_file_fn != 0 && diag_related_span_count_fn != 0 && diag_related_span_file_fn != 0 && diag_related_span_line_fn != 0 && diag_related_span_col_start_fn != 0 && diag_related_span_col_end_fn != 0 && diag_related_span_label_fn != 0 && diag_related_span_is_primary_fn != 0 && diag_note_count_fn != 0 && diag_note_fn != 0 && diag_help_count_fn != 0 && diag_help_fn != 0 && diag_fixit_count_fn != 0 && diag_fixit_file_fn != 0 && diag_fixit_line_fn != 0 && diag_fixit_col_start_fn != 0 && diag_fixit_col_end_fn != 0 && diag_fixit_replacement_fn != 0 && diag_fixit_applicability_fn != 0 && registry_default_fn != 0 && registry_cpu_count_fn != 0 && registry_cpu_id_fn != 0 && registry_cpu_view_fn != 0 && registry_cpu_view_family_fn != 0 && registry_cpu_view_mnemonic_count_fn != 0 && registry_cpu_view_mnemonic_fn != 0 && registry_free_fn != 0 && registry_cpu_view_free_fn != 0 && opcore_tokenize_fn != 0 && opcore_tokenize_status_fn != 0 && opcore_tokenize_count_fn != 0 && opcore_tokenize_kind_fn != 0 && opcore_tokenize_text_fn != 0 && opcore_tokenize_line_fn != 0 && opcore_tokenize_col_start_fn != 0 && opcore_tokenize_col_end_fn != 0 && opcore_tokenize_error_message_fn != 0 && opcore_tokenize_error_line_fn != 0 && opcore_tokenize_error_col_start_fn != 0 && opcore_tokenize_error_col_end_fn != 0 && opcore_tokenize_free_fn != 0 && opasm_tokenize_fn != 0 && opasm_tokenize_status_fn != 0 && opasm_tokenize_count_fn != 0 && opasm_tokenize_kind_fn != 0 && opasm_tokenize_text_fn != 0 && opasm_tokenize_line_fn != 0 && opasm_tokenize_col_start_fn != 0 && opasm_tokenize_col_end_fn != 0 && opasm_tokenize_error_message_fn != 0 && opasm_tokenize_error_line_fn != 0 && opasm_tokenize_error_col_start_fn != 0 && opasm_tokenize_error_col_end_fn != 0 && opasm_tokenize_free_fn != 0 && opasm_parse_fn != 0 && opasm_parse_free_fn != 0 && opasm_process_fn != 0 && opasm_process_trace_fn != 0 && processing_trace_count_fn != 0 && processing_trace_text_fn != 0 && processing_trace_free_fn != 0 && opasm_process_lockstep_fn != 0 && lockstep_match_count_fn != 0 && lockstep_divergence_count_fn != 0 && lockstep_match_stage_fn != 0 && lockstep_match_request_fn != 0 && lockstep_divergence_reason_fn != 0 && lockstep_report_free_fn != 0 && opasm_process_free_fn != 0 && opcore_expr_fn != 0 && opcore_expr_free_fn != 0 && opcore_module_item_fn != 0 && opcore_module_item_free_fn != 0 ? 0 : 1; }}\n",
        mode_rust = OPFORGE_EXECUTION_MODE_RUST,
        mode_vm = OPFORGE_EXECUTION_MODE_VM,
        mode_lockstep_rust = OPFORGE_EXECUTION_MODE_LOCKSTEP_RUST,
        mode_lockstep_vm = OPFORGE_EXECUTION_MODE_LOCKSTEP_VM,
        status_ok = OpforgeStatus::Ok as u32,
        status_invalid = OpforgeStatus::InvalidRequest as u32,
        status_assemble_error = OpforgeStatus::AssembleError as u32,
        output_format_text = OPFORGE_OUTPUT_FORMAT_TEXT,
        output_format_json = 1,
        label_format_default = 0,
        label_format_vice = OPFORGE_LABEL_OUTPUT_FORMAT_VICE,
        label_format_ctags = 2,
        processor_status_ok = OpforgeProcessorStatus::Ok as u32,
        processor_status_returned = OpforgeProcessorStatus::Returned as u32,
        expr_binary_kind = 18,
        line_use_kind = 4,
        asm_request_size = std::mem::size_of::<OpforgeAsmRequest>(),
        asm_request_align = std::mem::align_of::<OpforgeAsmRequest>(),
        asm_request_source_offset = std::mem::offset_of!(OpforgeAsmRequest, source),
        asm_request_execution_offset = std::mem::offset_of!(OpforgeAsmRequest, execution),
        asm_request_output_offset = std::mem::offset_of!(OpforgeAsmRequest, output),
        asm_request_diagnostics_offset = std::mem::offset_of!(OpforgeAsmRequest, diagnostics),
        asm_source_options_output_base_offset = std::mem::offset_of!(OpforgeAsmSourceOptions, output_base),
        asm_execution_options_size = std::mem::size_of::<OpforgeAsmExecutionOptions>(),
        asm_execution_options_align = std::mem::align_of::<OpforgeAsmExecutionOptions>(),
        asm_execution_options_execution_mode_offset = std::mem::offset_of!(OpforgeAsmExecutionOptions, execution_mode),
        asm_execution_options_cpu_override_offset = std::mem::offset_of!(OpforgeAsmExecutionOptions, cpu_override),
        asm_execution_options_max_loop_iterations_offset = std::mem::offset_of!(OpforgeAsmExecutionOptions, max_loop_iterations),
        asm_execution_options_opasm_package_path_offset = std::mem::offset_of!(OpforgeAsmExecutionOptions, opasm_package_path),
        asm_output_options_size = std::mem::size_of::<OpforgeAsmOutputOptions>(),
        asm_output_options_align = std::mem::align_of::<OpforgeAsmOutputOptions>(),
        asm_output_options_out_dir_offset = std::mem::offset_of!(OpforgeAsmOutputOptions, out_dir),
        asm_output_options_output_format_offset = std::mem::offset_of!(OpforgeAsmOutputOptions, output_format),
        asm_output_options_go_addr_offset = std::mem::offset_of!(OpforgeAsmOutputOptions, go_addr),
        asm_output_options_bin_specs_offset = std::mem::offset_of!(OpforgeAsmOutputOptions, bin_specs),
        asm_output_options_fill_byte_offset = std::mem::offset_of!(OpforgeAsmOutputOptions, fill_byte),
        asm_output_options_fill_byte_set_offset = std::mem::offset_of!(OpforgeAsmOutputOptions, fill_byte_set),
        asm_output_options_labels_file_offset = std::mem::offset_of!(OpforgeAsmOutputOptions, labels_file),
        asm_output_options_no_outputs_offset = std::mem::offset_of!(OpforgeAsmOutputOptions, no_outputs),
    );

    fs::write(&source_path, c_source).expect("write header ABI source");

    let output = compile_header_abi_check(&compiler, &source_path, &object_path, &header_dir);

    assert!(
        output.status.success(),
        "header ABI compile check failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn exported_release_ffi_library_exposes_full_header_symbol_surface() {
    build_release_ffi_cdylib(false);

    let library_path = release_ffi_library_path();
    let symbol_names = header_function_names_from_shipped_header();
    let mut missing = Vec::new();

    unsafe {
        let library = Library::new(&library_path).expect("load release-ffi cdylib");
        for symbol_name in &symbol_names {
            let mut symbol = symbol_name.as_bytes().to_vec();
            symbol.push(0);
            if library.get::<*const ()>(&symbol).is_err() {
                missing.push(symbol_name.clone());
            }
        }
    }

    assert!(
        missing.is_empty(),
        "release-ffi cdylib is missing exported C symbols declared in opforge.h: {missing:?}"
    );
}

#[test]
fn exported_header_documents_string_ownership_contract() {
    let header_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("opforge.h");
    let header = fs::read_to_string(&header_path).expect("read opforge.h");

    assert!(
        header.contains(
            "Returned string is borrowed from opforge_asm_report and remains valid until\n * opforge_asm_report_free(report). Do not free it."
        ),
        "header must keep the borrowed report-message ownership note"
    );
    assert!(
        header.contains(
            "Returned strings are borrowed from opforge_registry or\n * opforge_registry_cpu_view and remain valid until that owner is freed."
        ),
        "header must keep the registry borrowed-string ownership note"
    );
    assert!(
        header.contains(
            "Returned strings are borrowed from opforge_processing_trace and remain valid\n * until that owner is freed."
        ),
        "header must keep the processing-trace borrowed-string ownership note"
    );
    assert!(
        header.contains(
            "Returned strings are borrowed from opforge_lockstep_report and remain valid\n * until that owner is freed."
        ),
        "header must keep the lockstep borrowed-string ownership note"
    );
}

#[test]
fn exported_boundary_assembles_with_scalar_execution_mode() {
    let work_dir = make_temp_dir("smoke");
    let out_dir = work_dir.join("out");
    fs::create_dir_all(&out_dir).expect("create out dir");
    let source_path = work_dir.join("main.asm");
    fs::write(&source_path, ".module main\nstart:\n    nop\n.endmodule\n").expect("write source");

    let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
    let out = CString::new(out_dir.to_string_lossy().as_bytes()).expect("out cstr");
    let request = basic_request(
        root.as_ptr(),
        std::ptr::null(),
        out.as_ptr(),
        OPFORGE_EXECUTION_MODE_LOCKSTEP_VM,
        1,
    );

    let report = unsafe { opforge_asm_assemble_file_with_request(&request) };
    assert!(!report.is_null());
    assert_eq!(
        unsafe { opforge_asm_report_status(report) },
        OpforgeStatus::Ok
    );
    assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
    assert!(out_dir.join("main.lst").exists());
    unsafe { opforge_asm_report_free(report) };
}

#[test]
fn exported_grouped_request_session_path_supports_richer_config_surface() {
    let work_dir = make_temp_dir("grouped-request-session");
    let src_dir = work_dir.join("src");
    let include_dir = work_dir.join("include");
    let module_dir = work_dir.join("modules");
    let out_dir = work_dir.join("out");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::create_dir_all(&include_dir).expect("create include dir");
    fs::create_dir_all(&module_dir).expect("create module dir");
    fs::create_dir_all(&out_dir).expect("create out dir");

    let source_path = src_dir.join("main.asm");
    fs::write(
        &source_path,
        ".module main\n.include \"inc.asm\"\n.use dep (VALUE)\nstart:\n    .byte FROM_INC\n    .byte VALUE\n.endmodule\n",
    )
    .expect("write source");
    fs::write(include_dir.join("inc.asm"), "FROM_INC .const 1\n").expect("write include");
    fs::write(
        module_dir.join("dep.asm"),
        ".module dep\n.pub\nVALUE .const 7\n.priv\n.endmodule\n",
    )
    .expect("write module");

    let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
    let input_base_owned = source_path.with_extension("");
    let input_base =
        CString::new(input_base_owned.to_string_lossy().as_bytes()).expect("input base cstr");
    let out_dir_c = CString::new(out_dir.to_string_lossy().as_bytes()).expect("out dir cstr");
    let labels_path = out_dir.join("symbols.lbl");
    let labels_path_c =
        CString::new(labels_path.to_string_lossy().as_bytes()).expect("labels path cstr");
    let deps_path = out_dir.join("main.d");
    let deps_path_c = CString::new(deps_path.to_string_lossy().as_bytes()).expect("deps cstr");
    let cpu_override = CString::new("8085").expect("cpu override cstr");
    let header_title = CString::new("ffi grouped request").expect("header title cstr");

    let define_storage = [CString::new("FLAG=1").expect("define cstr")];
    let define_ptrs: Vec<*const c_char> = define_storage.iter().map(|item| item.as_ptr()).collect();
    let include_storage =
        [CString::new(include_dir.to_string_lossy().as_bytes()).expect("include dir cstr")];
    let include_ptrs: Vec<*const c_char> =
        include_storage.iter().map(|item| item.as_ptr()).collect();
    let module_storage =
        [CString::new(module_dir.to_string_lossy().as_bytes()).expect("module dir cstr")];
    let module_ptrs: Vec<*const c_char> = module_storage.iter().map(|item| item.as_ptr()).collect();

    let request = OpforgeAsmRequest {
        source: OpforgeAsmSourceOptions {
            root_path: root.as_ptr(),
            output_base: input_base.as_ptr(),
            defines: OpforgeStringList {
                items: define_ptrs.as_ptr(),
                count: define_ptrs.len(),
            },
            include_paths: OpforgeStringList {
                items: include_ptrs.as_ptr(),
                count: include_ptrs.len(),
            },
            module_paths: OpforgeStringList {
                items: module_ptrs.as_ptr(),
                count: module_ptrs.len(),
            },
            pp_macro_depth: 64,
        },
        execution: OpforgeAsmExecutionOptions {
            execution_mode: OPFORGE_EXECUTION_MODE_VM,
            cpu_override: cpu_override.as_ptr(),
            max_loop_iterations: 123,
            opasm_package_path: std::ptr::null(),
        },
        output: OpforgeAsmOutputOptions {
            out_dir: out_dir_c.as_ptr(),
            emit_outputs: OPFORGE_DEFAULT_OUTPUTS_ENABLE,
            output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
            go_addr: std::ptr::null(),
            bin_specs: OpforgeStringList {
                items: std::ptr::null(),
                count: 0,
            },
            fill_byte: 0,
            fill_byte_set: 0,
            labels_file: labels_path_c.as_ptr(),
            label_output_format: OPFORGE_LABEL_OUTPUT_FORMAT_VICE,
            dependency_output_path: deps_path_c.as_ptr(),
            dependency_append: 0,
            dependency_make_phony: 0,
            outfile_override: std::ptr::null(),
            list_name_override: std::ptr::null(),
            hex_name_override: std::ptr::null(),
            header_title: header_title.as_ptr(),
            no_outputs: 0,
        },
        diagnostics: OpforgeAsmDiagnosticsOptions {
            debug_conditionals: 1,
            tab_size: 8,
        },
    };

    let session = unsafe { opforge_asm_session_create_with_request(&request) };
    assert!(!session.is_null());
    let report = unsafe { opforge_asm_session_assemble(session) };
    assert!(!report.is_null());
    assert_eq!(
        unsafe { opforge_asm_report_status(report) },
        OpforgeStatus::Ok
    );
    assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);

    let listing_text = fs::read_to_string(out_dir.join("main.lst")).expect("read listing");
    assert!(
        listing_text.contains("FROM_INC"),
        "listing:\n{listing_text}"
    );
    assert!(out_dir.join("main.hex").exists());
    let labels_text = fs::read_to_string(&labels_path).expect("read labels");
    assert!(labels_text.contains("main.start"), "labels:\n{labels_text}");
    let deps_text = fs::read_to_string(&deps_path).expect("read deps");
    assert!(deps_text.contains(source_path.to_string_lossy().as_ref()));
    assert!(deps_text.contains(include_dir.join("inc.asm").to_string_lossy().as_ref()));
    assert!(deps_text.contains(module_dir.join("dep.asm").to_string_lossy().as_ref()));

    unsafe { opforge_asm_report_free(report) };
    unsafe { opforge_asm_session_free(session) };
}

#[test]
fn exported_grouped_request_session_create_returns_null_on_invalid_request() {
    assert!(unsafe { opforge_asm_session_create_with_request(std::ptr::null()) }.is_null());

    let request = basic_request(
        std::ptr::null(),
        std::ptr::null(),
        std::ptr::null(),
        OPFORGE_EXECUTION_MODE_VM,
        0,
    );
    let session = unsafe { opforge_asm_session_create_with_request(&request) };
    assert!(session.is_null());
}

#[test]
fn exported_grouped_request_session_create_report_exposes_failures_and_success() {
    let mut session = std::ptr::null_mut();
    let report =
        unsafe { opforge_asm_session_create_with_request_report(std::ptr::null(), &mut session) };
    assert!(!report.is_null());
    assert!(session.is_null());
    assert_eq!(
        unsafe { opforge_asm_report_status(report) },
        OpforgeStatus::InvalidRequest
    );
    unsafe { opforge_asm_report_free(report) };

    let work_dir = make_temp_dir("grouped-request-session-report");
    let source_path = work_dir.join("main.asm");
    fs::write(&source_path, ".module main\nstart:\n    nop\n.endmodule\n").expect("write source");
    let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
    let output_base_owned = source_path.with_extension("");
    let output_base =
        CString::new(output_base_owned.to_string_lossy().as_bytes()).expect("output base cstr");
    let request = basic_request(
        root.as_ptr(),
        output_base.as_ptr(),
        std::ptr::null(),
        OPFORGE_EXECUTION_MODE_VM,
        0,
    );

    let mut session = std::ptr::null_mut();
    let report = unsafe { opforge_asm_session_create_with_request_report(&request, &mut session) };
    assert!(!report.is_null());
    assert!(!session.is_null());
    assert_eq!(
        unsafe { opforge_asm_report_status(report) },
        OpforgeStatus::Ok
    );
    unsafe { opforge_asm_report_free(report) };

    let assemble_report = unsafe { opforge_asm_session_assemble(session) };
    assert!(!assemble_report.is_null());
    assert_eq!(
        unsafe { opforge_asm_report_status(assemble_report) },
        OpforgeStatus::Ok
    );
    unsafe { opforge_asm_report_free(assemble_report) };
    unsafe { opforge_asm_session_free(session) };
}

#[test]
fn exported_grouped_request_path_exposes_rich_diagnostics() {
    let work_dir = make_temp_dir("grouped-request-diagnostics");
    let source_path = work_dir.join("broken.asm");
    fs::write(&source_path, ".module main\nLD A,\n.endmodule\n").expect("write source");

    let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
    let input_base_owned = source_path.with_extension("");
    let input_base =
        CString::new(input_base_owned.to_string_lossy().as_bytes()).expect("input base cstr");
    let cpu_override = CString::new("8085").expect("cpu override cstr");

    let request = OpforgeAsmRequest {
        source: OpforgeAsmSourceOptions {
            root_path: root.as_ptr(),
            output_base: input_base.as_ptr(),
            defines: OpforgeStringList {
                items: std::ptr::null(),
                count: 0,
            },
            include_paths: OpforgeStringList {
                items: std::ptr::null(),
                count: 0,
            },
            module_paths: OpforgeStringList {
                items: std::ptr::null(),
                count: 0,
            },
            pp_macro_depth: 0,
        },
        execution: OpforgeAsmExecutionOptions {
            execution_mode: OPFORGE_EXECUTION_MODE_VM,
            cpu_override: cpu_override.as_ptr(),
            max_loop_iterations: 0,
            opasm_package_path: std::ptr::null(),
        },
        output: OpforgeAsmOutputOptions {
            out_dir: std::ptr::null(),
            emit_outputs: 0,
            output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
            go_addr: std::ptr::null(),
            bin_specs: OpforgeStringList {
                items: std::ptr::null(),
                count: 0,
            },
            fill_byte: 0,
            fill_byte_set: 0,
            labels_file: std::ptr::null(),
            label_output_format: OPFORGE_LABEL_OUTPUT_FORMAT_VICE,
            dependency_output_path: std::ptr::null(),
            dependency_append: 0,
            dependency_make_phony: 0,
            outfile_override: std::ptr::null(),
            list_name_override: std::ptr::null(),
            hex_name_override: std::ptr::null(),
            header_title: std::ptr::null(),
            no_outputs: 0,
        },
        diagnostics: OpforgeAsmDiagnosticsOptions {
            debug_conditionals: 1,
            tab_size: 8,
        },
    };

    let report = unsafe { opforge_asm_assemble_file_with_request(&request) };
    assert!(!report.is_null());
    assert_eq!(
        unsafe { opforge_asm_report_status(report) },
        OpforgeStatus::AssembleError
    );
    assert!(unsafe { opforge_asm_report_error_count(report) } > 0);

    let code = unsafe { CStr::from_ptr(opforge_diag_code_from_asm_report(report, 0)) }
        .to_str()
        .expect("diag code utf8");
    assert!(!code.is_empty());

    let help_count = unsafe { opforge_diag_help_count_from_asm_report(report, 0) };
    assert!(help_count > 0);
    let help = unsafe { CStr::from_ptr(opforge_diag_help_from_asm_report(report, 0, 0)) }
        .to_str()
        .expect("diag help utf8");
    assert!(help.contains("Z80 dialect"), "unexpected help: {help}");

    let fixit_count = unsafe { opforge_diag_fixit_count_from_asm_report(report, 0) };
    assert!(fixit_count > 0);
    let replacement =
        unsafe { CStr::from_ptr(opforge_diag_fixit_replacement_from_asm_report(report, 0, 0)) }
            .to_str()
            .expect("fixit replacement utf8");
    assert_eq!(replacement, "MOV");
    let applicability = unsafe {
        CStr::from_ptr(opforge_diag_fixit_applicability_from_asm_report(
            report, 0, 0,
        ))
    }
    .to_str()
    .expect("fixit applicability utf8");
    assert_eq!(applicability, "maybe-incorrect");

    unsafe { opforge_asm_report_free(report) };
}

#[test]
fn exported_boundary_rejects_invalid_requests() {
    let null_request = unsafe { opforge_asm_assemble_file_with_request(std::ptr::null()) };
    assert!(!null_request.is_null());
    assert_eq!(
        unsafe { opforge_asm_report_status(null_request) },
        OpforgeStatus::InvalidRequest
    );
    let null_request_message =
        unsafe { CStr::from_ptr(ffi::opforge_asm_report_message(null_request)) }
            .to_str()
            .expect("ffi invalid request message utf8");
    assert!(null_request_message.contains("request"));
    unsafe { opforge_asm_report_free(null_request) };

    let null_root_request = basic_request(
        std::ptr::null(),
        std::ptr::null(),
        std::ptr::null(),
        OPFORGE_EXECUTION_MODE_VM,
        0,
    );
    let null_root = unsafe { opforge_asm_assemble_file_with_request(&null_root_request) };
    assert!(!null_root.is_null());
    assert_eq!(
        unsafe { opforge_asm_report_status(null_root) },
        OpforgeStatus::InvalidRequest
    );
    let null_root_message = unsafe { CStr::from_ptr(ffi::opforge_asm_report_message(null_root)) }
        .to_str()
        .expect("ffi invalid request message utf8");
    assert!(null_root_message.contains("root_path"));
    unsafe { opforge_asm_report_free(null_root) };

    let invalid_utf8_request = basic_request(
        invalid_utf8_ptr(),
        std::ptr::null(),
        std::ptr::null(),
        OPFORGE_EXECUTION_MODE_VM,
        0,
    );
    let invalid_utf8 = unsafe { opforge_asm_assemble_file_with_request(&invalid_utf8_request) };
    assert!(!invalid_utf8.is_null());
    assert_eq!(
        unsafe { opforge_asm_report_status(invalid_utf8) },
        OpforgeStatus::InvalidRequest
    );
    let invalid_utf8_message =
        unsafe { CStr::from_ptr(ffi::opforge_asm_report_message(invalid_utf8)) }
            .to_str()
            .expect("ffi invalid request message utf8");
    assert!(invalid_utf8_message.contains("UTF-8"));
    unsafe { opforge_asm_report_free(invalid_utf8) };

    let work_dir = make_temp_dir("invalid-mode");
    let source_path = work_dir.join("main.asm");
    fs::write(&source_path, ".module main\nstart:\n    nop\n.endmodule\n").expect("write source");
    let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
    let invalid_mode_request =
        basic_request(root.as_ptr(), std::ptr::null(), std::ptr::null(), 99, 0);
    let invalid_mode = unsafe { opforge_asm_assemble_file_with_request(&invalid_mode_request) };
    assert!(!invalid_mode.is_null());
    assert_eq!(
        unsafe { opforge_asm_report_status(invalid_mode) },
        OpforgeStatus::InvalidRequest
    );
    let invalid_mode_message =
        unsafe { CStr::from_ptr(ffi::opforge_asm_report_message(invalid_mode)) }
            .to_str()
            .expect("ffi invalid request message utf8");
    assert!(invalid_mode_message.contains("execution_mode"));
    unsafe { opforge_asm_report_free(invalid_mode) };
}
