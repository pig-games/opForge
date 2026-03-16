// SPDX-License-Identifier: GPL-3.0-or-later

#ifndef OPFORGE_FFI_OPFORGE_H
#define OPFORGE_FFI_OPFORGE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * This header is maintained manually alongside crates/opforge-ffi/src/lib.rs.
 * Update it in the same change as any ABI adjustment and keep
 * crates/opforge-ffi/tests/abi_contract.rs green to catch contract drift.
 */

enum {
    OPFORGE_EXECUTION_MODE_DEFAULT = 0,
    OPFORGE_EXECUTION_MODE_RUST = 1,
    OPFORGE_EXECUTION_MODE_VM = 2,
    OPFORGE_EXECUTION_MODE_LOCKSTEP_RUST = 3,
    OPFORGE_EXECUTION_MODE_LOCKSTEP_VM = 4,
    OPFORGE_OUTPUT_FORMAT_TEXT = 0,
    OPFORGE_OUTPUT_FORMAT_JSON = 1,
    OPFORGE_DEFAULT_OUTPUTS_DEFAULT = 0,
    OPFORGE_DEFAULT_OUTPUTS_DISABLE = 1,
    OPFORGE_DEFAULT_OUTPUTS_ENABLE = 2,
    /* Use the stable Rust facade default label rendering. */
    OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT = 0,
    OPFORGE_LABEL_OUTPUT_FORMAT_VICE = 1,
    OPFORGE_LABEL_OUTPUT_FORMAT_CTAGS = 2,
};

typedef enum opforge_status {
    OPFORGE_STATUS_OK = 0,
    OPFORGE_STATUS_INVALID_REQUEST = 1,
    OPFORGE_STATUS_ASSEMBLE_ERROR = 2,
} opforge_status;

typedef enum opforge_diagnostic_severity {
    OPFORGE_DIAGNOSTIC_WARNING = 0,
    OPFORGE_DIAGNOSTIC_ERROR = 1,
    OPFORGE_DIAGNOSTIC_INVALID = 255,
} opforge_diagnostic_severity;

typedef enum opforge_processor_status {
    OPFORGE_PROCESSOR_STATUS_OK = 0,
    OPFORGE_PROCESSOR_STATUS_INVALID_REQUEST = 1,
    OPFORGE_PROCESSOR_STATUS_TOKENIZE_ERROR = 2,
    OPFORGE_PROCESSOR_STATUS_PARSE_ERROR = 3,
    OPFORGE_PROCESSOR_STATUS_RETURNED = 4,
} opforge_processor_status;

typedef enum opforge_token_kind {
    OPFORGE_TOKEN_IDENTIFIER = 0,
    OPFORGE_TOKEN_REGISTER = 1,
    OPFORGE_TOKEN_NUMBER = 2,
    OPFORGE_TOKEN_STRING = 3,
    OPFORGE_TOKEN_COMMA = 4,
    OPFORGE_TOKEN_COLON = 5,
    OPFORGE_TOKEN_DOLLAR = 6,
    OPFORGE_TOKEN_DOT = 7,
    OPFORGE_TOKEN_HASH = 8,
    OPFORGE_TOKEN_QUESTION = 9,
    OPFORGE_TOKEN_OPEN_BRACKET = 10,
    OPFORGE_TOKEN_CLOSE_BRACKET = 11,
    OPFORGE_TOKEN_OPEN_BRACE = 12,
    OPFORGE_TOKEN_CLOSE_BRACE = 13,
    OPFORGE_TOKEN_OPEN_PAREN = 14,
    OPFORGE_TOKEN_CLOSE_PAREN = 15,
    OPFORGE_TOKEN_OPERATOR = 16,
    OPFORGE_TOKEN_END = 17,
    OPFORGE_TOKEN_INVALID = 255,
} opforge_token_kind;

typedef enum opforge_expr_node_kind {
    OPFORGE_EXPR_NUMBER = 0,
    OPFORGE_EXPR_IDENTIFIER = 1,
    OPFORGE_EXPR_REGISTER = 2,
    OPFORGE_EXPR_LIST = 3,
    OPFORGE_EXPR_INDEX = 4,
    OPFORGE_EXPR_MEMBER = 5,
    OPFORGE_EXPR_STRUCT_LITERAL = 6,
    OPFORGE_EXPR_CALL = 7,
    OPFORGE_EXPR_PLACEHOLDER = 8,
    OPFORGE_EXPR_INDIRECT = 9,
    OPFORGE_EXPR_DOLLAR = 10,
    OPFORGE_EXPR_STRING = 11,
    OPFORGE_EXPR_IMMEDIATE = 12,
    OPFORGE_EXPR_INDIRECT_LONG = 13,
    OPFORGE_EXPR_TUPLE = 14,
    OPFORGE_EXPR_ERROR = 15,
    OPFORGE_EXPR_TERNARY = 16,
    OPFORGE_EXPR_UNARY = 17,
    OPFORGE_EXPR_BINARY = 18,
    OPFORGE_EXPR_RANGE = 19,
    OPFORGE_EXPR_INVALID = 255,
} opforge_expr_node_kind;

typedef enum opforge_line_ast_kind {
    OPFORGE_LINE_EMPTY = 0,
    OPFORGE_LINE_CONDITIONAL = 1,
    OPFORGE_LINE_PLACE = 2,
    OPFORGE_LINE_PACK = 3,
    OPFORGE_LINE_USE = 4,
    OPFORGE_LINE_STATEMENT_DEF = 5,
    OPFORGE_LINE_STATEMENT_END = 6,
    OPFORGE_LINE_ASSIGNMENT = 7,
    OPFORGE_LINE_STATEMENT = 8,
    OPFORGE_LINE_INVALID = 255,
} opforge_line_ast_kind;

typedef struct opforge_string_list {
    const char *const *items;
    size_t count;
} opforge_string_list;

typedef struct opforge_asm_source_options {
    const char *root_path;
    const char *output_base;
    opforge_string_list defines;
    /* For in-memory entry points, these remain filesystem-backed dependency roots. */
    opforge_string_list include_paths;
    /* For in-memory entry points, these remain filesystem-backed dependency roots. */
    opforge_string_list module_paths;
    size_t pp_macro_depth;
} opforge_asm_source_options;

typedef struct opforge_asm_execution_options {
    uint32_t execution_mode;
    const char *cpu_override;
    uint32_t max_loop_iterations;
    const char *opasm_package_path;
} opforge_asm_execution_options;

typedef struct opforge_asm_output_options {
    const char *out_dir;
    /* One of OPFORGE_DEFAULT_OUTPUTS_*; zero keeps the stable Rust facade default. */
    uint8_t emit_outputs;
    uint32_t output_format;
    const char *go_addr;
    opforge_string_list bin_specs;
    uint8_t fill_byte;
    uint8_t fill_byte_set;
    const char *labels_file;
    uint32_t label_output_format;
    const char *dependency_output_path;
    uint8_t dependency_append;
    uint8_t dependency_make_phony;
    const char *outfile_override;
    const char *list_name_override;
    const char *hex_name_override;
    const char *header_title;
    uint8_t no_outputs;
} opforge_asm_output_options;

typedef struct opforge_asm_diagnostics_options {
    uint8_t debug_conditionals;
    size_t tab_size;
} opforge_asm_diagnostics_options;

typedef struct opforge_asm_request {
    opforge_asm_source_options source;
    opforge_asm_execution_options execution;
    opforge_asm_output_options output;
    opforge_asm_diagnostics_options diagnostics;
} opforge_asm_request;

/* Initialize a request with stable Rust facade defaults before overriding fields. */
void opforge_asm_request_init(opforge_asm_request *request);

typedef struct opforge_asm_report opforge_asm_report;
typedef struct opforge_asm_session opforge_asm_session;
typedef struct opforge_prepared_asm_session opforge_prepared_asm_session;
typedef struct opforge_opcore_tokenize_report opforge_opcore_tokenize_report;
typedef struct opforge_opasm_tokenize_report opforge_opasm_tokenize_report;
typedef struct opforge_opasm_parse_report opforge_opasm_parse_report;
typedef struct opforge_opasm_process_report opforge_opasm_process_report;
typedef struct opforge_processing_trace opforge_processing_trace;
typedef struct opforge_lockstep_report opforge_lockstep_report;
typedef struct opforge_registry opforge_registry;
typedef struct opforge_registry_cpu_view opforge_registry_cpu_view;
typedef struct opforge_opcore_expr_report opforge_opcore_expr_report;
typedef struct opforge_opcore_module_item_report opforge_opcore_module_item_report;

typedef uint8_t (*opforge_create_dir_callback)(const char *path, void *user_data);
typedef uint8_t (*opforge_write_file_callback)(
    const char *path,
    const uint8_t *data,
    size_t len,
    void *user_data
);

typedef struct opforge_output_callbacks {
    opforge_create_dir_callback create_dir;
    opforge_write_file_callback write_file;
    void *user_data;
} opforge_output_callbacks;

typedef struct opforge_opasm_process_config {
    const char *line;
    uint32_t line_num;
    uint32_t execution_mode;
    const char *cpu_id;
    const char *dialect_override;
} opforge_opasm_process_config;

/*
 * High-level assembler FFI surface over the stable Rust API.
 * `request` must be non-null. Returned report handles remain owned by the
 * library and must be released with opforge_asm_report_free.
 */
opforge_asm_report *opforge_asm_assemble_file_with_request(
    const opforge_asm_request *request
);
opforge_asm_report *opforge_asm_check_file_with_request(
    const opforge_asm_request *request
);
/*
 * Returns NULL on invalid request or session-construction failure.
 * Use opforge_asm_session_create_with_request_report when you need
 * diagnosable validation errors.
 * Successful handles must be released with opforge_asm_session_free.
 */
opforge_asm_session *opforge_asm_session_create_with_request(
    const opforge_asm_request *request
);
/*
 * Creates a session and returns a report describing success or validation
 * failure. `out_session` must be non-null. On success, the returned report has
 * status OPFORGE_STATUS_OK and `*out_session` receives a valid session handle.
 * On failure, `*out_session` is set to NULL and the returned report contains
 * the error details. The returned report must be released with
 * opforge_asm_report_free.
 */
opforge_asm_report *opforge_asm_session_create_with_request_report(
    const opforge_asm_request *request,
    opforge_asm_session **out_session
);
opforge_prepared_asm_session *opforge_asm_session_prepare(const opforge_asm_session *session);
opforge_asm_report *opforge_asm_session_assemble(const opforge_asm_session *session);
opforge_asm_report *opforge_asm_session_check(const opforge_asm_session *session);
opforge_asm_report *opforge_prepared_asm_session_assemble(
    const opforge_prepared_asm_session *prepared
);
opforge_asm_report *opforge_prepared_asm_session_check(
    const opforge_prepared_asm_session *prepared
);
void opforge_asm_session_free(opforge_asm_session *session);
void opforge_prepared_asm_session_free(opforge_prepared_asm_session *prepared);
opforge_asm_report *opforge_asm_assemble_memory_with_request(
    const opforge_asm_request *request,
    const char *source_text,
    const opforge_output_callbacks *callbacks
);
/*
 * `source_text` supplies the synthetic root source. Any include/module search
 * roots in `request->source` remain filesystem-backed dependency paths.
 * Check-mode suppresses default and metadata-driven outputs, so callbacks are
 * optional and are not used for successful check-only runs.
 */
opforge_asm_report *opforge_asm_check_memory_with_request(
    const opforge_asm_request *request,
    const char *source_text,
    const opforge_output_callbacks *callbacks
);

opforge_status opforge_asm_report_status(const opforge_asm_report *report);
size_t opforge_asm_report_error_count(const opforge_asm_report *report);
size_t opforge_asm_report_warning_count(const opforge_asm_report *report);
size_t opforge_asm_report_lockstep_match_count(const opforge_asm_report *report);
size_t opforge_asm_report_lockstep_divergence_count(const opforge_asm_report *report);
/*
 * Returned string is borrowed from opforge_asm_report and remains valid until
 * opforge_asm_report_free(report). Do not free it.
 */
const char *opforge_asm_report_message(const opforge_asm_report *report);
void opforge_asm_report_free(opforge_asm_report *report);

/*
 * Stable registry query surface over the hardened Rust API.
 * Returned strings are borrowed from opforge_registry or
 * opforge_registry_cpu_view and remain valid until that owner is freed.
 */
opforge_registry *opforge_registry_default(void);
size_t opforge_registry_alias_count(const opforge_registry *registry);
const char *opforge_registry_alias(const opforge_registry *registry, size_t index);
size_t opforge_registry_cpu_count(const opforge_registry *registry);
const char *opforge_registry_cpu_id(const opforge_registry *registry, size_t index);
size_t opforge_registry_family_count(const opforge_registry *registry);
const char *opforge_registry_family_id(const opforge_registry *registry, size_t index);
size_t opforge_registry_dialect_count(const opforge_registry *registry);
const char *opforge_registry_dialect_id(const opforge_registry *registry, size_t index);
size_t opforge_registry_directive_keyword_count(const opforge_registry *registry);
const char *opforge_registry_directive_keyword(
    const opforge_registry *registry,
    size_t index
);
opforge_registry_cpu_view *opforge_registry_cpu_view_lookup(
    const opforge_registry *registry,
    const char *cpu_id
);
const char *opforge_registry_cpu_view_family_id(const opforge_registry_cpu_view *view);
const char *opforge_registry_cpu_view_dialect_id(const opforge_registry_cpu_view *view);
size_t opforge_registry_cpu_view_mnemonic_count(const opforge_registry_cpu_view *view);
const char *opforge_registry_cpu_view_mnemonic(
    const opforge_registry_cpu_view *view,
    size_t index
);
size_t opforge_registry_cpu_view_register_count(const opforge_registry_cpu_view *view);
const char *opforge_registry_cpu_view_register(
    const opforge_registry_cpu_view *view,
    size_t index
);
size_t opforge_registry_cpu_view_runtime_directive_count(
    const opforge_registry_cpu_view *view
);
const char *opforge_registry_cpu_view_runtime_directive(
    const opforge_registry_cpu_view *view,
    size_t index
);
void opforge_registry_free(opforge_registry *registry);
void opforge_registry_cpu_view_free(opforge_registry_cpu_view *view);

/*
 * Read-only diagnostics enumeration over an opforge_asm_report.
 * Returned strings are borrowed from the report and remain valid until the
 * report is freed.
 */
size_t opforge_diag_count_from_asm_report(const opforge_asm_report *report);
opforge_diagnostic_severity opforge_diag_severity_from_asm_report(
    const opforge_asm_report *report,
    size_t index
);
uint32_t opforge_diag_line_from_asm_report(const opforge_asm_report *report, size_t index);
size_t opforge_diag_column_from_asm_report(const opforge_asm_report *report, size_t index);
size_t opforge_diag_col_end_from_asm_report(const opforge_asm_report *report, size_t index);
const char *opforge_diag_message_from_asm_report(
    const opforge_asm_report *report,
    size_t index
);
const char *opforge_diag_code_from_asm_report(
    const opforge_asm_report *report,
    size_t index
);
const char *opforge_diag_file_from_asm_report(
    const opforge_asm_report *report,
    size_t index
);
size_t opforge_diag_related_span_count_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index
);
const char *opforge_diag_related_span_file_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t span_index
);
uint32_t opforge_diag_related_span_line_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t span_index
);
size_t opforge_diag_related_span_col_start_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t span_index
);
size_t opforge_diag_related_span_col_end_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t span_index
);
const char *opforge_diag_related_span_label_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t span_index
);
uint8_t opforge_diag_related_span_is_primary_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t span_index
);
size_t opforge_diag_note_count_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index
);
const char *opforge_diag_note_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t note_index
);
size_t opforge_diag_help_count_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index
);
const char *opforge_diag_help_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t help_index
);
size_t opforge_diag_fixit_count_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index
);
const char *opforge_diag_fixit_file_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t fixit_index
);
uint32_t opforge_diag_fixit_line_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t fixit_index
);
size_t opforge_diag_fixit_col_start_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t fixit_index
);
size_t opforge_diag_fixit_col_end_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t fixit_index
);
const char *opforge_diag_fixit_replacement_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t fixit_index
);
const char *opforge_diag_fixit_applicability_from_asm_report(
    const opforge_asm_report *report,
    size_t diag_index,
    size_t fixit_index
);

/*
 * Read-only token enumeration over an opforge_opcore tokenization report.
 * Returned strings are borrowed from the report and remain valid until the
 * report is freed.
 */
opforge_opcore_tokenize_report *opforge_opcore_tokenize_line(
    const char *line,
    uint32_t line_num
);
opforge_processor_status opforge_opcore_tokenize_report_status(
    const opforge_opcore_tokenize_report *report
);
size_t opforge_opcore_tokenize_report_token_count(
    const opforge_opcore_tokenize_report *report
);
opforge_token_kind opforge_opcore_tokenize_report_token_kind(
    const opforge_opcore_tokenize_report *report,
    size_t index
);
const char *opforge_opcore_tokenize_report_token_text(
    const opforge_opcore_tokenize_report *report,
    size_t index
);
uint32_t opforge_opcore_tokenize_report_token_line(
    const opforge_opcore_tokenize_report *report,
    size_t index
);
size_t opforge_opcore_tokenize_report_token_col_start(
    const opforge_opcore_tokenize_report *report,
    size_t index
);
size_t opforge_opcore_tokenize_report_token_col_end(
    const opforge_opcore_tokenize_report *report,
    size_t index
);
const char *opforge_opcore_tokenize_report_error_message(
    const opforge_opcore_tokenize_report *report
);
uint32_t opforge_opcore_tokenize_report_error_line(
    const opforge_opcore_tokenize_report *report
);
size_t opforge_opcore_tokenize_report_error_col_start(
    const opforge_opcore_tokenize_report *report
);
size_t opforge_opcore_tokenize_report_error_col_end(
    const opforge_opcore_tokenize_report *report
);
void opforge_opcore_tokenize_report_free(opforge_opcore_tokenize_report *report);

/*
 * Read-only token enumeration over an opforge_opasm statement-tokenization report.
 * Returned strings are borrowed from the report and remain valid until the
 * report is freed.
 */
opforge_opasm_tokenize_report *opforge_opasm_tokenize_statement(
    const char *line,
    uint32_t line_num
);
opforge_processor_status opforge_opasm_tokenize_report_status(
    const opforge_opasm_tokenize_report *report
);
size_t opforge_opasm_tokenize_report_token_count(
    const opforge_opasm_tokenize_report *report
);
opforge_token_kind opforge_opasm_tokenize_report_token_kind(
    const opforge_opasm_tokenize_report *report,
    size_t index
);
const char *opforge_opasm_tokenize_report_token_text(
    const opforge_opasm_tokenize_report *report,
    size_t index
);
uint32_t opforge_opasm_tokenize_report_token_line(
    const opforge_opasm_tokenize_report *report,
    size_t index
);
size_t opforge_opasm_tokenize_report_token_col_start(
    const opforge_opasm_tokenize_report *report,
    size_t index
);
size_t opforge_opasm_tokenize_report_token_col_end(
    const opforge_opasm_tokenize_report *report,
    size_t index
);
const char *opforge_opasm_tokenize_report_error_message(
    const opforge_opasm_tokenize_report *report
);
uint32_t opforge_opasm_tokenize_report_error_line(
    const opforge_opasm_tokenize_report *report
);
size_t opforge_opasm_tokenize_report_error_col_start(
    const opforge_opasm_tokenize_report *report
);
size_t opforge_opasm_tokenize_report_error_col_end(
    const opforge_opasm_tokenize_report *report
);
void opforge_opasm_tokenize_report_free(opforge_opasm_tokenize_report *report);

/*
 * Read-only parsed line enumeration over an opforge_opasm statement-parse report.
 * Returned strings are borrowed from the report and remain valid until the
 * report is freed.
 */
opforge_opasm_parse_report *opforge_opasm_parse_statement(
    const char *line,
    uint32_t line_num
);
opforge_processor_status opforge_opasm_parse_report_status(
    const opforge_opasm_parse_report *report
);
opforge_line_ast_kind opforge_opasm_parse_report_kind(
    const opforge_opasm_parse_report *report
);
const char *opforge_opasm_parse_report_use_module_id(
    const opforge_opasm_parse_report *report
);
const char *opforge_opasm_parse_report_use_alias(
    const opforge_opasm_parse_report *report
);
size_t opforge_opasm_parse_report_use_item_count(
    const opforge_opasm_parse_report *report
);
const char *opforge_opasm_parse_report_use_item_name(
    const opforge_opasm_parse_report *report,
    size_t index
);
const char *opforge_opasm_parse_report_statement_mnemonic(
    const opforge_opasm_parse_report *report
);
size_t opforge_opasm_parse_report_statement_operand_count(
    const opforge_opasm_parse_report *report
);
const char *opforge_opasm_parse_report_statement_operand_text(
    const opforge_opasm_parse_report *report,
    size_t index
);
const char *opforge_opasm_parse_report_error_message(
    const opforge_opasm_parse_report *report
);
uint32_t opforge_opasm_parse_report_error_line(
    const opforge_opasm_parse_report *report
);
size_t opforge_opasm_parse_report_error_col_start(
    const opforge_opasm_parse_report *report
);
size_t opforge_opasm_parse_report_error_col_end(
    const opforge_opasm_parse_report *report
);
void opforge_opasm_parse_report_free(opforge_opasm_parse_report *report);

/*
 * Read-only processed line enumeration over an opforge_opasm processing report.
 * Returned strings are borrowed from the report and remain valid until the
 * report is freed.
 */
opforge_opasm_process_report *opforge_opasm_process_statement(
    const opforge_opasm_process_config *request
);
opforge_processor_status opforge_opasm_process_report_status(
    const opforge_opasm_process_report *report
);
opforge_line_ast_kind opforge_opasm_process_report_kind(
    const opforge_opasm_process_report *report
);
const char *opforge_opasm_process_report_use_module_id(
    const opforge_opasm_process_report *report
);
const char *opforge_opasm_process_report_use_alias(
    const opforge_opasm_process_report *report
);
size_t opforge_opasm_process_report_use_item_count(
    const opforge_opasm_process_report *report
);
const char *opforge_opasm_process_report_use_item_name(
    const opforge_opasm_process_report *report,
    size_t index
);
const char *opforge_opasm_process_report_statement_mnemonic(
    const opforge_opasm_process_report *report
);
size_t opforge_opasm_process_report_statement_operand_count(
    const opforge_opasm_process_report *report
);
const char *opforge_opasm_process_report_statement_operand_text(
    const opforge_opasm_process_report *report,
    size_t index
);
size_t opforge_opasm_process_report_trace_request_count(
    const opforge_opasm_process_report *report
);
opforge_processing_trace *opforge_opasm_process_report_processing_trace(
    const opforge_opasm_process_report *report
);
/*
 * Returned strings are borrowed from opforge_processing_trace and remain valid
 * until that owner is freed.
 */
size_t opforge_processing_trace_request_count(
    const opforge_processing_trace *trace
);
const char *opforge_processing_trace_request_text(
    const opforge_processing_trace *trace,
    size_t index
);
size_t opforge_opasm_process_report_lockstep_match_count(
    const opforge_opasm_process_report *report
);
opforge_lockstep_report *opforge_opasm_process_report_lockstep_report(
    const opforge_opasm_process_report *report
);
size_t opforge_opasm_process_report_lockstep_divergence_count(
    const opforge_opasm_process_report *report
);
/*
 * Returned strings are borrowed from opforge_lockstep_report and remain valid
 * until that owner is freed.
 */
size_t opforge_lockstep_report_match_count(const opforge_lockstep_report *report);
size_t opforge_lockstep_report_divergence_count(const opforge_lockstep_report *report);
const char *opforge_lockstep_report_match_stage_text(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_match_request_text(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_match_category_text(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_divergence_stage_text(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_divergence_request_text(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_divergence_processor_domain(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_divergence_continuation_head(
    const opforge_lockstep_report *report,
    size_t index
);
uint32_t opforge_lockstep_report_divergence_source_line(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_divergence_active_cpu(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_divergence_active_dialect(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_divergence_left_text(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_divergence_right_text(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_divergence_category_text(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_lockstep_report_divergence_reason_code(
    const opforge_lockstep_report *report,
    size_t index
);
const char *opforge_opasm_process_report_error_message(
    const opforge_opasm_process_report *report
);
uint32_t opforge_opasm_process_report_error_line(
    const opforge_opasm_process_report *report
);
size_t opforge_opasm_process_report_error_col_start(
    const opforge_opasm_process_report *report
);
size_t opforge_opasm_process_report_error_col_end(
    const opforge_opasm_process_report *report
);
void opforge_opasm_process_report_free(opforge_opasm_process_report *report);
void opforge_processing_trace_free(opforge_processing_trace *trace);
void opforge_lockstep_report_free(opforge_lockstep_report *report);

/*
 * Read-only expression tree enumeration over an opforge_opcore expression report.
 * Returned strings are borrowed from the report and remain valid until the
 * report is freed.
 */
opforge_opcore_expr_report *opforge_opcore_parse_expression(
    const char *line,
    uint32_t line_num
);
opforge_processor_status opforge_opcore_expr_report_status(
    const opforge_opcore_expr_report *report
);
size_t opforge_opcore_expr_report_node_count(
    const opforge_opcore_expr_report *report
);
opforge_expr_node_kind opforge_opcore_expr_report_node_kind(
    const opforge_opcore_expr_report *report,
    size_t index
);
const char *opforge_opcore_expr_report_node_text(
    const opforge_opcore_expr_report *report,
    size_t index
);
uint32_t opforge_opcore_expr_report_node_line(
    const opforge_opcore_expr_report *report,
    size_t index
);
size_t opforge_opcore_expr_report_node_col_start(
    const opforge_opcore_expr_report *report,
    size_t index
);
size_t opforge_opcore_expr_report_node_col_end(
    const opforge_opcore_expr_report *report,
    size_t index
);
size_t opforge_opcore_expr_report_node_child_count(
    const opforge_opcore_expr_report *report,
    size_t index
);
size_t opforge_opcore_expr_report_node_child(
    const opforge_opcore_expr_report *report,
    size_t index,
    size_t child_index
);
const char *opforge_opcore_expr_report_node_child_label(
    const opforge_opcore_expr_report *report,
    size_t index,
    size_t child_index
);
const char *opforge_opcore_expr_report_error_message(
    const opforge_opcore_expr_report *report
);
uint32_t opforge_opcore_expr_report_error_line(
    const opforge_opcore_expr_report *report
);
size_t opforge_opcore_expr_report_error_col_start(
    const opforge_opcore_expr_report *report
);
size_t opforge_opcore_expr_report_error_col_end(
    const opforge_opcore_expr_report *report
);
void opforge_opcore_expr_report_free(opforge_opcore_expr_report *report);

/*
 * Read-only module-item processing report over an opforge_opcore module-item report.
 * Returned strings are borrowed from the report and remain valid until the
 * report is freed.
 */
opforge_opcore_module_item_report *opforge_opcore_process_module_item(
    const char *line,
    uint32_t line_num
);
opforge_processor_status opforge_opcore_module_item_report_status(
    const opforge_opcore_module_item_report *report
);
opforge_line_ast_kind opforge_opcore_module_item_report_kind(
    const opforge_opcore_module_item_report *report
);
const char *opforge_opcore_module_item_report_use_module_id(
    const opforge_opcore_module_item_report *report
);
const char *opforge_opcore_module_item_report_use_alias(
    const opforge_opcore_module_item_report *report
);
size_t opforge_opcore_module_item_report_use_item_count(
    const opforge_opcore_module_item_report *report
);
const char *opforge_opcore_module_item_report_use_item_name(
    const opforge_opcore_module_item_report *report,
    size_t index
);
const char *opforge_opcore_module_item_report_statement_mnemonic(
    const opforge_opcore_module_item_report *report
);
size_t opforge_opcore_module_item_report_statement_operand_count(
    const opforge_opcore_module_item_report *report
);
const char *opforge_opcore_module_item_report_statement_operand_text(
    const opforge_opcore_module_item_report *report,
    size_t index
);
const char *opforge_opcore_module_item_report_error_message(
    const opforge_opcore_module_item_report *report
);
uint32_t opforge_opcore_module_item_report_error_line(
    const opforge_opcore_module_item_report *report
);
size_t opforge_opcore_module_item_report_error_col_start(
    const opforge_opcore_module_item_report *report
);
size_t opforge_opcore_module_item_report_error_col_end(
    const opforge_opcore_module_item_report *report
);
void opforge_opcore_module_item_report_free(opforge_opcore_module_item_report *report);

#ifdef __cplusplus
}
#endif

#endif
