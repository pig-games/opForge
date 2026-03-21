// SPDX-License-Identifier: GPL-3.0-or-later

//! C and C++ FFI layer for libopforge.
//!
//! This crate is intentionally thin: it maps a small C ABI onto the same
//! `libopforge`/`api` boundary used by Rust hosts.

use std::any::Any;
use std::ffi::c_void;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};

use ::api::{
    asm::{parse_bin_output_arg, LabelOutputFormat, OutputFormat},
    diagnostics::{Diagnostic, Severity},
    io::{MemoryOutputSink, MemorySourceProvider},
    lockstep::ExecutionMode,
};

/// Use the stable Rust facade default execution mode.
pub const OPFORGE_EXECUTION_MODE_DEFAULT: u32 = 0;
/// Assemble via the direct Rust continuation head.
pub const OPFORGE_EXECUTION_MODE_RUST: u32 = 1;
/// Assemble via the VM continuation head.
pub const OPFORGE_EXECUTION_MODE_VM: u32 = 2;
/// Run lockstep assembly and report with the Rust continuation head leading.
pub const OPFORGE_EXECUTION_MODE_LOCKSTEP_RUST: u32 = 3;
/// Run lockstep assembly and report with the VM continuation head leading.
pub const OPFORGE_EXECUTION_MODE_LOCKSTEP_VM: u32 = 4;
/// Render text outputs such as listings, labels, and dependency files in text mode.
pub const OPFORGE_OUTPUT_FORMAT_TEXT: u32 = 0;
/// Render supported outputs in JSON mode where available.
pub const OPFORGE_OUTPUT_FORMAT_JSON: u32 = 1;
/// Use the stable Rust facade default output-emission behavior.
pub const OPFORGE_DEFAULT_OUTPUTS_DEFAULT: u8 = 0;
/// Suppress default outputs such as listing and hex files.
pub const OPFORGE_DEFAULT_OUTPUTS_DISABLE: u8 = 1;
/// Force default outputs such as listing and hex files on.
pub const OPFORGE_DEFAULT_OUTPUTS_ENABLE: u8 = 2;
/// Use the stable Rust facade default label rendering.
pub const OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT: u32 = 0;
/// Emit VICE-compatible labels.
pub const OPFORGE_LABEL_OUTPUT_FORMAT_VICE: u32 = 1;
/// Emit ctags-compatible labels.
pub const OPFORGE_LABEL_OUTPUT_FORMAT_CTAGS: u32 = 2;

type FfiAsmReportError = Box<OpforgeAsmReport>;

fn boxed_asm_report(report: OpforgeAsmReport) -> FfiAsmReportError {
    Box::new(report)
}

fn invalid_request_report(message: impl Into<String>) -> FfiAsmReportError {
    boxed_asm_report(OpforgeAsmReport::error(
        OpforgeStatus::InvalidRequest,
        Vec::new(),
        0,
        0,
        0,
        0,
        message,
    ))
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OpforgeStringList {
    /// Pointer to an array of `count` NUL-terminated UTF-8 string pointers.
    pub items: *const *const c_char,
    /// Number of entries in `items`.
    pub count: usize,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OpforgeAsmSourceOptions {
    /// Required source path. Must be a non-null NUL-terminated UTF-8 string.
    pub root_path: *const c_char,
    /// Optional output base. Null or empty falls back to `root_path` without its extension.
    pub output_base: *const c_char,
    /// Optional preprocessor defines.
    pub defines: OpforgeStringList,
    /// Optional include search roots. For the in-memory entry points, these are
    /// filesystem-backed dependency roots consulted after the synthetic root source.
    pub include_paths: OpforgeStringList,
    /// Optional module search roots. For the in-memory entry points, these are
    /// filesystem-backed dependency roots consulted after the synthetic root source.
    pub module_paths: OpforgeStringList,
    /// Optional preprocessor recursion limit. Zero keeps the library default.
    pub pp_macro_depth: usize,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OpforgeAsmExecutionOptions {
    /// One of the `OPFORGE_EXECUTION_MODE_*` constants. Zero keeps the stable Rust facade default.
    pub execution_mode: u32,
    /// Optional CPU override for the request.
    pub cpu_override: *const c_char,
    /// Optional loop-iteration ceiling. Zero keeps the library default.
    pub max_loop_iterations: u32,
    /// Optional explicit `.opasm` runtime package path for the request.
    pub opasm_package_path: *const c_char,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OpforgeAsmOutputOptions {
    /// Optional output directory. Null or empty keeps the library default.
    pub out_dir: *const c_char,
    /// One of the `OPFORGE_DEFAULT_OUTPUTS_*` constants. Zero keeps the stable Rust facade default.
    pub emit_outputs: u8,
    /// One of the `OPFORGE_OUTPUT_FORMAT_*` constants. Zero keeps the default.
    pub output_format: u32,
    /// Optional go address using the stable text form accepted by the Rust API.
    pub go_addr: *const c_char,
    /// Optional bin output specs using the stable textual bin-spec syntax.
    pub bin_specs: OpforgeStringList,
    /// Fill byte used for binary padding when `fill_byte_set` is non-zero.
    pub fill_byte: u8,
    /// Non-zero when `fill_byte` should override the library default.
    pub fill_byte_set: u8,
    /// Optional labels-file path.
    pub labels_file: *const c_char,
    /// One of the `OPFORGE_LABEL_OUTPUT_FORMAT_*` constants. Zero keeps the stable Rust facade default.
    pub label_output_format: u32,
    /// Optional dependency-output path.
    pub dependency_output_path: *const c_char,
    /// Non-zero to append to the dependency-output file.
    pub dependency_append: u8,
    /// Non-zero to emit phony dependency targets.
    pub dependency_make_phony: u8,
    /// Optional output-file override.
    pub outfile_override: *const c_char,
    /// Optional listing-file name override.
    pub list_name_override: *const c_char,
    /// Optional hex-file name override.
    pub hex_name_override: *const c_char,
    /// Optional listing header title.
    pub header_title: *const c_char,
    /// Non-zero to suppress all output emission even when output directives are present.
    pub no_outputs: u8,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OpforgeAsmDiagnosticsOptions {
    /// Non-zero to enable debug-conditional traces in diagnostics.
    pub debug_conditionals: u8,
    /// Optional tab width. Zero keeps the library default.
    pub tab_size: usize,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OpforgeAsmRequest {
    pub source: OpforgeAsmSourceOptions,
    pub execution: OpforgeAsmExecutionOptions,
    pub output: OpforgeAsmOutputOptions,
    pub diagnostics: OpforgeAsmDiagnosticsOptions,
}

pub type OpforgeCreateDirCallback =
    unsafe extern "C" fn(path: *const c_char, user_data: *mut c_void) -> u8;
pub type OpforgeWriteFileCallback = unsafe extern "C" fn(
    path: *const c_char,
    data: *const u8,
    len: usize,
    user_data: *mut c_void,
) -> u8;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OpforgeOutputCallbacks {
    pub create_dir: Option<OpforgeCreateDirCallback>,
    pub write_file: Option<OpforgeWriteFileCallback>,
    pub user_data: *mut c_void,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct OpforgeOpasmProcessConfig {
    /// Required source line. Must be a non-null NUL-terminated UTF-8 string.
    pub line: *const c_char,
    /// 1-based source line number.
    pub line_num: u32,
    /// One of the `OPFORGE_EXECUTION_MODE_*` constants.
    pub execution_mode: u32,
    /// Required for VM and lockstep processing; ignored for Rust mode.
    pub cpu_id: *const c_char,
    /// Optional dialect override for VM and lockstep processing.
    pub dialect_override: *const c_char,
}

#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OpforgeStatus {
    Ok = 0,
    InvalidRequest = 1,
    AssembleError = 2,
}

#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OpforgeDiagnosticSeverity {
    Warning = 0,
    Error = 1,
    Invalid = 255,
}

#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OpforgeProcessorStatus {
    Ok = 0,
    InvalidRequest = 1,
    TokenizeError = 2,
    ParseError = 3,
    Returned = 4,
}

#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OpforgeTokenKind {
    Identifier = 0,
    Register = 1,
    Number = 2,
    String = 3,
    Comma = 4,
    Colon = 5,
    Dollar = 6,
    Dot = 7,
    Hash = 8,
    Question = 9,
    OpenBracket = 10,
    CloseBracket = 11,
    OpenBrace = 12,
    CloseBrace = 13,
    OpenParen = 14,
    CloseParen = 15,
    Operator = 16,
    End = 17,
    Invalid = 255,
}

#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OpforgeExprNodeKind {
    Number = 0,
    Identifier = 1,
    Register = 2,
    List = 3,
    Index = 4,
    Member = 5,
    StructLiteral = 6,
    Call = 7,
    Placeholder = 8,
    Indirect = 9,
    Dollar = 10,
    String = 11,
    Immediate = 12,
    IndirectLong = 13,
    Tuple = 14,
    Error = 15,
    Ternary = 16,
    Unary = 17,
    Binary = 18,
    Range = 19,
    Invalid = 255,
}

#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OpforgeLineAstKind {
    Empty = 0,
    Conditional = 1,
    Place = 2,
    Pack = 3,
    Use = 4,
    StatementDef = 5,
    StatementEnd = 6,
    Assignment = 7,
    Statement = 8,
    Invalid = 255,
}

#[derive(Clone)]
pub struct OpforgeAsmReport {
    status: OpforgeStatus,
    error_count: usize,
    warning_count: usize,
    lockstep_match_count: usize,
    lockstep_divergence_count: usize,
    message: Option<CString>,
    diagnostics: Vec<Diagnostic>,
    diagnostic_messages: Vec<CString>,
    diagnostic_details: Vec<OpforgeCachedDiagnostic>,
}

#[derive(Clone)]
struct OpforgeCachedRelatedSpan {
    file: Option<CString>,
    label: Option<CString>,
}

#[derive(Clone)]
struct OpforgeCachedFixit {
    file: Option<CString>,
    replacement: CString,
    applicability: CString,
}

#[derive(Clone)]
struct OpforgeCachedDiagnostic {
    code: CString,
    file: Option<CString>,
    related_spans: Vec<OpforgeCachedRelatedSpan>,
    notes: Vec<CString>,
    help: Vec<CString>,
    fixits: Vec<OpforgeCachedFixit>,
}

pub struct OpforgeAsmSession {
    session: api::asm::AssemblerSession,
}

pub struct OpforgePreparedAsmSession {
    prepared: Option<api::asm::PreparedAssemblySession>,
    failure: Option<OpforgeAsmReport>,
}

pub struct OpforgeOpcoreTokenizeReport {
    status: OpforgeProcessorStatus,
    tokens: Vec<api::opcore::portable::PortableToken>,
    token_texts: Vec<CString>,
    error_message: Option<CString>,
    error_line: u32,
    error_col_start: usize,
    error_col_end: usize,
}

pub type OpforgeOpasmTokenizeReport = OpforgeOpcoreTokenizeReport;
pub type OpforgeOpasmParseReport = OpforgeOpcoreModuleItemReport;

pub struct OpforgeProcessingTrace {
    request_texts: Vec<CString>,
}

#[derive(Clone)]
struct OpforgeLockstepMatchRecord {
    stage_text: CString,
    request_text: CString,
    category_text: CString,
}

#[derive(Clone)]
struct OpforgeLockstepDivergenceRecord {
    stage_text: CString,
    processor_domain: CString,
    request_text: CString,
    continuation_head_text: CString,
    source_line: u32,
    active_cpu: Option<CString>,
    active_dialect: Option<CString>,
    left_text: CString,
    right_text: CString,
    category_text: CString,
    reason_code: CString,
}

pub struct OpforgeLockstepReport {
    matches: Vec<OpforgeLockstepMatchRecord>,
    divergences: Vec<OpforgeLockstepDivergenceRecord>,
}

pub struct OpforgeRegistry {
    aliases: Vec<CString>,
    family_ids: Vec<CString>,
    cpu_ids: Vec<CString>,
    dialect_ids: Vec<CString>,
    directive_keywords: Vec<CString>,
    snapshot: api::registry::CapabilitySnapshot,
}

pub struct OpforgeRegistryCpuView {
    family_id: CString,
    dialect_id: CString,
    mnemonics: Vec<CString>,
    registers: Vec<CString>,
    runtime_directives: Vec<CString>,
}

pub struct OpforgeOpasmProcessReport {
    parsed: OpforgeOpasmParseReport,
    processing_trace: OpforgeProcessingTrace,
    lockstep_report: OpforgeLockstepReport,
    trace_request_count: usize,
    lockstep_match_count: usize,
    lockstep_divergence_count: usize,
}

#[derive(Debug, Clone)]
struct OpforgeExprChildEdge {
    child_index: usize,
    label_text_index: Option<usize>,
}

#[derive(Debug, Clone)]
struct OpforgeExprNodeRecord {
    kind: OpforgeExprNodeKind,
    line: u32,
    col_start: usize,
    col_end: usize,
    text_index: Option<usize>,
    child_start: usize,
    child_len: usize,
}

pub struct OpforgeOpcoreExprReport {
    status: OpforgeProcessorStatus,
    nodes: Vec<OpforgeExprNodeRecord>,
    child_edges: Vec<OpforgeExprChildEdge>,
    texts: Vec<CString>,
    error_message: Option<CString>,
    error_line: u32,
    error_col_start: usize,
    error_col_end: usize,
}

pub struct OpforgeOpcoreModuleItemReport {
    status: OpforgeProcessorStatus,
    line_kind: OpforgeLineAstKind,
    use_module_id: Option<CString>,
    use_alias: Option<CString>,
    use_item_names: Vec<CString>,
    statement_mnemonic: Option<CString>,
    statement_operand_texts: Vec<CString>,
    error_message: Option<CString>,
    error_line: u32,
    error_col_start: usize,
    error_col_end: usize,
}

impl OpforgeOpasmProcessReport {
    fn ok(result: api::asm::opasm::portable::StatementProcessResult) -> Self {
        Self {
            parsed: OpforgeOpasmParseReport::ok(result.parsed.ast),
            processing_trace: OpforgeProcessingTrace::from_trace(&result.trace),
            lockstep_report: OpforgeLockstepReport::from_report(&result.lockstep_report),
            trace_request_count: result.trace.requests().len(),
            lockstep_match_count: result.lockstep_report.matches().len(),
            lockstep_divergence_count: result.lockstep_report.divergences().len(),
        }
    }

    fn error(message: impl Into<String>, line: u32, col_start: usize, col_end: usize) -> Self {
        Self {
            parsed: OpforgeOpasmParseReport::error(message, line, col_start, col_end),
            processing_trace: OpforgeProcessingTrace {
                request_texts: Vec::new(),
            },
            lockstep_report: OpforgeLockstepReport {
                matches: Vec::new(),
                divergences: Vec::new(),
            },
            trace_request_count: 0,
            lockstep_match_count: 0,
            lockstep_divergence_count: 0,
        }
    }
}

impl OpforgeProcessingTrace {
    fn from_trace(trace: &api::processing::LineProcessingTrace) -> Self {
        Self {
            request_texts: trace
                .requests()
                .iter()
                .map(|request| {
                    CString::new(processing_request_text(request).replace('\0', " "))
                        .expect("sanitized processing request text should not contain NULs")
                })
                .collect(),
        }
    }

    fn request_text_ptr(&self, index: usize) -> *const c_char {
        self.request_texts
            .get(index)
            .map_or(std::ptr::null(), |text| text.as_ptr())
    }
}

impl OpforgeLockstepReport {
    fn from_report(report: &api::lockstep::LockstepReport) -> Self {
        Self {
            matches: report
                .matches()
                .iter()
                .map(|entry| OpforgeLockstepMatchRecord {
                    stage_text: cstring(lockstep_stage_text(entry.stage)),
                    request_text: cstring(processing_request_text(&entry.request)),
                    category_text: cstring(lockstep_category_text(entry.category)),
                })
                .collect(),
            divergences: report
                .divergences()
                .iter()
                .map(|entry| OpforgeLockstepDivergenceRecord {
                    stage_text: cstring(lockstep_stage_text(entry.stage)),
                    processor_domain: cstring(entry.processor_domain.clone()),
                    request_text: cstring(processing_request_text(&entry.request)),
                    continuation_head_text: cstring(continuation_head_text(
                        entry.continuation_head,
                    )),
                    source_line: entry.source_line.unwrap_or(0),
                    active_cpu: entry
                        .active_cpu
                        .as_ref()
                        .map(|value| cstring(value.clone())),
                    active_dialect: entry
                        .active_dialect
                        .as_ref()
                        .map(|value| cstring(value.clone())),
                    left_text: cstring(lockstep_checkpoint_text(&entry.left)),
                    right_text: cstring(lockstep_checkpoint_text(&entry.right)),
                    category_text: cstring(lockstep_category_text(entry.category)),
                    reason_code: cstring(entry.reason_code.clone()),
                })
                .collect(),
        }
    }
}

impl OpforgeRegistry {
    fn default_registry() -> Self {
        let snapshot = api::registry::CapabilitySnapshot::from_registry(
            &api::registry::default_asm_registry(),
        );
        Self {
            aliases: snapshot
                .cpu_name_aliases
                .iter()
                .cloned()
                .map(cstring)
                .collect(),
            family_ids: snapshot.family_ids.iter().cloned().map(cstring).collect(),
            cpu_ids: snapshot.cpu_ids.iter().cloned().map(cstring).collect(),
            dialect_ids: snapshot.dialect_ids.iter().cloned().map(cstring).collect(),
            directive_keywords: snapshot
                .directive_keywords
                .iter()
                .cloned()
                .map(cstring)
                .collect(),
            snapshot,
        }
    }

    fn string_ptr(items: &[CString], index: usize) -> *const c_char {
        items
            .get(index)
            .map_or(std::ptr::null(), |item| item.as_ptr())
    }
}

impl OpforgeRegistryCpuView {
    fn from_view(view: &api::registry::CpuCapabilityView) -> Self {
        Self {
            family_id: cstring(view.family_id.clone()),
            dialect_id: cstring(view.dialect_id.clone()),
            mnemonics: view.mnemonics.iter().cloned().map(cstring).collect(),
            registers: view.registers.iter().cloned().map(cstring).collect(),
            runtime_directives: view
                .runtime_directives
                .iter()
                .cloned()
                .map(cstring)
                .collect(),
        }
    }

    fn string_ptr(items: &[CString], index: usize) -> *const c_char {
        items
            .get(index)
            .map_or(std::ptr::null(), |item| item.as_ptr())
    }
}

fn cstring(text: impl Into<String>) -> CString {
    CString::new(text.into().replace('\0', " "))
        .expect("sanitized FFI text should not contain NULs")
}

fn processing_request_text(request: &api::processing::ProcessingRequestKind) -> String {
    match request {
        api::processing::ProcessingRequestKind::Opcore(kind) => match kind {
            api::processing::OpcoreRequestKind::Expr => "opcore:expr".to_string(),
            api::processing::OpcoreRequestKind::Statement => "opcore:statement".to_string(),
            api::processing::OpcoreRequestKind::ModuleItem => "opcore:module-item".to_string(),
        },
        api::processing::ProcessingRequestKind::Processor { processor, kind } => {
            format!("processor:{processor}:{kind}")
        }
    }
}

fn continuation_head_text(head: api::lockstep::ContinuationHead) -> &'static str {
    match head {
        api::lockstep::ContinuationHead::Rust => "rust",
        api::lockstep::ContinuationHead::Vm => "vm",
    }
}

fn lockstep_stage_text(stage: api::lockstep::LockstepStage) -> &'static str {
    match stage {
        api::lockstep::LockstepStage::OpcoreExpr => "opcore:expr",
        api::lockstep::LockstepStage::OpasmStatementParse => "opasm:statement-parse",
    }
}

fn lockstep_category_text(category: api::lockstep::LockstepComparisonCategory) -> &'static str {
    match category {
        api::lockstep::LockstepComparisonCategory::Ast => "ast",
        api::lockstep::LockstepComparisonCategory::Diagnostics => "diagnostics",
    }
}

fn lockstep_checkpoint_text(checkpoint: &api::lockstep::LockstepCheckpoint) -> String {
    match checkpoint {
        api::lockstep::LockstepCheckpoint::CoreExprAst {
            normalized: checkpoint_text,
        } => {
            format!("core-expr-ast:{checkpoint_text}")
        }
        api::lockstep::LockstepCheckpoint::PortableLineAst {
            normalized: checkpoint_text,
        } => {
            format!("portable-line-ast:{checkpoint_text}")
        }
        api::lockstep::LockstepCheckpoint::Diagnostic {
            normalized: checkpoint_text,
        } => {
            format!("diagnostic:{checkpoint_text}")
        }
    }
}

impl OpforgeAsmReport {
    fn ok(
        diagnostics: Vec<Diagnostic>,
        error_count: usize,
        warning_count: usize,
        lockstep_match_count: usize,
        lockstep_divergence_count: usize,
    ) -> Self {
        let diagnostic_messages = diagnostics
            .iter()
            .map(|diag| {
                CString::new(diag.message().replace('\0', " "))
                    .expect("sanitized diagnostic message should not contain NULs")
            })
            .collect();
        let diagnostic_details = diagnostics
            .iter()
            .map(OpforgeCachedDiagnostic::from_diagnostic)
            .collect();
        Self {
            status: OpforgeStatus::Ok,
            error_count,
            warning_count,
            lockstep_match_count,
            lockstep_divergence_count,
            message: None,
            diagnostics,
            diagnostic_messages,
            diagnostic_details,
        }
    }

    fn error(
        status: OpforgeStatus,
        diagnostics: Vec<Diagnostic>,
        error_count: usize,
        warning_count: usize,
        lockstep_match_count: usize,
        lockstep_divergence_count: usize,
        message: impl Into<String>,
    ) -> Self {
        let diagnostic_messages = diagnostics
            .iter()
            .map(|diag| {
                CString::new(diag.message().replace('\0', " "))
                    .expect("sanitized diagnostic message should not contain NULs")
            })
            .collect();
        let diagnostic_details = diagnostics
            .iter()
            .map(OpforgeCachedDiagnostic::from_diagnostic)
            .collect();
        Self {
            status,
            error_count,
            warning_count,
            lockstep_match_count,
            lockstep_divergence_count,
            message: Some(
                CString::new(message.into().replace('\0', " "))
                    .expect("sanitized report message should not contain NULs"),
            ),
            diagnostics,
            diagnostic_messages,
            diagnostic_details,
        }
    }

    fn message_ptr(&self) -> *const c_char {
        self.message
            .as_ref()
            .map_or(std::ptr::null(), |message| message.as_ptr())
    }

    fn diagnostic_message_ptr(&self, index: usize) -> *const c_char {
        self.diagnostic_messages
            .get(index)
            .map_or(std::ptr::null(), |message| message.as_ptr())
    }

    fn diagnostic_detail(&self, index: usize) -> Option<&OpforgeCachedDiagnostic> {
        self.diagnostic_details.get(index)
    }

    fn diagnostic_code_ptr(&self, index: usize) -> *const c_char {
        self.diagnostic_detail(index)
            .map_or(std::ptr::null(), |detail| detail.code.as_ptr())
    }

    fn diagnostic_file_ptr(&self, index: usize) -> *const c_char {
        self.diagnostic_detail(index)
            .and_then(|detail| detail.file.as_ref())
            .map_or(std::ptr::null(), |file| file.as_ptr())
    }

    fn diagnostic_related_span_file_ptr(
        &self,
        diag_index: usize,
        span_index: usize,
    ) -> *const c_char {
        self.diagnostic_detail(diag_index)
            .and_then(|detail| detail.related_spans.get(span_index))
            .and_then(|span| span.file.as_ref())
            .map_or(std::ptr::null(), |file| file.as_ptr())
    }

    fn diagnostic_related_span_label_ptr(
        &self,
        diag_index: usize,
        span_index: usize,
    ) -> *const c_char {
        self.diagnostic_detail(diag_index)
            .and_then(|detail| detail.related_spans.get(span_index))
            .and_then(|span| span.label.as_ref())
            .map_or(std::ptr::null(), |label| label.as_ptr())
    }

    fn diagnostic_note_ptr(&self, diag_index: usize, note_index: usize) -> *const c_char {
        self.diagnostic_detail(diag_index)
            .and_then(|detail| detail.notes.get(note_index))
            .map_or(std::ptr::null(), |note| note.as_ptr())
    }

    fn diagnostic_help_ptr(&self, diag_index: usize, help_index: usize) -> *const c_char {
        self.diagnostic_detail(diag_index)
            .and_then(|detail| detail.help.get(help_index))
            .map_or(std::ptr::null(), |help| help.as_ptr())
    }

    fn diagnostic_fixit_file_ptr(&self, diag_index: usize, fixit_index: usize) -> *const c_char {
        self.diagnostic_detail(diag_index)
            .and_then(|detail| detail.fixits.get(fixit_index))
            .and_then(|fixit| fixit.file.as_ref())
            .map_or(std::ptr::null(), |file| file.as_ptr())
    }

    fn diagnostic_fixit_replacement_ptr(
        &self,
        diag_index: usize,
        fixit_index: usize,
    ) -> *const c_char {
        self.diagnostic_detail(diag_index)
            .and_then(|detail| detail.fixits.get(fixit_index))
            .map_or(std::ptr::null(), |fixit| fixit.replacement.as_ptr())
    }

    fn diagnostic_fixit_applicability_ptr(
        &self,
        diag_index: usize,
        fixit_index: usize,
    ) -> *const c_char {
        self.diagnostic_detail(diag_index)
            .and_then(|detail| detail.fixits.get(fixit_index))
            .map_or(std::ptr::null(), |fixit| fixit.applicability.as_ptr())
    }
}

impl OpforgeCachedDiagnostic {
    fn from_diagnostic(diag: &Diagnostic) -> Self {
        Self {
            code: cstring(diag.code()),
            file: diag.file().map(cstring),
            related_spans: diag
                .related_spans()
                .iter()
                .map(|span| OpforgeCachedRelatedSpan {
                    file: span.file.as_deref().map(cstring),
                    label: span.label.as_deref().map(cstring),
                })
                .collect(),
            notes: diag
                .notes()
                .iter()
                .map(|note| cstring(note.as_str()))
                .collect(),
            help: diag
                .help()
                .iter()
                .map(|help| cstring(help.as_str()))
                .collect(),
            fixits: diag
                .fixits()
                .iter()
                .map(|fixit| OpforgeCachedFixit {
                    file: fixit.file.as_deref().map(cstring),
                    replacement: cstring(fixit.replacement.as_str()),
                    applicability: cstring(fixit.applicability.as_str()),
                })
                .collect(),
        }
    }
}

impl OpforgeOpcoreTokenizeReport {
    fn ok(tokenized: api::opcore::portable::TokenizedLine) -> Self {
        let token_texts = tokenized
            .tokens
            .iter()
            .map(|token| {
                CString::new(portable_token_text(token).replace('\0', " "))
                    .expect("sanitized token text should not contain NULs")
            })
            .collect();
        Self {
            status: OpforgeProcessorStatus::Ok,
            tokens: tokenized.tokens,
            token_texts,
            error_message: None,
            error_line: 0,
            error_col_start: 0,
            error_col_end: 0,
        }
    }

    fn error(
        status: OpforgeProcessorStatus,
        message: impl Into<String>,
        line: u32,
        col_start: usize,
        col_end: usize,
    ) -> Self {
        Self {
            status,
            tokens: Vec::new(),
            token_texts: Vec::new(),
            error_message: Some(
                CString::new(message.into().replace('\0', " "))
                    .expect("sanitized tokenize error should not contain NULs"),
            ),
            error_line: line,
            error_col_start: col_start,
            error_col_end: col_end,
        }
    }

    fn error_message_ptr(&self) -> *const c_char {
        self.error_message
            .as_ref()
            .map_or(std::ptr::null(), |message| message.as_ptr())
    }

    fn token_text_ptr(&self, index: usize) -> *const c_char {
        self.token_texts
            .get(index)
            .map_or(std::ptr::null(), |text| text.as_ptr())
    }

    fn ok_opasm(tokenized: api::asm::opasm::portable::TokenizedStatement) -> Self {
        let token_texts = tokenized
            .tokens
            .iter()
            .map(|token| {
                CString::new(portable_token_text(token).replace('\0', " "))
                    .expect("sanitized token text should not contain NULs")
            })
            .collect();
        Self {
            status: OpforgeProcessorStatus::Ok,
            tokens: tokenized.tokens,
            token_texts,
            error_message: None,
            error_line: 0,
            error_col_start: 0,
            error_col_end: 0,
        }
    }
}

impl OpforgeOpcoreExprReport {
    fn ok(expr: api::opcore::portable::PortableAstExpr) -> Self {
        let mut report = Self {
            status: OpforgeProcessorStatus::Ok,
            nodes: Vec::new(),
            child_edges: Vec::new(),
            texts: Vec::new(),
            error_message: None,
            error_line: 0,
            error_col_start: 0,
            error_col_end: 0,
        };
        report.push_expr(&expr);
        report
    }

    fn error(
        status: OpforgeProcessorStatus,
        message: impl Into<String>,
        line: u32,
        col_start: usize,
        col_end: usize,
    ) -> Self {
        Self {
            status,
            nodes: Vec::new(),
            child_edges: Vec::new(),
            texts: Vec::new(),
            error_message: Some(
                CString::new(message.into().replace('\0', " "))
                    .expect("sanitized parse error should not contain NULs"),
            ),
            error_line: line,
            error_col_start: col_start,
            error_col_end: col_end,
        }
    }

    fn error_message_ptr(&self) -> *const c_char {
        self.error_message
            .as_ref()
            .map_or(std::ptr::null(), |message| message.as_ptr())
    }

    fn text_ptr(&self, index: Option<usize>) -> *const c_char {
        index
            .and_then(|idx| self.texts.get(idx))
            .map_or(std::ptr::null(), |text| text.as_ptr())
    }

    fn intern_text(&mut self, text: impl Into<String>) -> usize {
        let sanitized = text.into().replace('\0', " ");
        self.texts.push(
            CString::new(sanitized).expect("sanitized expression text should not contain NULs"),
        );
        self.texts.len() - 1
    }

    fn span_parts(expr: &api::opcore::portable::PortableAstExpr) -> (u32, usize, usize) {
        match expr {
            api::opcore::portable::PortableAstExpr::Number(_, span)
            | api::opcore::portable::PortableAstExpr::Identifier(_, span)
            | api::opcore::portable::PortableAstExpr::Register(_, span)
            | api::opcore::portable::PortableAstExpr::List(_, span)
            | api::opcore::portable::PortableAstExpr::Index { span, .. }
            | api::opcore::portable::PortableAstExpr::Member { span, .. }
            | api::opcore::portable::PortableAstExpr::StructLiteral { span, .. }
            | api::opcore::portable::PortableAstExpr::Call { span, .. }
            | api::opcore::portable::PortableAstExpr::Placeholder(span)
            | api::opcore::portable::PortableAstExpr::Indirect(_, span)
            | api::opcore::portable::PortableAstExpr::Dollar(span)
            | api::opcore::portable::PortableAstExpr::String(_, span)
            | api::opcore::portable::PortableAstExpr::Immediate(_, span)
            | api::opcore::portable::PortableAstExpr::IndirectLong(_, span)
            | api::opcore::portable::PortableAstExpr::Tuple(_, span)
            | api::opcore::portable::PortableAstExpr::Error(_, span)
            | api::opcore::portable::PortableAstExpr::Ternary { span, .. }
            | api::opcore::portable::PortableAstExpr::Unary { span, .. }
            | api::opcore::portable::PortableAstExpr::Binary { span, .. }
            | api::opcore::portable::PortableAstExpr::Range { span, .. } => {
                (span.line, span.col_start, span.col_end)
            }
        }
    }

    fn push_expr(&mut self, expr: &api::opcore::portable::PortableAstExpr) -> usize {
        let (kind, text_index) = match expr {
            api::opcore::portable::PortableAstExpr::Number(text, _) => (
                OpforgeExprNodeKind::Number,
                Some(self.intern_text(text.clone())),
            ),
            api::opcore::portable::PortableAstExpr::Identifier(name, _) => (
                OpforgeExprNodeKind::Identifier,
                Some(self.intern_text(name.clone())),
            ),
            api::opcore::portable::PortableAstExpr::Register(name, _) => (
                OpforgeExprNodeKind::Register,
                Some(self.intern_text(name.clone())),
            ),
            api::opcore::portable::PortableAstExpr::List(_, _) => (OpforgeExprNodeKind::List, None),
            api::opcore::portable::PortableAstExpr::Index { .. } => {
                (OpforgeExprNodeKind::Index, None)
            }
            api::opcore::portable::PortableAstExpr::Member { field, .. } => (
                OpforgeExprNodeKind::Member,
                Some(self.intern_text(field.clone())),
            ),
            api::opcore::portable::PortableAstExpr::StructLiteral { type_name, .. } => (
                OpforgeExprNodeKind::StructLiteral,
                Some(self.intern_text(type_name.clone())),
            ),
            api::opcore::portable::PortableAstExpr::Call { name, .. } => (
                OpforgeExprNodeKind::Call,
                Some(self.intern_text(name.clone())),
            ),
            api::opcore::portable::PortableAstExpr::Placeholder(_) => {
                (OpforgeExprNodeKind::Placeholder, None)
            }
            api::opcore::portable::PortableAstExpr::Indirect(_, _) => {
                (OpforgeExprNodeKind::Indirect, None)
            }
            api::opcore::portable::PortableAstExpr::Dollar(_) => {
                (OpforgeExprNodeKind::Dollar, None)
            }
            api::opcore::portable::PortableAstExpr::String(bytes, _) => (
                OpforgeExprNodeKind::String,
                Some(self.intern_text(String::from_utf8_lossy(bytes).to_string())),
            ),
            api::opcore::portable::PortableAstExpr::Immediate(_, _) => {
                (OpforgeExprNodeKind::Immediate, None)
            }
            api::opcore::portable::PortableAstExpr::IndirectLong(_, _) => {
                (OpforgeExprNodeKind::IndirectLong, None)
            }
            api::opcore::portable::PortableAstExpr::Tuple(_, _) => {
                (OpforgeExprNodeKind::Tuple, None)
            }
            api::opcore::portable::PortableAstExpr::Error(message, _) => (
                OpforgeExprNodeKind::Error,
                Some(self.intern_text(message.clone())),
            ),
            api::opcore::portable::PortableAstExpr::Ternary { .. } => {
                (OpforgeExprNodeKind::Ternary, None)
            }
            api::opcore::portable::PortableAstExpr::Unary { op, .. } => (
                OpforgeExprNodeKind::Unary,
                Some(self.intern_text(format!("{op:?}"))),
            ),
            api::opcore::portable::PortableAstExpr::Binary { op, .. } => (
                OpforgeExprNodeKind::Binary,
                Some(self.intern_text(format!("{op:?}"))),
            ),
            api::opcore::portable::PortableAstExpr::Range { inclusive, .. } => (
                OpforgeExprNodeKind::Range,
                Some(self.intern_text(if *inclusive { "..=" } else { ".." })),
            ),
        };
        let (line, col_start, col_end) = Self::span_parts(expr);
        let node_index = self.nodes.len();
        self.nodes.push(OpforgeExprNodeRecord {
            kind,
            line,
            col_start,
            col_end,
            text_index,
            child_start: self.child_edges.len(),
            child_len: 0,
        });

        match expr {
            api::opcore::portable::PortableAstExpr::List(items, _)
            | api::opcore::portable::PortableAstExpr::Tuple(items, _) => {
                for item in items {
                    let child_index = self.push_expr(item);
                    self.child_edges.push(OpforgeExprChildEdge {
                        child_index,
                        label_text_index: None,
                    });
                }
            }
            api::opcore::portable::PortableAstExpr::Index { base, index, .. } => {
                for child in [base.as_ref(), index.as_ref()] {
                    let child_index = self.push_expr(child);
                    self.child_edges.push(OpforgeExprChildEdge {
                        child_index,
                        label_text_index: None,
                    });
                }
            }
            api::opcore::portable::PortableAstExpr::Member { base, .. }
            | api::opcore::portable::PortableAstExpr::Indirect(base, _)
            | api::opcore::portable::PortableAstExpr::Immediate(base, _)
            | api::opcore::portable::PortableAstExpr::IndirectLong(base, _)
            | api::opcore::portable::PortableAstExpr::Unary { expr: base, .. } => {
                let child_index = self.push_expr(base);
                self.child_edges.push(OpforgeExprChildEdge {
                    child_index,
                    label_text_index: None,
                });
            }
            api::opcore::portable::PortableAstExpr::StructLiteral { fields, .. } => {
                for (field_name, value) in fields {
                    let child_index = self.push_expr(value);
                    let label_text_index = Some(self.intern_text(field_name.clone()));
                    self.child_edges.push(OpforgeExprChildEdge {
                        child_index,
                        label_text_index,
                    });
                }
            }
            api::opcore::portable::PortableAstExpr::Call { args, .. } => {
                for arg in args {
                    let child_index = self.push_expr(arg);
                    self.child_edges.push(OpforgeExprChildEdge {
                        child_index,
                        label_text_index: None,
                    });
                }
            }
            api::opcore::portable::PortableAstExpr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => {
                for child in [cond.as_ref(), then_expr.as_ref(), else_expr.as_ref()] {
                    let child_index = self.push_expr(child);
                    self.child_edges.push(OpforgeExprChildEdge {
                        child_index,
                        label_text_index: None,
                    });
                }
            }
            api::opcore::portable::PortableAstExpr::Binary { left, right, .. } => {
                for child in [left.as_ref(), right.as_ref()] {
                    let child_index = self.push_expr(child);
                    self.child_edges.push(OpforgeExprChildEdge {
                        child_index,
                        label_text_index: None,
                    });
                }
            }
            api::opcore::portable::PortableAstExpr::Range {
                start, end, step, ..
            } => {
                for child in [Some(start.as_ref()), Some(end.as_ref()), step.as_deref()]
                    .into_iter()
                    .flatten()
                {
                    let child_index = self.push_expr(child);
                    self.child_edges.push(OpforgeExprChildEdge {
                        child_index,
                        label_text_index: None,
                    });
                }
            }
            api::opcore::portable::PortableAstExpr::Number(_, _)
            | api::opcore::portable::PortableAstExpr::Identifier(_, _)
            | api::opcore::portable::PortableAstExpr::Register(_, _)
            | api::opcore::portable::PortableAstExpr::Placeholder(_)
            | api::opcore::portable::PortableAstExpr::Dollar(_)
            | api::opcore::portable::PortableAstExpr::String(_, _)
            | api::opcore::portable::PortableAstExpr::Error(_, _) => {}
        }

        self.nodes[node_index].child_len =
            self.child_edges.len() - self.nodes[node_index].child_start;
        node_index
    }
}

impl OpforgeOpcoreModuleItemReport {
    fn ok(line_ast: api::opcore::portable::PortableLineAst) -> Self {
        let mut report = Self {
            status: OpforgeProcessorStatus::Ok,
            line_kind: map_line_kind(&line_ast),
            use_module_id: None,
            use_alias: None,
            use_item_names: Vec::new(),
            statement_mnemonic: None,
            statement_operand_texts: Vec::new(),
            error_message: None,
            error_line: 0,
            error_col_start: 0,
            error_col_end: 0,
        };

        match line_ast {
            api::opcore::portable::PortableLineAst::Use {
                module_id,
                alias,
                items,
                ..
            } => {
                report.use_module_id = Some(report.intern_text(module_id));
                report.use_alias = alias.map(|alias| report.intern_text(alias));
                report.use_item_names = items
                    .into_iter()
                    .map(|item| report.intern_text(item.name))
                    .collect();
            }
            api::opcore::portable::PortableLineAst::Statement {
                mnemonic, operands, ..
            } => {
                report.statement_mnemonic = mnemonic.map(|mnemonic| report.intern_text(mnemonic));
                report.statement_operand_texts = operands
                    .iter()
                    .map(portable_expr_text)
                    .map(|text| report.intern_text(text))
                    .collect();
            }
            _ => {}
        }

        report
    }

    fn returned() -> Self {
        Self {
            status: OpforgeProcessorStatus::Returned,
            line_kind: OpforgeLineAstKind::Invalid,
            use_module_id: None,
            use_alias: None,
            use_item_names: Vec::new(),
            statement_mnemonic: None,
            statement_operand_texts: Vec::new(),
            error_message: None,
            error_line: 0,
            error_col_start: 0,
            error_col_end: 0,
        }
    }

    fn error(message: impl Into<String>, line: u32, col_start: usize, col_end: usize) -> Self {
        Self {
            status: OpforgeProcessorStatus::ParseError,
            line_kind: OpforgeLineAstKind::Invalid,
            use_module_id: None,
            use_alias: None,
            use_item_names: Vec::new(),
            statement_mnemonic: None,
            statement_operand_texts: Vec::new(),
            error_message: Some(
                CString::new(message.into().replace('\0', " "))
                    .expect("sanitized module-item error should not contain NULs"),
            ),
            error_line: line,
            error_col_start: col_start,
            error_col_end: col_end,
        }
    }

    fn intern_text(&self, text: impl Into<String>) -> CString {
        CString::new(text.into().replace('\0', " "))
            .expect("sanitized module-item text should not contain NULs")
    }

    fn text_ptr(text: Option<&CString>) -> *const c_char {
        text.map_or(std::ptr::null(), |text| text.as_ptr())
    }

    fn error_message_ptr(&self) -> *const c_char {
        Self::text_ptr(self.error_message.as_ref())
    }

    fn use_module_id_ptr(&self) -> *const c_char {
        Self::text_ptr(self.use_module_id.as_ref())
    }

    fn use_alias_ptr(&self) -> *const c_char {
        Self::text_ptr(self.use_alias.as_ref())
    }

    fn use_item_name_ptr(&self, index: usize) -> *const c_char {
        Self::text_ptr(self.use_item_names.get(index))
    }

    fn statement_mnemonic_ptr(&self) -> *const c_char {
        Self::text_ptr(self.statement_mnemonic.as_ref())
    }

    fn statement_operand_text_ptr(&self, index: usize) -> *const c_char {
        Self::text_ptr(self.statement_operand_texts.get(index))
    }
}

fn portable_token_text(token: &api::opcore::portable::PortableToken) -> String {
    match &token.kind {
        api::opcore::portable::PortableTokenKind::Identifier(name)
        | api::opcore::portable::PortableTokenKind::Register(name) => name.clone(),
        api::opcore::portable::PortableTokenKind::Number { text, .. } => text.clone(),
        api::opcore::portable::PortableTokenKind::String { raw, .. } => raw.clone(),
        api::opcore::portable::PortableTokenKind::Comma => ",".to_string(),
        api::opcore::portable::PortableTokenKind::Colon => ":".to_string(),
        api::opcore::portable::PortableTokenKind::Dollar => "$".to_string(),
        api::opcore::portable::PortableTokenKind::Dot => ".".to_string(),
        api::opcore::portable::PortableTokenKind::Hash => "#".to_string(),
        api::opcore::portable::PortableTokenKind::Question => "?".to_string(),
        api::opcore::portable::PortableTokenKind::OpenBracket => "[".to_string(),
        api::opcore::portable::PortableTokenKind::CloseBracket => "]".to_string(),
        api::opcore::portable::PortableTokenKind::OpenBrace => "{".to_string(),
        api::opcore::portable::PortableTokenKind::CloseBrace => "}".to_string(),
        api::opcore::portable::PortableTokenKind::OpenParen => "(".to_string(),
        api::opcore::portable::PortableTokenKind::CloseParen => ")".to_string(),
        api::opcore::portable::PortableTokenKind::Operator(op) => match op {
            api::opcore::portable::PortableOperatorKind::Range => "..".to_string(),
            api::opcore::portable::PortableOperatorKind::RangeInclusive => "..=".to_string(),
            api::opcore::portable::PortableOperatorKind::Plus => "+".to_string(),
            api::opcore::portable::PortableOperatorKind::Minus => "-".to_string(),
            api::opcore::portable::PortableOperatorKind::Multiply => "*".to_string(),
            api::opcore::portable::PortableOperatorKind::Power => "^".to_string(),
            api::opcore::portable::PortableOperatorKind::Divide => "/".to_string(),
            api::opcore::portable::PortableOperatorKind::Mod => "%".to_string(),
            api::opcore::portable::PortableOperatorKind::Shl => "<<".to_string(),
            api::opcore::portable::PortableOperatorKind::Shr => ">>".to_string(),
            api::opcore::portable::PortableOperatorKind::BitNot => "~".to_string(),
            api::opcore::portable::PortableOperatorKind::LogicNot => "!".to_string(),
            api::opcore::portable::PortableOperatorKind::BitAnd => "&".to_string(),
            api::opcore::portable::PortableOperatorKind::BitOr => "|".to_string(),
            api::opcore::portable::PortableOperatorKind::BitXor => "^".to_string(),
            api::opcore::portable::PortableOperatorKind::LogicAnd => "&&".to_string(),
            api::opcore::portable::PortableOperatorKind::LogicOr => "||".to_string(),
            api::opcore::portable::PortableOperatorKind::LogicXor => "^^".to_string(),
            api::opcore::portable::PortableOperatorKind::Eq => "==".to_string(),
            api::opcore::portable::PortableOperatorKind::Ne => "!=".to_string(),
            api::opcore::portable::PortableOperatorKind::Ge => ">=".to_string(),
            api::opcore::portable::PortableOperatorKind::Gt => ">".to_string(),
            api::opcore::portable::PortableOperatorKind::Le => "<=".to_string(),
            api::opcore::portable::PortableOperatorKind::Lt => "<".to_string(),
        },
    }
}

fn portable_expr_text(expr: &api::opcore::portable::PortableAstExpr) -> String {
    match expr {
        api::opcore::portable::PortableAstExpr::Number(text, _) => text.clone(),
        api::opcore::portable::PortableAstExpr::Identifier(name, _) => name.clone(),
        api::opcore::portable::PortableAstExpr::Register(name, _) => name.clone(),
        api::opcore::portable::PortableAstExpr::List(items, _) => format!(
            "{{{}}}",
            items
                .iter()
                .map(portable_expr_text)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        api::opcore::portable::PortableAstExpr::Index { base, index, .. } => {
            format!(
                "{}[{}]",
                portable_expr_text(base),
                portable_expr_text(index)
            )
        }
        api::opcore::portable::PortableAstExpr::Member { base, field, .. } => {
            format!("{}.{}", portable_expr_text(base), field)
        }
        api::opcore::portable::PortableAstExpr::StructLiteral {
            type_name, fields, ..
        } => format!(
            "{}{{{}}}",
            type_name,
            fields
                .iter()
                .map(|(name, value)| format!("{name}:{}", portable_expr_text(value)))
                .collect::<Vec<_>>()
                .join(",")
        ),
        api::opcore::portable::PortableAstExpr::Call { name, args, .. } => format!(
            "{}({})",
            name,
            args.iter()
                .map(portable_expr_text)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        api::opcore::portable::PortableAstExpr::Placeholder(_) => "?".to_string(),
        api::opcore::portable::PortableAstExpr::Indirect(inner, _) => {
            format!("({})", portable_expr_text(inner))
        }
        api::opcore::portable::PortableAstExpr::Dollar(_) => "$".to_string(),
        api::opcore::portable::PortableAstExpr::String(bytes, _) => {
            String::from_utf8_lossy(bytes).to_string()
        }
        api::opcore::portable::PortableAstExpr::Immediate(inner, _) => {
            format!("#{}", portable_expr_text(inner))
        }
        api::opcore::portable::PortableAstExpr::IndirectLong(inner, _) => {
            format!("[{}]", portable_expr_text(inner))
        }
        api::opcore::portable::PortableAstExpr::Tuple(items, _) => format!(
            "({})",
            items
                .iter()
                .map(portable_expr_text)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        api::opcore::portable::PortableAstExpr::Error(message, _) => message.clone(),
        api::opcore::portable::PortableAstExpr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => format!(
            "{} ? {} : {}",
            portable_expr_text(cond),
            portable_expr_text(then_expr),
            portable_expr_text(else_expr)
        ),
        api::opcore::portable::PortableAstExpr::Unary { op, expr, .. } => {
            format!("{op:?} {}", portable_expr_text(expr))
        }
        api::opcore::portable::PortableAstExpr::Binary {
            left, op, right, ..
        } => format!(
            "{} {:?} {}",
            portable_expr_text(left),
            op,
            portable_expr_text(right)
        ),
        api::opcore::portable::PortableAstExpr::Range {
            start,
            end,
            step,
            inclusive,
            ..
        } => {
            let range_op = if *inclusive { "..=" } else { ".." };
            let mut text = format!(
                "{}{}{}",
                portable_expr_text(start),
                range_op,
                portable_expr_text(end)
            );
            if let Some(step) = step {
                text.push(':');
                text.push_str(&portable_expr_text(step));
            }
            text
        }
    }
}

fn map_line_kind(line_ast: &api::opcore::portable::PortableLineAst) -> OpforgeLineAstKind {
    match line_ast {
        api::opcore::portable::PortableLineAst::Empty => OpforgeLineAstKind::Empty,
        api::opcore::portable::PortableLineAst::Conditional { .. } => {
            OpforgeLineAstKind::Conditional
        }
        api::opcore::portable::PortableLineAst::Place { .. } => OpforgeLineAstKind::Place,
        api::opcore::portable::PortableLineAst::Pack { .. } => OpforgeLineAstKind::Pack,
        api::opcore::portable::PortableLineAst::Use { .. } => OpforgeLineAstKind::Use,
        api::opcore::portable::PortableLineAst::StatementDef { .. } => {
            OpforgeLineAstKind::StatementDef
        }
        api::opcore::portable::PortableLineAst::StatementEnd { .. } => {
            OpforgeLineAstKind::StatementEnd
        }
        api::opcore::portable::PortableLineAst::Assignment { .. } => OpforgeLineAstKind::Assignment,
        api::opcore::portable::PortableLineAst::Statement { .. } => OpforgeLineAstKind::Statement,
    }
}

fn opt_c_str<'a>(ptr: *const c_char) -> Result<Option<&'a str>, String> {
    if ptr.is_null() {
        return Ok(None);
    }
    // SAFETY: caller promises a valid NUL-terminated string for non-null pointers.
    let c_str = unsafe { CStr::from_ptr(ptr) };
    let text = c_str
        .to_str()
        .map_err(|_| "input string must be valid UTF-8".to_string())?;
    Ok(Some(text))
}

fn derive_input_base(root_path: &Path) -> String {
    root_path.with_extension("").to_string_lossy().into_owned()
}

fn parse_string_list(list: OpforgeStringList, field_name: &str) -> Result<Vec<String>, String> {
    if list.count == 0 {
        return Ok(Vec::new());
    }
    if list.items.is_null() {
        return Err(format!(
            "{field_name}.items must not be null when count > 0"
        ));
    }
    // SAFETY: caller promises `items` points to `count` entries when non-null.
    let items = unsafe { std::slice::from_raw_parts(list.items, list.count) };
    let mut values = Vec::with_capacity(items.len());
    for (index, item) in items.iter().enumerate() {
        let text =
            opt_c_str(*item)?.ok_or_else(|| format!("{field_name}[{index}] must not be null"))?;
        values.push(text.to_string());
    }
    Ok(values)
}

fn parse_path_list(list: OpforgeStringList, field_name: &str) -> Result<Vec<PathBuf>, String> {
    parse_string_list(list, field_name).map(|items| items.into_iter().map(PathBuf::from).collect())
}

fn map_execution_mode(mode: u32) -> Result<ExecutionMode, String> {
    match mode {
        OPFORGE_EXECUTION_MODE_DEFAULT => {
            Ok(api::asm::OwnedExecutionOptions::default().execution_mode)
        }
        OPFORGE_EXECUTION_MODE_RUST => Ok(ExecutionMode::Rust),
        OPFORGE_EXECUTION_MODE_VM => Ok(ExecutionMode::Vm),
        OPFORGE_EXECUTION_MODE_LOCKSTEP_RUST => Ok(ExecutionMode::Lockstep {
            continuation_head: api::lockstep::ContinuationHead::Rust,
        }),
        OPFORGE_EXECUTION_MODE_LOCKSTEP_VM => Ok(ExecutionMode::Lockstep {
            continuation_head: api::lockstep::ContinuationHead::Vm,
        }),
        _ => Err(format!("execution_mode value {mode} is invalid")),
    }
}

fn map_default_outputs(mode: u8) -> Result<bool, String> {
    match mode {
        OPFORGE_DEFAULT_OUTPUTS_DEFAULT => {
            Ok(api::asm::OwnedOutputOptions::default().default_outputs)
        }
        OPFORGE_DEFAULT_OUTPUTS_DISABLE => Ok(false),
        OPFORGE_DEFAULT_OUTPUTS_ENABLE => Ok(true),
        _ => Err(format!("emit_outputs value {mode} is invalid")),
    }
}

fn map_output_format(mode: u32) -> Result<OutputFormat, String> {
    match mode {
        OPFORGE_OUTPUT_FORMAT_TEXT => Ok(OutputFormat::Text),
        OPFORGE_OUTPUT_FORMAT_JSON => Ok(OutputFormat::Json),
        _ => Err(format!("output_format value {mode} is invalid")),
    }
}

fn map_label_output_format(mode: u32) -> Result<LabelOutputFormat, String> {
    match mode {
        OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT => {
            Ok(api::asm::OwnedOutputOptions::default().label_output_format)
        }
        OPFORGE_LABEL_OUTPUT_FORMAT_VICE => Ok(LabelOutputFormat::Vice),
        OPFORGE_LABEL_OUTPUT_FORMAT_CTAGS => Ok(LabelOutputFormat::Ctags),
        _ => Err(format!("label_output_format value {mode} is invalid")),
    }
}

fn build_grouped_high_level_config(
    request: &OpforgeAsmRequest,
) -> Result<(PathBuf, api::asm::OwnedAssemblerConfig), FfiAsmReportError> {
    let root_path = match opt_c_str(request.source.root_path) {
        Ok(Some(path)) => PathBuf::from(path),
        Ok(None) => return Err(invalid_request_report("source.root_path must not be null")),
        Err(err) => return Err(invalid_request_report(err)),
    };
    let output_base_storage = match opt_c_str(request.source.output_base) {
        Ok(Some(text)) if !text.is_empty() => text.to_string(),
        Ok(_) => derive_input_base(&root_path),
        Err(err) => return Err(invalid_request_report(err)),
    };
    let defines = match parse_string_list(request.source.defines, "source.defines") {
        Ok(defines) => defines,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let include_paths = match parse_path_list(request.source.include_paths, "source.include_paths")
    {
        Ok(paths) => paths,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let module_paths = match parse_path_list(request.source.module_paths, "source.module_paths") {
        Ok(paths) => paths,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let execution_mode = match map_execution_mode(request.execution.execution_mode) {
        Ok(mode) => mode,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let cpu_override = match opt_c_str(request.execution.cpu_override) {
        Ok(Some(cpu)) if !cpu.is_empty() => Some(cpu.to_string()),
        Ok(_) => None,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let opasm_package_path = match opt_c_str(request.execution.opasm_package_path) {
        Ok(Some(path)) if !path.is_empty() => Some(PathBuf::from(path)),
        Ok(_) => None,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let out_dir_storage = match opt_c_str(request.output.out_dir) {
        Ok(Some(path)) if !path.is_empty() => Some(PathBuf::from(path)),
        Ok(_) => None,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let labels_file = match opt_c_str(request.output.labels_file) {
        Ok(Some(path)) if !path.is_empty() => Some(PathBuf::from(path)),
        Ok(_) => None,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let dependency_output = match opt_c_str(request.output.dependency_output_path) {
        Ok(Some(path)) if !path.is_empty() => Some(api::asm::DependencyOutputPolicy {
            path: PathBuf::from(path),
            append: request.output.dependency_append != 0,
            make_phony: request.output.dependency_make_phony != 0,
        }),
        Ok(_) => None,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let default_outputs = match map_default_outputs(request.output.emit_outputs) {
        Ok(default_outputs) => default_outputs,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let output_format = match map_output_format(request.output.output_format) {
        Ok(format) => format,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let go_addr = match opt_c_str(request.output.go_addr) {
        Ok(Some(text)) if !text.is_empty() => Some(text.to_string()),
        Ok(_) => None,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let bin_specs = match parse_string_list(request.output.bin_specs, "output.bin_specs") {
        Ok(items) => {
            let mut parsed = Vec::with_capacity(items.len());
            for item in items {
                match parse_bin_output_arg(item.as_str()) {
                    Ok(spec) => parsed.push(spec),
                    Err(err) => {
                        return Err(invalid_request_report(format!(
                            "output.bin_specs entry is invalid: {err}"
                        )))
                    }
                }
            }
            parsed
        }
        Err(err) => return Err(invalid_request_report(err)),
    };
    let label_output_format = match map_label_output_format(request.output.label_output_format) {
        Ok(format) => format,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let outfile_override = match opt_c_str(request.output.outfile_override) {
        Ok(Some(text)) if !text.is_empty() => Some(text.to_string()),
        Ok(_) => None,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let list_name_override = match opt_c_str(request.output.list_name_override) {
        Ok(Some(text)) if !text.is_empty() => Some(text.to_string()),
        Ok(_) => None,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let hex_name_override = match opt_c_str(request.output.hex_name_override) {
        Ok(Some(text)) if !text.is_empty() => Some(text.to_string()),
        Ok(_) => None,
        Err(err) => return Err(invalid_request_report(err)),
    };
    let header_title = match opt_c_str(request.output.header_title) {
        Ok(Some(text)) if !text.is_empty() => text.to_string(),
        Ok(_) => api::asm::OwnedOutputOptions::default().header_title,
        Err(err) => return Err(invalid_request_report(err)),
    };

    let mut config = api::asm::OwnedAssemblerConfig::default();
    config.source.output_base = output_base_storage;
    config.source.defines = defines;
    config.source.include_paths = include_paths;
    config.source.module_paths = module_paths;
    config.source.pp_macro_depth = if request.source.pp_macro_depth == 0 {
        api::asm::OwnedSourceOptions::default().pp_macro_depth
    } else {
        request.source.pp_macro_depth
    };

    config.execution.execution_mode = execution_mode;
    config.execution.cpu_override = cpu_override;
    config.execution.max_loop_iterations = if request.execution.max_loop_iterations == 0 {
        api::asm::OwnedExecutionOptions::default().max_loop_iterations
    } else {
        request.execution.max_loop_iterations
    };
    config.execution.opasm_package_path = opasm_package_path;

    config.output.out_dir = out_dir_storage;
    config.output.output_format = output_format;
    config.output.go_addr = go_addr;
    config.output.bin_specs = bin_specs;
    config.output.fill_byte = request.output.fill_byte;
    config.output.fill_byte_set = request.output.fill_byte_set != 0;
    config.output.default_outputs = default_outputs;
    config.output.labels_file = labels_file;
    config.output.label_output_format = label_output_format;
    config.output.dependency_output = dependency_output;
    config.output.outfile_override = outfile_override;
    config.output.list_name_override = list_name_override;
    config.output.hex_name_override = hex_name_override;
    config.output.header_title = header_title;
    config.output.no_outputs = request.output.no_outputs != 0;

    config.diagnostics.debug_conditionals = request.diagnostics.debug_conditionals != 0;
    config.diagnostics.tab_size = if request.diagnostics.tab_size == 0 {
        None
    } else {
        Some(request.diagnostics.tab_size)
    };

    Ok((root_path, config))
}

fn build_opasm_processor(
    execution_mode: u32,
    cpu_id: *const c_char,
    dialect_override: *const c_char,
) -> Result<api::asm::opasm::portable::Processor, String> {
    let execution_mode = map_execution_mode(execution_mode)?;
    let mut builder =
        api::asm::opasm::portable::ProcessorBuilder::new().execution_mode(execution_mode);

    if let Some(cpu_id) = opt_c_str(cpu_id)? {
        if !cpu_id.is_empty() {
            builder = builder.cpu_id(cpu_id);
        }
    }
    if let Some(dialect_override) = opt_c_str(dialect_override)? {
        if !dialect_override.is_empty() {
            builder = builder.dialect_override(dialect_override);
        }
    }

    builder.build().map_err(|err| err.to_string())
}

fn diagnostic_counts(diagnostics: &[Diagnostic]) -> (usize, usize) {
    let error_count = diagnostics
        .iter()
        .filter(|diag| diag.severity() == Severity::Error)
        .count();
    let warning_count = diagnostics
        .iter()
        .filter(|diag| diag.severity() == Severity::Warning)
        .count();
    (error_count, warning_count)
}

fn asm_report_from_workflow_result(
    result: Result<api::diagnostics::AsmRunReport, api::asm::AssemblerWorkflowError>,
) -> OpforgeAsmReport {
    match result {
        Ok(report) => OpforgeAsmReport::ok(
            report.diagnostics().to_vec(),
            report.error_count(),
            report.warning_count(),
            report.lockstep_report().matches().len(),
            report.lockstep_report().divergences().len(),
        ),
        Err(api::asm::AssemblerWorkflowError::Assemble(err)) => {
            let (error_count, warning_count) = diagnostic_counts(err.diagnostics());
            OpforgeAsmReport::error(
                OpforgeStatus::AssembleError,
                err.diagnostics().to_vec(),
                error_count,
                warning_count,
                err.lockstep_report().matches().len(),
                err.lockstep_report().divergences().len(),
                err.to_string(),
            )
        }
        Err(api::asm::AssemblerWorkflowError::InvalidArgument(err)) => OpforgeAsmReport::error(
            OpforgeStatus::InvalidRequest,
            Vec::new(),
            0,
            0,
            0,
            0,
            err.to_string(),
        ),
        Err(api::asm::AssemblerWorkflowError::InvalidRequest(err)) => OpforgeAsmReport::error(
            OpforgeStatus::InvalidRequest,
            Vec::new(),
            0,
            0,
            0,
            0,
            err.to_string(),
        ),
        Err(api::asm::AssemblerWorkflowError::Io(err)) => OpforgeAsmReport::error(
            OpforgeStatus::AssembleError,
            Vec::new(),
            0,
            0,
            0,
            0,
            err.to_string(),
        ),
        Err(api::asm::AssemblerWorkflowError::Internal(err)) => OpforgeAsmReport::error(
            OpforgeStatus::AssembleError,
            Vec::new(),
            0,
            0,
            0,
            0,
            err.to_string(),
        ),
    }
}

fn build_high_level_assembler_session_with_request(
    request: &OpforgeAsmRequest,
) -> Result<api::asm::AssemblerSession, FfiAsmReportError> {
    let (root_path, config) = build_grouped_high_level_config(request)?;
    Ok(api::asm::AssemblerSession::with_config(root_path, config))
}

fn run_high_level_assembler_with_request(
    request: &OpforgeAsmRequest,
    check_only: bool,
) -> Result<OpforgeAsmReport, FfiAsmReportError> {
    let session = build_high_level_assembler_session_with_request(request)?;
    let report = if check_only {
        asm_report_from_workflow_result(session.check())
    } else {
        asm_report_from_workflow_result(session.assemble())
    };
    match report.status {
        OpforgeStatus::Ok => Ok(report),
        _ => Err(boxed_asm_report(report)),
    }
}

fn run_high_level_assembler_in_memory_with_request(
    request: &OpforgeAsmRequest,
    source_text: *const c_char,
    callbacks: *const OpforgeOutputCallbacks,
    check_only: bool,
) -> Result<OpforgeAsmReport, FfiAsmReportError> {
    let (root_path, mut config) = build_grouped_high_level_config(request)?;
    let source_text = match opt_c_str(source_text) {
        Ok(Some(text)) => text.to_string(),
        Ok(None) => return Err(invalid_request_report("source_text must not be null")),
        Err(err) => return Err(invalid_request_report(err)),
    };
    let callbacks = callbacks_ref(callbacks);

    let source_provider = MemorySourceProvider::new()
        .with_file(root_path.clone(), source_text)
        .with_fs_fallback();
    let output_sink = MemoryOutputSink::new();
    config.source.source_provider = Some(std::sync::Arc::from(source_provider));
    config.output.output_sink = Some(std::sync::Arc::new(output_sink.clone()));

    let session = api::asm::AssemblerSession::with_config(root_path, config);
    let report = if check_only {
        asm_report_from_workflow_result(session.check())
    } else {
        asm_report_from_workflow_result(session.assemble())
    };

    if report.status == OpforgeStatus::Ok
        && callbacks.is_none()
        && memory_output_sink_has_outputs(&output_sink)
    {
        return Err(invalid_request_from_report(
            &report,
            "in-memory run produced outputs but no output callbacks were provided",
        ));
    }

    if report.status == OpforgeStatus::Ok {
        if let Some(callbacks) = callbacks {
            if let Err(err) = emit_memory_outputs_to_callbacks(callbacks, &output_sink) {
                return Err(boxed_asm_report(OpforgeAsmReport::error(
                    OpforgeStatus::AssembleError,
                    report.diagnostics.clone(),
                    report.error_count,
                    report.warning_count,
                    report.lockstep_match_count,
                    report.lockstep_divergence_count,
                    err,
                )));
            }
        }
    }

    match report.status {
        OpforgeStatus::Ok => Ok(report),
        _ => Err(boxed_asm_report(report)),
    }
}

fn into_report_handle(report: OpforgeAsmReport) -> *mut OpforgeAsmReport {
    Box::into_raw(Box::new(report))
}

fn into_session_handle(session: OpforgeAsmSession) -> *mut OpforgeAsmSession {
    Box::into_raw(Box::new(session))
}

fn into_prepared_session_handle(
    prepared: OpforgePreparedAsmSession,
) -> *mut OpforgePreparedAsmSession {
    Box::into_raw(Box::new(prepared))
}

fn ffi_panic_payload_message(payload: Box<dyn Any + Send>) -> String {
    if let Some(message) = payload.downcast_ref::<&str>() {
        (*message).to_string()
    } else if let Some(message) = payload.downcast_ref::<String>() {
        message.clone()
    } else {
        "panic payload was not a string".to_string()
    }
}

fn ffi_internal_error_report(function_name: &str, detail: impl Into<String>) -> OpforgeAsmReport {
    OpforgeAsmReport::error(
        OpforgeStatus::AssembleError,
        Vec::new(),
        1,
        0,
        0,
        0,
        format!(
            "internal libopforge panic in {function_name}: {}",
            detail.into()
        ),
    )
}

fn ffi_report_boundary(
    function_name: &'static str,
    body: impl FnOnce() -> *mut OpforgeAsmReport,
) -> *mut OpforgeAsmReport {
    match panic::catch_unwind(AssertUnwindSafe(body)) {
        Ok(report) => report,
        Err(payload) => into_report_handle(ffi_internal_error_report(
            function_name,
            ffi_panic_payload_message(payload),
        )),
    }
}

fn ffi_session_boundary(body: impl FnOnce() -> *mut OpforgeAsmSession) -> *mut OpforgeAsmSession {
    match panic::catch_unwind(AssertUnwindSafe(body)) {
        Ok(session) => session,
        Err(_) => std::ptr::null_mut(),
    }
}

fn ffi_prepared_session_boundary(
    function_name: &'static str,
    body: impl FnOnce() -> *mut OpforgePreparedAsmSession,
) -> *mut OpforgePreparedAsmSession {
    match panic::catch_unwind(AssertUnwindSafe(body)) {
        Ok(prepared) => prepared,
        Err(payload) => into_prepared_session_handle(OpforgePreparedAsmSession {
            prepared: None,
            failure: Some(ffi_internal_error_report(
                function_name,
                ffi_panic_payload_message(payload),
            )),
        }),
    }
}

#[cfg(any(test, feature = "panic-test-hooks"))]
mod ffi_test_hooks {
    use std::cell::Cell;
    use std::sync::atomic::{AtomicBool, Ordering};

    thread_local! {
        static PANIC_POINT: Cell<Option<&'static str>> = const { Cell::new(None) };
    }

    static PANIC_ASSEMBLE_FILE_WITH_REQUEST: AtomicBool = AtomicBool::new(false);

    #[cfg_attr(feature = "panic-test-hooks", allow(dead_code))]
    pub fn arm(point: &'static str) {
        PANIC_POINT.with(|slot| slot.set(Some(point)));
        if point == "opforge_asm_assemble_file_with_request" {
            PANIC_ASSEMBLE_FILE_WITH_REQUEST.store(true, Ordering::SeqCst);
        }
    }

    #[cfg(feature = "panic-test-hooks")]
    pub fn arm_assemble_file_with_request() {
        PANIC_ASSEMBLE_FILE_WITH_REQUEST.store(true, Ordering::SeqCst);
    }

    pub fn trigger(point: &'static str) {
        if point == "opforge_asm_assemble_file_with_request"
            && PANIC_ASSEMBLE_FILE_WITH_REQUEST.swap(false, Ordering::SeqCst)
        {
            panic!("forced ffi panic at {point}");
        }

        PANIC_POINT.with(|slot| {
            if slot.get() == Some(point) {
                slot.set(None);
                panic!("forced ffi panic at {point}");
            }
        });
    }
}

#[cfg(any(test, feature = "panic-test-hooks"))]
fn ffi_test_maybe_panic(point: &'static str) {
    ffi_test_hooks::trigger(point);
}

#[cfg(not(any(test, feature = "panic-test-hooks")))]
fn ffi_test_maybe_panic(_point: &'static str) {}

#[cfg(feature = "panic-test-hooks")]
#[no_mangle]
pub extern "C" fn opforge_test_force_next_assemble_file_with_request_panic() {
    ffi_test_hooks::arm_assemble_file_with_request();
}

fn report_ref<'a>(report: *const OpforgeAsmReport) -> Option<&'a OpforgeAsmReport> {
    if report.is_null() {
        return None;
    }
    // SAFETY: callers must pass a handle previously returned by this library.
    Some(unsafe { &*report })
}

fn asm_session_ref<'a>(session: *const OpforgeAsmSession) -> Option<&'a OpforgeAsmSession> {
    if session.is_null() {
        return None;
    }
    Some(unsafe { &*session })
}

fn prepared_asm_session_ref<'a>(
    prepared: *const OpforgePreparedAsmSession,
) -> Option<&'a OpforgePreparedAsmSession> {
    if prepared.is_null() {
        return None;
    }
    Some(unsafe { &*prepared })
}

fn tokenize_report_ref<'a>(
    report: *const OpforgeOpcoreTokenizeReport,
) -> Option<&'a OpforgeOpcoreTokenizeReport> {
    if report.is_null() {
        return None;
    }
    Some(unsafe { &*report })
}

fn expr_report_ref<'a>(
    report: *const OpforgeOpcoreExprReport,
) -> Option<&'a OpforgeOpcoreExprReport> {
    if report.is_null() {
        return None;
    }
    Some(unsafe { &*report })
}

fn module_item_report_ref<'a>(
    report: *const OpforgeOpcoreModuleItemReport,
) -> Option<&'a OpforgeOpcoreModuleItemReport> {
    if report.is_null() {
        return None;
    }
    Some(unsafe { &*report })
}

fn diagnostic_ref<'a>(report: *const OpforgeAsmReport, index: usize) -> Option<&'a Diagnostic> {
    report_ref(report)?.diagnostics.get(index)
}

fn diagnostic_detail_ref<'a>(
    report: *const OpforgeAsmReport,
    index: usize,
) -> Option<&'a OpforgeCachedDiagnostic> {
    report_ref(report)?.diagnostic_detail(index)
}

fn map_diagnostic_severity(severity: Severity) -> OpforgeDiagnosticSeverity {
    match severity {
        Severity::Warning => OpforgeDiagnosticSeverity::Warning,
        Severity::Error => OpforgeDiagnosticSeverity::Error,
    }
}

fn map_portable_token_kind(kind: &api::opcore::portable::PortableTokenKind) -> OpforgeTokenKind {
    match kind {
        api::opcore::portable::PortableTokenKind::Identifier(_) => OpforgeTokenKind::Identifier,
        api::opcore::portable::PortableTokenKind::Register(_) => OpforgeTokenKind::Register,
        api::opcore::portable::PortableTokenKind::Number { .. } => OpforgeTokenKind::Number,
        api::opcore::portable::PortableTokenKind::String { .. } => OpforgeTokenKind::String,
        api::opcore::portable::PortableTokenKind::Comma => OpforgeTokenKind::Comma,
        api::opcore::portable::PortableTokenKind::Colon => OpforgeTokenKind::Colon,
        api::opcore::portable::PortableTokenKind::Dollar => OpforgeTokenKind::Dollar,
        api::opcore::portable::PortableTokenKind::Dot => OpforgeTokenKind::Dot,
        api::opcore::portable::PortableTokenKind::Hash => OpforgeTokenKind::Hash,
        api::opcore::portable::PortableTokenKind::Question => OpforgeTokenKind::Question,
        api::opcore::portable::PortableTokenKind::OpenBracket => OpforgeTokenKind::OpenBracket,
        api::opcore::portable::PortableTokenKind::CloseBracket => OpforgeTokenKind::CloseBracket,
        api::opcore::portable::PortableTokenKind::OpenBrace => OpforgeTokenKind::OpenBrace,
        api::opcore::portable::PortableTokenKind::CloseBrace => OpforgeTokenKind::CloseBrace,
        api::opcore::portable::PortableTokenKind::OpenParen => OpforgeTokenKind::OpenParen,
        api::opcore::portable::PortableTokenKind::CloseParen => OpforgeTokenKind::CloseParen,
        api::opcore::portable::PortableTokenKind::Operator(_) => OpforgeTokenKind::Operator,
    }
}

fn callbacks_ref<'a>(
    callbacks: *const OpforgeOutputCallbacks,
) -> Option<&'a OpforgeOutputCallbacks> {
    if callbacks.is_null() {
        None
    } else {
        // SAFETY: callers must pass a valid pointer for non-null callbacks.
        Some(unsafe { &*callbacks })
    }
}

fn registry_cpu_view_lookup_impl(
    registry: *const OpforgeRegistry,
    cpu_id: *const c_char,
) -> *mut OpforgeRegistryCpuView {
    let Some(registry) = registry_ref(registry) else {
        return std::ptr::null_mut();
    };
    let Ok(cpu_id) = opt_c_str(cpu_id) else {
        return std::ptr::null_mut();
    };
    let Some(cpu_id) = cpu_id else {
        return std::ptr::null_mut();
    };
    let Some(view) = registry.snapshot.cpu_views.get(cpu_id) else {
        return std::ptr::null_mut();
    };
    Box::into_raw(Box::new(OpforgeRegistryCpuView::from_view(view)))
}

fn emit_memory_outputs_to_callbacks(
    callbacks: &OpforgeOutputCallbacks,
    output_sink: &MemoryOutputSink,
) -> Result<(), String> {
    let directories = output_sink.directories();
    let files = output_sink.files();
    if directories.is_empty() && files.is_empty() {
        return Ok(());
    }

    let write_file = callbacks.write_file.ok_or_else(|| {
        "write_file callback must not be null when output callbacks are provided".to_string()
    })?;

    for dir in directories {
        if let Some(create_dir) = callbacks.create_dir {
            let dir = CString::new(dir.to_string_lossy().replace('\0', " "))
                .expect("sanitized output directory should not contain NULs");
            let ok = unsafe { create_dir(dir.as_ptr(), callbacks.user_data) };
            if ok == 0 {
                return Err(format!(
                    "create_dir callback rejected output directory {}",
                    dir.to_string_lossy()
                ));
            }
        }
    }

    for (path, bytes) in files {
        let path = CString::new(path.to_string_lossy().replace('\0', " "))
            .expect("sanitized output path should not contain NULs");
        let ok = unsafe {
            write_file(
                path.as_ptr(),
                bytes.as_ptr(),
                bytes.len(),
                callbacks.user_data,
            )
        };
        if ok == 0 {
            return Err(format!(
                "write_file callback rejected output path {}",
                path.to_string_lossy()
            ));
        }
    }

    Ok(())
}

fn memory_output_sink_has_outputs(output_sink: &MemoryOutputSink) -> bool {
    !(output_sink.directories().is_empty() && output_sink.files().is_empty())
}

fn invalid_request_from_report(
    report: &OpforgeAsmReport,
    message: impl Into<String>,
) -> FfiAsmReportError {
    boxed_asm_report(OpforgeAsmReport::error(
        OpforgeStatus::InvalidRequest,
        report.diagnostics.clone(),
        report.error_count,
        report.warning_count,
        report.lockstep_match_count,
        report.lockstep_divergence_count,
        message,
    ))
}

fn default_asm_request() -> OpforgeAsmRequest {
    OpforgeAsmRequest {
        source: OpforgeAsmSourceOptions {
            root_path: std::ptr::null(),
            output_base: std::ptr::null(),
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
            execution_mode: OPFORGE_EXECUTION_MODE_DEFAULT,
            cpu_override: std::ptr::null(),
            max_loop_iterations: 0,
            opasm_package_path: std::ptr::null(),
        },
        output: OpforgeAsmOutputOptions {
            out_dir: std::ptr::null(),
            emit_outputs: OPFORGE_DEFAULT_OUTPUTS_DEFAULT,
            output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
            go_addr: std::ptr::null(),
            bin_specs: OpforgeStringList {
                items: std::ptr::null(),
                count: 0,
            },
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

fn invalid_request_asm_report(message: impl Into<String>) -> OpforgeAsmReport {
    OpforgeAsmReport::error(
        OpforgeStatus::InvalidRequest,
        Vec::new(),
        0,
        0,
        0,
        0,
        message,
    )
}

fn into_ffi_asm_report(
    result: Result<OpforgeAsmReport, FfiAsmReportError>,
) -> *mut OpforgeAsmReport {
    match result {
        Ok(report) => into_report_handle(report),
        Err(report) => into_report_handle(*report),
    }
}

fn with_request_report_boundary(
    boundary_name: &'static str,
    panic_point: Option<&'static str>,
    request: *const OpforgeAsmRequest,
    run: impl FnOnce(&OpforgeAsmRequest) -> Result<OpforgeAsmReport, FfiAsmReportError>,
) -> *mut OpforgeAsmReport {
    ffi_report_boundary(boundary_name, || {
        if let Some(point) = panic_point {
            ffi_test_maybe_panic(point);
        }
        let Some(request) = (!request.is_null()).then(|| unsafe { &*request }) else {
            return into_report_handle(invalid_request_asm_report(
                "request pointer must not be null",
            ));
        };
        into_ffi_asm_report(run(request))
    })
}

fn with_session_report_boundary(
    boundary_name: &'static str,
    session: *const OpforgeAsmSession,
    run: impl FnOnce(
        &api::asm::AssemblerSession,
    ) -> Result<api::diagnostics::AsmRunReport, api::asm::AssemblerWorkflowError>,
) -> *mut OpforgeAsmReport {
    ffi_report_boundary(boundary_name, || {
        ffi_test_maybe_panic(boundary_name);
        let Some(session) = asm_session_ref(session) else {
            return into_report_handle(invalid_request_asm_report(
                "session pointer must not be null",
            ));
        };
        into_report_handle(asm_report_from_workflow_result(run(&session.session)))
    })
}

fn with_prepared_session_report_boundary(
    boundary_name: &'static str,
    prepared: *const OpforgePreparedAsmSession,
    run: impl FnOnce(
        &api::asm::PreparedAssemblySession,
    ) -> Result<api::diagnostics::AsmRunReport, api::asm::AssemblerWorkflowError>,
) -> *mut OpforgeAsmReport {
    ffi_report_boundary(boundary_name, || {
        ffi_test_maybe_panic(boundary_name);
        let Some(prepared) = prepared_asm_session_ref(prepared) else {
            return into_report_handle(invalid_request_asm_report(
                "prepared session pointer must not be null",
            ));
        };
        if let Some(report) = prepared.failure.as_ref() {
            return into_report_handle(report.clone());
        }
        let Some(prepared_session) = prepared.prepared.as_ref() else {
            return into_report_handle(ffi_internal_error_report(
                boundary_name,
                "missing prepared session state",
            ));
        };
        into_report_handle(asm_report_from_workflow_result(run(prepared_session)))
    })
}

#[no_mangle]
/// Initialize a grouped high-level assembler request with stable Rust facade defaults.
///
/// # Safety
///
/// `request` must be null or point to writable storage for one [`OpforgeAsmRequest`].
pub unsafe extern "C" fn opforge_asm_request_init(request: *mut OpforgeAsmRequest) {
    if request.is_null() {
        return;
    }
    unsafe { *request = default_asm_request() };
}

#[no_mangle]
/// Assemble a single source file through the high-level `opforge_asm_*`
/// request surface.
///
/// # Safety
///
/// `request` must be non-null and must point to a valid [`OpforgeAsmRequest`]
/// for the duration of the call. Any non-null string pointer fields inside that
/// request must point to valid NUL-terminated UTF-8 strings.
pub unsafe extern "C" fn opforge_asm_assemble_file_with_request(
    request: *const OpforgeAsmRequest,
) -> *mut OpforgeAsmReport {
    with_request_report_boundary(
        "opforge_asm_assemble_file_with_request",
        Some("opforge_asm_assemble_file_with_request"),
        request,
        |request| run_high_level_assembler_with_request(request, false),
    )
}

#[no_mangle]
/// Check a single source file through the high-level `opforge_asm_*` request
/// surface.
///
/// # Safety
///
/// `request` must be non-null and must point to a valid [`OpforgeAsmRequest`]
/// for the duration of the call. Any non-null string pointer fields inside that
/// request must point to valid NUL-terminated UTF-8 strings.
pub unsafe extern "C" fn opforge_asm_check_file_with_request(
    request: *const OpforgeAsmRequest,
) -> *mut OpforgeAsmReport {
    with_request_report_boundary(
        "opforge_asm_check_file_with_request",
        Some("opforge_asm_check_file_with_request"),
        request,
        |request| run_high_level_assembler_with_request(request, true),
    )
}

#[no_mangle]
/// Assemble an in-memory source file through the high-level `opforge_asm_*`
/// request surface.
///
/// `request.source.root_path` acts as the virtual source path used for
/// diagnostics and output naming. `request.source.include_paths` and
/// `request.source.module_paths` remain filesystem-backed dependency roots
/// consulted after that synthetic root source. `callbacks` may be null unless
/// the assembly actually buffers outputs that need to be delivered back to the host.
/// Setting `request.output.emit_outputs` alone does not require callbacks if
/// the run produces no buffered outputs, and `request.output.no_outputs`
/// can prevent directive-driven or metadata-driven outputs from being buffered.
///
/// # Safety
///
/// `request` and `source_text` must be non-null. Any non-null string pointer
/// fields must point to valid NUL-terminated UTF-8 strings for the duration of
/// the call.
pub unsafe extern "C" fn opforge_asm_assemble_memory_with_request(
    request: *const OpforgeAsmRequest,
    source_text: *const c_char,
    callbacks: *const OpforgeOutputCallbacks,
) -> *mut OpforgeAsmReport {
    with_request_report_boundary(
        "opforge_asm_assemble_memory_with_request",
        Some("opforge_asm_assemble_memory_with_request"),
        request,
        |request| {
            run_high_level_assembler_in_memory_with_request(request, source_text, callbacks, false)
        },
    )
}

#[no_mangle]
/// Check an in-memory source file through the high-level `opforge_asm_*`
/// request surface.
///
/// `request.source.include_paths` and `request.source.module_paths` remain
/// filesystem-backed dependency roots consulted after the synthetic root
/// source supplied by `source_text`. Check-mode suppresses buffered outputs,
/// including default and metadata-driven artifacts, so `callbacks` are optional
/// and are not used for successful check-only runs.
///
/// # Safety
///
/// `request` and `source_text` must be non-null. Any non-null string pointer
/// fields must point to valid NUL-terminated UTF-8 strings for the duration of
/// the call.
pub unsafe extern "C" fn opforge_asm_check_memory_with_request(
    request: *const OpforgeAsmRequest,
    source_text: *const c_char,
    callbacks: *const OpforgeOutputCallbacks,
) -> *mut OpforgeAsmReport {
    with_request_report_boundary(
        "opforge_asm_check_memory_with_request",
        Some("opforge_asm_check_memory_with_request"),
        request,
        |request| {
            run_high_level_assembler_in_memory_with_request(request, source_text, callbacks, true)
        },
    )
}

#[no_mangle]
/// Create a reusable high-level assembler session through the request surface.
///
/// This convenience constructor returns null on invalid request data,
/// session-construction failure, or an internal panic while building the
/// session. Use `opforge_asm_session_create_with_request_report` when you need
/// diagnosable validation failures or structured internal-error reports.
///
/// # Safety
///
/// `request` must be non-null and any non-null string fields must point to
/// valid NUL-terminated UTF-8 strings for the duration of the call.
pub unsafe extern "C" fn opforge_asm_session_create_with_request(
    request: *const OpforgeAsmRequest,
) -> *mut OpforgeAsmSession {
    ffi_session_boundary(|| {
        ffi_test_maybe_panic("opforge_asm_session_create_with_request");
        if request.is_null() {
            return std::ptr::null_mut();
        }
        let request = unsafe { &*request };
        match build_high_level_assembler_session_with_request(request) {
            Ok(session) => into_session_handle(OpforgeAsmSession { session }),
            Err(_) => std::ptr::null_mut(),
        }
    })
}

#[no_mangle]
/// Create a reusable high-level assembler session and return a report handle
/// describing success or request-validation failure.
///
/// On success, `*out_session` receives a new session handle and the returned
/// report has status [`OpforgeStatus::Ok`]. On failure, `*out_session` is set
/// to null and the returned report contains the failure details, including
/// structured internal-error reports for panic boundaries.
///
/// # Safety
///
/// `request` and `out_session` must be non-null. Any non-null string fields
/// in `request` must point to valid NUL-terminated UTF-8 strings for the
/// duration of the call.
pub unsafe extern "C" fn opforge_asm_session_create_with_request_report(
    request: *const OpforgeAsmRequest,
    out_session: *mut *mut OpforgeAsmSession,
) -> *mut OpforgeAsmReport {
    ffi_report_boundary("opforge_asm_session_create_with_request_report", || {
        if out_session.is_null() {
            return into_report_handle(OpforgeAsmReport::error(
                OpforgeStatus::InvalidRequest,
                Vec::new(),
                0,
                0,
                0,
                0,
                "out_session pointer must not be null",
            ));
        }

        unsafe { *out_session = std::ptr::null_mut() };

        if request.is_null() {
            return into_report_handle(OpforgeAsmReport::error(
                OpforgeStatus::InvalidRequest,
                Vec::new(),
                0,
                0,
                0,
                0,
                "request pointer must not be null",
            ));
        }

        let request = unsafe { &*request };
        match build_high_level_assembler_session_with_request(request) {
            Ok(session) => {
                unsafe { *out_session = into_session_handle(OpforgeAsmSession { session }) };
                into_report_handle(OpforgeAsmReport::ok(Vec::new(), 0, 0, 0, 0))
            }
            Err(report) => into_report_handle(*report),
        }
    })
}

#[no_mangle]
/// Prepare a reusable high-level assembler session.
///
/// # Safety
///
/// `session` must be a handle previously returned by
/// [`opforge_asm_session_create`].
pub unsafe extern "C" fn opforge_asm_session_prepare(
    session: *const OpforgeAsmSession,
) -> *mut OpforgePreparedAsmSession {
    ffi_prepared_session_boundary("opforge_asm_session_prepare", || {
        let Some(session) = asm_session_ref(session) else {
            return into_prepared_session_handle(OpforgePreparedAsmSession {
                prepared: None,
                failure: Some(OpforgeAsmReport::error(
                    OpforgeStatus::InvalidRequest,
                    Vec::new(),
                    0,
                    0,
                    0,
                    0,
                    "session pointer must not be null",
                )),
            });
        };
        let prepared = session.session.prepare();
        match prepared {
            Ok(prepared) => into_prepared_session_handle(OpforgePreparedAsmSession {
                prepared: Some(prepared),
                failure: None,
            }),
            Err(err) => into_prepared_session_handle(OpforgePreparedAsmSession {
                prepared: None,
                failure: Some(asm_report_from_workflow_result(Err(err))),
            }),
        }
    })
}

#[no_mangle]
/// Assemble through a reusable high-level assembler session.
///
/// # Safety
///
/// `session` must be a handle previously returned by
/// [`opforge_asm_session_create`].
pub unsafe extern "C" fn opforge_asm_session_assemble(
    session: *const OpforgeAsmSession,
) -> *mut OpforgeAsmReport {
    with_session_report_boundary("opforge_asm_session_assemble", session, |session| {
        session.assemble()
    })
}

#[no_mangle]
/// Check through a reusable high-level assembler session.
///
/// # Safety
///
/// `session` must be a handle previously returned by
/// [`opforge_asm_session_create`].
pub unsafe extern "C" fn opforge_asm_session_check(
    session: *const OpforgeAsmSession,
) -> *mut OpforgeAsmReport {
    with_session_report_boundary("opforge_asm_session_check", session, |session| {
        session.check()
    })
}

#[no_mangle]
/// Assemble through a prepared high-level assembler session.
///
/// # Safety
///
/// `prepared` must be a handle previously returned by
/// [`opforge_asm_session_prepare`].
pub unsafe extern "C" fn opforge_prepared_asm_session_assemble(
    prepared: *const OpforgePreparedAsmSession,
) -> *mut OpforgeAsmReport {
    with_prepared_session_report_boundary(
        "opforge_prepared_asm_session_assemble",
        prepared,
        |prepared| prepared.assemble(),
    )
}

#[no_mangle]
/// Check through a prepared high-level assembler session.
///
/// # Safety
///
/// `prepared` must be a handle previously returned by
/// [`opforge_asm_session_prepare`].
pub unsafe extern "C" fn opforge_prepared_asm_session_check(
    prepared: *const OpforgePreparedAsmSession,
) -> *mut OpforgeAsmReport {
    with_prepared_session_report_boundary(
        "opforge_prepared_asm_session_check",
        prepared,
        |prepared| prepared.check(),
    )
}

#[no_mangle]
/// Free a reusable high-level assembler session handle.
///
/// # Safety
///
/// `session` must be null or a pointer previously returned by
/// [`opforge_asm_session_create`].
pub unsafe extern "C" fn opforge_asm_session_free(session: *mut OpforgeAsmSession) {
    if session.is_null() {
        return;
    }
    unsafe {
        drop(Box::from_raw(session));
    }
}

#[no_mangle]
/// Free a prepared high-level assembler session handle.
///
/// # Safety
///
/// `prepared` must be null or a pointer previously returned by
/// [`opforge_asm_session_prepare`].
pub unsafe extern "C" fn opforge_prepared_asm_session_free(
    prepared: *mut OpforgePreparedAsmSession,
) {
    if prepared.is_null() {
        return;
    }
    unsafe {
        drop(Box::from_raw(prepared));
    }
}

fn registry_ref(registry: *const OpforgeRegistry) -> Option<&'static OpforgeRegistry> {
    if registry.is_null() {
        None
    } else {
        Some(unsafe { &*registry })
    }
}

fn registry_cpu_view_ref(
    view: *const OpforgeRegistryCpuView,
) -> Option<&'static OpforgeRegistryCpuView> {
    if view.is_null() {
        None
    } else {
        Some(unsafe { &*view })
    }
}

#[no_mangle]
/// Create the default assembler registry handle over the stable Rust registry API.
pub extern "C" fn opforge_registry_default() -> *mut OpforgeRegistry {
    Box::into_raw(Box::new(OpforgeRegistry::default_registry()))
}

#[no_mangle]
/// Return the number of known CPU aliases in a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_alias_count(registry: *const OpforgeRegistry) -> usize {
    registry_ref(registry).map_or(0, |registry| registry.aliases.len())
}

#[no_mangle]
/// Borrow one known CPU alias from a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_alias(
    registry: *const OpforgeRegistry,
    index: usize,
) -> *const c_char {
    registry_ref(registry).map_or(std::ptr::null(), |registry| {
        OpforgeRegistry::string_ptr(&registry.aliases, index)
    })
}

#[no_mangle]
/// Return the number of known CPU ids in a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_cpu_count(registry: *const OpforgeRegistry) -> usize {
    registry_ref(registry).map_or(0, |registry| registry.cpu_ids.len())
}

#[no_mangle]
/// Borrow one CPU id from a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_cpu_id(
    registry: *const OpforgeRegistry,
    index: usize,
) -> *const c_char {
    registry_ref(registry).map_or(std::ptr::null(), |registry| {
        OpforgeRegistry::string_ptr(&registry.cpu_ids, index)
    })
}

#[no_mangle]
/// Return the number of known family ids in a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_family_count(registry: *const OpforgeRegistry) -> usize {
    registry_ref(registry).map_or(0, |registry| registry.family_ids.len())
}

#[no_mangle]
/// Borrow one family id from a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_family_id(
    registry: *const OpforgeRegistry,
    index: usize,
) -> *const c_char {
    registry_ref(registry).map_or(std::ptr::null(), |registry| {
        OpforgeRegistry::string_ptr(&registry.family_ids, index)
    })
}

#[no_mangle]
/// Return the number of known dialect ids in a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_dialect_count(registry: *const OpforgeRegistry) -> usize {
    registry_ref(registry).map_or(0, |registry| registry.dialect_ids.len())
}

#[no_mangle]
/// Borrow one dialect id from a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_dialect_id(
    registry: *const OpforgeRegistry,
    index: usize,
) -> *const c_char {
    registry_ref(registry).map_or(std::ptr::null(), |registry| {
        OpforgeRegistry::string_ptr(&registry.dialect_ids, index)
    })
}

#[no_mangle]
/// Return the number of directive keywords in a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_directive_keyword_count(
    registry: *const OpforgeRegistry,
) -> usize {
    registry_ref(registry).map_or(0, |registry| registry.directive_keywords.len())
}

#[no_mangle]
/// Borrow one directive keyword from a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_directive_keyword(
    registry: *const OpforgeRegistry,
    index: usize,
) -> *const c_char {
    registry_ref(registry).map_or(std::ptr::null(), |registry| {
        OpforgeRegistry::string_ptr(&registry.directive_keywords, index)
    })
}

#[no_mangle]
/// Clone the capability view for one CPU id from a registry handle.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`. `cpu_id` must be null or a valid NUL-terminated
/// UTF-8 string.
pub unsafe extern "C" fn opforge_registry_cpu_view(
    registry: *const OpforgeRegistry,
    cpu_id: *const c_char,
) -> *mut OpforgeRegistryCpuView {
    registry_cpu_view_lookup_impl(registry, cpu_id)
}

#[no_mangle]
/// Clone the capability view for one CPU id from a registry handle.
///
/// This name avoids the typedef/function identifier collision that plain C
/// compilers hit with `opforge_registry_cpu_view`.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by this library from
/// `opforge_registry_default`. `cpu_id` must be null or a valid NUL-terminated
/// UTF-8 string.
pub unsafe extern "C" fn opforge_registry_cpu_view_lookup(
    registry: *const OpforgeRegistry,
    cpu_id: *const c_char,
) -> *mut OpforgeRegistryCpuView {
    registry_cpu_view_lookup_impl(registry, cpu_id)
}

#[no_mangle]
/// Borrow the family id from a CPU capability view.
///
/// # Safety
///
/// `view` must be null or a pointer previously returned by this library from
/// `opforge_registry_cpu_view`.
pub unsafe extern "C" fn opforge_registry_cpu_view_family_id(
    view: *const OpforgeRegistryCpuView,
) -> *const c_char {
    registry_cpu_view_ref(view).map_or(std::ptr::null(), |view| view.family_id.as_ptr())
}

#[no_mangle]
/// Borrow the dialect id from a CPU capability view.
///
/// # Safety
///
/// `view` must be null or a pointer previously returned by this library from
/// `opforge_registry_cpu_view`.
pub unsafe extern "C" fn opforge_registry_cpu_view_dialect_id(
    view: *const OpforgeRegistryCpuView,
) -> *const c_char {
    registry_cpu_view_ref(view).map_or(std::ptr::null(), |view| view.dialect_id.as_ptr())
}

#[no_mangle]
/// Return the number of mnemonics in a CPU capability view.
///
/// # Safety
///
/// `view` must be null or a pointer previously returned by this library from
/// `opforge_registry_cpu_view`.
pub unsafe extern "C" fn opforge_registry_cpu_view_mnemonic_count(
    view: *const OpforgeRegistryCpuView,
) -> usize {
    registry_cpu_view_ref(view).map_or(0, |view| view.mnemonics.len())
}

#[no_mangle]
/// Borrow one mnemonic from a CPU capability view.
///
/// # Safety
///
/// `view` must be null or a pointer previously returned by this library from
/// `opforge_registry_cpu_view`.
pub unsafe extern "C" fn opforge_registry_cpu_view_mnemonic(
    view: *const OpforgeRegistryCpuView,
    index: usize,
) -> *const c_char {
    registry_cpu_view_ref(view).map_or(std::ptr::null(), |view| {
        OpforgeRegistryCpuView::string_ptr(&view.mnemonics, index)
    })
}

#[no_mangle]
/// Return the number of registers in a CPU capability view.
///
/// # Safety
///
/// `view` must be null or a pointer previously returned by this library from
/// `opforge_registry_cpu_view`.
pub unsafe extern "C" fn opforge_registry_cpu_view_register_count(
    view: *const OpforgeRegistryCpuView,
) -> usize {
    registry_cpu_view_ref(view).map_or(0, |view| view.registers.len())
}

#[no_mangle]
/// Borrow one register from a CPU capability view.
///
/// # Safety
///
/// `view` must be null or a pointer previously returned by this library from
/// `opforge_registry_cpu_view`.
pub unsafe extern "C" fn opforge_registry_cpu_view_register(
    view: *const OpforgeRegistryCpuView,
    index: usize,
) -> *const c_char {
    registry_cpu_view_ref(view).map_or(std::ptr::null(), |view| {
        OpforgeRegistryCpuView::string_ptr(&view.registers, index)
    })
}

#[no_mangle]
/// Return the number of runtime directives in a CPU capability view.
///
/// # Safety
///
/// `view` must be null or a pointer previously returned by this library from
/// `opforge_registry_cpu_view`.
pub unsafe extern "C" fn opforge_registry_cpu_view_runtime_directive_count(
    view: *const OpforgeRegistryCpuView,
) -> usize {
    registry_cpu_view_ref(view).map_or(0, |view| view.runtime_directives.len())
}

#[no_mangle]
/// Borrow one runtime directive from a CPU capability view.
///
/// # Safety
///
/// `view` must be null or a pointer previously returned by this library from
/// `opforge_registry_cpu_view`.
pub unsafe extern "C" fn opforge_registry_cpu_view_runtime_directive(
    view: *const OpforgeRegistryCpuView,
    index: usize,
) -> *const c_char {
    registry_cpu_view_ref(view).map_or(std::ptr::null(), |view| {
        OpforgeRegistryCpuView::string_ptr(&view.runtime_directives, index)
    })
}

#[no_mangle]
/// Free a registry handle previously returned by this library.
///
/// # Safety
///
/// `registry` must be null or a pointer previously returned by
/// `opforge_registry_default`.
pub unsafe extern "C" fn opforge_registry_free(registry: *mut OpforgeRegistry) {
    if registry.is_null() {
        return;
    }
    unsafe {
        drop(Box::from_raw(registry));
    }
}

#[no_mangle]
/// Free a CPU capability view previously returned by this library.
///
/// # Safety
///
/// `view` must be null or a pointer previously returned by
/// `opforge_registry_cpu_view`.
pub unsafe extern "C" fn opforge_registry_cpu_view_free(view: *mut OpforgeRegistryCpuView) {
    if view.is_null() {
        return;
    }
    unsafe {
        drop(Box::from_raw(view));
    }
}

#[no_mangle]
/// Tokenize one line through the stable lower-level `opforge_opcore_*` surface.
///
/// # Safety
///
/// `line` must be non-null and point to a valid NUL-terminated UTF-8 string
/// for the duration of the call.
pub unsafe extern "C" fn opforge_opcore_tokenize_line(
    line: *const c_char,
    line_num: u32,
) -> *mut OpforgeOpcoreTokenizeReport {
    let line = match opt_c_str(line) {
        Ok(Some(text)) => text,
        Ok(None) => {
            return Box::into_raw(Box::new(OpforgeOpcoreTokenizeReport::error(
                OpforgeProcessorStatus::InvalidRequest,
                "line must not be null",
                0,
                0,
                0,
            )));
        }
        Err(err) => {
            return Box::into_raw(Box::new(OpforgeOpcoreTokenizeReport::error(
                OpforgeProcessorStatus::InvalidRequest,
                err,
                0,
                0,
                0,
            )));
        }
    };

    let report = match api::opcore::portable::tokenize_line(line, line_num) {
        Ok(tokenized) => OpforgeOpcoreTokenizeReport::ok(tokenized),
        Err(err) => OpforgeOpcoreTokenizeReport::error(
            OpforgeProcessorStatus::TokenizeError,
            err.message,
            err.span.line,
            err.span.col_start,
            err.span.col_end,
        ),
    };
    Box::into_raw(Box::new(report))
}

fn tokenize_report_status_impl(
    report: *const OpforgeOpcoreTokenizeReport,
) -> OpforgeProcessorStatus {
    tokenize_report_ref(report).map_or(OpforgeProcessorStatus::InvalidRequest, |report| {
        report.status
    })
}

fn tokenize_report_token_count_impl(report: *const OpforgeOpcoreTokenizeReport) -> usize {
    tokenize_report_ref(report).map_or(0, |report| report.tokens.len())
}

fn tokenize_report_token_kind_impl(
    report: *const OpforgeOpcoreTokenizeReport,
    index: usize,
) -> OpforgeTokenKind {
    tokenize_report_ref(report)
        .and_then(|report| report.tokens.get(index))
        .map(|token| map_portable_token_kind(&token.kind))
        .unwrap_or(OpforgeTokenKind::Invalid)
}

fn tokenize_report_token_text_impl(
    report: *const OpforgeOpcoreTokenizeReport,
    index: usize,
) -> *const c_char {
    tokenize_report_ref(report).map_or(std::ptr::null(), |report| report.token_text_ptr(index))
}

fn tokenize_report_token_line_impl(
    report: *const OpforgeOpcoreTokenizeReport,
    index: usize,
) -> u32 {
    tokenize_report_ref(report)
        .and_then(|report| report.tokens.get(index))
        .map_or(0, |token| token.span.line)
}

fn tokenize_report_token_col_start_impl(
    report: *const OpforgeOpcoreTokenizeReport,
    index: usize,
) -> usize {
    tokenize_report_ref(report)
        .and_then(|report| report.tokens.get(index))
        .map_or(0, |token| token.span.col_start)
}

fn tokenize_report_token_col_end_impl(
    report: *const OpforgeOpcoreTokenizeReport,
    index: usize,
) -> usize {
    tokenize_report_ref(report)
        .and_then(|report| report.tokens.get(index))
        .map_or(0, |token| token.span.col_end)
}

fn tokenize_report_error_message_impl(report: *const OpforgeOpcoreTokenizeReport) -> *const c_char {
    tokenize_report_ref(report).map_or(std::ptr::null(), |report| report.error_message_ptr())
}

fn tokenize_report_error_line_impl(report: *const OpforgeOpcoreTokenizeReport) -> u32 {
    tokenize_report_ref(report).map_or(0, |report| report.error_line)
}

fn tokenize_report_error_col_start_impl(report: *const OpforgeOpcoreTokenizeReport) -> usize {
    tokenize_report_ref(report).map_or(0, |report| report.error_col_start)
}

fn tokenize_report_error_col_end_impl(report: *const OpforgeOpcoreTokenizeReport) -> usize {
    tokenize_report_ref(report).map_or(0, |report| report.error_col_end)
}

fn tokenize_report_free_impl(report: *mut OpforgeOpcoreTokenizeReport) {
    if report.is_null() {
        return;
    }
    unsafe {
        drop(Box::from_raw(report));
    }
}

macro_rules! define_tokenize_report_accessors {
    (
        report_ty = $report_ty:ty,
        surface = $surface:literal,
        constructor = $constructor:literal,
        status = $status_fn:ident,
        token_count = $token_count_fn:ident,
        token_kind = $token_kind_fn:ident,
        token_text = $token_text_fn:ident,
        token_line = $token_line_fn:ident,
        token_col_start = $token_col_start_fn:ident,
        token_col_end = $token_col_end_fn:ident,
        error_message = $error_message_fn:ident,
        error_line = $error_line_fn:ident,
        error_col_start = $error_col_start_fn:ident,
        error_col_end = $error_col_end_fn:ident,
        free = $free_fn:ident
    ) => {
        #[no_mangle]
        #[doc = concat!("Read the processor status from an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $status_fn(report: *const $report_ty) -> OpforgeProcessorStatus {
            tokenize_report_status_impl(report as *const OpforgeOpcoreTokenizeReport)
        }

        #[no_mangle]
        #[doc = concat!("Return the token count for an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $token_count_fn(report: *const $report_ty) -> usize {
            tokenize_report_token_count_impl(report as *const OpforgeOpcoreTokenizeReport)
        }

        #[no_mangle]
        #[doc = concat!("Return the token kind at `index` for an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $token_kind_fn(
            report: *const $report_ty,
            index: usize,
        ) -> OpforgeTokenKind {
            tokenize_report_token_kind_impl(report as *const OpforgeOpcoreTokenizeReport, index)
        }

        #[no_mangle]
        #[doc = concat!("Borrow the token source text at `index` for an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $token_text_fn(
            report: *const $report_ty,
            index: usize,
        ) -> *const c_char {
            tokenize_report_token_text_impl(report as *const OpforgeOpcoreTokenizeReport, index)
        }

        #[no_mangle]
        #[doc = concat!("Return the token line at `index` for an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $token_line_fn(report: *const $report_ty, index: usize) -> u32 {
            tokenize_report_token_line_impl(report as *const OpforgeOpcoreTokenizeReport, index)
        }

        #[no_mangle]
        #[doc = concat!("Return the token start column at `index` for an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $token_col_start_fn(
            report: *const $report_ty,
            index: usize,
        ) -> usize {
            tokenize_report_token_col_start_impl(
                report as *const OpforgeOpcoreTokenizeReport,
                index,
            )
        }

        #[no_mangle]
        #[doc = concat!("Return the token end column at `index` for an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $token_col_end_fn(
            report: *const $report_ty,
            index: usize,
        ) -> usize {
            tokenize_report_token_col_end_impl(
                report as *const OpforgeOpcoreTokenizeReport,
                index,
            )
        }

        #[no_mangle]
        #[doc = concat!("Borrow the error message from an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $error_message_fn(report: *const $report_ty) -> *const c_char {
            tokenize_report_error_message_impl(report as *const OpforgeOpcoreTokenizeReport)
        }

        #[no_mangle]
        #[doc = concat!("Return the error line from an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $error_line_fn(report: *const $report_ty) -> u32 {
            tokenize_report_error_line_impl(report as *const OpforgeOpcoreTokenizeReport)
        }

        #[no_mangle]
        #[doc = concat!("Return the error start column from an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $error_col_start_fn(report: *const $report_ty) -> usize {
            tokenize_report_error_col_start_impl(report as *const OpforgeOpcoreTokenizeReport)
        }

        #[no_mangle]
        #[doc = concat!("Return the error end column from an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $error_col_end_fn(report: *const $report_ty) -> usize {
            tokenize_report_error_col_end_impl(report as *const OpforgeOpcoreTokenizeReport)
        }

        #[no_mangle]
        #[doc = concat!("Free an `", $surface, "` tokenize report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $free_fn(report: *mut $report_ty) {
            tokenize_report_free_impl(report as *mut OpforgeOpcoreTokenizeReport);
        }
    };
}

define_tokenize_report_accessors!(
    report_ty = OpforgeOpcoreTokenizeReport,
    surface = "opforge_opcore_*",
    constructor = "opforge_opcore_tokenize_line",
    status = opforge_opcore_tokenize_report_status,
    token_count = opforge_opcore_tokenize_report_token_count,
    token_kind = opforge_opcore_tokenize_report_token_kind,
    token_text = opforge_opcore_tokenize_report_token_text,
    token_line = opforge_opcore_tokenize_report_token_line,
    token_col_start = opforge_opcore_tokenize_report_token_col_start,
    token_col_end = opforge_opcore_tokenize_report_token_col_end,
    error_message = opforge_opcore_tokenize_report_error_message,
    error_line = opforge_opcore_tokenize_report_error_line,
    error_col_start = opforge_opcore_tokenize_report_error_col_start,
    error_col_end = opforge_opcore_tokenize_report_error_col_end,
    free = opforge_opcore_tokenize_report_free
);

#[no_mangle]
/// Tokenize one assembler statement line through the stable lower-level `opforge_opasm_*` surface.
///
/// # Safety
///
/// `line` must be non-null and point to a valid NUL-terminated UTF-8 string
/// for the duration of the call.
pub unsafe extern "C" fn opforge_opasm_tokenize_statement(
    line: *const c_char,
    line_num: u32,
) -> *mut OpforgeOpasmTokenizeReport {
    let line = match opt_c_str(line) {
        Ok(Some(text)) => text,
        Ok(None) => {
            return Box::into_raw(Box::new(OpforgeOpcoreTokenizeReport::error(
                OpforgeProcessorStatus::InvalidRequest,
                "line must not be null",
                0,
                0,
                0,
            )));
        }
        Err(err) => {
            return Box::into_raw(Box::new(OpforgeOpcoreTokenizeReport::error(
                OpforgeProcessorStatus::InvalidRequest,
                err,
                0,
                0,
                0,
            )));
        }
    };

    let report = match api::asm::opasm::portable::tokenize_statement(
        api::asm::opasm::StatementRequest::new(line, line_num),
    ) {
        Ok(tokenized) => OpforgeOpcoreTokenizeReport::ok_opasm(tokenized),
        Err(err) => OpforgeOpcoreTokenizeReport::error(
            OpforgeProcessorStatus::ParseError,
            err.message,
            err.span.line,
            err.span.col_start,
            err.span.col_end,
        ),
    };
    Box::into_raw(Box::new(report))
}

define_tokenize_report_accessors!(
    report_ty = OpforgeOpasmTokenizeReport,
    surface = "opforge_opasm_*",
    constructor = "opforge_opasm_tokenize_statement",
    status = opforge_opasm_tokenize_report_status,
    token_count = opforge_opasm_tokenize_report_token_count,
    token_kind = opforge_opasm_tokenize_report_token_kind,
    token_text = opforge_opasm_tokenize_report_token_text,
    token_line = opforge_opasm_tokenize_report_token_line,
    token_col_start = opforge_opasm_tokenize_report_token_col_start,
    token_col_end = opforge_opasm_tokenize_report_token_col_end,
    error_message = opforge_opasm_tokenize_report_error_message,
    error_line = opforge_opasm_tokenize_report_error_line,
    error_col_start = opforge_opasm_tokenize_report_error_col_start,
    error_col_end = opforge_opasm_tokenize_report_error_col_end,
    free = opforge_opasm_tokenize_report_free
);

#[no_mangle]
/// Parse one assembler-oriented statement/directive through the stable lower-level
/// `opforge_opasm_*` surface.
///
/// # Safety
///
/// `line` must be non-null and point to a valid NUL-terminated UTF-8 string
/// for the duration of the call.
pub unsafe extern "C" fn opforge_opasm_parse_statement(
    line: *const c_char,
    line_num: u32,
) -> *mut OpforgeOpasmParseReport {
    let line = match opt_c_str(line) {
        Ok(Some(text)) => text,
        Ok(None) => {
            return Box::into_raw(Box::new(OpforgeOpasmParseReport::error(
                "line must not be null",
                0,
                0,
                0,
            )));
        }
        Err(err) => {
            return Box::into_raw(Box::new(OpforgeOpasmParseReport::error(err, 0, 0, 0)));
        }
    };

    let report = match api::asm::opasm::portable::parse_statement(
        api::asm::opasm::StatementRequest::new(line, line_num),
    ) {
        Ok(parsed) => OpforgeOpasmParseReport::ok(parsed.ast),
        Err(err) => OpforgeOpasmParseReport::error(
            err.message,
            err.span.line,
            err.span.col_start,
            err.span.col_end,
        ),
    };
    Box::into_raw(Box::new(report))
}

fn free_boxed_handle<T>(handle: *mut T) {
    if handle.is_null() {
        return;
    }
    unsafe {
        drop(Box::from_raw(handle));
    }
}

fn module_item_report_status_impl(
    report: Option<&OpforgeOpcoreModuleItemReport>,
) -> OpforgeProcessorStatus {
    report.map_or(OpforgeProcessorStatus::InvalidRequest, |report| {
        report.status
    })
}

fn module_item_report_kind_impl(
    report: Option<&OpforgeOpcoreModuleItemReport>,
) -> OpforgeLineAstKind {
    report.map_or(OpforgeLineAstKind::Invalid, |report| report.line_kind)
}

fn module_item_report_use_module_id_impl(
    report: Option<&OpforgeOpcoreModuleItemReport>,
) -> *const c_char {
    report.map_or(std::ptr::null(), |report| report.use_module_id_ptr())
}

fn module_item_report_use_alias_impl(
    report: Option<&OpforgeOpcoreModuleItemReport>,
) -> *const c_char {
    report.map_or(std::ptr::null(), |report| report.use_alias_ptr())
}

fn module_item_report_use_item_count_impl(report: Option<&OpforgeOpcoreModuleItemReport>) -> usize {
    report.map_or(0, |report| report.use_item_names.len())
}

fn module_item_report_use_item_name_impl(
    report: Option<&OpforgeOpcoreModuleItemReport>,
    index: usize,
) -> *const c_char {
    report.map_or(std::ptr::null(), |report| report.use_item_name_ptr(index))
}

fn module_item_report_statement_mnemonic_impl(
    report: Option<&OpforgeOpcoreModuleItemReport>,
) -> *const c_char {
    report.map_or(std::ptr::null(), |report| report.statement_mnemonic_ptr())
}

fn module_item_report_statement_operand_count_impl(
    report: Option<&OpforgeOpcoreModuleItemReport>,
) -> usize {
    report.map_or(0, |report| report.statement_operand_texts.len())
}

fn module_item_report_statement_operand_text_impl(
    report: Option<&OpforgeOpcoreModuleItemReport>,
    index: usize,
) -> *const c_char {
    report.map_or(std::ptr::null(), |report| {
        report.statement_operand_text_ptr(index)
    })
}

fn module_item_report_error_message_impl(
    report: Option<&OpforgeOpcoreModuleItemReport>,
) -> *const c_char {
    report.map_or(std::ptr::null(), |report| report.error_message_ptr())
}

fn module_item_report_error_line_impl(report: Option<&OpforgeOpcoreModuleItemReport>) -> u32 {
    report.map_or(0, |report| report.error_line)
}

fn module_item_report_error_col_start_impl(
    report: Option<&OpforgeOpcoreModuleItemReport>,
) -> usize {
    report.map_or(0, |report| report.error_col_start)
}

fn module_item_report_error_col_end_impl(report: Option<&OpforgeOpcoreModuleItemReport>) -> usize {
    report.map_or(0, |report| report.error_col_end)
}

macro_rules! define_module_item_report_accessors {
    (
        report_ty = $report_ty:ty,
        surface = $surface:literal,
        constructor = $constructor:literal,
        report_ref = $report_ref:expr,
        status = $status_fn:ident,
        kind = $kind_fn:ident,
        use_module_id = $use_module_id_fn:ident,
        use_alias = $use_alias_fn:ident,
        use_item_count = $use_item_count_fn:ident,
        use_item_name = $use_item_name_fn:ident,
        statement_mnemonic = $statement_mnemonic_fn:ident,
        statement_operand_count = $statement_operand_count_fn:ident,
        statement_operand_text = $statement_operand_text_fn:ident,
        error_message = $error_message_fn:ident,
        error_line = $error_line_fn:ident,
        error_col_start = $error_col_start_fn:ident,
        error_col_end = $error_col_end_fn:ident,
        free = $free_fn:ident
    ) => {
        #[no_mangle]
        #[doc = concat!("Read the processor status from an `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $status_fn(report: *const $report_ty) -> OpforgeProcessorStatus {
            module_item_report_status_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Return the line kind from an `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $kind_fn(report: *const $report_ty) -> OpforgeLineAstKind {
            module_item_report_kind_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Borrow the module id from a use-shaped `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $use_module_id_fn(report: *const $report_ty) -> *const c_char {
            module_item_report_use_module_id_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Borrow the alias from a use-shaped `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $use_alias_fn(report: *const $report_ty) -> *const c_char {
            module_item_report_use_alias_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Return the imported item count from a use-shaped `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $use_item_count_fn(report: *const $report_ty) -> usize {
            module_item_report_use_item_count_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Borrow one imported item name from a use-shaped `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $use_item_name_fn(
            report: *const $report_ty,
            index: usize,
        ) -> *const c_char {
            module_item_report_use_item_name_impl(($report_ref)(report), index)
        }

        #[no_mangle]
        #[doc = concat!("Borrow the statement mnemonic from a statement-shaped `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $statement_mnemonic_fn(
            report: *const $report_ty,
        ) -> *const c_char {
            module_item_report_statement_mnemonic_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Return the statement operand count from a statement-shaped `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $statement_operand_count_fn(report: *const $report_ty) -> usize {
            module_item_report_statement_operand_count_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Borrow one statement operand text from a statement-shaped `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $statement_operand_text_fn(
            report: *const $report_ty,
            index: usize,
        ) -> *const c_char {
            module_item_report_statement_operand_text_impl(($report_ref)(report), index)
        }

        #[no_mangle]
        #[doc = concat!("Borrow the error message from an `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $error_message_fn(report: *const $report_ty) -> *const c_char {
            module_item_report_error_message_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Return the error line from an `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $error_line_fn(report: *const $report_ty) -> u32 {
            module_item_report_error_line_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Return the error start column from an `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $error_col_start_fn(report: *const $report_ty) -> usize {
            module_item_report_error_col_start_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Return the error end column from an `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $error_col_end_fn(report: *const $report_ty) -> usize {
            module_item_report_error_col_end_impl(($report_ref)(report))
        }

        #[no_mangle]
        #[doc = concat!("Free an `", $surface, "` report.\n\n# Safety\n\n`report` must be null or a pointer previously returned by this library from\n`", $constructor, "`.")]
        pub unsafe extern "C" fn $free_fn(report: *mut $report_ty) {
            free_boxed_handle(report);
        }
    };
}

define_module_item_report_accessors!(
    report_ty = OpforgeOpasmParseReport,
    surface = "opforge_opasm_*",
    constructor = "opforge_opasm_parse_statement",
    report_ref = |report: *const OpforgeOpasmParseReport| module_item_report_ref(report),
    status = opforge_opasm_parse_report_status,
    kind = opforge_opasm_parse_report_kind,
    use_module_id = opforge_opasm_parse_report_use_module_id,
    use_alias = opforge_opasm_parse_report_use_alias,
    use_item_count = opforge_opasm_parse_report_use_item_count,
    use_item_name = opforge_opasm_parse_report_use_item_name,
    statement_mnemonic = opforge_opasm_parse_report_statement_mnemonic,
    statement_operand_count = opforge_opasm_parse_report_statement_operand_count,
    statement_operand_text = opforge_opasm_parse_report_statement_operand_text,
    error_message = opforge_opasm_parse_report_error_message,
    error_line = opforge_opasm_parse_report_error_line,
    error_col_start = opforge_opasm_parse_report_error_col_start,
    error_col_end = opforge_opasm_parse_report_error_col_end,
    free = opforge_opasm_parse_report_free
);

#[no_mangle]
/// Process one assembler-oriented line/directive through the stable lower-level
/// `opforge_opasm_*` surface.
///
/// # Safety
///
/// `request` must be non-null and point to a valid [`OpforgeOpasmProcessConfig`]
/// for the duration of the call. Any non-null string fields must be valid
/// NUL-terminated UTF-8 strings.
pub unsafe extern "C" fn opforge_opasm_process_statement(
    request: *const OpforgeOpasmProcessConfig,
) -> *mut OpforgeOpasmProcessReport {
    let request = if request.is_null() {
        return Box::into_raw(Box::new(OpforgeOpasmProcessReport::error(
            "request pointer must not be null",
            0,
            0,
            0,
        )));
    } else {
        unsafe { &*request }
    };

    let line = match opt_c_str(request.line) {
        Ok(Some(text)) => text,
        Ok(None) => {
            return Box::into_raw(Box::new(OpforgeOpasmProcessReport::error(
                "line must not be null",
                0,
                0,
                0,
            )));
        }
        Err(err) => {
            return Box::into_raw(Box::new(OpforgeOpasmProcessReport::error(err, 0, 0, 0)));
        }
    };

    let processor = match build_opasm_processor(
        request.execution_mode,
        request.cpu_id,
        request.dialect_override,
    ) {
        Ok(processor) => processor,
        Err(err) => return Box::into_raw(Box::new(OpforgeOpasmProcessReport::error(err, 0, 0, 0))),
    };

    let report = match api::asm::opasm::portable::process_statement_with_processor(
        &processor,
        line,
        request.line_num,
    ) {
        Ok(result) => OpforgeOpasmProcessReport::ok(result),
        Err(err) => OpforgeOpasmProcessReport::error(
            err.message,
            err.span.line,
            err.span.col_start,
            err.span.col_end,
        ),
    };
    Box::into_raw(Box::new(report))
}

fn process_report_ref(
    report: *const OpforgeOpasmProcessReport,
) -> Option<&'static OpforgeOpasmProcessReport> {
    if report.is_null() {
        None
    } else {
        Some(unsafe { &*report })
    }
}

fn processing_trace_ref(
    trace: *const OpforgeProcessingTrace,
) -> Option<&'static OpforgeProcessingTrace> {
    if trace.is_null() {
        None
    } else {
        Some(unsafe { &*trace })
    }
}

fn lockstep_report_ref(
    report: *const OpforgeLockstepReport,
) -> Option<&'static OpforgeLockstepReport> {
    if report.is_null() {
        None
    } else {
        Some(unsafe { &*report })
    }
}

define_module_item_report_accessors!(
    report_ty = OpforgeOpasmProcessReport,
    surface = "opforge_opasm_*",
    constructor = "opforge_opasm_process_statement",
    report_ref = |report: *const OpforgeOpasmProcessReport| {
        process_report_ref(report).map(|report| &report.parsed)
    },
    status = opforge_opasm_process_report_status,
    kind = opforge_opasm_process_report_kind,
    use_module_id = opforge_opasm_process_report_use_module_id,
    use_alias = opforge_opasm_process_report_use_alias,
    use_item_count = opforge_opasm_process_report_use_item_count,
    use_item_name = opforge_opasm_process_report_use_item_name,
    statement_mnemonic = opforge_opasm_process_report_statement_mnemonic,
    statement_operand_count = opforge_opasm_process_report_statement_operand_count,
    statement_operand_text = opforge_opasm_process_report_statement_operand_text,
    error_message = opforge_opasm_process_report_error_message,
    error_line = opforge_opasm_process_report_error_line,
    error_col_start = opforge_opasm_process_report_error_col_start,
    error_col_end = opforge_opasm_process_report_error_col_end,
    free = opforge_opasm_process_report_free
);

#[no_mangle]
/// Return the number of processing requests recorded for an `opforge_opasm_*` processing report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_statement`.
pub unsafe extern "C" fn opforge_opasm_process_report_trace_request_count(
    report: *const OpforgeOpasmProcessReport,
) -> usize {
    process_report_ref(report).map_or(0, |report| report.trace_request_count)
}

#[no_mangle]
/// Clone the processing trace recorded for an `opforge_opasm_*` processing report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_statement`.
pub unsafe extern "C" fn opforge_opasm_process_report_processing_trace(
    report: *const OpforgeOpasmProcessReport,
) -> *mut OpforgeProcessingTrace {
    process_report_ref(report).map_or(std::ptr::null_mut(), |report| {
        Box::into_raw(Box::new(OpforgeProcessingTrace {
            request_texts: report.processing_trace.request_texts.clone(),
        }))
    })
}

#[no_mangle]
/// Return the number of requests in a processing trace.
///
/// # Safety
///
/// `trace` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_processing_trace`.
pub unsafe extern "C" fn opforge_processing_trace_request_count(
    trace: *const OpforgeProcessingTrace,
) -> usize {
    processing_trace_ref(trace).map_or(0, |trace| trace.request_texts.len())
}

#[no_mangle]
/// Borrow one request text from a processing trace.
///
/// # Safety
///
/// `trace` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_processing_trace`.
pub unsafe extern "C" fn opforge_processing_trace_request_text(
    trace: *const OpforgeProcessingTrace,
    index: usize,
) -> *const c_char {
    processing_trace_ref(trace).map_or(std::ptr::null(), |trace| trace.request_text_ptr(index))
}

#[no_mangle]
/// Return the lockstep match count from an `opforge_opasm_*` processing report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_statement`.
pub unsafe extern "C" fn opforge_opasm_process_report_lockstep_match_count(
    report: *const OpforgeOpasmProcessReport,
) -> usize {
    process_report_ref(report).map_or(0, |report| report.lockstep_match_count)
}

#[no_mangle]
/// Clone the lockstep report recorded for an `opforge_opasm_*` processing report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_statement`.
pub unsafe extern "C" fn opforge_opasm_process_report_lockstep_report(
    report: *const OpforgeOpasmProcessReport,
) -> *mut OpforgeLockstepReport {
    process_report_ref(report).map_or(std::ptr::null_mut(), |report| {
        Box::into_raw(Box::new(OpforgeLockstepReport {
            matches: report.lockstep_report.matches.clone(),
            divergences: report.lockstep_report.divergences.clone(),
        }))
    })
}

#[no_mangle]
/// Return the lockstep divergence count from an `opforge_opasm_*` processing report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_statement`.
pub unsafe extern "C" fn opforge_opasm_process_report_lockstep_divergence_count(
    report: *const OpforgeOpasmProcessReport,
) -> usize {
    process_report_ref(report).map_or(0, |report| report.lockstep_divergence_count)
}

#[no_mangle]
/// Return the number of matches in a lockstep report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_match_count(
    report: *const OpforgeLockstepReport,
) -> usize {
    lockstep_report_ref(report).map_or(0, |report| report.matches.len())
}

#[no_mangle]
/// Return the number of divergences in a lockstep report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_count(
    report: *const OpforgeLockstepReport,
) -> usize {
    lockstep_report_ref(report).map_or(0, |report| report.divergences.len())
}

#[no_mangle]
/// Borrow the stage text for one lockstep match.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_match_stage_text(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .matches
            .get(index)
            .map_or(std::ptr::null(), |entry| entry.stage_text.as_ptr())
    })
}

#[no_mangle]
/// Borrow the request text for one lockstep match.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_match_request_text(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .matches
            .get(index)
            .map_or(std::ptr::null(), |entry| entry.request_text.as_ptr())
    })
}

#[no_mangle]
/// Borrow the category text for one lockstep match.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_match_category_text(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .matches
            .get(index)
            .map_or(std::ptr::null(), |entry| entry.category_text.as_ptr())
    })
}

#[no_mangle]
/// Borrow the stage text for one lockstep divergence.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_stage_text(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .divergences
            .get(index)
            .map_or(std::ptr::null(), |entry| entry.stage_text.as_ptr())
    })
}

#[no_mangle]
/// Borrow the request text for one lockstep divergence.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_request_text(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .divergences
            .get(index)
            .map_or(std::ptr::null(), |entry| entry.request_text.as_ptr())
    })
}

#[no_mangle]
/// Borrow the processor domain for one lockstep divergence.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_processor_domain(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .divergences
            .get(index)
            .map_or(std::ptr::null(), |entry| entry.processor_domain.as_ptr())
    })
}

#[no_mangle]
/// Borrow the continuation head text for one lockstep divergence.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_continuation_head(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .divergences
            .get(index)
            .map_or(std::ptr::null(), |entry| {
                entry.continuation_head_text.as_ptr()
            })
    })
}

#[no_mangle]
/// Return the source line for one lockstep divergence, or `0` when absent.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_source_line(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> u32 {
    lockstep_report_ref(report).map_or(0, |report| {
        report
            .divergences
            .get(index)
            .map_or(0, |entry| entry.source_line)
    })
}

#[no_mangle]
/// Borrow the active CPU for one lockstep divergence, or null when absent.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_active_cpu(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .divergences
            .get(index)
            .map_or(std::ptr::null(), |entry| {
                entry
                    .active_cpu
                    .as_ref()
                    .map_or(std::ptr::null(), |value| value.as_ptr())
            })
    })
}

#[no_mangle]
/// Borrow the active dialect for one lockstep divergence, or null when absent.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_active_dialect(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .divergences
            .get(index)
            .map_or(std::ptr::null(), |entry| {
                entry
                    .active_dialect
                    .as_ref()
                    .map_or(std::ptr::null(), |value| value.as_ptr())
            })
    })
}

#[no_mangle]
/// Borrow the left checkpoint summary text for one lockstep divergence.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_left_text(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .divergences
            .get(index)
            .map_or(std::ptr::null(), |entry| entry.left_text.as_ptr())
    })
}

#[no_mangle]
/// Borrow the right checkpoint summary text for one lockstep divergence.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_right_text(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .divergences
            .get(index)
            .map_or(std::ptr::null(), |entry| entry.right_text.as_ptr())
    })
}

#[no_mangle]
/// Borrow the category text for one lockstep divergence.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_category_text(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .divergences
            .get(index)
            .map_or(std::ptr::null(), |entry| entry.category_text.as_ptr())
    })
}

#[no_mangle]
/// Borrow the reason code for one lockstep divergence.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_divergence_reason_code(
    report: *const OpforgeLockstepReport,
    index: usize,
) -> *const c_char {
    lockstep_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .divergences
            .get(index)
            .map_or(std::ptr::null(), |entry| entry.reason_code.as_ptr())
    })
}

#[no_mangle]
/// Free a processing trace handle previously returned by this library.
///
/// # Safety
///
/// `trace` must be null or a pointer previously returned by
/// `opforge_opasm_process_report_processing_trace`.
pub unsafe extern "C" fn opforge_processing_trace_free(trace: *mut OpforgeProcessingTrace) {
    free_boxed_handle(trace);
}

#[no_mangle]
/// Free a lockstep report handle previously returned by this library.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by
/// `opforge_opasm_process_report_lockstep_report`.
pub unsafe extern "C" fn opforge_lockstep_report_free(report: *mut OpforgeLockstepReport) {
    free_boxed_handle(report);
}

#[no_mangle]
/// Parse one expression line through the stable lower-level `opforge_opcore_*` surface.
///
/// # Safety
///
/// `line` must be non-null and point to a valid NUL-terminated UTF-8 string
/// for the duration of the call.
pub unsafe extern "C" fn opforge_opcore_parse_expression(
    line: *const c_char,
    line_num: u32,
) -> *mut OpforgeOpcoreExprReport {
    let line = match opt_c_str(line) {
        Ok(Some(text)) => text,
        Ok(None) => {
            return Box::into_raw(Box::new(OpforgeOpcoreExprReport::error(
                OpforgeProcessorStatus::InvalidRequest,
                "line must not be null",
                0,
                0,
                0,
            )));
        }
        Err(err) => {
            return Box::into_raw(Box::new(OpforgeOpcoreExprReport::error(
                OpforgeProcessorStatus::InvalidRequest,
                err,
                0,
                0,
                0,
            )));
        }
    };

    let report = match api::opcore::portable::tokenize_line(line, line_num) {
        Ok(tokenized) => match api::opcore::portable::parse_expression(tokenized) {
            Ok(expr) => OpforgeOpcoreExprReport::ok(expr),
            Err(err) => OpforgeOpcoreExprReport::error(
                OpforgeProcessorStatus::ParseError,
                err.message,
                err.span.line,
                err.span.col_start,
                err.span.col_end,
            ),
        },
        Err(err) => OpforgeOpcoreExprReport::error(
            OpforgeProcessorStatus::TokenizeError,
            err.message,
            err.span.line,
            err.span.col_start,
            err.span.col_end,
        ),
    };
    Box::into_raw(Box::new(report))
}

#[no_mangle]
/// Read the processor status from an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_status(
    report: *const OpforgeOpcoreExprReport,
) -> OpforgeProcessorStatus {
    expr_report_ref(report).map_or(OpforgeProcessorStatus::InvalidRequest, |report| {
        report.status
    })
}

#[no_mangle]
/// Return the node count for an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_node_count(
    report: *const OpforgeOpcoreExprReport,
) -> usize {
    expr_report_ref(report).map_or(0, |report| report.nodes.len())
}

#[no_mangle]
/// Return the node kind at `index` for an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_node_kind(
    report: *const OpforgeOpcoreExprReport,
    index: usize,
) -> OpforgeExprNodeKind {
    expr_report_ref(report)
        .and_then(|report| report.nodes.get(index))
        .map_or(OpforgeExprNodeKind::Invalid, |node| node.kind)
}

#[no_mangle]
/// Borrow the node text at `index` for an `opforge_opcore_*` expression report.
///
/// For leaf/value nodes this is the literal/name text. For selected structural
/// nodes it carries stable auxiliary text such as member field names, call
/// names, struct type names, binary/unary operator names, or range operator
/// text.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_node_text(
    report: *const OpforgeOpcoreExprReport,
    index: usize,
) -> *const c_char {
    expr_report_ref(report).map_or(std::ptr::null(), |report| {
        report
            .nodes
            .get(index)
            .map_or(std::ptr::null(), |node| report.text_ptr(node.text_index))
    })
}

#[no_mangle]
/// Return the node line at `index` for an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_node_line(
    report: *const OpforgeOpcoreExprReport,
    index: usize,
) -> u32 {
    expr_report_ref(report)
        .and_then(|report| report.nodes.get(index))
        .map_or(0, |node| node.line)
}

#[no_mangle]
/// Return the node start column at `index` for an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_node_col_start(
    report: *const OpforgeOpcoreExprReport,
    index: usize,
) -> usize {
    expr_report_ref(report)
        .and_then(|report| report.nodes.get(index))
        .map_or(0, |node| node.col_start)
}

#[no_mangle]
/// Return the node end column at `index` for an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_node_col_end(
    report: *const OpforgeOpcoreExprReport,
    index: usize,
) -> usize {
    expr_report_ref(report)
        .and_then(|report| report.nodes.get(index))
        .map_or(0, |node| node.col_end)
}

#[no_mangle]
/// Return the child count for node `index` in an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_node_child_count(
    report: *const OpforgeOpcoreExprReport,
    index: usize,
) -> usize {
    expr_report_ref(report)
        .and_then(|report| report.nodes.get(index))
        .map_or(0, |node| node.child_len)
}

#[no_mangle]
/// Return the child node index at `child_index` for node `index`.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_node_child(
    report: *const OpforgeOpcoreExprReport,
    index: usize,
    child_index: usize,
) -> usize {
    expr_report_ref(report)
        .and_then(|report| report.nodes.get(index).map(|node| (report, node)))
        .and_then(|(report, node)| report.child_edges.get(node.child_start + child_index))
        .map_or(usize::MAX, |edge| edge.child_index)
}

#[no_mangle]
/// Borrow the child label at `child_index` for node `index`.
///
/// This is currently used for labeled child relationships such as
/// `StructLiteral` field names. Other node types return null.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_node_child_label(
    report: *const OpforgeOpcoreExprReport,
    index: usize,
    child_index: usize,
) -> *const c_char {
    expr_report_ref(report)
        .and_then(|report| report.nodes.get(index).map(|node| (report, node)))
        .and_then(|(report, node)| {
            report
                .child_edges
                .get(node.child_start + child_index)
                .map(|edge| (report, edge))
        })
        .map_or(std::ptr::null(), |(report, edge)| {
            report.text_ptr(edge.label_text_index)
        })
}

#[no_mangle]
/// Borrow the error message from an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_error_message(
    report: *const OpforgeOpcoreExprReport,
) -> *const c_char {
    expr_report_ref(report).map_or(std::ptr::null(), |report| report.error_message_ptr())
}

#[no_mangle]
/// Return the error line from an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_error_line(
    report: *const OpforgeOpcoreExprReport,
) -> u32 {
    expr_report_ref(report).map_or(0, |report| report.error_line)
}

#[no_mangle]
/// Return the error start column from an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_error_col_start(
    report: *const OpforgeOpcoreExprReport,
) -> usize {
    expr_report_ref(report).map_or(0, |report| report.error_col_start)
}

#[no_mangle]
/// Return the error end column from an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_error_col_end(
    report: *const OpforgeOpcoreExprReport,
) -> usize {
    expr_report_ref(report).map_or(0, |report| report.error_col_end)
}

#[no_mangle]
/// Free an `opforge_opcore_*` expression report.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_opcore_parse_expression`.
pub unsafe extern "C" fn opforge_opcore_expr_report_free(report: *mut OpforgeOpcoreExprReport) {
    if report.is_null() {
        return;
    }
    unsafe {
        drop(Box::from_raw(report));
    }
}

#[no_mangle]
/// Process one module-item line through the stable lower-level `opforge_opcore_*` surface.
///
/// # Safety
///
/// `line` must be non-null and point to a valid NUL-terminated UTF-8 string
/// for the duration of the call.
pub unsafe extern "C" fn opforge_opcore_process_module_item(
    line: *const c_char,
    line_num: u32,
) -> *mut OpforgeOpcoreModuleItemReport {
    let line = match opt_c_str(line) {
        Ok(Some(text)) => text,
        Ok(None) => {
            return Box::into_raw(Box::new(OpforgeOpcoreModuleItemReport::error(
                "line must not be null",
                0,
                0,
                0,
            )));
        }
        Err(err) => {
            return Box::into_raw(Box::new(OpforgeOpcoreModuleItemReport::error(err, 0, 0, 0)));
        }
    };

    let report = match api::opcore::portable::process_module_item(line, line_num) {
        api::processing::ProcessingOutcome::Done(line_ast) => {
            OpforgeOpcoreModuleItemReport::ok(line_ast)
        }
        api::processing::ProcessingOutcome::Return(_) => OpforgeOpcoreModuleItemReport::returned(),
        api::processing::ProcessingOutcome::Error(err) => OpforgeOpcoreModuleItemReport::error(
            err.message,
            err.span.line,
            err.span.col_start,
            err.span.col_end,
        ),
    };
    Box::into_raw(Box::new(report))
}

define_module_item_report_accessors!(
    report_ty = OpforgeOpcoreModuleItemReport,
    surface = "opforge_opcore_*",
    constructor = "opforge_opcore_process_module_item",
    report_ref = |report: *const OpforgeOpcoreModuleItemReport| module_item_report_ref(report),
    status = opforge_opcore_module_item_report_status,
    kind = opforge_opcore_module_item_report_kind,
    use_module_id = opforge_opcore_module_item_report_use_module_id,
    use_alias = opforge_opcore_module_item_report_use_alias,
    use_item_count = opforge_opcore_module_item_report_use_item_count,
    use_item_name = opforge_opcore_module_item_report_use_item_name,
    statement_mnemonic = opforge_opcore_module_item_report_statement_mnemonic,
    statement_operand_count = opforge_opcore_module_item_report_statement_operand_count,
    statement_operand_text = opforge_opcore_module_item_report_statement_operand_text,
    error_message = opforge_opcore_module_item_report_error_message,
    error_line = opforge_opcore_module_item_report_error_line,
    error_col_start = opforge_opcore_module_item_report_error_col_start,
    error_col_end = opforge_opcore_module_item_report_error_col_end,
    free = opforge_opcore_module_item_report_free
);

#[no_mangle]
/// Read the status from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_asm_report_status(
    report: *const OpforgeAsmReport,
) -> OpforgeStatus {
    report_ref(report).map_or(OpforgeStatus::InvalidRequest, |report| report.status)
}

#[no_mangle]
/// Read the error count from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_asm_report_error_count(report: *const OpforgeAsmReport) -> usize {
    report_ref(report).map_or(0, |report| report.error_count)
}

#[no_mangle]
/// Read the warning count from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_asm_report_warning_count(
    report: *const OpforgeAsmReport,
) -> usize {
    report_ref(report).map_or(0, |report| report.warning_count)
}

#[no_mangle]
/// Read the lockstep match count from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_asm_report_lockstep_match_count(
    report: *const OpforgeAsmReport,
) -> usize {
    report_ref(report).map_or(0, |report| report.lockstep_match_count)
}

#[no_mangle]
/// Read the lockstep divergence count from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_asm_report_lockstep_divergence_count(
    report: *const OpforgeAsmReport,
) -> usize {
    report_ref(report).map_or(0, |report| report.lockstep_divergence_count)
}

#[no_mangle]
/// Borrow the report message pointer from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_asm_report_message(
    report: *const OpforgeAsmReport,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| report.message_ptr())
}

#[no_mangle]
/// Return the number of diagnostics stored in an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_count_from_asm_report(
    report: *const OpforgeAsmReport,
) -> usize {
    report_ref(report).map_or(0, |report| report.diagnostics.len())
}

#[no_mangle]
/// Return the severity of a diagnostic in an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_severity_from_asm_report(
    report: *const OpforgeAsmReport,
    index: usize,
) -> OpforgeDiagnosticSeverity {
    diagnostic_ref(report, index)
        .map(|diag| map_diagnostic_severity(diag.severity()))
        .unwrap_or(OpforgeDiagnosticSeverity::Invalid)
}

#[no_mangle]
/// Return the 1-based line number for a diagnostic in an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_line_from_asm_report(
    report: *const OpforgeAsmReport,
    index: usize,
) -> u32 {
    diagnostic_ref(report, index).map_or(0, |diag| diag.line())
}

#[no_mangle]
/// Return the 1-based start column for a diagnostic in an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_column_from_asm_report(
    report: *const OpforgeAsmReport,
    index: usize,
) -> usize {
    diagnostic_ref(report, index)
        .and_then(|diag| diag.column())
        .unwrap_or(0)
}

#[no_mangle]
/// Return the 1-based end column for a diagnostic in an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_col_end_from_asm_report(
    report: *const OpforgeAsmReport,
    index: usize,
) -> usize {
    diagnostic_ref(report, index)
        .and_then(|diag| diag.col_end())
        .unwrap_or(0)
}

#[no_mangle]
/// Borrow the diagnostic message pointer from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_diag_message_from_asm_report(
    report: *const OpforgeAsmReport,
    index: usize,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| {
        report.diagnostic_message_ptr(index)
    })
}

#[no_mangle]
/// Borrow the diagnostic code pointer from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_diag_code_from_asm_report(
    report: *const OpforgeAsmReport,
    index: usize,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| report.diagnostic_code_ptr(index))
}

#[no_mangle]
/// Borrow the diagnostic file pointer from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_diag_file_from_asm_report(
    report: *const OpforgeAsmReport,
    index: usize,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| report.diagnostic_file_ptr(index))
}

#[no_mangle]
/// Return the number of related spans attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_related_span_count_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
) -> usize {
    diagnostic_ref(report, diag_index).map_or(0, |diag| diag.related_spans().len())
}

#[no_mangle]
/// Borrow the related-span file pointer from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_diag_related_span_file_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    span_index: usize,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| {
        report.diagnostic_related_span_file_ptr(diag_index, span_index)
    })
}

#[no_mangle]
/// Return the 1-based line number for a related span attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_related_span_line_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    span_index: usize,
) -> u32 {
    diagnostic_ref(report, diag_index)
        .and_then(|diag| diag.related_spans().get(span_index))
        .map_or(0, |span| span.line)
}

#[no_mangle]
/// Return the 1-based start column for a related span attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_related_span_col_start_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    span_index: usize,
) -> usize {
    diagnostic_ref(report, diag_index)
        .and_then(|diag| diag.related_spans().get(span_index))
        .and_then(|span| span.col_start)
        .unwrap_or(0)
}

#[no_mangle]
/// Return the 1-based end column for a related span attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_related_span_col_end_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    span_index: usize,
) -> usize {
    diagnostic_ref(report, diag_index)
        .and_then(|diag| diag.related_spans().get(span_index))
        .and_then(|span| span.col_end)
        .unwrap_or(0)
}

#[no_mangle]
/// Borrow the related-span label pointer from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_diag_related_span_label_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    span_index: usize,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| {
        report.diagnostic_related_span_label_ptr(diag_index, span_index)
    })
}

#[no_mangle]
/// Return non-zero when the related span is the primary span for the diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_related_span_is_primary_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    span_index: usize,
) -> u8 {
    diagnostic_ref(report, diag_index)
        .and_then(|diag| diag.related_spans().get(span_index))
        .map_or(0, |span| u8::from(span.is_primary))
}

#[no_mangle]
/// Return the number of notes attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_note_count_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
) -> usize {
    diagnostic_detail_ref(report, diag_index).map_or(0, |detail| detail.notes.len())
}

#[no_mangle]
/// Borrow a diagnostic note pointer from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_diag_note_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    note_index: usize,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| {
        report.diagnostic_note_ptr(diag_index, note_index)
    })
}

#[no_mangle]
/// Return the number of help entries attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_help_count_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
) -> usize {
    diagnostic_detail_ref(report, diag_index).map_or(0, |detail| detail.help.len())
}

#[no_mangle]
/// Borrow a diagnostic help pointer from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_diag_help_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    help_index: usize,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| {
        report.diagnostic_help_ptr(diag_index, help_index)
    })
}

#[no_mangle]
/// Return the number of fix-its attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_fixit_count_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
) -> usize {
    diagnostic_detail_ref(report, diag_index).map_or(0, |detail| detail.fixits.len())
}

#[no_mangle]
/// Borrow the fix-it file pointer from an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_diag_fixit_file_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    fixit_index: usize,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| {
        report.diagnostic_fixit_file_ptr(diag_index, fixit_index)
    })
}

#[no_mangle]
/// Return the 1-based line number for a fix-it attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_fixit_line_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    fixit_index: usize,
) -> u32 {
    diagnostic_ref(report, diag_index)
        .and_then(|diag| diag.fixits().get(fixit_index))
        .map_or(0, |fixit| fixit.line)
}

#[no_mangle]
/// Return the 1-based start column for a fix-it attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_fixit_col_start_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    fixit_index: usize,
) -> usize {
    diagnostic_ref(report, diag_index)
        .and_then(|diag| diag.fixits().get(fixit_index))
        .and_then(|fixit| fixit.col_start)
        .unwrap_or(0)
}

#[no_mangle]
/// Return the 1-based end column for a fix-it attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`.
pub unsafe extern "C" fn opforge_diag_fixit_col_end_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    fixit_index: usize,
) -> usize {
    diagnostic_ref(report, diag_index)
        .and_then(|diag| diag.fixits().get(fixit_index))
        .and_then(|fixit| fixit.col_end)
        .unwrap_or(0)
}

#[no_mangle]
/// Borrow the replacement text pointer for a fix-it attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_diag_fixit_replacement_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    fixit_index: usize,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| {
        report.diagnostic_fixit_replacement_ptr(diag_index, fixit_index)
    })
}

#[no_mangle]
/// Borrow the applicability text pointer for a fix-it attached to a diagnostic.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// `opforge_asm_assemble_file` or `opforge_asm_check_file`. The returned
/// string pointer is borrowed from `opforge_asm_report` and must not be freed
/// separately.
pub unsafe extern "C" fn opforge_diag_fixit_applicability_from_asm_report(
    report: *const OpforgeAsmReport,
    diag_index: usize,
    fixit_index: usize,
) -> *const c_char {
    report_ref(report).map_or(std::ptr::null(), |report| {
        report.diagnostic_fixit_applicability_ptr(diag_index, fixit_index)
    })
}

#[no_mangle]
/// Free an `opforge_asm_*` report handle.
///
/// # Safety
///
/// `report` must be null or a pointer previously returned by this library from
/// an `opforge_asm_*` report-producing function. Passing any other pointer is
/// undefined behavior.
pub unsafe extern "C" fn opforge_asm_report_free(report: *mut OpforgeAsmReport) {
    if report.is_null() {
        return;
    }
    // SAFETY: pointer must come from Box::into_raw in this library.
    unsafe {
        drop(Box::from_raw(report));
    }
}

#[cfg(test)]
mod tests {
    use super::{
        build_grouped_high_level_config, ffi_test_hooks, opforge_asm_assemble_file_with_request,
        opforge_asm_assemble_memory_with_request, opforge_asm_check_file_with_request,
        opforge_asm_check_memory_with_request, opforge_asm_report_error_count,
        opforge_asm_report_free, opforge_asm_report_lockstep_match_count,
        opforge_asm_report_message, opforge_asm_report_status, opforge_asm_report_warning_count,
        opforge_asm_request_init, opforge_asm_session_assemble, opforge_asm_session_check,
        opforge_asm_session_create_with_request, opforge_asm_session_free,
        opforge_asm_session_prepare, opforge_diag_code_from_asm_report,
        opforge_diag_col_end_from_asm_report, opforge_diag_column_from_asm_report,
        opforge_diag_count_from_asm_report, opforge_diag_file_from_asm_report,
        opforge_diag_fixit_applicability_from_asm_report,
        opforge_diag_fixit_col_end_from_asm_report, opforge_diag_fixit_col_start_from_asm_report,
        opforge_diag_fixit_count_from_asm_report, opforge_diag_fixit_file_from_asm_report,
        opforge_diag_fixit_line_from_asm_report, opforge_diag_fixit_replacement_from_asm_report,
        opforge_diag_help_count_from_asm_report, opforge_diag_help_from_asm_report,
        opforge_diag_line_from_asm_report, opforge_diag_message_from_asm_report,
        opforge_diag_note_count_from_asm_report, opforge_diag_note_from_asm_report,
        opforge_diag_related_span_col_end_from_asm_report,
        opforge_diag_related_span_col_start_from_asm_report,
        opforge_diag_related_span_count_from_asm_report,
        opforge_diag_related_span_file_from_asm_report,
        opforge_diag_related_span_is_primary_from_asm_report,
        opforge_diag_related_span_label_from_asm_report,
        opforge_diag_related_span_line_from_asm_report, opforge_diag_severity_from_asm_report,
        opforge_lockstep_report_divergence_count, opforge_lockstep_report_divergence_reason_code,
        opforge_lockstep_report_free, opforge_lockstep_report_match_count,
        opforge_lockstep_report_match_request_text, opforge_lockstep_report_match_stage_text,
        opforge_opasm_parse_report_error_col_end, opforge_opasm_parse_report_error_col_start,
        opforge_opasm_parse_report_error_line, opforge_opasm_parse_report_error_message,
        opforge_opasm_parse_report_free, opforge_opasm_parse_report_kind,
        opforge_opasm_parse_report_statement_mnemonic,
        opforge_opasm_parse_report_statement_operand_count,
        opforge_opasm_parse_report_statement_operand_text, opforge_opasm_parse_report_status,
        opforge_opasm_parse_statement, opforge_opasm_process_report_error_col_end,
        opforge_opasm_process_report_error_col_start, opforge_opasm_process_report_error_line,
        opforge_opasm_process_report_error_message, opforge_opasm_process_report_free,
        opforge_opasm_process_report_kind, opforge_opasm_process_report_lockstep_divergence_count,
        opforge_opasm_process_report_lockstep_match_count,
        opforge_opasm_process_report_lockstep_report,
        opforge_opasm_process_report_processing_trace,
        opforge_opasm_process_report_statement_mnemonic,
        opforge_opasm_process_report_statement_operand_count,
        opforge_opasm_process_report_statement_operand_text, opforge_opasm_process_report_status,
        opforge_opasm_process_report_trace_request_count, opforge_opasm_process_statement,
        opforge_opasm_tokenize_report_error_col_end, opforge_opasm_tokenize_report_error_col_start,
        opforge_opasm_tokenize_report_error_line, opforge_opasm_tokenize_report_error_message,
        opforge_opasm_tokenize_report_free, opforge_opasm_tokenize_report_status,
        opforge_opasm_tokenize_report_token_col_end, opforge_opasm_tokenize_report_token_col_start,
        opforge_opasm_tokenize_report_token_count, opforge_opasm_tokenize_report_token_kind,
        opforge_opasm_tokenize_report_token_line, opforge_opasm_tokenize_report_token_text,
        opforge_opasm_tokenize_statement, opforge_opcore_expr_report_error_col_end,
        opforge_opcore_expr_report_error_col_start, opforge_opcore_expr_report_error_line,
        opforge_opcore_expr_report_error_message, opforge_opcore_expr_report_free,
        opforge_opcore_expr_report_node_child, opforge_opcore_expr_report_node_child_count,
        opforge_opcore_expr_report_node_count, opforge_opcore_expr_report_node_kind,
        opforge_opcore_expr_report_node_line, opforge_opcore_expr_report_node_text,
        opforge_opcore_expr_report_status, opforge_opcore_module_item_report_error_col_end,
        opforge_opcore_module_item_report_error_col_start,
        opforge_opcore_module_item_report_error_line,
        opforge_opcore_module_item_report_error_message, opforge_opcore_module_item_report_free,
        opforge_opcore_module_item_report_kind,
        opforge_opcore_module_item_report_statement_mnemonic,
        opforge_opcore_module_item_report_statement_operand_count,
        opforge_opcore_module_item_report_statement_operand_text,
        opforge_opcore_module_item_report_status, opforge_opcore_module_item_report_use_alias,
        opforge_opcore_module_item_report_use_item_count,
        opforge_opcore_module_item_report_use_item_name,
        opforge_opcore_module_item_report_use_module_id, opforge_opcore_parse_expression,
        opforge_opcore_process_module_item, opforge_opcore_tokenize_line,
        opforge_opcore_tokenize_report_error_col_end,
        opforge_opcore_tokenize_report_error_col_start, opforge_opcore_tokenize_report_error_line,
        opforge_opcore_tokenize_report_error_message, opforge_opcore_tokenize_report_free,
        opforge_opcore_tokenize_report_status, opforge_opcore_tokenize_report_token_col_end,
        opforge_opcore_tokenize_report_token_col_start, opforge_opcore_tokenize_report_token_count,
        opforge_opcore_tokenize_report_token_kind, opforge_opcore_tokenize_report_token_line,
        opforge_opcore_tokenize_report_token_text, opforge_prepared_asm_session_assemble,
        opforge_prepared_asm_session_check, opforge_prepared_asm_session_free,
        opforge_processing_trace_free, opforge_processing_trace_request_count,
        opforge_processing_trace_request_text, opforge_registry_alias,
        opforge_registry_alias_count, opforge_registry_cpu_count, opforge_registry_cpu_id,
        opforge_registry_cpu_view, opforge_registry_cpu_view_dialect_id,
        opforge_registry_cpu_view_family_id, opforge_registry_cpu_view_free,
        opforge_registry_cpu_view_mnemonic, opforge_registry_cpu_view_mnemonic_count,
        opforge_registry_default, opforge_registry_dialect_count, opforge_registry_dialect_id,
        opforge_registry_directive_keyword, opforge_registry_directive_keyword_count,
        opforge_registry_family_count, opforge_registry_family_id, opforge_registry_free,
        LabelOutputFormat, OpforgeAsmDiagnosticsOptions, OpforgeAsmExecutionOptions,
        OpforgeAsmOutputOptions, OpforgeAsmRequest, OpforgeAsmSourceOptions,
        OpforgeDiagnosticSeverity, OpforgeExprNodeKind, OpforgeLineAstKind,
        OpforgeOpasmParseReport, OpforgeOpasmProcessConfig, OpforgeOpasmProcessReport,
        OpforgeOpasmTokenizeReport, OpforgeOpcoreModuleItemReport, OpforgeOpcoreTokenizeReport,
        OpforgeOutputCallbacks, OpforgePreparedAsmSession, OpforgeProcessorStatus, OpforgeStatus,
        OpforgeStringList, OpforgeTokenKind, OPFORGE_DEFAULT_OUTPUTS_DEFAULT,
        OPFORGE_DEFAULT_OUTPUTS_DISABLE, OPFORGE_DEFAULT_OUTPUTS_ENABLE,
        OPFORGE_EXECUTION_MODE_DEFAULT, OPFORGE_EXECUTION_MODE_LOCKSTEP_RUST,
        OPFORGE_EXECUTION_MODE_LOCKSTEP_VM, OPFORGE_EXECUTION_MODE_RUST, OPFORGE_EXECUTION_MODE_VM,
        OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT, OPFORGE_OUTPUT_FORMAT_TEXT,
    };
    use std::ffi::c_void;
    use std::ffi::{CStr, CString};
    use std::fs;
    use std::os::raw::c_char;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn make_temp_dir(name: &str) -> std::path::PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!(
            "libopforge-ffi-{name}-{}-{nanos}",
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

    #[test]
    fn ffi_opforge_registry_group_queries_default_snapshot() {
        let registry = opforge_registry_default();
        assert!(!registry.is_null());
        assert!(unsafe { opforge_registry_alias_count(registry) } > 0);
        assert!(unsafe { opforge_registry_cpu_count(registry) } > 0);
        assert!(unsafe { opforge_registry_family_count(registry) } > 0);
        assert!(unsafe { opforge_registry_dialect_count(registry) } > 0);
        assert!(unsafe { opforge_registry_directive_keyword_count(registry) } > 0);

        let first_cpu = unsafe { CStr::from_ptr(opforge_registry_cpu_id(registry, 0)) }
            .to_str()
            .expect("ffi registry cpu id utf8");
        assert!(!first_cpu.is_empty());
        let first_alias = unsafe { CStr::from_ptr(opforge_registry_alias(registry, 0)) }
            .to_str()
            .expect("ffi registry alias utf8");
        assert!(!first_alias.is_empty());
        let first_family = unsafe { CStr::from_ptr(opforge_registry_family_id(registry, 0)) }
            .to_str()
            .expect("ffi registry family id utf8");
        assert!(!first_family.is_empty());
        let first_dialect = unsafe { CStr::from_ptr(opforge_registry_dialect_id(registry, 0)) }
            .to_str()
            .expect("ffi registry dialect id utf8");
        assert!(!first_dialect.is_empty());
        let first_directive =
            unsafe { CStr::from_ptr(opforge_registry_directive_keyword(registry, 0)) }
                .to_str()
                .expect("ffi registry directive utf8");
        assert!(!first_directive.is_empty());

        let cpu_id = CString::new(first_cpu).expect("cpu cstr");
        let view = unsafe { opforge_registry_cpu_view(registry, cpu_id.as_ptr()) };
        assert!(!view.is_null());
        let family_id = unsafe { CStr::from_ptr(opforge_registry_cpu_view_family_id(view)) }
            .to_str()
            .expect("ffi cpu view family utf8");
        let dialect_id = unsafe { CStr::from_ptr(opforge_registry_cpu_view_dialect_id(view)) }
            .to_str()
            .expect("ffi cpu view dialect utf8");
        assert!(!family_id.is_empty());
        assert!(!dialect_id.is_empty());
        assert!(unsafe { opforge_registry_cpu_view_mnemonic_count(view) } > 0);
        let mnemonic = unsafe { CStr::from_ptr(opforge_registry_cpu_view_mnemonic(view, 0)) }
            .to_str()
            .expect("ffi cpu view mnemonic utf8");
        assert!(!mnemonic.is_empty());

        unsafe { opforge_registry_cpu_view_free(view) };
        unsafe { opforge_registry_free(registry) };
    }

    #[derive(Default)]
    struct CallbackCapture {
        dirs: Vec<String>,
        files: Vec<(String, Vec<u8>)>,
    }

    unsafe extern "C" fn capture_create_dir(path: *const c_char, user_data: *mut c_void) -> u8 {
        let capture = unsafe { &mut *(user_data as *mut CallbackCapture) };
        let path = unsafe { CStr::from_ptr(path) }
            .to_str()
            .expect("callback dir utf8")
            .to_string();
        capture.dirs.push(path);
        1
    }

    unsafe extern "C" fn capture_write_file(
        path: *const c_char,
        data: *const u8,
        len: usize,
        user_data: *mut c_void,
    ) -> u8 {
        let capture = unsafe { &mut *(user_data as *mut CallbackCapture) };
        let path = unsafe { CStr::from_ptr(path) }
            .to_str()
            .expect("callback file path utf8")
            .to_string();
        let bytes = unsafe { std::slice::from_raw_parts(data, len) }.to_vec();
        capture.files.push((path, bytes));
        1
    }

    #[test]
    fn ffi_smoke_assembles_file_through_public_api_boundary() {
        let work_dir = make_temp_dir("assemble");
        let out_dir = work_dir.join("out");
        fs::create_dir_all(&out_dir).expect("create out dir");
        let source_path = work_dir.join("main.asm");
        fs::write(&source_path, ".module main\nstart:\n    nop\n.endmodule\n")
            .expect("write source");

        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let out = CString::new(out_dir.to_string_lossy().as_bytes()).expect("out cstr");
        let input_base_owned = source_path.with_extension("");
        let input_base =
            CString::new(input_base_owned.to_string_lossy().as_bytes()).expect("base cstr");

        let request = basic_request(
            root.as_ptr(),
            input_base.as_ptr(),
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
        assert!(unsafe { opforge_asm_report_lockstep_match_count(report) } > 0);
        assert!(out_dir.join("main.lst").exists());
        assert!(out_dir.join("main.hex").exists());
        assert!(unsafe { opforge_asm_report_message(report) }.is_null());
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_group_assembles_file_through_public_api_boundary() {
        let work_dir = make_temp_dir("asm-group-assemble");
        let out_dir = work_dir.join("out");
        fs::create_dir_all(&out_dir).expect("create out dir");
        let source_path = work_dir.join("main.asm");
        fs::write(&source_path, ".module main\nstart:\n    nop\n.endmodule\n")
            .expect("write source");

        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let out = CString::new(out_dir.to_string_lossy().as_bytes()).expect("out cstr");
        let input_base_owned = source_path.with_extension("");
        let input_base =
            CString::new(input_base_owned.to_string_lossy().as_bytes()).expect("base cstr");

        let request = basic_request(
            root.as_ptr(),
            input_base.as_ptr(),
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
        assert!(unsafe { opforge_asm_report_lockstep_match_count(report) } > 0);
        assert!(unsafe { opforge_asm_report_message(report) }.is_null());
        assert!(out_dir.join("main.lst").exists());
        assert!(out_dir.join("main.hex").exists());
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_group_checks_file_without_outputs() {
        let work_dir = make_temp_dir("asm-group-check");
        let out_dir = work_dir.join("out");
        fs::create_dir_all(&out_dir).expect("create out dir");
        let source_path = work_dir.join("main.asm");
        fs::write(
            &source_path,
            ".module main\n\n.region ram, $1000, $10ff\n\n.section code\n.pub\nstart\n    .byte $42, $43\n.priv\n.endsection\n\n.place code in ram\n\n.output \"build/minimal.bin\", format=bin, sections=code\n.mapfile \"build/minimal.map\", symbols=public\n.exportsections dir=\"build/minimal_sections\", format=bin\n\n.endmodule\n",
        )
        .expect("write source");

        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let out = CString::new(out_dir.to_string_lossy().as_bytes()).expect("out cstr");
        let input_base_owned = source_path.with_extension("");
        let input_base =
            CString::new(input_base_owned.to_string_lossy().as_bytes()).expect("base cstr");

        let request = basic_request(
            root.as_ptr(),
            input_base.as_ptr(),
            out.as_ptr(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        let report = unsafe { opforge_asm_check_file_with_request(&request) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
        assert_eq!(unsafe { opforge_asm_report_warning_count(report) }, 0);
        assert!(unsafe { opforge_asm_report_message(report) }.is_null());
        assert!(!out_dir.join("main.lst").exists());
        assert!(!out_dir.join("main.hex").exists());
        assert!(!out_dir.join("build").join("minimal.bin").exists());
        assert!(!out_dir.join("build").join("minimal.map").exists());
        assert!(!out_dir.join("build").join("minimal_sections").exists());
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_session_group_assembles_file_through_owned_session_api() {
        let work_dir = make_temp_dir("asm-session-assemble");
        let out_dir = work_dir.join("out");
        fs::create_dir_all(&out_dir).expect("create out dir");
        let source_path = work_dir.join("main.asm");
        fs::write(&source_path, ".module main\nstart:\n    nop\n.endmodule\n")
            .expect("write source");

        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let out = CString::new(out_dir.to_string_lossy().as_bytes()).expect("out cstr");
        let input_base_owned = source_path.with_extension("");
        let input_base =
            CString::new(input_base_owned.to_string_lossy().as_bytes()).expect("base cstr");

        let request = basic_request(
            root.as_ptr(),
            input_base.as_ptr(),
            out.as_ptr(),
            OPFORGE_EXECUTION_MODE_LOCKSTEP_VM,
            1,
        );

        let session = unsafe { opforge_asm_session_create_with_request(&request) };
        assert!(!session.is_null());
        let report = unsafe { opforge_asm_session_assemble(session) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
        assert!(unsafe { opforge_asm_report_lockstep_match_count(report) } > 0);
        assert!(out_dir.join("main.lst").exists());
        assert!(out_dir.join("main.hex").exists());
        unsafe { opforge_asm_report_free(report) };
        unsafe { opforge_asm_session_free(session) };
    }

    #[test]
    fn ffi_opforge_prepared_asm_session_group_checks_through_prepared_handle() {
        let work_dir = make_temp_dir("prepared-asm-session-check");
        let out_dir = work_dir.join("out");
        fs::create_dir_all(&out_dir).expect("create out dir");
        let source_path = work_dir.join("main.asm");
        fs::write(
            &source_path,
            ".module main\n\n.region ram, $1000, $10ff\n\n.section code\n.pub\nstart\n    .byte $42, $43\n.priv\n.endsection\n\n.place code in ram\n\n.output \"build/minimal.bin\", format=bin, sections=code\n.mapfile \"build/minimal.map\", symbols=public\n.exportsections dir=\"build/minimal_sections\", format=bin\n\n.endmodule\n",
        )
        .expect("write source");

        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let out = CString::new(out_dir.to_string_lossy().as_bytes()).expect("out cstr");
        let input_base_owned = source_path.with_extension("");
        let input_base =
            CString::new(input_base_owned.to_string_lossy().as_bytes()).expect("base cstr");

        let request = basic_request(
            root.as_ptr(),
            input_base.as_ptr(),
            out.as_ptr(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        let session = unsafe { opforge_asm_session_create_with_request(&request) };
        assert!(!session.is_null());
        let prepared = unsafe { opforge_asm_session_prepare(session) };
        assert!(!prepared.is_null());
        let report = unsafe { opforge_prepared_asm_session_check(prepared) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
        assert_eq!(unsafe { opforge_asm_report_warning_count(report) }, 0);
        assert!(!out_dir.join("main.lst").exists());
        assert!(!out_dir.join("main.hex").exists());
        assert!(!out_dir.join("build").join("minimal.bin").exists());
        assert!(!out_dir.join("build").join("minimal.map").exists());
        assert!(!out_dir.join("build").join("minimal_sections").exists());
        unsafe { opforge_asm_report_free(report) };
        unsafe { opforge_prepared_asm_session_free(prepared) };
        unsafe { opforge_asm_session_free(session) };
    }

    #[test]
    fn ffi_opforge_asm_session_create_returns_null_on_creation_failure() {
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
    fn ffi_reports_assembly_errors_through_borrowed_report_message_storage() {
        let work_dir = make_temp_dir("error");
        let source_path = work_dir.join("broken.asm");
        fs::write(&source_path, ".module main\n    .unknown\n.endmodule\n").expect("write source");

        let report = {
            let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
            let request = basic_request(
                root.as_ptr(),
                std::ptr::null(),
                std::ptr::null(),
                OPFORGE_EXECUTION_MODE_VM,
                0,
            );

            unsafe { opforge_asm_assemble_file_with_request(&request) }
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::AssembleError
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi report message utf8")
            .to_string();
        assert!(!message.trim().is_empty());
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_report_message_is_borrowed_from_report_storage() {
        let work_dir = make_temp_dir("report-message-borrowed");
        let source_path = work_dir.join("broken.asm");
        fs::write(&source_path, ".module main\n    .unknown\n.endmodule\n").expect("write source");

        let report = {
            let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
            let request = basic_request(
                root.as_ptr(),
                std::ptr::null(),
                std::ptr::null(),
                OPFORGE_EXECUTION_MODE_VM,
                0,
            );

            unsafe { opforge_asm_assemble_file_with_request(&request) }
        };

        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::AssembleError
        );

        let first_ptr = unsafe { opforge_asm_report_message(report) };
        let second_ptr = unsafe { opforge_asm_report_message(report) };
        assert!(!first_ptr.is_null());
        assert_eq!(first_ptr, second_ptr);

        let message = unsafe { CStr::from_ptr(first_ptr) }
            .to_str()
            .expect("ffi report message utf8");
        assert!(!message.trim().is_empty());

        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_diag_group_enumerates_error_diagnostics() {
        let work_dir = make_temp_dir("diag-group");
        let source_path = work_dir.join("broken.asm");
        fs::write(&source_path, ".module main\n    .unknown\n.endmodule\n").expect("write source");

        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        let report = unsafe { opforge_asm_assemble_file_with_request(&request) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::AssembleError
        );
        assert!(unsafe { opforge_diag_count_from_asm_report(report) } > 0);
        assert_eq!(
            unsafe { opforge_diag_severity_from_asm_report(report, 0) },
            OpforgeDiagnosticSeverity::Error
        );
        assert_eq!(unsafe { opforge_diag_line_from_asm_report(report, 0) }, 2);
        let col_start = unsafe { opforge_diag_column_from_asm_report(report, 0) };
        let col_end = unsafe { opforge_diag_col_end_from_asm_report(report, 0) };
        assert!(col_start > 0);
        assert!(col_end == 0 || col_end >= col_start);
        let message = unsafe { CStr::from_ptr(opforge_diag_message_from_asm_report(report, 0)) }
            .to_str()
            .expect("ffi diagnostic message utf8");
        assert!(
            message.contains("Unknown directive")
                || message.to_ascii_lowercase().contains("unknown"),
            "unexpected diagnostic message: {message}"
        );
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_diag_group_exposes_rich_diagnostic_payloads() {
        let work_dir = make_temp_dir("diag-rich-group");
        let source_path = work_dir.join("broken.asm");
        fs::write(&source_path, ".module main\nLD A,\n.endmodule\n").expect("write source");

        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        let report = unsafe { opforge_asm_assemble_file_with_request(&request) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::AssembleError
        );
        assert!(unsafe { opforge_diag_count_from_asm_report(report) } > 0);

        let code = unsafe { CStr::from_ptr(opforge_diag_code_from_asm_report(report, 0)) }
            .to_str()
            .expect("ffi diagnostic code utf8");
        assert!(!code.is_empty(), "expected diagnostic code");

        let file = unsafe { CStr::from_ptr(opforge_diag_file_from_asm_report(report, 0)) }
            .to_str()
            .expect("ffi diagnostic file utf8");
        assert_eq!(file, source_path.to_string_lossy());

        assert_eq!(unsafe { opforge_diag_line_from_asm_report(report, 0) }, 2);

        let related_span_count =
            unsafe { opforge_diag_related_span_count_from_asm_report(report, 0) };
        assert!(related_span_count > 0, "expected related parser span");
        let related_file =
            unsafe { CStr::from_ptr(opforge_diag_related_span_file_from_asm_report(report, 0, 0)) }
                .to_str()
                .expect("ffi related span file utf8");
        assert_eq!(related_file, source_path.to_string_lossy());
        assert_eq!(
            unsafe { opforge_diag_related_span_line_from_asm_report(report, 0, 0) },
            2
        );
        assert!(unsafe { opforge_diag_related_span_col_start_from_asm_report(report, 0, 0) } > 0);
        assert!(unsafe { opforge_diag_related_span_col_end_from_asm_report(report, 0, 0) } > 0);
        let related_label = unsafe {
            CStr::from_ptr(opforge_diag_related_span_label_from_asm_report(
                report, 0, 0,
            ))
        }
        .to_str()
        .expect("ffi related span label utf8");
        assert!(related_label.contains("parsing"));
        assert_eq!(
            unsafe { opforge_diag_related_span_is_primary_from_asm_report(report, 0, 0) },
            1
        );

        assert_eq!(
            unsafe { opforge_diag_note_count_from_asm_report(report, 0) },
            0
        );
        assert!(unsafe { opforge_diag_note_from_asm_report(report, 0, 0) }.is_null());

        let help_count = unsafe { opforge_diag_help_count_from_asm_report(report, 0) };
        assert!(help_count > 0, "expected dialect help");
        let help = unsafe { CStr::from_ptr(opforge_diag_help_from_asm_report(report, 0, 0)) }
            .to_str()
            .expect("ffi help utf8");
        assert!(help.contains("Z80 dialect"), "unexpected help: {help}");

        let fixit_count = unsafe { opforge_diag_fixit_count_from_asm_report(report, 0) };
        assert!(fixit_count > 0, "expected parser fixit");
        let fixit_file =
            unsafe { CStr::from_ptr(opforge_diag_fixit_file_from_asm_report(report, 0, 0)) }
                .to_str()
                .expect("ffi fixit file utf8");
        assert_eq!(fixit_file, source_path.to_string_lossy());
        assert_eq!(
            unsafe { opforge_diag_fixit_line_from_asm_report(report, 0, 0) },
            2
        );
        assert!(unsafe { opforge_diag_fixit_col_start_from_asm_report(report, 0, 0) } > 0);
        assert!(unsafe { opforge_diag_fixit_col_end_from_asm_report(report, 0, 0) } > 0);
        let replacement =
            unsafe { CStr::from_ptr(opforge_diag_fixit_replacement_from_asm_report(report, 0, 0)) }
                .to_str()
                .expect("ffi fixit replacement utf8");
        assert_eq!(replacement, "MOV");
        let applicability = unsafe {
            CStr::from_ptr(opforge_diag_fixit_applicability_from_asm_report(
                report, 0, 0,
            ))
        }
        .to_str()
        .expect("ffi fixit applicability utf8");
        assert_eq!(applicability, "maybe-incorrect");

        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_memory_group_routes_outputs_through_callbacks() {
        let mut capture = CallbackCapture::default();
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let input_base = CString::new("/virtual/main").expect("input base cstr");
        let request = basic_request(
            root.as_ptr(),
            input_base.as_ptr(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            1,
        );
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");
        let callbacks = OpforgeOutputCallbacks {
            create_dir: Some(capture_create_dir),
            write_file: Some(capture_write_file),
            user_data: (&mut capture as *mut CallbackCapture).cast::<c_void>(),
        };

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(&request, source_text.as_ptr(), &callbacks)
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert!(capture
            .files
            .iter()
            .any(|(path, _)| path.ends_with("main.lst")));
        assert!(capture
            .files
            .iter()
            .any(|(path, _)| path.ends_with("main.hex")));
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_report_returning_entry_point_contains_forced_panic() {
        let work_dir = make_temp_dir("ffi-boundary-report-panic");
        let source_path = work_dir.join("main.asm");
        fs::write(&source_path, ".module main\n    nop\n.endmodule\n").expect("write source");
        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        ffi_test_hooks::arm("opforge_asm_assemble_file_with_request");
        let report = unsafe { opforge_asm_assemble_file_with_request(&request) };

        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::AssembleError
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi panic message utf8");
        assert!(message.contains("internal libopforge panic"), "{message}");
        assert!(
            message.contains("opforge_asm_assemble_file_with_request"),
            "{message}"
        );

        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_check_entry_point_contains_forced_panic() {
        let work_dir = make_temp_dir("ffi-boundary-check-panic");
        let source_path = work_dir.join("main.asm");
        fs::write(&source_path, ".module main\n    nop\n.endmodule\n").expect("write source");
        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        ffi_test_hooks::arm("opforge_asm_check_file_with_request");
        let report = unsafe { opforge_asm_check_file_with_request(&request) };

        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::AssembleError
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi panic message utf8");
        assert!(message.contains("internal libopforge panic"), "{message}");
        assert!(
            message.contains("opforge_asm_check_file_with_request"),
            "{message}"
        );

        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_nullable_handle_entry_point_contains_forced_panic() {
        let work_dir = make_temp_dir("ffi-boundary-session-panic");
        let source_path = work_dir.join("main.asm");
        fs::write(&source_path, ".module main\n    nop\n.endmodule\n").expect("write source");
        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        ffi_test_hooks::arm("opforge_asm_session_create_with_request");
        let session = unsafe { opforge_asm_session_create_with_request(&request) };

        assert!(session.is_null());
    }

    #[test]
    fn ffi_session_check_entry_point_contains_forced_panic() {
        let work_dir = make_temp_dir("ffi-boundary-session-check-panic");
        let source_path = work_dir.join("main.asm");
        fs::write(&source_path, ".module main\n    nop\n.endmodule\n").expect("write source");
        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        let session = unsafe { opforge_asm_session_create_with_request(&request) };
        assert!(!session.is_null());

        ffi_test_hooks::arm("opforge_asm_session_check");
        let report = unsafe { opforge_asm_session_check(session) };

        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::AssembleError
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi panic message utf8");
        assert!(message.contains("internal libopforge panic"), "{message}");
        assert!(message.contains("opforge_asm_session_check"), "{message}");

        unsafe { opforge_asm_report_free(report) };
        unsafe { opforge_asm_session_free(session) };
    }

    #[test]
    fn ffi_prepared_session_check_entry_point_contains_forced_panic() {
        let work_dir = make_temp_dir("ffi-boundary-prepared-check-panic");
        let source_path = work_dir.join("main.asm");
        fs::write(&source_path, ".module main\n    nop\n.endmodule\n").expect("write source");
        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        let session = unsafe { opforge_asm_session_create_with_request(&request) };
        assert!(!session.is_null());
        let prepared = unsafe { opforge_asm_session_prepare(session) };
        assert!(!prepared.is_null());

        ffi_test_hooks::arm("opforge_prepared_asm_session_check");
        let report = unsafe { opforge_prepared_asm_session_check(prepared) };

        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::AssembleError
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi panic message utf8");
        assert!(message.contains("internal libopforge panic"), "{message}");
        assert!(
            message.contains("opforge_prepared_asm_session_check"),
            "{message}"
        );

        unsafe { opforge_asm_report_free(report) };
        unsafe { opforge_prepared_asm_session_free(prepared) };
        unsafe { opforge_asm_session_free(session) };
    }

    #[test]
    fn ffi_prepared_session_assemble_handles_missing_prepared_state() {
        let prepared = OpforgePreparedAsmSession {
            prepared: None,
            failure: None,
        };

        let report = unsafe { opforge_prepared_asm_session_assemble(&prepared) };

        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::AssembleError
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi prepared message utf8");
        assert!(
            message.contains("missing prepared session state"),
            "{message}"
        );

        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_memory_group_rejects_output_requests_without_callbacks() {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            1,
        );
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(
                &request,
                source_text.as_ptr(),
                std::ptr::null(),
            )
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::InvalidRequest
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi memory request message utf8");
        assert!(message.contains("produced outputs"), "{message}");
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_check_memory_allows_missing_callbacks_when_outputs_are_suppressed() {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            1,
        );
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");

        let report = unsafe {
            opforge_asm_check_memory_with_request(&request, source_text.as_ptr(), std::ptr::null())
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_check_memory_with_request_allows_missing_write_file_when_no_outputs_exist() {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
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
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_ENABLE,
                output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
                go_addr: std::ptr::null(),
                bin_specs: OpforgeStringList {
                    items: std::ptr::null(),
                    count: 0,
                },
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
        };
        let callbacks = OpforgeOutputCallbacks {
            create_dir: None,
            write_file: None,
            user_data: std::ptr::null_mut(),
        };

        let report = unsafe {
            opforge_asm_check_memory_with_request(&request, source_text.as_ptr(), &callbacks)
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_check_memory_with_request_ignores_metadata_outputs_without_callbacks() {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let labels_path = CString::new("/virtual/out/symbols.lbl").expect("labels cstr");
        let dependency_path = CString::new("/virtual/out/main.d").expect("dependency cstr");
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
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
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_ENABLE,
                output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
                go_addr: std::ptr::null(),
                bin_specs: OpforgeStringList {
                    items: std::ptr::null(),
                    count: 0,
                },
                fill_byte: 0,
                fill_byte_set: 0,
                labels_file: labels_path.as_ptr(),
                label_output_format: OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT,
                dependency_output_path: dependency_path.as_ptr(),
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
        };

        let report = unsafe {
            opforge_asm_check_memory_with_request(&request, source_text.as_ptr(), std::ptr::null())
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_check_memory_with_request_suppresses_metadata_outputs_even_with_callbacks() {
        let mut capture = CallbackCapture::default();
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let labels_path = CString::new("/virtual/out/symbols.lbl").expect("labels cstr");
        let dependency_path = CString::new("/virtual/out/main.d").expect("dependency cstr");
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
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
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_ENABLE,
                output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
                go_addr: std::ptr::null(),
                bin_specs: OpforgeStringList {
                    items: std::ptr::null(),
                    count: 0,
                },
                fill_byte: 0,
                fill_byte_set: 0,
                labels_file: labels_path.as_ptr(),
                label_output_format: OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT,
                dependency_output_path: dependency_path.as_ptr(),
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
        };
        let callbacks = OpforgeOutputCallbacks {
            create_dir: Some(capture_create_dir),
            write_file: Some(capture_write_file),
            user_data: (&mut capture as *mut CallbackCapture).cast::<c_void>(),
        };

        let report = unsafe {
            opforge_asm_check_memory_with_request(&request, source_text.as_ptr(), &callbacks)
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
        assert!(
            capture.files.is_empty(),
            "captured files: {:?}",
            capture.files
        );
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_assemble_memory_with_request_resolves_filesystem_dependencies() {
        let temp_dir = make_temp_dir("asm-memory-fs-deps");
        let include_dir = temp_dir.join("includes");
        let module_dir = temp_dir.join("modules");
        fs::create_dir_all(&include_dir).expect("create include dir");
        fs::create_dir_all(&module_dir).expect("create module dir");
        fs::write(include_dir.join("inc.asm"), "FROM_INC .const 2\n").expect("write include");
        fs::write(
            module_dir.join("dep.asm"),
            ".module dep\n.pub\nVALUE .const 5\n.priv\n.endmodule\n",
        )
        .expect("write module");

        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let include_dir_cstr =
            CString::new(include_dir.to_string_lossy().as_bytes()).expect("include dir cstr");
        let module_dir_cstr =
            CString::new(module_dir.to_string_lossy().as_bytes()).expect("module dir cstr");
        let include_items = [include_dir_cstr.as_ptr()];
        let module_items = [module_dir_cstr.as_ptr()];
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
                defines: empty_string_list(),
                include_paths: OpforgeStringList {
                    items: include_items.as_ptr(),
                    count: include_items.len(),
                },
                module_paths: OpforgeStringList {
                    items: module_items.as_ptr(),
                    count: module_items.len(),
                },
                pp_macro_depth: 0,
            },
            execution: OpforgeAsmExecutionOptions {
                execution_mode: OPFORGE_EXECUTION_MODE_VM,
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_DISABLE,
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
        };
        let source_text = CString::new(
            ".module main\n.include \"inc.asm\"\n.use dep (VALUE)\nstart:\n    .byte FROM_INC + VALUE\n.endmodule\n",
        )
        .expect("source text cstr");

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(
                &request,
                source_text.as_ptr(),
                std::ptr::null(),
            )
        };

        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);

        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_memory_group_with_emit_outputs_zero_does_not_touch_filesystem() {
        let work_dir = make_temp_dir("asm-memory-no-fs");
        let root_path = work_dir.join("main.asm");
        let root = CString::new(root_path.to_string_lossy().as_bytes()).expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(
                &request,
                source_text.as_ptr(),
                std::ptr::null(),
            )
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
        assert!(!work_dir.join("main.lst").exists());
        assert!(!work_dir.join("main.hex").exists());
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_assemble_memory_errors_when_directive_outputs_have_no_callbacks() {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );
        let source_text = CString::new(
            ".module main\n.region ram, $1000, $10ff\n.section code\n.pub\nstart\n    .byte $42, $43\n.priv\n.endsection\n.place code in ram\n.mapfile \"build/minimal.map\", symbols=public\n.exportsections dir=\"build/minimal_sections\", format=bin\n.endmodule\n",
        )
        .expect("source text cstr");

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(
                &request,
                source_text.as_ptr(),
                std::ptr::null(),
            )
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::InvalidRequest
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi memory request message utf8");
        assert!(message.contains("produced outputs"), "{message}");
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_assemble_memory_with_request_errors_when_metadata_outputs_have_no_callbacks()
    {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let labels_path = CString::new("/virtual/out/symbols.lbl").expect("labels cstr");
        let dependency_path = CString::new("/virtual/out/main.d").expect("dependency cstr");
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
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
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_ENABLE,
                output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
                go_addr: std::ptr::null(),
                bin_specs: OpforgeStringList {
                    items: std::ptr::null(),
                    count: 0,
                },
                fill_byte: 0,
                fill_byte_set: 0,
                labels_file: labels_path.as_ptr(),
                label_output_format: OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT,
                dependency_output_path: dependency_path.as_ptr(),
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
        };

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(
                &request,
                source_text.as_ptr(),
                std::ptr::null(),
            )
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::InvalidRequest
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi grouped memory request message utf8");
        assert!(message.contains("produced outputs"), "{message}");
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_failed_memory_run_does_not_emit_output_callbacks() {
        let mut capture = CallbackCapture::default();
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let input_base = CString::new("/virtual/main").expect("input base cstr");
        let labels_path = CString::new("/virtual/out/symbols.lbl").expect("labels cstr");
        let dependency_path = CString::new("/virtual/out/main.d").expect("dependency cstr");
        let source_text = CString::new(
            ".module main\n.region ram, $1000, $10ff\n.section code\n.pub\nstart\n    .byte MISSING_VALUE\n.priv\n.endsection\n.place code in ram\n.output \"build/minimal.bin\", format=bin, sections=code\n.endmodule\n",
        )
        .expect("source text cstr");
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
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_ENABLE,
                output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
                go_addr: std::ptr::null(),
                bin_specs: OpforgeStringList {
                    items: std::ptr::null(),
                    count: 0,
                },
                fill_byte: 0,
                fill_byte_set: 0,
                labels_file: labels_path.as_ptr(),
                label_output_format: OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT,
                dependency_output_path: dependency_path.as_ptr(),
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
        };
        let callbacks = OpforgeOutputCallbacks {
            create_dir: Some(capture_create_dir),
            write_file: Some(capture_write_file),
            user_data: (&mut capture as *mut CallbackCapture).cast::<c_void>(),
        };

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(&request, source_text.as_ptr(), &callbacks)
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::AssembleError
        );
        assert!(unsafe { opforge_asm_report_error_count(report) } > 0);
        assert!(
            capture.files.is_empty(),
            "captured files: {:?}",
            capture.files
        );
        assert!(capture.dirs.is_empty(), "captured dirs: {:?}", capture.dirs);
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_assemble_memory_with_request_parses_bin_specs_and_fill_byte() {
        let mut capture = CallbackCapture::default();
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let input_base = CString::new("/virtual/main").expect("input base cstr");
        let source_text = CString::new(
            ".module main\n.region ram, $1000, $10ff\n.section code\n.pub\nstart\n    .byte $42, $43\n.priv\n.endsection\n.place code in ram\n.endmodule\n",
        )
        .expect("source text cstr");
        let bin_spec = CString::new("out.bin:1000:1003").expect("bin spec cstr");
        let bin_spec_items = [bin_spec.as_ptr()];
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
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_DISABLE,
                output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
                go_addr: std::ptr::null(),
                bin_specs: OpforgeStringList {
                    items: bin_spec_items.as_ptr(),
                    count: bin_spec_items.len(),
                },
                fill_byte: 0xaa,
                fill_byte_set: 1,
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
        };
        let callbacks = OpforgeOutputCallbacks {
            create_dir: Some(capture_create_dir),
            write_file: Some(capture_write_file),
            user_data: (&mut capture as *mut CallbackCapture).cast::<c_void>(),
        };

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(&request, source_text.as_ptr(), &callbacks)
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        let bin_file = capture
            .files
            .iter()
            .find(|(path, _)| path.ends_with("out.bin"))
            .expect("captured bin file");
        assert_eq!(bin_file.1, vec![0x42, 0x43, 0xaa, 0xaa]);
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_assemble_memory_with_request_rejects_invalid_bin_specs() {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");
        let bin_spec = CString::new("bad:spec:range:extra").expect("bin spec cstr");
        let bin_spec_items = [bin_spec.as_ptr()];
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
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
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_DISABLE,
                output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
                go_addr: std::ptr::null(),
                bin_specs: OpforgeStringList {
                    items: bin_spec_items.as_ptr(),
                    count: bin_spec_items.len(),
                },
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
        };

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(
                &request,
                source_text.as_ptr(),
                std::ptr::null(),
            )
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::InvalidRequest
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi grouped invalid bin spec message utf8");
        assert!(
            message.contains("output.bin_specs entry is invalid"),
            "{message}"
        );
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_assemble_memory_with_request_rejects_invalid_opasm_package_path() {
        let temp_dir = make_temp_dir("ffi-grouped-opasm-package");
        let bad_package_path = temp_dir.join("broken-runtime.opasm");
        fs::write(&bad_package_path, "not a valid opasm package").expect("write bad package");

        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");
        let package_path =
            CString::new(bad_package_path.to_string_lossy().as_bytes()).expect("package path cstr");
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
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
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: package_path.as_ptr(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_DISABLE,
                output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
                go_addr: std::ptr::null(),
                bin_specs: OpforgeStringList {
                    items: std::ptr::null(),
                    count: 0,
                },
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
        };

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(
                &request,
                source_text.as_ptr(),
                std::ptr::null(),
            )
        };
        assert!(!report.is_null());
        assert_ne!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_grouped_default_label_output_matches_stable_rust_default() {
        let mut capture = CallbackCapture::default();
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let labels_path = CString::new("/virtual/out/symbols.lbl").expect("labels cstr");
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
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
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_DISABLE,
                output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
                go_addr: std::ptr::null(),
                bin_specs: OpforgeStringList {
                    items: std::ptr::null(),
                    count: 0,
                },
                fill_byte: 0,
                fill_byte_set: 0,
                labels_file: labels_path.as_ptr(),
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
        };
        let callbacks = OpforgeOutputCallbacks {
            create_dir: Some(capture_create_dir),
            write_file: Some(capture_write_file),
            user_data: (&mut capture as *mut CallbackCapture).cast::<c_void>(),
        };

        assert_eq!(
            api::asm::OwnedOutputOptions::default().label_output_format,
            LabelOutputFormat::Vice
        );

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(&request, source_text.as_ptr(), &callbacks)
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );

        let labels_file = capture
            .files
            .iter()
            .find(|(path, _)| path.ends_with("symbols.lbl"))
            .expect("captured labels file");
        let labels_text = std::str::from_utf8(&labels_file.1).expect("labels utf8");
        assert!(labels_text.contains("al C:"), "labels:\n{labels_text}");

        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_grouped_zero_initialized_fields_match_stable_rust_defaults() {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
                defines: empty_string_list(),
                include_paths: empty_string_list(),
                module_paths: empty_string_list(),
                pp_macro_depth: 0,
            },
            execution: OpforgeAsmExecutionOptions {
                execution_mode: OPFORGE_EXECUTION_MODE_DEFAULT,
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_DEFAULT,
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
        };

        let (_, config) = match build_grouped_high_level_config(&request) {
            Ok(value) => value,
            Err(_) => panic!("valid grouped config"),
        };

        assert_eq!(
            config.execution.execution_mode,
            api::asm::OwnedExecutionOptions::default().execution_mode
        );
        assert_eq!(
            config.output.default_outputs,
            api::asm::OwnedOutputOptions::default().default_outputs
        );
    }

    #[test]
    fn ffi_grouped_request_init_matches_stable_rust_defaults() {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let mut request = unsafe { std::mem::zeroed::<OpforgeAsmRequest>() };

        unsafe { opforge_asm_request_init(&mut request) };
        request.source.root_path = root.as_ptr();

        let (_, config) = match build_grouped_high_level_config(&request) {
            Ok(value) => value,
            Err(_) => panic!("valid grouped config"),
        };

        assert_eq!(
            request.execution.execution_mode,
            OPFORGE_EXECUTION_MODE_DEFAULT
        );
        assert_eq!(request.output.emit_outputs, OPFORGE_DEFAULT_OUTPUTS_DEFAULT);
        assert_eq!(
            config.execution.execution_mode,
            api::asm::OwnedExecutionOptions::default().execution_mode
        );
        assert_eq!(
            config.output.default_outputs,
            api::asm::OwnedOutputOptions::default().default_outputs
        );
    }

    #[test]
    fn ffi_grouped_explicit_output_disable_overrides_default_outputs() {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
                defines: empty_string_list(),
                include_paths: empty_string_list(),
                module_paths: empty_string_list(),
                pp_macro_depth: 0,
            },
            execution: OpforgeAsmExecutionOptions {
                execution_mode: OPFORGE_EXECUTION_MODE_DEFAULT,
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_DISABLE,
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
        };

        let (_, config) = match build_grouped_high_level_config(&request) {
            Ok(value) => value,
            Err(_) => panic!("valid grouped config"),
        };
        assert!(!config.output.default_outputs);
    }

    #[test]
    fn ffi_opforge_asm_assemble_memory_with_request_suppress_outputs_skips_callback_requirement() {
        let root = CString::new("/virtual/main.asm").expect("root cstr");
        let labels_path = CString::new("/virtual/out/symbols.lbl").expect("labels cstr");
        let dependency_path = CString::new("/virtual/out/main.d").expect("dependency cstr");
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");
        let request = OpforgeAsmRequest {
            source: OpforgeAsmSourceOptions {
                root_path: root.as_ptr(),
                output_base: std::ptr::null(),
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
                cpu_override: std::ptr::null(),
                max_loop_iterations: 0,
                opasm_package_path: std::ptr::null(),
            },
            output: OpforgeAsmOutputOptions {
                out_dir: std::ptr::null(),
                emit_outputs: OPFORGE_DEFAULT_OUTPUTS_DISABLE,
                output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
                go_addr: std::ptr::null(),
                bin_specs: OpforgeStringList {
                    items: std::ptr::null(),
                    count: 0,
                },
                fill_byte: 0,
                fill_byte_set: 0,
                labels_file: labels_path.as_ptr(),
                label_output_format: OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT,
                dependency_output_path: dependency_path.as_ptr(),
                dependency_append: 0,
                dependency_make_phony: 0,
                outfile_override: std::ptr::null(),
                list_name_override: std::ptr::null(),
                hex_name_override: std::ptr::null(),
                header_title: std::ptr::null(),
                no_outputs: 1,
            },
            diagnostics: OpforgeAsmDiagnosticsOptions {
                debug_conditionals: 0,
                tab_size: 0,
            },
        };

        let report = unsafe {
            opforge_asm_assemble_memory_with_request(
                &request,
                source_text.as_ptr(),
                std::ptr::null(),
            )
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_asm_check_memory_group_with_emit_outputs_zero_does_not_touch_filesystem() {
        let work_dir = make_temp_dir("asm-check-memory-no-fs");
        let root_path = work_dir.join("main.asm");
        let root = CString::new(root_path.to_string_lossy().as_bytes()).expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );
        let source_text =
            CString::new(".module main\nstart:\n    nop\n.endmodule\n").expect("source text cstr");

        let report = unsafe {
            opforge_asm_check_memory_with_request(&request, source_text.as_ptr(), std::ptr::null())
        };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::Ok
        );
        assert_eq!(unsafe { opforge_asm_report_error_count(report) }, 0);
        assert!(!work_dir.join("main.lst").exists());
        assert!(!work_dir.join("main.hex").exists());
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opcore_tokenize_group_enumerates_tokens_and_spans() {
        let line = CString::new("lda #$10").expect("line cstr");
        let report = unsafe { opforge_opcore_tokenize_line(line.as_ptr(), 7) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_status(report) },
            OpforgeProcessorStatus::Ok
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_count(report) },
            3
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_kind(report, 0) },
            OpforgeTokenKind::Identifier
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_kind(report, 1) },
            OpforgeTokenKind::Hash
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_kind(report, 2) },
            OpforgeTokenKind::Number
        );
        let token_text =
            unsafe { CStr::from_ptr(opforge_opcore_tokenize_report_token_text(report, 0)) }
                .to_str()
                .expect("ffi token text utf8");
        assert_eq!(token_text, "lda");
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_line(report, 2) },
            7
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_col_start(report, 2) },
            6
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_col_end(report, 2) },
            9
        );
        unsafe { opforge_opcore_tokenize_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opcore_tokenize_group_reports_tokenize_errors() {
        let line = CString::new("\"unterminated").expect("line cstr");
        let report = unsafe { opforge_opcore_tokenize_line(line.as_ptr(), 12) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_status(report) },
            OpforgeProcessorStatus::TokenizeError
        );
        let message =
            unsafe { CStr::from_ptr(opforge_opcore_tokenize_report_error_message(report)) }
                .to_str()
                .expect("ffi tokenize error utf8");
        assert!(!message.is_empty());
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_error_line(report) },
            12
        );
        assert!(unsafe { opforge_opcore_tokenize_report_error_col_start(report) } > 0);
        assert!(
            unsafe { opforge_opcore_tokenize_report_error_col_end(report) }
                >= unsafe { opforge_opcore_tokenize_report_error_col_start(report) }
        );
        unsafe { opforge_opcore_tokenize_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opasm_tokenize_group_enumerates_statement_tokens_and_spans() {
        let line = CString::new(".byte 1, 2").expect("line cstr");
        let report = unsafe { opforge_opasm_tokenize_statement(line.as_ptr(), 13) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_status(report) },
            OpforgeProcessorStatus::Ok
        );
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_token_count(report) },
            5
        );
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_token_kind(report, 0) },
            OpforgeTokenKind::Dot
        );
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_token_kind(report, 1) },
            OpforgeTokenKind::Identifier
        );
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_token_kind(report, 3) },
            OpforgeTokenKind::Comma
        );
        let token_text =
            unsafe { CStr::from_ptr(opforge_opasm_tokenize_report_token_text(report, 1)) }
                .to_str()
                .expect("ffi opasm token text utf8");
        assert_eq!(token_text, "byte");
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_token_line(report, 4) },
            13
        );
        assert!(unsafe { opforge_opasm_tokenize_report_token_col_start(report, 4) } > 0);
        assert!(
            unsafe { opforge_opasm_tokenize_report_token_col_end(report, 4) }
                >= unsafe { opforge_opasm_tokenize_report_token_col_start(report, 4) }
        );
        unsafe { opforge_opasm_tokenize_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opasm_tokenize_group_reports_statement_errors() {
        let line = CString::new("\"unterminated").expect("line cstr");
        let report = unsafe { opforge_opasm_tokenize_statement(line.as_ptr(), 14) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_status(report) },
            OpforgeProcessorStatus::ParseError
        );
        let message =
            unsafe { CStr::from_ptr(opforge_opasm_tokenize_report_error_message(report)) }
                .to_str()
                .expect("ffi opasm tokenize error utf8");
        assert!(!message.is_empty());
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_error_line(report) },
            14
        );
        assert!(unsafe { opforge_opasm_tokenize_report_error_col_start(report) } > 0);
        assert!(
            unsafe { opforge_opasm_tokenize_report_error_col_end(report) }
                >= unsafe { opforge_opasm_tokenize_report_error_col_start(report) }
        );
        unsafe { opforge_opasm_tokenize_report_free(report) };
    }

    #[test]
    fn ffi_tokenize_report_accessors_preserve_null_defaults_across_surfaces() {
        let opcore = std::ptr::null::<OpforgeOpcoreTokenizeReport>();
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_status(opcore) },
            OpforgeProcessorStatus::InvalidRequest
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_count(opcore) },
            0
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_kind(opcore, 0) },
            OpforgeTokenKind::Invalid
        );
        assert!(unsafe { opforge_opcore_tokenize_report_token_text(opcore, 0) }.is_null());
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_line(opcore, 0) },
            0
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_col_start(opcore, 0) },
            0
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_token_col_end(opcore, 0) },
            0
        );
        assert!(unsafe { opforge_opcore_tokenize_report_error_message(opcore) }.is_null());
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_error_line(opcore) },
            0
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_error_col_start(opcore) },
            0
        );
        assert_eq!(
            unsafe { opforge_opcore_tokenize_report_error_col_end(opcore) },
            0
        );
        unsafe { opforge_opcore_tokenize_report_free(std::ptr::null_mut()) };

        let opasm = std::ptr::null::<OpforgeOpasmTokenizeReport>();
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_status(opasm) },
            OpforgeProcessorStatus::InvalidRequest
        );
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_token_count(opasm) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_token_kind(opasm, 0) },
            OpforgeTokenKind::Invalid
        );
        assert!(unsafe { opforge_opasm_tokenize_report_token_text(opasm, 0) }.is_null());
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_token_line(opasm, 0) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_token_col_start(opasm, 0) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_token_col_end(opasm, 0) },
            0
        );
        assert!(unsafe { opforge_opasm_tokenize_report_error_message(opasm) }.is_null());
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_error_line(opasm) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_error_col_start(opasm) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_tokenize_report_error_col_end(opasm) },
            0
        );
        unsafe { opforge_opasm_tokenize_report_free(std::ptr::null_mut()) };
    }

    #[test]
    fn ffi_opforge_opasm_parse_group_enumerates_statement_shape() {
        let line = CString::new(".byte 1, value").expect("line cstr");
        let report = unsafe { opforge_opasm_parse_statement(line.as_ptr(), 15) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opasm_parse_report_status(report) },
            OpforgeProcessorStatus::Ok
        );
        assert_eq!(
            unsafe { opforge_opasm_parse_report_kind(report) },
            OpforgeLineAstKind::Statement
        );
        let mnemonic =
            unsafe { CStr::from_ptr(opforge_opasm_parse_report_statement_mnemonic(report)) }
                .to_str()
                .expect("ffi opasm parse mnemonic utf8");
        assert_eq!(mnemonic, ".byte");
        assert_eq!(
            unsafe { opforge_opasm_parse_report_statement_operand_count(report) },
            2
        );
        let operand0 =
            unsafe { CStr::from_ptr(opforge_opasm_parse_report_statement_operand_text(report, 0)) }
                .to_str()
                .expect("ffi opasm parse operand 0 utf8");
        let operand1 =
            unsafe { CStr::from_ptr(opforge_opasm_parse_report_statement_operand_text(report, 1)) }
                .to_str()
                .expect("ffi opasm parse operand 1 utf8");
        assert_eq!(operand0, "1");
        assert_eq!(operand1, "value");
        unsafe { opforge_opasm_parse_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opasm_parse_group_rejects_use_shape() {
        let line = CString::new(".use math as m { add, sub }").expect("line cstr");
        let report = unsafe { opforge_opasm_parse_statement(line.as_ptr(), 16) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opasm_parse_report_status(report) },
            OpforgeProcessorStatus::ParseError
        );
        let message = unsafe { CStr::from_ptr(opforge_opasm_parse_report_error_message(report)) }
            .to_str()
            .expect("ffi opasm parse error utf8");
        assert!(
            message.contains("Unexpected trailing tokens after .use"),
            "{message}"
        );
        unsafe { opforge_opasm_parse_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opasm_parse_group_reports_statement_errors() {
        let line = CString::new("\"unterminated").expect("line cstr");
        let report = unsafe { opforge_opasm_parse_statement(line.as_ptr(), 17) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opasm_parse_report_status(report) },
            OpforgeProcessorStatus::ParseError
        );
        let message = unsafe { CStr::from_ptr(opforge_opasm_parse_report_error_message(report)) }
            .to_str()
            .expect("ffi opasm parse error utf8");
        assert!(!message.is_empty());
        assert_eq!(unsafe { opforge_opasm_parse_report_error_line(report) }, 17);
        assert!(unsafe { opforge_opasm_parse_report_error_col_start(report) } > 0);
        assert!(
            unsafe { opforge_opasm_parse_report_error_col_end(report) }
                >= unsafe { opforge_opasm_parse_report_error_col_start(report) }
        );
        unsafe { opforge_opasm_parse_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opasm_process_group_reports_rust_trace_summary() {
        let line = CString::new(".module demo").expect("line cstr");
        let request = OpforgeOpasmProcessConfig {
            line: line.as_ptr(),
            line_num: 18,
            execution_mode: OPFORGE_EXECUTION_MODE_RUST,
            cpu_id: std::ptr::null(),
            dialect_override: std::ptr::null(),
        };
        let report = unsafe { opforge_opasm_process_statement(&request) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opasm_process_report_status(report) },
            OpforgeProcessorStatus::Ok
        );
        assert_eq!(
            unsafe { opforge_opasm_process_report_kind(report) },
            OpforgeLineAstKind::Statement
        );
        assert_eq!(
            unsafe { opforge_opasm_process_report_trace_request_count(report) },
            1
        );
        assert_eq!(
            unsafe { opforge_opasm_process_report_lockstep_match_count(report) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_process_report_lockstep_divergence_count(report) },
            0
        );
        unsafe { opforge_opasm_process_report_free(report) };
    }

    #[test]
    fn ffi_opforge_processing_group_reads_trace_requests_from_opasm_report() {
        let line = CString::new("    lda #$42").expect("line cstr");
        let cpu_id = CString::new("m6502").expect("cpu cstr");
        let request = OpforgeOpasmProcessConfig {
            line: line.as_ptr(),
            line_num: 18,
            execution_mode: OPFORGE_EXECUTION_MODE_LOCKSTEP_RUST,
            cpu_id: cpu_id.as_ptr(),
            dialect_override: std::ptr::null(),
        };
        let report = unsafe { opforge_opasm_process_statement(&request) };
        assert!(!report.is_null());

        let trace = unsafe { opforge_opasm_process_report_processing_trace(report) };
        assert!(!trace.is_null());
        assert_eq!(
            unsafe { opforge_processing_trace_request_count(trace) },
            unsafe { opforge_opasm_process_report_trace_request_count(report) }
        );
        assert_eq!(unsafe { opforge_processing_trace_request_count(trace) }, 1);

        let request0 = unsafe { CStr::from_ptr(opforge_processing_trace_request_text(trace, 0)) }
            .to_str()
            .expect("ffi processing trace request0 utf8");
        assert_eq!(request0, "processor:asm:statement");

        unsafe { opforge_processing_trace_free(trace) };
        unsafe { opforge_opasm_process_report_free(report) };
    }

    #[test]
    fn ffi_opforge_lockstep_group_reads_report_from_opasm_processing() {
        let line = CString::new("    lda #$42").expect("line cstr");
        let cpu_id = CString::new("m6502").expect("cpu cstr");
        let request = OpforgeOpasmProcessConfig {
            line: line.as_ptr(),
            line_num: 19,
            execution_mode: OPFORGE_EXECUTION_MODE_LOCKSTEP_RUST,
            cpu_id: cpu_id.as_ptr(),
            dialect_override: std::ptr::null(),
        };
        let report = unsafe { opforge_opasm_process_statement(&request) };
        assert!(!report.is_null());

        let lockstep = unsafe { opforge_opasm_process_report_lockstep_report(report) };
        assert!(!lockstep.is_null());
        let match_count = unsafe { opforge_lockstep_report_match_count(lockstep) };
        let divergence_count = unsafe { opforge_lockstep_report_divergence_count(lockstep) };
        assert_eq!(match_count, unsafe {
            opforge_opasm_process_report_lockstep_match_count(report)
        });
        assert_eq!(divergence_count, unsafe {
            opforge_opasm_process_report_lockstep_divergence_count(report)
        });
        assert!(match_count > 0 || divergence_count > 0);

        if match_count > 0 {
            let stage =
                unsafe { CStr::from_ptr(opforge_lockstep_report_match_stage_text(lockstep, 0)) }
                    .to_str()
                    .expect("ffi lockstep match stage utf8");
            let request =
                unsafe { CStr::from_ptr(opforge_lockstep_report_match_request_text(lockstep, 0)) }
                    .to_str()
                    .expect("ffi lockstep match request utf8");
            assert!(!stage.is_empty());
            assert!(!request.is_empty());
        } else {
            let reason = unsafe {
                CStr::from_ptr(opforge_lockstep_report_divergence_reason_code(lockstep, 0))
            }
            .to_str()
            .expect("ffi lockstep divergence reason utf8");
            assert!(!reason.is_empty());
        }

        unsafe { opforge_lockstep_report_free(lockstep) };
        unsafe { opforge_opasm_process_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opasm_process_group_supports_lockstep_processing() {
        let line = CString::new("    lda #$42").expect("line cstr");
        let cpu_id = CString::new("m6502").expect("cpu cstr");
        let request = OpforgeOpasmProcessConfig {
            line: line.as_ptr(),
            line_num: 19,
            execution_mode: OPFORGE_EXECUTION_MODE_LOCKSTEP_RUST,
            cpu_id: cpu_id.as_ptr(),
            dialect_override: std::ptr::null(),
        };
        let report = unsafe { opforge_opasm_process_statement(&request) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opasm_process_report_status(report) },
            OpforgeProcessorStatus::Ok
        );
        let mnemonic =
            unsafe { CStr::from_ptr(opforge_opasm_process_report_statement_mnemonic(report)) }
                .to_str()
                .expect("ffi opasm process mnemonic utf8");
        assert_eq!(mnemonic, "lda");
        assert_eq!(
            unsafe { opforge_opasm_process_report_statement_operand_count(report) },
            1
        );
        let operand0 = unsafe {
            CStr::from_ptr(opforge_opasm_process_report_statement_operand_text(
                report, 0,
            ))
        }
        .to_str()
        .expect("ffi opasm process operand0 utf8");
        assert_eq!(operand0, "#$42");
        assert_eq!(
            unsafe { opforge_opasm_process_report_trace_request_count(report) },
            1
        );
        assert!(
            unsafe { opforge_opasm_process_report_lockstep_match_count(report) } > 0
                || unsafe { opforge_opasm_process_report_lockstep_divergence_count(report) } > 0
        );
        unsafe { opforge_opasm_process_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opasm_process_group_reports_invalid_vm_requests() {
        let line = CString::new("lda #$42").expect("line cstr");
        let request = OpforgeOpasmProcessConfig {
            line: line.as_ptr(),
            line_num: 20,
            execution_mode: OPFORGE_EXECUTION_MODE_VM,
            cpu_id: std::ptr::null(),
            dialect_override: std::ptr::null(),
        };
        let report = unsafe { opforge_opasm_process_statement(&request) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opasm_process_report_status(report) },
            OpforgeProcessorStatus::ParseError
        );
        let message = unsafe { CStr::from_ptr(opforge_opasm_process_report_error_message(report)) }
            .to_str()
            .expect("ffi opasm process error utf8");
        assert!(message.contains("cpu_id"));
        assert_eq!(
            unsafe { opforge_opasm_process_report_error_line(report) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_process_report_error_col_start(report) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_process_report_error_col_end(report) },
            0
        );
        unsafe { opforge_opasm_process_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opcore_expr_group_enumerates_expression_tree() {
        let line = CString::new("1 + value").expect("line cstr");
        let report = unsafe { opforge_opcore_parse_expression(line.as_ptr(), 9) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opcore_expr_report_status(report) },
            OpforgeProcessorStatus::Ok
        );
        assert_eq!(unsafe { opforge_opcore_expr_report_node_count(report) }, 3);
        assert_eq!(
            unsafe { opforge_opcore_expr_report_node_kind(report, 0) },
            OpforgeExprNodeKind::Binary
        );
        let root_text = unsafe { CStr::from_ptr(opforge_opcore_expr_report_node_text(report, 0)) }
            .to_str()
            .expect("ffi expr root text utf8");
        assert_eq!(root_text, "Add");
        assert_eq!(
            unsafe { opforge_opcore_expr_report_node_child_count(report, 0) },
            2
        );
        assert_eq!(
            unsafe { opforge_opcore_expr_report_node_child(report, 0, 0) },
            1
        );
        assert_eq!(
            unsafe { opforge_opcore_expr_report_node_child(report, 0, 1) },
            2
        );
        assert_eq!(
            unsafe { opforge_opcore_expr_report_node_kind(report, 1) },
            OpforgeExprNodeKind::Number
        );
        assert_eq!(
            unsafe { opforge_opcore_expr_report_node_kind(report, 2) },
            OpforgeExprNodeKind::Identifier
        );
        let ident_text = unsafe { CStr::from_ptr(opforge_opcore_expr_report_node_text(report, 2)) }
            .to_str()
            .expect("ffi expr ident text utf8");
        assert_eq!(ident_text, "value");
        assert_eq!(
            unsafe { opforge_opcore_expr_report_node_line(report, 2) },
            9
        );
        unsafe { opforge_opcore_expr_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opcore_expr_group_reports_parse_errors() {
        let line = CString::new("1 +").expect("line cstr");
        let report = unsafe { opforge_opcore_parse_expression(line.as_ptr(), 15) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opcore_expr_report_status(report) },
            OpforgeProcessorStatus::ParseError
        );
        let message = unsafe { CStr::from_ptr(opforge_opcore_expr_report_error_message(report)) }
            .to_str()
            .expect("ffi expr parse error utf8");
        assert!(!message.is_empty());
        assert_eq!(unsafe { opforge_opcore_expr_report_error_line(report) }, 15);
        assert!(unsafe { opforge_opcore_expr_report_error_col_start(report) } > 0);
        assert!(
            unsafe { opforge_opcore_expr_report_error_col_end(report) }
                >= unsafe { opforge_opcore_expr_report_error_col_start(report) }
        );
        unsafe { opforge_opcore_expr_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opcore_module_item_group_enumerates_use_directive() {
        let line = CString::new(".use math as m (foo, bar)").expect("line cstr");
        let report = unsafe { opforge_opcore_process_module_item(line.as_ptr(), 21) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_status(report) },
            OpforgeProcessorStatus::Ok
        );
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_kind(report) },
            OpforgeLineAstKind::Use
        );
        let module_id =
            unsafe { CStr::from_ptr(opforge_opcore_module_item_report_use_module_id(report)) }
                .to_str()
                .expect("ffi use module id utf8");
        assert_eq!(module_id, "math");
        let alias = unsafe { CStr::from_ptr(opforge_opcore_module_item_report_use_alias(report)) }
            .to_str()
            .expect("ffi use alias utf8");
        assert_eq!(alias, "m");
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_use_item_count(report) },
            2
        );
        let first_item =
            unsafe { CStr::from_ptr(opforge_opcore_module_item_report_use_item_name(report, 0)) }
                .to_str()
                .expect("ffi use item utf8");
        let second_item =
            unsafe { CStr::from_ptr(opforge_opcore_module_item_report_use_item_name(report, 1)) }
                .to_str()
                .expect("ffi use item utf8");
        assert_eq!(first_item, "foo");
        assert_eq!(second_item, "bar");
        unsafe { opforge_opcore_module_item_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opcore_module_item_group_enumerates_module_statement() {
        let line = CString::new(".module demo").expect("line cstr");
        let report = unsafe { opforge_opcore_process_module_item(line.as_ptr(), 22) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_status(report) },
            OpforgeProcessorStatus::Ok
        );
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_kind(report) },
            OpforgeLineAstKind::Statement
        );
        let mnemonic =
            unsafe { CStr::from_ptr(opforge_opcore_module_item_report_statement_mnemonic(report)) }
                .to_str()
                .expect("ffi statement mnemonic utf8");
        assert_eq!(mnemonic, ".module");
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_statement_operand_count(report) },
            1
        );
        let operand = unsafe {
            CStr::from_ptr(opforge_opcore_module_item_report_statement_operand_text(
                report, 0,
            ))
        }
        .to_str()
        .expect("ffi statement operand utf8");
        assert_eq!(operand, "demo");
        unsafe { opforge_opcore_module_item_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opcore_module_item_group_reports_processor_return() {
        let line = CString::new("lda #1").expect("line cstr");
        let report = unsafe { opforge_opcore_process_module_item(line.as_ptr(), 23) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_status(report) },
            OpforgeProcessorStatus::Returned
        );
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_kind(report) },
            OpforgeLineAstKind::Invalid
        );
        unsafe { opforge_opcore_module_item_report_free(report) };
    }

    #[test]
    fn ffi_opforge_opcore_module_item_group_reports_parse_errors() {
        let line = CString::new(".use").expect("line cstr");
        let report = unsafe { opforge_opcore_process_module_item(line.as_ptr(), 24) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_status(report) },
            OpforgeProcessorStatus::ParseError
        );
        let message =
            unsafe { CStr::from_ptr(opforge_opcore_module_item_report_error_message(report)) }
                .to_str()
                .expect("ffi module-item error utf8");
        assert!(!message.is_empty());
        unsafe { opforge_opcore_module_item_report_free(report) };
    }

    #[test]
    fn ffi_module_item_report_accessors_preserve_null_defaults_across_surfaces() {
        let parse = std::ptr::null::<OpforgeOpasmParseReport>();
        assert_eq!(
            unsafe { opforge_opasm_parse_report_status(parse) },
            OpforgeProcessorStatus::InvalidRequest
        );
        assert_eq!(
            unsafe { opforge_opasm_parse_report_kind(parse) },
            OpforgeLineAstKind::Invalid
        );
        assert_eq!(
            unsafe { opforge_opasm_parse_report_statement_operand_count(parse) },
            0
        );
        assert!(unsafe { opforge_opasm_parse_report_error_message(parse) }.is_null());
        assert_eq!(unsafe { opforge_opasm_parse_report_error_line(parse) }, 0);
        assert_eq!(
            unsafe { opforge_opasm_parse_report_error_col_start(parse) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_parse_report_error_col_end(parse) },
            0
        );
        unsafe { opforge_opasm_parse_report_free(std::ptr::null_mut()) };

        let process = std::ptr::null::<OpforgeOpasmProcessReport>();
        assert_eq!(
            unsafe { opforge_opasm_process_report_status(process) },
            OpforgeProcessorStatus::InvalidRequest
        );
        assert_eq!(
            unsafe { opforge_opasm_process_report_kind(process) },
            OpforgeLineAstKind::Invalid
        );
        assert_eq!(
            unsafe { opforge_opasm_process_report_statement_operand_count(process) },
            0
        );
        assert!(unsafe { opforge_opasm_process_report_error_message(process) }.is_null());
        assert_eq!(
            unsafe { opforge_opasm_process_report_error_line(process) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_process_report_error_col_start(process) },
            0
        );
        assert_eq!(
            unsafe { opforge_opasm_process_report_error_col_end(process) },
            0
        );
        unsafe { opforge_opasm_process_report_free(std::ptr::null_mut()) };

        let module_item = std::ptr::null::<OpforgeOpcoreModuleItemReport>();
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_status(module_item) },
            OpforgeProcessorStatus::InvalidRequest
        );
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_kind(module_item) },
            OpforgeLineAstKind::Invalid
        );
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_statement_operand_count(module_item) },
            0
        );
        assert!(unsafe { opforge_opcore_module_item_report_error_message(module_item) }.is_null());
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_error_line(module_item) },
            0
        );
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_error_col_start(module_item) },
            0
        );
        assert_eq!(
            unsafe { opforge_opcore_module_item_report_error_col_end(module_item) },
            0
        );
        unsafe { opforge_opcore_module_item_report_free(std::ptr::null_mut()) };
    }

    #[test]
    fn ffi_rejects_null_request_pointer() {
        let report = unsafe { opforge_asm_assemble_file_with_request(std::ptr::null()) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::InvalidRequest
        );
        assert!(!unsafe { opforge_asm_report_message(report) }.is_null());
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_rejects_invalid_execution_mode_value() {
        let work_dir = make_temp_dir("invalid-mode");
        let source_path = work_dir.join("main.asm");
        fs::write(&source_path, ".module main\nstart:\n    nop\n.endmodule\n")
            .expect("write source");

        let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
        let request = basic_request(
            root.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            u32::MAX,
            0,
        );

        let report = unsafe { opforge_asm_assemble_file_with_request(&request) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::InvalidRequest
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi invalid request message utf8");
        assert!(message.contains("execution_mode"));
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_rejects_null_root_path() {
        let request = basic_request(
            std::ptr::null(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        let report = unsafe { opforge_asm_assemble_file_with_request(&request) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::InvalidRequest
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi invalid request message utf8");
        assert!(message.contains("root_path"));
        unsafe { opforge_asm_report_free(report) };
    }

    #[test]
    fn ffi_rejects_invalid_utf8_request_fields() {
        let request = basic_request(
            invalid_utf8_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            OPFORGE_EXECUTION_MODE_VM,
            0,
        );

        let report = unsafe { opforge_asm_assemble_file_with_request(&request) };
        assert!(!report.is_null());
        assert_eq!(
            unsafe { opforge_asm_report_status(report) },
            OpforgeStatus::InvalidRequest
        );
        let message = unsafe { CStr::from_ptr(opforge_asm_report_message(report)) }
            .to_str()
            .expect("ffi invalid request message utf8");
        assert!(message.contains("UTF-8"));
        unsafe { opforge_asm_report_free(report) };
    }
}
