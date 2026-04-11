// SPDX-License-Identifier: GPL-3.0-or-later

//! Assembly session orchestration for libopforge.

mod io;
mod processing;
mod source_graph;
#[cfg(test)]
mod source_graph_tests;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::io::Write;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{Arc, Mutex, OnceLock};

use asm::engine::Assembler;
use asm::error::{AsmError, AsmErrorKind, AsmRunError, AsmRunReport, Diagnostic, Severity};
use asm::line::RuntimeLineRouter;
use asm::output::{
    anchor_relative_output_path, format_addr, resolve_bin_path_checked, resolve_output_base,
    resolve_output_path_checked, BinOutputSpec, BinRange, DependencyOutputPolicy,
    ExportSectionsDirective, LinkerOutputDirective, LinkerOutputFormat, MapFileDirective,
    RegionState, RootMetadata, SectionState,
};
use families::{
    register_intel8080_family_stack, register_mos6502_family_stack,
    register_motorola68000_family_stack, register_motorola6800_family_stack,
};
use opcore::parser::{LineAst, ParseError};
use opcore::preprocess::{PreprocessFileLoader, Preprocessor};
use opcore::tokenizer::{Span, Token};
use registry::cpu::CpuType;
pub use registry::registry::AsmRegistry;
use registry::syntax::{register_checker_none, RegisterChecker};
use serde_json::json;
pub use types::artifacts::{LabelOutputFormat, OutputFormat};
use types::source_map::SourceMap;
use types::symbol::{SymbolTable, SymbolVisibility};
use vm::builder::build_hierarchy_package_from_registry;
use vm::runtime_bootstrap;
use vm::vm_opasm::{
    build_bin_output_payload as build_bin_payload_with_vm,
    build_export_sections_payloads as build_export_sections_payloads_with_vm,
    build_hex_output_payload as build_hex_payload_with_vm,
    build_linker_output_payload as build_linker_output_payload_with_vm,
    build_mapfile_text as build_mapfile_text_with_vm,
    build_srec_output_payload as build_srec_payload_with_vm,
    parse_statement_line_with_model as parse_line_with_vm_model,
    render_dependencies as render_dependencies_with_vm, render_labels as render_labels_with_vm,
    tokenize_statement_line_with_model as tokenize_with_vm_model, HierarchyExecutionModel,
    ListingWriter,
};

pub use io::{
    FsOutputSink, FsSourceProvider, MemoryOutputSink, MemorySourceProvider, OutputSink,
    SourceProvider,
};
pub use processing::{
    editor_route_line, editor_route_line_with_model, editor_route_line_with_model_in_mode,
    process_opcore_expression_request, process_opcore_expression_request_with_mode,
    route_module_item_line, route_module_item_line_with_model, EngineError,
};
pub use source_graph::{
    load_module_graph, load_module_graph_with_provider, module_search_root_for_path,
    ModuleGraphResult,
};
pub use types::lockstep::{
    ContinuationHead, ExecutionMode, LockstepCheckpoint, LockstepComparisonCategory,
    LockstepDivergence, LockstepMatch, LockstepReport, LockstepStage,
};
pub use types::processing::{
    LineProcessingTrace, OpcoreRequestKind, ProcessingOutcome, ProcessingRequestKind,
    ProcessingReturn,
};

pub const DEFAULT_CPU: CpuType = CpuType::new("8085");
pub const DEFAULT_TOKENIZER_CPU_ID: &str = "m6502";

#[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
fn default_runtime_artifact_path_for_dir(base_dir: &Path) -> PathBuf {
    runtime_bootstrap::runtime_package_artifact_path_for_dir(base_dir)
}

struct EngineRuntimeLineRouter {
    execution_mode: ExecutionMode,
}

fn core_error_into_parse_error(err: opcore::CoreError, fallback_line: u32) -> ParseError {
    match err {
        opcore::CoreError::Parse(err) => err,
        opcore::CoreError::ModuleItem(err) => ParseError {
            message: err.message,
            span: err.span,
        },
        opcore::CoreError::LineParse(err) => ParseError {
            message: err.message,
            span: err.span,
        },
        opcore::CoreError::Tokenize(err) => ParseError {
            message: err.message,
            span: err.span,
        },
        opcore::CoreError::Expr(err) => ParseError {
            message: err.message,
            span: err.span.unwrap_or(Span {
                line: fallback_line,
                col_start: 1,
                col_end: 1,
            }),
        },
        opcore::CoreError::Macro(err) => ParseError {
            message: err.message().to_string(),
            span: Span {
                line: err.line().unwrap_or(fallback_line),
                col_start: err.column().unwrap_or(1),
                col_end: err.column().unwrap_or(1),
            },
        },
        opcore::CoreError::Preprocess(err) => ParseError {
            message: err.message().to_string(),
            span: Span {
                line: err.line().unwrap_or(fallback_line),
                col_start: err.column().unwrap_or(1),
                col_end: err.column().unwrap_or(1),
            },
        },
        other => ParseError {
            message: other.summary().to_string(),
            span: Span {
                line: fallback_line,
                col_start: 1,
                col_end: 1,
            },
        },
    }
}

impl RuntimeLineRouter for EngineRuntimeLineRouter {
    fn parse_line(
        &self,
        model: &HierarchyExecutionModel,
        cpu_id: &str,
        line: &str,
        line_num: u32,
        register_checker: &RegisterChecker,
    ) -> Result<
        (
            LineAst,
            Span,
            Option<String>,
            Option<LineProcessingTrace>,
            Option<LockstepReport>,
        ),
        ParseError,
    > {
        let (_, end_span, end_token_text) =
            tokenize_with_vm_model(model, cpu_id, None, line, line_num, register_checker)?;
        let (ast, trace, lockstep_report) = editor_route_line_with_model_in_mode(
            model,
            cpu_id,
            None,
            line,
            line_num,
            register_checker,
            self.execution_mode,
        )
        .map_err(|err| match err {
            EngineError::Core(err) => core_error_into_parse_error(err, line_num),
            EngineError::Processor(err) => ParseError {
                message: err.summary().to_string(),
                span: Span {
                    line: line_num,
                    col_start: 1,
                    col_end: 1,
                },
            },
        })?;
        Ok((
            ast,
            end_span,
            end_token_text,
            Some(trace),
            Some(lockstep_report),
        ))
    }
}

pub fn make_runtime_line_router(execution_mode: ExecutionMode) -> Rc<dyn RuntimeLineRouter> {
    Rc::new(EngineRuntimeLineRouter { execution_mode })
}

fn module_id_from_path(path: &Path) -> Result<String, AsmRunError> {
    let stem = path.file_stem().and_then(|s| s.to_str()).ok_or_else(|| {
        AsmRunError::new(
            AsmError::new(AsmErrorKind::Cli, "Invalid module filename", None),
            Vec::new(),
            Vec::new(),
        )
    })?;
    Ok(stem.to_string())
}

struct SourceProviderFileLoader<'a> {
    source_provider: &'a dyn SourceProvider,
}

impl fmt::Debug for SourceProviderFileLoader<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("SourceProviderFileLoader(..)")
    }
}

impl PreprocessFileLoader for SourceProviderFileLoader<'_> {
    fn read_to_string(&self, path: &Path) -> std::io::Result<String> {
        self.source_provider.read_string(path)
    }

    fn read_bytes(&self, path: &Path) -> std::io::Result<Vec<u8>> {
        self.source_provider.read_bytes(path)
    }

    fn is_file(&self, path: &Path) -> bool {
        self.source_provider.is_file(path).unwrap_or(false)
    }

    fn canonicalize(&self, path: &Path) -> std::io::Result<PathBuf> {
        self.source_provider.canonicalize(path)
    }
}

pub fn root_module_id_from_lines(
    root_path: &Path,
    root_lines: &[String],
) -> Result<String, AsmRunError> {
    let explicit = source_graph::scan_module_ids_from_processing(root_lines);
    if explicit.is_empty() {
        return module_id_from_path(root_path);
    }
    let implicit = module_id_from_path(root_path)?;
    if let Some(matched) = explicit
        .iter()
        .find(|module_id| module_id.eq_ignore_ascii_case(&implicit))
    {
        return Ok(matched.clone());
    }
    Ok(explicit[0].clone())
}

pub fn expand_source_file(
    path: &Path,
    defines: &[String],
    include_roots: &[std::path::PathBuf],
    pp_macro_depth: usize,
) -> Result<Vec<String>, AsmRunError> {
    let (lines, _) =
        expand_source_file_with_dependencies(path, defines, include_roots, pp_macro_depth)?;
    Ok(lines)
}

pub fn expand_source_file_with_dependencies(
    path: &Path,
    defines: &[String],
    include_roots: &[std::path::PathBuf],
    pp_macro_depth: usize,
) -> Result<(Vec<String>, Vec<std::path::PathBuf>), AsmRunError> {
    let source_provider = FsSourceProvider;
    expand_source_file_with_dependencies_with_provider(
        path,
        defines,
        include_roots,
        pp_macro_depth,
        &source_provider,
    )
}

pub fn expand_source_file_with_dependencies_with_provider(
    path: &Path,
    defines: &[String],
    include_roots: &[std::path::PathBuf],
    pp_macro_depth: usize,
    source_provider: &dyn SourceProvider,
) -> Result<(Vec<String>, Vec<std::path::PathBuf>), AsmRunError> {
    let mut pp = Preprocessor::with_max_depth(pp_macro_depth);
    if let Some(parent) = path.parent() {
        pp.add_include_root(parent.to_path_buf());
    }
    for root in include_roots {
        pp.add_include_root(root.clone());
    }
    for def in defines {
        if let Some((name, value)) = def.split_once('=') {
            pp.define(name, value);
        } else {
            pp.define(def, "1");
        }
    }
    let loader = SourceProviderFileLoader { source_provider };
    if let Err(err) = pp.process_file_with_loader(path.to_string_lossy().as_ref(), &loader) {
        let err_msg = AsmError::new(AsmErrorKind::Preprocess, err.message(), None);
        let mut diagnostics = Vec::new();
        let mut source_lines = Vec::new();
        if let (Some(line), Some(file)) = (err.line(), err.file()) {
            if let Ok(contents) = source_provider.read_string(Path::new(file)) {
                source_lines = contents.lines().map(|s| s.to_string()).collect();
            }
            let source_override = if source_lines.is_empty() {
                err.source().map(|s| s.to_string())
            } else {
                None
            };
            diagnostics.push(
                Diagnostic::new(line, Severity::Error, err_msg.clone())
                    .with_column(err.column())
                    .with_file(Some(file.to_string()))
                    .with_source(source_override),
            );
        }
        return Err(AsmRunError::new(err_msg, diagnostics, source_lines));
    }
    Ok((pp.lines().to_vec(), pp.seen_files()))
}

pub fn build_default_asm_registry() -> AsmRegistry {
    let mut registry = AsmRegistry::new();
    register_intel8080_family_stack(&mut registry);
    register_mos6502_family_stack(&mut registry);
    register_motorola6800_family_stack(&mut registry);
    register_motorola68000_family_stack(&mut registry);
    registry
}

#[doc(hidden)]
pub fn build_default_registry() -> AsmRegistry {
    build_default_asm_registry()
}

pub fn build_default_runtime_package_bytes() -> Option<Vec<u8>> {
    let registry = build_default_asm_registry();
    build_hierarchy_package_from_registry(&registry).ok()
}

#[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
fn default_runtime_model() -> Option<&'static HierarchyExecutionModel> {
    let base_dir = std::env::current_dir().ok()?;
    editor_default_runtime_model_for_dir(base_dir.as_path())
}

#[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
fn editor_default_runtime_model_for_dir(
    base_dir: &Path,
) -> Option<&'static HierarchyExecutionModel> {
    let artifact_path = default_runtime_artifact_path_for_dir(base_dir);
    default_runtime_model_for_artifact_path(artifact_path.as_path())
}

#[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RuntimeArtifactFingerprint {
    len: u64,
    modified: Option<std::time::SystemTime>,
}

#[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
impl RuntimeArtifactFingerprint {
    fn from_path(path: &Path) -> Option<Self> {
        let metadata = std::fs::metadata(path).ok()?;
        Some(Self {
            len: metadata.len(),
            modified: metadata.modified().ok(),
        })
    }
}

#[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
#[derive(Debug, Clone, Copy)]
struct CachedRuntimeModel {
    fingerprint: RuntimeArtifactFingerprint,
    model: &'static HierarchyExecutionModel,
}

#[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
fn default_runtime_model_for_artifact_path(
    artifact_path: &Path,
) -> Option<&'static HierarchyExecutionModel> {
    static MODEL_CACHE: OnceLock<Mutex<HashMap<PathBuf, CachedRuntimeModel>>> = OnceLock::new();

    let fingerprint = RuntimeArtifactFingerprint::from_path(artifact_path)?;
    let cache = MODEL_CACHE.get_or_init(|| Mutex::new(HashMap::new()));
    if let Some(cached) = cache
        .lock()
        .expect("runtime model cache lock")
        .get(artifact_path)
        .copied()
    {
        if cached.fingerprint == fingerprint {
            return Some(cached.model);
        }
    }

    let model = runtime_bootstrap::bootstrap_execution_model(Some(artifact_path), None, false)?;

    let mut cache = cache.lock().expect("runtime model cache lock");
    if let Some(cached) = cache.get(artifact_path).copied() {
        if cached.fingerprint == fingerprint {
            return Some(cached.model);
        }
    }

    let leaked: &'static HierarchyExecutionModel = Box::leak(Box::new(model));
    cache.insert(
        artifact_path.to_path_buf(),
        CachedRuntimeModel {
            fingerprint,
            model: leaked,
        },
    );
    Some(leaked)
}

#[cfg(not(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact")))]
fn default_runtime_model() -> Option<&'static HierarchyExecutionModel> {
    static MODEL: OnceLock<Option<HierarchyExecutionModel>> = OnceLock::new();
    MODEL.get_or_init(build_default_runtime_model).as_ref()
}

#[cfg_attr(
    all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"),
    allow(dead_code)
)]
fn build_default_runtime_model() -> Option<HierarchyExecutionModel> {
    #[cfg(feature = "vm-runtime-only")]
    {
        #[cfg(feature = "vm-runtime-opasm-artifact")]
        {
            if let Ok(base_dir) = std::env::current_dir() {
                if let Some(model) = build_default_runtime_model_for_dir(base_dir.as_path()) {
                    return Some(model);
                }
            }
        }
        None
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    {
        let package_bytes = build_default_runtime_package_bytes()?;
        runtime_bootstrap::bootstrap_execution_model(None, Some(package_bytes.as_slice()), false)
    }
}

#[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
#[allow(dead_code)]
fn build_default_runtime_model_for_dir(base_dir: &Path) -> Option<HierarchyExecutionModel> {
    let path = default_runtime_artifact_path_for_dir(base_dir);
    runtime_bootstrap::bootstrap_execution_model(Some(path.as_path()), None, false)
}

pub fn editor_default_runtime_model() -> Option<&'static HierarchyExecutionModel> {
    default_runtime_model()
}

pub fn editor_parse_line(line: &str, line_num: u32) -> Result<LineAst, ParseError> {
    let model = default_runtime_model().ok_or_else(|| ParseError {
        message: "VM tokenizer runtime model is unavailable".to_string(),
        span: Span {
            line: line_num,
            col_start: 1,
            col_end: 1,
        },
    })?;
    let register_checker = register_checker_none();
    let (line_ast, _, _) = parse_line_with_vm_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        line,
        line_num,
        &register_checker,
    )?;
    Ok(line_ast)
}

pub fn editor_parse_line_with_model(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> Result<(LineAst, Span, Option<String>), ParseError> {
    parse_line_with_vm_model(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
    )
}

pub fn editor_tokenize_line(line: &str, line_num: u32) -> Result<Vec<Token>, ParseError> {
    let model = default_runtime_model().ok_or_else(|| ParseError {
        message: "VM tokenizer runtime model is unavailable".to_string(),
        span: Span {
            line: line_num,
            col_start: 1,
            col_end: 1,
        },
    })?;
    let register_checker = register_checker_none();
    let (tokens, _, _) = tokenize_with_vm_model(
        model,
        DEFAULT_TOKENIZER_CPU_ID,
        None,
        line,
        line_num,
        &register_checker,
    )?;
    Ok(tokens)
}

pub fn editor_tokenize_line_with_model(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> Result<(Vec<Token>, Span, Option<String>), ParseError> {
    tokenize_with_vm_model(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
    )
}

fn resolve_artifact_output_path(path: &str, out_dir: Option<&Path>) -> Result<PathBuf, AsmError> {
    let raw_path = PathBuf::from(path);
    if raw_path.is_absolute() {
        if out_dir.is_some() {
            return Err(AsmError::new(
                AsmErrorKind::Directive,
                "Output path escapes resolved output root",
                Some(raw_path.to_string_lossy().as_ref()),
            ));
        }
        Ok(raw_path)
    } else if let Some(dir) = out_dir {
        anchor_relative_output_path(dir, &raw_path).map_err(|err| {
            AsmError::new(
                AsmErrorKind::Directive,
                &err,
                Some(raw_path.to_string_lossy().as_ref()),
            )
        })
    } else {
        Ok(raw_path)
    }
}

fn ensure_parent_dir(sink: &dyn OutputSink, path: &Path) -> Result<(), AsmError> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            if let Err(err) = sink.create_dir_all(parent) {
                let path_text = path.to_string_lossy().to_string();
                return Err(AsmError::new(
                    AsmErrorKind::Io,
                    &err.to_string(),
                    Some(&path_text),
                ));
            }
        }
    }
    Ok(())
}

pub fn emit_linker_outputs(
    outputs: &[asm::output::LinkerOutputDirective],
    sections: &HashMap<String, SectionState>,
    out_dir: Option<&Path>,
    output_sink: &dyn OutputSink,
) -> Result<(), AsmError> {
    for output in outputs {
        let payload = build_linker_output_payload_with_vm(output, sections)
            .map_err(|err| AsmError::new(AsmErrorKind::Directive, err.message(), err.subject()))?;
        let output_path = resolve_artifact_output_path(&output.path, out_dir)?;
        ensure_parent_dir(output_sink, &output_path)?;
        let mut file = match output_sink.create_file(&output_path) {
            Ok(file) => file,
            Err(err) => {
                let path_text = output_path.to_string_lossy().to_string();
                return Err(AsmError::new(
                    AsmErrorKind::Io,
                    &err.to_string(),
                    Some(&path_text),
                ));
            }
        };
        if let Err(err) = file.write_all(&payload) {
            let path_text = output_path.to_string_lossy().to_string();
            return Err(AsmError::new(
                AsmErrorKind::Io,
                &err.to_string(),
                Some(&path_text),
            ));
        }
    }
    Ok(())
}

fn synthetic_hunk_output(path: &str, assembler: &Assembler) -> LinkerOutputDirective {
    let mut output = LinkerOutputDirective {
        path: path.to_string(),
        format_id: LinkerOutputFormat::Hunk.format_id().to_string(),
        options: Default::default(),
        relocation_disposition: Default::default(),
    };
    output.relocation_disposition = assembler.hunk_output_relocation_disposition_for(&output);
    output
}

pub fn emit_export_sections(
    directives: &[ExportSectionsDirective],
    sections: &HashMap<String, SectionState>,
    out_dir: Option<&Path>,
    output_sink: &dyn OutputSink,
) -> Result<(), AsmError> {
    for directive in directives {
        let target_dir = resolve_artifact_output_path(&directive.dir, out_dir)?;
        if let Err(err) = output_sink.create_dir_all(&target_dir) {
            let dir_text = target_dir.to_string_lossy().to_string();
            return Err(AsmError::new(
                AsmErrorKind::Io,
                &err.to_string(),
                Some(&dir_text),
            ));
        }
        for (filename, payload) in build_export_sections_payloads_with_vm(directive, sections) {
            let path = target_dir.join(filename);
            let mut file = match output_sink.create_file(&path) {
                Ok(file) => file,
                Err(err) => {
                    let path_text = path.to_string_lossy().to_string();
                    return Err(AsmError::new(
                        AsmErrorKind::Io,
                        &err.to_string(),
                        Some(&path_text),
                    ));
                }
            };
            if let Err(err) = file.write_all(&payload) {
                let path_text = path.to_string_lossy().to_string();
                return Err(AsmError::new(
                    AsmErrorKind::Io,
                    &err.to_string(),
                    Some(&path_text),
                ));
            }
        }
    }
    Ok(())
}

pub fn emit_mapfiles(
    directives: &[MapFileDirective],
    regions: &HashMap<String, RegionState>,
    sections: &HashMap<String, SectionState>,
    symbols: &SymbolTable,
    out_dir: Option<&Path>,
    output_sink: &dyn OutputSink,
) -> Result<(), AsmError> {
    for directive in directives {
        let map_text = build_mapfile_text_with_vm(directive, regions, sections, symbols);
        let output_path = resolve_artifact_output_path(&directive.path, out_dir)?;
        ensure_parent_dir(output_sink, &output_path)?;
        if let Err(err) = output_sink.write_text(&output_path, &map_text) {
            let path_text = output_path.to_string_lossy().to_string();
            return Err(AsmError::new(
                AsmErrorKind::Io,
                &err.to_string(),
                Some(&path_text),
            ));
        }
    }
    Ok(())
}

pub fn emit_labels_file(
    path: &Path,
    format: LabelOutputFormat,
    output_format: OutputFormat,
    symbols: &SymbolTable,
    source_lines: std::sync::Arc<Vec<String>>,
    output_sink: &dyn OutputSink,
) -> Result<(), AsmRunError> {
    let output = render_labels_with_vm(format, output_format, symbols);
    ensure_parent_dir(output_sink, path)
        .map_err(|err| AsmRunError::new(err, Vec::new(), source_lines.clone()))?;
    output_sink.write_text(path, &output).map_err(|err| {
        AsmRunError::new(
            AsmError::new(
                AsmErrorKind::Io,
                &format!("Error writing labels file: {err}"),
                Some(path.to_string_lossy().as_ref()),
            ),
            Vec::new(),
            source_lines,
        )
    })
}

pub fn emit_dependency_file(
    policy: &DependencyOutputPolicy,
    output_format: OutputFormat,
    targets: &[String],
    dependencies: Vec<PathBuf>,
    source_lines: std::sync::Arc<Vec<String>>,
    output_sink: &dyn OutputSink,
) -> Result<(), AsmRunError> {
    let Some(body) =
        render_dependencies_with_vm(output_format, targets, &dependencies, policy.make_phony)
    else {
        return Ok(());
    };
    ensure_parent_dir(output_sink, &policy.path)
        .map_err(|err| AsmRunError::new(err, Vec::new(), source_lines.clone()))?;
    output_sink.write_text(&policy.path, &body).map_err(|err| {
        AsmRunError::new(
            AsmError::new(
                AsmErrorKind::Io,
                &format!("Error writing dependency file: {err}"),
                Some(policy.path.to_string_lossy().as_ref()),
            ),
            Vec::new(),
            source_lines,
        )
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CpuResolutionError {
    requested: String,
    known: Vec<String>,
}

impl CpuResolutionError {
    pub fn requested(&self) -> &str {
        self.requested.as_str()
    }

    pub fn known(&self) -> &[String] {
        &self.known
    }
}

impl std::fmt::Display for CpuResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Unknown CPU: {}. Known CPUs: {}",
            self.requested,
            self.known.join(", ")
        )
    }
}

impl std::error::Error for CpuResolutionError {}

pub struct AssemblerSessionConfig {
    cpu: CpuType,
    registry: AsmRegistry,
    max_loop_iterations: u32,
}

pub struct PreparedAssemblySession {
    session: AssemblerSessionConfig,
    root_module_id: String,
    expanded_lines: Vec<String>,
    source_map: SourceMap,
    dependency_files: Vec<PathBuf>,
    module_macro_names: HashMap<String, HashMap<String, SymbolVisibility>>,
}

pub struct PreparedAssemblyExecutionRequest<'a> {
    pub input_base: &'a str,
    pub cpu: CpuType,
    pub registry: Arc<Mutex<AsmRegistry>>,
    pub max_loop_iterations: u32,
    pub opasm_package_path: Option<&'a Path>,
    pub root_module_id: String,
    pub prepared_lines: Vec<String>,
    pub source_map: SourceMap,
    pub dependency_files: Vec<PathBuf>,
    pub module_macro_names: HashMap<String, HashMap<String, SymbolVisibility>>,
    pub out_dir: Option<&'a Path>,
    pub debug_conditionals: bool,
    pub tab_size: Option<usize>,
    pub output_format: OutputFormat,
    pub go_addr: Option<&'a str>,
    pub bin_specs: &'a [BinOutputSpec],
    pub fill_byte: u8,
    pub fill_byte_set: bool,
    pub default_outputs: bool,
    pub labels_file: Option<&'a Path>,
    pub label_output_format: LabelOutputFormat,
    pub dependency_output: Option<&'a DependencyOutputPolicy>,
    pub outfile_override: Option<&'a str>,
    pub list_name_override: Option<&'a str>,
    pub hex_name_override: Option<&'a str>,
    pub srec_name_override: Option<&'a str>,
    pub hunk_name_override: Option<&'a str>,
    pub header_title: &'a str,
    pub output_sink: Option<&'a dyn OutputSink>,
    pub execution_mode: ExecutionMode,
    pub suppress_outputs: bool,
}

struct PreparedAssemblyRuntime {
    cpu: CpuType,
    registry: AsmRegistry,
    max_loop_iterations: u32,
    opasm_package_path: Option<PathBuf>,
    root_module_id: String,
    expanded_lines: Arc<Vec<String>>,
    source_map: SourceMap,
    dependency_files: Vec<PathBuf>,
    module_macro_names: HashMap<String, HashMap<String, SymbolVisibility>>,
}

struct AssemblerExecutionGuard<'a> {
    assembler: Option<Assembler>,
    registry_slot: Option<&'a mut AsmRegistry>,
}

impl<'a> AssemblerExecutionGuard<'a> {
    fn new(assembler: Assembler, registry_slot: Option<&'a mut AsmRegistry>) -> Self {
        Self {
            assembler: Some(assembler),
            registry_slot,
        }
    }
}

impl Deref for AssemblerExecutionGuard<'_> {
    type Target = Assembler;

    fn deref(&self) -> &Self::Target {
        self.assembler
            .as_ref()
            .expect("assembler guard should hold assembler")
    }
}

impl DerefMut for AssemblerExecutionGuard<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.assembler
            .as_mut()
            .expect("assembler guard should hold assembler")
    }
}

impl Drop for AssemblerExecutionGuard<'_> {
    fn drop(&mut self) {
        if let (Some(registry_slot), Some(assembler)) =
            (self.registry_slot.as_deref_mut(), self.assembler.take())
        {
            *registry_slot = assembler.registry;
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedBinOutput {
    pub path: String,
    pub range: Option<BinRange>,
}

pub struct OutputPlanningRequest<'a> {
    pub input_base: &'a str,
    pub source_lines: &'a [String],
    pub out_dir: Option<&'a Path>,
    pub metadata: &'a RootMetadata,
    pub cpu_name: &'a str,
    pub outfile_override: Option<&'a str>,
    pub list_name_override: Option<&'a str>,
    pub hex_name_override: Option<&'a str>,
    pub srec_name_override: Option<&'a str>,
    pub hunk_name_override: Option<&'a str>,
    pub bin_specs_override: &'a [BinOutputSpec],
    pub fill_byte: u8,
    pub fill_byte_set: bool,
    pub default_outputs: bool,
    pub go_addr: Option<&'a str>,
    pub pass1_errors: u32,
    pub suppress_outputs: bool,
}

pub struct ResolvedOutputPlan {
    out_base: String,
    list_path: Option<String>,
    hex_path: Option<String>,
    srec_path: Option<String>,
    hunk_path: Option<String>,
    effective_bin_specs: Vec<BinOutputSpec>,
    effective_fill_byte: u8,
}

pub struct FormatterPathResolutionRequest<'a> {
    pub root_path: &'a Path,
    pub asm_exts: &'a [String],
    pub inc_exts: &'a [String],
    pub defines: &'a [String],
    pub include_paths: &'a [PathBuf],
    pub module_paths: &'a [PathBuf],
    pub pp_macro_depth: usize,
}

pub type PreparedAssemblySessionParts = (
    AssemblerSessionConfig,
    String,
    Vec<String>,
    SourceMap,
    Vec<PathBuf>,
    HashMap<String, HashMap<String, SymbolVisibility>>,
);

pub struct AssemblyPreparationRequest<'a> {
    pub root_path: &'a Path,
    pub defines: &'a [String],
    pub include_roots: &'a [PathBuf],
    pub module_paths: &'a [PathBuf],
    pub pp_macro_depth: usize,
    pub registry: AsmRegistry,
    pub cpu_override: Option<&'a str>,
    pub default_cpu: CpuType,
    pub max_loop_iterations: u32,
    pub source_provider: Option<&'a dyn SourceProvider>,
}

pub struct AssemblyExecutionRequest<'a> {
    pub root_path: &'a Path,
    pub input_base: &'a str,
    pub defines: &'a [String],
    pub include_paths: &'a [PathBuf],
    pub module_paths: &'a [PathBuf],
    pub pp_macro_depth: usize,
    pub cpu_override: Option<&'a str>,
    pub default_cpu: CpuType,
    pub max_loop_iterations: u32,
    pub opasm_package_path: Option<&'a Path>,
    pub out_dir: Option<&'a Path>,
    pub debug_conditionals: bool,
    pub tab_size: Option<usize>,
    pub output_format: OutputFormat,
    pub go_addr: Option<&'a str>,
    pub bin_specs: &'a [BinOutputSpec],
    pub fill_byte: u8,
    pub fill_byte_set: bool,
    pub default_outputs: bool,
    pub labels_file: Option<&'a Path>,
    pub label_output_format: LabelOutputFormat,
    pub dependency_output: Option<&'a DependencyOutputPolicy>,
    pub outfile_override: Option<&'a str>,
    pub list_name_override: Option<&'a str>,
    pub hex_name_override: Option<&'a str>,
    pub srec_name_override: Option<&'a str>,
    pub hunk_name_override: Option<&'a str>,
    pub header_title: &'a str,
    pub output_sink: Option<&'a dyn OutputSink>,
    pub source_provider: Option<&'a dyn SourceProvider>,
    pub execution_mode: ExecutionMode,
    pub suppress_outputs: bool,
}

pub fn default_cpu() -> CpuType {
    DEFAULT_CPU
}

pub struct AsmRegistryContext {
    registry: AsmRegistry,
    snapshot: CapabilitySnapshot,
}

impl AsmRegistryContext {
    pub fn new(registry: AsmRegistry) -> Self {
        let snapshot = CapabilitySnapshot::from_registry(&registry);
        Self { registry, snapshot }
    }

    pub fn registry(&self) -> &AsmRegistry {
        &self.registry
    }

    pub fn snapshot(&self) -> &CapabilitySnapshot {
        &self.snapshot
    }

    pub fn rebuild_snapshot(&mut self) {
        self.snapshot = CapabilitySnapshot::from_registry(&self.registry);
    }
}

#[doc(hidden)]
pub type AssemblyRegistryContext = AsmRegistryContext;

impl AssemblerSessionConfig {
    pub fn resolve(
        registry: AsmRegistry,
        cpu_override: Option<&str>,
        default_cpu: CpuType,
        max_loop_iterations: u32,
    ) -> Result<Self, CpuResolutionError> {
        let cpu = resolve_target_cpu(&registry, cpu_override, default_cpu)?;
        Ok(Self {
            cpu,
            registry,
            max_loop_iterations,
        })
    }

    pub fn cpu(&self) -> CpuType {
        self.cpu
    }

    pub fn max_loop_iterations(&self) -> u32 {
        self.max_loop_iterations
    }

    pub fn into_parts(self) -> (CpuType, AsmRegistry, u32) {
        (self.cpu, self.registry, self.max_loop_iterations)
    }
}

impl PreparedAssemblySession {
    pub fn root_module_id(&self) -> &str {
        self.root_module_id.as_str()
    }

    pub fn expanded_lines(&self) -> &[String] {
        &self.expanded_lines
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    pub fn dependency_files(&self) -> &[PathBuf] {
        &self.dependency_files
    }

    pub fn module_macro_names(&self) -> &HashMap<String, HashMap<String, SymbolVisibility>> {
        &self.module_macro_names
    }

    pub fn into_parts(self) -> PreparedAssemblySessionParts {
        (
            self.session,
            self.root_module_id,
            self.expanded_lines,
            self.source_map,
            self.dependency_files,
            self.module_macro_names,
        )
    }
}

impl ResolvedOutputPlan {
    pub fn out_base(&self) -> &str {
        self.out_base.as_str()
    }

    pub fn list_path(&self) -> Option<&str> {
        self.list_path.as_deref()
    }

    pub fn hex_path(&self) -> Option<&str> {
        self.hex_path.as_deref()
    }

    pub fn srec_path(&self) -> Option<&str> {
        self.srec_path.as_deref()
    }

    pub fn hunk_path(&self) -> Option<&str> {
        self.hunk_path.as_deref()
    }

    pub fn effective_fill_byte(&self) -> u8 {
        self.effective_fill_byte
    }

    pub fn dependency_targets(&self) -> Vec<String> {
        let mut targets = Vec::new();
        if let Some(path) = &self.list_path {
            targets.push(path.clone());
        }
        if let Some(path) = &self.hex_path {
            targets.push(path.clone());
        }
        if let Some(path) = &self.srec_path {
            targets.push(path.clone());
        }
        if let Some(path) = &self.hunk_path {
            targets.push(path.clone());
        }
        targets
    }

    pub fn resolve_bin_outputs(
        &self,
        auto_output_range: Option<(u32, u32)>,
    ) -> Result<Vec<ResolvedBinOutput>, String> {
        let bin_count = self.effective_bin_specs.len();
        self.effective_bin_specs
            .iter()
            .enumerate()
            .map(|(index, spec)| {
                let range = spec.range.clone().or_else(|| {
                    auto_output_range.map(|(start, end)| BinRange {
                        start_str: format_addr(start),
                        start,
                        end,
                    })
                });
                let path = resolve_bin_path_checked(
                    &self.out_base,
                    spec.name.as_deref(),
                    range.as_ref(),
                    bin_count,
                    index,
                )?;
                Ok(ResolvedBinOutput { path, range })
            })
            .collect()
    }
}

fn linker_output_targets(
    outputs: &[asm::output::LinkerOutputDirective],
    out_dir: Option<&Path>,
) -> Result<Vec<String>, AsmError> {
    outputs
        .iter()
        .map(|output| {
            resolve_artifact_output_path(&output.path, out_dir)
                .map(|path| path.to_string_lossy().to_string())
        })
        .collect()
}

fn export_sections_targets(
    directives: &[ExportSectionsDirective],
    sections: &HashMap<String, SectionState>,
    out_dir: Option<&Path>,
) -> Result<Vec<String>, AsmError> {
    let mut targets = Vec::new();
    for directive in directives {
        let target_dir = resolve_artifact_output_path(&directive.dir, out_dir)?;
        for (filename, _) in build_export_sections_payloads_with_vm(directive, sections) {
            targets.push(target_dir.join(filename).to_string_lossy().to_string());
        }
    }
    Ok(targets)
}

fn mapfile_targets(
    directives: &[MapFileDirective],
    out_dir: Option<&Path>,
) -> Result<Vec<String>, AsmError> {
    directives
        .iter()
        .map(|directive| {
            resolve_artifact_output_path(&directive.path, out_dir)
                .map(|path| path.to_string_lossy().to_string())
        })
        .collect()
}

pub fn prepare_assembly_session(
    request: AssemblyPreparationRequest<'_>,
) -> Result<PreparedAssemblySession, AsmRunError> {
    let fs_source_provider = FsSourceProvider;
    let source_provider: &dyn SourceProvider =
        request.source_provider.unwrap_or(&fs_source_provider);
    let (root_lines, root_dependency_files) = expand_source_file_with_dependencies_with_provider(
        request.root_path,
        request.defines,
        request.include_roots,
        request.pp_macro_depth,
        source_provider,
    )?;
    let root_module_id = root_module_id_from_lines(request.root_path, &root_lines)?;
    let graph = load_module_graph_with_provider(
        request.root_path,
        root_lines,
        request.defines,
        request.include_roots,
        request.module_paths,
        request.pp_macro_depth,
        source_provider,
    )?;
    let session = AssemblerSessionConfig::resolve(
        request.registry,
        request.cpu_override,
        request.default_cpu,
        request.max_loop_iterations,
    )
    .map_err(|err| {
        AsmRunError::new(
            AsmError::new(AsmErrorKind::Cli, &err.to_string(), None),
            Vec::new(),
            graph.lines.clone(),
        )
    })?;

    let mut dependency_files: HashSet<PathBuf> = root_dependency_files.into_iter().collect();
    for path in graph.dependency_files {
        dependency_files.insert(path);
    }
    let mut dependency_files: Vec<PathBuf> = dependency_files.into_iter().collect();
    dependency_files.sort();

    Ok(PreparedAssemblySession {
        session,
        root_module_id,
        expanded_lines: graph.lines,
        source_map: graph.source_map,
        dependency_files,
        module_macro_names: graph.module_macro_names,
    })
}

pub fn effective_include_paths_for_root(
    root_path: &Path,
    include_paths: &[PathBuf],
) -> Vec<PathBuf> {
    let mut effective = Vec::new();
    if let Some(parent) = root_path.parent() {
        effective.push(parent.to_path_buf());
    }
    effective.extend_from_slice(include_paths);
    effective
}

fn is_formatter_source_path(path: &Path, asm_exts: &[String], inc_exts: &[String]) -> bool {
    let ext = path.extension().and_then(|s| s.to_str()).unwrap_or("");
    asm_exts
        .iter()
        .chain(inc_exts.iter())
        .any(|allowed| allowed.eq_ignore_ascii_case(ext))
}

pub fn resolve_formatter_module_paths(
    request: FormatterPathResolutionRequest<'_>,
) -> Result<Vec<PathBuf>, AsmRunError> {
    let effective_include_paths =
        effective_include_paths_for_root(request.root_path, request.include_paths);
    let (root_lines, root_dependency_files) = expand_source_file_with_dependencies(
        request.root_path,
        request.defines,
        &effective_include_paths,
        request.pp_macro_depth,
    )?;
    let graph = load_module_graph(
        request.root_path,
        root_lines,
        request.defines,
        &effective_include_paths,
        request.module_paths,
        request.pp_macro_depth,
    )?;

    let mut files = HashSet::new();
    for path in root_dependency_files {
        if is_formatter_source_path(&path, request.asm_exts, request.inc_exts) {
            files.insert(path.canonicalize().unwrap_or(path));
        }
    }
    for path in graph.dependency_files {
        if is_formatter_source_path(&path, request.asm_exts, request.inc_exts) {
            files.insert(path.canonicalize().unwrap_or(path));
        }
    }
    files.insert(
        request
            .root_path
            .canonicalize()
            .unwrap_or_else(|_| request.root_path.to_path_buf()),
    );

    let mut sorted: Vec<PathBuf> = files.into_iter().collect();
    sorted.sort();
    Ok(sorted)
}

pub fn resolve_output_plan(
    request: OutputPlanningRequest<'_>,
) -> Result<ResolvedOutputPlan, AsmRunError> {
    if request.suppress_outputs {
        return Ok(ResolvedOutputPlan {
            out_base: request.input_base.to_string(),
            list_path: None,
            hex_path: None,
            srec_path: None,
            hunk_path: None,
            effective_bin_specs: Vec::new(),
            effective_fill_byte: request.fill_byte,
        });
    }

    let output_config = request.metadata.output_config_for_cpu(request.cpu_name);
    let metadata_output = output_config.name.as_deref();
    let meta_outputs_requested = output_config.list_name.is_some()
        || output_config.hex_name.is_some()
        || !output_config.bin_specs.is_empty();
    let effective_default_outputs = request.default_outputs && !meta_outputs_requested;
    let base_is_resolved = metadata_output.is_some()
        || request.outfile_override.is_some()
        || request.out_dir.is_some()
        || !request.input_base.is_empty();
    if request.pass1_errors == 0 && effective_default_outputs && !base_is_resolved {
        return Err(AsmRunError::new(
            AsmError::new(
                AsmErrorKind::Cli,
                "No outputs selected. Provide .meta.output.name (or -o) or specify output flags",
                None,
            ),
            Vec::new(),
            request.source_lines.to_vec(),
        ));
    }

    let out_base = resolve_output_base(
        request.input_base,
        request.out_dir,
        request.metadata,
        request.cpu_name,
        request.outfile_override,
    );
    let list_path = match request.list_name_override {
        Some(name) => resolve_output_path_checked(&out_base, Some(name.to_string()), "lst"),
        None if effective_default_outputs => {
            resolve_output_path_checked(&out_base, Some(String::new()), "lst")
        }
        None => resolve_output_path_checked(&out_base, output_config.list_name.clone(), "lst"),
    }
    .map_err(|err| {
        AsmRunError::new(
            AsmError::new(AsmErrorKind::Cli, &err, None),
            Vec::new(),
            request.source_lines.to_vec(),
        )
    })?;
    let hex_path = match request.hex_name_override {
        Some(name) => resolve_output_path_checked(&out_base, Some(name.to_string()), "hex"),
        None if effective_default_outputs => {
            resolve_output_path_checked(&out_base, Some(String::new()), "hex")
        }
        None => resolve_output_path_checked(&out_base, output_config.hex_name.clone(), "hex"),
    }
    .map_err(|err| {
        AsmRunError::new(
            AsmError::new(AsmErrorKind::Cli, &err, None),
            Vec::new(),
            request.source_lines.to_vec(),
        )
    })?;
    let srec_path = request
        .srec_name_override
        .map(|name| resolve_output_path_checked(&out_base, Some(name.to_string()), "srec"))
        .transpose()
        .map_err(|err| {
            AsmRunError::new(
                AsmError::new(AsmErrorKind::Cli, &err, None),
                Vec::new(),
                request.source_lines.to_vec(),
            )
        })?
        .flatten();
    let hunk_path = request
        .hunk_name_override
        .map(|name| resolve_output_path_checked(&out_base, Some(name.to_string()), "hunk"))
        .transpose()
        .map_err(|err| {
            AsmRunError::new(
                AsmError::new(AsmErrorKind::Cli, &err, None),
                Vec::new(),
                request.source_lines.to_vec(),
            )
        })?
        .flatten();
    if request.pass1_errors == 0
        && request.go_addr.is_some()
        && hex_path.is_none()
        && srec_path.is_none()
    {
        return Err(AsmRunError::new(
            AsmError::new(
                AsmErrorKind::Cli,
                "-g/--go requires hex or S-record output (-x/--hex, -s/--srec, or output metadata)",
                None,
            ),
            Vec::new(),
            request.source_lines.to_vec(),
        ));
    }

    let effective_bin_specs = if request.bin_specs_override.is_empty() {
        output_config.bin_specs
    } else {
        request.bin_specs_override.to_vec()
    };
    if request.fill_byte_set && effective_bin_specs.is_empty() {
        return Err(AsmRunError::new(
            AsmError::new(
                AsmErrorKind::Cli,
                "-f/--fill requires binary output (-b/--bin or output metadata)",
                None,
            ),
            Vec::new(),
            request.source_lines.to_vec(),
        ));
    }
    let effective_fill_byte = if request.fill_byte_set {
        request.fill_byte
    } else {
        output_config.fill_byte.unwrap_or(request.fill_byte)
    };

    Ok(ResolvedOutputPlan {
        out_base,
        list_path,
        hex_path,
        srec_path,
        hunk_path,
        effective_bin_specs,
        effective_fill_byte,
    })
}

pub fn run_assembly(request: AssemblyExecutionRequest<'_>) -> Result<AsmRunReport, AsmRunError> {
    let fs_output_sink = FsOutputSink;
    let output_sink: &dyn OutputSink = request.output_sink.unwrap_or(&fs_output_sink);
    let effective_include_paths =
        effective_include_paths_for_root(request.root_path, request.include_paths);
    let prepared = prepare_assembly_session(AssemblyPreparationRequest {
        root_path: request.root_path,
        defines: request.defines,
        include_roots: &effective_include_paths,
        module_paths: request.module_paths,
        pp_macro_depth: request.pp_macro_depth,
        registry: build_default_asm_registry(),
        cpu_override: request.cpu_override,
        default_cpu: request.default_cpu,
        max_loop_iterations: request.max_loop_iterations,
        source_provider: request.source_provider,
    })?;
    let (session, root_module_id, prepared_lines, source_map, dependency_files, module_macro_names) =
        prepared.into_parts();
    let expanded_lines = Arc::new(prepared_lines);
    let (cpu, registry, max_loop_iterations) = session.into_parts();
    run_assembly_with_prepared(
        PreparedAssemblyRuntime {
            cpu,
            registry,
            max_loop_iterations,
            opasm_package_path: request.opasm_package_path.map(Path::to_path_buf),
            root_module_id,
            expanded_lines,
            source_map,
            dependency_files,
            module_macro_names,
        },
        output_sink,
        request,
        None,
    )
}

pub fn run_prepared_assembly(
    request: PreparedAssemblyExecutionRequest<'_>,
) -> Result<AsmRunReport, AsmRunError> {
    let fs_output_sink = FsOutputSink;
    let output_sink: &dyn OutputSink = request.output_sink.unwrap_or(&fs_output_sink);
    let mut registry_guard = request.registry.lock().expect("prepared registry lock");
    let prepared_registry = std::mem::replace(&mut *registry_guard, AsmRegistry::new());
    run_assembly_with_prepared(
        PreparedAssemblyRuntime {
            cpu: request.cpu,
            registry: prepared_registry,
            max_loop_iterations: request.max_loop_iterations,
            opasm_package_path: request.opasm_package_path.map(Path::to_path_buf),
            root_module_id: request.root_module_id,
            expanded_lines: Arc::new(request.prepared_lines),
            source_map: request.source_map,
            dependency_files: request.dependency_files,
            module_macro_names: request.module_macro_names,
        },
        output_sink,
        AssemblyExecutionRequest {
            root_path: Path::new(request.input_base),
            input_base: request.input_base,
            defines: &[],
            include_paths: &[],
            module_paths: &[],
            pp_macro_depth: 0,
            cpu_override: None,
            default_cpu: request.cpu,
            max_loop_iterations: request.max_loop_iterations,
            opasm_package_path: request.opasm_package_path,
            out_dir: request.out_dir,
            debug_conditionals: request.debug_conditionals,
            tab_size: request.tab_size,
            output_format: request.output_format,
            go_addr: request.go_addr,
            bin_specs: request.bin_specs,
            fill_byte: request.fill_byte,
            fill_byte_set: request.fill_byte_set,
            default_outputs: request.default_outputs,
            labels_file: request.labels_file,
            label_output_format: request.label_output_format,
            dependency_output: request.dependency_output,
            outfile_override: request.outfile_override,
            list_name_override: request.list_name_override,
            hex_name_override: request.hex_name_override,
            srec_name_override: request.srec_name_override,
            hunk_name_override: request.hunk_name_override,
            header_title: request.header_title,
            output_sink: request.output_sink,
            source_provider: None,
            execution_mode: request.execution_mode,
            suppress_outputs: request.suppress_outputs,
        },
        Some(&mut *registry_guard),
    )
}

fn run_assembly_with_prepared(
    runtime: PreparedAssemblyRuntime,
    output_sink: &dyn OutputSink,
    request: AssemblyExecutionRequest<'_>,
    registry_slot: Option<&mut AsmRegistry>,
) -> Result<AsmRunReport, AsmRunError> {
    let PreparedAssemblyRuntime {
        cpu,
        registry,
        max_loop_iterations,
        opasm_package_path,
        root_module_id,
        expanded_lines,
        source_map,
        dependency_files,
        module_macro_names,
    } = runtime;
    let mut assembler = AssemblerExecutionGuard::new(
        Assembler::with_cpu_and_registry(cpu, registry),
        registry_slot,
    );
    assembler.set_runtime_line_router(Some(make_runtime_line_router(request.execution_mode)));
    assembler.max_loop_iterations = max_loop_iterations;
    assembler.opasm_package_path = opasm_package_path;
    assembler.root_metadata.root_module_id = Some(root_module_id);
    assembler.module_macro_names = module_macro_names;
    assembler.set_implicit_hunk_output_requested(request.hunk_name_override.is_some());
    let remap_diags = |mut diagnostics: Vec<Diagnostic>| {
        remap_diagnostics_with_source_map(&mut diagnostics, &source_map);
        diagnostics
    };

    assembler.clear_diagnostics();
    let pass1 = assembler.pass1(&expanded_lines);
    let output_plan = resolve_output_plan(OutputPlanningRequest {
        input_base: request.input_base,
        source_lines: &expanded_lines,
        out_dir: request.out_dir,
        metadata: &assembler.root_metadata,
        cpu_name: assembler.cpu().as_str(),
        outfile_override: request.outfile_override,
        list_name_override: request.list_name_override,
        hex_name_override: request.hex_name_override,
        srec_name_override: request.srec_name_override,
        hunk_name_override: request.hunk_name_override,
        bin_specs_override: request.bin_specs,
        fill_byte: request.fill_byte,
        fill_byte_set: request.fill_byte_set,
        default_outputs: request.default_outputs,
        go_addr: request.go_addr,
        pass1_errors: pass1.errors,
        suppress_outputs: request.suppress_outputs,
    })?;
    let mut dependency_targets = output_plan.dependency_targets();

    let mut list_output: Box<dyn Write> = if let Some(path) = output_plan.list_path() {
        ensure_parent_dir(output_sink, Path::new(path))
            .map_err(|err| AsmRunError::new(err, Vec::new(), Vec::new()))?;
        output_sink.create_file(Path::new(path)).map_err(|_| {
            AsmRunError::new(
                AsmError::new(AsmErrorKind::Io, "Error opening file for write", Some(path)),
                Vec::new(),
                Vec::new(),
            )
        })?
    } else {
        Box::new(std::io::sink())
    };
    let mut listing = ListingWriter::new_with_options(
        &mut *list_output,
        request.debug_conditionals,
        request.tab_size,
    );
    if let Err(err) = listing.header(request.header_title) {
        let traces = assembler.runtime_processing_traces().to_vec();
        return Err(AsmRunError::new_with_traces(
            AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
            assembler.take_diagnostics(),
            expanded_lines.clone(),
            traces,
        ));
    }
    let pass2 = match assembler.pass2(&expanded_lines, &mut listing) {
        Ok(counts) => counts,
        Err(err) => {
            let traces = assembler.runtime_processing_traces().to_vec();
            return Err(AsmRunError::new_with_traces(
                AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            ));
        }
    };
    let generated_output = assembler.image().entries().map_err(|err| {
        let traces = assembler.runtime_processing_traces().to_vec();
        AsmRunError::new_with_traces(
            AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
            remap_diags(assembler.take_diagnostics()),
            expanded_lines.clone(),
            traces,
        )
    })?;
    if let Err(err) = listing.footer_with_generated_output(
        &pass2,
        assembler.symbols(),
        assembler.image().num_entries(),
        &generated_output,
    ) {
        let traces = assembler.runtime_processing_traces().to_vec();
        return Err(AsmRunError::new_with_traces(
            AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
            remap_diags(assembler.take_diagnostics()),
            expanded_lines.clone(),
            traces,
        ));
    }

    let had_source_errors = pass1.errors > 0 || pass2.errors > 0;

    if had_source_errors {
        let traces = assembler.runtime_processing_traces().to_vec();
        return Err(AsmRunError::new_with_traces(
            AsmError::new(AsmErrorKind::Assembler, "Errors detected in source.", None),
            remap_diags(assembler.take_diagnostics()),
            expanded_lines.clone(),
            traces,
        ));
    }

    if let Some(hex_path) = output_plan.hex_path() {
        ensure_parent_dir(output_sink, Path::new(hex_path)).map_err(|err| {
            let traces = assembler.runtime_processing_traces().to_vec();
            AsmRunError::new_with_traces(
                err,
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            )
        })?;
        let mut hex_file = output_sink.create_file(Path::new(hex_path)).map_err(|_| {
            let traces = assembler.runtime_processing_traces().to_vec();
            AsmRunError::new_with_traces(
                AsmError::new(
                    AsmErrorKind::Io,
                    "Error opening file for write",
                    Some(hex_path),
                ),
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            )
        })?;
        let payload =
            build_hex_payload_with_vm(assembler.image(), request.go_addr).map_err(|err| {
                let traces = assembler.runtime_processing_traces().to_vec();
                AsmRunError::new_with_traces(
                    AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
                    remap_diags(assembler.take_diagnostics()),
                    expanded_lines.clone(),
                    traces,
                )
            })?;
        if let Err(err) = hex_file.write_all(&payload) {
            let traces = assembler.runtime_processing_traces().to_vec();
            return Err(AsmRunError::new_with_traces(
                AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            ));
        }
    }

    if let Some(srec_path) = output_plan.srec_path() {
        ensure_parent_dir(output_sink, Path::new(srec_path)).map_err(|err| {
            let traces = assembler.runtime_processing_traces().to_vec();
            AsmRunError::new_with_traces(
                err,
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            )
        })?;
        let mut srec_file = output_sink.create_file(Path::new(srec_path)).map_err(|_| {
            let traces = assembler.runtime_processing_traces().to_vec();
            AsmRunError::new_with_traces(
                AsmError::new(
                    AsmErrorKind::Io,
                    "Error opening file for write",
                    Some(srec_path),
                ),
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            )
        })?;
        let payload =
            build_srec_payload_with_vm(assembler.image(), request.go_addr).map_err(|err| {
                let traces = assembler.runtime_processing_traces().to_vec();
                AsmRunError::new_with_traces(
                    AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
                    remap_diags(assembler.take_diagnostics()),
                    expanded_lines.clone(),
                    traces,
                )
            })?;
        if let Err(err) = srec_file.write_all(&payload) {
            let traces = assembler.runtime_processing_traces().to_vec();
            return Err(AsmRunError::new_with_traces(
                AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            ));
        }
    }

    let auto_output_range = assembler.image().output_range().map_err(|err| {
        let traces = assembler.runtime_processing_traces().to_vec();
        AsmRunError::new_with_traces(
            AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
            remap_diags(assembler.take_diagnostics()),
            expanded_lines.clone(),
            traces,
        )
    })?;
    for bin_output in output_plan
        .resolve_bin_outputs(auto_output_range)
        .map_err(|err| {
            let traces = assembler.runtime_processing_traces().to_vec();
            AsmRunError::new_with_traces(
                AsmError::new(AsmErrorKind::Cli, &err, None),
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            )
        })?
    {
        dependency_targets.push(bin_output.path.clone());
        ensure_parent_dir(output_sink, Path::new(&bin_output.path)).map_err(|err| {
            let traces = assembler.runtime_processing_traces().to_vec();
            AsmRunError::new_with_traces(
                err,
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            )
        })?;
        let mut bin_file = match output_sink.create_file(Path::new(&bin_output.path)) {
            Ok(file) => file,
            Err(_) => {
                let traces = assembler.runtime_processing_traces().to_vec();
                return Err(AsmRunError::new_with_traces(
                    AsmError::new(
                        AsmErrorKind::Io,
                        "Error opening file for write",
                        Some(&bin_output.path),
                    ),
                    remap_diags(assembler.take_diagnostics()),
                    expanded_lines.clone(),
                    traces,
                ));
            }
        };
        if let Some(range) = bin_output.range {
            let payload = build_bin_payload_with_vm(
                assembler.image(),
                range.start,
                range.end,
                output_plan.effective_fill_byte(),
            )
            .map_err(|err| {
                let traces = assembler.runtime_processing_traces().to_vec();
                AsmRunError::new_with_traces(
                    AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
                    remap_diags(assembler.take_diagnostics()),
                    expanded_lines.clone(),
                    traces,
                )
            })?;
            if let Err(err) = bin_file.write_all(&payload) {
                let traces = assembler.runtime_processing_traces().to_vec();
                return Err(AsmRunError::new_with_traces(
                    AsmError::new(AsmErrorKind::Io, &err.to_string(), None),
                    remap_diags(assembler.take_diagnostics()),
                    expanded_lines.clone(),
                    traces,
                ));
            }
        }
    }

    if let Some(path) = request.labels_file {
        dependency_targets.push(path.to_string_lossy().to_string());
    }
    dependency_targets.extend(
        linker_output_targets(&assembler.root_metadata.linker_outputs, request.out_dir).map_err(
            |err| {
                let traces = assembler.runtime_processing_traces().to_vec();
                AsmRunError::new_with_traces(
                    err,
                    remap_diags(assembler.take_diagnostics()),
                    expanded_lines.clone(),
                    traces,
                )
            },
        )?,
    );
    dependency_targets.extend(
        export_sections_targets(
            &assembler.root_metadata.export_sections,
            assembler.sections(),
            request.out_dir,
        )
        .map_err(|err| {
            let traces = assembler.runtime_processing_traces().to_vec();
            AsmRunError::new_with_traces(
                err,
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            )
        })?,
    );
    dependency_targets.extend(
        mapfile_targets(&assembler.root_metadata.mapfiles, request.out_dir).map_err(|err| {
            let traces = assembler.runtime_processing_traces().to_vec();
            AsmRunError::new_with_traces(
                err,
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            )
        })?,
    );

    if !request.suppress_outputs {
        if let Err(err) = emit_linker_outputs(
            &assembler.root_metadata.linker_outputs,
            assembler.sections(),
            request.out_dir,
            output_sink,
        ) {
            let traces = assembler.runtime_processing_traces().to_vec();
            return Err(AsmRunError::new_with_traces(
                err,
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            ));
        }
        if let Some(hunk_path) = output_plan.hunk_path() {
            let output = synthetic_hunk_output(hunk_path, &assembler);
            if let Err(err) = emit_linker_outputs(
                std::slice::from_ref(&output),
                assembler.sections(),
                None,
                output_sink,
            ) {
                let traces = assembler.runtime_processing_traces().to_vec();
                return Err(AsmRunError::new_with_traces(
                    err,
                    remap_diags(assembler.take_diagnostics()),
                    expanded_lines.clone(),
                    traces,
                ));
            }
        }
        if let Err(err) = emit_export_sections(
            &assembler.root_metadata.export_sections,
            assembler.sections(),
            request.out_dir,
            output_sink,
        ) {
            let traces = assembler.runtime_processing_traces().to_vec();
            return Err(AsmRunError::new_with_traces(
                err,
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            ));
        }
        if let Err(err) = emit_mapfiles(
            &assembler.root_metadata.mapfiles,
            assembler.regions(),
            assembler.sections(),
            assembler.symbols(),
            request.out_dir,
            output_sink,
        ) {
            let traces = assembler.runtime_processing_traces().to_vec();
            return Err(AsmRunError::new_with_traces(
                err,
                remap_diags(assembler.take_diagnostics()),
                expanded_lines.clone(),
                traces,
            ));
        }

        if let Some(path) = request.labels_file {
            emit_labels_file(
                path,
                request.label_output_format,
                request.output_format,
                assembler.symbols(),
                expanded_lines.clone(),
                output_sink,
            )?;
        }

        if let Some(policy) = request.dependency_output {
            emit_dependency_file(
                policy,
                request.output_format,
                &dependency_targets,
                dependency_files,
                expanded_lines.clone(),
                output_sink,
            )?;
        }
    }

    Ok(AsmRunReport::new(
        remap_diags(assembler.take_diagnostics()),
        expanded_lines,
        assembler.runtime_processing_traces().to_vec(),
    )
    .with_lockstep_report(assembler.runtime_lockstep_report().clone()))
}

pub fn remap_diagnostics_with_source_map(diagnostics: &mut [Diagnostic], source_map: &SourceMap) {
    for diagnostic in diagnostics {
        remap_primary_diagnostic_span(diagnostic, source_map);
        for span in &mut diagnostic.related_spans {
            remap_span_line_file(&mut span.file, &mut span.line, source_map);
        }
        for fixit in &mut diagnostic.fixits {
            remap_span_line_file(&mut fixit.file, &mut fixit.line, source_map);
        }
    }
}

pub fn warnings_as_errors(reports: &[AsmRunReport]) -> Option<AsmRunError> {
    let mut warning_diags = Vec::new();
    let mut source_lines = Vec::new();
    for report in reports {
        if source_lines.is_empty() {
            source_lines = report.source_lines().to_vec();
        }
        for diag in report.diagnostics() {
            if diag.severity == Severity::Warning {
                let mut warning = diag.clone();
                warning.severity = Severity::Error;
                warning_diags.push(warning);
            }
        }
    }
    if warning_diags.is_empty() {
        return None;
    }
    Some(AsmRunError::new(
        AsmError::new(
            AsmErrorKind::Assembler,
            "Warnings treated as errors (-Werror)",
            None,
        ),
        warning_diags,
        source_lines,
    ))
}

fn remap_primary_diagnostic_span(diagnostic: &mut Diagnostic, source_map: &SourceMap) {
    remap_span_line_file(&mut diagnostic.file, &mut diagnostic.line, source_map);
}

fn remap_span_line_file(file: &mut Option<String>, line: &mut u32, source_map: &SourceMap) {
    if file.is_some() || *line == 0 {
        return;
    }
    if let Some(origin) = source_map.origin_for_line(*line) {
        if let Some(origin_file) = &origin.file {
            *file = Some(origin_file.clone());
        }
        *line = origin.line;
    }
}

#[derive(Debug, Clone, Default)]
pub struct CpuCapabilityView {
    pub family_id: String,
    pub dialect_id: String,
    pub mnemonics: Vec<String>,
    pub registers: Vec<String>,
    pub runtime_directives: Vec<String>,
    pub mnemonic_owner: HashMap<String, String>,
}

#[derive(Debug, Clone, Default)]
pub struct CapabilitySnapshot {
    pub cpu_name_aliases: Vec<String>,
    pub family_ids: Vec<String>,
    pub cpu_ids: Vec<String>,
    pub dialect_ids: Vec<String>,
    pub directive_keywords: Vec<String>,
    pub cpu_views: HashMap<String, CpuCapabilityView>,
}

impl CapabilitySnapshot {
    pub fn from_registry(registry: &AsmRegistry) -> Self {
        let cpu_name_aliases = registry.cpu_name_list();
        let family_ids = registry
            .family_ids()
            .into_iter()
            .map(|f| f.as_str().to_string())
            .collect();
        let cpu_ids = registry
            .cpu_ids()
            .into_iter()
            .map(|c| c.as_str().to_string())
            .collect();

        let mut dialect_ids = HashSet::new();
        for family in registry.family_ids() {
            for dialect in registry.dialect_ids_for_family(family) {
                dialect_ids.insert(dialect);
            }
        }
        let dialect_ids = {
            let mut items: Vec<String> = dialect_ids.into_iter().collect();
            items.sort();
            items
        };

        let mut snapshot = Self {
            cpu_name_aliases,
            family_ids,
            cpu_ids,
            dialect_ids,
            ..Self::default()
        };

        for cpu in registry.cpu_ids() {
            let cpu_key = cpu.as_str().to_string();
            if let Some(view) = cpu_capability_view(registry, cpu) {
                snapshot.cpu_views.insert(cpu_key, view);
            }
        }

        snapshot.directive_keywords = collect_global_directives(&snapshot.cpu_views);
        snapshot
    }

    pub fn view_for_cpu(&self, cpu: CpuType) -> Option<&CpuCapabilityView> {
        self.cpu_views.get(cpu.as_str())
    }
}

pub fn scan_cpu_transitions(lines: &[String], registry: &AsmRegistry) -> Vec<(u32, CpuType)> {
    let mut out = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        let line_num = (idx + 1) as u32;
        let Some(name) = parse_cpu_directive_name(line) else {
            continue;
        };
        if let Some(cpu) = registry.resolve_cpu_name(&name) {
            out.push((line_num, cpu));
        }
    }
    out
}

pub fn resolve_cpu_for_line(
    line: u32,
    transitions: &[(u32, CpuType)],
    workspace_default_cpu: Option<CpuType>,
) -> CpuType {
    let mut selected = None;
    for (transition_line, cpu) in transitions {
        if *transition_line <= line {
            selected = Some(*cpu);
        } else {
            break;
        }
    }
    selected.or(workspace_default_cpu).unwrap_or(DEFAULT_CPU)
}

fn canonical_m68k_cpu_name(cpu: &str) -> Option<&'static str> {
    let lower = cpu.to_ascii_lowercase();
    let canonical = if let Some(rest) = lower.strip_prefix("mc") {
        rest
    } else if let Some(rest) = lower.strip_prefix('m') {
        rest
    } else {
        lower.as_str()
    };

    match canonical {
        "68000" => Some("68000"),
        "68010" => Some("68010"),
        "68020" => Some("68020"),
        "68030" => Some("68030"),
        "68040" => Some("68040"),
        "68080" => Some("68080"),
        _ => None,
    }
}

type M68kCpuScope = (
    &'static [&'static str],
    &'static [&'static str],
    &'static [&'static str],
    &'static str,
);

fn m68k_cpu_scope(cpu: &str) -> Option<M68kCpuScope> {
    match canonical_m68k_cpu_name(cpu)? {
        "68000" => Some((&["none"], &["none"], &[], "baseline-integer")),
        "68010" => Some((
            &["none"],
            &["none"],
            &[],
            "baseline-integer-plus-68010-delta",
        )),
        "68020" => Some((
            &["none"],
            &["none", "68881", "68882"],
            &[],
            "full-extension-addressing",
        )),
        "68030" => Some((
            &["pflush"],
            &["none", "68881", "68882"],
            &[],
            "full-extension-addressing",
        )),
        "68040" => Some((
            &["movec-registers", "pflush"],
            &["none", "68040"],
            &[],
            "full-extension-addressing,move16",
        )),
        "68080" => Some((
            &["movec-registers", "pflush"],
            &["none", "68080"],
            &["apollo", "ammx", "fpu68080"],
            "full-extension-addressing,move16,68080-full-extension-contract",
        )),
        _ => None,
    }
}

fn cpu_support_json_entry(registry: &AsmRegistry, cpu: CpuType) -> serde_json::Value {
    let family = registry
        .cpu_family_id(cpu)
        .map(|id| id.as_str().to_string());
    let default_dialect = registry.cpu_default_dialect(cpu).map(str::to_string);
    let runtime_directives = registry.cpu_runtime_directive_ids(cpu);
    let (mmu_surface, fpu_targets, extension_surfaces, scope_note) =
        match m68k_cpu_scope(cpu.as_str()) {
            Some((mmu_surface, fpu_targets, extension_surfaces, scope_note)) => (
                mmu_surface
                    .iter()
                    .map(|value| (*value).to_string())
                    .collect::<Vec<_>>(),
                fpu_targets
                    .iter()
                    .map(|value| (*value).to_string())
                    .collect::<Vec<_>>(),
                extension_surfaces
                    .iter()
                    .map(|value| (*value).to_string())
                    .collect::<Vec<_>>(),
                Some(scope_note.to_string()),
            ),
            None => (Vec::new(), Vec::new(), Vec::new(), None),
        };

    json!({
        "cpu": cpu.as_str(),
        "family": family,
        "default_dialect": default_dialect,
        "runtime_directives": runtime_directives,
        "mmu_surface": mmu_surface,
        "fpu_targets": fpu_targets,
        "extension_surfaces": extension_surfaces,
        "scope_note": scope_note,
    })
}

pub fn parse_cpu_directive_name(line: &str) -> Option<String> {
    let trimmed = line.trim_start();
    if !trimmed.to_ascii_lowercase().starts_with(".cpu") {
        return None;
    }
    let rest = trimmed.get(4..)?.trim_start();
    if rest.is_empty() {
        return None;
    }
    let token = rest
        .split(|ch: char| ch.is_whitespace() || ch == ';' || ch == ',')
        .next()
        .unwrap_or_default()
        .trim_matches('"')
        .trim_matches('\'');
    if token.is_empty() {
        None
    } else {
        Some(token.to_string())
    }
}

pub fn cpusupport_report(registry: &AsmRegistry) -> String {
    let mut lines = vec!["opforge-cpusupport-v1".to_string()];
    let mut cpu_ids = registry.cpu_ids();
    cpu_ids.sort_by_key(|cpu| cpu.as_str());

    for cpu in cpu_ids {
        let family = registry
            .cpu_family_id(cpu)
            .map(|id| id.as_str().to_string())
            .unwrap_or_else(|| "unknown".to_string());
        let dialect = registry.cpu_default_dialect(cpu).unwrap_or("none");
        let runtime_directives = registry.cpu_runtime_directive_ids(cpu).join(",");
        let (mmu_surface, fpu_targets, extension_surfaces, scope_note) =
            match m68k_cpu_scope(cpu.as_str()) {
                Some((mmu_surface, fpu_targets, extension_surfaces, scope_note)) => (
                    mmu_surface.join(","),
                    fpu_targets.join(","),
                    extension_surfaces.join(","),
                    scope_note,
                ),
                None => (String::new(), String::new(), String::new(), ""),
            };
        lines.push(format!(
            "cpu={};family={};default_dialect={};runtime_directives={};mmu_surface={};fpu_targets={};extension_surfaces={};scope_note={}",
            cpu.as_str(),
            family,
            dialect,
            runtime_directives,
            mmu_surface,
            fpu_targets,
            extension_surfaces,
            scope_note,
        ));
    }

    lines.join("\n")
}

pub fn cpusupport_report_json(registry: &AsmRegistry) -> serde_json::Value {
    let mut cpu_ids = registry.cpu_ids();
    cpu_ids.sort_by_key(|cpu| cpu.as_str());

    let cpus: Vec<serde_json::Value> = cpu_ids
        .into_iter()
        .map(|cpu| cpu_support_json_entry(registry, cpu))
        .collect();

    json!({
        "schema": "opforge-cpusupport-v1",
        "cpus": cpus,
    })
}

pub fn capabilities_report(registry: &AsmRegistry, version: &str, build_profile: &str) -> String {
    let mut lines = vec![
        "opforge-capabilities-v1".to_string(),
        format!("version={version}"),
        format!("build_profile={build_profile}"),
        "feature=include-path".to_string(),
        "feature=module-path".to_string(),
        "feature=input-extension-policy".to_string(),
        "feature=diagnostics-routing".to_string(),
        "feature=warning-policy".to_string(),
        "feature=cpu-override".to_string(),
        "feature=dependency-output".to_string(),
    ];

    let mut family_ids = registry.family_ids();
    family_ids.sort_by_key(|family| family.as_str());
    for family in family_ids {
        lines.push(format!("family={}", family.as_str()));
    }

    lines.extend(
        cpusupport_report(registry)
            .lines()
            .map(|line| line.to_string()),
    );
    format!("{}\n", lines.join("\n"))
}

pub fn capabilities_report_json(
    registry: &AsmRegistry,
    version: &str,
    build_profile: &str,
) -> String {
    let mut family_ids = registry.family_ids();
    family_ids.sort_by_key(|family| family.as_str());
    let families: Vec<String> = family_ids
        .into_iter()
        .map(|family| family.as_str().to_string())
        .collect();
    let features = vec![
        "include-path",
        "module-path",
        "input-extension-policy",
        "diagnostics-routing",
        "warning-policy",
        "cpu-override",
        "dependency-output",
    ];

    json!({
        "schema": "opforge-capabilities-v1",
        "version": version,
        "build_profile": build_profile,
        "features": features,
        "families": families,
        "cpusupport": cpusupport_report_json(registry),
    })
    .to_string()
}

fn cpu_capability_view(registry: &AsmRegistry, cpu: CpuType) -> Option<CpuCapabilityView> {
    let pipeline = registry.resolve_pipeline(cpu, None).ok()?;
    let family = pipeline.family_id;
    let dialect_id = pipeline.dialect_id.to_ascii_lowercase();
    let family_id = family.as_str().to_string();

    let mut mnemonic_owner = HashMap::new();
    let mut mnemonics = Vec::new();

    for mnemonic in registry.dialect_form_mnemonics(family, &dialect_id) {
        let key = mnemonic.to_ascii_lowercase();
        if !mnemonics
            .iter()
            .any(|m: &String| m.eq_ignore_ascii_case(&key))
        {
            mnemonics.push(key.clone());
        }
        mnemonic_owner.insert(key, format!("dialect:{dialect_id}"));
    }
    for mnemonic in registry.cpu_form_mnemonics(cpu) {
        let key = mnemonic.to_ascii_lowercase();
        if !mnemonics
            .iter()
            .any(|m: &String| m.eq_ignore_ascii_case(&key))
        {
            mnemonics.push(key.clone());
        }
        mnemonic_owner
            .entry(key)
            .or_insert_with(|| format!("cpu:{}", cpu.as_str().to_ascii_lowercase()));
    }
    for mnemonic in registry.family_form_mnemonics(family) {
        let key = mnemonic.to_ascii_lowercase();
        if !mnemonics
            .iter()
            .any(|m: &String| m.eq_ignore_ascii_case(&key))
        {
            mnemonics.push(key.clone());
        }
        mnemonic_owner
            .entry(key)
            .or_insert_with(|| format!("family:{family_id}"));
    }
    mnemonics.sort();

    let mut registers = registry.cpu_register_ids(cpu);
    registers.extend(registry.family_register_ids(family));
    registers.sort_by_key(|name| name.to_ascii_lowercase());
    registers.dedup_by(|left, right| left.eq_ignore_ascii_case(right));

    let mut runtime_directives: Vec<String> = registry
        .cpu_runtime_directive_ids(cpu)
        .into_iter()
        .map(|name| format!(".{}", name.to_ascii_lowercase()))
        .collect();
    runtime_directives.sort();
    runtime_directives.dedup();

    Some(CpuCapabilityView {
        family_id,
        dialect_id,
        mnemonics,
        registers,
        runtime_directives,
        mnemonic_owner,
    })
}

fn collect_global_directives(cpu_views: &HashMap<String, CpuCapabilityView>) -> Vec<String> {
    let mut out = vec![
        ".cpu".to_string(),
        ".if".to_string(),
        ".endif".to_string(),
        ".for".to_string(),
        ".bfor".to_string(),
        ".endfor".to_string(),
        ".while".to_string(),
        ".bwhile".to_string(),
        ".endwhile".to_string(),
        ".struct".to_string(),
        ".endstruct".to_string(),
        ".module".to_string(),
        ".endmodule".to_string(),
        ".use".to_string(),
        ".namespace".to_string(),
        ".endnamespace".to_string(),
        ".macro".to_string(),
        ".endmacro".to_string(),
        ".section".to_string(),
        ".endsection".to_string(),
        ".org".to_string(),
    ];
    for view in cpu_views.values() {
        out.extend(view.runtime_directives.iter().cloned());
    }
    out.sort();
    out.dedup();
    out
}

pub fn resolve_target_cpu(
    registry: &AsmRegistry,
    cpu_override: Option<&str>,
    default_cpu: CpuType,
) -> Result<CpuType, CpuResolutionError> {
    match cpu_override {
        Some(cpu_name) => registry
            .resolve_cpu_name(cpu_name)
            .ok_or_else(|| CpuResolutionError {
                requested: cpu_name.to_string(),
                known: registry.cpu_name_list(),
            }),
        None => Ok(default_cpu),
    }
}

#[cfg(test)]
mod tests {
    use super::{
        build_default_asm_registry, capabilities_report, capabilities_report_json,
        cpusupport_report, cpusupport_report_json, emit_export_sections, emit_linker_outputs,
        emit_mapfiles, export_sections_targets, linker_output_targets, make_runtime_line_router,
        mapfile_targets, parse_cpu_directive_name, prepare_assembly_session, resolve_cpu_for_line,
        resolve_formatter_module_paths, resolve_output_plan, resolve_target_cpu,
        root_module_id_from_lines, run_assembly, run_prepared_assembly, scan_cpu_transitions,
        AssemblerSessionConfig, AssemblyExecutionRequest, AssemblyPreparationRequest,
        CapabilitySnapshot, CpuResolutionError, ExecutionMode, FormatterPathResolutionRequest,
        MemoryOutputSink, MemorySourceProvider, OutputPlanningRequest,
        PreparedAssemblyExecutionRequest,
    };
    use asm::engine::Assembler;
    use asm::error::AsmErrorKind;
    use asm::output::{
        BinOutputSpec, ExportSectionsDirective, ExportSectionsFormat, ExportSectionsInclude,
        LabelOutputFormat, LinkerOutputDirective, LinkerOutputFormat, LinkerOutputOptionValue,
        LinkerOutputRelocationDisposition, MapFileDirective, MapSymbolsMode, OutputFormat,
        RootMetadata, SectionState,
    };
    use opcore::parser::Expr;
    use registry::cpu::{CpuFamily, CpuType};
    use registry::family::{AssemblerContext, EncodeResult, FamilyParseError};
    use registry::registry::{
        AsmRegistry, CpuHandlerDyn, CpuModule, DialectModule, FamilyHandlerDyn, FamilyModule,
        FamilyOperandSet, OperandSet,
    };
    use std::collections::BTreeMap;
    use std::path::{Path, PathBuf};
    #[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
    use std::sync::atomic::{AtomicU64, Ordering};
    #[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
    use std::time::{SystemTime, UNIX_EPOCH};
    use types::processing::{OpcoreRequestKind, ProcessingRequestKind};
    use types::symbol::SymbolTable;

    const TEST_FAMILY: CpuFamily = CpuFamily::new("test-family");
    const TEST_CPU: CpuType = CpuType::new("test-cpu");
    #[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
    static TEMP_DIR_SEQ: AtomicU64 = AtomicU64::new(1);

    #[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
    fn unique_temp_dir(prefix: &str) -> PathBuf {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let seq = TEMP_DIR_SEQ.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("{prefix}-{now}-{seq}"));
        std::fs::create_dir_all(&dir).expect("create temp dir");
        dir
    }

    #[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
    #[test]
    fn runtime_model_artifact_path_matches_shared_bootstrap_helper() {
        let temp_dir = unique_temp_dir("engine-runtime-path-parity");
        let engine_path = super::default_runtime_artifact_path_for_dir(temp_dir.as_path());
        let shared_path =
            vm::runtime_bootstrap::runtime_package_artifact_path_for_dir(temp_dir.as_path());

        assert_eq!(engine_path, shared_path);
    }

    #[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
    #[test]
    fn default_runtime_model_for_dir_returns_none_without_artifact() {
        let temp_dir = unique_temp_dir("engine-runtime-missing-artifact");
        assert!(
            super::build_default_runtime_model_for_dir(temp_dir.as_path()).is_none(),
            "expected vm-runtime-only artifact lookup to fail without artifact bytes"
        );
    }

    #[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
    #[test]
    fn default_runtime_model_for_dir_loads_present_artifact() {
        let temp_dir = unique_temp_dir("engine-runtime-present-artifact");
        let artifact_path = super::default_runtime_artifact_path_for_dir(temp_dir.as_path());
        let package_bytes = super::build_default_runtime_package_bytes()
            .expect("build default runtime package bytes");
        std::fs::create_dir_all(artifact_path.parent().expect("artifact parent"))
            .expect("create artifact parent");
        std::fs::write(&artifact_path, package_bytes).expect("write runtime artifact");

        assert!(
            super::build_default_runtime_model_for_dir(temp_dir.as_path()).is_some(),
            "expected vm-runtime-only artifact lookup to load the default runtime model"
        );
        assert!(
            vm::runtime_bootstrap::bootstrap_execution_model(
                Some(artifact_path.as_path()),
                None,
                false
            )
            .is_some(),
            "shared bootstrap should load the same runtime artifact"
        );
    }

    #[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
    #[test]
    fn editor_default_runtime_model_for_dir_recovers_after_artifact_is_created() {
        let temp_dir = unique_temp_dir("engine-runtime-recovers-after-miss");

        assert!(
            super::editor_default_runtime_model_for_dir(temp_dir.as_path()).is_none(),
            "expected no editor runtime model before artifact exists"
        );

        let artifact_path = super::default_runtime_artifact_path_for_dir(temp_dir.as_path());
        let package_bytes = super::build_default_runtime_package_bytes()
            .expect("build default runtime package bytes");
        std::fs::create_dir_all(artifact_path.parent().expect("artifact parent"))
            .expect("create artifact parent");
        std::fs::write(artifact_path, package_bytes).expect("write runtime artifact");

        assert!(
            super::editor_default_runtime_model_for_dir(temp_dir.as_path()).is_some(),
            "expected editor runtime model lookup to recover after artifact is created"
        );
    }

    #[cfg(all(feature = "vm-runtime-only", feature = "vm-runtime-opasm-artifact"))]
    #[test]
    fn editor_default_runtime_model_for_dir_invalidates_replaced_artifact() {
        let temp_dir = unique_temp_dir("engine-runtime-invalidates-replaced-artifact");
        let artifact_path = super::default_runtime_artifact_path_for_dir(temp_dir.as_path());
        let package_bytes = super::build_default_runtime_package_bytes()
            .expect("build default runtime package bytes");
        std::fs::create_dir_all(artifact_path.parent().expect("artifact parent"))
            .expect("create artifact parent");
        std::fs::write(&artifact_path, package_bytes).expect("write runtime artifact");

        assert!(
            super::editor_default_runtime_model_for_dir(temp_dir.as_path()).is_some(),
            "expected initial artifact lookup to succeed"
        );

        std::fs::write(&artifact_path, b"not-a-valid-runtime-package")
            .expect("replace runtime artifact with invalid bytes");

        assert!(
            super::editor_default_runtime_model_for_dir(temp_dir.as_path()).is_none(),
            "expected replaced artifact to invalidate the cached runtime model"
        );
    }

    #[test]
    fn resolve_output_plan_requires_a_resolved_base_for_default_outputs() {
        let source_lines = Vec::new();
        let result = resolve_output_plan(OutputPlanningRequest {
            input_base: "",
            source_lines: &source_lines,
            out_dir: None,
            metadata: &RootMetadata::default(),
            cpu_name: "8085",
            outfile_override: None,
            list_name_override: None,
            hex_name_override: None,
            srec_name_override: None,
            hunk_name_override: None,
            bin_specs_override: &[],
            fill_byte: 0,
            fill_byte_set: false,
            default_outputs: true,
            go_addr: None,
            pass1_errors: 0,
            suppress_outputs: false,
        });

        let error = match result {
            Ok(_) => panic!("default outputs without a resolved base should fail"),
            Err(error) => error,
        };

        assert!(
            error.to_string().contains(
                "No outputs selected. Provide .meta.output.name (or -o) or specify output flags"
            ),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn resolve_output_plan_rewrites_default_outputs_into_out_dir_for_absolute_input_base() {
        let source_lines = Vec::new();
        let out_dir = PathBuf::from("/virtual/out");
        let plan = resolve_output_plan(OutputPlanningRequest {
            input_base: "/virtual/main",
            source_lines: &source_lines,
            out_dir: Some(&out_dir),
            metadata: &RootMetadata::default(),
            cpu_name: "8085",
            outfile_override: None,
            list_name_override: None,
            hex_name_override: None,
            srec_name_override: None,
            hunk_name_override: None,
            bin_specs_override: &[],
            fill_byte: 0,
            fill_byte_set: false,
            default_outputs: true,
            go_addr: None,
            pass1_errors: 0,
            suppress_outputs: false,
        })
        .expect("out_dir plus absolute input_base should resolve default outputs");

        assert_eq!(plan.out_base(), "/virtual/out/main");
        assert_eq!(plan.list_path(), Some("/virtual/out/main.lst"));
        assert_eq!(plan.hex_path(), Some("/virtual/out/main.hex"));
    }

    #[test]
    fn resolve_output_plan_rejects_list_name_escape() {
        let source_lines = Vec::new();
        let result = resolve_output_plan(OutputPlanningRequest {
            input_base: "/virtual/main",
            source_lines: &source_lines,
            out_dir: Some(Path::new("/virtual/out")),
            metadata: &RootMetadata::default(),
            cpu_name: "8085",
            outfile_override: None,
            list_name_override: Some("../escape"),
            hex_name_override: None,
            srec_name_override: None,
            hunk_name_override: None,
            bin_specs_override: &[],
            fill_byte: 0,
            fill_byte_set: false,
            default_outputs: false,
            go_addr: None,
            pass1_errors: 0,
            suppress_outputs: false,
        });

        let error = match result {
            Ok(_) => panic!("list path escape should be rejected"),
            Err(error) => error,
        };

        assert!(error.to_string().contains("escapes resolved output root"));
    }

    #[test]
    fn resolved_output_plan_rejects_bin_name_escape() {
        let source_lines = Vec::new();
        let plan = resolve_output_plan(OutputPlanningRequest {
            input_base: "/virtual/main",
            source_lines: &source_lines,
            out_dir: Some(Path::new("/virtual/out")),
            metadata: &RootMetadata::default(),
            cpu_name: "8085",
            outfile_override: None,
            list_name_override: None,
            hex_name_override: None,
            srec_name_override: None,
            hunk_name_override: None,
            bin_specs_override: &[BinOutputSpec {
                name: Some("../escape".to_string()),
                range: None,
            }],
            fill_byte: 0,
            fill_byte_set: false,
            default_outputs: false,
            go_addr: None,
            pass1_errors: 0,
            suppress_outputs: false,
        })
        .expect("plan creation should succeed before bin path resolution");

        let error = plan
            .resolve_bin_outputs(Some((0, 1)))
            .expect_err("bin path escape should be rejected");

        assert!(error.contains("escapes resolved output root"));
    }

    #[test]
    fn resolve_artifact_output_path_rejects_out_dir_escape() {
        let error =
            super::resolve_artifact_output_path("../escape.map", Some(Path::new("/virtual/out")))
                .expect_err("directive path escape should be rejected");

        assert_eq!(error.kind(), AsmErrorKind::Directive);
        assert!(error.to_string().contains("escapes resolved output root"));
    }

    fn directive_test_sections() -> std::collections::HashMap<String, SectionState> {
        let mut sections = std::collections::HashMap::new();
        sections.insert(
            "code".to_string(),
            SectionState {
                base_addr: Some(0x2000),
                layout_placed: true,
                bytes: vec![0xaa, 0xbb],
                max_pc: 2,
                ..SectionState::default()
            },
        );
        sections
    }

    #[test]
    fn linker_output_targets_reject_absolute_directive_path_under_out_dir() {
        let outputs = vec![LinkerOutputDirective {
            path: "/tmp/output.bin".to_string(),
            format_id: LinkerOutputFormat::Bin.format_id().to_string(),
            options: BTreeMap::from([(
                "sections".to_string(),
                LinkerOutputOptionValue::TextList(vec!["code".to_string()]),
            )]),
            relocation_disposition: LinkerOutputRelocationDisposition::Unknown,
        }];

        let error = linker_output_targets(&outputs, Some(Path::new("/virtual/out")))
            .expect_err("absolute directive path should be rejected under out_dir");

        assert_eq!(error.kind(), AsmErrorKind::Directive);
        assert!(error.to_string().contains("escapes resolved output root"));
    }

    #[test]
    fn emit_linker_outputs_rejects_parent_escape_under_out_dir() {
        let outputs = vec![LinkerOutputDirective {
            path: "../escape.bin".to_string(),
            format_id: LinkerOutputFormat::Bin.format_id().to_string(),
            options: BTreeMap::from([(
                "sections".to_string(),
                LinkerOutputOptionValue::TextList(vec!["code".to_string()]),
            )]),
            relocation_disposition: LinkerOutputRelocationDisposition::Unknown,
        }];
        let sink = MemoryOutputSink::new();

        let error = emit_linker_outputs(
            &outputs,
            &directive_test_sections(),
            Some(Path::new("/virtual/out")),
            &sink,
        )
        .expect_err("parent escape should be rejected during linker output emission");

        assert_eq!(error.kind(), AsmErrorKind::Directive);
        assert!(error.to_string().contains("escapes resolved output root"));
        assert!(sink.files().is_empty());
    }

    #[test]
    fn emit_linker_outputs_writes_hunk_payload_when_relocation_is_proven() {
        let outputs = vec![LinkerOutputDirective {
            path: "build/out.hunk".to_string(),
            format_id: LinkerOutputFormat::Hunk.format_id().to_string(),
            options: BTreeMap::from([(
                "sections".to_string(),
                LinkerOutputOptionValue::TextList(vec!["code".to_string()]),
            )]),
            relocation_disposition: LinkerOutputRelocationDisposition::ProvenRelocationFree,
        }];
        let sink = MemoryOutputSink::new();

        emit_linker_outputs(&outputs, &directive_test_sections(), None, &sink)
            .expect("proven hunk output should be written");

        let files = sink.files();
        let (_, payload) = files
            .iter()
            .find(|(path, _)| path == Path::new("build/out.hunk"))
            .expect("written hunk payload");
        assert_eq!(
            payload,
            &vec![
                0x00, 0x00, 0x03, 0xf3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x03, 0xe9,
                0x00, 0x00, 0x00, 0x01, 0xaa, 0xbb, 0x00, 0x00, 0x00, 0x00, 0x03, 0xf2,
            ]
        );
    }

    #[test]
    fn export_sections_targets_reject_absolute_directive_path_under_out_dir() {
        let directives = vec![ExportSectionsDirective {
            dir: "/tmp/export".to_string(),
            format: ExportSectionsFormat::Bin,
            include: ExportSectionsInclude::NoBss,
        }];

        let error = export_sections_targets(
            &directives,
            &directive_test_sections(),
            Some(Path::new("/virtual/out")),
        )
        .expect_err("absolute export-sections path should be rejected under out_dir");

        assert_eq!(error.kind(), AsmErrorKind::Directive);
        assert!(error.to_string().contains("escapes resolved output root"));
    }

    #[test]
    fn emit_export_sections_rejects_parent_escape_under_out_dir() {
        let directives = vec![ExportSectionsDirective {
            dir: "../escape".to_string(),
            format: ExportSectionsFormat::Bin,
            include: ExportSectionsInclude::NoBss,
        }];
        let sink = MemoryOutputSink::new();

        let error = emit_export_sections(
            &directives,
            &directive_test_sections(),
            Some(Path::new("/virtual/out")),
            &sink,
        )
        .expect_err("parent escape should be rejected during export-sections emission");

        assert_eq!(error.kind(), AsmErrorKind::Directive);
        assert!(error.to_string().contains("escapes resolved output root"));
        assert!(sink.files().is_empty());
    }

    #[test]
    fn mapfile_targets_reject_absolute_directive_path_under_out_dir() {
        let directives = vec![MapFileDirective {
            path: "/tmp/output.map".to_string(),
            symbols: MapSymbolsMode::None,
        }];

        let error = mapfile_targets(&directives, Some(Path::new("/virtual/out")))
            .expect_err("absolute mapfile path should be rejected under out_dir");

        assert_eq!(error.kind(), AsmErrorKind::Directive);
        assert!(error.to_string().contains("escapes resolved output root"));
    }

    #[test]
    fn emit_mapfiles_rejects_parent_escape_under_out_dir() {
        let directives = vec![MapFileDirective {
            path: "../escape.map".to_string(),
            symbols: MapSymbolsMode::None,
        }];
        let sink = MemoryOutputSink::new();

        let error = emit_mapfiles(
            &directives,
            &std::collections::HashMap::new(),
            &directive_test_sections(),
            &SymbolTable::new(),
            Some(Path::new("/virtual/out")),
            &sink,
        )
        .expect_err("parent escape should be rejected during mapfile emission");

        assert_eq!(error.kind(), AsmErrorKind::Directive);
        assert!(error.to_string().contains("escapes resolved output root"));
        assert!(sink.files().is_empty());
    }

    #[test]
    fn directive_artifact_targets_anchor_rooted_relative_paths_under_out_dir() {
        let linker_outputs = vec![LinkerOutputDirective {
            path: "nested/output.bin".to_string(),
            format_id: LinkerOutputFormat::Bin.format_id().to_string(),
            options: BTreeMap::from([(
                "sections".to_string(),
                LinkerOutputOptionValue::TextList(vec!["code".to_string()]),
            )]),
            relocation_disposition: LinkerOutputRelocationDisposition::Unknown,
        }];
        let export_directives = vec![ExportSectionsDirective {
            dir: "nested/export".to_string(),
            format: ExportSectionsFormat::Bin,
            include: ExportSectionsInclude::NoBss,
        }];
        let mapfile_directives = vec![MapFileDirective {
            path: "nested/output.map".to_string(),
            symbols: MapSymbolsMode::None,
        }];
        let sections = directive_test_sections();

        assert_eq!(
            linker_output_targets(&linker_outputs, Some(Path::new("/virtual/out")))
                .expect("relative linker output should be anchored"),
            vec!["/virtual/out/nested/output.bin".to_string()]
        );
        assert_eq!(
            export_sections_targets(
                &export_directives,
                &sections,
                Some(Path::new("/virtual/out")),
            )
            .expect("relative export-sections path should be anchored"),
            vec!["/virtual/out/nested/export/code.bin".to_string()]
        );
        assert_eq!(
            mapfile_targets(&mapfile_directives, Some(Path::new("/virtual/out")))
                .expect("relative mapfile path should be anchored"),
            vec!["/virtual/out/nested/output.map".to_string()]
        );
    }

    #[derive(Clone)]
    struct StubOperands;

    impl FamilyOperandSet for StubOperands {
        fn as_any(&self) -> &dyn std::any::Any {
            self
        }

        fn clone_box(&self) -> Box<dyn FamilyOperandSet> {
            Box::new(self.clone())
        }
    }

    impl OperandSet for StubOperands {
        fn as_any(&self) -> &dyn std::any::Any {
            self
        }

        fn clone_box(&self) -> Box<dyn OperandSet> {
            Box::new(self.clone())
        }
    }

    struct StubFamilyHandler;

    impl FamilyHandlerDyn for StubFamilyHandler {
        fn family_id(&self) -> CpuFamily {
            TEST_FAMILY
        }

        fn parse_operands(
            &self,
            _mnemonic: &str,
            _exprs: &[Expr],
        ) -> Result<Box<dyn FamilyOperandSet>, FamilyParseError> {
            Ok(Box::new(StubOperands))
        }

        fn encode_instruction(
            &self,
            _mnemonic: &str,
            _operands: &dyn OperandSet,
            _ctx: &dyn AssemblerContext,
        ) -> EncodeResult<Vec<u8>> {
            EncodeResult::NotFound
        }

        fn is_register(&self, _name: &str) -> bool {
            false
        }

        fn is_condition(&self, _name: &str) -> bool {
            false
        }
    }

    struct StubCpuHandler;

    impl CpuHandlerDyn for StubCpuHandler {
        fn cpu_id(&self) -> CpuType {
            TEST_CPU
        }

        fn family_id(&self) -> CpuFamily {
            TEST_FAMILY
        }

        fn resolve_operands(
            &self,
            _mnemonic: &str,
            _family_operands: &dyn FamilyOperandSet,
            _ctx: &dyn AssemblerContext,
        ) -> Result<Box<dyn OperandSet>, String> {
            Ok(Box::new(StubOperands))
        }

        fn encode_instruction(
            &self,
            _mnemonic: &str,
            _operands: &dyn OperandSet,
            _ctx: &dyn AssemblerContext,
        ) -> EncodeResult<Vec<u8>> {
            EncodeResult::NotFound
        }

        fn supports_mnemonic(&self, _mnemonic: &str) -> bool {
            false
        }
    }

    struct StubDialect;

    impl DialectModule for StubDialect {
        fn dialect_id(&self) -> &'static str {
            "stub"
        }

        fn family_id(&self) -> CpuFamily {
            TEST_FAMILY
        }

        fn map_mnemonic(
            &self,
            _mnemonic: &str,
            _operands: &dyn FamilyOperandSet,
        ) -> Option<(String, Box<dyn FamilyOperandSet>)> {
            None
        }
    }

    struct StubFamilyModule;

    impl FamilyModule for StubFamilyModule {
        fn family_id(&self) -> CpuFamily {
            TEST_FAMILY
        }

        fn canonical_dialect(&self) -> &'static str {
            "stub"
        }

        fn dialects(&self) -> Vec<Box<dyn DialectModule>> {
            vec![Box::new(StubDialect)]
        }

        fn handler(&self) -> Box<dyn FamilyHandlerDyn> {
            Box::new(StubFamilyHandler)
        }
    }

    struct StubCpuModule;

    impl CpuModule for StubCpuModule {
        fn cpu_id(&self) -> CpuType {
            TEST_CPU
        }

        fn family_id(&self) -> CpuFamily {
            TEST_FAMILY
        }

        fn cpu_name(&self) -> &'static str {
            "stubcpu"
        }

        fn cpu_aliases(&self) -> &'static [&'static str] {
            &["stubalias"]
        }

        fn default_dialect(&self) -> &'static str {
            "stub"
        }

        fn handler(&self) -> Box<dyn CpuHandlerDyn> {
            Box::new(StubCpuHandler)
        }
    }

    #[test]
    fn default_registry_contains_expected_aliases() {
        let registry = build_default_asm_registry();
        assert_eq!(
            registry.resolve_cpu_name("8080"),
            Some(CpuType::new("8085"))
        );
        assert_eq!(
            registry.resolve_cpu_name("6502"),
            Some(CpuType::new("m6502"))
        );
        assert_eq!(
            registry.resolve_cpu_name("65c816"),
            Some(CpuType::new("65816"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mega65"),
            Some(CpuType::new("45gs02"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68000"),
            Some(CpuType::new("m68000"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68000"),
            Some(CpuType::new("m68000"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68010"),
            Some(CpuType::new("m68010"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68010"),
            Some(CpuType::new("m68010"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68020"),
            Some(CpuType::new("m68020"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68020"),
            Some(CpuType::new("m68020"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68030"),
            Some(CpuType::new("m68030"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68030"),
            Some(CpuType::new("m68030"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68040"),
            Some(CpuType::new("m68040"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68040"),
            Some(CpuType::new("m68040"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68080"),
            Some(CpuType::new("m68080"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68080"),
            Some(CpuType::new("m68080"))
        );
    }

    #[test]
    fn root_module_id_prefers_matching_explicit_module() {
        let lines = vec![".module main".to_string(), "lda #$01".to_string()];
        let module_id = root_module_id_from_lines(Path::new("main.asm"), &lines)
            .expect("root module id should resolve");
        assert_eq!(module_id, "main");
    }

    #[test]
    fn root_module_id_ignores_inactive_conditional_modules() {
        let lines = vec![
            ".if 0".to_string(),
            ".module dead".to_string(),
            ".endif".to_string(),
            ".module main".to_string(),
        ];
        let module_id = root_module_id_from_lines(Path::new("main.asm"), &lines)
            .expect("root module id should resolve");
        assert_eq!(module_id, "main");
    }

    #[test]
    fn resolve_target_cpu_uses_default_without_override() {
        let registry = AsmRegistry::new();
        let resolved = resolve_target_cpu(&registry, None, TEST_CPU).expect("default cpu");
        assert_eq!(resolved, TEST_CPU);
    }

    #[test]
    fn resolve_target_cpu_accepts_registered_aliases() {
        let mut registry = AsmRegistry::new();
        registry.register_family(Box::new(StubFamilyModule));
        registry.register_cpu(Box::new(StubCpuModule));

        let resolved = resolve_target_cpu(&registry, Some("stubalias"), CpuType::new("fallback"))
            .expect("registered alias");
        assert_eq!(resolved, TEST_CPU);
    }

    #[test]
    fn resolve_target_cpu_reports_requested_and_known_names() {
        let mut registry = AsmRegistry::new();
        registry.register_family(Box::new(StubFamilyModule));
        registry.register_cpu(Box::new(StubCpuModule));

        let err = resolve_target_cpu(&registry, Some("missing"), TEST_CPU)
            .expect_err("unknown cpu should fail");
        assert_eq!(
            err,
            CpuResolutionError {
                requested: "missing".to_string(),
                known: vec!["stubalias".to_string(), "stubcpu".to_string()],
            }
        );
        assert_eq!(
            err.to_string(),
            "Unknown CPU: missing. Known CPUs: stubalias, stubcpu"
        );
    }

    #[test]
    fn assembler_session_config_resolves_and_preserves_inputs() {
        let mut registry = AsmRegistry::new();
        registry.register_family(Box::new(StubFamilyModule));
        registry.register_cpu(Box::new(StubCpuModule));

        let config = AssemblerSessionConfig::resolve(
            registry,
            Some("stubalias"),
            CpuType::new("fallback"),
            42,
        )
        .expect("session config");
        assert_eq!(config.cpu(), TEST_CPU);
        assert_eq!(config.max_loop_iterations(), 42);

        let (cpu, registry, max_loop_iterations) = config.into_parts();
        assert_eq!(cpu, TEST_CPU);
        assert_eq!(max_loop_iterations, 42);
        assert_eq!(registry.resolve_cpu_name("stubcpu"), Some(TEST_CPU));
    }

    #[test]
    fn cpu_directive_parser_handles_quotes_and_comments() {
        assert_eq!(
            parse_cpu_directive_name("  .cpu \"stubalias\" ; comment"),
            Some("stubalias".to_string())
        );
        assert_eq!(parse_cpu_directive_name("lda #1"), None);
    }

    #[test]
    fn scan_and_resolve_cpu_transitions_track_nearest_prior_directive() {
        let mut registry = AsmRegistry::new();
        registry.register_family(Box::new(StubFamilyModule));
        registry.register_cpu(Box::new(StubCpuModule));

        let lines = vec![
            ".cpu stubcpu".to_string(),
            "nop".to_string(),
            ".cpu stubalias".to_string(),
            "nop".to_string(),
        ];
        let transitions = scan_cpu_transitions(&lines, &registry);
        assert_eq!(transitions, vec![(1, TEST_CPU), (3, TEST_CPU)]);
        assert_eq!(
            resolve_cpu_for_line(2, &transitions, Some(CpuType::new("fallback"))),
            TEST_CPU
        );
        assert_eq!(
            resolve_cpu_for_line(99, &[], Some(CpuType::new("fallback"))),
            CpuType::new("fallback")
        );
        assert_eq!(resolve_cpu_for_line(99, &[], None), CpuType::new("8085"));
    }

    #[test]
    fn report_builders_emit_expected_shapes() {
        let mut registry = AsmRegistry::new();
        registry.register_family(Box::new(StubFamilyModule));
        registry.register_cpu(Box::new(StubCpuModule));

        let text = cpusupport_report(&registry);
        assert!(text.starts_with("opforge-cpusupport-v1\n"));
        assert!(text.contains("cpu=test-cpu;family=test-family;default_dialect=stub"));

        let json = cpusupport_report_json(&registry);
        assert_eq!(json["schema"], "opforge-cpusupport-v1");

        let capabilities = capabilities_report(&registry, "1.2.3", "dev");
        assert!(capabilities.contains("version=1.2.3"));
        assert!(capabilities.contains("build_profile=dev"));
        assert!(capabilities.contains("family=test-family"));

        let capabilities_json = capabilities_report_json(&registry, "1.2.3", "dev");
        assert!(capabilities_json.contains("\"schema\":\"opforge-capabilities-v1\""));
    }

    #[test]
    fn capability_snapshot_contains_runtime_directives() {
        let mut registry = AsmRegistry::new();
        registry.register_family(Box::new(StubFamilyModule));
        registry.register_cpu(Box::new(StubCpuModule));

        let snapshot = CapabilitySnapshot::from_registry(&registry);
        assert!(snapshot.directive_keywords.iter().any(|d| d == ".struct"));
        assert!(snapshot.directive_keywords.iter().any(|d| d == ".for"));
        assert!(snapshot.directive_keywords.iter().any(|d| d == ".while"));
        let view = snapshot
            .view_for_cpu(TEST_CPU)
            .expect("snapshot should contain cpu view");
        assert_eq!(view.family_id, "test-family");
        assert_eq!(view.dialect_id, "stub");
    }

    #[test]
    fn prepare_assembly_session_builds_project_root_graph() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..");
        let root_path = repo_root
            .join("examples")
            .join("opcore")
            .join("project_root")
            .join("main.asm");
        let include_roots = vec![root_path.parent().expect("root parent").to_path_buf()];
        let prepared = prepare_assembly_session(AssemblyPreparationRequest {
            root_path: &root_path,
            defines: &[],
            include_roots: &include_roots,
            module_paths: &[],
            pp_macro_depth: 32,
            registry: build_default_asm_registry(),
            cpu_override: None,
            default_cpu: CpuType::new("8085"),
            max_loop_iterations: 1000,
            source_provider: None,
        })
        .expect("session should prepare");

        assert_eq!(prepared.root_module_id(), "main");
        assert!(prepared
            .expanded_lines()
            .iter()
            .any(|line| line.contains(".module util")));
        assert!(prepared
            .dependency_files()
            .iter()
            .any(|path| path.ends_with("util.asm")));
        assert!(prepared
            .source_map()
            .origins()
            .iter()
            .filter_map(|origin| origin.file.as_deref())
            .any(|file| file.ends_with("util.asm")));
    }

    #[test]
    fn prepare_assembly_session_supports_in_memory_source_provider() {
        let source_provider = MemorySourceProvider::default()
            .with_file(
                "/virtual/main.asm",
                ".include \"inc.asm\"\n.module main\n.byte VALUE\n",
            )
            .with_file("/virtual/inc.asm", "VALUE .const 7\n");
        let include_roots = vec![PathBuf::from("/virtual")];

        let prepared = prepare_assembly_session(AssemblyPreparationRequest {
            root_path: Path::new("/virtual/main.asm"),
            defines: &[],
            include_roots: &include_roots,
            module_paths: &[],
            pp_macro_depth: 32,
            registry: build_default_asm_registry(),
            cpu_override: None,
            default_cpu: CpuType::new("8085"),
            max_loop_iterations: 1000,
            source_provider: Some(&source_provider),
        })
        .expect("session should prepare from memory");

        assert_eq!(prepared.root_module_id(), "main");
        assert!(prepared
            .expanded_lines()
            .iter()
            .any(|line| line.contains("VALUE .const 7")));
        assert!(prepared
            .dependency_files()
            .iter()
            .any(|path| path == &PathBuf::from("/virtual/inc.asm")));
    }

    #[test]
    fn run_assembly_supports_in_memory_io_boundaries() {
        let source_provider = MemorySourceProvider::default()
            .with_file("/virtual/main.asm", ".module main\nnop\n.endmodule\n");
        let output_sink = MemoryOutputSink::default();

        let report = run_assembly(AssemblyExecutionRequest {
            root_path: Path::new("/virtual/main.asm"),
            execution_mode: ExecutionMode::Vm,
            input_base: "/virtual/main",
            defines: &[],
            include_paths: &[],
            module_paths: &[],
            pp_macro_depth: 32,
            cpu_override: None,
            default_cpu: CpuType::new("8085"),
            max_loop_iterations: 1000,
            opasm_package_path: None,
            out_dir: None,
            debug_conditionals: false,
            tab_size: None,
            output_format: OutputFormat::Text,
            go_addr: None,
            bin_specs: &[],
            fill_byte: 0,
            fill_byte_set: false,
            default_outputs: true,
            labels_file: None,
            label_output_format: LabelOutputFormat::Vice,
            dependency_output: None,
            outfile_override: None,
            list_name_override: None,
            hex_name_override: None,
            srec_name_override: Some(""),
            hunk_name_override: None,
            header_title: "test",
            output_sink: Some(&output_sink),
            source_provider: Some(&source_provider),
            suppress_outputs: false,
        })
        .expect("assembly should run from memory");

        assert_eq!(
            report.error_count(),
            0,
            "unexpected diagnostics: {:?}",
            report.diagnostics()
        );
        assert!(
            !report.runtime_processing_traces().is_empty(),
            "expected partitioned runtime traces in assembly report"
        );
        assert!(report
            .runtime_processing_traces()
            .iter()
            .any(|(_, _, trace)| {
                trace.requests().iter().any(|request| {
                    matches!(
                        request,
                        ProcessingRequestKind::Processor { processor, kind }
                        if processor == "asm" && kind == "statement"
                    )
                })
            }));
        let listing = output_sink
            .text("/virtual/main.lst")
            .expect("utf8 output")
            .expect("listing output should be captured");
        let hex = output_sink
            .text("/virtual/main.hex")
            .expect("utf8 output")
            .expect("hex output should be captured");
        let srec = output_sink
            .text("/virtual/main.srec")
            .expect("utf8 output")
            .expect("S-record output should be captured");
        assert!(listing.contains("nop"));
        assert!(!hex.trim().is_empty(), "hex:\n{hex}");
        assert!(hex.contains(":00000001FF"), "hex:\n{hex}");
        assert!(srec.contains("S9"), "srec:\n{srec}");
    }

    #[test]
    fn run_assembly_supports_cli_hunk_output_for_flat_source() {
        let source_provider = MemorySourceProvider::default().with_file(
            "/virtual/main.asm",
            "start: MOVE.L #target,D1\n RTS\ntarget: RTS\n",
        );
        let output_sink = MemoryOutputSink::default();

        let report = run_assembly(AssemblyExecutionRequest {
            root_path: Path::new("/virtual/main.asm"),
            execution_mode: ExecutionMode::Vm,
            input_base: "/virtual/main",
            defines: &[],
            include_paths: &[],
            module_paths: &[],
            pp_macro_depth: 32,
            cpu_override: Some("68000"),
            default_cpu: CpuType::new("8085"),
            max_loop_iterations: 1000,
            opasm_package_path: None,
            out_dir: None,
            debug_conditionals: false,
            tab_size: None,
            output_format: OutputFormat::Text,
            go_addr: None,
            bin_specs: &[],
            fill_byte: 0,
            fill_byte_set: false,
            default_outputs: false,
            labels_file: None,
            label_output_format: LabelOutputFormat::Vice,
            dependency_output: None,
            outfile_override: None,
            list_name_override: None,
            hex_name_override: None,
            srec_name_override: None,
            hunk_name_override: Some("/virtual/main.hunk"),
            header_title: "test",
            output_sink: Some(&output_sink),
            source_provider: Some(&source_provider),
            suppress_outputs: false,
        })
        .expect("flat hunk assembly should run from memory");

        assert_eq!(
            report.error_count(),
            0,
            "unexpected diagnostics: {:?}",
            report.diagnostics()
        );
        let hunk = output_sink
            .bytes("/virtual/main.hunk")
            .expect("hunk output should be captured");
        assert!(
            hunk.starts_with(&[0x00, 0x00, 0x03, 0xf3]),
            "expected HUNK_HEADER payload: {hunk:02X?}"
        );
    }

    #[test]
    fn run_prepared_assembly_preserves_prepare_time_registry() {
        let source_provider = MemorySourceProvider::default().with_file(
            "/virtual/main.asm",
            ".module main\n.cpu \"stubalias\"\n.endmodule\n",
        );
        let include_roots = vec![PathBuf::from("/virtual")];
        let mut registry = AsmRegistry::new();
        registry.register_family(Box::new(StubFamilyModule));
        registry.register_cpu(Box::new(StubCpuModule));

        let prepared = prepare_assembly_session(AssemblyPreparationRequest {
            root_path: Path::new("/virtual/main.asm"),
            defines: &[],
            include_roots: &include_roots,
            module_paths: &[],
            pp_macro_depth: 32,
            registry,
            cpu_override: None,
            default_cpu: TEST_CPU,
            max_loop_iterations: 1000,
            source_provider: Some(&source_provider),
        })
        .expect("session should prepare with custom registry");

        let (
            session,
            root_module_id,
            prepared_lines,
            source_map,
            dependency_files,
            module_macro_names,
        ) = prepared.into_parts();
        let (cpu, registry, max_loop_iterations) = session.into_parts();
        let report = run_prepared_assembly(PreparedAssemblyExecutionRequest {
            input_base: "/virtual/main",
            cpu,
            registry: std::sync::Arc::new(std::sync::Mutex::new(registry)),
            max_loop_iterations,
            opasm_package_path: None,
            root_module_id,
            prepared_lines,
            source_map,
            dependency_files,
            module_macro_names,
            out_dir: None,
            debug_conditionals: false,
            tab_size: None,
            output_format: OutputFormat::Text,
            go_addr: None,
            bin_specs: &[],
            fill_byte: 0,
            fill_byte_set: false,
            default_outputs: false,
            labels_file: None,
            label_output_format: LabelOutputFormat::Vice,
            dependency_output: None,
            outfile_override: None,
            list_name_override: None,
            hex_name_override: None,
            srec_name_override: None,
            hunk_name_override: None,
            header_title: "test",
            output_sink: None,
            execution_mode: ExecutionMode::Rust,
            suppress_outputs: true,
        })
        .expect("prepared execution should preserve custom registry");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
    }

    #[test]
    fn resolve_formatter_module_paths_collects_project_root_sources() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..");
        let root_path = repo_root
            .join("examples")
            .join("opcore")
            .join("project_root")
            .join("main.asm");
        let include_paths = vec![root_path.parent().expect("root parent").to_path_buf()];
        let files = resolve_formatter_module_paths(FormatterPathResolutionRequest {
            root_path: &root_path,
            asm_exts: &["asm".to_string()],
            inc_exts: &["inc".to_string()],
            defines: &[],
            include_paths: &include_paths,
            module_paths: &[],
            pp_macro_depth: 32,
        })
        .expect("formatter paths should resolve");

        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|path| path.ends_with("main.asm")));
        assert!(files.iter().any(|path| path.ends_with("util.asm")));
    }

    #[test]
    fn live_assembly_path_records_opcore_trace_for_use_directive() {
        let mut assembler =
            Assembler::with_cpu_and_registry(CpuType::new("8085"), build_default_asm_registry());
        assembler.set_runtime_line_router(Some(make_runtime_line_router(ExecutionMode::Vm)));

        let lines = vec![
            ".module main".to_string(),
            ".use math as m".to_string(),
            ".endmodule".to_string(),
        ];

        let _counts = assembler.pass1(&lines);

        let use_trace = assembler
            .runtime_processing_traces()
            .iter()
            .find(|(pass, line_num, _)| *pass == 1 && *line_num == 2)
            .map(|(_, _, trace)| trace)
            .expect("trace for .use line should be recorded");

        assert_eq!(
            use_trace.requests(),
            &[ProcessingRequestKind::Opcore(OpcoreRequestKind::Statement)]
        );
    }

    #[test]
    fn live_assembly_path_records_opcore_trace_for_module_directive() {
        let mut assembler =
            Assembler::with_cpu_and_registry(CpuType::new("8085"), build_default_asm_registry());
        assembler.set_runtime_line_router(Some(make_runtime_line_router(ExecutionMode::Vm)));

        let lines = vec![".module main".to_string(), ".endmodule".to_string()];

        let counts = assembler.pass1(&lines);
        assert_eq!(counts.errors, 0);

        let module_trace = assembler
            .runtime_processing_traces()
            .iter()
            .find(|(pass, line_num, _)| *pass == 1 && *line_num == 1)
            .map(|(_, _, trace)| trace)
            .expect("trace for .module line should be recorded");

        assert_eq!(
            module_trace.requests(),
            &[ProcessingRequestKind::Opcore(OpcoreRequestKind::Statement)]
        );
    }

    #[test]
    fn live_assembly_path_records_asm_fallback_and_expr_handoff_for_instruction() {
        let mut assembler =
            Assembler::with_cpu_and_registry(CpuType::new("8085"), build_default_asm_registry());
        assembler.set_runtime_line_router(Some(make_runtime_line_router(ExecutionMode::Vm)));

        let lines = vec!["    MVI A,1+2".to_string()];

        let counts = assembler.pass1(&lines);
        assert_eq!(counts.errors, 0);

        let trace = assembler
            .runtime_processing_traces()
            .iter()
            .find(|(pass, line_num, _)| *pass == 1 && *line_num == 1)
            .map(|(_, _, trace)| trace)
            .expect("trace for instruction line should be recorded");

        assert_eq!(
            trace.requests(),
            &[
                ProcessingRequestKind::Opcore(OpcoreRequestKind::Statement),
                ProcessingRequestKind::Processor {
                    processor: "asm".to_string(),
                    kind: "statement".to_string(),
                },
                ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr),
                ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr),
            ]
        );
    }

    struct StubContext {
        symbols: SymbolTable,
    }

    impl AssemblerContext for StubContext {
        fn eval_expr(&self, _expr: &Expr) -> Result<i64, String> {
            Ok(0)
        }

        fn symbols(&self) -> &SymbolTable {
            &self.symbols
        }

        fn has_symbol(&self, _name: &str) -> bool {
            false
        }

        fn symbol_is_finalized(&self, _name: &str) -> Option<bool> {
            None
        }

        fn current_address(&self) -> u32 {
            0
        }

        fn pass(&self) -> u8 {
            1
        }
    }

    #[test]
    fn stub_handlers_link_cleanly_for_registry_test_modules() {
        let ctx = StubContext {
            symbols: SymbolTable::new(),
        };
        let family = StubFamilyHandler;
        let cpu = StubCpuHandler;
        let operands = StubOperands;
        assert!(matches!(
            family.encode_instruction("noop", &operands, &ctx),
            EncodeResult::NotFound
        ));
        assert!(matches!(
            cpu.encode_instruction("noop", &operands, &ctx),
            EncodeResult::NotFound
        ));
        assert!(family.parse_operands("noop", &[]).is_ok());
        assert!(cpu.resolve_operands("noop", &operands, &ctx).is_ok());
    }
}
