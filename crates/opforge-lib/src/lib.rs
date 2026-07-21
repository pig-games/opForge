// SPDX-License-Identifier: GPL-3.0-or-later

//! Curated host-facing Rust API for libopforge.
//!
//! The normal Rust embedding path is [`Assembler`] with [`AssemblerConfig`].
//! Lower-level free functions remain available where they are part of the
//! assembler host boundary, and host-facing tooling exports live under
//! dedicated stable modules.
//!
//! ```
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! use libopforge::asm::{Assembler, LabelOutputFormat, OutputFormat};
//! use libopforge::io::{MemoryOutputSink, MemorySourceProvider};
//! use std::path::Path;
//!
//! let source_provider = MemorySourceProvider::new().with_file(
//!     "/virtual/main.asm",
//!     ".module main\n    .byte $2a\n.endmodule\n",
//! );
//! let output_sink = MemoryOutputSink::new();
//!
//! let report = Assembler::builder(Path::new("/virtual/main.asm"))
//!     .output_base("/virtual/main")
//!     .output_format(OutputFormat::Text)
//!     .label_output_format(LabelOutputFormat::Vice)
//!     .source_provider(&source_provider)
//!     .output_sink(&output_sink)
//!     .assemble()?;
//!
//! assert_eq!(report.error_count(), 0);
//! assert!(output_sink
//!     .text("/virtual/main.lst")?
//!     .expect("listing output")
//!     .contains(".byte $2a"));
//! # Ok(())
//! # }
//! ```
//!
//! ```
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! use libopforge::asm::{AssemblerSession, LabelOutputFormat, OutputFormat};
//! use libopforge::io::{MemoryOutputSink, MemorySourceProvider};
//!
//! let source_provider = MemorySourceProvider::new().with_file(
//!     "/virtual/main.asm",
//!     ".module main\n    .byte $00\n.endmodule\n",
//! );
//! let output_sink = MemoryOutputSink::new();
//!
//! let prepared = AssemblerSession::builder("/virtual/main.asm")
//!     .output_base("/virtual/main")
//!     .source_provider(source_provider.clone())
//!     .output_sink(output_sink.clone())
//!     .output_format(OutputFormat::Text)
//!     .label_output_format(LabelOutputFormat::Vice)
//!     .prepare()?;
//!
//! assert_eq!(prepared.root_module_id(), "main");
//! assert!(!prepared.cpu_name().is_empty());
//! assert_eq!(prepared.assemble()?.error_count(), 0);
//! # Ok(())
//! # }
//! ```

use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use types::source_map::SourceMap;
use types::symbol::SymbolVisibility;

use crate::asm::{BinOutputSpec, DependencyOutputPolicy, LabelOutputFormat, OutputFormat};
use crate::diagnostics::{AsmRunError, AsmRunReport};
use crate::io::{OutputSink, SourceProvider};
use crate::lockstep::ExecutionMode;

/// Concern inventory: stable assembler diagnostics and run-report payloads.
///
/// Owns host-facing access to `Diagnostic`, `AsmRunReport`, `AsmRunError`, and
/// the assembler diagnostic taxonomy used by high-level assembly results.
/// It does not own source loading, CPU discovery, or orchestration policy.
pub mod diagnostics {
    pub use ::asm::error::{
        build_context_lines, build_context_lines_with_range, AsmError, AsmErrorKind, AsmRunError,
        AsmRunReport, Diagnostic, Severity,
    };
}

/// Concern inventory: host I/O adapters and abstraction traits.
///
/// Owns stable filesystem and memory-backed `SourceProvider` and `OutputSink`
/// adapters for embedding hosts. It does not own diagnostics, CPU selection,
/// or assembly execution semantics.
pub mod io {
    pub use ::engine::{
        FsOutputSink, FsSourceProvider, MemoryOutputSink, MemorySourceProvider, OutputSink,
        SourceProvider,
    };
}

/// Concern inventory: processor-neutral routing and failure contracts.
///
/// Owns line-routing helpers, processing traces, and the stable neutral
/// `ProcessorError` surface used when processor-local failures cross the engine
/// boundary. It does not own assembler workflow packaging or generic language
/// parsing APIs.
pub mod processing {
    pub use ::engine::{
        process_opcore_expression_request, process_opcore_expression_request_with_mode,
    };
    pub use ::registry::syntax::{
        register_checker_from_fn, register_checker_none, RegisterChecker,
    };
    pub use ::types::processing::{
        LineProcessingTrace, OpcoreRequestKind, ProcessingOutcome, ProcessingRequestKind,
        ProcessingReturn, ProcessorError, ProcessorErrorKind, ProcessorFailureDetail,
    };
    pub use ::vm::vm_opasm::HierarchyExecutionModel;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum EngineErrorKind {
        Core,
        Processor,
    }

    #[derive(Debug, Clone)]
    pub enum EngineError {
        Core(super::opcore::CoreError),
        Processor(ProcessorError),
    }

    impl EngineError {
        pub fn kind(&self) -> EngineErrorKind {
            match self {
                Self::Core(_) => EngineErrorKind::Core,
                Self::Processor(_) => EngineErrorKind::Processor,
            }
        }

        pub fn code(&self) -> &str {
            match self {
                Self::Core(err) => err.code(),
                Self::Processor(err) => err.code(),
            }
        }

        pub fn summary(&self) -> &str {
            match self {
                Self::Core(err) => err.summary(),
                Self::Processor(err) => err.summary(),
            }
        }

        pub fn as_core(&self) -> Option<&super::opcore::CoreError> {
            match self {
                Self::Core(err) => Some(err),
                Self::Processor(_) => None,
            }
        }

        pub fn as_processor(&self) -> Option<&ProcessorError> {
            match self {
                Self::Core(_) => None,
                Self::Processor(err) => Some(err),
            }
        }
    }

    impl std::fmt::Display for EngineError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(self.summary())
        }
    }

    impl std::error::Error for EngineError {}

    fn map_engine_error(err: ::engine::EngineError) -> EngineError {
        match err {
            ::engine::EngineError::Core(err) => EngineError::Core(err),
            ::engine::EngineError::Processor(err) => EngineError::Processor(err),
        }
    }

    /// Routes one line through the default editor-style processing boundary.
    ///
    /// Use this when a host wants to ask whether a line belongs to `opcore`
    /// or should fall through to assembler processing without prebuilding an
    /// explicit runtime model. See `documentation/libopforge-diagnostics-and-fixits-guide.md`.
    pub fn editor_route_line(
        line: &str,
        line_num: u32,
    ) -> Result<(super::opcore::LineAst, LineProcessingTrace), EngineError> {
        ::engine::editor_route_line(line, line_num).map_err(map_engine_error)
    }

    /// Routes a line only when the caller expects an `opcore` module-item path.
    ///
    /// Use this for `.module`, `.use`, and related editor flows that want to
    /// keep assembler-only lines as `None` instead of forcing statement
    /// processing. See `documentation/libopforge-diagnostics-and-fixits-guide.md`.
    pub fn route_module_item_line(
        line: &str,
        line_num: u32,
    ) -> Result<(Option<super::opcore::LineAst>, LineProcessingTrace), EngineError> {
        ::engine::route_module_item_line(line, line_num).map_err(map_engine_error)
    }

    /// Routes a module-item line with an explicit runtime model and CPU context.
    ///
    /// Use this when the host already owns the execution model, CPU id,
    /// dialect override, and register-checking policy rather than relying on
    /// the default runtime-model lookup path. See
    /// `documentation/libopforge-cpu-family-extension-guide.md` for the
    /// lower-level model and registry boundary.
    pub fn route_module_item_line_with_model(
        model: &::vm::vm_opasm::HierarchyExecutionModel,
        cpu_id: &str,
        dialect_override: Option<&str>,
        line: &str,
        line_num: u32,
        register_checker: &::registry::syntax::RegisterChecker,
    ) -> Result<(Option<super::opcore::LineAst>, LineProcessingTrace), EngineError> {
        ::engine::route_module_item_line_with_model(
            model,
            cpu_id,
            dialect_override,
            line,
            line_num,
            register_checker,
        )
        .map_err(map_engine_error)
    }

    /// Routes one line with an explicit runtime model and CPU context.
    ///
    /// Use this when an editor or service wants the default routing behavior
    /// but must control the execution model, CPU id, dialect override, or
    /// register-checking policy explicitly. See
    /// `documentation/libopforge-diagnostics-and-fixits-guide.md`.
    pub fn editor_route_line_with_model(
        model: &::vm::vm_opasm::HierarchyExecutionModel,
        cpu_id: &str,
        dialect_override: Option<&str>,
        line: &str,
        line_num: u32,
        register_checker: &::registry::syntax::RegisterChecker,
    ) -> Result<(super::opcore::LineAst, LineProcessingTrace), EngineError> {
        ::engine::editor_route_line_with_model(
            model,
            cpu_id,
            dialect_override,
            line,
            line_num,
            register_checker,
        )
        .map_err(map_engine_error)
    }

    /// Routes one line with an explicit runtime model and lockstep-aware mode.
    ///
    /// Use this when the host wants editor-style routing plus an explicit
    /// execution-mode choice, including `Lockstep`, and needs the resulting
    /// `LockstepReport`. See
    /// `documentation/libopforge-execution-modes-and-lockstep-guide.md` and
    /// `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`.
    pub fn editor_route_line_with_model_in_mode(
        model: &::vm::vm_opasm::HierarchyExecutionModel,
        cpu_id: &str,
        dialect_override: Option<&str>,
        line: &str,
        line_num: u32,
        register_checker: &::registry::syntax::RegisterChecker,
        execution_mode: super::lockstep::ExecutionMode,
    ) -> Result<
        (
            super::opcore::LineAst,
            LineProcessingTrace,
            super::lockstep::LockstepReport,
        ),
        EngineError,
    > {
        ::engine::editor_route_line_with_model_in_mode(
            model,
            cpu_id,
            dialect_override,
            line,
            line_num,
            register_checker,
            execution_mode,
        )
        .map_err(map_engine_error)
    }
}

/// Concern inventory: CPU, family, and capability discovery.
///
/// Owns stable registry lookup, builtin capability snapshots, and CPU
/// resolution helpers for hosts that need discovery or validation. It does not
/// own full custom extension authoring workflows beyond that discovery surface.
pub mod registry {
    /// ```
    /// use libopforge::registry;
    ///
    /// let asm_registry = registry::default_asm_registry();
    /// let snapshot = registry::CapabilitySnapshot::from_registry(&asm_registry);
    /// let resolved = registry::resolve_target_cpu(
    ///     &asm_registry,
    ///     Some("8085"),
    ///     registry::CpuType::new("8085"),
    /// )
    /// .expect("registered cpu should resolve");
    /// let view = snapshot
    ///     .view_for_cpu(resolved)
    ///     .expect("capability view should exist");
    ///
    /// assert_eq!(view.family_id, "intel8080");
    /// assert!(!view.mnemonics.is_empty());
    /// ```
    pub use ::engine::{
        capabilities_report, capabilities_report_json, cpusupport_report, cpusupport_report_json,
        default_cpu, parse_cpu_directive_name, resolve_cpu_for_line, resolve_target_cpu,
        scan_cpu_transitions, AsmRegistryContext, CapabilitySnapshot, CpuCapabilityView,
        CpuResolutionError,
    };
    pub use ::registry::{AsmRegistry, CpuFamily, CpuType};

    pub fn default_asm_registry() -> AsmRegistry {
        ::engine::build_default_asm_registry()
    }
}

/// Concern inventory: stable formatter entrypoints and reports.
///
/// Owns formatter configuration, run reports, and file-level formatting output.
/// It does not redefine assembler or language-core diagnostics taxonomies.
pub mod formatter {
    /// ```
    /// use libopforge::formatter::{FormatterConfig, FormatterEngine};
    ///
    /// let engine = FormatterEngine::new(FormatterConfig::default());
    /// let output = engine.format_source_with_diagnostics("start:  lda #$10,x ; comment\n");
    ///
    /// assert_eq!(output.rendered, "start:  lda #$10, x  ; comment\n");
    /// assert!(output.diagnostics.is_empty());
    /// ```
    pub use ::formatter::{
        FormatMode, FormatterConfig, FormatterDiagnostic, FormatterEngine, FormatterFileReport,
        FormatterOutput, FormatterRunReport, FormatterRunSummary,
    };
}

/// Concern inventory: generic non-assembler language services.
///
/// Owns tokenization, expression parsing, module-item handling, macro and
/// preprocess concerns, and the stable `CoreError` domain for generic language
/// failures. It does not own assembler statement encoding or artifact output.
pub mod opcore {
    /// ```
    /// use libopforge::opcore;
    /// use libopforge::processing;
    ///
    /// let tokenized = opcore::tokenize_line("1 + 2", 1).expect("tokenization should succeed");
    /// let expr = opcore::parse_expression(tokenized).expect("expression parse should succeed");
    /// assert!(matches!(expr, opcore::Expr::Binary { .. }));
    ///
    /// match opcore::process_module_item(".use math as m", 2) {
    ///     processing::ProcessingOutcome::Done(opcore::LineAst::Use(use_ast)) => {
    ///         assert_eq!(use_ast.module_id, "math");
    ///         assert_eq!(use_ast.alias.as_deref(), Some("m"));
    ///     }
    ///     other => panic!("expected .use AST, got {other:?}"),
    /// }
    /// ```
    pub use ::opcore::expr::EvalError;
    pub use ::opcore::expression::expr_text;
    pub use ::opcore::macro_processor::{MacroError, MacroProcessor};
    pub use ::opcore::parser::{AssignOp, Expr, Label, LineAst, ParseError, UseItem};
    pub use ::opcore::preprocess::{
        parse_incbin_target_from_source_line, parse_include_target_from_source_line,
        PreprocessError, Preprocessor,
    };
    pub use ::opcore::services::{
        parse_expression, parse_expression_tokens, tokenize_line, TokenizedLine,
    };
    pub use ::opcore::tokenizer::{Span, Token, TokenKind, TokenizeError, Tokenizer};
    pub use ::opcore::{CoreError, CoreErrorKind, LineParseError, ModuleItemError};

    pub fn editor_parse_line(line: &str, line_num: u32) -> Result<LineAst, LineParseError> {
        ::engine::editor_parse_line(line, line_num)
            .map_err(|err| LineParseError::from_line_parse(line, err))
    }

    pub fn process_module_item(
        line: &str,
        line_num: u32,
    ) -> ::types::processing::ProcessingOutcome<LineAst, ModuleItemError> {
        match ::opcore::services::process_module_item(line, line_num) {
            ::types::processing::ProcessingOutcome::Done(ast) => {
                ::types::processing::ProcessingOutcome::Done(ast)
            }
            ::types::processing::ProcessingOutcome::Return(ret) => {
                ::types::processing::ProcessingOutcome::Return(ret)
            }
            ::types::processing::ProcessingOutcome::Error(err) => {
                ::types::processing::ProcessingOutcome::Error(
                    ModuleItemError::from_module_item_parse(line, err),
                )
            }
        }
    }

    pub mod portable {
        use ::types::processing::ProcessingOutcome;

        pub use ::vm::portable_contract::{
            PortableAstExpr, PortableLineAst, PortableOperatorKind, PortableSpan, PortableToken,
            PortableTokenKind,
        };

        pub use super::{ModuleItemError, ParseError, TokenizeError};

        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct TokenizedLine {
            pub tokens: Vec<PortableToken>,
            pub end_span: PortableSpan,
            pub end_token_text: Option<String>,
        }

        pub fn tokenize_line(line: &str, line_num: u32) -> Result<TokenizedLine, TokenizeError> {
            let tokenized = super::tokenize_line(line, line_num)?;
            Ok(TokenizedLine {
                tokens: tokenized
                    .tokens
                    .into_iter()
                    .filter_map(PortableToken::from_core_token)
                    .collect(),
                end_span: tokenized.end_span.into(),
                end_token_text: tokenized.end_token_text,
            })
        }

        pub fn parse_expression(tokenized: TokenizedLine) -> Result<PortableAstExpr, ParseError> {
            let expr = super::parse_expression_tokens(
                tokenized
                    .tokens
                    .iter()
                    .map(PortableToken::to_core_token)
                    .collect(),
                tokenized.end_span.into(),
                tokenized.end_token_text,
            )?;
            Ok(PortableAstExpr::from_core_expr(&expr))
        }

        pub fn process_module_item(
            line: &str,
            line_num: u32,
        ) -> ProcessingOutcome<PortableLineAst, ModuleItemError> {
            match super::process_module_item(line, line_num) {
                ProcessingOutcome::Done(ast) => {
                    ProcessingOutcome::Done(PortableLineAst::from_core_line_ast(&ast))
                }
                ProcessingOutcome::Return(ret) => ProcessingOutcome::Return(ret),
                ProcessingOutcome::Error(err) => ProcessingOutcome::Error(err),
            }
        }
    }
}

/// Concern inventory: execution-head coordination and parity reporting.
///
/// Owns `ExecutionMode`, continuation-head selection, lockstep checkpoints,
/// and lockstep divergence or match reporting. It does not own statement
/// processing or registry discovery.
pub mod lockstep {
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use libopforge::asm::{AssemblerSession, LabelOutputFormat, OutputFormat};
    /// use libopforge::io::{MemoryOutputSink, MemorySourceProvider};
    /// use libopforge::lockstep::{ContinuationHead, ExecutionMode};
    ///
    /// let source_provider = MemorySourceProvider::new().with_file(
    ///     "/virtual/main.asm",
    ///     ".module main\n    lda #$42\n.endmodule\n",
    /// );
    /// let output_sink = MemoryOutputSink::new();
    ///
    /// let report = AssemblerSession::builder("/virtual/main.asm")
    ///     .output_base("/virtual/main")
    ///     .source_provider(source_provider.clone())
    ///     .output_sink(output_sink.clone())
    ///     .execution_mode(ExecutionMode::Lockstep {
    ///         continuation_head: ContinuationHead::Vm,
    ///     })
    ///     .output_format(OutputFormat::Text)
    ///     .label_output_format(LabelOutputFormat::Vice)
    ///     .assemble()?;
    ///
    /// assert_eq!(report.error_count(), 0);
    /// assert!(!report.lockstep_report().matches().is_empty());
    /// # Ok(())
    /// # }
    /// ```
    pub use ::engine::{
        ContinuationHead, ExecutionMode, LockstepCheckpoint, LockstepComparisonCategory,
        LockstepDivergence, LockstepMatch, LockstepReport, LockstepStage,
    };
}

/// Concern inventory: high-level assembler workflow and assembler-specific APIs.
///
/// Owns the supported assembly embedding path, assembler workflow errors,
/// grouped config/session lifecycle types, and output-oriented assembly helpers.
/// Root-level assembler re-exports remain compatibility-oriented; canonical
/// host imports for this domain live under `libopforge::asm`.
pub mod asm {
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use libopforge::asm::{Assembler, AssemblerWorkflowErrorKind, LabelOutputFormat, OutputFormat};
    /// use libopforge::diagnostics::AsmErrorKind;
    /// use libopforge::io::{MemoryOutputSink, MemorySourceProvider};
    /// use std::path::Path;
    ///
    /// let source_provider = MemorySourceProvider::new().with_file(
    ///     "/virtual/main.asm",
    ///     ".module main\n.this_is_not_a_real_directive\n.endmodule\n",
    /// );
    /// let output_sink = MemoryOutputSink::new();
    ///
    /// let err = match Assembler::builder(Path::new("/virtual/main.asm"))
    ///     .output_base("/virtual/main")
    ///     .output_format(OutputFormat::Text)
    ///     .label_output_format(LabelOutputFormat::Vice)
    ///     .source_provider(&source_provider)
    ///     .output_sink(&output_sink)
    ///     .assemble()
    /// {
    ///     Ok(_) => panic!("invalid source should fail"),
    ///     Err(err) => err,
    /// };
    ///
    /// assert_eq!(err.kind(), AssemblerWorkflowErrorKind::Assemble);
    /// assert_eq!(err.as_assemble().expect("assemble payload").kind(), AsmErrorKind::Assembler);
    /// # Ok(())
    /// # }
    /// ```
    pub use ::asm::output::{
        parse_bin_output_arg, parse_bin_range_str, resolve_bin_path, resolve_output_path,
        BinOutputSpec, BinRange, DependencyOutputPolicy, LabelOutputFormat, OutputFormat,
    };
    pub use ::engine::{ContinuationHead, ExecutionMode};

    fn workflow_code(kind: AssemblerWorkflowErrorKind) -> &'static str {
        match kind {
            AssemblerWorkflowErrorKind::InvalidArgument => "asm.workflow.invalid_argument",
            AssemblerWorkflowErrorKind::InvalidRequest => "asm.workflow.invalid_request",
            AssemblerWorkflowErrorKind::Assemble => "asm.workflow.assemble",
            AssemblerWorkflowErrorKind::Io => "asm.workflow.io",
            AssemblerWorkflowErrorKind::Internal => "asm.workflow.internal",
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum AssemblerWorkflowErrorKind {
        InvalidArgument,
        InvalidRequest,
        Assemble,
        Io,
        Internal,
    }

    #[derive(Debug, Clone)]
    struct WorkflowErrorDetail {
        code: String,
        summary: String,
    }

    impl WorkflowErrorDetail {
        fn new(code: impl Into<String>, summary: impl Into<String>) -> Self {
            Self {
                code: code.into(),
                summary: summary.into(),
            }
        }

        fn code(&self) -> &str {
            &self.code
        }

        fn summary(&self) -> &str {
            &self.summary
        }
    }

    impl std::fmt::Display for WorkflowErrorDetail {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(self.summary())
        }
    }

    macro_rules! define_workflow_error_wrapper {
        ($name:ident) => {
            #[derive(Debug, Clone)]
            pub struct $name {
                detail: WorkflowErrorDetail,
            }

            impl $name {
                pub fn new(code: impl Into<String>, summary: impl Into<String>) -> Self {
                    Self {
                        detail: WorkflowErrorDetail::new(code, summary),
                    }
                }

                pub fn code(&self) -> &str {
                    self.detail.code()
                }

                pub fn summary(&self) -> &str {
                    self.detail.summary()
                }
            }

            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    self.detail.fmt(f)
                }
            }

            impl std::error::Error for $name {}
        };
    }

    define_workflow_error_wrapper!(InvalidArgumentError);
    define_workflow_error_wrapper!(InvalidRequestError);
    define_workflow_error_wrapper!(HostIoError);
    define_workflow_error_wrapper!(InternalErrorReport);

    #[derive(Debug)]
    pub enum AssemblerWorkflowError {
        InvalidArgument(InvalidArgumentError),
        InvalidRequest(InvalidRequestError),
        Assemble(super::diagnostics::AsmRunError),
        Io(HostIoError),
        Internal(InternalErrorReport),
    }

    impl AssemblerWorkflowError {
        pub fn kind(&self) -> AssemblerWorkflowErrorKind {
            match self {
                Self::InvalidArgument(_) => AssemblerWorkflowErrorKind::InvalidArgument,
                Self::InvalidRequest(_) => AssemblerWorkflowErrorKind::InvalidRequest,
                Self::Assemble(_) => AssemblerWorkflowErrorKind::Assemble,
                Self::Io(_) => AssemblerWorkflowErrorKind::Io,
                Self::Internal(_) => AssemblerWorkflowErrorKind::Internal,
            }
        }

        pub fn code(&self) -> &str {
            match self {
                Self::InvalidArgument(err) => err.code(),
                Self::InvalidRequest(err) => err.code(),
                Self::Assemble(_) => workflow_code(self.kind()),
                Self::Io(err) => err.code(),
                Self::Internal(err) => err.code(),
            }
        }

        pub fn summary(&self) -> &str {
            match self {
                Self::InvalidArgument(err) => err.summary(),
                Self::InvalidRequest(err) => err.summary(),
                Self::Assemble(err) => err.summary(),
                Self::Io(err) => err.summary(),
                Self::Internal(err) => err.summary(),
            }
        }

        pub fn as_assemble(&self) -> Option<&super::diagnostics::AsmRunError> {
            match self {
                Self::Assemble(err) => Some(err),
                _ => None,
            }
        }

        pub fn as_invalid_argument(&self) -> Option<&InvalidArgumentError> {
            match self {
                Self::InvalidArgument(err) => Some(err),
                _ => None,
            }
        }

        pub fn as_invalid_request(&self) -> Option<&InvalidRequestError> {
            match self {
                Self::InvalidRequest(err) => Some(err),
                _ => None,
            }
        }

        pub fn as_io(&self) -> Option<&HostIoError> {
            match self {
                Self::Io(err) => Some(err),
                _ => None,
            }
        }

        pub fn as_internal(&self) -> Option<&InternalErrorReport> {
            match self {
                Self::Internal(err) => Some(err),
                _ => None,
            }
        }
    }

    impl std::fmt::Display for AssemblerWorkflowError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(self.summary())
        }
    }

    impl std::error::Error for AssemblerWorkflowError {}

    impl From<super::diagnostics::AsmRunError> for AssemblerWorkflowError {
        fn from(err: super::diagnostics::AsmRunError) -> Self {
            Self::Assemble(err)
        }
    }

    /// Concern inventory: lower-level assembler statement processing.
    ///
    /// Owns CPU-aware statement tokenization, parsing, and processing without a
    /// full assembly session, including portable statement forms. It does not
    /// own high-level assembly orchestration or artifact emission.
    pub mod opasm {
        /// ```
        /// use libopforge::asm::opasm;
        /// use libopforge::opcore;
        /// use libopforge::processing;
        ///
        /// let tokenized = opasm::tokenize_statement(opasm::StatementRequest::new(".byte 1, 2", 1))
        ///     .expect("statement tokenization should succeed");
        /// assert!(!tokenized.tokens.is_empty());
        ///
        /// let processed = opasm::process_statement(opasm::StatementRequest::new(".module demo", 2), None)
        ///     .expect("statement processing should succeed");
        /// assert!(matches!(processed.parsed.ast, opcore::LineAst::Statement(..)));
        /// assert_eq!(
        ///     processed.trace.requests(),
        ///     &[processing::ProcessingRequestKind::Processor {
        ///         processor: "asm".to_string(),
        ///         kind: "statement".to_string(),
        ///     }]
        /// );
        /// ```
        pub use ::asm::opasm::{
            default_register_checker, parse_statement, process_statement, tokenize_statement,
            StatementExprProcessor, StatementParseResult, StatementProcessResult, StatementRequest,
            TokenizedStatement,
        };
        pub use ::opcore::parser::{LineAst, ParseError};
        pub use ::opcore::tokenizer::{Span, Token};
        pub use ::types::lockstep::LockstepReport;
        pub use ::types::processing::{
            LineProcessingTrace, ProcessingOutcome, ProcessingRequestKind, ProcessingReturn,
        };

        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct ProcessorBuildError {
            message: String,
        }

        impl ProcessorBuildError {
            fn new(message: impl Into<String>) -> Self {
                Self {
                    message: message.into(),
                }
            }
        }

        impl std::fmt::Display for ProcessorBuildError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(&self.message)
            }
        }

        impl std::error::Error for ProcessorBuildError {}

        #[derive(Debug, Clone)]
        pub struct ProcessorBuilder {
            execution_mode: super::ExecutionMode,
            cpu_id: String,
            dialect_override: Option<String>,
        }

        impl Default for ProcessorBuilder {
            fn default() -> Self {
                Self {
                    execution_mode: super::ExecutionMode::Rust,
                    cpu_id: String::new(),
                    dialect_override: None,
                }
            }
        }

        impl ProcessorBuilder {
            pub fn new() -> Self {
                Self::default()
            }

            pub fn execution_mode(mut self, execution_mode: super::ExecutionMode) -> Self {
                self.execution_mode = execution_mode;
                self
            }

            pub fn cpu_id(mut self, cpu_id: impl Into<String>) -> Self {
                self.cpu_id = cpu_id.into();
                self
            }

            pub fn dialect_override(mut self, dialect_override: impl Into<String>) -> Self {
                self.dialect_override = Some(dialect_override.into());
                self
            }

            pub fn build(self) -> Result<Processor, ProcessorBuildError> {
                let model = match self.execution_mode {
                    super::ExecutionMode::Rust => None,
                    super::ExecutionMode::Vm | super::ExecutionMode::Lockstep { .. } => {
                        if self.cpu_id.is_empty() {
                            return Err(ProcessorBuildError::new(
                                "cpu_id is required for VM or lockstep opasm processing",
                            ));
                        }
                        let registry = ::engine::build_default_asm_registry();
                        Some(
                            ::vm::vm_opasm::load_model_from_registry(&registry)
                                .map_err(|err| ProcessorBuildError::new(err.to_string()))?,
                        )
                    }
                };

                Ok(Processor {
                    execution_mode: self.execution_mode,
                    model,
                    cpu_id: self.cpu_id,
                    dialect_override: self.dialect_override,
                })
            }
        }

        pub struct Processor {
            execution_mode: super::ExecutionMode,
            model: Option<::vm::vm_opasm::HierarchyExecutionModel>,
            cpu_id: String,
            dialect_override: Option<String>,
        }

        impl Processor {
            pub fn builder() -> ProcessorBuilder {
                ProcessorBuilder::new()
            }

            fn statement_request<'a>(
                &'a self,
                line: &'a str,
                line_num: u32,
            ) -> StatementRequest<'a> {
                let request =
                    StatementRequest::new(line, line_num).with_execution_mode(self.execution_mode);
                if let Some(model) = self.model.as_ref() {
                    request.with_model(
                        model,
                        self.cpu_id.as_str(),
                        self.dialect_override.as_deref(),
                    )
                } else {
                    request
                }
            }

            pub fn tokenize_statement(
                &self,
                line: &str,
                line_num: u32,
            ) -> Result<TokenizedStatement, ParseError> {
                tokenize_statement(self.statement_request(line, line_num))
            }

            pub fn parse_statement(
                &self,
                line: &str,
                line_num: u32,
            ) -> Result<StatementParseResult, ParseError> {
                parse_statement(self.statement_request(line, line_num))
            }

            pub fn process_statement(
                &self,
                line: &str,
                line_num: u32,
            ) -> Result<StatementProcessResult, ParseError> {
                process_statement(self.statement_request(line, line_num), None)
            }
        }

        pub mod portable {
            pub use ::vm::portable_contract::{
                PortableLineAst, PortableOperatorKind, PortableSpan, PortableToken,
                PortableTokenKind,
            };

            pub use super::{
                LockstepReport, ParseError, Processor, ProcessorBuildError, ProcessorBuilder,
                StatementExprProcessor, StatementRequest,
            };
            pub use ::types::processing::LineProcessingTrace;

            #[derive(Debug, Clone, PartialEq, Eq)]
            pub struct TokenizedStatement {
                pub tokens: Vec<PortableToken>,
                pub end_span: PortableSpan,
                pub end_token_text: Option<String>,
            }

            #[derive(Debug, Clone, PartialEq, Eq)]
            pub struct StatementParseResult {
                pub ast: PortableLineAst,
                pub end_span: PortableSpan,
                pub end_token_text: Option<String>,
            }

            #[derive(Debug, Clone)]
            pub struct StatementProcessResult {
                pub parsed: StatementParseResult,
                pub trace: LineProcessingTrace,
                pub lockstep_report: LockstepReport,
            }

            pub fn tokenize_statement(
                request: StatementRequest<'_>,
            ) -> Result<TokenizedStatement, ParseError> {
                let tokenized = super::tokenize_statement(request)?;
                Ok(TokenizedStatement {
                    tokens: tokenized
                        .tokens
                        .into_iter()
                        .filter_map(PortableToken::from_core_token)
                        .collect(),
                    end_span: tokenized.end_span.into(),
                    end_token_text: tokenized.end_token_text,
                })
            }

            pub fn parse_statement(
                request: StatementRequest<'_>,
            ) -> Result<StatementParseResult, ParseError> {
                let parsed = super::parse_statement(request)?;
                Ok(StatementParseResult {
                    ast: PortableLineAst::from_core_line_ast(&parsed.ast),
                    end_span: parsed.end_span.into(),
                    end_token_text: parsed.end_token_text,
                })
            }

            pub fn process_statement(
                request: StatementRequest<'_>,
                expr_processor: Option<&mut dyn StatementExprProcessor>,
            ) -> Result<StatementProcessResult, ParseError> {
                let processed = super::process_statement(request, expr_processor)?;
                Ok(StatementProcessResult {
                    parsed: StatementParseResult {
                        ast: PortableLineAst::from_core_line_ast(&processed.parsed.ast),
                        end_span: processed.parsed.end_span.into(),
                        end_token_text: processed.parsed.end_token_text,
                    },
                    trace: processed.trace,
                    lockstep_report: processed.lockstep_report,
                })
            }

            pub fn tokenize_statement_with_processor(
                processor: &Processor,
                line: &str,
                line_num: u32,
            ) -> Result<TokenizedStatement, ParseError> {
                let tokenized = processor.tokenize_statement(line, line_num)?;
                Ok(TokenizedStatement {
                    tokens: tokenized
                        .tokens
                        .into_iter()
                        .filter_map(PortableToken::from_core_token)
                        .collect(),
                    end_span: tokenized.end_span.into(),
                    end_token_text: tokenized.end_token_text,
                })
            }

            pub fn parse_statement_with_processor(
                processor: &Processor,
                line: &str,
                line_num: u32,
            ) -> Result<StatementParseResult, ParseError> {
                let parsed = processor.parse_statement(line, line_num)?;
                Ok(StatementParseResult {
                    ast: PortableLineAst::from_core_line_ast(&parsed.ast),
                    end_span: parsed.end_span.into(),
                    end_token_text: parsed.end_token_text,
                })
            }

            pub fn process_statement_with_processor(
                processor: &Processor,
                line: &str,
                line_num: u32,
            ) -> Result<StatementProcessResult, ParseError> {
                let processed = processor.process_statement(line, line_num)?;
                Ok(StatementProcessResult {
                    parsed: StatementParseResult {
                        ast: PortableLineAst::from_core_line_ast(&processed.parsed.ast),
                        end_span: processed.parsed.end_span.into(),
                        end_token_text: processed.parsed.end_token_text,
                    },
                    trace: processed.trace,
                    lockstep_report: processed.lockstep_report,
                })
            }
        }
    }

    // Compatibility re-exports for the assembler-oriented root facade.
    // Canonical host imports for these symbols live under `libopforge::asm`.
    pub use crate::{
        assemble, prepare, AssembleOptions, Assembler, AssemblerBuilder, AssemblerConfig,
        AssemblerSession, AssemblerSessionBuilder, DiagnosticsOptions, ExecutionOptions,
        OutputOptions, OwnedAssemblerConfig, OwnedExecutionOptions, OwnedOutputOptions,
        OwnedSourceOptions, PrepareOptions, PreparedAssembly, PreparedAssemblySession,
        SourceOptions,
    };
}

/// Owned source configuration for hosts that keep assembly settings across runs.
///
/// Use this with [`AssemblerSessionBuilder`], [`AssemblerSession`], and
/// [`OwnedAssemblerConfig`] when the host wants to own strings, paths, and
/// provider handles instead of borrowing them for a single call.
///
/// Defaults keep the path lists empty, set `pp_macro_depth` to `32`, and leave
/// `source_provider` unset. If `output_base` is left empty, the public
/// `prepare()` and `assemble()` flows derive it from the root source path by
/// removing the source extension.
///
/// See the developer guide sections `4.1 output_base matters` and
/// `5.2 Owned or non-borrowing host integration` for the intended owned-host
/// workflow.
#[derive(Clone)]
#[non_exhaustive]
pub struct OwnedSourceOptions {
    pub output_base: String,
    pub defines: Vec<String>,
    pub include_paths: Vec<PathBuf>,
    pub module_paths: Vec<PathBuf>,
    pub pp_macro_depth: usize,
    pub source_provider: Option<Arc<dyn SourceProvider>>,
}

impl Default for OwnedSourceOptions {
    fn default() -> Self {
        Self {
            output_base: String::new(),
            defines: Vec::new(),
            include_paths: Vec::new(),
            module_paths: Vec::new(),
            pp_macro_depth: 32,
            source_provider: None,
        }
    }
}

/// Owned execution settings for reusable assembly sessions.
///
/// Use this when the host needs the owned-session API to remember execution
/// mode, CPU overrides, or package paths across multiple `prepare()`,
/// `assemble()`, or `check()` calls.
///
/// Defaults use [`ExecutionMode::Vm`], no CPU override, `1000`
/// `max_loop_iterations`, and no opasm package path.
///
/// See the developer guide sections `4.2 Execution mode defaults` and
/// `5.2 Owned or non-borrowing host integration`.
#[derive(Clone)]
#[non_exhaustive]
pub struct OwnedExecutionOptions {
    pub execution_mode: ExecutionMode,
    pub cpu_override: Option<String>,
    pub max_loop_iterations: u32,
    pub opasm_package_path: Option<PathBuf>,
    pub collect_runtime_traces: bool,
}

impl Default for OwnedExecutionOptions {
    fn default() -> Self {
        Self {
            execution_mode: ExecutionMode::Vm,
            cpu_override: None,
            max_loop_iterations: 1000,
            opasm_package_path: None,
            collect_runtime_traces: true,
        }
    }
}

/// Owned output settings for reusable assembly sessions.
///
/// Use this with [`AssemblerSession`] when artifact routing, output naming, or
/// sink configuration must outlive a single borrowed call.
///
/// Defaults keep outputs enabled, use [`OutputFormat::Text`], use
/// [`LabelOutputFormat::Vice`], leave all file overrides unset, and use
/// `"libopforge"` as the default header title.
///
/// See the developer guide sections `4.1 output_base matters`,
/// `4.3 check() versus assemble()`, and `5.2 Owned or non-borrowing host
/// integration`.
#[derive(Clone)]
#[non_exhaustive]
pub struct OwnedOutputOptions {
    pub out_dir: Option<PathBuf>,
    pub output_format: OutputFormat,
    pub go_addr: Option<String>,
    pub bin_specs: Vec<BinOutputSpec>,
    pub fill_byte: u8,
    pub fill_byte_set: bool,
    pub default_outputs: bool,
    pub labels_file: Option<PathBuf>,
    pub label_output_format: LabelOutputFormat,
    pub dependency_output: Option<DependencyOutputPolicy>,
    pub outfile_override: Option<String>,
    pub list_name_override: Option<String>,
    pub hex_name_override: Option<String>,
    pub srec_name_override: Option<String>,
    pub hunk_name_override: Option<String>,
    pub header_title: String,
    pub output_sink: Option<Arc<dyn OutputSink>>,
    pub no_outputs: bool,
}

impl Default for OwnedOutputOptions {
    fn default() -> Self {
        Self {
            out_dir: None,
            output_format: OutputFormat::Text,
            go_addr: None,
            bin_specs: Vec::new(),
            fill_byte: 0,
            fill_byte_set: false,
            default_outputs: true,
            labels_file: None,
            label_output_format: LabelOutputFormat::Vice,
            dependency_output: None,
            outfile_override: None,
            list_name_override: None,
            hex_name_override: None,
            srec_name_override: None,
            hunk_name_override: None,
            header_title: "libopforge".to_string(),
            output_sink: None,
            no_outputs: false,
        }
    }
}

/// Grouped owned configuration for [`AssemblerSession`] and
/// [`AssemblerSessionBuilder`].
///
/// This is the owned counterpart to [`AssemblerConfig`]. Prefer it when the
/// host naturally stores `String`, `PathBuf`, and `Arc` state between runs.
/// The default value is just the combination of the individual owned option
/// defaults for source, execution, output, and diagnostics.
///
/// See the developer guide sections `5.2 Owned or non-borrowing host
/// integration` and `5.4 Reusing prepared state`.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct OwnedAssemblerConfig {
    pub source: OwnedSourceOptions,
    pub execution: OwnedExecutionOptions,
    pub output: OwnedOutputOptions,
    pub diagnostics: DiagnosticsOptions,
}

impl OwnedAssemblerConfig {
    fn as_borrowed(&self) -> AssembleOptions<'_> {
        AssembleOptions {
            execution_mode: self.execution.execution_mode,
            output_base: self.source.output_base.as_str(),
            defines: &self.source.defines,
            include_paths: &self.source.include_paths,
            module_paths: &self.source.module_paths,
            pp_macro_depth: self.source.pp_macro_depth,
            cpu_override: self.execution.cpu_override.as_deref(),
            max_loop_iterations: self.execution.max_loop_iterations,
            opasm_package_path: self.execution.opasm_package_path.as_deref(),
            collect_runtime_traces: self.execution.collect_runtime_traces,
            out_dir: self.output.out_dir.as_deref(),
            debug_conditionals: self.diagnostics.debug_conditionals,
            tab_size: self.diagnostics.tab_size,
            output_format: self.output.output_format,
            go_addr: self.output.go_addr.as_deref(),
            bin_specs: &self.output.bin_specs,
            fill_byte: self.output.fill_byte,
            fill_byte_set: self.output.fill_byte_set,
            default_outputs: self.output.default_outputs,
            labels_file: self.output.labels_file.as_deref(),
            label_output_format: self.output.label_output_format,
            dependency_output: self.output.dependency_output.as_ref(),
            outfile_override: self.output.outfile_override.as_deref(),
            list_name_override: self.output.list_name_override.as_deref(),
            hex_name_override: self.output.hex_name_override.as_deref(),
            srec_name_override: self.output.srec_name_override.as_deref(),
            hunk_name_override: self.output.hunk_name_override.as_deref(),
            header_title: self.output.header_title.as_str(),
            source_provider: self.source.source_provider.as_deref(),
            output_sink: self.output.output_sink.as_deref(),
            no_outputs: self.output.no_outputs,
        }
    }
}

trait SharedBuilderSourceConfig {
    type OutputBase;
    type Defines;
    type IncludePaths;
    type ModulePaths;
    type SourceProvider;

    fn set_output_base(&mut self, output_base: Self::OutputBase);
    fn set_defines(&mut self, defines: Self::Defines);
    fn set_include_paths(&mut self, include_paths: Self::IncludePaths);
    fn set_module_paths(&mut self, module_paths: Self::ModulePaths);
    fn set_pp_macro_depth(&mut self, pp_macro_depth: usize);
    fn set_source_provider(&mut self, source_provider: Self::SourceProvider);
}

trait SharedBuilderExecutionConfig {
    type CpuOverride;
    type OpasmPackagePath;

    fn set_execution_mode(&mut self, execution_mode: ExecutionMode);
    fn set_cpu_override(&mut self, cpu_override: Self::CpuOverride);
    fn set_max_loop_iterations(&mut self, max_loop_iterations: u32);
    fn set_opasm_package_path(&mut self, opasm_package_path: Self::OpasmPackagePath);
    fn set_collect_runtime_traces(&mut self, collect_runtime_traces: bool);
}

trait SharedBuilderOutputConfig {
    type OutDir;
    type GoAddr;
    type BinSpecs;
    type LabelsFile;
    type DependencyOutput;
    type OutfileOverride;
    type ListNameOverride;
    type HexNameOverride;
    type SrecNameOverride;
    type HunkNameOverride;
    type HeaderTitle;
    type OutputSink;

    fn set_out_dir(&mut self, out_dir: Self::OutDir);
    fn set_output_format(&mut self, output_format: OutputFormat);
    fn set_go_addr(&mut self, go_addr: Self::GoAddr);
    fn set_bin_specs(&mut self, bin_specs: Self::BinSpecs);
    fn set_fill_byte(&mut self, fill_byte: u8);
    fn set_labels_file(&mut self, labels_file: Self::LabelsFile);
    fn set_dependency_output(&mut self, dependency_output: Self::DependencyOutput);
    fn set_outfile_override(&mut self, outfile_override: Self::OutfileOverride);
    fn set_list_name_override(&mut self, list_name_override: Self::ListNameOverride);
    fn set_hex_name_override(&mut self, hex_name_override: Self::HexNameOverride);
    fn set_srec_name_override(&mut self, srec_name_override: Self::SrecNameOverride);
    fn set_hunk_name_override(&mut self, hunk_name_override: Self::HunkNameOverride);
    fn set_label_output_format(&mut self, label_output_format: LabelOutputFormat);
    fn set_header_title(&mut self, header_title: Self::HeaderTitle);
    fn set_default_outputs(&mut self, default_outputs: bool);
    fn set_no_outputs(&mut self, no_outputs: bool);
    fn set_output_sink(&mut self, output_sink: Self::OutputSink);
}

trait SharedBuilderDiagnosticsConfig {
    fn set_debug_conditionals(&mut self, debug_conditionals: bool);
    fn set_tab_size(&mut self, tab_size: usize);
}

impl<'a> SharedBuilderSourceConfig for AssemblerConfig<'a> {
    type OutputBase = &'a str;
    type Defines = &'a [String];
    type IncludePaths = &'a [PathBuf];
    type ModulePaths = &'a [PathBuf];
    type SourceProvider = &'a dyn SourceProvider;

    fn set_output_base(&mut self, output_base: Self::OutputBase) {
        self.source.output_base = output_base;
    }

    fn set_defines(&mut self, defines: Self::Defines) {
        self.source.defines = defines;
    }

    fn set_include_paths(&mut self, include_paths: Self::IncludePaths) {
        self.source.include_paths = include_paths;
    }

    fn set_module_paths(&mut self, module_paths: Self::ModulePaths) {
        self.source.module_paths = module_paths;
    }

    fn set_pp_macro_depth(&mut self, pp_macro_depth: usize) {
        self.source.pp_macro_depth = pp_macro_depth;
    }

    fn set_source_provider(&mut self, source_provider: Self::SourceProvider) {
        self.source.source_provider = Some(source_provider);
    }
}

impl SharedBuilderSourceConfig for OwnedAssemblerConfig {
    type OutputBase = String;
    type Defines = Vec<String>;
    type IncludePaths = Vec<PathBuf>;
    type ModulePaths = Vec<PathBuf>;
    type SourceProvider = Arc<dyn SourceProvider>;

    fn set_output_base(&mut self, output_base: Self::OutputBase) {
        self.source.output_base = output_base;
    }

    fn set_defines(&mut self, defines: Self::Defines) {
        self.source.defines = defines;
    }

    fn set_include_paths(&mut self, include_paths: Self::IncludePaths) {
        self.source.include_paths = include_paths;
    }

    fn set_module_paths(&mut self, module_paths: Self::ModulePaths) {
        self.source.module_paths = module_paths;
    }

    fn set_pp_macro_depth(&mut self, pp_macro_depth: usize) {
        self.source.pp_macro_depth = pp_macro_depth;
    }

    fn set_source_provider(&mut self, source_provider: Self::SourceProvider) {
        self.source.source_provider = Some(source_provider);
    }
}

impl<'a> SharedBuilderExecutionConfig for AssemblerConfig<'a> {
    type CpuOverride = &'a str;
    type OpasmPackagePath = &'a Path;

    fn set_execution_mode(&mut self, execution_mode: ExecutionMode) {
        self.execution.execution_mode = execution_mode;
    }

    fn set_cpu_override(&mut self, cpu_override: Self::CpuOverride) {
        self.execution.cpu_override = Some(cpu_override);
    }

    fn set_max_loop_iterations(&mut self, max_loop_iterations: u32) {
        self.execution.max_loop_iterations = max_loop_iterations;
    }

    fn set_opasm_package_path(&mut self, opasm_package_path: Self::OpasmPackagePath) {
        self.execution.opasm_package_path = Some(opasm_package_path);
    }

    fn set_collect_runtime_traces(&mut self, collect_runtime_traces: bool) {
        self.execution.collect_runtime_traces = collect_runtime_traces;
    }
}

impl SharedBuilderExecutionConfig for OwnedAssemblerConfig {
    type CpuOverride = String;
    type OpasmPackagePath = PathBuf;

    fn set_execution_mode(&mut self, execution_mode: ExecutionMode) {
        self.execution.execution_mode = execution_mode;
    }

    fn set_cpu_override(&mut self, cpu_override: Self::CpuOverride) {
        self.execution.cpu_override = Some(cpu_override);
    }

    fn set_max_loop_iterations(&mut self, max_loop_iterations: u32) {
        self.execution.max_loop_iterations = max_loop_iterations;
    }

    fn set_opasm_package_path(&mut self, opasm_package_path: Self::OpasmPackagePath) {
        self.execution.opasm_package_path = Some(opasm_package_path);
    }

    fn set_collect_runtime_traces(&mut self, collect_runtime_traces: bool) {
        self.execution.collect_runtime_traces = collect_runtime_traces;
    }
}

impl<'a> SharedBuilderOutputConfig for AssemblerConfig<'a> {
    type OutDir = &'a Path;
    type GoAddr = &'a str;
    type BinSpecs = &'a [BinOutputSpec];
    type LabelsFile = &'a Path;
    type DependencyOutput = &'a DependencyOutputPolicy;
    type OutfileOverride = &'a str;
    type ListNameOverride = &'a str;
    type HexNameOverride = &'a str;
    type SrecNameOverride = &'a str;
    type HunkNameOverride = &'a str;
    type HeaderTitle = &'a str;
    type OutputSink = &'a dyn OutputSink;

    fn set_out_dir(&mut self, out_dir: Self::OutDir) {
        self.output.out_dir = Some(out_dir);
    }

    fn set_output_format(&mut self, output_format: OutputFormat) {
        self.output.output_format = output_format;
    }

    fn set_go_addr(&mut self, go_addr: Self::GoAddr) {
        self.output.go_addr = Some(go_addr);
    }

    fn set_bin_specs(&mut self, bin_specs: Self::BinSpecs) {
        self.output.bin_specs = bin_specs;
    }

    fn set_fill_byte(&mut self, fill_byte: u8) {
        self.output.fill_byte = fill_byte;
        self.output.fill_byte_set = true;
    }

    fn set_labels_file(&mut self, labels_file: Self::LabelsFile) {
        self.output.labels_file = Some(labels_file);
    }

    fn set_dependency_output(&mut self, dependency_output: Self::DependencyOutput) {
        self.output.dependency_output = Some(dependency_output);
    }

    fn set_outfile_override(&mut self, outfile_override: Self::OutfileOverride) {
        self.output.outfile_override = Some(outfile_override);
    }

    fn set_list_name_override(&mut self, list_name_override: Self::ListNameOverride) {
        self.output.list_name_override = Some(list_name_override);
    }

    fn set_hex_name_override(&mut self, hex_name_override: Self::HexNameOverride) {
        self.output.hex_name_override = Some(hex_name_override);
    }

    fn set_srec_name_override(&mut self, srec_name_override: Self::SrecNameOverride) {
        self.output.srec_name_override = Some(srec_name_override);
    }

    fn set_hunk_name_override(&mut self, hunk_name_override: Self::HunkNameOverride) {
        self.output.hunk_name_override = Some(hunk_name_override);
    }

    fn set_label_output_format(&mut self, label_output_format: LabelOutputFormat) {
        self.output.label_output_format = label_output_format;
    }

    fn set_header_title(&mut self, header_title: Self::HeaderTitle) {
        self.output.header_title = header_title;
    }

    fn set_default_outputs(&mut self, default_outputs: bool) {
        self.output.default_outputs = default_outputs;
    }

    fn set_no_outputs(&mut self, no_outputs: bool) {
        self.output.no_outputs = no_outputs;
    }

    fn set_output_sink(&mut self, output_sink: Self::OutputSink) {
        self.output.output_sink = Some(output_sink);
    }
}

impl SharedBuilderOutputConfig for OwnedAssemblerConfig {
    type OutDir = PathBuf;
    type GoAddr = String;
    type BinSpecs = Vec<BinOutputSpec>;
    type LabelsFile = PathBuf;
    type DependencyOutput = DependencyOutputPolicy;
    type OutfileOverride = String;
    type ListNameOverride = String;
    type HexNameOverride = String;
    type SrecNameOverride = String;
    type HunkNameOverride = String;
    type HeaderTitle = String;
    type OutputSink = Arc<dyn OutputSink>;

    fn set_out_dir(&mut self, out_dir: Self::OutDir) {
        self.output.out_dir = Some(out_dir);
    }

    fn set_output_format(&mut self, output_format: OutputFormat) {
        self.output.output_format = output_format;
    }

    fn set_go_addr(&mut self, go_addr: Self::GoAddr) {
        self.output.go_addr = Some(go_addr);
    }

    fn set_bin_specs(&mut self, bin_specs: Self::BinSpecs) {
        self.output.bin_specs = bin_specs;
    }

    fn set_fill_byte(&mut self, fill_byte: u8) {
        self.output.fill_byte = fill_byte;
        self.output.fill_byte_set = true;
    }

    fn set_labels_file(&mut self, labels_file: Self::LabelsFile) {
        self.output.labels_file = Some(labels_file);
    }

    fn set_dependency_output(&mut self, dependency_output: Self::DependencyOutput) {
        self.output.dependency_output = Some(dependency_output);
    }

    fn set_outfile_override(&mut self, outfile_override: Self::OutfileOverride) {
        self.output.outfile_override = Some(outfile_override);
    }

    fn set_list_name_override(&mut self, list_name_override: Self::ListNameOverride) {
        self.output.list_name_override = Some(list_name_override);
    }

    fn set_hex_name_override(&mut self, hex_name_override: Self::HexNameOverride) {
        self.output.hex_name_override = Some(hex_name_override);
    }

    fn set_srec_name_override(&mut self, srec_name_override: Self::SrecNameOverride) {
        self.output.srec_name_override = Some(srec_name_override);
    }

    fn set_hunk_name_override(&mut self, hunk_name_override: Self::HunkNameOverride) {
        self.output.hunk_name_override = Some(hunk_name_override);
    }

    fn set_label_output_format(&mut self, label_output_format: LabelOutputFormat) {
        self.output.label_output_format = label_output_format;
    }

    fn set_header_title(&mut self, header_title: Self::HeaderTitle) {
        self.output.header_title = header_title;
    }

    fn set_default_outputs(&mut self, default_outputs: bool) {
        self.output.default_outputs = default_outputs;
    }

    fn set_no_outputs(&mut self, no_outputs: bool) {
        self.output.no_outputs = no_outputs;
    }

    fn set_output_sink(&mut self, output_sink: Self::OutputSink) {
        self.output.output_sink = Some(output_sink);
    }
}

impl<'a> SharedBuilderDiagnosticsConfig for AssemblerConfig<'a> {
    fn set_debug_conditionals(&mut self, debug_conditionals: bool) {
        self.diagnostics.debug_conditionals = debug_conditionals;
    }

    fn set_tab_size(&mut self, tab_size: usize) {
        self.diagnostics.tab_size = Some(tab_size);
    }
}

impl SharedBuilderDiagnosticsConfig for OwnedAssemblerConfig {
    fn set_debug_conditionals(&mut self, debug_conditionals: bool) {
        self.diagnostics.debug_conditionals = debug_conditionals;
    }

    fn set_tab_size(&mut self, tab_size: usize) {
        self.diagnostics.tab_size = Some(tab_size);
    }
}

struct SharedBuilderMut<'a, C> {
    config: &'a mut C,
}

impl<'a, C> SharedBuilderMut<'a, C> {
    fn new(config: &'a mut C) -> Self {
        Self { config }
    }
}

impl<C> SharedBuilderMut<'_, C>
where
    C: SharedBuilderSourceConfig,
{
    fn output_base(&mut self, output_base: C::OutputBase) {
        self.config.set_output_base(output_base);
    }

    fn defines(&mut self, defines: C::Defines) {
        self.config.set_defines(defines);
    }

    fn include_paths(&mut self, include_paths: C::IncludePaths) {
        self.config.set_include_paths(include_paths);
    }

    fn module_paths(&mut self, module_paths: C::ModulePaths) {
        self.config.set_module_paths(module_paths);
    }

    fn pp_macro_depth(&mut self, pp_macro_depth: usize) {
        self.config.set_pp_macro_depth(pp_macro_depth);
    }

    fn source_provider(&mut self, source_provider: C::SourceProvider) {
        self.config.set_source_provider(source_provider);
    }
}

impl<C> SharedBuilderMut<'_, C>
where
    C: SharedBuilderExecutionConfig,
{
    fn execution_mode(&mut self, execution_mode: ExecutionMode) {
        self.config.set_execution_mode(execution_mode);
    }

    fn cpu_override(&mut self, cpu_override: C::CpuOverride) {
        self.config.set_cpu_override(cpu_override);
    }

    fn max_loop_iterations(&mut self, max_loop_iterations: u32) {
        self.config.set_max_loop_iterations(max_loop_iterations);
    }

    fn opasm_package_path(&mut self, opasm_package_path: C::OpasmPackagePath) {
        self.config.set_opasm_package_path(opasm_package_path);
    }

    fn collect_runtime_traces(&mut self, collect_runtime_traces: bool) {
        self.config
            .set_collect_runtime_traces(collect_runtime_traces);
    }
}

impl<C> SharedBuilderMut<'_, C>
where
    C: SharedBuilderOutputConfig,
{
    fn out_dir(&mut self, out_dir: C::OutDir) {
        self.config.set_out_dir(out_dir);
    }

    fn output_format(&mut self, output_format: OutputFormat) {
        self.config.set_output_format(output_format);
    }

    fn go_addr(&mut self, go_addr: C::GoAddr) {
        self.config.set_go_addr(go_addr);
    }

    fn bin_specs(&mut self, bin_specs: C::BinSpecs) {
        self.config.set_bin_specs(bin_specs);
    }

    fn fill_byte(&mut self, fill_byte: u8) {
        self.config.set_fill_byte(fill_byte);
    }

    fn labels_file(&mut self, labels_file: C::LabelsFile) {
        self.config.set_labels_file(labels_file);
    }

    fn dependency_output(&mut self, dependency_output: C::DependencyOutput) {
        self.config.set_dependency_output(dependency_output);
    }

    fn outfile_override(&mut self, outfile_override: C::OutfileOverride) {
        self.config.set_outfile_override(outfile_override);
    }

    fn list_name_override(&mut self, list_name_override: C::ListNameOverride) {
        self.config.set_list_name_override(list_name_override);
    }

    fn hex_name_override(&mut self, hex_name_override: C::HexNameOverride) {
        self.config.set_hex_name_override(hex_name_override);
    }

    fn srec_name_override(&mut self, srec_name_override: C::SrecNameOverride) {
        self.config.set_srec_name_override(srec_name_override);
    }

    fn hunk_name_override(&mut self, hunk_name_override: C::HunkNameOverride) {
        self.config.set_hunk_name_override(hunk_name_override);
    }

    fn label_output_format(&mut self, label_output_format: LabelOutputFormat) {
        self.config.set_label_output_format(label_output_format);
    }

    fn header_title(&mut self, header_title: C::HeaderTitle) {
        self.config.set_header_title(header_title);
    }

    fn default_outputs(&mut self, default_outputs: bool) {
        self.config.set_default_outputs(default_outputs);
    }

    fn no_outputs(&mut self, no_outputs: bool) {
        self.config.set_no_outputs(no_outputs);
    }

    fn output_sink(&mut self, output_sink: C::OutputSink) {
        self.config.set_output_sink(output_sink);
    }
}

impl<C> SharedBuilderMut<'_, C>
where
    C: SharedBuilderDiagnosticsConfig,
{
    fn debug_conditionals(&mut self, debug_conditionals: bool) {
        self.config.set_debug_conditionals(debug_conditionals);
    }

    fn tab_size(&mut self, tab_size: usize) {
        self.config.set_tab_size(tab_size);
    }
}

fn normalize_output_options_for_check(output: &mut OutputOptions<'_>) {
    output.default_outputs = false;
    output.out_dir = None;
    output.labels_file = None;
    output.dependency_output = None;
    output.outfile_override = None;
    output.list_name_override = None;
    output.hex_name_override = None;
    output.srec_name_override = None;
    output.hunk_name_override = None;
    output.bin_specs = &[];
    output.no_outputs = true;
}

fn normalize_owned_config_for_check(config: &mut OwnedAssemblerConfig) {
    config.output.default_outputs = false;
    config.output.out_dir = None;
    config.output.labels_file = None;
    config.output.dependency_output = None;
    config.output.outfile_override = None;
    config.output.list_name_override = None;
    config.output.hex_name_override = None;
    config.output.srec_name_override = None;
    config.output.hunk_name_override = None;
    config.output.bin_specs.clear();
    config.output.no_outputs = true;
}

fn derive_output_base(root_path: &Path) -> String {
    root_path.with_extension("").to_string_lossy().into_owned()
}

fn effective_output_base<'a>(root_path: &Path, output_base: &'a str) -> Cow<'a, str> {
    if output_base.is_empty() {
        Cow::Owned(derive_output_base(root_path))
    } else {
        Cow::Borrowed(output_base)
    }
}

struct EffectivePrepareConfig<'a> {
    output_base: Cow<'a, str>,
    include_roots: Vec<PathBuf>,
}

struct PublicPrepareRequest<'a> {
    root_path: &'a Path,
    output_base: &'a str,
    defines: &'a [String],
    include_paths: &'a [PathBuf],
    module_paths: &'a [PathBuf],
    pp_macro_depth: usize,
    cpu_override: Option<&'a str>,
    max_loop_iterations: u32,
    source_provider: Option<&'a dyn SourceProvider>,
}

struct PreparedAssemblyCore {
    registry: Arc<Mutex<::registry::AsmRegistry>>,
    cpu: ::registry::CpuType,
    max_loop_iterations: u32,
    root_module_id: String,
    expanded_lines: Vec<String>,
    source_map: SourceMap,
    dependency_files: Vec<PathBuf>,
    module_macro_names:
        std::collections::HashMap<String, std::collections::HashMap<String, SymbolVisibility>>,
}

struct PreparedExecutionCoreRef<'a> {
    output_base: &'a str,
    registry: &'a Arc<Mutex<::registry::AsmRegistry>>,
    cpu: ::registry::CpuType,
    max_loop_iterations: u32,
    root_module_id: &'a str,
    prepared_lines: &'a [String],
    source_map: &'a SourceMap,
    dependency_files: &'a [PathBuf],
    module_macro_names:
        &'a std::collections::HashMap<String, std::collections::HashMap<String, SymbolVisibility>>,
}

fn effective_prepare_config<'a>(
    root_path: &Path,
    output_base: &'a str,
    include_paths: &[PathBuf],
) -> EffectivePrepareConfig<'a> {
    EffectivePrepareConfig {
        output_base: effective_output_base(root_path, output_base),
        include_roots: engine::effective_include_paths_for_root(root_path, include_paths),
    }
}

fn prepare_public_assembly<'a>(
    request: PublicPrepareRequest<'a>,
) -> Result<(PreparedAssemblyCore, EffectivePrepareConfig<'a>), AsmRunError> {
    let effective = effective_prepare_config(
        request.root_path,
        request.output_base,
        request.include_paths,
    );
    let prepared = engine::prepare_assembly_session(engine::AssemblyPreparationRequest {
        root_path: request.root_path,
        defines: request.defines,
        include_roots: &effective.include_roots,
        module_paths: request.module_paths,
        pp_macro_depth: request.pp_macro_depth,
        registry: engine::build_default_asm_registry(),
        cpu_override: request.cpu_override,
        default_cpu: engine::default_cpu(),
        max_loop_iterations: request.max_loop_iterations,
        source_provider: request.source_provider,
    })?;
    let (session, root_module_id, expanded_lines, source_map, dependency_files, module_macro_names) =
        prepared.into_parts();
    let (cpu, registry, max_loop_iterations) = session.into_parts();

    Ok((
        PreparedAssemblyCore {
            registry: Arc::new(Mutex::new(registry)),
            cpu,
            max_loop_iterations,
            root_module_id,
            expanded_lines,
            source_map,
            dependency_files,
            module_macro_names,
        },
        effective,
    ))
}

fn run_public_prepared_assembly(
    prepared: PreparedExecutionCoreRef<'_>,
    options: AssembleOptions<'_>,
) -> Result<AsmRunReport, AsmRunError> {
    engine::run_prepared_assembly(engine::PreparedAssemblyExecutionRequest {
        input_base: prepared.output_base,
        cpu: prepared.cpu,
        registry: Arc::clone(prepared.registry),
        max_loop_iterations: prepared.max_loop_iterations,
        root_module_id: prepared.root_module_id.to_owned(),
        prepared_lines: prepared.prepared_lines.to_vec(),
        source_map: prepared.source_map.clone(),
        dependency_files: prepared.dependency_files.to_vec(),
        module_macro_names: prepared.module_macro_names.clone(),
        out_dir: options.out_dir,
        debug_conditionals: options.debug_conditionals,
        tab_size: options.tab_size,
        output_format: options.output_format,
        go_addr: options.go_addr,
        bin_specs: options.bin_specs,
        fill_byte: options.fill_byte,
        fill_byte_set: options.fill_byte_set,
        default_outputs: options.default_outputs,
        labels_file: options.labels_file,
        label_output_format: options.label_output_format,
        dependency_output: options.dependency_output,
        outfile_override: options.outfile_override,
        list_name_override: options.list_name_override,
        hex_name_override: options.hex_name_override,
        srec_name_override: options.srec_name_override,
        hunk_name_override: options.hunk_name_override,
        header_title: options.header_title,
        output_sink: options.output_sink,
        execution_mode: options.execution_mode,
        collect_runtime_traces: options.collect_runtime_traces,
        opasm_package_path: options.opasm_package_path,
        suppress_outputs: options.no_outputs,
    })
}

#[cfg(test)]
fn map_core_error_to_asm_kind(kind: opcore::CoreErrorKind) -> diagnostics::AsmErrorKind {
    match kind {
        opcore::CoreErrorKind::Tokenize
        | opcore::CoreErrorKind::Parse
        | opcore::CoreErrorKind::Statement
        | opcore::CoreErrorKind::Module
        | opcore::CoreErrorKind::Use
        | opcore::CoreErrorKind::Import => diagnostics::AsmErrorKind::Parser,
        opcore::CoreErrorKind::Expr | opcore::CoreErrorKind::Struct => {
            diagnostics::AsmErrorKind::Expression
        }
        opcore::CoreErrorKind::Conditional => diagnostics::AsmErrorKind::Conditional,
        opcore::CoreErrorKind::Preprocess => diagnostics::AsmErrorKind::Preprocess,
        opcore::CoreErrorKind::Macro
        | opcore::CoreErrorKind::Repetition
        | opcore::CoreErrorKind::Namespace
        | opcore::CoreErrorKind::Scope
        | opcore::CoreErrorKind::Segment => diagnostics::AsmErrorKind::Directive,
    }
}

#[cfg(test)]
fn map_core_error_to_workflow(err: opcore::CoreError) -> asm::AssemblerWorkflowError {
    let asm_error =
        diagnostics::AsmError::new(map_core_error_to_asm_kind(err.kind()), err.summary(), None);
    asm::AssemblerWorkflowError::Assemble(diagnostics::AsmRunError::new(
        asm_error,
        Vec::new(),
        Vec::new(),
    ))
}

#[cfg(test)]
fn map_processor_error_to_workflow(err: processing::ProcessorError) -> asm::AssemblerWorkflowError {
    match err.kind() {
        processing::ProcessorErrorKind::InvalidRequest => {
            asm::AssemblerWorkflowError::InvalidRequest(asm::InvalidRequestError::new(
                err.code(),
                err.summary(),
            ))
        }
        processing::ProcessorErrorKind::Io => {
            asm::AssemblerWorkflowError::Io(asm::HostIoError::new(err.code(), err.summary()))
        }
        processing::ProcessorErrorKind::Internal => asm::AssemblerWorkflowError::Internal(
            asm::InternalErrorReport::new(err.code(), err.summary()),
        ),
        processing::ProcessorErrorKind::ProcessorDiagnostic => {
            asm::AssemblerWorkflowError::Assemble(diagnostics::AsmRunError::new(
                diagnostics::AsmError::new(
                    diagnostics::AsmErrorKind::Assembler,
                    err.summary(),
                    None,
                ),
                Vec::new(),
                Vec::new(),
            ))
        }
    }
}

fn map_cli_asm_run_error_to_workflow(err: AsmRunError) -> asm::AssemblerWorkflowError {
    let summary = err.summary().to_string();
    let normalized = summary.to_ascii_lowercase();
    if normalized.contains("unsupported")
        || normalized.contains("unavailable")
        || normalized.contains("conflict")
        || normalized.contains("already")
    {
        asm::AssemblerWorkflowError::InvalidRequest(asm::InvalidRequestError::new(
            "asm.workflow.invalid_request",
            summary,
        ))
    } else {
        asm::AssemblerWorkflowError::InvalidArgument(asm::InvalidArgumentError::new(
            "asm.workflow.invalid_argument",
            summary,
        ))
    }
}

fn map_asm_run_error_to_workflow(err: AsmRunError) -> asm::AssemblerWorkflowError {
    match err.kind() {
        diagnostics::AsmErrorKind::Io => {
            asm::AssemblerWorkflowError::Io(asm::HostIoError::new("asm.workflow.io", err.summary()))
        }
        diagnostics::AsmErrorKind::Cli => map_cli_asm_run_error_to_workflow(err),
        _ => err.into(),
    }
}

/// Borrowed prepare-time configuration for the free [`prepare()`] helper.
///
/// Use this for a preparation-first workflow when borrowed slices and path
/// references are already available. If `output_base` is empty, the prepared
/// flow derives it from `root_path` and stores the resolved value for later
/// prepared execution.
///
/// Defaults use [`ExecutionMode::Vm`], no CPU override, `1000`
/// `max_loop_iterations`, empty define and path lists, and no source provider.
///
/// See the developer guide sections `4.1 output_base matters`,
/// `4.2 Execution mode defaults`, `4.4 prepare() versus one-shot execution`,
/// and `5.1 Borrowed host integration`.
#[derive(Clone)]
#[non_exhaustive]
pub struct PrepareOptions<'a> {
    pub execution_mode: ExecutionMode,
    pub output_base: &'a str,
    pub defines: &'a [String],
    pub include_paths: &'a [PathBuf],
    pub module_paths: &'a [PathBuf],
    pub pp_macro_depth: usize,
    pub cpu_override: Option<&'a str>,
    pub max_loop_iterations: u32,
    pub opasm_package_path: Option<&'a Path>,
    pub source_provider: Option<&'a dyn SourceProvider>,
}

impl<'a> Default for PrepareOptions<'a> {
    fn default() -> Self {
        Self {
            execution_mode: ExecutionMode::Vm,
            output_base: "",
            defines: &[],
            include_paths: &[],
            module_paths: &[],
            pp_macro_depth: 32,
            cpu_override: None,
            max_loop_iterations: 1000,
            opasm_package_path: None,
            source_provider: None,
        }
    }
}

/// Borrowed one-shot assembly configuration for the free [`assemble()`]
/// helper.
///
/// Use this when the host wants a direct assemble call without building an
/// [`Assembler`] or [`AssemblerSession`]. If `output_base` is empty, the public
/// assembly path derives it from `root_path` by removing the source extension.
///
/// Defaults use [`ExecutionMode::Vm`], [`OutputFormat::Text`],
/// [`LabelOutputFormat::Vice`], `default_outputs = true`, `no_outputs = false`,
/// and no file overrides, sink, or source provider.
///
/// See the developer guide sections `4.1 output_base matters`,
/// `4.2 Execution mode defaults`, `4.3 check() versus assemble()`, and
/// `5.1 Borrowed host integration`.
#[derive(Clone)]
#[non_exhaustive]
pub struct AssembleOptions<'a> {
    pub execution_mode: ExecutionMode,
    pub collect_runtime_traces: bool,
    pub output_base: &'a str,
    pub defines: &'a [String],
    pub include_paths: &'a [PathBuf],
    pub module_paths: &'a [PathBuf],
    pub pp_macro_depth: usize,
    pub cpu_override: Option<&'a str>,
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
    pub source_provider: Option<&'a dyn SourceProvider>,
    pub output_sink: Option<&'a dyn OutputSink>,
    pub no_outputs: bool,
}

impl<'a> Default for AssembleOptions<'a> {
    fn default() -> Self {
        Self {
            execution_mode: ExecutionMode::Vm,
            collect_runtime_traces: true,
            output_base: "",
            defines: &[],
            include_paths: &[],
            module_paths: &[],
            pp_macro_depth: 32,
            cpu_override: None,
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
            srec_name_override: None,
            hunk_name_override: None,
            header_title: "libopforge",
            source_provider: None,
            output_sink: None,
            no_outputs: false,
        }
    }
}

/// Borrowed source-only configuration used by [`AssemblerConfig`].
///
/// Prefer this when the host already owns the underlying strings and paths and
/// only needs to borrow them for a single assembly workflow.
/// Defaults keep `output_base` empty so it can be derived later, keep the path
/// and define lists empty, set `pp_macro_depth` to `32`, and leave
/// `source_provider` unset.
///
/// See the developer guide sections `4.1 output_base matters` and
/// `5.1 Borrowed host integration`.
#[derive(Clone)]
#[non_exhaustive]
pub struct SourceOptions<'a> {
    pub output_base: &'a str,
    pub defines: &'a [String],
    pub include_paths: &'a [PathBuf],
    pub module_paths: &'a [PathBuf],
    pub pp_macro_depth: usize,
    pub source_provider: Option<&'a dyn SourceProvider>,
}

impl<'a> Default for SourceOptions<'a> {
    fn default() -> Self {
        Self {
            output_base: "",
            defines: &[],
            include_paths: &[],
            module_paths: &[],
            pp_macro_depth: 32,
            source_provider: None,
        }
    }
}

/// Borrowed execution-only configuration used by [`AssemblerConfig`].
///
/// Prefer this when the host wants borrowed grouped configuration but still
/// needs to override execution mode, CPU selection, or opasm package source.
/// Defaults use [`ExecutionMode::Vm`], no CPU override, `1000`
/// `max_loop_iterations`, and no opasm package path.
///
/// See the developer guide sections `4.2 Execution mode defaults` and
/// `5.1 Borrowed host integration`.
#[derive(Clone)]
#[non_exhaustive]
pub struct ExecutionOptions<'a> {
    pub execution_mode: ExecutionMode,
    pub cpu_override: Option<&'a str>,
    pub max_loop_iterations: u32,
    pub opasm_package_path: Option<&'a Path>,
    pub collect_runtime_traces: bool,
}

impl<'a> Default for ExecutionOptions<'a> {
    fn default() -> Self {
        Self {
            execution_mode: ExecutionMode::Vm,
            cpu_override: None,
            max_loop_iterations: 1000,
            opasm_package_path: None,
            collect_runtime_traces: true,
        }
    }
}

/// Borrowed output-only configuration used by [`AssemblerConfig`].
///
/// Prefer this when the host wants borrowed grouped configuration for output
/// routing without committing to the one-shot [`AssembleOptions`] shape.
/// Defaults use [`OutputFormat::Text`], [`LabelOutputFormat::Vice`],
/// `default_outputs = true`, `no_outputs = false`, and no output-directory,
/// file, or sink overrides.
///
/// See the developer guide sections `4.1 output_base matters`,
/// `4.3 check() versus assemble()`, and `5.1 Borrowed host integration`.
#[derive(Clone)]
#[non_exhaustive]
pub struct OutputOptions<'a> {
    pub out_dir: Option<&'a Path>,
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
    pub no_outputs: bool,
}

impl<'a> Default for OutputOptions<'a> {
    fn default() -> Self {
        Self {
            out_dir: None,
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
            srec_name_override: None,
            hunk_name_override: None,
            header_title: "libopforge",
            output_sink: None,
            no_outputs: false,
        }
    }
}

/// Borrowed diagnostics configuration used by [`AssemblerConfig`].
///
/// Use this to opt into debug conditional traces or a specific tab width while
/// keeping the rest of the grouped config borrowed.
/// Defaults leave debug conditionals disabled and do not force a tab size.
///
/// See the developer guide sections `4.3 check() versus assemble()` and
/// `7. Diagnostics and metadata`.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct DiagnosticsOptions {
    pub debug_conditionals: bool,
    pub tab_size: Option<usize>,
}

/// Grouped borrowed configuration for [`Assembler`] and [`AssemblerBuilder`].
///
/// Prefer this when the host already owns the underlying config data and can
/// lend it to the facade for the lifetime of one builder or assembler value.
/// The default value is just the combination of the individual borrowed option
/// defaults for source, execution, output, and diagnostics.
///
/// See the developer guide sections `5.1 Borrowed host integration`,
/// `4.1 output_base matters`, and `4.2 Execution mode defaults`.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct AssemblerConfig<'a> {
    pub source: SourceOptions<'a>,
    pub execution: ExecutionOptions<'a>,
    pub output: OutputOptions<'a>,
    pub diagnostics: DiagnosticsOptions,
}

impl<'a> From<AssembleOptions<'a>> for AssemblerConfig<'a> {
    fn from(options: AssembleOptions<'a>) -> Self {
        Self {
            source: SourceOptions {
                output_base: options.output_base,
                defines: options.defines,
                include_paths: options.include_paths,
                module_paths: options.module_paths,
                pp_macro_depth: options.pp_macro_depth,
                source_provider: options.source_provider,
            },
            execution: ExecutionOptions {
                execution_mode: options.execution_mode,
                cpu_override: options.cpu_override,
                max_loop_iterations: options.max_loop_iterations,
                opasm_package_path: options.opasm_package_path,
                collect_runtime_traces: options.collect_runtime_traces,
            },
            output: OutputOptions {
                out_dir: options.out_dir,
                output_format: options.output_format,
                go_addr: options.go_addr,
                bin_specs: options.bin_specs,
                fill_byte: options.fill_byte,
                fill_byte_set: options.fill_byte_set,
                default_outputs: options.default_outputs,
                labels_file: options.labels_file,
                label_output_format: options.label_output_format,
                dependency_output: options.dependency_output,
                outfile_override: options.outfile_override,
                list_name_override: options.list_name_override,
                hex_name_override: options.hex_name_override,
                srec_name_override: options.srec_name_override,
                hunk_name_override: options.hunk_name_override,
                header_title: options.header_title,
                output_sink: options.output_sink,
                no_outputs: options.no_outputs,
            },
            diagnostics: DiagnosticsOptions {
                debug_conditionals: options.debug_conditionals,
                tab_size: options.tab_size,
            },
        }
    }
}

impl<'a> From<AssemblerConfig<'a>> for AssembleOptions<'a> {
    fn from(config: AssemblerConfig<'a>) -> Self {
        Self {
            execution_mode: config.execution.execution_mode,
            collect_runtime_traces: config.execution.collect_runtime_traces,
            output_base: config.source.output_base,
            defines: config.source.defines,
            include_paths: config.source.include_paths,
            module_paths: config.source.module_paths,
            pp_macro_depth: config.source.pp_macro_depth,
            cpu_override: config.execution.cpu_override,
            max_loop_iterations: config.execution.max_loop_iterations,
            opasm_package_path: config.execution.opasm_package_path,
            out_dir: config.output.out_dir,
            debug_conditionals: config.diagnostics.debug_conditionals,
            tab_size: config.diagnostics.tab_size,
            output_format: config.output.output_format,
            go_addr: config.output.go_addr,
            bin_specs: config.output.bin_specs,
            fill_byte: config.output.fill_byte,
            fill_byte_set: config.output.fill_byte_set,
            default_outputs: config.output.default_outputs,
            labels_file: config.output.labels_file,
            label_output_format: config.output.label_output_format,
            dependency_output: config.output.dependency_output,
            outfile_override: config.output.outfile_override,
            list_name_override: config.output.list_name_override,
            hex_name_override: config.output.hex_name_override,
            srec_name_override: config.output.srec_name_override,
            hunk_name_override: config.output.hunk_name_override,
            header_title: config.output.header_title,
            source_provider: config.source.source_provider,
            output_sink: config.output.output_sink,
            no_outputs: config.output.no_outputs,
        }
    }
}

/// Builder for the borrowed high-level [`Assembler`] workflow.
///
/// Use this when the host wants fluent configuration on top of borrowed data.
/// Prefer [`AssemblerConfig`] when grouped config construction is a better fit
/// than chained setters. This is the borrowed happy path when you want to
/// configure once and then call `build()`, `prepare()`, `assemble()`, or
/// `check()` immediately.
///
/// See the developer guide sections `5.1 Borrowed host integration` and
/// `4.4 prepare() versus one-shot execution`.
pub struct AssemblerBuilder<'a> {
    root_path: &'a Path,
    config: AssemblerConfig<'a>,
}

impl<'a> AssemblerBuilder<'a> {
    /// Starts a borrowed builder from an existing grouped config value.
    ///
    /// Use this when the host already assembled an [`AssemblerConfig`] and only
    /// wants the fluent builder entrypoint for the last few adjustments.
    pub fn with_config(root_path: &'a Path, config: AssemblerConfig<'a>) -> Self {
        Self { root_path, config }
    }

    /// Overrides the derived output base used for default artifact names.
    ///
    /// Use this when listing, hex, or label outputs must not be derived from
    /// `root_path`. See `documentation/libopforge-embedding-cookbook.md` and
    /// `documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs`.
    pub fn output_base(mut self, output_base: &'a str) -> Self {
        SharedBuilderMut::new(&mut self.config).output_base(output_base);
        self
    }

    /// Replaces the preprocessor define list for this borrowed request.
    ///
    /// Use this when the host already owns the full define set and wants to
    /// pass it through unchanged on a one-shot borrowed build.
    pub fn defines(mut self, defines: &'a [String]) -> Self {
        SharedBuilderMut::new(&mut self.config).defines(defines);
        self
    }

    /// Replaces the include search roots for source loading.
    ///
    /// Use this when the host controls include resolution directly instead of
    /// relying on the default path layout.
    pub fn include_paths(mut self, include_paths: &'a [PathBuf]) -> Self {
        SharedBuilderMut::new(&mut self.config).include_paths(include_paths);
        self
    }

    /// Replaces the module search roots for `.use` and related module loading.
    ///
    /// Use this when module resolution must follow host-owned search roots.
    pub fn module_paths(mut self, module_paths: &'a [PathBuf]) -> Self {
        SharedBuilderMut::new(&mut self.config).module_paths(module_paths);
        self
    }

    /// Overrides the preprocessor macro-expansion depth limit.
    ///
    /// Use this only when the host needs a non-default recursion budget for a
    /// known source corpus.
    pub fn pp_macro_depth(mut self, pp_macro_depth: usize) -> Self {
        SharedBuilderMut::new(&mut self.config).pp_macro_depth(pp_macro_depth);
        self
    }

    /// Supplies a borrowed source provider for non-filesystem or remapped input.
    ///
    /// Use this when the host already owns a `SourceProvider`, such as the
    /// memory-backed path in
    /// `documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs`.
    pub fn source_provider(mut self, source_provider: &'a dyn SourceProvider) -> Self {
        SharedBuilderMut::new(&mut self.config).source_provider(source_provider);
        self
    }

    /// Chooses the execution head for this borrowed request.
    ///
    /// Use this only when the request owns an explicit `Vm`, `Rust`, or
    /// `Lockstep` decision instead of using the stable default. See
    /// `documentation/libopforge-execution-modes-and-lockstep-guide.md` and
    /// `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`.
    pub fn execution_mode(mut self, execution_mode: ExecutionMode) -> Self {
        SharedBuilderMut::new(&mut self.config).execution_mode(execution_mode);
        self
    }

    /// Forces a target CPU instead of relying on source-driven resolution.
    ///
    /// Use this when the host owns CPU selection in project config or UI.
    pub fn cpu_override(mut self, cpu_override: &'a str) -> Self {
        SharedBuilderMut::new(&mut self.config).cpu_override(cpu_override);
        self
    }

    /// Overrides the loop-iteration safety budget for this request.
    ///
    /// Use this only when the host needs a deliberate non-default execution
    /// ceiling for trusted input.
    pub fn max_loop_iterations(mut self, max_loop_iterations: u32) -> Self {
        SharedBuilderMut::new(&mut self.config).max_loop_iterations(max_loop_iterations);
        self
    }

    /// Supplies an explicit opasm runtime package artifact path.
    ///
    /// Use this when the host manages VM runtime artifacts directly instead of
    /// relying on the default lookup path.
    pub fn opasm_package_path(mut self, opasm_package_path: &'a Path) -> Self {
        SharedBuilderMut::new(&mut self.config).opasm_package_path(opasm_package_path);
        self
    }

    /// Enables or disables runtime processing trace collection.
    ///
    /// Leave this enabled when the host needs `AsmRunReport` trace metadata.
    /// Turn it off for product assembly paths that only need outputs and
    /// diagnostics.
    pub fn collect_runtime_traces(mut self, collect_runtime_traces: bool) -> Self {
        SharedBuilderMut::new(&mut self.config).collect_runtime_traces(collect_runtime_traces);
        self
    }

    /// Sets the output directory for filesystem-backed artifact emission.
    ///
    /// Use this when artifact files should be emitted away from the source
    /// tree. See `documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs`.
    pub fn out_dir(mut self, out_dir: &'a Path) -> Self {
        SharedBuilderMut::new(&mut self.config).out_dir(out_dir);
        self
    }

    /// Chooses the primary output format for the high-level run.
    ///
    /// Use this when the host wants text, binary, or other output selection as
    /// part of the request shape.
    pub fn output_format(mut self, output_format: OutputFormat) -> Self {
        SharedBuilderMut::new(&mut self.config).output_format(output_format);
        self
    }

    /// Sets the optional go/start address override.
    ///
    /// Use this when the host owns the emitted start-address policy rather
    /// than leaving it to source-driven defaults.
    pub fn go_addr(mut self, go_addr: &'a str) -> Self {
        SharedBuilderMut::new(&mut self.config).go_addr(go_addr);
        self
    }

    /// Replaces the explicit binary-output specifications.
    ///
    /// Use this when the host needs one or more named binary ranges instead of
    /// the default output set.
    pub fn bin_specs(mut self, bin_specs: &'a [BinOutputSpec]) -> Self {
        SharedBuilderMut::new(&mut self.config).bin_specs(bin_specs);
        self
    }

    /// Sets the fill byte and marks it as explicitly configured.
    ///
    /// Use this when emitted binary ranges must use a host-chosen fill value
    /// rather than the default behavior.
    pub fn fill_byte(mut self, fill_byte: u8) -> Self {
        SharedBuilderMut::new(&mut self.config).fill_byte(fill_byte);
        self
    }

    /// Writes labels to an explicit labels-file path.
    ///
    /// Use this when the host wants label output as a separate artifact rather
    /// than relying only on the main report.
    pub fn labels_file(mut self, labels_file: &'a Path) -> Self {
        SharedBuilderMut::new(&mut self.config).labels_file(labels_file);
        self
    }

    /// Enables dependency-file emission for the configured output policy.
    ///
    /// Use this when the host wants make-style or related dependency output in
    /// addition to the main run report.
    pub fn dependency_output(mut self, dependency_output: &'a DependencyOutputPolicy) -> Self {
        SharedBuilderMut::new(&mut self.config).dependency_output(dependency_output);
        self
    }

    /// Overrides the primary output filename stem or path.
    ///
    /// Use this when artifact naming must follow the host's naming contract
    /// instead of the derived `output_base` path.
    pub fn outfile_override(mut self, outfile_override: &'a str) -> Self {
        SharedBuilderMut::new(&mut self.config).outfile_override(outfile_override);
        self
    }

    /// Overrides the default listing filename.
    ///
    /// Use this when listing output must use a host-chosen name distinct from
    /// the derived output base.
    pub fn list_name_override(mut self, list_name_override: &'a str) -> Self {
        SharedBuilderMut::new(&mut self.config).list_name_override(list_name_override);
        self
    }

    /// Overrides the default hex filename.
    ///
    /// Use this when hex output must use a host-chosen name distinct from the
    /// derived output base.
    pub fn hex_name_override(mut self, hex_name_override: &'a str) -> Self {
        SharedBuilderMut::new(&mut self.config).hex_name_override(hex_name_override);
        self
    }

    /// Overrides the default Motorola S-record filename.
    ///
    /// Use this when S-record output must use a host-chosen name distinct from
    /// the derived output base.
    pub fn srec_name_override(mut self, srec_name_override: &'a str) -> Self {
        SharedBuilderMut::new(&mut self.config).srec_name_override(srec_name_override);
        self
    }

    /// Overrides the default AmigaOS Hunk executable filename.
    ///
    /// Use this when Hunk output must use a host-chosen name distinct from the
    /// derived output base.
    pub fn hunk_name_override(mut self, hunk_name_override: &'a str) -> Self {
        SharedBuilderMut::new(&mut self.config).hunk_name_override(hunk_name_override);
        self
    }

    /// Chooses the label-file format when label emission is enabled.
    ///
    /// Use this when downstream tools expect a specific label syntax such as
    /// the VICE-oriented example in
    /// `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`.
    pub fn label_output_format(mut self, label_output_format: LabelOutputFormat) -> Self {
        SharedBuilderMut::new(&mut self.config).label_output_format(label_output_format);
        self
    }

    /// Sets the optional header title used by text-oriented outputs.
    ///
    /// Use this when the host wants a stable presentation title in listing or
    /// related text outputs.
    pub fn header_title(mut self, header_title: &'a str) -> Self {
        SharedBuilderMut::new(&mut self.config).header_title(header_title);
        self
    }

    /// Toggles the normal default artifact set for this request.
    ///
    /// Use this when the host wants the standard emitted artifacts on or off
    /// without overriding each output class individually.
    pub fn default_outputs(mut self, default_outputs: bool) -> Self {
        SharedBuilderMut::new(&mut self.config).default_outputs(default_outputs);
        self
    }

    /// Forces output suppression even when output-related settings are present.
    ///
    /// Use this when the host is intentionally building a diagnostics-first
    /// flow and wants the same no-output behavior that `check()` applies.
    pub fn no_outputs(mut self, no_outputs: bool) -> Self {
        SharedBuilderMut::new(&mut self.config).no_outputs(no_outputs);
        self
    }

    /// Enables or disables conditional-debug diagnostics for this request.
    ///
    /// Use this when the host wants extra conditional-expansion visibility in
    /// emitted diagnostics.
    pub fn debug_conditionals(mut self, debug_conditionals: bool) -> Self {
        SharedBuilderMut::new(&mut self.config).debug_conditionals(debug_conditionals);
        self
    }

    /// Sets the tab width used when rendering diagnostics.
    ///
    /// Use this when the host's editor or source presentation uses a known tab
    /// size and diagnostics should align to that view.
    pub fn tab_size(mut self, tab_size: usize) -> Self {
        SharedBuilderMut::new(&mut self.config).tab_size(tab_size);
        self
    }

    /// Supplies a borrowed output sink for non-filesystem or captured artifacts.
    ///
    /// Use this when the host already owns an `OutputSink`, such as the
    /// memory-backed path in
    /// `documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs`.
    pub fn output_sink(mut self, output_sink: &'a dyn OutputSink) -> Self {
        SharedBuilderMut::new(&mut self.config).output_sink(output_sink);
        self
    }

    /// Finishes configuration and returns the borrowed assembler handle.
    ///
    /// Prefer this over `assemble()` or `prepare()` when the host wants to keep
    /// the assembled borrowed workflow object around for repeated high-level
    /// calls.
    pub fn build(self) -> Assembler<'a> {
        Assembler::with_config(self.root_path, self.config)
    }

    /// Prepares the assembly once and returns reusable prepared state.
    ///
    /// This preserves the resolved `output_base`, expanded sources, and
    /// dependency metadata so the host can inspect or reuse the prepared state
    /// before later output emission.
    pub fn prepare(self) -> Result<PreparedAssembly<'a>, asm::AssemblerWorkflowError> {
        self.build().prepare()
    }

    /// Runs a one-shot high-level assembly using the builder configuration.
    ///
    /// Use this when the host wants the borrowed happy path without keeping the
    /// intermediate [`Assembler`] value.
    pub fn assemble(self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        self.build().assemble()
    }

    /// Validates without emitting outputs.
    ///
    /// This normalizes the output configuration so default outputs, labels,
    /// dependency output, bin specs, and output-file overrides are suppressed.
    /// Use it when the host wants diagnostics and dependency discovery without
    /// producing listing, hex, label, or other artifact files.
    pub fn check(self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        self.build().check()
    }
}

impl AssemblerSessionBuilder {
    /// Starts an owned builder with default grouped configuration.
    ///
    /// This is the easiest owned entrypoint for services, FFI-facing hosts, and
    /// other integrations that want to move `String`, `PathBuf`, and `Arc`
    /// values into the facade.
    pub fn new(root_path: impl Into<PathBuf>) -> Self {
        Self::with_config(root_path, OwnedAssemblerConfig::default())
    }

    /// Starts an owned builder from an existing grouped config value.
    ///
    /// Use this when the host already materialized an [`OwnedAssemblerConfig`]
    /// and wants the fluent builder only for final overrides.
    pub fn with_config(root_path: impl Into<PathBuf>, config: OwnedAssemblerConfig) -> Self {
        Self {
            root_path: root_path.into(),
            config,
        }
    }

    /// Overrides the derived output base used for default artifact names.
    ///
    /// Use this when the owned host wants stable artifact naming independent
    /// of `root_path`. See `documentation/libopforge-embedding-cookbook.md` and
    /// `documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs`.
    pub fn output_base(mut self, output_base: impl Into<String>) -> Self {
        SharedBuilderMut::new(&mut self.config).output_base(output_base.into());
        self
    }

    /// Replaces the preprocessor define list for this owned request.
    ///
    /// Use this when the host already owns the full define set as session
    /// state and wants to move it into the facade.
    pub fn defines(mut self, defines: impl Into<Vec<String>>) -> Self {
        SharedBuilderMut::new(&mut self.config).defines(defines.into());
        self
    }

    /// Replaces the include search roots for source loading.
    ///
    /// Use this when the owned host controls include resolution directly.
    pub fn include_paths(mut self, include_paths: impl Into<Vec<PathBuf>>) -> Self {
        SharedBuilderMut::new(&mut self.config).include_paths(include_paths.into());
        self
    }

    /// Replaces the module search roots for `.use` and related module loading.
    ///
    /// Use this when module resolution must follow host-owned search roots.
    pub fn module_paths(mut self, module_paths: impl Into<Vec<PathBuf>>) -> Self {
        SharedBuilderMut::new(&mut self.config).module_paths(module_paths.into());
        self
    }

    /// Overrides the preprocessor macro-expansion depth limit.
    ///
    /// Use this only when the host needs a deliberate non-default recursion
    /// budget for trusted inputs.
    pub fn pp_macro_depth(mut self, pp_macro_depth: usize) -> Self {
        SharedBuilderMut::new(&mut self.config).pp_macro_depth(pp_macro_depth);
        self
    }

    /// Chooses the execution head for this owned request.
    ///
    /// Use this only when the session owns an explicit `Vm`, `Rust`, or
    /// `Lockstep` decision instead of the stable default. See
    /// `documentation/libopforge-execution-modes-and-lockstep-guide.md` and
    /// `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`.
    pub fn execution_mode(mut self, execution_mode: ExecutionMode) -> Self {
        SharedBuilderMut::new(&mut self.config).execution_mode(execution_mode);
        self
    }

    /// Forces a target CPU instead of relying on source-driven resolution.
    ///
    /// Use this when CPU choice lives in project settings, service state, or a
    /// host UI rather than the source text.
    pub fn cpu_override(mut self, cpu_override: impl Into<String>) -> Self {
        SharedBuilderMut::new(&mut self.config).cpu_override(cpu_override.into());
        self
    }

    /// Overrides the loop-iteration safety budget for this owned request.
    ///
    /// Use this only when the host needs a deliberate non-default execution
    /// ceiling for trusted input.
    pub fn max_loop_iterations(mut self, max_loop_iterations: u32) -> Self {
        SharedBuilderMut::new(&mut self.config).max_loop_iterations(max_loop_iterations);
        self
    }

    /// Supplies an explicit opasm runtime package artifact path.
    ///
    /// Use this when the host manages VM runtime artifacts directly instead of
    /// relying on the default lookup path.
    pub fn opasm_package_path(mut self, opasm_package_path: impl Into<PathBuf>) -> Self {
        SharedBuilderMut::new(&mut self.config).opasm_package_path(opasm_package_path.into());
        self
    }

    /// Enables or disables runtime processing trace collection.
    ///
    /// Leave this enabled when the host needs `AsmRunReport` trace metadata.
    /// Turn it off for product assembly paths that only need outputs and
    /// diagnostics.
    pub fn collect_runtime_traces(mut self, collect_runtime_traces: bool) -> Self {
        SharedBuilderMut::new(&mut self.config).collect_runtime_traces(collect_runtime_traces);
        self
    }

    /// Sets the output directory for filesystem-backed artifact emission.
    ///
    /// Use this when emitted files should land outside the source tree. See
    /// `documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs`.
    pub fn out_dir(mut self, out_dir: impl Into<PathBuf>) -> Self {
        SharedBuilderMut::new(&mut self.config).out_dir(out_dir.into());
        self
    }

    /// Chooses the primary output format for the owned session.
    ///
    /// Use this when the host wants output selection to be part of the stored
    /// request shape.
    pub fn output_format(mut self, output_format: OutputFormat) -> Self {
        SharedBuilderMut::new(&mut self.config).output_format(output_format);
        self
    }

    /// Sets the optional go/start address override.
    ///
    /// Use this when the host owns emitted start-address policy directly.
    pub fn go_addr(mut self, go_addr: impl Into<String>) -> Self {
        SharedBuilderMut::new(&mut self.config).go_addr(go_addr.into());
        self
    }

    /// Replaces the explicit binary-output specifications.
    ///
    /// Use this when the host needs named binary ranges instead of the default
    /// output set.
    pub fn bin_specs(mut self, bin_specs: impl Into<Vec<BinOutputSpec>>) -> Self {
        SharedBuilderMut::new(&mut self.config).bin_specs(bin_specs.into());
        self
    }

    /// Sets the fill byte and marks it as explicitly configured.
    ///
    /// Use this when emitted binary ranges must use a host-chosen fill value.
    pub fn fill_byte(mut self, fill_byte: u8) -> Self {
        SharedBuilderMut::new(&mut self.config).fill_byte(fill_byte);
        self
    }

    /// Writes labels to an explicit labels-file path.
    ///
    /// Use this when label output should be emitted as a separate artifact.
    pub fn labels_file(mut self, labels_file: impl Into<PathBuf>) -> Self {
        SharedBuilderMut::new(&mut self.config).labels_file(labels_file.into());
        self
    }

    /// Enables dependency-file emission for the configured output policy.
    ///
    /// Use this when the host wants make-style or related dependency output in
    /// addition to the main run report.
    pub fn dependency_output(mut self, dependency_output: DependencyOutputPolicy) -> Self {
        SharedBuilderMut::new(&mut self.config).dependency_output(dependency_output);
        self
    }

    /// Overrides the primary output filename stem or path.
    ///
    /// Use this when artifact naming must follow the host's naming contract
    /// rather than the derived `output_base` path.
    pub fn outfile_override(mut self, outfile_override: impl Into<String>) -> Self {
        SharedBuilderMut::new(&mut self.config).outfile_override(outfile_override.into());
        self
    }

    /// Overrides the default listing filename.
    ///
    /// Use this when listing output must use a host-chosen name distinct from
    /// the derived output base.
    pub fn list_name_override(mut self, list_name_override: impl Into<String>) -> Self {
        SharedBuilderMut::new(&mut self.config).list_name_override(list_name_override.into());
        self
    }

    /// Overrides the default hex filename.
    ///
    /// Use this when hex output must use a host-chosen name distinct from the
    /// derived output base.
    pub fn hex_name_override(mut self, hex_name_override: impl Into<String>) -> Self {
        SharedBuilderMut::new(&mut self.config).hex_name_override(hex_name_override.into());
        self
    }

    /// Overrides the default Motorola S-record filename.
    ///
    /// Use this when S-record output must use a host-chosen name distinct from
    /// the derived output base.
    pub fn srec_name_override(mut self, srec_name_override: impl Into<String>) -> Self {
        SharedBuilderMut::new(&mut self.config).srec_name_override(srec_name_override.into());
        self
    }

    /// Overrides the default AmigaOS Hunk executable filename.
    ///
    /// Use this when Hunk output must use a host-chosen name distinct from the
    /// derived output base.
    pub fn hunk_name_override(mut self, hunk_name_override: impl Into<String>) -> Self {
        SharedBuilderMut::new(&mut self.config).hunk_name_override(hunk_name_override.into());
        self
    }

    /// Chooses the label-file format when label emission is enabled.
    ///
    /// Use this when downstream tools expect a specific label syntax such as
    /// the VICE-oriented example in
    /// `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`.
    pub fn label_output_format(mut self, label_output_format: LabelOutputFormat) -> Self {
        SharedBuilderMut::new(&mut self.config).label_output_format(label_output_format);
        self
    }

    /// Sets the optional header title used by text-oriented outputs.
    ///
    /// Use this when the host wants a stable presentation title in listing or
    /// related text outputs.
    pub fn header_title(mut self, header_title: impl Into<String>) -> Self {
        SharedBuilderMut::new(&mut self.config).header_title(header_title.into());
        self
    }

    /// Toggles the normal default artifact set for this owned request.
    ///
    /// Use this when the host wants the standard emitted artifacts on or off
    /// without overriding each output class individually.
    pub fn default_outputs(mut self, default_outputs: bool) -> Self {
        SharedBuilderMut::new(&mut self.config).default_outputs(default_outputs);
        self
    }

    /// Forces output suppression even when output-related settings are present.
    ///
    /// Use this when the host is intentionally building a diagnostics-first
    /// flow and wants the same no-output behavior that `check()` applies.
    pub fn no_outputs(mut self, no_outputs: bool) -> Self {
        SharedBuilderMut::new(&mut self.config).no_outputs(no_outputs);
        self
    }

    /// Enables or disables conditional-debug diagnostics for this request.
    ///
    /// Use this when the host wants extra conditional-expansion visibility in
    /// emitted diagnostics.
    pub fn debug_conditionals(mut self, debug_conditionals: bool) -> Self {
        SharedBuilderMut::new(&mut self.config).debug_conditionals(debug_conditionals);
        self
    }

    /// Sets the tab width used when rendering diagnostics.
    ///
    /// Use this when the host's editor or source presentation uses a known tab
    /// size and diagnostics should align to that view.
    pub fn tab_size(mut self, tab_size: usize) -> Self {
        SharedBuilderMut::new(&mut self.config).tab_size(tab_size);
        self
    }

    /// Moves a concrete source provider into the owned session.
    ///
    /// Use this when the host wants the session to own the provider directly,
    /// such as the in-memory path in
    /// `documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs`.
    pub fn source_provider<T>(mut self, source_provider: T) -> Self
    where
        T: SourceProvider + 'static,
    {
        SharedBuilderMut::new(&mut self.config).source_provider(Arc::new(source_provider));
        self
    }

    /// Reuses an existing shared `Arc<dyn SourceProvider>` in the owned session.
    ///
    /// Use this when the host already shares one provider across workers or
    /// long-lived sessions and does not want another allocation boundary.
    pub fn source_provider_arc(mut self, source_provider: Arc<dyn SourceProvider>) -> Self {
        SharedBuilderMut::new(&mut self.config).source_provider(source_provider);
        self
    }

    /// Moves a concrete output sink into the owned session.
    ///
    /// Use this when the host wants the session to own the sink directly, such
    /// as the in-memory path in
    /// `documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs`.
    pub fn output_sink<T>(mut self, output_sink: T) -> Self
    where
        T: OutputSink + 'static,
    {
        SharedBuilderMut::new(&mut self.config).output_sink(Arc::new(output_sink));
        self
    }

    /// Reuses an existing shared `Arc<dyn OutputSink>` in the owned session.
    ///
    /// Use this when the host already shares one sink across workers or
    /// long-lived sessions and wants the session to adopt that shared handle.
    pub fn output_sink_arc(mut self, output_sink: Arc<dyn OutputSink>) -> Self {
        SharedBuilderMut::new(&mut self.config).output_sink(output_sink);
        self
    }

    /// Finishes configuration and returns the owned-session handle.
    ///
    /// Prefer this when the host wants to retain the session and call
    /// `prepare()`, `assemble()`, or `check()` multiple times with the same
    /// owned configuration.
    pub fn build(self) -> AssemblerSession {
        AssemblerSession {
            root_path: self.root_path,
            config: self.config,
        }
    }

    /// Prepares the session once and returns reusable owned prepared state.
    ///
    /// This is the owned reuse path described in the developer guide section
    /// `5.4 Reusing prepared state`. The prepared session keeps the resolved
    /// `output_base`, expanded lines, source map, and dependency metadata for
    /// later `assemble()` or `check()` calls.
    pub fn prepare(self) -> Result<PreparedAssemblySession, asm::AssemblerWorkflowError> {
        self.build().prepare()
    }

    /// Runs a one-shot high-level assembly using the owned configuration.
    ///
    /// Use this when the host wants owned ergonomics but does not need to keep
    /// the intermediate [`AssemblerSession`] or prepared state.
    pub fn assemble(self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        self.build().assemble()
    }

    /// Validates without emitting outputs.
    ///
    /// This normalizes the output configuration so default outputs, labels,
    /// dependency output, bin specs, and output-file overrides are suppressed.
    /// Use it when the host wants owned configuration and full validation
    /// without producing artifacts on disk or in the configured sink.
    pub fn check(self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        self.build().check()
    }
}

/// Borrowed high-level assembler entrypoint.
///
/// Use this when the host can borrow configuration for the duration of one
/// workflow or one prepared session. Prefer [`AssemblerSession`] when the host
/// needs owned configuration that survives independently of borrowed inputs.
/// `Assembler` is the main borrowed facade type for the high-level happy path:
/// create it with [`Assembler::new`] or [`Assembler::with_config`], then call
/// `assemble()`, `prepare()`, or `check()`.
///
/// See the developer guide sections `5.1 Borrowed host integration` and
/// `4.4 prepare() versus one-shot execution`.
pub struct Assembler<'a> {
    root_path: &'a Path,
    config: AssemblerConfig<'a>,
}

/// Builder for the owned high-level [`AssemblerSession`] workflow.
///
/// Prefer this when the host wants fluent setters but needs owned `String`,
/// `PathBuf`, and `Arc` values rather than borrowed references. This is the
/// owned happy path before choosing whether to keep an [`AssemblerSession`],
/// emit outputs immediately, or prepare reusable state.
///
/// See the developer guide sections `5.2 Owned or non-borrowing host
/// integration` and `5.4 Reusing prepared state`.
pub struct AssemblerSessionBuilder {
    root_path: PathBuf,
    config: OwnedAssemblerConfig,
}

#[derive(Clone)]
/// Owned high-level assembler session.
///
/// Use this when configuration must be retained and reused across multiple
/// prepare, assemble, or check calls without keeping borrowed inputs alive.
/// This is the stable owned counterpart to [`Assembler`] for long-lived host
/// integrations.
///
/// See the developer guide sections `5.2 Owned or non-borrowing host
/// integration` and `5.4 Reusing prepared state`.
pub struct AssemblerSession {
    root_path: PathBuf,
    config: OwnedAssemblerConfig,
}

#[derive(Clone)]
/// Prepared owned session state returned by [`AssemblerSession::prepare`].
///
/// This preserves the expanded module graph, source map, dependency files, and
/// resolved output base so the host can inspect metadata before later
/// `assemble()` or `check()` calls. Reuse this when preparation is expensive
/// relative to repeated execution or validation.
///
/// See the developer guide sections `4.4 prepare() versus one-shot execution`
/// and `5.4 Reusing prepared state`.
pub struct PreparedAssemblySession {
    root_path: PathBuf,
    config: OwnedAssemblerConfig,
    registry: Arc<Mutex<::registry::AsmRegistry>>,
    cpu: ::registry::CpuType,
    max_loop_iterations: u32,
    root_module_id: String,
    expanded_lines: Vec<String>,
    source_map: SourceMap,
    dependency_files: Vec<PathBuf>,
    module_macro_names:
        std::collections::HashMap<String, std::collections::HashMap<String, SymbolVisibility>>,
}

#[derive(Clone)]
/// Prepared borrowed assembly state returned by [`Assembler::prepare`] or the
/// free [`prepare()`] helper.
///
/// Use this when the host wants to inspect preparation metadata such as the
/// root module, resolved CPU, source map, or dependency files before deciding
/// whether to emit outputs. The prepared value retains the resolved
/// `output_base` so later borrowed `assemble()` or `check()` calls stay aligned
/// with the preparation step.
///
/// See the developer guide sections `4.4 prepare() versus one-shot execution`
/// and `5.4 Reusing prepared state`.
pub struct PreparedAssembly<'a> {
    root_path: &'a Path,
    config: AssemblerConfig<'a>,
    resolved_output_base: Option<String>,
    registry: Arc<Mutex<::registry::AsmRegistry>>,
    cpu: ::registry::CpuType,
    max_loop_iterations: u32,
    root_module_id: String,
    expanded_lines: Vec<String>,
    source_map: SourceMap,
    dependency_files: Vec<PathBuf>,
    module_macro_names:
        std::collections::HashMap<String, std::collections::HashMap<String, SymbolVisibility>>,
}

impl<'a> Assembler<'a> {
    /// Creates a borrowed assembler with default grouped configuration.
    ///
    /// Use this when the root path is known and the rest of the borrowed
    /// configuration can be applied through the returned value or its builder.
    pub fn new(root_path: &'a Path) -> Self {
        Self {
            root_path,
            config: AssemblerConfig::default(),
        }
    }

    /// Starts a borrowed builder with default grouped configuration.
    ///
    /// Prefer this over [`Assembler::new`] when the host wants the fluent
    /// builder path from the start.
    pub fn builder(root_path: &'a Path) -> AssemblerBuilder<'a> {
        AssemblerBuilder::with_config(root_path, AssemblerConfig::default())
    }

    /// Creates a borrowed assembler from an existing grouped config value.
    ///
    /// Use this when the host already has an [`AssemblerConfig`] and wants the
    /// stable borrowed workflow handle directly.
    pub fn with_config<T>(root_path: &'a Path, config: T) -> Self
    where
        T: Into<AssemblerConfig<'a>>,
    {
        Self {
            root_path,
            config: config.into(),
        }
    }

    /// Returns the root source path used for preparation and assembly.
    pub fn root_path(&self) -> &'a Path {
        self.root_path
    }

    /// Returns the current grouped borrowed configuration.
    pub fn config(&self) -> &AssemblerConfig<'a> {
        &self.config
    }

    /// Returns mutable access to the grouped borrowed configuration.
    pub fn config_mut(&mut self) -> &mut AssemblerConfig<'a> {
        &mut self.config
    }

    /// Runs a one-shot high-level assembly.
    ///
    /// This is the direct borrowed execution path when the host wants outputs
    /// now and does not need to retain prepared state.
    pub fn assemble(&self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        assemble(self.root_path, self.config.clone().into())
    }

    /// Prepares the assembly and returns reusable prepared state.
    ///
    /// When `output_base` is omitted in the config, the prepared value stores
    /// the root-path-derived base so later prepared execution uses the same
    /// resolved path. Use this when the host needs to inspect metadata or reuse
    /// the expanded assembly state before deciding whether to emit outputs.
    pub fn prepare(&self) -> Result<PreparedAssembly<'a>, asm::AssemblerWorkflowError> {
        let (prepared, effective) = prepare_public_assembly(PublicPrepareRequest {
            root_path: self.root_path,
            output_base: self.config.source.output_base,
            defines: self.config.source.defines,
            include_paths: self.config.source.include_paths,
            module_paths: self.config.source.module_paths,
            pp_macro_depth: self.config.source.pp_macro_depth,
            cpu_override: self.config.execution.cpu_override,
            max_loop_iterations: self.config.execution.max_loop_iterations,
            source_provider: self.config.source.source_provider,
        })
        .map_err(map_asm_run_error_to_workflow)?;
        Ok(PreparedAssembly {
            root_path: self.root_path,
            config: self.config.clone(),
            resolved_output_base: Some(effective.output_base.into_owned()),
            registry: prepared.registry,
            cpu: prepared.cpu,
            max_loop_iterations: prepared.max_loop_iterations,
            root_module_id: prepared.root_module_id,
            expanded_lines: prepared.expanded_lines,
            source_map: prepared.source_map,
            dependency_files: prepared.dependency_files,
            module_macro_names: prepared.module_macro_names,
        })
    }

    /// Validates without emitting outputs.
    ///
    /// This suppresses default outputs, labels, dependency output, bin specs,
    /// and output-file overrides before running the high-level workflow. Use it
    /// when the host wants the same high-level pipeline as `assemble()` but with
    /// artifact emission suppressed.
    pub fn check(&self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        let mut config = self.config.clone();
        normalize_output_options_for_check(&mut config.output);
        assemble(self.root_path, config.into())
    }
}

impl AssemblerSession {
    /// Starts an owned builder with default grouped configuration.
    ///
    /// Prefer this for owned or non-borrowing integrations that want the
    /// fluent builder happy path. See
    /// `documentation/libopforge-embedding-cookbook.md`.
    pub fn builder(root_path: impl Into<PathBuf>) -> AssemblerSessionBuilder {
        AssemblerSessionBuilder::new(root_path)
    }

    /// Creates an owned session from an existing grouped config value.
    ///
    /// Use this when the host already has an [`OwnedAssemblerConfig`] and wants
    /// the stable owned workflow handle directly. See
    /// `documentation/libopforge-embedding-cookbook.md`.
    pub fn with_config(root_path: impl Into<PathBuf>, config: OwnedAssemblerConfig) -> Self {
        AssemblerSessionBuilder::with_config(root_path, config).build()
    }

    /// Returns the root source path used for preparation and assembly.
    pub fn root_path(&self) -> &Path {
        self.root_path.as_path()
    }

    /// Returns the current grouped owned configuration.
    pub fn config(&self) -> &OwnedAssemblerConfig {
        &self.config
    }

    /// Runs a one-shot high-level assembly.
    ///
    /// This is the direct owned execution path when the host wants outputs now
    /// and does not need to retain prepared state. See
    /// `documentation/libopforge-embedding-cookbook.md`.
    pub fn assemble(&self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        assemble_raw(self.root_path.as_path(), self.config.as_borrowed())
            .map_err(map_asm_run_error_to_workflow)
    }

    /// Prepares the assembly and returns reusable owned prepared state.
    ///
    /// When `output_base` is omitted in the config, the prepared session stores
    /// the root-path-derived base so later prepared execution uses the same
    /// resolved path. Use this when the host needs reusable prepared state that
    /// is independent of borrowed input lifetimes. See
    /// `documentation/libopforge-embedding-cookbook.md` and
    /// `documentation/libopforge-developer-guide-examples/libopforge_prepared.rs`.
    pub fn prepare(&self) -> Result<PreparedAssemblySession, asm::AssemblerWorkflowError> {
        let mut config = self.config.clone();
        let (prepared, resolved_input_base) = {
            let borrowed = config.as_borrowed();
            let (prepared, effective) = prepare_public_assembly(PublicPrepareRequest {
                root_path: self.root_path.as_path(),
                output_base: borrowed.output_base,
                defines: borrowed.defines,
                include_paths: borrowed.include_paths,
                module_paths: borrowed.module_paths,
                pp_macro_depth: borrowed.pp_macro_depth,
                cpu_override: borrowed.cpu_override,
                max_loop_iterations: borrowed.max_loop_iterations,
                source_provider: borrowed.source_provider,
            })
            .map_err(map_asm_run_error_to_workflow)?;
            (prepared, effective.output_base.into_owned())
        };
        if config.source.output_base.is_empty() {
            config.source.output_base = resolved_input_base;
        }
        Ok(PreparedAssemblySession {
            root_path: self.root_path.clone(),
            config,
            registry: prepared.registry,
            cpu: prepared.cpu,
            max_loop_iterations: prepared.max_loop_iterations,
            root_module_id: prepared.root_module_id,
            expanded_lines: prepared.expanded_lines,
            source_map: prepared.source_map,
            dependency_files: prepared.dependency_files,
            module_macro_names: prepared.module_macro_names,
        })
    }

    /// Validates without emitting outputs.
    ///
    /// This suppresses default outputs, labels, dependency output, bin specs,
    /// and output-file overrides before running the high-level workflow. Use it
    /// when the host wants the owned-session path but only needs diagnostics and
    /// metadata. See `documentation/libopforge-diagnostics-and-fixits-guide.md`.
    pub fn check(&self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        let mut config = self.config.clone();
        normalize_owned_config_for_check(&mut config);
        assemble_raw(self.root_path.as_path(), config.as_borrowed())
            .map_err(map_asm_run_error_to_workflow)
    }
}

impl<'a> PreparedAssembly<'a> {
    /// Returns the root source path used to create this prepared state.
    pub fn root_path(&self) -> &'a Path {
        self.root_path
    }

    /// Returns the prepared root module identifier.
    ///
    /// Use this when the host needs a stable module identifier before final
    /// emission, such as the prepared-session flow in
    /// `documentation/libopforge-developer-guide-examples/libopforge_prepared.rs`.
    pub fn root_module_id(&self) -> &str {
        self.root_module_id.as_str()
    }

    /// Returns the resolved CPU name for the prepared assembly.
    ///
    /// Use this when the host wants to display or persist the CPU chosen by
    /// preparation before running outputs.
    pub fn cpu_name(&self) -> &str {
        self.cpu.as_str()
    }

    /// Returns the source map collected during preparation.
    ///
    /// Use this when diagnostics or editor navigation must map expanded lines
    /// back to original source files. See
    /// `documentation/libopforge-diagnostics-and-fixits-guide.md`.
    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    /// Returns dependency files discovered during preparation.
    ///
    /// Use this when the host wants dependency metadata before choosing
    /// whether to emit outputs.
    pub fn dependency_files(&self) -> &[PathBuf] {
        &self.dependency_files
    }

    /// Executes the prepared assembly using the stored prepared state.
    ///
    /// Reuses the output-base resolution captured during `prepare()` so the
    /// emitted artifacts stay aligned with the preparation step.
    pub fn assemble(&self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        let input_base = self
            .resolved_output_base
            .as_deref()
            .unwrap_or(self.config.source.output_base);
        run_public_prepared_assembly(
            PreparedExecutionCoreRef {
                output_base: input_base,
                registry: &self.registry,
                cpu: self.cpu,
                max_loop_iterations: self.max_loop_iterations,
                root_module_id: self.root_module_id.as_str(),
                prepared_lines: &self.expanded_lines,
                source_map: &self.source_map,
                dependency_files: &self.dependency_files,
                module_macro_names: &self.module_macro_names,
            },
            self.config.clone().into(),
        )
        .map_err(map_asm_run_error_to_workflow)
    }

    /// Validates the prepared assembly without emitting outputs.
    ///
    /// This reuses the stored prepared state but suppresses artifact emission in
    /// the same way as the high-level borrowed `check()` path.
    pub fn check(&self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        let mut config = self.config.clone();
        normalize_output_options_for_check(&mut config.output);
        PreparedAssembly {
            config,
            ..self.clone()
        }
        .assemble()
    }
}

impl PreparedAssemblySession {
    /// Returns the root source path used to create this prepared state.
    pub fn root_path(&self) -> &Path {
        self.root_path.as_path()
    }

    /// Returns the prepared root module identifier.
    pub fn root_module_id(&self) -> &str {
        self.root_module_id.as_str()
    }

    /// Returns the resolved CPU name for the prepared assembly.
    pub fn cpu_name(&self) -> &str {
        self.cpu.as_str()
    }

    /// Returns the source map collected during preparation.
    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    /// Returns dependency files discovered during preparation.
    pub fn dependency_files(&self) -> &[PathBuf] {
        &self.dependency_files
    }

    /// Executes the prepared assembly using the stored prepared state.
    ///
    /// Reuses the owned prepared state, including the resolved `output_base`,
    /// expanded lines, and dependency metadata captured during `prepare()`. See
    /// `documentation/libopforge-developer-guide-examples/libopforge_prepared.rs`.
    pub fn assemble(&self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        let borrowed = self.config.as_borrowed();
        run_public_prepared_assembly(
            PreparedExecutionCoreRef {
                output_base: borrowed.output_base,
                registry: &self.registry,
                cpu: self.cpu,
                max_loop_iterations: self.max_loop_iterations,
                root_module_id: self.root_module_id.as_str(),
                prepared_lines: &self.expanded_lines,
                source_map: &self.source_map,
                dependency_files: &self.dependency_files,
                module_macro_names: &self.module_macro_names,
            },
            borrowed,
        )
        .map_err(map_asm_run_error_to_workflow)
    }

    /// Validates the prepared assembly without emitting outputs.
    ///
    /// This keeps the prepared-session reuse path but suppresses artifact
    /// emission in the same way as the owned high-level `check()` path. See
    /// `documentation/libopforge-diagnostics-and-fixits-guide.md`.
    pub fn check(&self) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
        let mut config = self.config.clone();
        normalize_owned_config_for_check(&mut config);
        PreparedAssemblySession {
            config,
            ..self.clone()
        }
        .assemble()
    }
}

/// Prepares an assembly using borrowed grouped options and returns reusable
/// prepared state.
pub fn prepare<'a>(
    root_path: &'a Path,
    options: PrepareOptions<'a>,
) -> Result<PreparedAssembly<'a>, asm::AssemblerWorkflowError> {
    Assembler::with_config(
        root_path,
        AssemblerConfig {
            source: SourceOptions {
                output_base: options.output_base,
                defines: options.defines,
                include_paths: options.include_paths,
                module_paths: options.module_paths,
                pp_macro_depth: options.pp_macro_depth,
                source_provider: options.source_provider,
            },
            execution: ExecutionOptions {
                execution_mode: options.execution_mode,
                cpu_override: options.cpu_override,
                max_loop_iterations: options.max_loop_iterations,
                opasm_package_path: options.opasm_package_path,
                collect_runtime_traces: true,
            },
            ..AssemblerConfig::default()
        },
    )
    .prepare()
}

fn assemble_raw(
    root_path: &Path,
    options: AssembleOptions<'_>,
) -> Result<AsmRunReport, AsmRunError> {
    let output_base = effective_output_base(root_path, options.output_base);
    engine::run_assembly(engine::AssemblyExecutionRequest {
        root_path,
        execution_mode: options.execution_mode,
        input_base: output_base.as_ref(),
        defines: options.defines,
        include_paths: options.include_paths,
        module_paths: options.module_paths,
        pp_macro_depth: options.pp_macro_depth,
        cpu_override: options.cpu_override,
        default_cpu: engine::default_cpu(),
        max_loop_iterations: options.max_loop_iterations,
        opasm_package_path: options.opasm_package_path,
        collect_runtime_traces: options.collect_runtime_traces,
        out_dir: options.out_dir,
        debug_conditionals: options.debug_conditionals,
        tab_size: options.tab_size,
        output_format: options.output_format,
        go_addr: options.go_addr,
        bin_specs: options.bin_specs,
        fill_byte: options.fill_byte,
        fill_byte_set: options.fill_byte_set,
        default_outputs: options.default_outputs,
        labels_file: options.labels_file,
        label_output_format: options.label_output_format,
        dependency_output: options.dependency_output,
        outfile_override: options.outfile_override,
        list_name_override: options.list_name_override,
        hex_name_override: options.hex_name_override,
        srec_name_override: options.srec_name_override,
        hunk_name_override: options.hunk_name_override,
        header_title: options.header_title,
        output_sink: options.output_sink,
        source_provider: options.source_provider,
        suppress_outputs: options.no_outputs,
    })
}

/// Runs a one-shot high-level assembly using borrowed grouped options.
pub fn assemble(
    root_path: &Path,
    options: AssembleOptions<'_>,
) -> Result<AsmRunReport, asm::AssemblerWorkflowError> {
    assemble_raw(root_path, options).map_err(map_asm_run_error_to_workflow)
}

#[cfg(test)]
mod tests {
    use super::{
        asm, diagnostics, io, lockstep, opcore, prepare, processing, registry, AssembleOptions,
        Assembler, AssemblerConfig, AssemblerSession, DiagnosticsOptions, OutputOptions,
        OwnedAssemblerConfig, OwnedExecutionOptions, OwnedOutputOptions, OwnedSourceOptions,
        PrepareOptions, SourceOptions,
    };
    use std::fs;
    use std::io::{self as stdio, Write};
    use std::path::{Path, PathBuf};
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::Arc;
    use std::time::{SystemTime, UNIX_EPOCH};
    use types::path_display::stable_path_string;

    static TEMP_DIR_SEQ: AtomicU64 = AtomicU64::new(1);

    const CONCERN_INVENTORY_GUIDE_PATH: &str =
        include_str!("../../../documentation/libopforge-developer-guide.md");
    const GUIDE_EXAMPLE_BORROWED: &str = include_str!(
        "../../../documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs"
    );
    const GUIDE_EXAMPLE_FILESYSTEM: &str = include_str!(
        "../../../documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs"
    );
    const GUIDE_EXAMPLE_FORMATTER: &str = include_str!(
        "../../../documentation/libopforge-developer-guide-examples/libopforge_formatter.rs"
    );
    const GUIDE_EXAMPLE_IN_MEMORY: &str = include_str!(
        "../../../documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs"
    );
    const GUIDE_EXAMPLE_LOCKSTEP: &str = include_str!(
        "../../../documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs"
    );
    const GUIDE_EXAMPLE_OPASM: &str = include_str!(
        "../../../documentation/libopforge-developer-guide-examples/libopforge_opasm.rs"
    );
    const GUIDE_EXAMPLE_OPCORE: &str = include_str!(
        "../../../documentation/libopforge-developer-guide-examples/libopforge_opcore.rs"
    );
    const GUIDE_EXAMPLE_PREPARED: &str = include_str!(
        "../../../documentation/libopforge-developer-guide-examples/libopforge_prepared.rs"
    );
    const GUIDE_EXAMPLE_REGISTRY: &str = include_str!(
        "../../../documentation/libopforge-developer-guide-examples/libopforge_registry.rs"
    );
    const GUIDE_EXAMPLE_WORKFLOW_ERROR: &str = include_str!(
        "../../../documentation/libopforge-developer-guide-examples/libopforge_workflow_error.rs"
    );

    mod guide_example_borrowed {
        use crate as libopforge;

        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs"
        ));
    }
    mod guide_example_filesystem {
        use crate as libopforge;

        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs"
        ));
    }
    mod guide_example_formatter {
        use crate as libopforge;

        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../documentation/libopforge-developer-guide-examples/libopforge_formatter.rs"
        ));
    }
    mod guide_example_in_memory {
        use crate as libopforge;

        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs"
        ));
    }
    mod guide_example_lockstep {
        use crate as libopforge;

        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs"
        ));
    }
    mod guide_example_opasm {
        use crate as libopforge;

        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../documentation/libopforge-developer-guide-examples/libopforge_opasm.rs"
        ));
    }
    mod guide_example_opcore {
        use crate as libopforge;

        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../documentation/libopforge-developer-guide-examples/libopforge_opcore.rs"
        ));
    }
    mod guide_example_prepared {
        use crate as libopforge;

        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../documentation/libopforge-developer-guide-examples/libopforge_prepared.rs"
        ));
    }
    mod guide_example_registry {
        use crate as libopforge;

        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../documentation/libopforge-developer-guide-examples/libopforge_registry.rs"
        ));
    }
    mod guide_example_workflow_error {
        use crate as libopforge;

        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../documentation/libopforge-developer-guide-examples/libopforge_workflow_error.rs"
        ));
    }

    const ASM_ROOT_COMPAT_EXPORT_AUDIT: &[&str] = &[
        "AssembleOptions",
        "Assembler",
        "AssemblerBuilder",
        "AssemblerConfig",
        "AssemblerSession",
        "AssemblerSessionBuilder",
        "DiagnosticsOptions",
        "ExecutionOptions",
        "OutputOptions",
        "OwnedAssemblerConfig",
        "OwnedExecutionOptions",
        "OwnedOutputOptions",
        "OwnedSourceOptions",
        "PrepareOptions",
        "PreparedAssembly",
        "PreparedAssemblySession",
        "SourceOptions",
        "assemble",
        "prepare",
    ];

    fn unique_temp_dir(prefix: &str) -> PathBuf {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let seq = TEMP_DIR_SEQ.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("{prefix}-{now}-{seq}"));
        fs::create_dir_all(&dir).expect("create temp dir");
        dir
    }

    fn expect_text(output_sink: &io::MemoryOutputSink, path: impl AsRef<Path>) -> String {
        output_sink
            .text(path.as_ref())
            .expect("utf8 output")
            .expect("text output")
    }

    fn has_text(output_sink: &io::MemoryOutputSink, path: impl AsRef<Path>) -> bool {
        output_sink
            .text(path.as_ref())
            .expect("utf8 output")
            .is_some()
    }

    fn missing_text(output_sink: &io::MemoryOutputSink, path: impl AsRef<Path>) -> bool {
        output_sink
            .text(path.as_ref())
            .expect("utf8 output")
            .is_none()
    }

    fn compatibility_reexports_for_module(module_name: &str) -> Vec<String> {
        let source = include_str!("lib.rs");
        let module_marker = format!("pub mod {module_name} {{");
        let module_start = source
            .find(&module_marker)
            .expect("module should exist in facade source");
        let module_source = &source[module_start..];
        let reexport_start = module_source
            .find("pub use crate::{")
            .expect("module should contain crate re-export block");
        let reexport_source = &module_source[reexport_start + "pub use crate::{".len()..];
        let reexport_end = reexport_source
            .find("};")
            .expect("crate re-export block should terminate");
        let symbols = &reexport_source[..reexport_end];

        let mut entries: Vec<String> = symbols
            .split(',')
            .map(str::trim)
            .filter(|entry| !entry.is_empty())
            .map(ToOwned::to_owned)
            .collect();
        entries.sort();
        entries
    }

    #[derive(Clone, Copy, Default)]
    struct FailingOutputSink;

    impl io::OutputSink for FailingOutputSink {
        fn create_dir_all(&self, _path: &Path) -> stdio::Result<()> {
            Err(stdio::Error::other("forced output failure"))
        }

        fn create_file(&self, _path: &Path) -> stdio::Result<Box<dyn Write>> {
            Err(stdio::Error::other("forced output failure"))
        }

        fn write_text(&self, _path: &Path, _content: &str) -> stdio::Result<()> {
            Err(stdio::Error::other("forced output failure"))
        }

        fn write_bytes(&self, _path: &Path, _bytes: &[u8]) -> stdio::Result<()> {
            Err(stdio::Error::other("forced output failure"))
        }
    }

    #[test]
    fn public_memory_output_sink_text_reports_binary_utf8_error() {
        let output_sink = io::MemoryOutputSink::new();

        <io::MemoryOutputSink as io::OutputSink>::write_bytes(
            &output_sink,
            Path::new("/virtual/out.bin"),
            &[0xff, 0x00, 0x41],
        )
        .expect("write binary output");

        let err = output_sink
            .text("/virtual/out.bin")
            .expect_err("binary output should report invalid utf8");
        assert_eq!(err.utf8_error().valid_up_to(), 0);
        assert_eq!(
            output_sink.bytes("/virtual/out.bin"),
            Some(vec![0xff, 0x00, 0x41])
        );
    }

    #[test]
    fn public_api_prepares_and_runs_with_in_memory_io() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file(
                "/virtual/main.asm",
                ".include \"inc.asm\"\n.module main\n.byte $00\n.endmodule\n",
            )
            .with_file("/virtual/inc.asm", ";\n");

        let prepared = prepare(
            Path::new("/virtual/main.asm"),
            PrepareOptions {
                source_provider: Some(&source_provider),
                ..PrepareOptions::default()
            },
        )
        .expect("prepare should succeed");

        assert_eq!(prepared.root_module_id(), "main");
        assert_eq!(prepared.cpu_name(), "8085");

        let output_sink = io::MemoryOutputSink::new();
        let report = AssemblerSession::builder("/virtual/main.asm")
            .execution_mode(lockstep::ExecutionMode::Vm)
            .output_base("/virtual/main")
            .output_format(asm::OutputFormat::Text)
            .label_output_format(asm::LabelOutputFormat::Vice)
            .header_title("test")
            .source_provider(source_provider.clone())
            .output_sink(output_sink.clone())
            .prepare()
            .expect("builder prepare should succeed")
            .assemble()
            .expect("prepared assembly should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        let listing = expect_text(&output_sink, "/virtual/main.lst");
        assert!(listing.contains(".byte $00"), "listing:\n{listing}");
        let hex = expect_text(&output_sink, "/virtual/main.hex");
        assert!(hex.contains(":0100000000FF"));
    }

    #[test]
    fn public_api_borrowed_prepare_derives_input_base_and_include_roots() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file(
                "/virtual/main.asm",
                ".module main\n.include \"inc.asm\"\n.byte $00\n.endmodule\n",
            )
            .with_file("/virtual/inc.asm", "; included\n");
        let output_sink = io::MemoryOutputSink::new();

        let report = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssemblerConfig {
                source: SourceOptions {
                    source_provider: Some(&source_provider),
                    ..SourceOptions::default()
                },
                output: OutputOptions {
                    output_format: asm::OutputFormat::Text,
                    label_output_format: asm::LabelOutputFormat::Vice,
                    output_sink: Some(&output_sink),
                    ..OutputOptions::default()
                },
                ..AssemblerConfig::default()
            },
        )
        .prepare()
        .expect("prepare should succeed without explicit include paths or input base")
        .assemble()
        .expect("prepared borrowed assembly should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(has_text(&output_sink, "/virtual/main.lst"));
        assert!(has_text(&output_sink, "/virtual/main.hex"));
    }

    #[test]
    fn public_borrowed_builder_path_setters_accept_path_refs() {
        let assembler = Assembler::builder(Path::new("/virtual/main.asm"))
            .opasm_package_path(Path::new("/virtual/runtime.opasm"))
            .out_dir(Path::new("/virtual/out"))
            .labels_file(Path::new("/virtual/symbols.lbl"))
            .build();

        assert_eq!(
            assembler.config().execution.opasm_package_path,
            Some(Path::new("/virtual/runtime.opasm"))
        );
        assert_eq!(
            assembler.config().output.out_dir,
            Some(Path::new("/virtual/out"))
        );
        assert_eq!(
            assembler.config().output.labels_file,
            Some(Path::new("/virtual/symbols.lbl"))
        );
    }

    #[test]
    fn public_api_owned_prepare_derives_input_base_when_omitted() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\n.byte $00\n.endmodule\n");
        let output_sink = io::MemoryOutputSink::new();

        let report = AssemblerSession::builder("/virtual/main.asm")
            .execution_mode(lockstep::ExecutionMode::Vm)
            .output_format(asm::OutputFormat::Text)
            .label_output_format(asm::LabelOutputFormat::Vice)
            .source_provider(source_provider.clone())
            .output_sink(output_sink.clone())
            .prepare()
            .expect("owned prepare should succeed without explicit input base")
            .assemble()
            .expect("prepared owned assembly should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(has_text(&output_sink, "/virtual/main.lst"));
        assert!(has_text(&output_sink, "/virtual/main.hex"));
    }

    #[test]
    fn public_free_prepare_supports_explicit_input_base_for_reuse() {
        let temp_dir = unique_temp_dir("libopforge-free-prepare");
        let source_path = temp_dir.join("main.asm");
        let custom_base = temp_dir.join("custom-output");
        fs::write(&source_path, ".module main\n.byte $00\n.endmodule\n").expect("write source");

        let prepared = prepare(
            source_path.as_path(),
            PrepareOptions {
                output_base: custom_base.to_str().expect("custom base"),
                ..PrepareOptions::default()
            },
        )
        .expect("free prepare should succeed");

        let report = prepared
            .assemble()
            .expect("prepared assembly should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(custom_base.with_extension("lst").is_file());
        assert!(custom_base.with_extension("hex").is_file());
    }

    #[test]
    fn public_free_prepare_preserves_explicit_execution_mode_for_reuse() {
        let temp_dir = unique_temp_dir("libopforge-free-prepare-lockstep");
        let source_path = temp_dir.join("main.asm");
        fs::write(&source_path, ".module main\n.byte $00\n.endmodule\n").expect("write source");

        let prepared = prepare(
            source_path.as_path(),
            PrepareOptions {
                execution_mode: lockstep::ExecutionMode::Lockstep {
                    continuation_head: lockstep::ContinuationHead::Rust,
                },
                ..PrepareOptions::default()
            },
        )
        .expect("free prepare should preserve explicit execution mode");

        let report = prepared
            .assemble()
            .expect("prepared assembly should succeed in lockstep mode");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(
            !report.lockstep_report().matches().is_empty(),
            "expected lockstep execution to record matches"
        );
    }

    #[test]
    fn public_free_prepare_derives_default_input_base_for_reuse() {
        let temp_dir = unique_temp_dir("libopforge-free-prepare-default");
        let source_path = temp_dir.join("main.asm");
        let default_base = temp_dir.join("main");
        fs::write(&source_path, ".module main\n.byte $00\n.endmodule\n").expect("write source");

        let prepared = prepare(source_path.as_path(), PrepareOptions::default())
            .expect("free prepare should succeed with default options");

        let report = prepared
            .assemble()
            .expect("prepared assembly should succeed with derived default base");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(default_base.with_extension("lst").is_file());
        assert!(default_base.with_extension("hex").is_file());
    }

    #[test]
    fn public_free_prepare_defaults_to_vm_execution_mode() {
        assert!(matches!(
            PrepareOptions::default().execution_mode,
            lockstep::ExecutionMode::Vm
        ));
    }

    #[test]
    fn public_owned_session_api_supports_check_without_outputs() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\nnop\n.endmodule\n");
        let output_sink = io::MemoryOutputSink::new();

        let report = AssemblerSession::builder("/virtual/main.asm")
            .execution_mode(lockstep::ExecutionMode::Vm)
            .output_base("/virtual/main")
            .output_format(asm::OutputFormat::Text)
            .label_output_format(asm::LabelOutputFormat::Vice)
            .source_provider(source_provider.clone())
            .output_sink(output_sink.clone())
            .check()
            .expect("owned session check should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(missing_text(&output_sink, "/virtual/main.lst"));
        assert!(missing_text(&output_sink, "/virtual/main.hex"));
    }

    #[test]
    fn public_owned_session_config_supports_grouped_non_borrowing_setup() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file(
                "/virtual/main.asm",
                ".module main\n.include \"inc.asm\"\n.use dep (VALUE)\nstart:\n    .byte FROM_INC\n    .byte VALUE\n.endmodule\n",
            )
            .with_file("/virtual/inc.asm", "FROM_INC .const 1\n")
            .with_file(
                "/virtual/modules/dep.asm",
                ".module dep\n.pub\nVALUE .const 7\n.priv\n.endmodule\n",
            );
        let output_sink = io::MemoryOutputSink::new();
        let out_dir = PathBuf::from("/virtual/out");
        let labels_path = out_dir.join("symbols.lbl");
        let dependency_output = asm::DependencyOutputPolicy {
            path: out_dir.join("main.d"),
            append: false,
            make_phony: false,
        };

        let report = AssemblerSession::with_config(
            "/virtual/main.asm",
            OwnedAssemblerConfig {
                source: OwnedSourceOptions {
                    output_base: "/virtual/main".to_string(),
                    include_paths: vec![PathBuf::from("/virtual")],
                    module_paths: vec![PathBuf::from("/virtual/modules")],
                    source_provider: Some(Arc::new(source_provider.clone())),
                    ..OwnedSourceOptions::default()
                },
                execution: OwnedExecutionOptions {
                    execution_mode: lockstep::ExecutionMode::Vm,
                    cpu_override: Some("8085".to_string()),
                    max_loop_iterations: 123,
                    opasm_package_path: None,
                    ..OwnedExecutionOptions::default()
                },
                output: OwnedOutputOptions {
                    out_dir: Some(out_dir.clone()),
                    output_format: asm::OutputFormat::Text,
                    labels_file: Some(labels_path.clone()),
                    dependency_output: Some(dependency_output.clone()),
                    label_output_format: asm::LabelOutputFormat::Vice,
                    output_sink: Some(Arc::new(output_sink.clone())),
                    ..OwnedOutputOptions::default()
                },
                diagnostics: DiagnosticsOptions {
                    debug_conditionals: true,
                    tab_size: Some(8),
                },
            },
        )
        .assemble()
        .expect("owned grouped config assembly should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        let listing = expect_text(&output_sink, "/virtual/out/main.lst");
        assert!(listing.contains("FROM_INC"), "listing:\n{listing}");
        assert!(has_text(&output_sink, "/virtual/out/main.hex"));
        assert!(has_text(&output_sink, labels_path));
        let dependency_text = expect_text(&output_sink, dependency_output.path);
        assert!(dependency_text.contains("/virtual/main.asm"));
        assert!(dependency_text.contains("/virtual/inc.asm"));
        assert!(dependency_text.contains("/virtual/modules/dep.asm"));
    }

    #[test]
    fn public_owned_session_check_suppresses_directive_driven_outputs() {
        let source_provider = io::MemorySourceProvider::new().with_file(
            "/virtual/main.asm",
            ".module main\n\n.region ram, $1000, $10ff\n\n.section code\n.pub\nstart\n    .byte $42, $43\n.priv\n.endsection\n\n.place code in ram\n\n.output \"build/minimal.bin\", format=bin, sections=code\n.mapfile \"build/minimal.map\", symbols=public\n.exportsections dir=\"build/minimal_sections\", format=bin\n\n.endmodule\n",
        );
        let output_sink = io::MemoryOutputSink::new();
        let out_dir = PathBuf::from("/virtual/out");

        let report = AssemblerSession::builder("/virtual/main.asm")
            .execution_mode(lockstep::ExecutionMode::Vm)
            .output_base("/virtual/main")
            .out_dir(out_dir.clone())
            .output_format(asm::OutputFormat::Text)
            .label_output_format(asm::LabelOutputFormat::Vice)
            .source_provider(source_provider.clone())
            .output_sink(output_sink.clone())
            .check()
            .expect("owned session check should suppress directive-driven outputs");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(missing_text(&output_sink, "/virtual/out/main.lst"));
        assert!(missing_text(&output_sink, "/virtual/out/main.hex"));
        assert!(output_sink
            .bytes("/virtual/out/build/minimal.bin")
            .is_none());
        assert!(missing_text(&output_sink, "/virtual/out/build/minimal.map"));
        assert!(!output_sink
            .directories()
            .iter()
            .any(|path| path == &PathBuf::from("/virtual/out/build/minimal_sections")));
    }

    #[test]
    fn public_api_prepared_assembly_supports_check_without_outputs() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\nnop\n.endmodule\n");
        let output_sink = io::MemoryOutputSink::new();

        let report = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssemblerConfig {
                source: SourceOptions {
                    output_base: "/virtual/main",
                    source_provider: Some(&source_provider),
                    ..SourceOptions::default()
                },
                output: OutputOptions {
                    output_format: asm::OutputFormat::Text,
                    label_output_format: asm::LabelOutputFormat::Vice,
                    output_sink: Some(&output_sink),
                    ..OutputOptions::default()
                },
                ..AssemblerConfig::default()
            },
        )
        .prepare()
        .expect("prepare should succeed")
        .check()
        .expect("prepared check should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(missing_text(&output_sink, "/virtual/main.lst"));
        assert!(missing_text(&output_sink, "/virtual/main.hex"));
    }

    #[test]
    fn public_api_supports_explicit_rust_execution_mode() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\nnop\n.endmodule\n");
        let output_sink = io::MemoryOutputSink::new();

        let report = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Rust,
                output_base: "/virtual/main",
                output_format: asm::OutputFormat::Text,
                label_output_format: asm::LabelOutputFormat::Vice,
                header_title: "test",
                source_provider: Some(&source_provider),
                output_sink: Some(&output_sink),
                ..AssembleOptions::default()
            },
        )
        .assemble()
        .expect("rust-mode assembly should succeed");

        assert_eq!(report.error_count(), 0);
        assert!(report.lockstep_report().matches().is_empty());
        assert!(report.lockstep_report().divergences().is_empty());
    }

    #[test]
    fn public_api_supports_explicit_lockstep_execution_mode() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\nnop\n.endmodule\n");
        let output_sink = io::MemoryOutputSink::new();

        let report = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Lockstep {
                    continuation_head: lockstep::ContinuationHead::Vm,
                },
                output_base: "/virtual/main",
                output_format: asm::OutputFormat::Text,
                label_output_format: asm::LabelOutputFormat::Vice,
                header_title: "test",
                source_provider: Some(&source_provider),
                output_sink: Some(&output_sink),
                ..AssembleOptions::default()
            },
        )
        .assemble()
        .expect("lockstep assembly should succeed");

        assert_eq!(report.error_count(), 0);
        assert!(
            report
                .lockstep_report()
                .matches()
                .iter()
                .any(|entry| entry.stage == lockstep::LockstepStage::OpasmStatementParse),
            "expected public lockstep API to record statement parse matches"
        );
        assert!(report.lockstep_report().divergences().is_empty());
    }

    #[test]
    fn public_api_rewrites_out_dir_for_absolute_input_base() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\n.byte $00\n.endmodule\n");
        let output_sink = io::MemoryOutputSink::new();
        let out_dir = PathBuf::from("/virtual/out");

        let report = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: "/virtual/main",
                out_dir: Some(&out_dir),
                output_format: asm::OutputFormat::Text,
                label_output_format: asm::LabelOutputFormat::Vice,
                header_title: "test",
                source_provider: Some(&source_provider),
                output_sink: Some(&output_sink),
                ..AssembleOptions::default()
            },
        )
        .assemble()
        .expect("assembly with out_dir should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(missing_text(&output_sink, "/virtual/main.lst"));
        assert!(missing_text(&output_sink, "/virtual/main.hex"));

        let listing = expect_text(&output_sink, "/virtual/out/main.lst");
        assert!(listing.contains(".byte $00"), "listing:\n{listing}");

        let hex = expect_text(&output_sink, "/virtual/out/main.hex");
        assert!(hex.contains(":0100000000FF"), "hex:\n{hex}");
    }

    #[test]
    fn public_api_supports_assembler_check_without_outputs() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\nnop\n.endmodule\n");
        let output_sink = io::MemoryOutputSink::new();

        let report = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: "/virtual/main",
                output_format: asm::OutputFormat::Text,
                label_output_format: asm::LabelOutputFormat::Vice,
                source_provider: Some(&source_provider),
                output_sink: Some(&output_sink),
                ..AssembleOptions::default()
            },
        )
        .check()
        .expect("check should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(missing_text(&output_sink, "/virtual/main.lst"));
        assert!(missing_text(&output_sink, "/virtual/main.hex"));
    }

    #[test]
    fn public_api_builder_supports_grouped_config_path() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\n.byte $00\n.endmodule\n");
        let output_sink = io::MemoryOutputSink::new();

        let report = Assembler::builder(Path::new("/virtual/main.asm"))
            .execution_mode(lockstep::ExecutionMode::Vm)
            .output_base("/virtual/main")
            .output_format(asm::OutputFormat::Text)
            .label_output_format(asm::LabelOutputFormat::Vice)
            .header_title("builder test")
            .source_provider(&source_provider)
            .output_sink(&output_sink)
            .assemble()
            .expect("builder-based assembly should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        let listing = expect_text(&output_sink, "/virtual/main.lst");
        assert!(listing.contains(".byte $00"), "listing:\n{listing}");
        let hex = expect_text(&output_sink, "/virtual/main.hex");
        assert!(hex.contains(":0100000000FF"), "hex:\n{hex}");
    }

    #[test]
    fn public_borrowed_and_owned_builders_share_common_config_mutations() {
        let defines = vec!["FEATURE=1".to_string()];
        let include_paths = vec![PathBuf::from("/virtual/include")];
        let module_paths = vec![PathBuf::from("/virtual/modules")];
        let bin_specs = vec![asm::BinOutputSpec {
            name: Some("bank0.bin".to_string()),
            range: None,
        }];
        let dependency_output = asm::DependencyOutputPolicy {
            path: PathBuf::from("/virtual/out/main.d"),
            append: true,
            make_phony: false,
        };
        let labels_file = PathBuf::from("/virtual/out/symbols.lbl");
        let opasm_package_path = PathBuf::from("/virtual/runtime.opasm");
        let out_dir = PathBuf::from("/virtual/out");
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\nnop\n.endmodule\n");
        let borrowed_output_sink = io::MemoryOutputSink::new();
        let owned_output_sink = io::MemoryOutputSink::new();

        let borrowed = Assembler::builder(Path::new("/virtual/main.asm"))
            .output_base("/virtual/main")
            .defines(&defines)
            .include_paths(&include_paths)
            .module_paths(&module_paths)
            .pp_macro_depth(64)
            .source_provider(&source_provider)
            .execution_mode(lockstep::ExecutionMode::Lockstep {
                continuation_head: lockstep::ContinuationHead::Rust,
            })
            .cpu_override("8085")
            .max_loop_iterations(2048)
            .opasm_package_path(opasm_package_path.as_path())
            .out_dir(out_dir.as_path())
            .output_format(asm::OutputFormat::Text)
            .go_addr("1234")
            .bin_specs(&bin_specs)
            .fill_byte(0xEA)
            .labels_file(labels_file.as_path())
            .dependency_output(&dependency_output)
            .outfile_override("custom.out")
            .list_name_override("custom.lst")
            .hex_name_override("custom.hex")
            .srec_name_override("custom.srec")
            .hunk_name_override("custom.hunk")
            .label_output_format(asm::LabelOutputFormat::Vice)
            .header_title("Shared builder test")
            .default_outputs(false)
            .no_outputs(true)
            .debug_conditionals(true)
            .tab_size(8)
            .output_sink(&borrowed_output_sink)
            .build();

        let owned = AssemblerSession::builder("/virtual/main.asm")
            .output_base("/virtual/main")
            .defines(defines.clone())
            .include_paths(include_paths.clone())
            .module_paths(module_paths.clone())
            .pp_macro_depth(64)
            .source_provider(source_provider.clone())
            .execution_mode(lockstep::ExecutionMode::Lockstep {
                continuation_head: lockstep::ContinuationHead::Rust,
            })
            .cpu_override("8085")
            .max_loop_iterations(2048)
            .opasm_package_path(opasm_package_path.clone())
            .out_dir(out_dir.clone())
            .output_format(asm::OutputFormat::Text)
            .go_addr("1234")
            .bin_specs(bin_specs.clone())
            .fill_byte(0xEA)
            .labels_file(labels_file.clone())
            .dependency_output(dependency_output.clone())
            .outfile_override("custom.out")
            .list_name_override("custom.lst")
            .hex_name_override("custom.hex")
            .srec_name_override("custom.srec")
            .hunk_name_override("custom.hunk")
            .label_output_format(asm::LabelOutputFormat::Vice)
            .header_title("Shared builder test")
            .default_outputs(false)
            .no_outputs(true)
            .debug_conditionals(true)
            .tab_size(8)
            .output_sink(owned_output_sink.clone())
            .build();

        let borrowed_options: AssembleOptions<'_> = borrowed.config().clone().into();
        let owned_options = owned.config().as_borrowed();

        assert_eq!(borrowed_options.output_base, owned_options.output_base);
        assert_eq!(borrowed_options.defines, owned_options.defines);
        assert_eq!(borrowed_options.include_paths, owned_options.include_paths);
        assert_eq!(borrowed_options.module_paths, owned_options.module_paths);
        assert_eq!(
            borrowed_options.pp_macro_depth,
            owned_options.pp_macro_depth
        );
        assert_eq!(borrowed_options.cpu_override, owned_options.cpu_override);
        assert_eq!(
            borrowed_options.max_loop_iterations,
            owned_options.max_loop_iterations
        );
        assert_eq!(
            borrowed_options.opasm_package_path,
            owned_options.opasm_package_path
        );
        assert_eq!(borrowed_options.out_dir, owned_options.out_dir);
        assert_eq!(borrowed_options.output_format, owned_options.output_format);
        assert_eq!(borrowed_options.go_addr, owned_options.go_addr);
        assert_eq!(
            borrowed_options.bin_specs.len(),
            owned_options.bin_specs.len()
        );
        assert_eq!(
            borrowed_options.bin_specs[0].name.as_deref(),
            owned_options.bin_specs[0].name.as_deref()
        );
        assert!(borrowed_options.bin_specs[0].range.is_none());
        assert!(owned_options.bin_specs[0].range.is_none());
        assert_eq!(borrowed_options.fill_byte, owned_options.fill_byte);
        assert_eq!(borrowed_options.fill_byte_set, owned_options.fill_byte_set);
        assert_eq!(
            borrowed_options.default_outputs,
            owned_options.default_outputs
        );
        assert_eq!(borrowed_options.labels_file, owned_options.labels_file);
        let borrowed_dep = borrowed_options
            .dependency_output
            .expect("borrowed dependency output");
        let owned_dep = owned_options
            .dependency_output
            .expect("owned dependency output");
        assert_eq!(borrowed_dep.path, owned_dep.path);
        assert_eq!(borrowed_dep.append, owned_dep.append);
        assert_eq!(borrowed_dep.make_phony, owned_dep.make_phony);
        assert_eq!(
            borrowed_options.label_output_format,
            owned_options.label_output_format
        );
        assert_eq!(
            borrowed_options.outfile_override,
            owned_options.outfile_override
        );
        assert_eq!(
            borrowed_options.list_name_override,
            owned_options.list_name_override
        );
        assert_eq!(
            borrowed_options.hex_name_override,
            owned_options.hex_name_override
        );
        assert_eq!(
            borrowed_options.srec_name_override,
            owned_options.srec_name_override
        );
        assert_eq!(
            borrowed_options.hunk_name_override,
            owned_options.hunk_name_override
        );
        assert_eq!(borrowed_options.header_title, owned_options.header_title);
        assert_eq!(
            borrowed_options.debug_conditionals,
            owned_options.debug_conditionals
        );
        assert_eq!(borrowed_options.tab_size, owned_options.tab_size);
        assert!(borrowed_options.source_provider.is_some());
        assert!(owned_options.source_provider.is_some());
        assert!(borrowed_options.output_sink.is_some());
        assert!(owned_options.output_sink.is_some());
        assert_eq!(borrowed_options.no_outputs, owned_options.no_outputs);
    }

    #[test]
    fn public_api_prepare_uses_engine_module_item_path() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file(
                "/virtual/main.asm",
                ".module main\n.use dep\n.byte VALUE\n.endmodule\n",
            )
            .with_file(
                "/virtual/modules/dep.asm",
                ".module dep\nVALUE .const 7\n.endmodule\n",
            );
        let module_paths = vec![PathBuf::from("/virtual/modules")];

        let prepared = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssemblerConfig {
                source: SourceOptions {
                    module_paths: &module_paths,
                    source_provider: Some(&source_provider),
                    ..SourceOptions::default()
                },
                ..AssemblerConfig::default()
            },
        )
        .prepare()
        .expect("prepare should resolve module-item graph through engine");

        assert_eq!(prepared.root_module_id(), "main");
        assert!(
            prepared
                .dependency_files()
                .iter()
                .any(|path| path == &PathBuf::from("/virtual/modules/dep.asm")),
            "dependencies: {:?}",
            prepared.dependency_files()
        );
        assert!(
            prepared
                .source_map()
                .origins()
                .iter()
                .filter_map(|origin| origin.file.as_deref())
                .any(|file| file == "/virtual/modules/dep.asm"),
            "source origins: {:?}",
            prepared.source_map().origins()
        );
    }

    #[test]
    fn public_opcore_api_tokenizes_and_parses_expression() {
        let tokenized = opcore::tokenize_line("1 + 2", 1).expect("tokenization should succeed");
        let expr = opcore::parse_expression(tokenized).expect("expression parse should succeed");
        match expr {
            opcore::Expr::Binary { .. } => {}
            other => panic!("expected binary expression, got {other:?}"),
        }
    }

    #[test]
    fn public_opcore_api_processes_module_item() {
        let outcome = opcore::process_module_item(".use math as m", 1);
        match outcome {
            processing::ProcessingOutcome::Done(opcore::LineAst::Use(use_ast)) => {
                assert_eq!(use_ast.module_id, "math");
                assert_eq!(use_ast.alias.as_deref(), Some("m"));
            }
            other => panic!("expected opcore module-item success, got {other:?}"),
        }
    }

    #[test]
    fn public_opcore_core_error_classifies_leaf_failures() {
        let span = opcore::Span {
            line: 7,
            col_start: 2,
            col_end: 4,
        };

        let tokenize = opcore::CoreError::from(opcore::TokenizeError {
            message: "bad token".to_string(),
            span,
        });
        assert_eq!(tokenize.kind(), opcore::CoreErrorKind::Tokenize);
        assert_eq!(tokenize.summary(), "bad token");
        assert_eq!(tokenize.code(), "opcore.tokenize");

        let parse = opcore::CoreError::from(opcore::ParseError {
            message: "bad parse".to_string(),
            span,
        });
        assert_eq!(parse.kind(), opcore::CoreErrorKind::Parse);
        assert_eq!(parse.summary(), "bad parse");
        assert_eq!(parse.code(), "opcore.parse");

        let expr = opcore::CoreError::from(opcore::EvalError::with_span("bad expr", span));
        assert_eq!(expr.kind(), opcore::CoreErrorKind::Expr);
        assert_eq!(expr.summary(), "bad expr");
        assert_eq!(expr.code(), "opcore.expr");
    }

    #[test]
    fn public_opcore_module_item_errors_classify_module_use_and_import_failures() {
        let module_err = match opcore::process_module_item(".module \"unterminated", 11) {
            processing::ProcessingOutcome::Error(err) => err,
            other => panic!("expected module-item error, got {other:?}"),
        };
        assert_eq!(module_err.kind(), opcore::CoreErrorKind::Module);
        assert_eq!(module_err.code(), "opcore.module");
        assert!(!module_err.summary().is_empty());

        let use_err = match opcore::process_module_item(".use", 12) {
            processing::ProcessingOutcome::Error(err) => err,
            other => panic!("expected .use error, got {other:?}"),
        };
        assert_eq!(use_err.kind(), opcore::CoreErrorKind::Use);
        assert_eq!(use_err.code(), "opcore.use");

        let import_err = match opcore::process_module_item(".use math ()", 13) {
            processing::ProcessingOutcome::Error(err) => err,
            other => panic!("expected import error, got {other:?}"),
        };
        assert_eq!(import_err.kind(), opcore::CoreErrorKind::Import);
        assert_eq!(import_err.code(), "opcore.import");

        let wrapped = opcore::CoreError::from(import_err);
        assert_eq!(wrapped.kind(), opcore::CoreErrorKind::Import);
        assert_eq!(wrapped.code(), "opcore.import");
    }

    #[test]
    fn public_opcore_core_error_classifies_macro_conditional_and_repetition_failures() {
        let mut macro_processor = opcore::MacroProcessor::new();
        let macro_err = macro_processor
            .expand(&[".endmacro".to_string()])
            .expect_err("macro expansion should fail");
        let macro_core = opcore::CoreError::from(macro_err);
        assert_eq!(macro_core.kind(), opcore::CoreErrorKind::Macro);
        assert_eq!(macro_core.code(), "opcore.macro");

        let conditional_err = opcore::editor_parse_line(".if \"unterminated", 31)
            .expect_err("conditional parse should fail");
        assert_eq!(conditional_err.kind(), opcore::CoreErrorKind::Conditional);
        assert_eq!(conditional_err.code(), "opcore.conditional");

        let repetition_err = opcore::editor_parse_line(".for \"unterminated", 32)
            .expect_err("repetition parse should fail");
        assert_eq!(repetition_err.kind(), opcore::CoreErrorKind::Repetition);
        assert_eq!(repetition_err.code(), "opcore.repetition");

        let wrapped = opcore::CoreError::from(repetition_err);
        assert_eq!(wrapped.kind(), opcore::CoreErrorKind::Repetition);
        assert_eq!(wrapped.code(), "opcore.repetition");
    }

    #[test]
    fn public_opcore_core_error_classifies_namespace_scope_and_preprocess_failures() {
        let namespace_err = opcore::MacroProcessor::new()
            .expand(&[".endn".to_string()])
            .expect_err("namespace close should fail");
        let namespace_core = opcore::CoreError::from(namespace_err);
        assert_eq!(namespace_core.kind(), opcore::CoreErrorKind::Namespace);
        assert_eq!(namespace_core.code(), "opcore.namespace");

        let scope_err = opcore::MacroProcessor::new()
            .expand(&[".endblock".to_string()])
            .expect_err("scope close should fail");
        let scope_core = opcore::CoreError::from(scope_err);
        assert_eq!(scope_core.kind(), opcore::CoreErrorKind::Scope);
        assert_eq!(scope_core.code(), "opcore.scope");

        let temp_dir = unique_temp_dir("libopforge-preprocess");
        let source_path = temp_dir.join("bad.asm");
        fs::write(&source_path, "#include \"inc.asm\"\n").expect("write preprocess source");

        let preprocess_err = opcore::Preprocessor::new()
            .process_file(source_path.to_str().expect("source path utf8"))
            .expect_err("preprocess should fail");
        let preprocess_core = opcore::CoreError::from(preprocess_err);
        assert_eq!(preprocess_core.kind(), opcore::CoreErrorKind::Preprocess);
        assert_eq!(preprocess_core.code(), "opcore.preprocess");

        fs::remove_dir_all(&temp_dir).expect("cleanup temp dir");
    }

    #[test]
    fn public_opcore_core_error_classifies_struct_and_segment_failures() {
        let tokenized =
            opcore::tokenize_line("Point{field}", 41).expect("struct literal tokens should parse");
        let struct_err =
            opcore::parse_expression(tokenized).expect_err("invalid struct literal should fail");
        let struct_core = opcore::CoreError::from(struct_err);
        assert_eq!(struct_core.kind(), opcore::CoreErrorKind::Struct);
        assert_eq!(struct_core.code(), "opcore.struct");

        let segment_err = opcore::MacroProcessor::new()
            .expand(&[".endsegment".to_string()])
            .expect_err("segment close should fail");
        let segment_core = opcore::CoreError::from(segment_err);
        assert_eq!(segment_core.kind(), opcore::CoreErrorKind::Segment);
        assert_eq!(segment_core.code(), "opcore.segment");
    }

    #[test]
    fn public_opcore_surface_does_not_require_assembler_workflow_errors() {
        let _expr_text_fn: fn(&opcore::Expr) -> Option<String> = opcore::expr_text;
        let _core_error_type: Option<opcore::CoreError> = None;
        let _tokenize_error_type: Option<opcore::TokenizeError> = None;
        let _parse_error_type: Option<opcore::ParseError> = None;
        let _eval_error_type: Option<opcore::EvalError> = None;
    }

    #[test]
    fn facade_export_audit_lists_only_approved_root_asm_compatibility_exports() {
        let mut expected: Vec<String> = ASM_ROOT_COMPAT_EXPORT_AUDIT
            .iter()
            .map(|symbol| (*symbol).to_string())
            .collect();
        expected.sort();

        assert_eq!(compatibility_reexports_for_module("asm"), expected);
    }

    #[test]
    fn facade_export_audit_keeps_new_core_and_processing_errors_in_canonical_modules() {
        let compat_exports = compatibility_reexports_for_module("asm");

        assert!(!compat_exports.iter().any(|symbol| symbol == "CoreError"));
        assert!(!compat_exports
            .iter()
            .any(|symbol| symbol == "CoreErrorKind"));
        assert!(!compat_exports
            .iter()
            .any(|symbol| symbol == "ProcessorError"));
        assert!(!compat_exports
            .iter()
            .any(|symbol| symbol == "ProcessorErrorKind"));
        assert!(!compat_exports
            .iter()
            .any(|symbol| symbol == "ProcessorFailureDetail"));

        let _core_error_type: Option<opcore::CoreError> = None;
        let _core_error_kind_type: Option<opcore::CoreErrorKind> = None;
        let _processor_error_type: Option<processing::ProcessorError> = None;
        let _processor_error_kind_type: Option<processing::ProcessorErrorKind> = None;
        let _processor_failure_detail_type: Option<processing::ProcessorFailureDetail> = None;
    }

    #[test]
    fn facade_concern_inventory_docs_publish_all_stable_modules() {
        let guide = CONCERN_INVENTORY_GUIDE_PATH;

        for module in [
            "`libopforge::asm`",
            "`libopforge::asm::opasm`",
            "`libopforge::formatter`",
            "`libopforge::processing`",
            "`libopforge::registry`",
            "`libopforge::lockstep`",
            "`libopforge::io`",
            "`libopforge::diagnostics`",
            "`libopforge::opcore`",
        ] {
            assert!(
                guide.contains(module),
                "developer guide should include concern inventory for {module}"
            );
        }

        assert!(guide.contains("Concern inventories"));
        assert!(guide.contains("CLI or host presentation may specialize wording"));
        assert!(guide.contains("module-first API"));
    }

    #[test]
    fn facade_guide_high_level_workflow_error_contract_matches_public_api() {
        let guide = CONCERN_INVENTORY_GUIDE_PATH;

        assert!(guide.contains(
            "High-level assembly returns `AsmRunReport` on success and `AssemblerWorkflowError` on failure."
        ));
        assert!(guide.contains(
            "`AssemblerWorkflowError::Assemble` variant carries the underlying `AsmRunError` payload"
        ));
        assert!(!guide.contains(
            "High-level assembly returns `AsmRunReport` on success and `AsmRunError` on failure."
        ));
    }

    #[test]
    fn facade_guide_examples_publish_and_run_stable_entry_paths() {
        let guide = CONCERN_INVENTORY_GUIDE_PATH;

        for (path, source, marker) in [
            (
                "documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs",
                GUIDE_EXAMPLE_BORROWED,
                "Assembler::builder",
            ),
            (
                "documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs",
                GUIDE_EXAMPLE_IN_MEMORY,
                "AssemblerSession::builder",
            ),
            (
                "documentation/libopforge-developer-guide-examples/libopforge_prepared.rs",
                GUIDE_EXAMPLE_PREPARED,
                ".prepare()?",
            ),
            (
                "documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs",
                GUIDE_EXAMPLE_LOCKSTEP,
                "ExecutionMode::Lockstep",
            ),
            (
                "documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs",
                GUIDE_EXAMPLE_FILESYSTEM,
                "out_dir",
            ),
            (
                "documentation/libopforge-developer-guide-examples/libopforge_workflow_error.rs",
                GUIDE_EXAMPLE_WORKFLOW_ERROR,
                "AssemblerWorkflowErrorKind",
            ),
            (
                "documentation/libopforge-developer-guide-examples/libopforge_opcore.rs",
                GUIDE_EXAMPLE_OPCORE,
                "opcore::tokenize_line",
            ),
            (
                "documentation/libopforge-developer-guide-examples/libopforge_registry.rs",
                GUIDE_EXAMPLE_REGISTRY,
                "CapabilitySnapshot::from_registry",
            ),
            (
                "documentation/libopforge-developer-guide-examples/libopforge_formatter.rs",
                GUIDE_EXAMPLE_FORMATTER,
                "FormatterEngine::new",
            ),
            (
                "documentation/libopforge-developer-guide-examples/libopforge_opasm.rs",
                GUIDE_EXAMPLE_OPASM,
                "opasm::process_statement",
            ),
        ] {
            assert!(
                guide.contains(path),
                "developer guide should reference {path}"
            );
            assert!(
                source.contains(marker),
                "example {path} should include {marker}"
            );
            assert!(
                source.contains("run_example"),
                "example {path} should expose run_example"
            );
        }

        guide_example_borrowed::run_example().expect("borrowed guide example");
        guide_example_in_memory::run_example().expect("in-memory guide example");
        guide_example_prepared::run_example().expect("prepared guide example");
        guide_example_lockstep::run_example().expect("lockstep guide example");
        guide_example_filesystem::run_example().expect("filesystem guide example");
        guide_example_workflow_error::run_example().expect("workflow error guide example");
        guide_example_opcore::run_example().expect("opcore guide example");
        guide_example_registry::run_example().expect("registry guide example");
        guide_example_formatter::run_example().expect("formatter guide example");
        guide_example_opasm::run_example().expect("opasm guide example");
    }

    #[test]
    fn public_portable_opcore_api_tokenizes_and_parses_expression() {
        let tokenized =
            opcore::portable::tokenize_line("1 + 2", 1).expect("tokenization should succeed");
        assert_eq!(tokenized.tokens.len(), 3);
        let expr =
            opcore::portable::parse_expression(tokenized).expect("expression parse succeeds");
        match expr {
            opcore::portable::PortableAstExpr::Binary { .. } => {}
            other => panic!("expected portable binary expression, got {other:?}"),
        }
    }

    #[test]
    fn public_portable_opcore_api_processes_module_item() {
        let outcome = opcore::portable::process_module_item(".use math as m", 1);
        match outcome {
            processing::ProcessingOutcome::Done(opcore::portable::PortableLineAst::Use {
                module_id,
                alias,
                ..
            }) => {
                assert_eq!(module_id, "math");
                assert_eq!(alias.as_deref(), Some("m"));
            }
            other => panic!("expected portable use item, got {other:?}"),
        }
    }

    #[test]
    fn public_module_qualified_imports_cover_supported_paths() {
        let _diagnostic_type: Option<diagnostics::Diagnostic> = None;
        let _report_type: Option<diagnostics::AsmRunReport> = None;
        let _severity_type: Option<diagnostics::Severity> = None;
        let _workflow_error_type: Option<asm::AssemblerWorkflowError> = None;
        let _workflow_error_kind = asm::AssemblerWorkflowErrorKind::Assemble;
        let _invalid_argument_type: Option<asm::InvalidArgumentError> = None;
        let _invalid_request_type: Option<asm::InvalidRequestError> = None;
        let _host_io_error_type: Option<asm::HostIoError> = None;
        let _internal_error_type: Option<asm::InternalErrorReport> = None;
        let _output_format = asm::OutputFormat::Text;
        let _label_output_format = asm::LabelOutputFormat::Vice;
        let _dependency_output: Option<asm::DependencyOutputPolicy> = None;
        let _bin_spec: Option<asm::BinOutputSpec> = None;
        let _bin_range: Option<asm::BinRange> = None;
        let _core_error_type: Option<opcore::CoreError> = None;
        let _core_error_kind = opcore::CoreErrorKind::Tokenize;
        let _eval_error_type: Option<opcore::EvalError> = None;
        let _macro_error_type: Option<opcore::MacroError> = None;
        let _macro_processor_type: Option<opcore::MacroProcessor> = None;
        let _preprocess_error_type: Option<opcore::PreprocessError> = None;
        let _preprocessor_type: Option<opcore::Preprocessor> = None;
        let _module_item_error_type: Option<opcore::ModuleItemError> = None;
        let _line_parse_error_type: Option<opcore::LineParseError> = None;
        let _engine_error_type: Option<processing::EngineError> = None;
        let _engine_error_kind = processing::EngineErrorKind::Core;
        let _processor_error_type: Option<processing::ProcessorError> = None;
        let _processor_error_kind = processing::ProcessorErrorKind::InvalidRequest;
        let _processor_detail_type: Option<processing::ProcessorFailureDetail> = None;
    }

    #[test]
    fn public_asm_run_error_exposes_stable_inspection_and_workflow_conversion() {
        let run_error = diagnostics::AsmRunError::new(
            diagnostics::AsmError::new(diagnostics::AsmErrorKind::Directive, "bad directive", None),
            Vec::new(),
            Vec::new(),
        );

        assert_eq!(run_error.kind(), diagnostics::AsmErrorKind::Directive);
        assert_eq!(run_error.summary(), "bad directive");
        assert_eq!(
            run_error.error().kind(),
            diagnostics::AsmErrorKind::Directive
        );

        let workflow_error = asm::AssemblerWorkflowError::from(run_error);
        assert_eq!(
            workflow_error.kind(),
            asm::AssemblerWorkflowErrorKind::Assemble
        );
        assert_eq!(workflow_error.code(), "asm.workflow.assemble");
        assert_eq!(workflow_error.summary(), "bad directive");
        assert_eq!(
            workflow_error.as_assemble().expect("assemble error").kind(),
            diagnostics::AsmErrorKind::Directive
        );
    }

    #[test]
    fn public_workflow_error_wrappers_preserve_stable_wrapper_behavior() {
        let invalid_argument =
            asm::InvalidArgumentError::new("asm.workflow.invalid_argument", "bad cpu override");
        assert_eq!(invalid_argument.code(), "asm.workflow.invalid_argument");
        assert_eq!(invalid_argument.summary(), "bad cpu override");
        assert_eq!(invalid_argument.to_string(), "bad cpu override");
        let invalid_argument_err: &dyn std::error::Error = &invalid_argument;
        assert_eq!(invalid_argument_err.to_string(), "bad cpu override");
        let invalid_argument_workflow =
            asm::AssemblerWorkflowError::InvalidArgument(invalid_argument.clone());
        assert_eq!(
            invalid_argument_workflow.kind(),
            asm::AssemblerWorkflowErrorKind::InvalidArgument
        );
        assert_eq!(
            invalid_argument_workflow
                .as_invalid_argument()
                .expect("invalid argument payload")
                .code(),
            "asm.workflow.invalid_argument"
        );
        assert_eq!(invalid_argument_workflow.summary(), "bad cpu override");
        assert_eq!(invalid_argument_workflow.to_string(), "bad cpu override");

        let invalid_request = asm::InvalidRequestError::new(
            "asm.workflow.invalid_request",
            "runtime model is unavailable",
        );
        assert_eq!(invalid_request.code(), "asm.workflow.invalid_request");
        assert_eq!(invalid_request.summary(), "runtime model is unavailable");
        assert_eq!(invalid_request.to_string(), "runtime model is unavailable");
        let invalid_request_workflow =
            asm::AssemblerWorkflowError::InvalidRequest(invalid_request.clone());
        assert_eq!(
            invalid_request_workflow.kind(),
            asm::AssemblerWorkflowErrorKind::InvalidRequest
        );
        assert_eq!(
            invalid_request_workflow
                .as_invalid_request()
                .expect("invalid request payload")
                .summary(),
            "runtime model is unavailable"
        );
        assert_eq!(
            invalid_request_workflow.code(),
            "asm.workflow.invalid_request"
        );

        let host_io = asm::HostIoError::new("asm.workflow.io", "failed to write listing");
        assert_eq!(host_io.code(), "asm.workflow.io");
        assert_eq!(host_io.summary(), "failed to write listing");
        assert_eq!(host_io.to_string(), "failed to write listing");
        let host_io_workflow = asm::AssemblerWorkflowError::Io(host_io.clone());
        assert_eq!(host_io_workflow.kind(), asm::AssemblerWorkflowErrorKind::Io);
        assert_eq!(
            host_io_workflow.as_io().expect("io payload").summary(),
            "failed to write listing"
        );
        assert_eq!(host_io_workflow.to_string(), "failed to write listing");

        let internal =
            asm::InternalErrorReport::new("asm.workflow.internal", "processor contract failed");
        assert_eq!(internal.code(), "asm.workflow.internal");
        assert_eq!(internal.summary(), "processor contract failed");
        assert_eq!(internal.to_string(), "processor contract failed");
        let internal_workflow = asm::AssemblerWorkflowError::Internal(internal.clone());
        assert_eq!(
            internal_workflow.kind(),
            asm::AssemblerWorkflowErrorKind::Internal
        );
        assert_eq!(
            internal_workflow
                .as_internal()
                .expect("internal payload")
                .code(),
            "asm.workflow.internal"
        );
        assert_eq!(internal_workflow.summary(), "processor contract failed");
    }

    #[test]
    fn public_borrowed_asm_workflow_wraps_failed_assembly_path() {
        let source_provider = io::MemorySourceProvider::new().with_file(
            "/virtual/main.asm",
            ".module main\n.this_is_not_a_real_directive\n.endmodule\n",
        );
        let output_sink = io::MemoryOutputSink::new();

        let err = match Assembler::builder(Path::new("/virtual/main.asm"))
            .output_base("/virtual/main")
            .output_format(asm::OutputFormat::Text)
            .label_output_format(asm::LabelOutputFormat::Vice)
            .source_provider(&source_provider)
            .output_sink(&output_sink)
            .assemble()
        {
            Ok(_) => panic!("invalid borrowed assembly should fail"),
            Err(err) => err,
        };

        assert_eq!(err.kind(), asm::AssemblerWorkflowErrorKind::Assemble);
        assert_eq!(err.code(), "asm.workflow.assemble");
        assert_eq!(
            err.as_assemble().expect("assemble payload").kind(),
            diagnostics::AsmErrorKind::Assembler
        );
    }

    #[test]
    fn public_owned_asm_workflow_wraps_failed_assembly_path() {
        let source_provider = io::MemorySourceProvider::new().with_file(
            "/virtual/main.asm",
            ".module main\n.this_is_not_a_real_directive\n.endmodule\n",
        );
        let output_sink = io::MemoryOutputSink::new();

        let err = match AssemblerSession::builder("/virtual/main.asm")
            .output_base("/virtual/main")
            .output_format(asm::OutputFormat::Text)
            .label_output_format(asm::LabelOutputFormat::Vice)
            .source_provider(source_provider.clone())
            .output_sink(output_sink.clone())
            .assemble()
        {
            Ok(_) => panic!("invalid owned assembly should fail"),
            Err(err) => err,
        };

        assert_eq!(err.kind(), asm::AssemblerWorkflowErrorKind::Assemble);
        assert_eq!(err.code(), "asm.workflow.assemble");
        assert_eq!(
            err.as_assemble().expect("assemble payload").kind(),
            diagnostics::AsmErrorKind::Assembler
        );
    }

    #[test]
    fn public_asm_workflow_lockstep_failed_assembly_preserves_assemble_category() {
        for continuation_head in [
            lockstep::ContinuationHead::Rust,
            lockstep::ContinuationHead::Vm,
        ] {
            let source_provider = io::MemorySourceProvider::new().with_file(
                "/virtual/main.asm",
                ".module main\n.this_is_not_a_real_directive\n.endmodule\n",
            );
            let borrowed_output_sink = io::MemoryOutputSink::new();
            let owned_output_sink = io::MemoryOutputSink::new();

            let borrowed_err = match Assembler::with_config(
                Path::new("/virtual/main.asm"),
                AssembleOptions {
                    execution_mode: lockstep::ExecutionMode::Lockstep { continuation_head },
                    output_base: "/virtual/main",
                    output_format: asm::OutputFormat::Text,
                    label_output_format: asm::LabelOutputFormat::Vice,
                    source_provider: Some(&source_provider),
                    output_sink: Some(&borrowed_output_sink),
                    ..AssembleOptions::default()
                },
            )
            .assemble()
            {
                Ok(_) => panic!("lockstep borrowed assembly should fail"),
                Err(err) => err,
            };

            assert_eq!(
                borrowed_err.kind(),
                asm::AssemblerWorkflowErrorKind::Assemble
            );
            assert_eq!(borrowed_err.code(), "asm.workflow.assemble");
            assert_eq!(
                borrowed_err
                    .as_assemble()
                    .expect("borrowed assemble payload")
                    .kind(),
                diagnostics::AsmErrorKind::Assembler
            );

            let owned_err = match AssemblerSession::builder("/virtual/main.asm")
                .execution_mode(lockstep::ExecutionMode::Lockstep { continuation_head })
                .output_base("/virtual/main")
                .output_format(asm::OutputFormat::Text)
                .label_output_format(asm::LabelOutputFormat::Vice)
                .source_provider(source_provider.clone())
                .output_sink(owned_output_sink)
                .assemble()
            {
                Ok(_) => panic!("lockstep owned assembly should fail"),
                Err(err) => err,
            };

            assert_eq!(owned_err.kind(), asm::AssemblerWorkflowErrorKind::Assemble);
            assert_eq!(owned_err.code(), "asm.workflow.assemble");
            assert_eq!(
                owned_err
                    .as_assemble()
                    .expect("owned assemble payload")
                    .kind(),
                diagnostics::AsmErrorKind::Assembler
            );
        }
    }

    #[test]
    fn public_asm_workflow_invalid_argument_category_survives_borrowed_and_owned_paths() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\nnop\n.endmodule\n");
        let borrowed_output_sink = io::MemoryOutputSink::new();
        let owned_output_sink = io::MemoryOutputSink::new();

        let borrowed_err = match Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: "/virtual/main",
                output_format: asm::OutputFormat::Text,
                cpu_override: Some("definitely-not-a-real-cpu"),
                label_output_format: asm::LabelOutputFormat::Vice,
                source_provider: Some(&source_provider),
                output_sink: Some(&borrowed_output_sink),
                ..AssembleOptions::default()
            },
        )
        .assemble()
        {
            Ok(_) => panic!("borrowed invalid-argument assembly should fail"),
            Err(err) => err,
        };
        assert_eq!(
            borrowed_err.kind(),
            asm::AssemblerWorkflowErrorKind::InvalidArgument
        );

        let owned_err = match AssemblerSession::builder("/virtual/main.asm")
            .output_base("/virtual/main")
            .output_format(asm::OutputFormat::Text)
            .label_output_format(asm::LabelOutputFormat::Vice)
            .cpu_override("definitely-not-a-real-cpu")
            .source_provider(source_provider)
            .output_sink(owned_output_sink)
            .assemble()
        {
            Ok(_) => panic!("owned invalid-argument assembly should fail"),
            Err(err) => err,
        };
        assert_eq!(
            owned_err.kind(),
            asm::AssemblerWorkflowErrorKind::InvalidArgument
        );
    }

    #[test]
    fn public_asm_workflow_io_category_survives_borrowed_and_owned_paths() {
        let source_provider = io::MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\nnop\n.endmodule\n");
        let failing_output = FailingOutputSink;

        let borrowed_err = match Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: "/virtual/main",
                output_format: asm::OutputFormat::Text,
                label_output_format: asm::LabelOutputFormat::Vice,
                source_provider: Some(&source_provider),
                output_sink: Some(&failing_output),
                ..AssembleOptions::default()
            },
        )
        .assemble()
        {
            Ok(_) => panic!("borrowed failing output should fail"),
            Err(err) => err,
        };
        assert_eq!(borrowed_err.kind(), asm::AssemblerWorkflowErrorKind::Io);
        assert_eq!(borrowed_err.code(), "asm.workflow.io");

        let owned_err = match AssemblerSession::builder("/virtual/main.asm")
            .output_base("/virtual/main")
            .output_format(asm::OutputFormat::Text)
            .label_output_format(asm::LabelOutputFormat::Vice)
            .source_provider(source_provider)
            .output_sink(failing_output)
            .assemble()
        {
            Ok(_) => panic!("owned failing output should fail"),
            Err(err) => err,
        };
        assert_eq!(owned_err.kind(), asm::AssemblerWorkflowErrorKind::Io);
        assert_eq!(owned_err.code(), "asm.workflow.io");
    }

    #[test]
    fn public_asm_workflow_minimal_core_and_processor_mappings_are_stable() {
        let core_error = opcore::CoreError::from(
            opcore::tokenize_line(".if \"unterminated", 1)
                .expect_err("tokenize failure should produce a core error"),
        );
        let core_workflow = super::map_core_error_to_workflow(core_error);
        assert_eq!(
            core_workflow.kind(),
            asm::AssemblerWorkflowErrorKind::Assemble
        );
        assert_eq!(
            core_workflow
                .as_assemble()
                .expect("assemble payload")
                .kind(),
            diagnostics::AsmErrorKind::Parser
        );

        let processor_error = processing::ProcessorError::new(
            "asm",
            processing::ProcessorErrorKind::InvalidRequest,
            "processing.runtime_model.unavailable",
            "VM tokenizer runtime model is unavailable",
            vec![processing::ProcessorFailureDetail::new(
                "processing.runtime_model.unavailable",
                "VM tokenizer runtime model is unavailable",
                Some("execution_mode"),
            )],
        );
        let processor_workflow = super::map_processor_error_to_workflow(processor_error);
        assert_eq!(
            processor_workflow.kind(),
            asm::AssemblerWorkflowErrorKind::InvalidRequest
        );
        assert_eq!(
            processor_workflow.code(),
            "processing.runtime_model.unavailable"
        );
        assert_eq!(
            processor_workflow.summary(),
            "VM tokenizer runtime model is unavailable"
        );

        let internal_error = processing::ProcessorError::new(
            "asm",
            processing::ProcessorErrorKind::Internal,
            "processing.internal",
            "processor contract failed",
            Vec::new(),
        );
        let internal_workflow = super::map_processor_error_to_workflow(internal_error);
        assert_eq!(
            internal_workflow.kind(),
            asm::AssemblerWorkflowErrorKind::Internal
        );
        assert_eq!(internal_workflow.code(), "processing.internal");

        let diagnostic_error = processing::ProcessorError::new(
            "asm",
            processing::ProcessorErrorKind::ProcessorDiagnostic,
            "processing.processor_diagnostic",
            "processor parse failed",
            Vec::new(),
        );
        let diagnostic_workflow = super::map_processor_error_to_workflow(diagnostic_error);
        assert_eq!(
            diagnostic_workflow.kind(),
            asm::AssemblerWorkflowErrorKind::Assemble
        );
        assert_eq!(
            diagnostic_workflow
                .as_assemble()
                .expect("assemble payload")
                .kind(),
            diagnostics::AsmErrorKind::Assembler
        );
    }

    #[test]
    fn public_asm_workflow_preserves_diagnostic_error_payload_structure() {
        let source_provider = io::MemorySourceProvider::new().with_file(
            "/virtual/main.asm",
            ".module main\nstart:\n    .byte MISSING_VALUE\n.endmodule\n",
        );
        let output_sink = io::MemoryOutputSink::new();

        let err = match Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: "/virtual/main",
                output_format: asm::OutputFormat::Text,
                label_output_format: asm::LabelOutputFormat::Vice,
                source_provider: Some(&source_provider),
                output_sink: Some(&output_sink),
                ..AssembleOptions::default()
            },
        )
        .assemble()
        {
            Ok(_) => panic!("assembly should fail"),
            Err(err) => err,
        };

        let assemble_err = err.as_assemble().expect("assemble payload");
        let diagnostic = &assemble_err.diagnostics()[0];
        assert_eq!(diagnostic.severity, diagnostics::Severity::Error);
        assert_eq!(
            diagnostic.error.kind(),
            diagnostics::AsmErrorKind::Expression
        );
        assert!(diagnostic.error.message().contains("MISSING_VALUE"));
    }

    #[test]
    fn public_processing_api_routes_module_item_without_unstable() {
        match processing::route_module_item_line(".module demo", 1) {
            Ok((ast, trace)) => {
                assert!(matches!(ast, Some(opcore::LineAst::Statement(..))));
                assert_eq!(
                    trace.requests(),
                    &[processing::ProcessingRequestKind::Opcore(
                        processing::OpcoreRequestKind::ModuleItem
                    )]
                );
            }
            Err(err) => match err {
                processing::EngineError::Processor(err) => {
                    assert_eq!(err.processor_id(), "asm");
                    assert_eq!(err.kind(), processing::ProcessorErrorKind::InvalidRequest);
                    assert_eq!(err.code(), "processing.runtime_model.unavailable");
                    assert_eq!(err.summary(), "VM tokenizer runtime model is unavailable");
                }
                other => panic!("expected processor error, got {other:?}"),
            },
        }
    }

    #[test]
    fn public_processing_api_default_helpers_split_core_and_processor_runtime_model_contracts() {
        let route_result = processing::route_module_item_line(".module demo", 1);
        let editor_result = processing::editor_route_line(".module demo", 1);

        match (route_result, editor_result) {
            (Ok((route_ast, route_trace)), Ok((editor_ast, _editor_trace))) => {
                assert!(matches!(route_ast, Some(opcore::LineAst::Statement(..))));
                assert!(matches!(editor_ast, opcore::LineAst::Statement(..)));
                assert_eq!(
                    route_trace.requests(),
                    &[processing::ProcessingRequestKind::Opcore(
                        processing::OpcoreRequestKind::ModuleItem
                    )]
                );
            }
            (Err(route_err), Err(editor_err)) => {
                match route_err {
                    processing::EngineError::Processor(err) => {
                        assert_eq!(err.processor_id(), "asm");
                        assert_eq!(err.kind(), processing::ProcessorErrorKind::InvalidRequest);
                        assert_eq!(err.code(), "processing.runtime_model.unavailable");
                        assert_eq!(err.summary(), "VM tokenizer runtime model is unavailable");
                        assert_eq!(err.details().len(), 1);
                    }
                    other => panic!("expected route processor error, got {other:?}"),
                }
                match editor_err {
                    processing::EngineError::Processor(err) => {
                        assert_eq!(err.processor_id(), "asm");
                        assert_eq!(err.kind(), processing::ProcessorErrorKind::InvalidRequest);
                        assert_eq!(err.code(), "processing.runtime_model.unavailable");
                        assert_eq!(err.summary(), "VM tokenizer runtime model is unavailable");
                        assert_eq!(err.details().len(), 1);
                    }
                    other => panic!("expected processor error, got {other:?}"),
                }
            }
            (route, editor) => {
                panic!("default processing helpers diverged: route={route:?}, editor={editor:?}")
            }
        }
    }

    #[test]
    fn public_processing_api_exports_module_item_with_model_inputs_through_facade() {
        let asm_registry = registry::default_asm_registry();
        let model = processing::HierarchyExecutionModel::from_registry(&asm_registry)
            .expect("runtime model should build");
        let register_checker = processing::register_checker_none();

        let (ast, trace) = processing::route_module_item_line_with_model(
            &model,
            "8085",
            None,
            ".module demo",
            1,
            &register_checker,
        )
        .expect("facade-only module-item with-model path should be usable");

        assert!(matches!(ast, Some(opcore::LineAst::Statement(..))));
        assert_eq!(
            trace.requests(),
            &[processing::ProcessingRequestKind::Opcore(
                processing::OpcoreRequestKind::ModuleItem
            )]
        );
    }

    #[test]
    fn public_processing_api_routes_core_failures_through_core_error() {
        let asm_registry = registry::default_asm_registry();
        let model = processing::HierarchyExecutionModel::from_registry(&asm_registry)
            .expect("runtime model should build");
        let register_checker = processing::register_checker_none();
        let err = processing::editor_route_line_with_model(
            &model,
            "m6502",
            None,
            ".if \"unterminated",
            1,
            &register_checker,
        )
        .expect_err("invalid opcore conditional should fail");

        match err {
            processing::EngineError::Core(err) => {
                assert_eq!(err.kind(), opcore::CoreErrorKind::Conditional);
                assert_eq!(err.code(), "opcore.conditional");
            }
            other => panic!("expected core error, got {other:?}"),
        }
    }

    #[test]
    fn public_processing_api_routes_module_item_failures_through_specific_core_error() {
        let asm_registry = registry::default_asm_registry();
        let model = processing::HierarchyExecutionModel::from_registry(&asm_registry)
            .expect("runtime model should build");
        let register_checker = processing::register_checker_none();
        let err = processing::route_module_item_line_with_model(
            &model,
            "8085",
            None,
            ".use",
            1,
            &register_checker,
        )
        .expect_err("invalid use directive should fail");

        match err {
            processing::EngineError::Core(err) => {
                assert_eq!(err.kind(), opcore::CoreErrorKind::Use);
                assert_eq!(err.code(), "opcore.use");
            }
            other => panic!("expected core error, got {other:?}"),
        }
    }

    #[test]
    fn public_processing_api_exports_with_model_inputs_through_facade() {
        let asm_registry = registry::default_asm_registry();
        let model = processing::HierarchyExecutionModel::from_registry(&asm_registry)
            .expect("runtime model should build");
        let register_checker = processing::register_checker_none();

        let (ast, trace) = processing::editor_route_line_with_model(
            &model,
            "8085",
            None,
            ".module demo",
            1,
            &register_checker,
        )
        .expect("facade-only with-model path should be usable");

        assert!(matches!(ast, opcore::LineAst::Statement(..)));
        assert_eq!(
            trace.requests(),
            &[processing::ProcessingRequestKind::Opcore(
                processing::OpcoreRequestKind::Statement
            )]
        );
    }

    #[test]
    fn public_processing_processor_error_surface_exposes_stable_inspection() {
        let detail = processing::ProcessorFailureDetail::new(
            "processing.processor_diagnostic",
            "processor parse failed",
            Some("operand"),
        );
        let err = processing::ProcessorError::new(
            "asm",
            processing::ProcessorErrorKind::ProcessorDiagnostic,
            "processing.processor_diagnostic",
            "processor parse failed",
            vec![detail],
        );

        assert_eq!(err.processor_id(), "asm");
        assert_eq!(
            err.kind(),
            processing::ProcessorErrorKind::ProcessorDiagnostic
        );
        assert_eq!(err.code(), "processing.processor_diagnostic");
        assert_eq!(err.summary(), "processor parse failed");
        assert_eq!(err.details().len(), 1);
        assert_eq!(err.details()[0].code(), "processing.processor_diagnostic");
        assert_eq!(err.details()[0].summary(), "processor parse failed");
        assert_eq!(err.details()[0].field(), Some("operand"));
    }

    #[test]
    fn public_registry_api_queries_capabilities_without_unstable() {
        let asm_registry = registry::default_asm_registry();
        let snapshot = registry::CapabilitySnapshot::from_registry(&asm_registry);
        let resolved = registry::resolve_target_cpu(
            &asm_registry,
            Some("8085"),
            registry::CpuType::new("8085"),
        )
        .expect("registered cpu should resolve");
        let view = snapshot
            .view_for_cpu(resolved)
            .expect("capability view should exist");
        assert_eq!(view.family_id, "intel8080");
        assert!(
            !view.mnemonics.is_empty(),
            "expected populated cpu capabilities"
        );
    }

    #[test]
    fn public_opasm_api_processes_statement_without_unstable() {
        let result = asm::opasm::process_statement(
            asm::opasm::StatementRequest::new(".module demo", 1),
            None,
        )
        .expect("opasm statement processing should succeed");

        assert!(matches!(result.parsed.ast, opcore::LineAst::Statement(..)));
        assert_eq!(
            result.trace.requests(),
            &[processing::ProcessingRequestKind::Processor {
                processor: "asm".to_string(),
                kind: "statement".to_string(),
            }]
        );
    }

    #[test]
    fn public_opasm_api_tokenizes_and_parses_statement_without_unstable() {
        let tokenized =
            asm::opasm::tokenize_statement(asm::opasm::StatementRequest::new(".byte 1, 2", 1))
                .expect("opasm tokenization");
        assert!(!tokenized.tokens.is_empty(), "expected statement tokens");

        let parsed =
            asm::opasm::parse_statement(asm::opasm::StatementRequest::new(".byte 1, 2", 1))
                .expect("opasm parse should succeed");

        match parsed.ast {
            opcore::LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some(".byte"));
                assert_eq!(statement.operands.len(), 2);
            }
            other => panic!("expected statement AST, got {other:?}"),
        }
    }

    #[test]
    fn public_portable_opasm_api_tokenizes_and_parses_statement() {
        let tokenized = asm::opasm::portable::tokenize_statement(
            asm::opasm::StatementRequest::new(".byte 1, 2", 1),
        )
        .expect("portable opasm tokenization");
        assert!(!tokenized.tokens.is_empty(), "expected statement tokens");

        let parsed = asm::opasm::portable::parse_statement(asm::opasm::StatementRequest::new(
            ".byte 1, 2",
            1,
        ))
        .expect("portable opasm parse should succeed");

        match parsed.ast {
            opcore::portable::PortableLineAst::Statement {
                mnemonic, operands, ..
            } => {
                assert_eq!(mnemonic.as_deref(), Some(".byte"));
                assert_eq!(operands.len(), 2);
            }
            other => panic!("expected portable statement AST, got {other:?}"),
        }
    }

    #[test]
    fn public_opasm_processor_builder_supports_rust_processing() {
        let processor = asm::opasm::Processor::builder()
            .build()
            .expect("rust-mode processor");
        let result = processor
            .process_statement(".module demo", 1)
            .expect("processor rust statement");

        assert!(matches!(result.parsed.ast, opcore::LineAst::Statement(..)));
    }

    #[test]
    fn public_portable_opasm_processor_supports_lockstep_processing() {
        let processor = asm::opasm::portable::ProcessorBuilder::new()
            .execution_mode(asm::ExecutionMode::Lockstep {
                continuation_head: asm::ContinuationHead::Rust,
            })
            .cpu_id("m6502")
            .build()
            .expect("lockstep opasm processor");
        let result =
            asm::opasm::portable::process_statement_with_processor(&processor, "    lda #$42", 1)
                .expect("portable processor lockstep statement");

        assert!(matches!(
            result.parsed.ast,
            opcore::portable::PortableLineAst::Statement { .. }
        ));
        assert!(!result.lockstep_report.matches().is_empty());
    }

    #[test]
    fn public_api_check_suppresses_metadata_and_output_only_flags() {
        let source_provider = io::MemorySourceProvider::new().with_file(
            "/virtual/main.asm",
            ".module main\n    .meta\n        .output\n            .list\n            .hex \"meta-hex\"\n            .bin \"0000:0000\"\n        .endoutput\n    .endmeta\n    .org 0\n    nop\n.endmodule\n",
        );
        let output_sink = io::MemoryOutputSink::new();
        let bin_specs = vec![asm::BinOutputSpec {
            name: Some("explicit.bin".to_string()),
            range: None,
        }];
        let labels_path = PathBuf::from("/virtual/symbols.lbl");
        let dependency_policy = asm::DependencyOutputPolicy {
            path: PathBuf::from("/virtual/main.d"),
            append: false,
            make_phony: false,
        };

        let report = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: "/virtual/main",
                output_format: asm::OutputFormat::Text,
                go_addr: Some("0000"),
                bin_specs: &bin_specs,
                fill_byte: 0xff,
                fill_byte_set: true,
                labels_file: Some(&labels_path),
                dependency_output: Some(&dependency_policy),
                label_output_format: asm::LabelOutputFormat::Vice,
                source_provider: Some(&source_provider),
                output_sink: Some(&output_sink),
                ..AssembleOptions::default()
            },
        )
        .check()
        .expect("check should ignore output-only configuration");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(missing_text(&output_sink, "/virtual/main.lst"));
        assert!(missing_text(&output_sink, "/virtual/main.hex"));
        assert!(missing_text(&output_sink, "/virtual/meta-hex.hex"));
        assert!(output_sink.bytes("/virtual/explicit.bin").is_none());
        assert!(missing_text(&output_sink, "/virtual/symbols.lbl"));
        assert!(missing_text(&output_sink, "/virtual/main.d"));
    }

    #[test]
    fn public_api_check_suppresses_all_artifact_classes() {
        let source_provider = io::MemorySourceProvider::new().with_file(
            "/virtual/main.asm",
            ".module main\n\n.region ram, $1000, $10ff\n\n.section code\n.pub\nstart\n    .byte $42, $43\n.priv\n.endsection\n\n.place code in ram\n\n.output \"build/minimal.bin\", format=bin, sections=code\n.mapfile \"build/minimal.map\", symbols=public\n.exportsections dir=\"build/minimal_sections\", format=bin\n\n.endmodule\n",
        );
        let output_sink = io::MemoryOutputSink::new();
        let out_dir = PathBuf::from("/virtual/out");
        let labels_path = PathBuf::from("/virtual/out/symbols.lbl");
        let dependency_policy = asm::DependencyOutputPolicy {
            path: PathBuf::from("/virtual/out/main.d"),
            append: false,
            make_phony: false,
        };

        let report = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: "/virtual/main",
                out_dir: Some(&out_dir),
                output_format: asm::OutputFormat::Text,
                labels_file: Some(&labels_path),
                dependency_output: Some(&dependency_policy),
                label_output_format: asm::LabelOutputFormat::Vice,
                source_provider: Some(&source_provider),
                output_sink: Some(&output_sink),
                ..AssembleOptions::default()
            },
        )
        .check()
        .expect("check should suppress every artifact class");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        assert!(missing_text(&output_sink, "/virtual/out/main.lst"));
        assert!(missing_text(&output_sink, "/virtual/out/main.hex"));
        assert!(output_sink
            .bytes("/virtual/out/build/minimal.bin")
            .is_none());
        assert!(missing_text(&output_sink, "/virtual/out/build/minimal.map"));
        assert!(missing_text(&output_sink, "/virtual/out/symbols.lbl"));
        assert!(missing_text(&output_sink, "/virtual/out/main.d"));
        assert!(!output_sink
            .directories()
            .iter()
            .any(|path| path == &PathBuf::from("/virtual/out/build/minimal_sections")));
    }

    #[test]
    fn public_api_failed_assembly_suppresses_success_path_artifacts() {
        let source_provider = io::MemorySourceProvider::new().with_file(
            "/virtual/main.asm",
            ".module main\n.region ram, $1000, $10ff\n.section code\n.pub\nstart\n    .byte MISSING_VALUE\n.priv\n.endsection\n.place code in ram\n.output \"build/minimal.bin\", format=bin, sections=code\n.mapfile \"build/minimal.map\", symbols=public\n.exportsections dir=\"build/minimal_sections\", format=bin\n.endmodule\n",
        );
        let output_sink = io::MemoryOutputSink::new();
        let out_dir = PathBuf::from("/virtual/out");
        let labels_path = PathBuf::from("/virtual/out/symbols.lbl");
        let dependency_policy = asm::DependencyOutputPolicy {
            path: PathBuf::from("/virtual/out/main.d"),
            append: false,
            make_phony: false,
        };
        let bin_specs = vec![asm::BinOutputSpec {
            name: Some("explicit.bin".to_string()),
            range: None,
        }];

        let err = match Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: "/virtual/main",
                out_dir: Some(&out_dir),
                output_format: asm::OutputFormat::Text,
                bin_specs: &bin_specs,
                labels_file: Some(&labels_path),
                dependency_output: Some(&dependency_policy),
                label_output_format: asm::LabelOutputFormat::Vice,
                source_provider: Some(&source_provider),
                output_sink: Some(&output_sink),
                ..AssembleOptions::default()
            },
        )
        .assemble()
        {
            Ok(_) => panic!("assembly should fail"),
            Err(err) => err,
        };

        let assemble_err = err.as_assemble().expect("assemble failure payload");
        assert!(
            !assemble_err.diagnostics().is_empty(),
            "expected source diagnostics: {:?}",
            assemble_err.diagnostics()
        );
        assert!(missing_text(&output_sink, "/virtual/out/main.hex"));
        assert!(output_sink.bytes("/virtual/out/explicit.bin").is_none());
        assert!(output_sink
            .bytes("/virtual/out/build/minimal.bin")
            .is_none());
        assert!(missing_text(&output_sink, "/virtual/out/build/minimal.map"));
        assert!(missing_text(&output_sink, "/virtual/out/symbols.lbl"));
        assert!(missing_text(&output_sink, "/virtual/out/main.d"));
        assert!(!output_sink
            .directories()
            .iter()
            .any(|path| path == &PathBuf::from("/virtual/out/build/minimal_sections")));
    }

    #[test]
    fn public_api_creates_parent_directories_for_labels_and_dependency_outputs() {
        let temp_dir = unique_temp_dir("libopforge-nested-output-paths");
        let source_path = temp_dir.join("main.asm");
        let output_base_owned = temp_dir.join("build/main");
        let labels_path = temp_dir.join("artifacts/labels/symbols.lbl");
        let dependency_path = temp_dir.join("artifacts/deps/main.d");
        let dependency_policy = asm::DependencyOutputPolicy {
            path: dependency_path.clone(),
            append: false,
            make_phony: false,
        };

        fs::write(&source_path, ".module main\nstart:\n    nop\n.endmodule\n")
            .expect("write source");

        let report = Assembler::with_config(
            source_path.as_path(),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: output_base_owned.to_str().expect("output base utf8"),
                output_format: asm::OutputFormat::Text,
                labels_file: Some(&labels_path),
                dependency_output: Some(&dependency_policy),
                label_output_format: asm::LabelOutputFormat::Vice,
                ..AssembleOptions::default()
            },
        )
        .assemble()
        .expect("assembly should succeed");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        let labels_text = fs::read_to_string(&labels_path).expect("read labels");
        assert!(labels_text.contains("main.start"), "labels:\n{labels_text}");
        let dependency_text = fs::read_to_string(&dependency_path).expect("read deps");
        let canonical_source_path = fs::canonicalize(&source_path).expect("canonical source");
        assert!(
            dependency_text.contains(stable_path_string(&canonical_source_path).as_str()),
            "deps:\n{dependency_text}"
        );
    }

    #[test]
    fn public_api_dependency_output_includes_labels_target() {
        let source_provider = io::MemorySourceProvider::new().with_file(
            "/virtual/main.asm",
            ".module main\nstart:\n    nop\n.endmodule\n",
        );
        let output_sink = io::MemoryOutputSink::new();
        let labels_path = PathBuf::from("/virtual/out/symbols.lbl");
        let dependency_policy = asm::DependencyOutputPolicy {
            path: PathBuf::from("/virtual/out/main.d"),
            append: false,
            make_phony: false,
        };

        let report = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: "/virtual/out/main",
                output_format: asm::OutputFormat::Text,
                labels_file: Some(&labels_path),
                dependency_output: Some(&dependency_policy),
                label_output_format: asm::LabelOutputFormat::Vice,
                source_provider: Some(&source_provider),
                output_sink: Some(&output_sink),
                ..AssembleOptions::default()
            },
        )
        .assemble()
        .expect("assembly should emit labels and dependency output");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        let dependency_text = expect_text(&output_sink, &dependency_policy.path);
        assert!(
            dependency_text.contains("/virtual/out/main.lst"),
            "deps:\n{dependency_text}"
        );
        assert!(
            dependency_text.contains("/virtual/out/main.hex"),
            "deps:\n{dependency_text}"
        );
        assert!(
            dependency_text.contains(stable_path_string(&labels_path).as_str()),
            "deps:\n{dependency_text}"
        );
    }

    #[test]
    fn public_api_dependency_output_includes_directive_driven_targets() {
        let source_provider = io::MemorySourceProvider::new().with_file(
            "/virtual/main.asm",
            ".module main\n.region ram, $1000, $10ff\n.section code\n.pub\nstart\n    .byte $42, $43\n.priv\n.endsection\n.place code in ram\n.mapfile \"build/minimal.map\", symbols=public\n.exportsections dir=\"build/minimal_sections\", format=bin\n.endmodule\n",
        );
        let output_sink = io::MemoryOutputSink::new();
        let dependency_policy = asm::DependencyOutputPolicy {
            path: PathBuf::from("/virtual/out/main.d"),
            append: false,
            make_phony: false,
        };

        let report = Assembler::with_config(
            Path::new("/virtual/main.asm"),
            AssembleOptions {
                execution_mode: lockstep::ExecutionMode::Vm,
                output_base: "/virtual/out/main",
                out_dir: Some(Path::new("/virtual/out")),
                output_format: asm::OutputFormat::Text,
                dependency_output: Some(&dependency_policy),
                source_provider: Some(&source_provider),
                output_sink: Some(&output_sink),
                ..AssembleOptions::default()
            },
        )
        .assemble()
        .expect("assembly should emit directive-driven outputs and dependency output");

        assert_eq!(report.error_count(), 0, "{:?}", report.diagnostics());
        let dependency_text = expect_text(&output_sink, &dependency_policy.path);
        assert!(
            dependency_text.contains("/virtual/out/build/minimal.map"),
            "deps:\n{dependency_text}"
        );
        assert!(
            dependency_text.contains("/virtual/out/build/minimal_sections/code.bin"),
            "deps:\n{dependency_text}"
        );
    }
}
