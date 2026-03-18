// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Error types, diagnostics, and reporting for the assembler.

use std::fmt;
use std::sync::Arc;

use opcore::parser::ParseError;
use types::lockstep::LockstepReport;
use types::processing::LineProcessingTrace;

pub use types::assembler::{LineStatus, PassCounts};
pub use types::diagnostics::{
    build_context_lines, AsmError, AsmErrorKind, Diagnostic, DiagnosticCode, DiagnosticLevel,
    Fixit, LabeledSpan, ParseDiagnostic, Severity, SourceLocation,
};

/// Convert parser errors into a stable diagnostic payload without coupling
/// `types` to parser internals.
pub fn parse_error_to_diagnostic(parse_error: ParseError) -> ParseDiagnostic {
    ParseDiagnostic {
        message: parse_error.message,
        location: SourceLocation {
            file: None,
            line: parse_error.span.line,
            col_start: Some(parse_error.span.col_start),
            col_end: Some(parse_error.span.col_end),
        },
    }
}

/// Report from a successful assembly run.
pub struct AsmRunReport {
    diagnostics: Vec<Diagnostic>,
    source_lines: Arc<Vec<String>>,
    runtime_processing_traces: Vec<(u8, u32, LineProcessingTrace)>,
    lockstep_report: LockstepReport,
}

impl AsmRunReport {
    pub fn new(
        diagnostics: Vec<Diagnostic>,
        source_lines: impl Into<Arc<Vec<String>>>,
        runtime_processing_traces: Vec<(u8, u32, LineProcessingTrace)>,
    ) -> Self {
        Self {
            diagnostics,
            source_lines: source_lines.into(),
            runtime_processing_traces,
            lockstep_report: LockstepReport::default(),
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn source_lines(&self) -> &[String] {
        &self.source_lines
    }

    pub fn runtime_processing_traces(&self) -> &[(u8, u32, LineProcessingTrace)] {
        &self.runtime_processing_traces
    }

    pub fn lockstep_report(&self) -> &LockstepReport {
        &self.lockstep_report
    }

    pub fn with_lockstep_report(mut self, lockstep_report: LockstepReport) -> Self {
        self.lockstep_report = lockstep_report;
        self
    }

    pub fn error_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .count()
    }

    pub fn warning_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .count()
    }
}

/// Error from a failed assembly run.
#[derive(Debug)]
pub struct AsmRunError {
    error: AsmError,
    diagnostics: Vec<Diagnostic>,
    source_lines: Arc<Vec<String>>,
    runtime_processing_traces: Vec<(u8, u32, LineProcessingTrace)>,
    lockstep_report: Box<LockstepReport>,
}

impl AsmRunError {
    pub fn new(
        error: AsmError,
        diagnostics: Vec<Diagnostic>,
        source_lines: impl Into<Arc<Vec<String>>>,
    ) -> Self {
        Self::new_with_traces(error, diagnostics, source_lines, Vec::new())
    }

    pub fn new_with_traces(
        error: AsmError,
        diagnostics: Vec<Diagnostic>,
        source_lines: impl Into<Arc<Vec<String>>>,
        runtime_processing_traces: Vec<(u8, u32, LineProcessingTrace)>,
    ) -> Self {
        Self {
            error,
            diagnostics,
            source_lines: source_lines.into(),
            runtime_processing_traces,
            lockstep_report: Box::new(LockstepReport::default()),
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn error(&self) -> &AsmError {
        &self.error
    }

    pub fn kind(&self) -> AsmErrorKind {
        self.error.kind()
    }

    pub fn summary(&self) -> &str {
        self.error.message()
    }

    pub fn source_lines(&self) -> &[String] {
        &self.source_lines
    }

    pub fn runtime_processing_traces(&self) -> &[(u8, u32, LineProcessingTrace)] {
        &self.runtime_processing_traces
    }

    pub fn lockstep_report(&self) -> &LockstepReport {
        &self.lockstep_report
    }

    pub fn with_lockstep_report(mut self, lockstep_report: LockstepReport) -> Self {
        self.lockstep_report = Box::new(lockstep_report);
        self
    }
}

impl fmt::Display for AsmRunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.error)
    }
}

impl std::error::Error for AsmRunError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_error_conversion_preserves_span_and_message() {
        let parse_error = ParseError {
            message: "otp004: unexpected token".to_string(),
            span: opcore::tokenizer::Span {
                line: 5,
                col_start: 3,
                col_end: 4,
            },
        };

        let converted = parse_error_to_diagnostic(parse_error);
        assert_eq!(converted.message, "otp004: unexpected token");
        assert_eq!(converted.location.line, 5);
        assert_eq!(converted.location.col_start, Some(3));
        assert_eq!(converted.location.col_end, Some(4));
    }

    #[test]
    fn build_context_lines_falls_back_when_no_source_available() {
        let lines = build_context_lines(3, None, None, None, false);
        assert_eq!(lines, vec!["    3 | <source unavailable>".to_string()]);
    }
}
