// SPDX-License-Identifier: GPL-3.0-or-later

//! Diagnostic and assembler error model types.

use std::fmt;

/// Categories of assembler errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmErrorKind {
    Assembler,
    Cli,
    Conditional,
    Directive,
    Expression,
    Instruction,
    Io,
    Parser,
    Preprocess,
    Symbol,
}

/// An assembler error with a kind and message.
#[derive(Debug, Clone)]
pub struct AsmError {
    kind: AsmErrorKind,
    message: String,
}

impl AsmError {
    pub fn new(kind: AsmErrorKind, msg: &str, param: Option<&str>) -> Self {
        Self {
            kind,
            message: format_error(msg, param),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn kind(&self) -> AsmErrorKind {
        self.kind
    }
}

impl fmt::Display for AsmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for AsmError {}

/// Severity level for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Warning,
    Error,
}

/// Backward-compatible severity alias during extraction.
pub type Severity = DiagnosticLevel;

/// A structured diagnostic code wrapper.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticCode(pub String);

impl DiagnosticCode {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

/// Source location information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub file: Option<String>,
    pub line: u32,
    pub col_start: Option<usize>,
    pub col_end: Option<usize>,
}

/// Parse diagnostic payload used to enrich assembler diagnostics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseDiagnostic {
    pub message: String,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LabeledSpan {
    pub file: Option<String>,
    pub line: u32,
    pub col_start: Option<usize>,
    pub col_end: Option<usize>,
    pub label: Option<String>,
    pub is_primary: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fixit {
    pub file: Option<String>,
    pub line: u32,
    pub col_start: Option<usize>,
    pub col_end: Option<usize>,
    pub replacement: String,
    pub applicability: String,
}

/// A diagnostic message with location and context.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub line: u32,
    pub column: Option<usize>,
    pub col_end: Option<usize>,
    pub code: String,
    pub severity: Severity,
    pub error: AsmError,
    pub file: Option<String>,
    pub source: Option<String>,
    pub parser_error: Option<ParseDiagnostic>,
    pub related_spans: Vec<LabeledSpan>,
    pub notes: Vec<String>,
    pub help: Vec<String>,
    pub fixits: Vec<Fixit>,
}

impl Diagnostic {
    pub fn new(line: u32, severity: Severity, error: AsmError) -> Self {
        Self {
            line,
            column: None,
            col_end: None,
            code: default_diagnostic_code(error.kind()).to_string(),
            severity,
            error,
            file: None,
            source: None,
            parser_error: None,
            related_spans: Vec::new(),
            notes: Vec::new(),
            help: Vec::new(),
            fixits: Vec::new(),
        }
    }

    pub fn with_column(mut self, column: Option<usize>) -> Self {
        self.column = column;
        self
    }

    pub fn with_col_end(mut self, col_end: Option<usize>) -> Self {
        self.col_end = col_end;
        self
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = code.into();
        self
    }

    pub fn with_file(mut self, file: Option<String>) -> Self {
        self.file = file;
        self
    }

    pub fn with_source(mut self, source: Option<String>) -> Self {
        self.source = source;
        self
    }

    pub fn with_parser_error(mut self, parser_error: Option<ParseDiagnostic>) -> Self {
        self.parser_error = parser_error;
        if let Some(parser_error) = &self.parser_error {
            if self.column.is_none() {
                self.column = parser_error.location.col_start;
            }
            if self.col_end.is_none() {
                self.col_end = parser_error.location.col_end;
            }
            if self.related_spans.is_empty() {
                self.related_spans.push(LabeledSpan {
                    file: parser_error
                        .location
                        .file
                        .clone()
                        .or_else(|| self.file.clone()),
                    line: parser_error.location.line,
                    col_start: parser_error.location.col_start,
                    col_end: parser_error.location.col_end,
                    label: Some("while parsing this statement".to_string()),
                    is_primary: true,
                });
            }
            if let Some((code, message)) = split_prefixed_diagnostic(parser_error.message.as_str())
            {
                self.code = code.to_string();
                self.error.message = message.to_string();
            } else if let Some(code) = infer_parser_diagnostic_code(parser_error.message.as_str()) {
                self.code = code.to_string();
            }
            if self.fixits.is_empty() {
                if let Some(fixit) = parser_error_default_fixit(&self, parser_error) {
                    self.fixits.push(fixit);
                }
            }
        }
        self
    }

    pub fn with_related_span(mut self, span: LabeledSpan) -> Self {
        self.related_spans.push(span);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help.push(help.into());
        self
    }

    pub fn with_fixit(mut self, fixit: Fixit) -> Self {
        self.fixits.push(fixit);
        self
    }

    pub fn format(&self) -> String {
        let sev = match self.severity {
            Severity::Warning => "WARNING",
            Severity::Error => "ERROR",
        };
        format!(
            "{}: {} [{}] - {}",
            self.line,
            sev,
            self.code,
            self.error.message()
        )
    }

    pub fn format_with_context(&self, lines: Option<&[String]>, use_color: bool) -> String {
        let sev = match self.severity {
            Severity::Warning => "WARNING",
            Severity::Error => "ERROR",
        };
        let header = match &self.file {
            Some(file) => format!("{file}:{}: {sev} [{}]", self.line, self.code),
            None => format!("{}: {sev} [{}]", self.line, self.code),
        };

        let mut out = String::new();
        out.push_str(&header);
        out.push('\n');

        let context = build_context_lines(
            self.line,
            self.column,
            lines,
            self.source.as_deref(),
            use_color,
        );
        for line in context {
            out.push_str(&line);
            out.push('\n');
        }

        for related in self.related_spans.iter().filter(|span| !span.is_primary) {
            let ctx = build_context_lines(related.line, related.col_start, lines, None, use_color);
            for line in ctx {
                out.push_str("      = ");
                out.push_str(line.trim_start());
                out.push('\n');
            }
            if let Some(label) = &related.label {
                out.push_str("      = note: ");
                out.push_str(label);
                out.push('\n');
            }
        }

        for note in &self.notes {
            out.push_str("note: ");
            out.push_str(note);
            out.push('\n');
        }

        for help in &self.help {
            out.push_str("help: ");
            out.push_str(help);
            out.push('\n');
        }

        for fixit in &self.fixits {
            out.push_str("suggestion: replace ");
            out.push_str(&format_span_bounds(
                fixit.line,
                fixit.col_start,
                fixit.col_end,
            ));
            out.push_str(" with ");
            out.push_str(&format!("{:?}", fixit.replacement));
            out.push('\n');
        }

        out.push_str(&format!("{sev}: {}", self.error.message()));
        out
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn code(&self) -> &str {
        self.code.as_str()
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> Option<usize> {
        self.column
    }

    pub fn col_end(&self) -> Option<usize> {
        self.col_end
    }

    pub fn file(&self) -> Option<&str> {
        self.file.as_deref()
    }

    pub fn message(&self) -> &str {
        self.error.message()
    }

    pub fn related_spans(&self) -> &[LabeledSpan] {
        &self.related_spans
    }

    pub fn notes(&self) -> &[String] {
        &self.notes
    }

    pub fn help(&self) -> &[String] {
        &self.help
    }

    pub fn fixits(&self) -> &[Fixit] {
        &self.fixits
    }
}

/// Build context lines for error display.
pub fn build_context_lines(
    line_num: u32,
    column: Option<usize>,
    lines: Option<&[String]>,
    source_override: Option<&str>,
    use_color: bool,
) -> Vec<String> {
    let mut out = Vec::new();
    let line_idx = line_num.saturating_sub(1) as usize;

    if let Some(source) = source_override {
        let highlighted = highlight_line(source, column, use_color);
        out.push(format!("{:>5} | {}", line_num, highlighted));
        return out;
    }

    let lines = match lines {
        Some(lines) if !lines.is_empty() => lines,
        _ => {
            out.push(format!("{:>5} | <source unavailable>", line_num));
            return out;
        }
    };

    if line_idx >= lines.len() {
        let marker = if line_idx == lines.len() {
            "<end of file>"
        } else {
            "<source unavailable>"
        };
        out.push(format!("{:>5} | {marker}", line_num));
        return out;
    }

    let line = &lines[line_idx];
    let display = highlight_line(line, column, use_color);
    out.push(format!("{:>5} | {}", line_num, display));

    out
}

fn highlight_line(line: &str, column: Option<usize>, use_color: bool) -> String {
    match column {
        Some(col) if col > 0 => {
            let idx = col - 1;
            if idx >= line.len() {
                if use_color {
                    return format!("{line}\x1b[31m^\x1b[0m");
                }
                return format!("{line}\n{}^", " ".repeat(line.len()));
            }
            let (head, tail) = line.split_at(idx);
            let ch = tail.chars().next().unwrap_or(' ');
            let rest = &tail[ch.len_utf8()..];
            if use_color {
                format!("{head}\x1b[31m{ch}\x1b[0m{rest}")
            } else {
                format!("{head}{ch}{rest}\n{}^", " ".repeat(idx))
            }
        }
        _ => line.to_string(),
    }
}

fn split_prefixed_diagnostic(message: &str) -> Option<(&str, &str)> {
    let (code, tail) = message.split_once(':')?;
    let code = code.trim();
    if code.len() < 6 || code.len() > 8 {
        return None;
    }
    let prefix_len = code
        .chars()
        .take_while(|ch| ch.is_ascii_alphabetic())
        .count();
    if prefix_len < 2 {
        return None;
    }
    let digits = &code[prefix_len..];
    if digits.len() != 3 || !digits.chars().all(|ch| ch.is_ascii_digit()) {
        return None;
    }
    Some((code, tail.trim_start()))
}

fn infer_parser_diagnostic_code(message: &str) -> Option<&'static str> {
    let lower = message.to_ascii_lowercase();
    if lower.contains("unexpected token") {
        Some("otp001")
    } else if lower.contains("expected expression") {
        Some("otp002")
    } else if lower.contains("expected operand") {
        Some("otp003")
    } else {
        None
    }
}

fn parser_error_default_fixit(diag: &Diagnostic, parser_error: &ParseDiagnostic) -> Option<Fixit> {
    match diag.code.as_str() {
        "otp002" | "otp003" => Some(Fixit {
            file: diag.file.clone(),
            line: parser_error.location.line,
            col_start: parser_error.location.col_start,
            col_end: parser_error.location.col_start,
            replacement: "0".to_string(),
            applicability: "maybe-incorrect".to_string(),
        }),
        _ => None,
    }
}

fn format_span_bounds(line: u32, col_start: Option<usize>, col_end: Option<usize>) -> String {
    match (col_start, col_end) {
        (Some(start), Some(end)) => format!("{line}:{start}-{end}"),
        (Some(start), None) => format!("{line}:{start}"),
        _ => format!("{line}"),
    }
}

fn default_diagnostic_code(kind: AsmErrorKind) -> &'static str {
    match kind {
        AsmErrorKind::Assembler => "asm001",
        AsmErrorKind::Cli => "asm101",
        AsmErrorKind::Conditional => "asm201",
        AsmErrorKind::Directive => "asm202",
        AsmErrorKind::Expression => "asm401",
        AsmErrorKind::Instruction => "asm402",
        AsmErrorKind::Io => "asm501",
        AsmErrorKind::Parser => "otp004",
        AsmErrorKind::Preprocess => "asm102",
        AsmErrorKind::Symbol => "asm301",
    }
}

fn format_error(msg: &str, param: Option<&str>) -> String {
    match param {
        Some(p) => format!("{msg}: {p}"),
        None => msg.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagnostic_format_includes_line_and_severity() {
        let err = AsmError::new(AsmErrorKind::Assembler, "Bad thing", None);
        let diag = Diagnostic::new(12, Severity::Error, err);
        assert_eq!(diag.format(), "12: ERROR [asm001] - Bad thing");
    }

    #[test]
    fn with_parser_error_applies_code_and_fixit_defaults() {
        let err = AsmError::new(AsmErrorKind::Parser, "unexpected", None);
        let parser_error = ParseDiagnostic {
            message: "otp002: expected expression".to_string(),
            location: SourceLocation {
                file: None,
                line: 4,
                col_start: Some(7),
                col_end: Some(8),
            },
        };

        let diag = Diagnostic::new(4, Severity::Error, err).with_parser_error(Some(parser_error));

        assert_eq!(diag.code(), "otp002");
        assert_eq!(diag.message(), "expected expression");
        assert_eq!(diag.fixits().len(), 1);
    }
}
