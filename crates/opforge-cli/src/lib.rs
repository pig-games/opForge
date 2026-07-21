// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use std::fs::OpenOptions;
use std::hash::{Hash, Hasher};
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use clap::Parser;
use serde_json::json;

use api::asm::AssemblerWorkflowError;
use api::diagnostics::{build_context_lines, AsmRunError, Diagnostic, Severity};
use cli_core::{
    resolve_formatter_input_paths, resolve_formatter_project_paths, run_with_cli_with_context,
    validate_cli, Cli, CliConfig, CliRunError, CliRunReport, DiagnosticsSinkConfig,
    DiagnosticsStyle, FormatterMode as CliFormatterMode, OutputFormat, BUILD_PROFILE_SUMMARY,
    VERSION,
};

struct DiagnosticsSink {
    writer: Option<Box<dyn Write>>,
}

impl DiagnosticsSink {
    fn from_config(config: &DiagnosticsSinkConfig) -> io::Result<Self> {
        match config {
            DiagnosticsSinkConfig::Disabled => Ok(Self { writer: None }),
            DiagnosticsSinkConfig::Stderr => Ok(Self {
                writer: Some(Box::new(io::stderr())),
            }),
            DiagnosticsSinkConfig::File { path, append } => {
                let mut opts = OpenOptions::new();
                opts.create(true).write(true);
                if *append {
                    opts.append(true);
                } else {
                    opts.truncate(true);
                }
                let file = opts.open(path)?;
                Ok(Self {
                    writer: Some(Box::new(file)),
                })
            }
        }
    }

    fn emit_line(&mut self, line: &str) -> io::Result<()> {
        if let Some(writer) = &mut self.writer {
            writeln!(writer, "{line}")?;
        }
        Ok(())
    }

    fn emit_diagnostics(
        &mut self,
        source_lines: Option<&[String]>,
        diagnostics: &[Diagnostic],
        use_color: bool,
        format: OutputFormat,
        style: DiagnosticsStyle,
    ) -> io::Result<()> {
        for diag in diagnostics {
            self.emit_line(&format_diagnostic_line(
                diag,
                source_lines,
                use_color,
                format,
                style,
            ))?;
        }
        Ok(())
    }
}

fn severity_to_str(severity: Severity) -> &'static str {
    match severity {
        Severity::Warning => "warning",
        Severity::Error => "error",
    }
}

fn format_diagnostic_line(
    diag: &Diagnostic,
    source_lines: Option<&[String]>,
    use_color: bool,
    format: OutputFormat,
    style: DiagnosticsStyle,
) -> String {
    if format == OutputFormat::Json {
        json!({
            "code": diag.code(),
            "severity": severity_to_str(diag.severity()),
            "message": diag.message(),
            "file": diag.file(),
            "line": diag.line(),
            "col_start": diag.column(),
            "col_end": diag.col_end(),
            "related_spans": diagnostic_related_spans_json(diag),
            "notes": diag.notes(),
            "help": diag.help(),
            "fixits": diagnostic_fixits_json(diag),
        })
        .to_string()
    } else if style == DiagnosticsStyle::Classic {
        format_diagnostic_line_classic(diag, source_lines, use_color)
    } else {
        diag.format_with_context(source_lines, use_color)
    }
}

fn diagnostic_related_spans_json(diag: &Diagnostic) -> Vec<serde_json::Value> {
    diag.related_spans()
        .iter()
        .map(|span| {
            json!({
                "file": span.file.clone(),
                "line": span.line,
                "col_start": span.col_start,
                "col_end": span.col_end,
                "label": span.label.clone(),
                "is_primary": span.is_primary,
            })
        })
        .collect()
}

fn diagnostic_fixits_json(diag: &Diagnostic) -> Vec<serde_json::Value> {
    diag.fixits()
        .iter()
        .map(|fixit| {
            json!({
                "file": fixit.file.clone(),
                "line": fixit.line,
                "col_start": fixit.col_start,
                "col_end": fixit.col_end,
                "replacement": fixit.replacement.clone(),
                "applicability": fixit.applicability.clone(),
            })
        })
        .collect()
}

fn format_diagnostic_line_classic(
    diag: &Diagnostic,
    source_lines: Option<&[String]>,
    use_color: bool,
) -> String {
    let sev = match diag.severity() {
        Severity::Warning => "WARNING",
        Severity::Error => "ERROR",
    };
    let header = match diag.file() {
        Some(file) => format!("{file}:{}: {sev} [{}]", diag.line(), diag.code()),
        None => format!("{}: {sev} [{}]", diag.line(), diag.code()),
    };
    let mut out = String::new();
    out.push_str(&header);
    out.push('\n');
    for line in build_context_lines(diag.line(), diag.column(), source_lines, None, use_color) {
        out.push_str(&line);
        out.push('\n');
    }
    out.push_str(&format!("{sev}: {}", diag.message()));
    out
}

#[derive(Debug, Clone)]
struct PlannedFixit {
    file: PathBuf,
    line: u32,
    col_start: usize,
    col_end: usize,
    replacement: String,
    applicability: String,
}

#[derive(Debug, Clone)]
struct FileGuard {
    len: u64,
    content_hash: u64,
}

#[derive(Debug, Clone, Copy)]
struct LineByteRange {
    start: usize,
    content_end: usize,
}

#[derive(Debug)]
struct ResolvedFixit {
    start: usize,
    end: usize,
    replacement: Vec<u8>,
}

#[derive(Debug, Clone)]
struct FixitActionResult {
    planned: Vec<PlannedFixit>,
    applied: bool,
    failure: Option<String>,
}

fn merge_fixit_action_result(
    result: FixitActionResult,
    planned: &mut Vec<PlannedFixit>,
    applied: &mut bool,
    saw_planned_fixits: &mut bool,
    failure: &mut Option<String>,
) {
    *saw_planned_fixits |= !result.planned.is_empty();
    if !result.planned.is_empty() && !result.applied {
        *applied = false;
    }
    planned.extend(result.planned);
    if let Some(message) = result.failure {
        *applied = false;
        *failure = Some(message);
    }
}

fn write_fixit_report_with_failure_tracking(
    sink: &mut DiagnosticsSink,
    cli_config: &CliConfig,
    planned: &[PlannedFixit],
    applied: bool,
    failure: &mut Option<String>,
) -> Result<(), String> {
    let Some(path) = cli_config.fixits_output.as_deref() else {
        return Ok(());
    };
    if let Err(err) = write_fixit_report(path, planned, applied) {
        let message = format!("fixits: failed to write report: {err}");
        sink.emit_line(&message).map_err(|sink_err| {
            format!("diagnostics sink write failed while reporting `{message}`: {sink_err}")
        })?;
        if failure.is_none() {
            *failure = Some(message);
        }
    }
    Ok(())
}

fn collect_machine_applicable_fixits(
    diagnostics: &[Diagnostic],
    fallback_file: Option<&Path>,
) -> Vec<PlannedFixit> {
    let mut planned = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for diag in diagnostics {
        for fixit in diag.fixits() {
            if !fixit
                .applicability
                .eq_ignore_ascii_case("machine-applicable")
            {
                continue;
            }
            let file_path = match fixit.file.as_deref() {
                Some(path) => PathBuf::from(path),
                None => match fallback_file {
                    Some(path) => path.to_path_buf(),
                    None => continue,
                },
            };
            let col_start = fixit.col_start.unwrap_or(1).max(1);
            let col_end = fixit.col_end.unwrap_or(fixit.col_start.unwrap_or(1)).max(1);
            let dedup_key = (
                file_path.clone(),
                fixit.line,
                col_start,
                col_end,
                fixit.replacement.clone(),
                fixit.applicability.to_ascii_lowercase(),
            );
            if !seen.insert(dedup_key) {
                continue;
            }
            planned.push(PlannedFixit {
                file: file_path,
                line: fixit.line,
                col_start,
                col_end,
                replacement: fixit.replacement.clone(),
                applicability: fixit.applicability.clone(),
            });
        }
    }
    planned
}

fn with_fallback_file(
    diagnostics: Vec<Diagnostic>,
    fallback_file: Option<&Path>,
) -> Vec<Diagnostic> {
    let fallback = fallback_file.map(|path| path.to_string_lossy().to_string());
    diagnostics
        .into_iter()
        .map(|diag| {
            if diag.file().is_none() {
                diag.with_file(fallback.clone())
            } else {
                diag
            }
        })
        .collect()
}

fn filter_recoverable_diagnostics(
    diagnostics: &[Diagnostic],
    emit_warnings: bool,
) -> Vec<Diagnostic> {
    diagnostics
        .iter()
        .filter(|diag| emit_warnings || diag.severity() != Severity::Warning)
        .cloned()
        .collect()
}

fn prepare_recoverable_diagnostics(
    diagnostics: &[Diagnostic],
    cli_config: &CliConfig,
    fallback: Option<&Path>,
) -> Vec<Diagnostic> {
    let diagnostics =
        filter_recoverable_diagnostics(diagnostics, cli_config.warning_policy.emit_warnings);
    with_fallback_file(diagnostics, fallback)
}

fn fatal_error_is_represented(diagnostics: &[Diagnostic], error: &AsmRunError) -> bool {
    diagnostics.iter().any(|diagnostic| {
        diagnostic.severity() == Severity::Error && diagnostic.message() == error.summary()
    })
}

fn prepare_terminal_failure_diagnostics(
    error: &AsmRunError,
    cli_config: &CliConfig,
    fallback: Option<&Path>,
) -> Vec<Diagnostic> {
    let mut diagnostics =
        prepare_recoverable_diagnostics(error.diagnostics(), cli_config, fallback);
    if !fatal_error_is_represented(&diagnostics, error) {
        let line = error.source_lines().len().saturating_add(1) as u32;
        diagnostics.push(
            Diagnostic::new(line, Severity::Error, error.error().clone())
                .with_file(fallback.map(|path| path.to_string_lossy().to_string())),
        );
    }
    diagnostics
}

fn emit_recoverable_diagnostics(
    sink: &mut DiagnosticsSink,
    source_lines: &[String],
    diagnostics: &[Diagnostic],
    cli_config: &CliConfig,
    use_color: bool,
) -> Result<(), String> {
    sink.emit_diagnostics(
        Some(source_lines),
        diagnostics,
        use_color,
        cli_config.output_format,
        cli_config.diagnostics_style,
    )
    .map_err(|err| format!("diagnostics sink write failed: {err}"))
}

fn fixits_have_overlaps(fixits: &[PlannedFixit]) -> bool {
    let mut by_file: std::collections::HashMap<&Path, Vec<&PlannedFixit>> =
        std::collections::HashMap::new();
    for fixit in fixits {
        by_file.entry(fixit.file.as_path()).or_default().push(fixit);
    }
    for edits in by_file.values_mut() {
        edits.sort_by_key(|edit| (edit.line, edit.col_start, edit.col_end));
        for pair in edits.windows(2) {
            let left = pair[0];
            let right = pair[1];
            if left.line == right.line && right.col_start < left.col_end {
                return true;
            }
        }
    }
    false
}

fn compute_file_guard(path: &Path) -> io::Result<FileGuard> {
    let content = std::fs::read(path)?;
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    content.hash(&mut hasher);
    Ok(FileGuard {
        len: content.len() as u64,
        content_hash: hasher.finish(),
    })
}

fn capture_fixit_guards(
    fixits: &[PlannedFixit],
) -> io::Result<std::collections::HashMap<PathBuf, FileGuard>> {
    let mut guards = std::collections::HashMap::new();
    for fixit in fixits {
        guards
            .entry(fixit.file.clone())
            .or_insert(compute_file_guard(fixit.file.as_path())?);
    }
    Ok(guards)
}

fn verify_fixit_guards(
    guards: &std::collections::HashMap<PathBuf, FileGuard>,
    path: &Path,
) -> io::Result<()> {
    let Some(expected) = guards.get(path) else {
        return Ok(());
    };
    let current = compute_file_guard(path)?;
    if expected.len != current.len || expected.content_hash != current.content_hash {
        return Err(io::Error::other(format!(
            "stale source detected before applying fixits for {}",
            path.display()
        )));
    }
    Ok(())
}

fn collect_line_byte_ranges(content: &[u8]) -> Vec<LineByteRange> {
    let mut ranges = Vec::new();
    let mut line_start = 0usize;
    let mut idx = 0usize;

    while idx < content.len() {
        while idx < content.len() && content[idx] != b'\n' && content[idx] != b'\r' {
            idx += 1;
        }
        let content_end = idx;
        if idx < content.len() {
            if content[idx] == b'\r' && idx + 1 < content.len() && content[idx + 1] == b'\n' {
                idx += 2;
            } else {
                idx += 1;
            }
        }
        ranges.push(LineByteRange {
            start: line_start,
            content_end,
        });
        line_start = idx;
    }

    ranges
}

fn preferred_newline_sequence(content: &[u8]) -> &'static [u8] {
    let mut idx = 0usize;
    while idx < content.len() {
        match content[idx] {
            b'\r' => {
                if idx + 1 < content.len() && content[idx + 1] == b'\n' {
                    return b"\r\n";
                }
                return b"\r";
            }
            b'\n' => return b"\n",
            _ => idx += 1,
        }
    }
    b"\n"
}

fn ends_with_line_terminator(content: &[u8]) -> bool {
    content.ends_with(b"\n") || content.ends_with(b"\r")
}

fn resolve_fixit_offset(
    content: &[u8],
    line_ranges: &[LineByteRange],
    line: u32,
    column: usize,
) -> io::Result<usize> {
    if line == 0 {
        return Err(io::Error::other("fixits: line numbers are 1-based"));
    }

    if line_ranges.is_empty() && line == 1 {
        return Ok(0);
    }

    let line_index = line.saturating_sub(1) as usize;
    if let Some(range) = line_ranges.get(line_index) {
        let line_len = range.content_end.saturating_sub(range.start);
        return Ok(range.start + column.saturating_sub(1).min(line_len));
    }

    if line_index == line_ranges.len() && column <= 1 {
        return Ok(content.len());
    }

    Err(io::Error::other(format!(
        "fixits: span points outside the original file for line {line}, column {column}"
    )))
}

fn resolve_fixit(
    content: &[u8],
    line_ranges: &[LineByteRange],
    edit: &PlannedFixit,
) -> io::Result<ResolvedFixit> {
    let next_line_index = line_ranges.len() + 1;
    if edit.line as usize == next_line_index && edit.col_start == 1 && edit.col_end == 1 {
        let newline = preferred_newline_sequence(content);
        let mut replacement = Vec::new();
        if !content.is_empty() && !ends_with_line_terminator(content) {
            replacement.extend_from_slice(newline);
        }
        replacement.extend_from_slice(edit.replacement.as_bytes());
        if ends_with_line_terminator(content) {
            replacement.extend_from_slice(newline);
        }
        return Ok(ResolvedFixit {
            start: content.len(),
            end: content.len(),
            replacement,
        });
    }

    let start = resolve_fixit_offset(content, line_ranges, edit.line, edit.col_start)?;
    let end = resolve_fixit_offset(content, line_ranges, edit.line, edit.col_end)?;
    let (start, end) = if start <= end {
        (start, end)
    } else {
        (end, start)
    };

    Ok(ResolvedFixit {
        start,
        end,
        replacement: edit.replacement.as_bytes().to_vec(),
    })
}

fn resolved_fixits_have_overlaps(fixits: &[ResolvedFixit]) -> bool {
    fixits.windows(2).any(|pair| pair[1].start < pair[0].end)
}

fn apply_fixits_in_place(
    fixits: &[PlannedFixit],
    guards: Option<&std::collections::HashMap<PathBuf, FileGuard>>,
) -> io::Result<usize> {
    let mut by_file: std::collections::HashMap<&Path, Vec<&PlannedFixit>> =
        std::collections::HashMap::new();
    for fixit in fixits {
        by_file.entry(fixit.file.as_path()).or_default().push(fixit);
    }

    let mut applied = 0usize;
    for (file, edits) in by_file {
        if let Some(guards) = guards {
            verify_fixit_guards(guards, file)?;
        }
        let original = std::fs::read(file)?;
        let line_ranges = collect_line_byte_ranges(&original);
        let mut resolved = Vec::with_capacity(edits.len());
        for edit in edits {
            resolved.push(resolve_fixit(&original, &line_ranges, edit)?);
        }

        resolved.sort_by_key(|edit| (edit.start, edit.end));
        if resolved_fixits_have_overlaps(&resolved) {
            return Err(io::Error::other(format!(
                "fixits: overlap detected after resolving byte spans for {}",
                file.display()
            )));
        }

        let mut updated = original;
        for edit in resolved.into_iter().rev() {
            updated.splice(edit.start..edit.end, edit.replacement);
            applied += 1;
        }

        std::fs::write(file, updated)?;
    }
    Ok(applied)
}

fn handle_fixits(
    sink: &mut DiagnosticsSink,
    cli_config: &CliConfig,
    diagnostics: &[Diagnostic],
    fallback: Option<&Path>,
) -> Result<FixitActionResult, String> {
    if !(cli_config.apply_fixits || cli_config.fixits_dry_run || cli_config.fixits_output.is_some())
    {
        return Ok(FixitActionResult {
            planned: Vec::new(),
            applied: false,
            failure: None,
        });
    }

    let planned = collect_machine_applicable_fixits(diagnostics, fallback);
    let mut applied = false;
    let mut failure = None;

    if fixits_have_overlaps(&planned) {
        let message = "fixits: overlap detected; aborting fixit application".to_string();
        sink.emit_line(&message).map_err(|err| err.to_string())?;
        failure = Some(message);
    } else if cli_config.apply_fixits {
        match capture_fixit_guards(&planned)
            .and_then(|guards| apply_fixits_in_place(&planned, Some(&guards)))
        {
            Ok(applied_count) => {
                sink.emit_line(&format!("fixits: applied {applied_count} edits"))
                    .map_err(|err| err.to_string())?;
                applied = applied_count > 0;
            }
            Err(err) => {
                let message = format!("fixits: apply failed: {err}");
                sink.emit_line(&message).map_err(|err| err.to_string())?;
                failure = Some(message);
            }
        }
    } else if cli_config.fixits_dry_run {
        sink.emit_line(&format!("fixits: dry-run planned {} edits", planned.len()))
            .map_err(|err| err.to_string())?;
    }

    Ok(FixitActionResult {
        planned,
        applied,
        failure,
    })
}

fn promote_warning_diagnostics(run_report: &CliRunReport) -> Vec<Diagnostic> {
    let promoted: Vec<Diagnostic> = run_report
        .report
        .diagnostics()
        .iter()
        .filter(|diag| diag.severity() == Severity::Warning)
        .map(|diag| {
            let mut promoted = diag.clone();
            promoted.severity = Severity::Error;
            promoted
        })
        .collect();
    with_fallback_file(promoted, Some(run_report.input_path.as_path()))
}

fn write_fixit_report(path: &Path, fixits: &[PlannedFixit], applied: bool) -> io::Result<()> {
    let payload = json!({
        "schema": "opforge-fixits-v2",
        "applied": applied,
        "fixits": fixits.iter().map(|fixit| {
            json!({
                "file": fixit.file.to_string_lossy().to_string(),
                "line": fixit.line,
                "col_start": fixit.col_start,
                "col_end": fixit.col_end,
                "replacement": fixit.replacement,
                "applicability": fixit.applicability,
            })
        }).collect::<Vec<_>>(),
    });
    let mut serialized = serde_json::to_string_pretty(&payload).map_err(io::Error::other)?;
    serialized.push('\n');
    std::fs::write(path, serialized)
}

fn process_successful_reports(
    sink: &mut DiagnosticsSink,
    cli_config: &CliConfig,
    reports: &[CliRunReport],
    use_color: bool,
) -> Result<(), String> {
    let mut failure = None;
    let mut planned = Vec::new();
    let mut applied = cli_config.apply_fixits;
    let mut saw_planned_fixits = false;

    for run_report in reports {
        let fallback = Some(run_report.input_path.as_path());
        let diagnostics =
            prepare_recoverable_diagnostics(run_report.report.diagnostics(), cli_config, fallback);
        if !cli_config.quiet {
            emit_recoverable_diagnostics(
                sink,
                run_report.report.source_lines(),
                &diagnostics,
                cli_config,
                use_color,
            )?;
        }
        let result = handle_fixits(sink, cli_config, &diagnostics, fallback)?;
        merge_fixit_action_result(
            result,
            &mut planned,
            &mut applied,
            &mut saw_planned_fixits,
            &mut failure,
        );
    }

    if !saw_planned_fixits {
        applied = false;
    }

    write_fixit_report_with_failure_tracking(sink, cli_config, &planned, applied, &mut failure)?;

    match failure {
        Some(message) => Err(message),
        None => Ok(()),
    }
}

fn process_failed_assembly_run(
    sink: &mut DiagnosticsSink,
    cli_config: &CliConfig,
    reports: &[CliRunReport],
    input_path: Option<&Path>,
    error: &AsmRunError,
    use_color: bool,
) -> Result<(), String> {
    let mut failure = None;
    let mut planned = Vec::new();
    let mut applied = cli_config.apply_fixits;
    let mut saw_planned_fixits = false;

    for run_report in reports {
        let fallback = Some(run_report.input_path.as_path());
        let diagnostics =
            prepare_recoverable_diagnostics(run_report.report.diagnostics(), cli_config, fallback);
        if !cli_config.quiet {
            emit_recoverable_diagnostics(
                sink,
                run_report.report.source_lines(),
                &diagnostics,
                cli_config,
                use_color,
            )?;
        }
        let result = handle_fixits(sink, cli_config, &diagnostics, fallback)?;
        merge_fixit_action_result(
            result,
            &mut planned,
            &mut applied,
            &mut saw_planned_fixits,
            &mut failure,
        );
    }

    let fallback = input_path;
    let diagnostics = prepare_terminal_failure_diagnostics(error, cli_config, fallback);
    emit_recoverable_diagnostics(
        sink,
        error.source_lines(),
        &diagnostics,
        cli_config,
        use_color,
    )?;
    let result = handle_fixits(sink, cli_config, &diagnostics, fallback)?;
    merge_fixit_action_result(
        result,
        &mut planned,
        &mut applied,
        &mut saw_planned_fixits,
        &mut failure,
    );

    if !saw_planned_fixits {
        applied = false;
    }

    write_fixit_report_with_failure_tracking(sink, cli_config, &planned, applied, &mut failure)?;
    match failure {
        Some(message) => Err(message),
        None => Ok(()),
    }
}

fn process_werror_reports(
    sink: &mut DiagnosticsSink,
    cli_config: &CliConfig,
    reports: &[CliRunReport],
    use_color: bool,
) -> Result<(), String> {
    let mut failure = None;
    let mut planned = Vec::new();
    let mut applied = cli_config.apply_fixits;
    let mut saw_planned_fixits = false;

    for run_report in reports {
        let promoted = promote_warning_diagnostics(run_report);
        if promoted.is_empty() {
            continue;
        }
        emit_recoverable_diagnostics(
            sink,
            run_report.report.source_lines(),
            &promoted,
            cli_config,
            use_color,
        )?;
        let result = handle_fixits(
            sink,
            cli_config,
            &promoted,
            Some(run_report.input_path.as_path()),
        )?;
        merge_fixit_action_result(
            result,
            &mut planned,
            &mut applied,
            &mut saw_planned_fixits,
            &mut failure,
        );
    }

    if !saw_planned_fixits {
        applied = false;
    }

    write_fixit_report_with_failure_tracking(sink, cli_config, &planned, applied, &mut failure)?;

    match failure {
        Some(message) => Err(message),
        None => Ok(()),
    }
}

fn emit_workflow_fatal(
    sink: &mut DiagnosticsSink,
    cli_config: &CliConfig,
    error: &AssemblerWorkflowError,
    input_path: Option<&Path>,
) -> Result<(), String> {
    let kind = format!("{:?}", error.kind()).to_ascii_lowercase();
    let line = if cli_config.output_format == OutputFormat::Json {
        json!({
            "type": "fatal",
            "code": error.code(),
            "kind": kind,
            "message": error.summary(),
            "input": input_path.map(|path| path.to_string_lossy().to_string()),
        })
        .to_string()
    } else {
        match input_path {
            Some(path) => format!(
                "{}: error [{}] ({kind})\nerror: {}",
                path.display(),
                error.code(),
                error.summary()
            ),
            None => format!(
                "error [{}] ({kind})\nerror: {}",
                error.code(),
                error.summary()
            ),
        }
    };
    sink.emit_line(&line)
        .map_err(|err| format!("diagnostics sink write failed: {err}"))
}

fn process_run_failure(
    sink: &mut DiagnosticsSink,
    cli_config: &CliConfig,
    run_error: &CliRunError,
    use_color: bool,
) -> Result<(), String> {
    match run_error {
        CliRunError::Assembler {
            reports,
            input_path,
            error,
        } => process_failed_assembly_run(
            sink,
            cli_config,
            reports,
            input_path.as_deref(),
            error.as_ref(),
            use_color,
        ),
        CliRunError::WarningsAsErrors { reports } => {
            process_werror_reports(sink, cli_config, reports, use_color)
        }
        CliRunError::Workflow {
            input_path, error, ..
        } => emit_workflow_fatal(sink, cli_config, error, input_path.as_deref()),
    }
}

fn run_formatter_mode(cli_config: &CliConfig) -> Result<i32, String> {
    let Some(formatter) = cli_config.formatter.as_ref() else {
        return Ok(0);
    };
    let formatter_config = if let Some(path) = formatter.config_path.as_deref() {
        api::formatter::FormatterConfig::load_from_path(path)
            .map_err(|err| format!("formatter config load failed: {err}"))?
    } else {
        api::formatter::FormatterConfig::default()
    };
    let engine = api::formatter::FormatterEngine::new(formatter_config);
    let mode = match formatter.mode {
        CliFormatterMode::Check => api::formatter::FormatMode::Check,
        CliFormatterMode::Write => api::formatter::FormatMode::Write,
        CliFormatterMode::Stdout => api::formatter::FormatMode::Stdout,
    };
    let formatter_paths = if mode == api::formatter::FormatMode::Stdout {
        resolve_formatter_input_paths(cli_config)
    } else {
        resolve_formatter_project_paths(cli_config)
    }
    .map_err(|err| format!("formatter target resolution failed: {err}"))?;

    if mode == api::formatter::FormatMode::Stdout {
        if formatter_paths.len() != 1 {
            return Err("--fmt-stdout requires exactly one resolved source file".to_string());
        }
        let input = &formatter_paths[0];
        let rendered = engine
            .format_path_to_string(input)
            .map_err(|err| format!("formatter read failed: {err}"))?;
        print!("{rendered}");
        return Ok(0);
    }

    let report = engine
        .run_paths_with_report(&formatter_paths, mode)
        .map_err(|err| format!("formatter run failed: {err}"))?;
    let summary = report.summary;

    for file in &report.files {
        for diagnostic in &file.diagnostics {
            eprintln!(
                "fmt warning: {}:{}: {}",
                file.path.display(),
                diagnostic.line_number,
                diagnostic.message
            );
        }
    }

    if cli_config.output_format == OutputFormat::Json {
        println!(
            "{}",
            json!({
                "schema": "formatter-v1",
                "mode": match formatter.mode {
                    CliFormatterMode::Check => "check",
                    CliFormatterMode::Write => "write",
                    CliFormatterMode::Stdout => "stdout",
                },
                "files_seen": summary.files_seen,
                "files_changed": summary.files_changed,
                "warnings": summary.warnings,
                "files_with_warnings": summary.files_with_warnings,
            })
        );
    } else {
        match formatter.mode {
            CliFormatterMode::Check => {
                println!(
                    "fmt: checked {} file(s), {} would change, {} warning(s)",
                    summary.files_seen, summary.files_changed, summary.warnings
                );
            }
            CliFormatterMode::Write => {
                println!(
                    "fmt: processed {} file(s), {} changed, {} warning(s)",
                    summary.files_seen, summary.files_changed, summary.warnings
                );
            }
            CliFormatterMode::Stdout => {}
        }
    }

    if formatter.mode == CliFormatterMode::Check && summary.files_changed > 0 {
        Ok(1)
    } else {
        Ok(0)
    }
}

fn emit_last_resort_failure(cli_config: &CliConfig, message: &str) {
    if !matches!(cli_config.diagnostics_sink, DiagnosticsSinkConfig::Disabled) {
        eprintln!("opForge: {message}");
    }
}

pub fn run_main() {
    let cli = Cli::parse();
    let registry = if cli.print_cpusupport || cli.print_capabilities {
        Some(api::registry::default_asm_registry())
    } else {
        None
    };
    if let Some(path) = &cli.opasm_package {
        if !path.is_file() {
            eprintln!(
                "--opasm-package path does not exist or is not a file: {}",
                path.display()
            );
            std::process::exit(1);
        }
    }
    if cli.print_cpusupport {
        let registry = registry.as_ref().expect("report registry");
        if cli.format == OutputFormat::Json {
            println!("{}", api::registry::cpusupport_report_json(registry));
        } else {
            println!("{}", api::registry::cpusupport_report(registry));
        }
        return;
    }
    if cli.print_capabilities {
        let registry = registry.as_ref().expect("report registry");
        if cli.format == OutputFormat::Json {
            println!(
                "{}",
                api::registry::capabilities_report_json(registry, VERSION, BUILD_PROFILE_SUMMARY)
            );
        } else {
            println!(
                "{}",
                api::registry::capabilities_report(registry, VERSION, BUILD_PROFILE_SUMMARY)
            );
        }
        return;
    }
    let cli_config = match validate_cli(&cli) {
        Ok(config) => config,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };

    if cli_config.formatter.is_some() {
        match run_formatter_mode(&cli_config) {
            Ok(code) => {
                if code != 0 {
                    std::process::exit(code);
                }
                return;
            }
            Err(message) => {
                eprintln!("{message}");
                std::process::exit(1);
            }
        }
    }

    let mut sink = match DiagnosticsSink::from_config(&cli_config.diagnostics_sink) {
        Ok(sink) => sink,
        Err(err) => {
            eprintln!("Failed to open diagnostics sink: {err}");
            std::process::exit(1);
        }
    };

    let use_color = std::env::var("NO_COLOR").is_err();
    match run_with_cli_with_context(&cli) {
        Ok(reports) => {
            if let Err(message) =
                process_successful_reports(&mut sink, &cli_config, &reports, use_color)
            {
                emit_last_resort_failure(&cli_config, &message);
                std::process::exit(1);
            }
        }
        Err(run_error) => {
            if let Err(message) = process_run_failure(&mut sink, &cli_config, &run_error, use_color)
            {
                emit_last_resort_failure(&cli_config, &message);
            }
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{process_run_failure, process_successful_reports, Cli, DiagnosticsSink};
    use api::asm::{AssemblerWorkflowError, HostIoError};
    use api::diagnostics::{
        AsmError, AsmErrorKind, AsmRunError, AsmRunReport, Diagnostic, Severity,
    };
    use clap::Parser;
    use cli_core::{run_with_cli_with_context, validate_cli, CliRunError};
    use serde_json::Value;
    use std::fs;
    use std::io::{self, Write};
    #[cfg(unix)]
    use std::os::unix::fs::PermissionsExt;
    use std::path::{Path, PathBuf};
    use std::process;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};

    static TEMP_DIR_SEQ: AtomicU64 = AtomicU64::new(1);

    struct FailingWriter;

    impl Write for FailingWriter {
        fn write(&mut self, _buf: &[u8]) -> io::Result<usize> {
            Err(io::Error::other("synthetic diagnostics sink failure"))
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    fn unique_temp_dir(prefix: &str) -> PathBuf {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let pid = process::id();
        let seq = TEMP_DIR_SEQ.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("{prefix}-{pid}-{now}-{seq}"));
        fs::create_dir_all(&dir).expect("create temp dir");
        dir
    }

    fn write_text(path: &PathBuf, text: &str) {
        fs::write(path, text).expect("write file");
    }

    fn synthetic_success_report_with_fixits(path: &Path) -> cli_core::CliRunReport {
        let cli = Cli::parse_from(["opforge", "--infile", path.to_str().expect("input path")]);
        let run_error = run_with_cli_with_context(&cli).expect_err("input should fail");
        let error = match run_error {
            CliRunError::Assembler { error, .. } => *error,
            CliRunError::WarningsAsErrors { .. } => {
                panic!("synthetic fixit report should come from assembler failure")
            }
            CliRunError::Workflow { .. } => {
                panic!("synthetic fixit report should not come from workflow failure")
            }
        };
        cli_core::CliRunReport {
            input_path: path.to_path_buf(),
            report: AsmRunReport::new(
                error.diagnostics().to_vec(),
                error.source_lines().to_vec(),
                Vec::new(),
            ),
        }
    }

    fn synthetic_success_report_with_diagnostics(
        path: &Path,
        source_line: &str,
        diagnostics: Vec<Diagnostic>,
    ) -> cli_core::CliRunReport {
        cli_core::CliRunReport {
            input_path: path.to_path_buf(),
            report: AsmRunReport::new(diagnostics, vec![source_line.to_string()], Vec::new()),
        }
    }

    #[test]
    fn quiet_successful_runs_still_write_requested_fixit_report() {
        let temp_dir = unique_temp_dir("cli-quiet-fixits");
        let source_path = temp_dir.join("main.asm");
        let fixits_path = temp_dir.join("fixits.json");
        write_text(&source_path, ".module main\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--quiet",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let reports = run_with_cli_with_context(&cli).expect("successful run");
        let mut sink = DiagnosticsSink { writer: None };

        process_successful_reports(&mut sink, &cli_config, &reports, false)
            .expect("quiet processing should succeed");

        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        assert!(report.contains("\"schema\": \"opforge-fixits-v2\""));
        assert!(report.contains("\"applied\": false"));
    }

    #[test]
    fn failed_runs_apply_fileless_fixits_to_the_originating_input() {
        let temp_dir = unique_temp_dir("cli-failed-fixit-fallback");
        let first_path = temp_dir.join("first.asm");
        let second_path = temp_dir.join("second.asm");
        write_text(&first_path, ".module first\nnop\n.endmodule\n");
        write_text(&second_path, ".module second\n.if 1\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--list",
            "--outfile",
            temp_dir.to_str().expect("temp dir"),
            "--infile",
            first_path.to_str().expect("first path"),
            "--infile",
            second_path.to_str().expect("second path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let run_error = run_with_cli_with_context(&cli).expect_err("assembly should fail");
        let mut sink = DiagnosticsSink { writer: None };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("fixit application should succeed");

        let first_text = fs::read_to_string(&first_path).expect("first file");
        let second_text = fs::read_to_string(&second_path).expect("second file");
        assert_eq!(first_text, ".module first\nnop\n.endmodule\n");
        assert!(
            second_text.ends_with(".endmodule\n.endif\n"),
            "{second_text}"
        );
    }

    #[test]
    fn successful_runs_fail_when_fixit_report_write_fails() {
        let temp_dir = unique_temp_dir("cli-fixit-report-failure");
        let source_path = temp_dir.join("main.asm");
        let fixits_path = temp_dir.join("missing").join("fixits.json");
        write_text(&source_path, ".module main\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let reports = run_with_cli_with_context(&cli).expect("successful run");
        let mut sink = DiagnosticsSink { writer: None };

        let result = process_successful_reports(&mut sink, &cli_config, &reports, false);
        assert!(result.is_err(), "report write failure should propagate");
    }

    #[test]
    fn fixit_reports_record_failed_application_as_not_applied() {
        let temp_dir = unique_temp_dir("cli-fixit-apply-failure");
        let source_path = temp_dir.join("main.asm");
        let fixits_path = temp_dir.join("fixits.json");
        write_text(&source_path, ".module main\n.if 1\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let run_error = run_with_cli_with_context(&cli).expect_err("assembly should fail");
        #[cfg(unix)]
        {
            let mut perms = fs::metadata(&source_path).expect("metadata").permissions();
            perms.set_mode(0o444);
            fs::set_permissions(&source_path, perms).expect("chmod readonly");
        }
        let mut sink = DiagnosticsSink { writer: None };

        let result = process_run_failure(&mut sink, &cli_config, &run_error, false);
        assert!(result.is_err(), "failed fixit application should propagate");

        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        assert!(report.contains("\"applied\": false"), "{report}");
    }

    #[test]
    fn failed_runs_with_zero_machine_applicable_fixits_report_not_applied() {
        let temp_dir = unique_temp_dir("cli-fixit-zero-planned");
        let source_path = temp_dir.join("main.asm");
        let fixits_path = temp_dir.join("fixits.json");
        write_text(&source_path, ".module main\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let run_error = CliRunError::Assembler {
            reports: Vec::new(),
            input_path: Some(source_path.clone()),
            error: Box::new(AsmRunError::new(
                AsmError::new(AsmErrorKind::Assembler, "synthetic failure", None),
                vec![Diagnostic::new(
                    1,
                    Severity::Error,
                    AsmError::new(AsmErrorKind::Assembler, "synthetic failure", None),
                )],
                vec!["nop".to_string()],
            )),
        };
        let mut sink = DiagnosticsSink { writer: None };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("zero-fixit failed runs should still write a report");

        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        let payload: Value = serde_json::from_str(&report).expect("parse fixit report");
        assert_eq!(payload["applied"], false, "{report}");
        assert_eq!(payload["fixits"], Value::Array(Vec::new()), "{report}");
    }

    #[test]
    fn terminal_failure_with_no_diagnostics_renders_the_fatal_summary() {
        let temp_dir = unique_temp_dir("cli-terminal-fatal-empty-diagnostics");
        let source_path = temp_dir.join("missing.asm");
        let diagnostics_path = temp_dir.join("diagnostics.txt");
        let cli = Cli::parse_from([
            "opforge",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let run_error = CliRunError::Assembler {
            reports: Vec::new(),
            input_path: Some(source_path.clone()),
            error: Box::new(AsmRunError::new(
                AsmError::new(AsmErrorKind::Io, "output write failed", None),
                Vec::new(),
                Vec::new(),
            )),
        };
        let mut sink = DiagnosticsSink {
            writer: Some(Box::new(
                fs::File::create(&diagnostics_path).expect("create diagnostics file"),
            )),
        };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("fatal summary should render");
        drop(sink);

        let output = fs::read_to_string(&diagnostics_path).expect("read diagnostics");
        assert!(output.contains("output write failed"), "{output}");
        assert!(
            output.contains(source_path.to_string_lossy().as_ref()),
            "{output}"
        );
    }

    #[test]
    fn terminal_failure_is_rendered_after_unrelated_warnings() {
        let temp_dir = unique_temp_dir("cli-terminal-fatal-warning");
        let source_path = temp_dir.join("input.asm");
        let diagnostics_path = temp_dir.join("diagnostics.txt");
        let cli = Cli::parse_from([
            "opforge",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let warning = Diagnostic::new(
            1,
            Severity::Warning,
            AsmError::new(AsmErrorKind::Assembler, "unrelated warning", None),
        );
        let run_error = CliRunError::Assembler {
            reports: Vec::new(),
            input_path: Some(source_path),
            error: Box::new(AsmRunError::new(
                AsmError::new(AsmErrorKind::Io, "terminal output failure", None),
                vec![warning],
                vec!["nop".to_string()],
            )),
        };
        let mut sink = DiagnosticsSink {
            writer: Some(Box::new(
                fs::File::create(&diagnostics_path).expect("create diagnostics file"),
            )),
        };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("fatal summary should render");
        drop(sink);

        let output = fs::read_to_string(&diagnostics_path).expect("read diagnostics");
        assert!(output.contains("unrelated warning"), "{output}");
        assert!(output.contains("terminal output failure"), "{output}");
    }

    #[test]
    fn terminal_failure_does_not_duplicate_matching_error_diagnostic() {
        let temp_dir = unique_temp_dir("cli-terminal-fatal-dedup");
        let source_path = temp_dir.join("input.asm");
        let diagnostics_path = temp_dir.join("diagnostics.txt");
        let cli = Cli::parse_from([
            "opforge",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let message = "already represented terminal failure";
        let run_error = CliRunError::Assembler {
            reports: Vec::new(),
            input_path: Some(source_path),
            error: Box::new(AsmRunError::new(
                AsmError::new(AsmErrorKind::Io, message, None),
                vec![Diagnostic::new(
                    1,
                    Severity::Error,
                    AsmError::new(AsmErrorKind::Io, message, None),
                )],
                vec!["nop".to_string()],
            )),
        };
        let mut sink = DiagnosticsSink {
            writer: Some(Box::new(
                fs::File::create(&diagnostics_path).expect("create diagnostics file"),
            )),
        };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("matching diagnostic should render");
        drop(sink);

        let output = fs::read_to_string(&diagnostics_path).expect("read diagnostics");
        assert_eq!(output.matches(message).count(), 1, "{output}");
    }

    #[test]
    fn terminal_failure_with_no_diagnostics_emits_json() {
        let temp_dir = unique_temp_dir("cli-terminal-fatal-json");
        let source_path = temp_dir.join("missing.asm");
        let diagnostics_path = temp_dir.join("diagnostics.jsonl");
        let cli = Cli::parse_from([
            "opforge",
            "--format",
            "json",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let run_error = CliRunError::Assembler {
            reports: Vec::new(),
            input_path: Some(source_path.clone()),
            error: Box::new(AsmRunError::new(
                AsmError::new(AsmErrorKind::Io, "json terminal failure", None),
                Vec::new(),
                Vec::new(),
            )),
        };
        let mut sink = DiagnosticsSink {
            writer: Some(Box::new(
                fs::File::create(&diagnostics_path).expect("create diagnostics file"),
            )),
        };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("fatal JSON should render");
        drop(sink);

        let output = fs::read_to_string(&diagnostics_path).expect("read diagnostics");
        let payload: Value = serde_json::from_str(output.trim()).expect("valid fatal JSON");
        assert_eq!(payload["severity"], "error");
        assert_eq!(payload["code"], "asm501");
        assert_eq!(payload["message"], "json terminal failure");
        assert_eq!(payload["file"], source_path.to_string_lossy().as_ref());
    }

    #[test]
    fn failed_diagnostics_sink_propagates_the_write_failure() {
        let temp_dir = unique_temp_dir("cli-failing-diagnostics-sink");
        let source_path = temp_dir.join("input.asm");
        let cli = Cli::parse_from([
            "opforge",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let run_error = CliRunError::Assembler {
            reports: Vec::new(),
            input_path: Some(source_path),
            error: Box::new(AsmRunError::new(
                AsmError::new(AsmErrorKind::Io, "terminal output failure", None),
                Vec::new(),
                Vec::new(),
            )),
        };
        let mut sink = DiagnosticsSink {
            writer: Some(Box::new(FailingWriter)),
        };

        let error = process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect_err("diagnostics sink failure should propagate");
        assert!(error.contains("diagnostics sink write failed"), "{error}");
        assert!(
            error.contains("synthetic diagnostics sink failure"),
            "{error}"
        );
    }

    #[test]
    fn no_error_keeps_terminal_failure_output_intentionally_suppressed() {
        let temp_dir = unique_temp_dir("cli-no-error-terminal-failure");
        let source_path = temp_dir.join("input.asm");
        let cli = Cli::parse_from([
            "opforge",
            "--no-error",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let run_error = CliRunError::Assembler {
            reports: Vec::new(),
            input_path: Some(source_path),
            error: Box::new(AsmRunError::new(
                AsmError::new(AsmErrorKind::Io, "terminal output failure", None),
                Vec::new(),
                Vec::new(),
            )),
        };
        let mut sink = DiagnosticsSink::from_config(&cli_config.diagnostics_sink)
            .expect("disabled diagnostics sink");

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("disabled diagnostics should not create a reporting failure");
    }

    #[test]
    fn workflow_failure_renders_its_stable_code_and_input_path() {
        let temp_dir = unique_temp_dir("cli-workflow-failure-text");
        let source_path = temp_dir.join("input.asm");
        let diagnostics_path = temp_dir.join("diagnostics.txt");
        let cli = Cli::parse_from([
            "opforge",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let run_error = CliRunError::Workflow {
            reports: Vec::new(),
            input_path: Some(source_path.clone()),
            error: Box::new(AssemblerWorkflowError::Io(HostIoError::new(
                "asm.workflow.io",
                "workflow I/O failure",
            ))),
        };
        let mut sink = DiagnosticsSink {
            writer: Some(Box::new(
                fs::File::create(&diagnostics_path).expect("create diagnostics file"),
            )),
        };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("workflow fatal should render");
        drop(sink);

        let output = fs::read_to_string(&diagnostics_path).expect("read diagnostics");
        assert!(output.contains("asm.workflow.io"), "{output}");
        assert!(output.contains("workflow I/O failure"), "{output}");
        assert!(
            output.contains(source_path.to_string_lossy().as_ref()),
            "{output}"
        );
    }

    #[test]
    fn workflow_failure_emits_a_json_fatal_record() {
        let temp_dir = unique_temp_dir("cli-workflow-failure-json");
        let source_path = temp_dir.join("input.asm");
        let diagnostics_path = temp_dir.join("diagnostics.jsonl");
        let cli = Cli::parse_from([
            "opforge",
            "--format",
            "json",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let run_error = CliRunError::Workflow {
            reports: Vec::new(),
            input_path: Some(source_path.clone()),
            error: Box::new(AssemblerWorkflowError::Io(HostIoError::new(
                "asm.workflow.io",
                "workflow JSON failure",
            ))),
        };
        let mut sink = DiagnosticsSink {
            writer: Some(Box::new(
                fs::File::create(&diagnostics_path).expect("create diagnostics file"),
            )),
        };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("workflow fatal JSON should render");
        drop(sink);

        let output = fs::read_to_string(&diagnostics_path).expect("read diagnostics");
        let payload: Value = serde_json::from_str(output.trim()).expect("valid workflow JSON");
        assert_eq!(payload["type"], "fatal");
        assert_eq!(payload["code"], "asm.workflow.io");
        assert_eq!(payload["kind"], "io");
        assert_eq!(payload["message"], "workflow JSON failure");
        assert_eq!(payload["input"], source_path.to_string_lossy().as_ref());
    }

    #[test]
    fn quiet_processing_does_not_require_diagnostics_to_write_fixit_report() {
        let temp_dir = unique_temp_dir("cli-quiet-empty-report");
        let source_path = temp_dir.join("main.asm");
        let fixits_path = temp_dir.join("fixits.json");
        let mut sink = DiagnosticsSink { writer: None };
        write_text(&source_path, ".module main\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--quiet",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let reports = vec![cli_core::CliRunReport {
            input_path: source_path,
            report: AsmRunReport::new(Vec::new(), vec!["nop".to_string()], Vec::new()),
        }];

        process_successful_reports(&mut sink, &cli_config, &reports, false)
            .expect("quiet empty report should succeed");
        assert!(fixits_path.is_file());
        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        assert!(report.contains("\"fixits\": []"));
    }

    #[test]
    fn multi_input_fixit_reports_preserve_planned_fixits_from_every_processed_input() {
        let temp_dir = unique_temp_dir("cli-multi-fixit-report");
        let first_path = temp_dir.join("first.asm");
        let second_path = temp_dir.join("second.asm");
        let fixits_path = temp_dir.join("fixits.json");
        write_text(&first_path, ".module first\n.if 1\nnop\n.endmodule\n");
        write_text(&second_path, ".module second\n.if 1\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--list",
            "--outfile",
            temp_dir.to_str().expect("temp dir"),
            "--infile",
            first_path.to_str().expect("first path"),
            "--infile",
            second_path.to_str().expect("second path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let reports = vec![
            synthetic_success_report_with_fixits(&first_path),
            synthetic_success_report_with_fixits(&second_path),
        ];
        let mut sink = DiagnosticsSink { writer: None };

        process_successful_reports(&mut sink, &cli_config, &reports, false)
            .expect("aggregated fixit reporting should succeed");

        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        let payload: Value = serde_json::from_str(&report).expect("parse fixit report");
        assert_eq!(payload["schema"], "opforge-fixits-v2");
        assert_eq!(payload["applied"], false);
        let fixits = payload["fixits"].as_array().expect("fixits array");
        assert_eq!(fixits.len(), 2, "{report}");
        let files: Vec<&str> = fixits
            .iter()
            .map(|fixit| fixit["file"].as_str().expect("fixit file"))
            .collect();
        assert!(files.contains(&first_path.to_string_lossy().as_ref()));
        assert!(files.contains(&second_path.to_string_lossy().as_ref()));
    }

    #[test]
    fn multi_input_fixit_reports_record_applied_fixits_across_every_processed_input() {
        let temp_dir = unique_temp_dir("cli-multi-fixit-apply");
        let first_path = temp_dir.join("first.asm");
        let second_path = temp_dir.join("second.asm");
        let fixits_path = temp_dir.join("fixits.json");
        write_text(&first_path, ".module first\n.if 1\nnop\n.endmodule\n");
        write_text(&second_path, ".module second\n.if 1\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--list",
            "--outfile",
            temp_dir.to_str().expect("temp dir"),
            "--infile",
            first_path.to_str().expect("first path"),
            "--infile",
            second_path.to_str().expect("second path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let reports = vec![
            synthetic_success_report_with_fixits(&first_path),
            synthetic_success_report_with_fixits(&second_path),
        ];
        let mut sink = DiagnosticsSink { writer: None };

        process_successful_reports(&mut sink, &cli_config, &reports, false)
            .expect("aggregated fixit application should succeed");

        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        let payload: Value = serde_json::from_str(&report).expect("parse fixit report");
        assert_eq!(payload["applied"], true);
        let fixits = payload["fixits"].as_array().expect("fixits array");
        assert_eq!(fixits.len(), 2, "{report}");

        let first_text = fs::read_to_string(&first_path).expect("first file");
        let second_text = fs::read_to_string(&second_path).expect("second file");
        assert!(first_text.ends_with(".endmodule\n.endif\n"), "{first_text}");
        assert!(
            second_text.ends_with(".endmodule\n.endif\n"),
            "{second_text}"
        );
    }

    #[test]
    fn multi_input_fixit_reports_mark_applied_false_when_any_apply_fails() {
        let temp_dir = unique_temp_dir("cli-multi-fixit-partial-failure");
        let first_path = temp_dir.join("first.asm");
        let second_path = temp_dir.join("second.asm");
        let fixits_path = temp_dir.join("fixits.json");
        write_text(&first_path, ".module first\n.if 1\nnop\n.endmodule\n");
        write_text(&second_path, ".module second\n.if 1\nnop\n.endmodule\n");

        #[cfg(unix)]
        {
            let mut perms = fs::metadata(&second_path).expect("metadata").permissions();
            perms.set_mode(0o444);
            fs::set_permissions(&second_path, perms).expect("chmod readonly");
        }

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--list",
            "--outfile",
            temp_dir.to_str().expect("temp dir"),
            "--infile",
            first_path.to_str().expect("first path"),
            "--infile",
            second_path.to_str().expect("second path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let reports = vec![
            synthetic_success_report_with_fixits(&first_path),
            synthetic_success_report_with_fixits(&second_path),
        ];
        let mut sink = DiagnosticsSink { writer: None };

        let result = process_successful_reports(&mut sink, &cli_config, &reports, false);
        assert!(result.is_err(), "partial fixit application should fail");

        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        let payload: Value = serde_json::from_str(&report).expect("parse fixit report");
        assert_eq!(payload["applied"], false, "{report}");
        let fixits = payload["fixits"].as_array().expect("fixits array");
        assert_eq!(fixits.len(), 2, "{report}");

        let first_text = fs::read_to_string(&first_path).expect("first file");
        let second_text = fs::read_to_string(&second_path).expect("second file");
        assert!(first_text.ends_with(".endmodule\n.endif\n"), "{first_text}");
        assert_eq!(second_text, ".module second\n.if 1\nnop\n.endmodule\n");
    }

    #[test]
    fn multi_input_werror_production_path_renders_each_warning_with_its_own_source_line() {
        let temp_dir = unique_temp_dir("cli-werror-production");
        let first_path = temp_dir.join("alpha.asm");
        let second_path = temp_dir.join("beta.asm");
        let out_dir = temp_dir.join("out");
        let diag_path = temp_dir.join("diag.txt");
        fs::create_dir_all(&out_dir).expect("create out dir");
        write_text(&first_path, ".byte 300 ; alpha sentinel\n");
        write_text(&second_path, ".byte 400 ; beta sentinel\n");

        let cli = Cli::parse_from([
            "opforge",
            "--Werror",
            "--list",
            "--outfile",
            out_dir.to_str().expect("out dir"),
            "--infile",
            first_path.to_str().expect("first path"),
            "--infile",
            second_path.to_str().expect("second path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let run_error = run_with_cli_with_context(&cli).expect_err("warnings should fail");
        let mut sink = DiagnosticsSink {
            writer: Some(Box::new(
                fs::File::create(&diag_path).expect("create diag file"),
            )),
        };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("Werror rendering should succeed");
        drop(sink);

        let output = fs::read_to_string(&diag_path).expect("read diag output");
        assert!(
            output.contains("alpha sentinel"),
            "alpha source context missing:\n{output}"
        );
        assert!(
            output.contains("beta sentinel"),
            "beta source context missing:\n{output}"
        );
        assert!(
            output.contains(second_path.to_string_lossy().as_ref()),
            "beta file path missing from Werror output:\n{output}"
        );
    }

    #[test]
    fn werror_fileless_fixits_fall_back_to_the_originating_report_path() {
        let temp_dir = unique_temp_dir("cli-werror-fixit-fallback");
        let first_path = temp_dir.join("alpha.asm");
        let second_path = temp_dir.join("beta.asm");
        write_text(&first_path, ".module alpha\nnop\n.endmodule\n");
        write_text(&second_path, ".module beta\n.if 1\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--list",
            "--outfile",
            temp_dir.to_str().expect("temp dir"),
            "--infile",
            first_path.to_str().expect("first path"),
            "--infile",
            second_path.to_str().expect("second path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let mut second_report = synthetic_success_report_with_fixits(&second_path);
        let warning_diagnostics: Vec<Diagnostic> = second_report
            .report
            .diagnostics()
            .iter()
            .cloned()
            .map(|mut diagnostic| {
                diagnostic.severity = Severity::Warning;
                diagnostic
            })
            .collect();
        second_report.report = AsmRunReport::new(
            warning_diagnostics,
            second_report.report.source_lines().to_vec(),
            Vec::new(),
        );
        let run_error = CliRunError::WarningsAsErrors {
            reports: vec![
                cli_core::CliRunReport {
                    input_path: first_path.clone(),
                    report: AsmRunReport::new(Vec::new(), vec!["nop".to_string()], Vec::new()),
                },
                second_report,
            ],
        };
        let mut sink = DiagnosticsSink { writer: None };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("warning fixit fallback should succeed");

        let first_text = fs::read_to_string(&first_path).expect("first file");
        let second_text = fs::read_to_string(&second_path).expect("second file");
        assert_eq!(first_text, ".module alpha\nnop\n.endmodule\n");
        assert!(
            second_text.ends_with(".endmodule\n.endif\n"),
            "{second_text}"
        );
    }

    #[test]
    fn multi_input_werror_fixit_reports_preserve_planned_fixits_from_every_report() {
        let temp_dir = unique_temp_dir("cli-werror-fixit-report");
        let first_path = temp_dir.join("alpha.asm");
        let second_path = temp_dir.join("beta.asm");
        let fixits_path = temp_dir.join("fixits.json");
        write_text(&first_path, ".module alpha\n.if 1\nnop\n.endmodule\n");
        write_text(&second_path, ".module beta\n.if 1\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--Werror",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--list",
            "--outfile",
            temp_dir.to_str().expect("temp dir"),
            "--infile",
            first_path.to_str().expect("first path"),
            "--infile",
            second_path.to_str().expect("second path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let mut first_report = synthetic_success_report_with_fixits(&first_path);
        let first_warnings: Vec<Diagnostic> = first_report
            .report
            .diagnostics()
            .iter()
            .cloned()
            .map(|mut diagnostic| {
                diagnostic.severity = Severity::Warning;
                diagnostic
            })
            .collect();
        first_report.report = AsmRunReport::new(
            first_warnings,
            first_report.report.source_lines().to_vec(),
            Vec::new(),
        );
        let mut second_report = synthetic_success_report_with_fixits(&second_path);
        let second_warnings: Vec<Diagnostic> = second_report
            .report
            .diagnostics()
            .iter()
            .cloned()
            .map(|mut diagnostic| {
                diagnostic.severity = Severity::Warning;
                diagnostic
            })
            .collect();
        second_report.report = AsmRunReport::new(
            second_warnings,
            second_report.report.source_lines().to_vec(),
            Vec::new(),
        );
        let run_error = CliRunError::WarningsAsErrors {
            reports: vec![first_report, second_report],
        };
        let mut sink = DiagnosticsSink { writer: None };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("Werror fixit aggregation should succeed");

        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        let payload: Value = serde_json::from_str(&report).expect("parse fixit report");
        assert_eq!(payload["schema"], "opforge-fixits-v2");
        assert_eq!(payload["applied"], false, "{report}");
        let fixits = payload["fixits"].as_array().expect("fixits array");
        assert_eq!(fixits.len(), 2, "{report}");
        let files: Vec<&str> = fixits
            .iter()
            .map(|fixit| fixit["file"].as_str().expect("fixit file"))
            .collect();
        assert!(files.contains(&first_path.to_string_lossy().as_ref()));
        assert!(files.contains(&second_path.to_string_lossy().as_ref()));
    }

    #[test]
    fn failed_multi_input_runs_aggregate_fixits_from_earlier_reports_and_terminal_failure() {
        let temp_dir = unique_temp_dir("cli-failed-multi-input-fixit-report");
        let first_path = temp_dir.join("first.asm");
        let second_path = temp_dir.join("second.asm");
        let fixits_path = temp_dir.join("fixits.json");
        write_text(&first_path, ".module first\n.if 1\nnop\n.endmodule\n");
        write_text(&second_path, ".module second\n.if 1\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--list",
            "--outfile",
            temp_dir.to_str().expect("temp dir"),
            "--infile",
            first_path.to_str().expect("first path"),
            "--infile",
            second_path.to_str().expect("second path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");

        let terminal_failure = match run_with_cli_with_context(&Cli::parse_from([
            "opforge",
            "--infile",
            second_path.to_str().expect("second path"),
        ]))
        .expect_err("second input should fail")
        {
            CliRunError::Assembler { error, .. } => *error,
            CliRunError::WarningsAsErrors { .. } => {
                panic!("synthetic failure should be an assembler error")
            }
            CliRunError::Workflow { .. } => {
                panic!("synthetic failure should not be a workflow error")
            }
        };
        let run_error = CliRunError::Assembler {
            reports: vec![synthetic_success_report_with_fixits(&first_path)],
            input_path: Some(second_path.clone()),
            error: Box::new(terminal_failure),
        };
        let mut sink = DiagnosticsSink { writer: None };

        process_run_failure(&mut sink, &cli_config, &run_error, false)
            .expect("failed-run aggregation should succeed");

        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        let payload: Value = serde_json::from_str(&report).expect("parse fixit report");
        assert_eq!(payload["schema"], "opforge-fixits-v2");
        assert_eq!(payload["applied"], false, "{report}");
        let fixits = payload["fixits"].as_array().expect("fixits array");
        assert_eq!(fixits.len(), 2, "{report}");
        let files: Vec<&str> = fixits
            .iter()
            .map(|fixit| fixit["file"].as_str().expect("fixit file"))
            .collect();
        assert!(files.contains(&first_path.to_string_lossy().as_ref()));
        assert!(files.contains(&second_path.to_string_lossy().as_ref()));
    }

    #[test]
    fn adjacent_fixits_on_the_same_line_are_not_treated_as_overlaps() {
        let temp_dir = unique_temp_dir("cli-adjacent-fixits");
        let source_path = temp_dir.join("main.asm");
        let fixits_path = temp_dir.join("fixits.json");
        write_text(&source_path, ".module main\n.if 1\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let base_report = synthetic_success_report_with_fixits(&source_path);
        let mut left = base_report.report.diagnostics()[0].clone();
        left.fixits[0].line = 1;
        left.fixits[0].col_start = Some(1);
        left.fixits[0].col_end = Some(3);
        left.fixits[0].replacement = "AB".to_string();

        let mut right = base_report.report.diagnostics()[0].clone();
        right.fixits[0].line = 1;
        right.fixits[0].col_start = Some(3);
        right.fixits[0].col_end = Some(5);
        right.fixits[0].replacement = "CD".to_string();

        write_text(&source_path, "abcd\n");
        let diagnostics = vec![left, right];
        let reports = vec![synthetic_success_report_with_diagnostics(
            &source_path,
            "abcd",
            diagnostics,
        )];
        let mut sink = DiagnosticsSink { writer: None };

        process_successful_reports(&mut sink, &cli_config, &reports, false)
            .expect("adjacent fixits should be applied successfully");

        let updated = fs::read_to_string(&source_path).expect("updated source");
        assert_eq!(updated, "ABCD\n");

        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        let payload: Value = serde_json::from_str(&report).expect("parse fixit report");
        assert_eq!(payload["applied"], true, "{report}");
        assert_eq!(payload["fixits"].as_array().expect("fixits array").len(), 2);
    }

    #[test]
    fn truly_overlapping_fixits_on_the_same_line_are_still_rejected() {
        let temp_dir = unique_temp_dir("cli-overlapping-fixits");
        let source_path = temp_dir.join("main.asm");
        let fixits_path = temp_dir.join("fixits.json");
        write_text(&source_path, ".module main\n.if 1\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--fixits-output",
            fixits_path.to_str().expect("fixits path"),
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let base_report = synthetic_success_report_with_fixits(&source_path);
        let mut left = base_report.report.diagnostics()[0].clone();
        left.fixits[0].line = 1;
        left.fixits[0].col_start = Some(1);
        left.fixits[0].col_end = Some(3);
        left.fixits[0].replacement = "AB".to_string();

        let mut right = base_report.report.diagnostics()[0].clone();
        right.fixits[0].line = 1;
        right.fixits[0].col_start = Some(2);
        right.fixits[0].col_end = Some(4);
        right.fixits[0].replacement = "BC".to_string();

        write_text(&source_path, "abcd\n");
        let diagnostics = vec![left, right];
        let reports = vec![synthetic_success_report_with_diagnostics(
            &source_path,
            "abcd",
            diagnostics,
        )];
        let mut sink = DiagnosticsSink { writer: None };

        let result = process_successful_reports(&mut sink, &cli_config, &reports, false);
        assert!(result.is_err(), "true overlaps should still be rejected");

        let unchanged = fs::read_to_string(&source_path).expect("source text");
        assert_eq!(unchanged, "abcd\n");

        let report = fs::read_to_string(&fixits_path).expect("fixit report");
        let payload: Value = serde_json::from_str(&report).expect("parse fixit report");
        assert_eq!(payload["applied"], false, "{report}");
        assert_eq!(payload["fixits"].as_array().expect("fixits array").len(), 2);
    }

    #[test]
    fn apply_fixits_preserves_crlf_when_appending_a_missing_directive() {
        let temp_dir = unique_temp_dir("cli-crlf-fixits");
        let source_path = temp_dir.join("main.asm");
        write_text(&source_path, ".module main\n.if 1\nnop\n.endmodule\n");
        let reports = vec![synthetic_success_report_with_fixits(&source_path)];
        fs::write(
            &source_path,
            b".module main\r\n.if 1\r\nnop\r\n.endmodule\r\n",
        )
        .expect("write crlf source");

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let mut sink = DiagnosticsSink { writer: None };

        process_successful_reports(&mut sink, &cli_config, &reports, false)
            .expect("CRLF fixit application should succeed");

        let updated = fs::read(&source_path).expect("updated file");
        assert_eq!(
            updated,
            b".module main\r\n.if 1\r\nnop\r\n.endmodule\r\n.endif\r\n"
        );
    }

    #[test]
    fn apply_fixits_preserves_missing_final_newline_when_appending_a_missing_directive() {
        let temp_dir = unique_temp_dir("cli-no-final-newline-fixits");
        let source_path = temp_dir.join("main.asm");
        write_text(&source_path, ".module main\n.if 1\nnop\n.endmodule\n");
        let reports = vec![synthetic_success_report_with_fixits(&source_path)];
        fs::write(&source_path, b".module main\n.if 1\nnop\n.endmodule")
            .expect("write source without final newline");

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let mut sink = DiagnosticsSink { writer: None };

        process_successful_reports(&mut sink, &cli_config, &reports, false)
            .expect("no-final-newline fixit application should succeed");

        let updated = fs::read(&source_path).expect("updated file");
        assert_eq!(updated, b".module main\n.if 1\nnop\n.endmodule\n.endif");
    }

    #[test]
    fn apply_fixits_use_byte_columns_for_unicode_bearing_lines() {
        let temp_dir = unique_temp_dir("cli-unicode-byte-fixits");
        let source_path = temp_dir.join("main.asm");
        write_text(&source_path, ".module main\n.if 1\nnop\n.endmodule\n");
        let base_report = synthetic_success_report_with_fixits(&source_path);
        let mut diagnostic = base_report.report.diagnostics()[0].clone();
        diagnostic.fixits[0].line = 1;
        diagnostic.fixits[0].col_start = Some(3);
        diagnostic.fixits[0].col_end = Some(4);
        diagnostic.fixits[0].replacement = "Y".to_string();

        fs::write(&source_path, "éx\n").expect("write unicode source");
        let reports = vec![synthetic_success_report_with_diagnostics(
            &source_path,
            "éx",
            vec![diagnostic],
        )];

        let cli = Cli::parse_from([
            "opforge",
            "--apply-fixits",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let cli_config = validate_cli(&cli).expect("validate cli");
        let mut sink = DiagnosticsSink { writer: None };

        process_successful_reports(&mut sink, &cli_config, &reports, false)
            .expect("unicode byte-column fixit application should succeed");

        let updated = fs::read(&source_path).expect("updated unicode file");
        assert_eq!(updated, "éY\n".as_bytes());
    }
}
