// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use std::path::{Component, Path, PathBuf};

use libopforge::asm::{Assembler, AssemblerWorkflowError};
use libopforge::diagnostics::{AsmRunReport, Diagnostic, Severity};

use crate::lsp::config::LspConfig;

#[derive(Debug, Clone, Default)]
pub struct ValidationFixit {
    pub file: Option<String>,
    pub line: u32,
    pub col_start: Option<u32>,
    pub col_end: Option<u32>,
    pub replacement: String,
    pub applicability: String,
}

#[derive(Debug, Clone, Default)]
pub struct ValidationDiagnostic {
    pub code: String,
    pub severity: String,
    pub message: String,
    pub file: Option<String>,
    pub line: u32,
    pub col_start: Option<u32>,
    pub col_end: Option<u32>,
    pub fixits: Vec<ValidationFixit>,
}

#[derive(Debug, Clone, Default)]
pub struct ValidationRunResult {
    pub diagnostics: Vec<ValidationDiagnostic>,
}

pub fn run_validation(
    config: &LspConfig,
    root_file: &Path,
    working_dir: &Path,
    source_root: &Path,
    effective_module_roots: &[PathBuf],
) -> ValidationRunResult {
    let include_paths: Vec<PathBuf> = config
        .include_paths
        .iter()
        .map(|include| PathBuf::from(rebase_config_path(include, source_root, working_dir)))
        .collect();
    let module_paths = if effective_module_roots.is_empty() {
        config
            .module_paths
            .iter()
            .map(|module| PathBuf::from(rebase_config_path(module, source_root, working_dir)))
            .collect()
    } else {
        rebase_effective_module_roots(effective_module_roots, source_root, working_dir)
    };

    let mut builder = Assembler::builder(root_file)
        .defines(&config.defines)
        .include_paths(&include_paths)
        .module_paths(&module_paths);
    if let Some(cpu) = &config.default_cpu {
        builder = builder.cpu_override(cpu);
    }

    match builder.check() {
        Ok(report) => ValidationRunResult {
            diagnostics: map_report_diagnostics(&report),
        },
        Err(error) => ValidationRunResult {
            diagnostics: map_workflow_error(root_file, error),
        },
    }
}

fn rebase_effective_module_roots(
    roots: &[PathBuf],
    source_root: &Path,
    overlay_root: &Path,
) -> Vec<PathBuf> {
    let normalized_source_root = normalize_path(source_root);
    let mut rebased = Vec::new();
    for root in roots {
        let candidate = if root.is_absolute() {
            let normalized_root = normalize_path(root);
            if let Ok(relative) = normalized_root.strip_prefix(&normalized_source_root) {
                overlay_root.join(relative)
            } else {
                normalized_root
            }
        } else {
            PathBuf::from(rebase_config_path(
                root.to_string_lossy().as_ref(),
                source_root,
                overlay_root,
            ))
        };
        if !rebased.iter().any(|existing| existing == &candidate) {
            rebased.push(candidate);
        }
    }
    rebased
}

fn validation_failure_diagnostic(root_file: &Path, detail: String) -> ValidationDiagnostic {
    ValidationDiagnostic {
        code: "LSPVALIDATOR".to_string(),
        severity: "error".to_string(),
        message: format!("Validation did not complete: {detail}"),
        file: Some(root_file.to_string_lossy().to_string()),
        line: 1,
        col_start: Some(1),
        col_end: Some(1),
        fixits: Vec::new(),
    }
}

fn map_report_diagnostics(report: &AsmRunReport) -> Vec<ValidationDiagnostic> {
    report.diagnostics().iter().map(map_diagnostic).collect()
}

fn rebase_config_path(path: &str, source_root: &Path, overlay_root: &Path) -> String {
    let candidate = Path::new(path);
    if candidate.is_absolute() {
        return candidate.to_string_lossy().to_string();
    }

    let normalized_source_root = normalize_path(source_root);
    let resolved_source_path = normalize_path(&normalized_source_root.join(candidate));
    if let Ok(relative) = resolved_source_path.strip_prefix(&normalized_source_root) {
        return overlay_root.join(relative).to_string_lossy().to_string();
    }

    resolved_source_path.to_string_lossy().to_string()
}

fn normalize_path(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                let _ = normalized.pop();
            }
            other => normalized.push(other.as_os_str()),
        }
    }
    normalized
}

fn map_workflow_error(
    root_file: &Path,
    error: AssemblerWorkflowError,
) -> Vec<ValidationDiagnostic> {
    match error {
        AssemblerWorkflowError::Assemble(error) => {
            let mut diagnostics: Vec<ValidationDiagnostic> =
                error.diagnostics().iter().map(map_diagnostic).collect();
            if diagnostics.is_empty() {
                diagnostics.push(validation_failure_diagnostic(
                    root_file,
                    error.summary().to_string(),
                ));
            }
            diagnostics
        }
        other => vec![ValidationDiagnostic {
            code: other.code().to_string(),
            severity: "error".to_string(),
            message: other.summary().to_string(),
            file: Some(root_file.to_string_lossy().to_string()),
            line: 1,
            col_start: Some(1),
            col_end: Some(1),
            fixits: Vec::new(),
        }],
    }
}

fn map_diagnostic(diagnostic: &Diagnostic) -> ValidationDiagnostic {
    let file = diagnostic.file.clone().or_else(|| {
        diagnostic
            .parser_error
            .as_ref()
            .and_then(|parse| parse.location.file.clone())
    });
    let line = diagnostic
        .parser_error
        .as_ref()
        .map(|parse| parse.location.line)
        .unwrap_or(diagnostic.line);
    let col_start = diagnostic
        .parser_error
        .as_ref()
        .and_then(|parse| parse.location.col_start)
        .or(diagnostic.column)
        .map(|value| value as u32);
    let col_end = diagnostic
        .parser_error
        .as_ref()
        .and_then(|parse| parse.location.col_end)
        .or(diagnostic.col_end)
        .map(|value| value as u32);

    ValidationDiagnostic {
        code: diagnostic.code.clone(),
        severity: severity_name(diagnostic.severity).to_string(),
        message: diagnostic.error.message().to_string(),
        file,
        line,
        col_start,
        col_end,
        fixits: diagnostic
            .fixits
            .iter()
            .map(|fixit| ValidationFixit {
                file: fixit.file.clone(),
                line: fixit.line,
                col_start: fixit.col_start.map(|value| value as u32),
                col_end: fixit.col_end.map(|value| value as u32),
                replacement: fixit.replacement.clone(),
                applicability: fixit.applicability.clone(),
            })
            .collect(),
    }
}

fn severity_name(severity: Severity) -> &'static str {
    match severity {
        Severity::Warning => "warning",
        Severity::Error => "error",
    }
}

#[cfg(test)]
fn unique_temp_dir(prefix: &str) -> PathBuf {
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("clock")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("{prefix}-{nanos}-{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

#[cfg(test)]
fn write_text(path: &Path, text: &str) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("create parent dir");
    }
    std::fs::write(path, text).expect("write test file");
}

#[cfg(test)]
fn artifact_emitting_invalid_source() -> &'static str {
    ".cpu \"6502\"\n.meta.output.name \"artifact\"\n.mapfile \"build/diagnostic.map\", symbols=all\n.org $1000\n.bogus\n"
}

#[cfg(test)]
fn temp_path_exists(path: &Path) -> bool {
    std::fs::metadata(path).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rebase_config_path_keeps_workspace_relative_targets_in_overlay() {
        let source_root = Path::new("/workspace/project");
        let overlay_root = Path::new("/tmp/lsp-overlay/workspace");

        let rebased = rebase_config_path("shared/modules", source_root, overlay_root);

        assert_eq!(rebased, "/tmp/lsp-overlay/workspace/shared/modules");
    }

    #[test]
    fn rebase_config_path_resolves_external_relative_targets_from_workspace_root() {
        let source_root = Path::new("/workspace/project");
        let overlay_root = Path::new("/tmp/lsp-overlay/workspace");

        let rebased = rebase_config_path("../external/modules", source_root, overlay_root);

        assert_eq!(rebased, "/workspace/external/modules");
    }

    #[test]
    fn rebase_effective_module_roots_maps_workspace_roots_into_overlay() {
        let source_root = Path::new("/workspace/project");
        let overlay_root = Path::new("/tmp/lsp-overlay/workspace");
        let roots = vec![
            PathBuf::from("/workspace/project"),
            PathBuf::from("/workspace/project/app"),
            PathBuf::from("/workspace/external/modules"),
        ];

        let rebased = rebase_effective_module_roots(&roots, source_root, overlay_root);

        assert_eq!(
            rebased,
            vec![
                PathBuf::from("/tmp/lsp-overlay/workspace"),
                PathBuf::from("/tmp/lsp-overlay/workspace/app"),
                PathBuf::from("/workspace/external/modules"),
            ]
        );
    }

    #[test]
    fn run_validation_uses_read_only_check_path() {
        let temp_dir = unique_temp_dir("lsp-validation-read-only");
        let root_file = temp_dir.join("main.asm");
        write_text(&root_file, artifact_emitting_invalid_source());

        let result = run_validation(&LspConfig::default(), &root_file, &temp_dir, &temp_dir, &[]);

        assert!(
            !result.diagnostics.is_empty(),
            "expected diagnostics for invalid source"
        );
        assert!(
            result
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.severity == "error"),
            "expected at least one error diagnostic"
        );
        assert!(
            !temp_path_exists(&temp_dir.join("artifact.lst")),
            "validation should not emit default listing output"
        );
        assert!(
            !temp_path_exists(&temp_dir.join("artifact.hex")),
            "validation should not emit default hex output"
        );
        assert!(
            !temp_path_exists(&temp_dir.join("build/diagnostic.map")),
            "validation should not emit directive-owned mapfiles"
        );

        let _ = std::fs::remove_dir_all(temp_dir);
    }
}
