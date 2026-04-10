// SPDX-License-Identifier: GPL-3.0-or-later

use std::fmt;
use std::path::{Path, PathBuf};

use api::asm::Assembler;
use api::diagnostics::{AsmError, AsmErrorKind, AsmRunError, AsmRunReport, Severity};

use crate::{
    input_base_from_path, validate_cli, Cli, CliConfig, OutputFormat, BUILD_PROFILE_SUMMARY,
    VERSION,
};

pub struct CliRunReport {
    pub input_path: PathBuf,
    pub report: AsmRunReport,
}

impl fmt::Debug for CliRunReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CliRunReport")
            .field("input_path", &self.input_path)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub enum CliRunError {
    Assembler {
        reports: Vec<CliRunReport>,
        input_path: Option<PathBuf>,
        error: Box<AsmRunError>,
    },
    WarningsAsErrors {
        reports: Vec<CliRunReport>,
    },
}

impl CliRunError {
    fn assembler(
        reports: Vec<CliRunReport>,
        input_path: Option<PathBuf>,
        error: AsmRunError,
    ) -> Self {
        Self::Assembler {
            reports,
            input_path,
            error: Box::new(error),
        }
    }
}

pub fn run_with_cli_with_context(cli: &Cli) -> Result<Vec<CliRunReport>, CliRunError> {
    let config =
        validate_cli(cli).map_err(|error| CliRunError::assembler(Vec::new(), None, error))?;

    let mut reports = Vec::new();
    for input_path in &config.input_paths {
        let (asm_name, input_base) =
            match input_base_from_path(input_path, &config.input_extensions) {
                Ok(paths) => paths,
                Err(error) => {
                    return Err(CliRunError::assembler(
                        reports,
                        Some(input_path.clone()),
                        error,
                    ));
                }
            };
        let report = match run_one(cli, &asm_name, &input_base, &config) {
            Ok(report) => report,
            Err(error) => {
                return Err(CliRunError::assembler(
                    reports,
                    Some(input_path.clone()),
                    error,
                ));
            }
        };
        reports.push(CliRunReport {
            input_path: input_path.clone(),
            report,
        });
    }

    if config.warning_policy.treat_warnings_as_errors && has_werror_violations(&reports) {
        return Err(CliRunError::WarningsAsErrors { reports });
    }

    Ok(reports)
}
/// Check whether any warnings exist in the reports that should be treated as
/// errors under `-Werror`. Returns `true` if any report contains at least one
/// warning diagnostic.
pub fn has_werror_violations(reports: &[CliRunReport]) -> bool {
    reports.iter().any(|entry| {
        entry
            .report
            .diagnostics()
            .iter()
            .any(|d| d.severity == Severity::Warning)
    })
}

fn workflow_error_to_asm_run_error(error: api::asm::AssemblerWorkflowError) -> AsmRunError {
    match error {
        api::asm::AssemblerWorkflowError::Assemble(error) => error,
        api::asm::AssemblerWorkflowError::InvalidArgument(error) => AsmRunError::new(
            AsmError::new(AsmErrorKind::Cli, error.summary(), None),
            Vec::new(),
            Vec::new(),
        ),
        api::asm::AssemblerWorkflowError::InvalidRequest(error) => AsmRunError::new(
            AsmError::new(AsmErrorKind::Cli, error.summary(), None),
            Vec::new(),
            Vec::new(),
        ),
        api::asm::AssemblerWorkflowError::Io(error) => AsmRunError::new(
            AsmError::new(AsmErrorKind::Io, error.summary(), None),
            Vec::new(),
            Vec::new(),
        ),
        api::asm::AssemblerWorkflowError::Internal(error) => AsmRunError::new(
            AsmError::new(AsmErrorKind::Assembler, error.summary(), None),
            Vec::new(),
            Vec::new(),
        ),
    }
}

fn run_one(
    cli: &Cli,
    asm_name: &str,
    output_base: &str,
    config: &CliConfig,
) -> Result<AsmRunReport, AsmRunError> {
    let root_path = Path::new(asm_name);
    let output_format = match config.output_format {
        OutputFormat::Text => api::asm::OutputFormat::Text,
        OutputFormat::Json => api::asm::OutputFormat::Json,
    };
    let header_title = format!("opForge Assembler v{VERSION} | {BUILD_PROFILE_SUMMARY}");
    let mut builder = Assembler::builder(root_path)
        .output_base(output_base)
        .defines(&config.defines)
        .include_paths(&config.include_paths)
        .module_paths(&config.module_paths)
        .pp_macro_depth(config.pp_macro_depth)
        .max_loop_iterations(config.max_loop_iterations)
        .output_format(output_format)
        .bin_specs(&config.bin_specs)
        .label_output_format(config.label_output_format)
        .header_title(&header_title)
        .default_outputs(config.default_outputs)
        .debug_conditionals(config.debug_conditionals);

    if let Some(cpu_override) = config.cpu_override.as_deref() {
        builder = builder.cpu_override(cpu_override);
    }
    if let Some(opasm_package_path) = config.opasm_package.as_deref() {
        builder = builder.opasm_package_path(opasm_package_path);
    }
    if let Some(out_dir) = config.out_dir.as_deref() {
        builder = builder.out_dir(out_dir);
    }
    if let Some(go_addr) = config.go_addr.as_deref() {
        builder = builder.go_addr(go_addr);
    }
    if config.fill_byte_set {
        builder = builder.fill_byte(config.fill_byte);
    }
    if let Some(labels_file) = config.labels_file.as_deref() {
        builder = builder.labels_file(labels_file);
    }
    if let Some(dependency_output) = config.dependency_output.as_ref() {
        builder = builder.dependency_output(dependency_output);
    }
    if let Some(outfile_override) = cli.outfile.as_deref() {
        builder = builder.outfile_override(outfile_override);
    }
    if let Some(list_name_override) = cli.list_name.as_deref() {
        builder = builder.list_name_override(list_name_override);
    }
    if let Some(hex_name_override) = cli.hex_name.as_deref() {
        builder = builder.hex_name_override(hex_name_override);
    }
    if let Some(srec_name_override) = config.srec_name.as_deref() {
        builder = builder.srec_name_override(srec_name_override);
    }
    if let Some(hunk_name_override) = config.hunk_name.as_deref() {
        builder = builder.hunk_name_override(hunk_name_override);
    }
    if let Some(tab_size) = config.tab_size {
        builder = builder.tab_size(tab_size);
    }

    builder.assemble().map_err(workflow_error_to_asm_run_error)
}

#[cfg(test)]
mod tests {
    use super::{has_werror_violations, run_with_cli_with_context, CliRunError};
    use crate::Cli;
    use api::diagnostics::Severity;
    use clap::Parser;
    use std::fs;
    use std::path::PathBuf;
    use std::process;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};

    static TEMP_DIR_SEQ: AtomicU64 = AtomicU64::new(1);

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

    #[test]
    fn run_with_cli_with_context_reports_promoted_warning_under_werror() {
        let temp_dir = unique_temp_dir("cli-core-werror");
        let source_path = temp_dir.join("warn.asm");
        let list_path = temp_dir.join("warn.lst");
        write_text(&source_path, ".byte 300\n");

        let cli = Cli::parse_from([
            "opforge",
            "--infile",
            source_path.to_str().expect("source path"),
            "--list",
            list_path.to_str().expect("list path"),
            "--Werror",
        ]);

        let err = run_with_cli_with_context(&cli).expect_err("warnings should fail under --Werror");
        match err {
            CliRunError::WarningsAsErrors { reports } => {
                assert_eq!(reports.len(), 1);
                assert_eq!(reports[0].input_path, source_path);
                assert!(has_werror_violations(&reports));
                assert_eq!(reports[0].report.diagnostics().len(), 1);
                assert_eq!(
                    reports[0].report.diagnostics()[0].severity,
                    Severity::Warning
                );
            }
            other => panic!("expected Werror reports, got {other:?}"),
        }
    }

    #[test]
    fn run_with_cli_with_context_preserves_each_warning_report_under_werror() {
        let temp_dir = unique_temp_dir("cli-core-werror-multi-context");
        let first_path = temp_dir.join("alpha.asm");
        let second_path = temp_dir.join("beta.asm");
        let out_dir = temp_dir.join("out");
        fs::create_dir_all(&out_dir).expect("create out dir");
        write_text(&first_path, ".byte 300 ; alpha sentinel\n");
        write_text(&second_path, ".byte 400 ; beta sentinel\n");

        let cli = Cli::parse_from([
            "opforge",
            "--infile",
            first_path.to_str().expect("first path"),
            "--infile",
            second_path.to_str().expect("second path"),
            "--list",
            "--outfile",
            out_dir.to_str().expect("out dir"),
            "--Werror",
        ]);

        let err = run_with_cli_with_context(&cli).expect_err("warnings should fail under --Werror");
        match err {
            CliRunError::WarningsAsErrors { reports } => {
                assert_eq!(reports.len(), 2);
                assert_eq!(reports[0].input_path, first_path);
                assert_eq!(reports[1].input_path, second_path);
                assert!(reports[0].report.source_lines()[0].contains("alpha sentinel"));
                assert!(reports[1].report.source_lines()[0].contains("beta sentinel"));
            }
            other => panic!("expected Werror reports, got {other:?}"),
        }
    }

    #[test]
    fn run_with_cli_does_not_leak_opasm_package_selection_across_runs() {
        let temp_dir = unique_temp_dir("cli-core-opasm-package-scope");
        let source_path = temp_dir.join("main.asm");
        let bad_package_path = temp_dir.join("broken-runtime.opasm");
        write_text(&source_path, ".module main\nnop\n.endmodule\n");
        write_text(&bad_package_path, "not a valid opasm package");

        let failing_cli = Cli::parse_from([
            "opforge",
            "--opasm-package",
            bad_package_path.to_str().expect("bad package path"),
            "--infile",
            source_path.to_str().expect("source path"),
        ]);
        let succeeding_cli = Cli::parse_from([
            "opforge",
            "--infile",
            source_path.to_str().expect("source path"),
        ]);

        assert!(
            run_with_cli_with_context(&failing_cli).is_err(),
            "invalid explicit opasm package should fail"
        );
        assert!(
            run_with_cli_with_context(&succeeding_cli).is_ok(),
            "subsequent run without an override should not inherit the prior explicit package"
        );
    }

    #[test]
    fn run_with_cli_with_context_retains_earlier_reports_when_a_later_input_fails() {
        let temp_dir = unique_temp_dir("cli-core-retain-reports-on-failure");
        let first_path = temp_dir.join("first.asm");
        let second_path = temp_dir.join("second.asm");
        let out_dir = temp_dir.join("out");
        fs::create_dir_all(&out_dir).expect("create out dir");
        write_text(
            &first_path,
            ".byte 300 ; first warning survives later failure\n",
        );
        write_text(&second_path, ".module second\n.if 1\nnop\n.endmodule\n");

        let cli = Cli::parse_from([
            "opforge",
            "--list",
            "--outfile",
            out_dir.to_str().expect("out dir"),
            "--infile",
            first_path.to_str().expect("first path"),
            "--infile",
            second_path.to_str().expect("second path"),
        ]);

        let err = run_with_cli_with_context(&cli).expect_err("later input should fail assembly");
        match err {
            CliRunError::Assembler {
                reports,
                input_path,
                error,
            } => {
                assert_eq!(input_path, Some(second_path.clone()));
                assert_eq!(reports.len(), 1);
                assert_eq!(reports[0].input_path, first_path);
                assert!(reports[0].report.source_lines()[0]
                    .contains("first warning survives later failure"));
                assert!(error.source_lines()[1].contains(".if 1"));
            }
            other => panic!("expected assembler failure, got {other:?}"),
        }
    }
}
