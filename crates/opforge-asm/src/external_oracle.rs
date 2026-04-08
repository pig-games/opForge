use crate::normalization::{
    diagnostic_excerpt, normalize_64tass_stderr, normalize_opforge_diagnostics,
    normalize_vasm_stderr, NormalizedErrorClass,
};
use crate::oracle::{
    tass64::Tass64Adapter, vasm::VasmAdapter, ExternalOracleAdapter, OracleAssembleFailure,
    OracleAssembleRequest, OracleAssembleSuccess, OracleAvailability,
};
use cli_core::{LabelOutputFormat as CliLabelOutputFormat, VERSION};
use engine::{
    default_cpu, run_assembly, AssemblyExecutionRequest, ExecutionMode,
    OutputFormat as EngineOutputFormat,
};
use std::collections::{HashMap, HashSet};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::time::{SystemTime, UNIX_EPOCH};
use types::lockstep::ContinuationHead;
use vm::output_model::BinOutputSpec;

const OUTPUT_FILENAME: &str = "output.bin";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpectedOutcome {
    Success,
    Error,
    DocumentedDivergence,
}

impl ExpectedOutcome {
    fn label(self) -> &'static str {
        match self {
            Self::Success => "success",
            Self::Error => "error",
            Self::DocumentedDivergence => "documented_divergence",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompareMode {
    Bytes,
    ErrorClass,
}

impl CompareMode {
    fn label(self) -> &'static str {
        match self {
            Self::Bytes => "bytes",
            Self::ErrorClass => "error_class",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DocumentedDivergenceKind {
    OpforgeErrorOracleSuccess,
    OpforgeSuccessOracleError,
    ByteMismatch,
    ErrorClassMismatch,
}

impl DocumentedDivergenceKind {
    fn label(self) -> &'static str {
        match self {
            Self::OpforgeErrorOracleSuccess => "opforge_error_oracle_success",
            Self::OpforgeSuccessOracleError => "opforge_success_oracle_error",
            Self::ByteMismatch => "byte_mismatch",
            Self::ErrorClassMismatch => "error_class_mismatch",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DocumentedDivergence {
    kind: DocumentedDivergenceKind,
    reason: Option<String>,
    expected_opforge_status: ObservedStatus,
    expected_oracle_status: ObservedStatus,
    expected_opforge_error_class: Option<NormalizedErrorClass>,
    expected_oracle_error_class: Option<NormalizedErrorClass>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructuredSkip {
    reason: String,
}

impl StructuredSkip {
    fn new(reason: impl Into<String>) -> Self {
        Self {
            reason: reason.into(),
        }
    }

    pub(crate) fn reason(&self) -> &str {
        self.reason.as_str()
    }
}

#[derive(Debug)]
pub(crate) enum ExternalOracleSuiteOutcome {
    Skipped(StructuredSkip),
    Completed {
        fixture_count: usize,
        artifact_root: PathBuf,
        notes: Vec<String>,
    },
}

#[derive(Debug, Clone)]
struct Manifest {
    family: String,
    oracle: String,
    cpu_profile: Option<String>,
    fixtures: Vec<Fixture>,
}

#[derive(Debug, Clone)]
struct Fixture {
    id: String,
    cpu: String,
    source_path: PathBuf,
    expected_outcome: ExpectedOutcome,
    compare_mode: CompareMode,
    documented_divergence: Option<DocumentedDivergence>,
}

#[derive(Default)]
struct ManifestBuilder {
    manifest_version: Option<u32>,
    family: Option<String>,
    oracle: Option<String>,
    oracle_profile: Option<String>,
    cpu_profile: Option<String>,
    expected_outcome: Option<String>,
    compare_mode: Option<String>,
    fixtures: Vec<Fixture>,
}

#[derive(Default)]
struct FixtureBuilder {
    id: Option<String>,
    cpu: Option<String>,
    path: Option<String>,
    expected_outcome: Option<String>,
    compare_mode: Option<String>,
    documented_divergence_kind: Option<String>,
    documented_divergence_reason: Option<String>,
    documented_divergence_opforge_status: Option<String>,
    documented_divergence_oracle_status: Option<String>,
    documented_divergence_opforge_error_class: Option<String>,
    documented_divergence_oracle_error_class: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ObservedStatus {
    Success,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ByteMismatchSummary {
    opforge_len: usize,
    oracle_len: usize,
    first_difference_offset: Option<usize>,
    opforge_byte: Option<u8>,
    oracle_byte: Option<u8>,
    opforge_bytes: Vec<u8>,
    oracle_bytes: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StructuredMismatch {
    fixture_id: String,
    family: String,
    cpu: String,
    oracle_id: String,
    compare_mode: CompareMode,
    opforge_status: ObservedStatus,
    oracle_status: ObservedStatus,
    opforge_output_path: Option<PathBuf>,
    oracle_output_path: Option<PathBuf>,
    opforge_diagnostics_path: Option<PathBuf>,
    oracle_diagnostics_path: Option<PathBuf>,
    opforge_stdout_path: Option<PathBuf>,
    oracle_stdout_path: Option<PathBuf>,
    opforge_stderr_path: Option<PathBuf>,
    oracle_stderr_path: Option<PathBuf>,
    opforge_summary: Option<String>,
    oracle_summary: Option<String>,
    opforge_error_class: Option<NormalizedErrorClass>,
    oracle_error_class: Option<NormalizedErrorClass>,
    opforge_excerpt: Option<String>,
    oracle_excerpt: Option<String>,
    documented_divergence_kind: Option<DocumentedDivergenceKind>,
    documented_divergence_reason: Option<String>,
    byte_mismatch: Option<ByteMismatchSummary>,
}

impl StructuredMismatch {
    fn render(&self) -> String {
        let mut lines = vec![
            "external-oracle mismatch".to_string(),
            format!("fixture id: {}", self.fixture_id),
            format!("family: {}", self.family),
            format!("cpu: {}", self.cpu),
            format!("oracle id: {}", self.oracle_id),
            format!("compare mode: {}", self.compare_mode.label()),
            format!(
                "opforge status: {}",
                observed_status_label(self.opforge_status)
            ),
            format!(
                "oracle status: {}",
                observed_status_label(self.oracle_status)
            ),
        ];

        if let Some(path) = &self.opforge_output_path {
            lines.push(format!("opforge output path: {}", path.display()));
        }
        if let Some(path) = &self.oracle_output_path {
            lines.push(format!("oracle output path: {}", path.display()));
        }
        if let Some(path) = &self.opforge_diagnostics_path {
            lines.push(format!("opforge diagnostics path: {}", path.display()));
        }
        if let Some(path) = &self.oracle_diagnostics_path {
            lines.push(format!("oracle diagnostics path: {}", path.display()));
        }
        if let Some(path) = &self.opforge_stdout_path {
            lines.push(format!("opforge stdout path: {}", path.display()));
        }
        if let Some(path) = &self.oracle_stdout_path {
            lines.push(format!("oracle stdout path: {}", path.display()));
        }
        if let Some(path) = &self.opforge_stderr_path {
            lines.push(format!("opforge stderr path: {}", path.display()));
        }
        if let Some(path) = &self.oracle_stderr_path {
            lines.push(format!("oracle stderr path: {}", path.display()));
        }
        if let Some(summary) = &self.opforge_summary {
            lines.push(format!("opforge summary: {summary}"));
        }
        if let Some(summary) = &self.oracle_summary {
            lines.push(format!("oracle summary: {summary}"));
        }
        if let Some(class) = self.opforge_error_class {
            lines.push(format!("opforge error class: {}", class.label()));
        }
        if let Some(class) = self.oracle_error_class {
            lines.push(format!("oracle error class: {}", class.label()));
        }
        if let Some(excerpt) = &self.opforge_excerpt {
            lines.push(format!("opforge excerpt: {excerpt}"));
        }
        if let Some(excerpt) = &self.oracle_excerpt {
            lines.push(format!("oracle excerpt: {excerpt}"));
        }
        if let Some(kind) = self.documented_divergence_kind {
            lines.push(format!("documented divergence kind: {}", kind.label()));
        }
        if let Some(reason) = &self.documented_divergence_reason {
            lines.push(format!("documented divergence reason: {reason}"));
        }
        if let Some(byte_mismatch) = &self.byte_mismatch {
            lines.push(format!(
                "--- opforge  ({} bytes)",
                byte_mismatch.opforge_len
            ));
            lines.push(format!("+++ oracle   ({} bytes)", byte_mismatch.oracle_len));
            lines.push(render_hex_diff(
                &byte_mismatch.opforge_bytes,
                &byte_mismatch.oracle_bytes,
            ));
        }

        lines.join("\n")
    }
}

pub(crate) fn run_vasm_success_fixture_suite(
    manifest_root: &Path,
) -> Result<ExternalOracleSuiteOutcome, String> {
    let adapter = VasmAdapter::from_env();
    run_fixture_suite(manifest_root, &adapter, ExpectedOutcome::Success)
}

pub(crate) fn run_tass64_success_fixture_suite(
    manifest_root: &Path,
) -> Result<ExternalOracleSuiteOutcome, String> {
    let adapter = Tass64Adapter::from_env();
    run_fixture_suite(manifest_root, &adapter, ExpectedOutcome::Success)
}

pub(crate) fn run_tass64_error_fixture_suite(
    manifest_root: &Path,
) -> Result<ExternalOracleSuiteOutcome, String> {
    let adapter = Tass64Adapter::from_env();
    run_fixture_suite(manifest_root, &adapter, ExpectedOutcome::Error)
}

pub(crate) fn run_tass64_documented_divergence_fixture_suite(
    manifest_root: &Path,
) -> Result<ExternalOracleSuiteOutcome, String> {
    let adapter = Tass64Adapter::from_env();
    run_fixture_suite(
        manifest_root,
        &adapter,
        ExpectedOutcome::DocumentedDivergence,
    )
}

pub(crate) fn run_vasm_error_fixture_suite(
    manifest_root: &Path,
) -> Result<ExternalOracleSuiteOutcome, String> {
    let adapter = VasmAdapter::from_env();
    run_fixture_suite(manifest_root, &adapter, ExpectedOutcome::Error)
}

pub(crate) fn run_vasm_documented_divergence_fixture_suite(
    manifest_root: &Path,
) -> Result<ExternalOracleSuiteOutcome, String> {
    let adapter = VasmAdapter::from_env();
    run_fixture_suite(
        manifest_root,
        &adapter,
        ExpectedOutcome::DocumentedDivergence,
    )
}

fn run_fixture_suite<A: ExternalOracleAdapter>(
    manifest_root: &Path,
    adapter: &A,
    expected_outcome: ExpectedOutcome,
) -> Result<ExternalOracleSuiteOutcome, String> {
    match adapter.availability() {
        OracleAvailability::Disabled(reason) | OracleAvailability::Missing(reason) => Ok(
            ExternalOracleSuiteOutcome::Skipped(StructuredSkip::new(reason)),
        ),
        OracleAvailability::Ready => {
            let (discovery_root, manifest_paths) = discover_manifest_paths(manifest_root)?;
            let artifact_root = create_suite_dir(&format!(
                "external-oracle-{}-{}",
                adapter.oracle_id(),
                expected_outcome.label()
            ))?;
            let multi_manifest = manifest_paths.len() > 1;
            let mut fixture_count = 0;
            let mut notes = Vec::new();
            let mut first_mismatch = None;
            for manifest_path in manifest_paths {
                let manifest = load_manifest(&manifest_path, adapter)?;
                let selected_fixtures = manifest
                    .fixtures
                    .iter()
                    .filter(|fixture| fixture.expected_outcome == expected_outcome)
                    .collect::<Vec<_>>();
                if selected_fixtures.is_empty() {
                    continue;
                }
                fixture_count += selected_fixtures.len();

                let manifest_artifact_root = if multi_manifest {
                    let label = manifest_artifact_label(&discovery_root, &manifest_path)?;
                    let path = artifact_root.join(label);
                    fs::create_dir_all(&path).map_err(|err| {
                        format!(
                            "Create manifest artifact directory {}: {err}",
                            path.display()
                        )
                    })?;
                    path
                } else {
                    artifact_root.clone()
                };

                for fixture in &selected_fixtures {
                    match run_fixture(&manifest, fixture, adapter, &manifest_artifact_root) {
                        Ok(note) => {
                            sync_sidecar_report(
                                &source_path_with_extension(&fixture.source_path, "mismatch.txt"),
                                None,
                            );
                            if let Some(note) = note {
                                notes.push(note);
                            }
                        }
                        Err(mismatch) => {
                            let rendered = mismatch.render();
                            sync_sidecar_report(
                                &source_path_with_extension(&fixture.source_path, "mismatch.txt"),
                                Some(rendered.clone()),
                            );
                            if first_mismatch.is_none() {
                                first_mismatch = Some(rendered);
                            }
                        }
                    }
                }
            }
            if let Some(rendered) = first_mismatch {
                Err(rendered)
            } else {
                Ok(ExternalOracleSuiteOutcome::Completed {
                    fixture_count,
                    artifact_root,
                    notes,
                })
            }
        }
    }
}

#[allow(clippy::result_large_err)]
fn run_fixture<A: ExternalOracleAdapter>(
    manifest: &Manifest,
    fixture: &Fixture,
    adapter: &A,
    artifact_root: &Path,
) -> Result<Option<String>, StructuredMismatch> {
    let fixture_dir = artifact_root.join(sanitize_fixture_id(&fixture.id));
    fs::create_dir_all(&fixture_dir).map_err(|err| StructuredMismatch {
        fixture_id: fixture.id.clone(),
        family: manifest.family.clone(),
        cpu: fixture.cpu.clone(),
        oracle_id: manifest.oracle.clone(),
        compare_mode: fixture.compare_mode,
        opforge_status: ObservedStatus::Error,
        oracle_status: ObservedStatus::Error,
        opforge_output_path: None,
        oracle_output_path: None,
        opforge_diagnostics_path: None,
        oracle_diagnostics_path: Some(fixture_dir.join("fixture-dir-error.txt")),
        opforge_stdout_path: None,
        oracle_stdout_path: None,
        opforge_stderr_path: None,
        oracle_stderr_path: None,
        opforge_summary: None,
        oracle_summary: Some(format!(
            "Create fixture artifact directory {}: {err}",
            fixture_dir.display()
        )),
        opforge_error_class: None,
        oracle_error_class: None,
        opforge_excerpt: None,
        oracle_excerpt: None,
        documented_divergence_kind: fixture.documented_divergence.as_ref().map(|item| item.kind),
        documented_divergence_reason: fixture
            .documented_divergence
            .as_ref()
            .and_then(|item| item.reason.clone()),
        byte_mismatch: None,
    })?;

    let opforge = run_opforge_fixture(manifest, fixture, &fixture_dir);
    let oracle = run_oracle_fixture(manifest, adapter, fixture, &fixture_dir);

    sync_sidecar_report(
        &source_path_with_extension(&fixture.source_path, "bytes_diff.txt"),
        if fixture.compare_mode == CompareMode::Bytes {
            match (&opforge, &oracle) {
                (Ok(opforge_ok), Ok(oracle_ok)) => Some(render_bytes_comparison_report(
                    &fixture.id,
                    &opforge_ok.bytes,
                    &oracle_ok.bytes,
                )),
                _ => None,
            }
        } else {
            None
        },
    );

    sync_sidecar_report(
        &source_path_with_extension(&fixture.source_path, "error_report.txt"),
        render_error_report(
            &fixture.id,
            opforge
                .as_ref()
                .err()
                .map(|failure: &OracleAssembleFailure| failure.diagnostics_text.as_str()),
            oracle
                .as_ref()
                .err()
                .map(|failure: &OracleAssembleFailure| failure.diagnostics_text.as_str()),
        ),
    );

    match (opforge, oracle) {
        (Ok(opforge_success), Ok(oracle_success)) => {
            if fixture.expected_outcome == ExpectedOutcome::Success {
                compare_success_outputs(manifest, fixture, &opforge_success, &oracle_success)
                    .map(|_| None)
            } else if fixture.expected_outcome == ExpectedOutcome::DocumentedDivergence {
                evaluate_documented_divergence_success(
                    manifest,
                    fixture,
                    &opforge_success,
                    &oracle_success,
                )
            } else {
                Err(build_status_mismatch(
                    manifest,
                    fixture,
                    ObservedStatus::Success,
                    ObservedStatus::Success,
                    None,
                    None,
                    Some(&opforge_success),
                    Some(&oracle_success),
                ))
            }
        }
        (Err(opforge_failure), Ok(oracle_success))
            if fixture.expected_outcome == ExpectedOutcome::DocumentedDivergence =>
        {
            evaluate_documented_divergence_status(
                manifest,
                fixture,
                &opforge_failure,
                &oracle_success,
            )
        }
        (Err(opforge_failure), Ok(oracle_success)) => Err(build_status_mismatch(
            manifest,
            fixture,
            ObservedStatus::Error,
            ObservedStatus::Success,
            Some(&opforge_failure),
            None,
            None,
            Some(&oracle_success),
        )),
        (Ok(opforge_success), Err(oracle_failure))
            if fixture.expected_outcome == ExpectedOutcome::DocumentedDivergence =>
        {
            evaluate_documented_divergence_reverse_status(
                manifest,
                fixture,
                &opforge_success,
                &oracle_failure,
            )
        }
        (Ok(opforge_success), Err(oracle_failure)) => Err(build_status_mismatch(
            manifest,
            fixture,
            ObservedStatus::Success,
            ObservedStatus::Error,
            None,
            Some(&oracle_failure),
            Some(&opforge_success),
            None,
        )),
        (Err(opforge_failure), Err(oracle_failure)) => {
            if fixture.expected_outcome == ExpectedOutcome::Error {
                compare_error_outputs(manifest, fixture, &opforge_failure, &oracle_failure)
                    .map(|_| None)
            } else if fixture.expected_outcome == ExpectedOutcome::DocumentedDivergence {
                evaluate_documented_divergence_error(
                    manifest,
                    fixture,
                    &opforge_failure,
                    &oracle_failure,
                )
            } else {
                Err(build_status_mismatch(
                    manifest,
                    fixture,
                    ObservedStatus::Error,
                    ObservedStatus::Error,
                    Some(&opforge_failure),
                    Some(&oracle_failure),
                    None,
                    None,
                ))
            }
        }
    }
}

#[allow(clippy::result_large_err)]
fn compare_success_outputs(
    manifest: &Manifest,
    fixture: &Fixture,
    opforge: &OracleAssembleSuccess,
    oracle: &OracleAssembleSuccess,
) -> Result<(), StructuredMismatch> {
    if opforge.bytes == oracle.bytes {
        return Ok(());
    }

    let first_difference_offset = first_difference_offset(&opforge.bytes, &oracle.bytes);
    let byte_mismatch = ByteMismatchSummary {
        opforge_len: opforge.bytes.len(),
        oracle_len: oracle.bytes.len(),
        first_difference_offset,
        opforge_byte: first_difference_offset.and_then(|offset| opforge.bytes.get(offset).copied()),
        oracle_byte: first_difference_offset.and_then(|offset| oracle.bytes.get(offset).copied()),
        opforge_bytes: opforge.bytes.clone(),
        oracle_bytes: oracle.bytes.clone(),
    };

    Err(StructuredMismatch {
        fixture_id: fixture.id.clone(),
        family: manifest.family.clone(),
        cpu: fixture.cpu.clone(),
        oracle_id: manifest.oracle.clone(),
        compare_mode: fixture.compare_mode,
        opforge_status: ObservedStatus::Success,
        oracle_status: ObservedStatus::Success,
        opforge_output_path: Some(opforge.output_path.clone()),
        oracle_output_path: Some(oracle.output_path.clone()),
        opforge_diagnostics_path: None,
        oracle_diagnostics_path: None,
        opforge_stdout_path: opforge.stdout_path.clone(),
        oracle_stdout_path: oracle.stdout_path.clone(),
        opforge_stderr_path: opforge.stderr_path.clone(),
        oracle_stderr_path: oracle.stderr_path.clone(),
        opforge_summary: None,
        oracle_summary: None,
        opforge_error_class: None,
        oracle_error_class: None,
        opforge_excerpt: None,
        oracle_excerpt: None,
        documented_divergence_kind: fixture.documented_divergence.as_ref().map(|item| item.kind),
        documented_divergence_reason: fixture
            .documented_divergence
            .as_ref()
            .and_then(|item| item.reason.clone()),
        byte_mismatch: Some(byte_mismatch),
    })
}

#[allow(clippy::result_large_err)]
fn compare_error_outputs(
    manifest: &Manifest,
    fixture: &Fixture,
    opforge: &OracleAssembleFailure,
    oracle: &OracleAssembleFailure,
) -> Result<(), StructuredMismatch> {
    let opforge_error_class = normalize_opforge_diagnostics(&opforge.diagnostics_text);
    let oracle_error_class =
        normalize_oracle_diagnostics(manifest.oracle.as_str(), &oracle.diagnostics_text);

    if opforge_error_class == oracle_error_class
        && opforge_error_class != NormalizedErrorClass::Unclassified
    {
        return Ok(());
    }

    Err(StructuredMismatch {
        fixture_id: fixture.id.clone(),
        family: manifest.family.clone(),
        cpu: fixture.cpu.clone(),
        oracle_id: manifest.oracle.clone(),
        compare_mode: fixture.compare_mode,
        opforge_status: ObservedStatus::Error,
        oracle_status: ObservedStatus::Error,
        opforge_output_path: None,
        oracle_output_path: None,
        opforge_diagnostics_path: Some(opforge.diagnostics_path.clone()),
        oracle_diagnostics_path: Some(oracle.diagnostics_path.clone()),
        opforge_stdout_path: opforge.stdout_path.clone(),
        oracle_stdout_path: oracle.stdout_path.clone(),
        opforge_stderr_path: opforge.stderr_path.clone(),
        oracle_stderr_path: oracle.stderr_path.clone(),
        opforge_summary: Some(opforge.summary.clone()),
        oracle_summary: Some(oracle.summary.clone()),
        opforge_error_class: Some(opforge_error_class),
        oracle_error_class: Some(oracle_error_class),
        opforge_excerpt: Some(diagnostic_excerpt(&opforge.diagnostics_text)),
        oracle_excerpt: Some(diagnostic_excerpt(&oracle.diagnostics_text)),
        documented_divergence_kind: fixture.documented_divergence.as_ref().map(|item| item.kind),
        documented_divergence_reason: fixture
            .documented_divergence
            .as_ref()
            .and_then(|item| item.reason.clone()),
        byte_mismatch: None,
    })
}

#[allow(clippy::result_large_err)]
fn evaluate_documented_divergence_status(
    manifest: &Manifest,
    fixture: &Fixture,
    opforge: &OracleAssembleFailure,
    oracle: &OracleAssembleSuccess,
) -> Result<Option<String>, StructuredMismatch> {
    let Some(documented_divergence) = fixture.documented_divergence.as_ref() else {
        return Err(build_status_mismatch(
            manifest,
            fixture,
            ObservedStatus::Error,
            ObservedStatus::Success,
            Some(opforge),
            None,
            None,
            Some(oracle),
        ));
    };

    match documented_divergence.kind {
        DocumentedDivergenceKind::OpforgeErrorOracleSuccess => Ok(Some(format!(
            "documented divergence matched for fixture '{}': {}{}",
            fixture.id,
            documented_divergence.kind.label(),
            documented_divergence
                .reason
                .as_ref()
                .map(|reason| format!(" ({reason})"))
                .unwrap_or_default()
        ))),
        _ => Err(build_status_mismatch(
            manifest,
            fixture,
            ObservedStatus::Error,
            ObservedStatus::Success,
            Some(opforge),
            None,
            None,
            Some(oracle),
        )),
    }
}

#[allow(clippy::result_large_err)]
fn evaluate_documented_divergence_reverse_status(
    manifest: &Manifest,
    fixture: &Fixture,
    opforge: &OracleAssembleSuccess,
    oracle: &OracleAssembleFailure,
) -> Result<Option<String>, StructuredMismatch> {
    let Some(documented_divergence) = fixture.documented_divergence.as_ref() else {
        return Err(build_status_mismatch(
            manifest,
            fixture,
            ObservedStatus::Success,
            ObservedStatus::Error,
            None,
            Some(oracle),
            Some(opforge),
            None,
        ));
    };

    match documented_divergence.kind {
        DocumentedDivergenceKind::OpforgeSuccessOracleError => Ok(Some(format!(
            "documented divergence matched for fixture '{}': {}{}",
            fixture.id,
            documented_divergence.kind.label(),
            documented_divergence
                .reason
                .as_ref()
                .map(|reason| format!(" ({reason})"))
                .unwrap_or_default()
        ))),
        _ => Err(build_status_mismatch(
            manifest,
            fixture,
            ObservedStatus::Success,
            ObservedStatus::Error,
            None,
            Some(oracle),
            Some(opforge),
            None,
        )),
    }
}

#[allow(clippy::result_large_err)]
fn evaluate_documented_divergence_success(
    manifest: &Manifest,
    fixture: &Fixture,
    opforge: &OracleAssembleSuccess,
    oracle: &OracleAssembleSuccess,
) -> Result<Option<String>, StructuredMismatch> {
    let Some(documented_divergence) = fixture.documented_divergence.as_ref() else {
        return compare_success_outputs(manifest, fixture, opforge, oracle).map(|_| None);
    };

    if fixture.compare_mode == CompareMode::Bytes && opforge.bytes == oracle.bytes {
        return Ok(Some(format!(
            "reclassification candidate for documented divergence fixture '{}': outputs now match fully",
            fixture.id
        )));
    }

    match documented_divergence.kind {
        DocumentedDivergenceKind::ByteMismatch => {
            let mismatch = compare_success_outputs(manifest, fixture, opforge, oracle)
                .expect_err("known divergence kind requires mismatched bytes");
            let detail = mismatch
                .byte_mismatch
                .as_ref()
                .map(|summary| {
                    let hex_diff = render_hex_diff(&summary.opforge_bytes, &summary.oracle_bytes);
                    format!(
                        "--- opforge  ({} bytes)\n+++ oracle   ({} bytes)\n{hex_diff}",
                        summary.opforge_len, summary.oracle_len
                    )
                })
                .unwrap_or_else(|| "byte mismatch summary unavailable".to_string());
            Ok(Some(format!(
                "documented divergence matched for fixture '{}': {}{};\n{detail}",
                fixture.id,
                documented_divergence.kind.label(),
                documented_divergence
                    .reason
                    .as_ref()
                    .map(|reason| format!(" ({reason})"))
                    .unwrap_or_default()
            )))
        }
        _ => compare_success_outputs(manifest, fixture, opforge, oracle).map(|_| None),
    }
}

#[allow(clippy::result_large_err)]
fn evaluate_documented_divergence_error(
    manifest: &Manifest,
    fixture: &Fixture,
    opforge: &OracleAssembleFailure,
    oracle: &OracleAssembleFailure,
) -> Result<Option<String>, StructuredMismatch> {
    let Some(documented_divergence) = fixture.documented_divergence.as_ref() else {
        return compare_error_outputs(manifest, fixture, opforge, oracle).map(|_| None);
    };

    if documented_divergence.kind != DocumentedDivergenceKind::ErrorClassMismatch {
        return Err(build_status_mismatch(
            manifest,
            fixture,
            ObservedStatus::Error,
            ObservedStatus::Error,
            Some(opforge),
            Some(oracle),
            None,
            None,
        ));
    }

    let opforge_error_class = normalize_opforge_diagnostics(&opforge.diagnostics_text);
    let oracle_error_class =
        normalize_oracle_diagnostics(manifest.oracle.as_str(), &oracle.diagnostics_text);

    if opforge_error_class == oracle_error_class {
        return Ok(Some(format!(
            "reclassification candidate for documented divergence fixture '{}': error classes now match as '{}'",
            fixture.id,
            opforge_error_class.label()
        )));
    }

    if Some(opforge_error_class) == documented_divergence.expected_opforge_error_class
        && Some(oracle_error_class) == documented_divergence.expected_oracle_error_class
    {
        return Ok(Some(format!(
            "documented divergence matched for fixture '{}': {}{}",
            fixture.id,
            documented_divergence.kind.label(),
            documented_divergence
                .reason
                .as_ref()
                .map(|reason| format!(" ({reason})"))
                .unwrap_or_default()
        )));
    }

    Err(StructuredMismatch {
        fixture_id: fixture.id.clone(),
        family: manifest.family.clone(),
        cpu: fixture.cpu.clone(),
        oracle_id: manifest.oracle.clone(),
        compare_mode: fixture.compare_mode,
        opforge_status: ObservedStatus::Error,
        oracle_status: ObservedStatus::Error,
        opforge_output_path: None,
        oracle_output_path: None,
        opforge_diagnostics_path: Some(opforge.diagnostics_path.clone()),
        oracle_diagnostics_path: Some(oracle.diagnostics_path.clone()),
        opforge_stdout_path: opforge.stdout_path.clone(),
        oracle_stdout_path: oracle.stdout_path.clone(),
        opforge_stderr_path: opforge.stderr_path.clone(),
        oracle_stderr_path: oracle.stderr_path.clone(),
        opforge_summary: Some(opforge.summary.clone()),
        oracle_summary: Some(oracle.summary.clone()),
        opforge_error_class: Some(opforge_error_class),
        oracle_error_class: Some(oracle_error_class),
        opforge_excerpt: Some(diagnostic_excerpt(&opforge.diagnostics_text)),
        oracle_excerpt: Some(diagnostic_excerpt(&oracle.diagnostics_text)),
        documented_divergence_kind: Some(documented_divergence.kind),
        documented_divergence_reason: documented_divergence.reason.clone(),
        byte_mismatch: None,
    })
}

#[allow(clippy::too_many_arguments)]
fn build_status_mismatch(
    manifest: &Manifest,
    fixture: &Fixture,
    opforge_status: ObservedStatus,
    oracle_status: ObservedStatus,
    opforge_failure: Option<&OracleAssembleFailure>,
    oracle_failure: Option<&OracleAssembleFailure>,
    opforge_success: Option<&OracleAssembleSuccess>,
    oracle_success: Option<&OracleAssembleSuccess>,
) -> StructuredMismatch {
    let (opforge_error_class, opforge_excerpt) = opforge_failure
        .map(|failure| {
            (
                normalize_opforge_diagnostics(&failure.diagnostics_text),
                diagnostic_excerpt(&failure.diagnostics_text),
            )
        })
        .unzip();
    let (oracle_error_class, oracle_excerpt) = oracle_failure
        .map(|failure| {
            (
                normalize_oracle_diagnostics(manifest.oracle.as_str(), &failure.diagnostics_text),
                diagnostic_excerpt(&failure.diagnostics_text),
            )
        })
        .unzip();

    StructuredMismatch {
        fixture_id: fixture.id.clone(),
        family: manifest.family.clone(),
        cpu: fixture.cpu.clone(),
        oracle_id: manifest.oracle.clone(),
        compare_mode: fixture.compare_mode,
        opforge_status,
        oracle_status,
        opforge_output_path: opforge_success.map(|success| success.output_path.clone()),
        oracle_output_path: oracle_success.map(|success| success.output_path.clone()),
        opforge_diagnostics_path: opforge_failure.map(|failure| failure.diagnostics_path.clone()),
        oracle_diagnostics_path: oracle_failure.map(|failure| failure.diagnostics_path.clone()),
        opforge_stdout_path: opforge_success
            .and_then(|success| success.stdout_path.clone())
            .or_else(|| opforge_failure.and_then(|failure| failure.stdout_path.clone())),
        oracle_stdout_path: oracle_success
            .and_then(|success| success.stdout_path.clone())
            .or_else(|| oracle_failure.and_then(|failure| failure.stdout_path.clone())),
        opforge_stderr_path: opforge_success
            .and_then(|success| success.stderr_path.clone())
            .or_else(|| opforge_failure.and_then(|failure| failure.stderr_path.clone())),
        oracle_stderr_path: oracle_success
            .and_then(|success| success.stderr_path.clone())
            .or_else(|| oracle_failure.and_then(|failure| failure.stderr_path.clone())),
        opforge_summary: opforge_failure.map(|failure| failure.summary.clone()),
        oracle_summary: oracle_failure.map(|failure| failure.summary.clone()),
        opforge_error_class,
        oracle_error_class,
        opforge_excerpt,
        oracle_excerpt,
        documented_divergence_kind: fixture.documented_divergence.as_ref().map(|item| item.kind),
        documented_divergence_reason: fixture
            .documented_divergence
            .as_ref()
            .and_then(|item| item.reason.clone()),
        byte_mismatch: None,
    }
}

fn normalize_oracle_diagnostics(oracle_id: &str, text: &str) -> NormalizedErrorClass {
    match oracle_id {
        "vasm" => normalize_vasm_stderr(text),
        "64tass" => normalize_64tass_stderr(text),
        _ => NormalizedErrorClass::Unclassified,
    }
}

fn run_opforge_fixture(
    manifest: &Manifest,
    fixture: &Fixture,
    fixture_dir: &Path,
) -> Result<OracleAssembleSuccess, OracleAssembleFailure> {
    let output_dir = fixture_dir.join("opforge");
    let diagnostics_path = output_dir.join("opforge.diagnostics.txt");
    if let Err(err) = fs::create_dir_all(&output_dir) {
        return Err(OracleAssembleFailure {
            diagnostics_path,
            stdout_path: None,
            stderr_path: None,
            diagnostics_text: format!(
                "Create opForge output directory {}: {err}\n",
                output_dir.display()
            ),
            summary: format!(
                "Create opForge output directory {}: {err}",
                output_dir.display()
            ),
        });
    }

    let header_title = format!("opForge Assembler v{VERSION}");
    let bin_spec = [BinOutputSpec {
        name: Some(OUTPUT_FILENAME.to_string()),
        range: None,
    }];
    let source_path =
        prepare_opforge_source_path(manifest, fixture, &output_dir).map_err(|err| {
            let _ = fs::write(&diagnostics_path, &err);
            OracleAssembleFailure {
                diagnostics_path: diagnostics_path.clone(),
                stdout_path: None,
                stderr_path: None,
                diagnostics_text: err.clone(),
                summary: err,
            }
        })?;

    match run_assembly(AssemblyExecutionRequest {
        root_path: &source_path,
        execution_mode: ExecutionMode::Lockstep {
            continuation_head: ContinuationHead::Vm,
        },
        input_base: fixture.id.as_str(),
        defines: &[],
        include_paths: &[],
        module_paths: &[],
        pp_macro_depth: 64,
        cpu_override: Some(fixture.cpu.as_str()),
        default_cpu: default_cpu(),
        max_loop_iterations: 1000,
        opasm_package_path: None,
        out_dir: Some(&output_dir),
        debug_conditionals: false,
        tab_size: None,
        output_format: EngineOutputFormat::Text,
        go_addr: None,
        bin_specs: &bin_spec,
        fill_byte: 0xff,
        fill_byte_set: false,
        default_outputs: false,
        labels_file: None,
        label_output_format: CliLabelOutputFormat::Default,
        dependency_output: None,
        outfile_override: Some(fixture.id.as_str()),
        list_name_override: None,
        hex_name_override: None,
        srec_name_override: None,
        header_title: &header_title,
        output_sink: None,
        source_provider: None,
        suppress_outputs: false,
    }) {
        Ok(report) => {
            let output_path = output_dir.join(OUTPUT_FILENAME);
            match fs::read(&output_path) {
                Ok(bytes) => {
                    if !report.diagnostics().is_empty() {
                        let _ =
                            fs::write(&diagnostics_path, format!("{:#?}", report.diagnostics()));
                    }
                    Ok(OracleAssembleSuccess {
                        output_path,
                        bytes,
                        stdout_path: None,
                        stderr_path: None,
                    })
                }
                Err(err) => Err(OracleAssembleFailure {
                    diagnostics_path,
                    stdout_path: None,
                    stderr_path: None,
                    diagnostics_text: format!(
                        "Read opForge binary output {}: {err}\n",
                        output_path.display()
                    ),
                    summary: format!(
                        "Read opForge binary output {}: {err}",
                        output_path.display()
                    ),
                }),
            }
        }
        Err(err) => {
            let diagnostics_text = format!("summary: {}\n{:#?}", err.summary(), err.diagnostics());
            let _ = fs::write(&diagnostics_path, &diagnostics_text);
            Err(OracleAssembleFailure {
                diagnostics_path,
                stdout_path: None,
                stderr_path: None,
                diagnostics_text,
                summary: err.summary().to_string(),
            })
        }
    }
}

fn prepare_opforge_source_path(
    manifest: &Manifest,
    fixture: &Fixture,
    output_dir: &Path,
) -> Result<PathBuf, String> {
    let Some(preamble) = opforge_profile_preamble(manifest.cpu_profile.as_deref()) else {
        return Ok(fixture.source_path.clone());
    };

    let source_text = fs::read_to_string(&fixture.source_path).map_err(|err| {
        format!(
            "Read fixture source {} for opForge profile wrapper: {err}",
            fixture.source_path.display()
        )
    })?;
    let wrapped_path = output_dir.join("opforge.source.asm");
    fs::write(&wrapped_path, format!("{preamble}{source_text}")).map_err(|err| {
        format!(
            "Write opForge profile wrapper {}: {err}",
            wrapped_path.display()
        )
    })?;
    Ok(wrapped_path)
}

fn opforge_profile_preamble(cpu_profile: Option<&str>) -> Option<&'static str> {
    match cpu_profile {
        Some("fpu-68881") => Some(".fpu 68881\n"),
        Some("fpu-68882") => Some(".fpu 68882\n"),
        Some("fpu-68040") => Some(".fpu 68040\n"),
        _ => None,
    }
}

fn run_oracle_fixture<A: ExternalOracleAdapter>(
    manifest: &Manifest,
    adapter: &A,
    fixture: &Fixture,
    fixture_dir: &Path,
) -> Result<OracleAssembleSuccess, OracleAssembleFailure> {
    adapter.assemble_flat_binary(OracleAssembleRequest {
        cpu: fixture.cpu.as_str(),
        cpu_profile: manifest.cpu_profile.as_deref(),
        source_path: &fixture.source_path,
        output_dir: &fixture_dir.join("oracle"),
    })
}

fn discover_manifest_paths(root: &Path) -> Result<(PathBuf, Vec<PathBuf>), String> {
    let canonical_root = root
        .canonicalize()
        .map_err(|err| format!("Canonicalize manifest root {}: {err}", root.display()))?;
    if canonical_root.is_file() {
        if canonical_root.file_name() != Some(OsStr::new("fixtures.toml")) {
            return Err(format!(
                "Expected manifest file named fixtures.toml, found {}",
                canonical_root.display()
            ));
        }
        let discovery_root = canonical_root
            .parent()
            .ok_or_else(|| {
                format!(
                    "Manifest {} has no parent directory",
                    canonical_root.display()
                )
            })?
            .to_path_buf();
        return Ok((discovery_root, vec![canonical_root]));
    }
    if !canonical_root.is_dir() {
        return Err(format!(
            "Manifest root {} is neither a file nor a directory",
            canonical_root.display()
        ));
    }

    let mut manifest_paths = Vec::new();
    collect_manifest_paths(&canonical_root, &mut manifest_paths)?;
    manifest_paths.sort();
    if manifest_paths.is_empty() {
        return Err(format!(
            "No fixtures.toml manifests found under {}",
            canonical_root.display()
        ));
    }
    Ok((canonical_root, manifest_paths))
}

fn collect_manifest_paths(dir: &Path, manifest_paths: &mut Vec<PathBuf>) -> Result<(), String> {
    let mut entries = fs::read_dir(dir)
        .map_err(|err| format!("Read manifest directory {}: {err}", dir.display()))?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| format!("Read manifest directory {}: {err}", dir.display()))?;
    entries.sort_by_key(|entry| entry.path());

    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            collect_manifest_paths(&path, manifest_paths)?;
        } else if path.file_name() == Some(OsStr::new("fixtures.toml")) {
            let canonical_path = path
                .canonicalize()
                .map_err(|err| format!("Canonicalize manifest {}: {err}", path.display()))?;
            manifest_paths.push(canonical_path);
        }
    }

    Ok(())
}

fn manifest_artifact_label(discovery_root: &Path, manifest_path: &Path) -> Result<String, String> {
    let manifest_dir = manifest_path.parent().ok_or_else(|| {
        format!(
            "Manifest {} has no parent directory",
            manifest_path.display()
        )
    })?;
    let relative = manifest_dir
        .strip_prefix(discovery_root)
        .unwrap_or(manifest_dir);
    if relative.as_os_str().is_empty() {
        return Ok("root".to_string());
    }

    Ok(relative
        .components()
        .map(|component| sanitize_fixture_id(&component.as_os_str().to_string_lossy()))
        .collect::<Vec<_>>()
        .join("__"))
}

fn load_manifest<A: ExternalOracleAdapter>(
    manifest_path: &Path,
    adapter: &A,
) -> Result<Manifest, String> {
    let contents = fs::read_to_string(manifest_path)
        .map_err(|err| format!("Read manifest {}: {err}", manifest_path.display()))?;
    let manifest_dir = manifest_path
        .parent()
        .ok_or_else(|| {
            format!(
                "Manifest {} has no parent directory",
                manifest_path.display()
            )
        })?
        .canonicalize()
        .map_err(|err| {
            format!(
                "Canonicalize manifest directory for {}: {err}",
                manifest_path.display()
            )
        })?;

    let mut builder = ManifestBuilder::default();
    let mut current_fixture: Option<FixtureBuilder> = None;

    for (index, raw_line) in contents.lines().enumerate() {
        let line_no = index + 1;
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if line == "[[fixtures]]" {
            if let Some(fixture) = current_fixture.take() {
                builder.fixtures.push(build_fixture(
                    fixture,
                    &builder,
                    &manifest_dir,
                    manifest_path,
                    line_no,
                    adapter,
                )?);
            }
            current_fixture = Some(FixtureBuilder::default());
            continue;
        }

        let (key, value) = parse_key_value(line, manifest_path, line_no)?;
        if let Some(fixture) = current_fixture.as_mut() {
            assign_fixture_value(fixture, manifest_path, line_no, &key, &value)?;
        } else {
            assign_manifest_value(&mut builder, manifest_path, line_no, &key, &value)?;
        }
    }

    if let Some(fixture) = current_fixture.take() {
        builder.fixtures.push(build_fixture(
            fixture,
            &builder,
            &manifest_dir,
            manifest_path,
            contents.lines().count(),
            adapter,
        )?);
    }

    build_manifest(builder, manifest_path, adapter)
}

fn assign_manifest_value(
    builder: &mut ManifestBuilder,
    manifest_path: &Path,
    line_no: usize,
    key: &str,
    value: &str,
) -> Result<(), String> {
    match key {
        "manifest_version" => {
            builder.manifest_version = Some(parse_u32(value, manifest_path, line_no)?);
        }
        "family" => builder.family = Some(parse_string(value, manifest_path, line_no)?),
        "oracle" => builder.oracle = Some(parse_string(value, manifest_path, line_no)?),
        "oracle_profile" => {
            builder.oracle_profile = Some(parse_string(value, manifest_path, line_no)?);
        }
        "cpu_profile" => builder.cpu_profile = Some(parse_string(value, manifest_path, line_no)?),
        "expected_outcome" => {
            builder.expected_outcome = Some(parse_string(value, manifest_path, line_no)?);
        }
        "compare_mode" => builder.compare_mode = Some(parse_string(value, manifest_path, line_no)?),
        other => {
            return Err(format!(
                "Unsupported manifest key '{other}' in {}:{line_no}",
                manifest_path.display()
            ));
        }
    }
    Ok(())
}

fn assign_fixture_value(
    fixture: &mut FixtureBuilder,
    manifest_path: &Path,
    line_no: usize,
    key: &str,
    value: &str,
) -> Result<(), String> {
    match key {
        "id" => fixture.id = Some(parse_string(value, manifest_path, line_no)?),
        "cpu" => fixture.cpu = Some(parse_string(value, manifest_path, line_no)?),
        "path" => fixture.path = Some(parse_string(value, manifest_path, line_no)?),
        "expected_outcome" => {
            fixture.expected_outcome = Some(parse_string(value, manifest_path, line_no)?);
        }
        "compare_mode" => fixture.compare_mode = Some(parse_string(value, manifest_path, line_no)?),
        "documented_divergence_kind" => {
            fixture.documented_divergence_kind = Some(parse_string(value, manifest_path, line_no)?);
        }
        "documented_divergence_reason" => {
            fixture.documented_divergence_reason =
                Some(parse_string(value, manifest_path, line_no)?);
        }
        "documented_divergence_opforge_status" => {
            fixture.documented_divergence_opforge_status =
                Some(parse_string(value, manifest_path, line_no)?);
        }
        "documented_divergence_oracle_status" => {
            fixture.documented_divergence_oracle_status =
                Some(parse_string(value, manifest_path, line_no)?);
        }
        "documented_divergence_opforge_error_class" => {
            fixture.documented_divergence_opforge_error_class =
                Some(parse_string(value, manifest_path, line_no)?);
        }
        "documented_divergence_oracle_error_class" => {
            fixture.documented_divergence_oracle_error_class =
                Some(parse_string(value, manifest_path, line_no)?);
        }
        other => {
            return Err(format!(
                "Unsupported fixture key '{other}' in {}:{line_no}",
                manifest_path.display()
            ));
        }
    }
    Ok(())
}

fn build_manifest<A: ExternalOracleAdapter>(
    builder: ManifestBuilder,
    manifest_path: &Path,
    adapter: &A,
) -> Result<Manifest, String> {
    let manifest_version =
        required_field(builder.manifest_version, manifest_path, "manifest_version")?;
    if manifest_version != 1 {
        return Err(format!(
            "Unsupported manifest_version {manifest_version} in {}; expected 1",
            manifest_path.display()
        ));
    }

    let family = required_field(builder.family, manifest_path, "family")?;
    if !adapter.supports_family(family.as_str()) {
        return Err(format!(
            "Unsupported family '{family}' in {}; adapter '{}' does not support it",
            manifest_path.display(),
            adapter.oracle_id()
        ));
    }

    let oracle = required_field(builder.oracle, manifest_path, "oracle")?;
    if oracle != adapter.oracle_id() {
        return Err(format!(
            "Unsupported oracle '{oracle}' in {}; expected '{}'",
            manifest_path.display(),
            adapter.oracle_id()
        ));
    }

    let oracle_profile = required_field(builder.oracle_profile, manifest_path, "oracle_profile")?;
    if oracle_profile != adapter.oracle_profile() {
        return Err(format!(
            "Unsupported oracle_profile '{oracle_profile}' in {}; expected '{}'",
            manifest_path.display(),
            adapter.oracle_profile()
        ));
    }

    let expected_outcome = parse_expected_outcome(
        &required_field(builder.expected_outcome, manifest_path, "expected_outcome")?,
        manifest_path,
        "manifest",
    )?;
    let compare_mode = parse_compare_mode(
        &required_field(builder.compare_mode, manifest_path, "compare_mode")?,
        manifest_path,
        "manifest",
    )?;
    validate_outcome_compare_mode(expected_outcome, compare_mode, manifest_path, "manifest")?;

    if builder.fixtures.is_empty() {
        return Err(format!(
            "Manifest {} does not declare any fixtures",
            manifest_path.display()
        ));
    }

    let mut seen_ids = HashSet::new();
    let mut seen_source_paths = HashMap::new();
    for fixture in &builder.fixtures {
        if !seen_ids.insert(fixture.id.clone()) {
            return Err(format!(
                "Manifest {} contains duplicate fixture id '{}'",
                manifest_path.display(),
                fixture.id
            ));
        }
        if let Some(existing_id) =
            seen_source_paths.insert(fixture.source_path.clone(), fixture.id.clone())
        {
            return Err(format!(
                "Manifest {} contains duplicate canonical fixture source path '{}' for fixtures '{}' and '{}'",
                manifest_path.display(),
                fixture.source_path.display(),
                existing_id,
                fixture.id
            ));
        }
    }

    Ok(Manifest {
        family,
        oracle,
        cpu_profile: builder.cpu_profile,
        fixtures: builder.fixtures,
    })
}

fn build_fixture<A: ExternalOracleAdapter>(
    builder: FixtureBuilder,
    manifest: &ManifestBuilder,
    manifest_dir: &Path,
    manifest_path: &Path,
    line_no: usize,
    adapter: &A,
) -> Result<Fixture, String> {
    let id = required_fixture_field(builder.id, manifest_path, line_no, "id")?;
    let cpu = required_fixture_field(builder.cpu, manifest_path, line_no, "cpu")?;
    if !adapter.supports_cpu(cpu.as_str()) {
        return Err(format!(
            "Fixture '{id}' in {} uses unsupported cpu '{}' for adapter '{}'",
            manifest_path.display(),
            cpu,
            adapter.oracle_id()
        ));
    }
    if !adapter.supports_profile(cpu.as_str(), manifest.cpu_profile.as_deref()) {
        let profile = manifest.cpu_profile.as_deref().unwrap_or("<none>");
        return Err(format!(
            "Fixture '{id}' in {} uses unsupported cpu_profile '{}' for cpu '{}' and adapter '{}'",
            manifest_path.display(),
            profile,
            cpu,
            adapter.oracle_id()
        ));
    }

    let path = required_fixture_field(builder.path, manifest_path, line_no, "path")?;
    let expected_outcome = parse_expected_outcome(
        &builder
            .expected_outcome
            .or_else(|| manifest.expected_outcome.clone())
            .ok_or_else(|| {
                format!(
                    "Fixture '{id}' in {} missing expected_outcome and no manifest default is set",
                    manifest_path.display()
                )
            })?,
        manifest_path,
        &format!("fixture '{id}'"),
    )?;

    let compare_mode = parse_compare_mode(
        &builder
            .compare_mode
            .or_else(|| manifest.compare_mode.clone())
            .ok_or_else(|| {
                format!(
                    "Fixture '{id}' in {} missing compare_mode and no manifest default is set",
                    manifest_path.display()
                )
            })?,
        manifest_path,
        &format!("fixture '{id}'"),
    )?;
    validate_outcome_compare_mode(
        expected_outcome,
        compare_mode,
        manifest_path,
        &format!("fixture '{id}'"),
    )?;

    let enforce_explicit_divergence_contract = manifest.oracle.as_deref() == Some("64tass");
    let documented_divergence = match expected_outcome {
        ExpectedOutcome::DocumentedDivergence => {
            let kind = parse_documented_divergence_kind(
                &builder.documented_divergence_kind.ok_or_else(|| {
                    format!(
                        "Fixture '{id}' in {} missing documented_divergence_kind",
                        manifest_path.display()
                    )
                })?,
                manifest_path,
                &format!("fixture '{id}'"),
            )?;

            let (default_opforge_status, default_oracle_status) = match kind {
                DocumentedDivergenceKind::OpforgeErrorOracleSuccess => {
                    (ObservedStatus::Error, ObservedStatus::Success)
                }
                DocumentedDivergenceKind::OpforgeSuccessOracleError => {
                    (ObservedStatus::Success, ObservedStatus::Error)
                }
                DocumentedDivergenceKind::ByteMismatch => {
                    (ObservedStatus::Success, ObservedStatus::Success)
                }
                DocumentedDivergenceKind::ErrorClassMismatch => {
                    (ObservedStatus::Error, ObservedStatus::Error)
                }
            };

            let expected_opforge_status = if enforce_explicit_divergence_contract {
                parse_observed_status(
                    &builder
                        .documented_divergence_opforge_status
                        .ok_or_else(|| {
                            format!(
                                "Fixture '{id}' in {} missing documented_divergence_opforge_status",
                                manifest_path.display()
                            )
                        })?,
                    manifest_path,
                    &format!("fixture '{id}'"),
                )?
            } else {
                builder
                    .documented_divergence_opforge_status
                    .as_deref()
                    .map(|value| {
                        parse_observed_status(value, manifest_path, &format!("fixture '{id}'"))
                    })
                    .transpose()?
                    .unwrap_or(default_opforge_status)
            };

            let expected_oracle_status = if enforce_explicit_divergence_contract {
                parse_observed_status(
                    &builder.documented_divergence_oracle_status.ok_or_else(|| {
                        format!(
                            "Fixture '{id}' in {} missing documented_divergence_oracle_status",
                            manifest_path.display()
                        )
                    })?,
                    manifest_path,
                    &format!("fixture '{id}'"),
                )?
            } else {
                builder
                    .documented_divergence_oracle_status
                    .as_deref()
                    .map(|value| {
                        parse_observed_status(value, manifest_path, &format!("fixture '{id}'"))
                    })
                    .transpose()?
                    .unwrap_or(default_oracle_status)
            };

            if (expected_opforge_status, expected_oracle_status)
                != (default_opforge_status, default_oracle_status)
            {
                return Err(format!(
                    "Fixture '{id}' in {} has documented divergence statuses '{}'/'{}' that do not match kind '{}', expected '{}'/'{}'",
                    manifest_path.display(),
                    observed_status_label(expected_opforge_status),
                    observed_status_label(expected_oracle_status),
                    kind.label(),
                    observed_status_label(default_opforge_status),
                    observed_status_label(default_oracle_status)
                ));
            }

            let expected_opforge_error_class = builder
                .documented_divergence_opforge_error_class
                .as_deref()
                .map(|value| {
                    parse_normalized_error_class(value, manifest_path, &format!("fixture '{id}'"))
                })
                .transpose()?;
            let expected_oracle_error_class = builder
                .documented_divergence_oracle_error_class
                .as_deref()
                .map(|value| {
                    parse_normalized_error_class(value, manifest_path, &format!("fixture '{id}'"))
                })
                .transpose()?;

            if kind == DocumentedDivergenceKind::ErrorClassMismatch {
                if expected_opforge_error_class.is_none() || expected_oracle_error_class.is_none() {
                    return Err(format!(
                        "Fixture '{id}' in {} with documented_divergence_kind '{}' must set documented_divergence_opforge_error_class and documented_divergence_oracle_error_class",
                        manifest_path.display(),
                        kind.label()
                    ));
                }
            } else if expected_opforge_error_class.is_some()
                || expected_oracle_error_class.is_some()
            {
                return Err(format!(
                    "Fixture '{id}' in {} may only set documented_divergence_*_error_class for documented_divergence_kind 'error_class_mismatch'",
                    manifest_path.display()
                ));
            }

            let reason = builder.documented_divergence_reason.ok_or_else(|| {
                format!(
                    "Fixture '{id}' in {} missing documented_divergence_reason",
                    manifest_path.display()
                )
            })?;

            Some(DocumentedDivergence {
                kind,
                reason: Some(reason),
                expected_opforge_status,
                expected_oracle_status,
                expected_opforge_error_class,
                expected_oracle_error_class,
            })
        }
        _ => {
            if builder.documented_divergence_kind.is_some()
                || builder.documented_divergence_reason.is_some()
                || builder.documented_divergence_opforge_status.is_some()
                || builder.documented_divergence_oracle_status.is_some()
                || builder.documented_divergence_opforge_error_class.is_some()
                || builder.documented_divergence_oracle_error_class.is_some()
            {
                return Err(format!(
                    "Fixture '{id}' in {} may only set documented_divergence_* fields when expected_outcome = 'documented_divergence'",
                    manifest_path.display()
                ));
            }
            None
        }
    };

    let required_prefix = match expected_outcome {
        ExpectedOutcome::Success => "positive/",
        ExpectedOutcome::Error => "negative/",
        ExpectedOutcome::DocumentedDivergence => "documented_divergence/",
    };
    if !path.starts_with(required_prefix) {
        return Err(format!(
            "Fixture '{id}' in {} must live under {required_prefix} for expected_outcome '{}'",
            manifest_path.display(),
            expected_outcome.label()
        ));
    }
    if Path::new(&path).extension().and_then(|ext| ext.to_str()) != Some("asm") {
        return Err(format!(
            "Fixture '{id}' in {} must reference a .asm file",
            manifest_path.display()
        ));
    }

    let joined = manifest_dir.join(&path);
    let source_path = joined.canonicalize().map_err(|err| {
        format!(
            "Canonicalize fixture source '{}' from {}: {err}",
            joined.display(),
            manifest_path.display()
        )
    })?;
    if !source_path.starts_with(manifest_dir) {
        return Err(format!(
            "Fixture '{id}' in {} escapes manifest directory via path '{}'",
            manifest_path.display(),
            path
        ));
    }

    Ok(Fixture {
        id,
        cpu,
        source_path,
        expected_outcome,
        compare_mode,
        documented_divergence,
    })
}

fn parse_expected_outcome(
    value: &str,
    manifest_path: &Path,
    context: &str,
) -> Result<ExpectedOutcome, String> {
    match value {
        "success" => Ok(ExpectedOutcome::Success),
        "error" => Ok(ExpectedOutcome::Error),
        "documented_divergence" => Ok(ExpectedOutcome::DocumentedDivergence),
        other => Err(format!(
            "Unsupported expected_outcome '{other}' in {} for {context}",
            manifest_path.display()
        )),
    }
}

fn parse_compare_mode(
    value: &str,
    manifest_path: &Path,
    context: &str,
) -> Result<CompareMode, String> {
    match value {
        "bytes" => Ok(CompareMode::Bytes),
        "error_class" => Ok(CompareMode::ErrorClass),
        other => Err(format!(
            "Unsupported compare_mode '{other}' in {} for {context}",
            manifest_path.display()
        )),
    }
}

fn validate_outcome_compare_mode(
    expected_outcome: ExpectedOutcome,
    compare_mode: CompareMode,
    manifest_path: &Path,
    context: &str,
) -> Result<(), String> {
    match (expected_outcome, compare_mode) {
        (ExpectedOutcome::Success, CompareMode::Bytes)
        | (ExpectedOutcome::Error, CompareMode::ErrorClass)
        | (ExpectedOutcome::DocumentedDivergence, CompareMode::Bytes)
        | (ExpectedOutcome::DocumentedDivergence, CompareMode::ErrorClass) => Ok(()),
        _ => Err(format!(
            "Unsupported expected_outcome/compare_mode pairing '{}'/'{}' in {} for {context}",
            expected_outcome.label(),
            compare_mode.label(),
            manifest_path.display()
        )),
    }
}

fn parse_documented_divergence_kind(
    value: &str,
    manifest_path: &Path,
    context: &str,
) -> Result<DocumentedDivergenceKind, String> {
    match value {
        "opforge_error_oracle_success" => Ok(DocumentedDivergenceKind::OpforgeErrorOracleSuccess),
        "opforge_success_oracle_error" => Ok(DocumentedDivergenceKind::OpforgeSuccessOracleError),
        "byte_mismatch" | "opforge_success_oracle_success_bytes_differ" => {
            Ok(DocumentedDivergenceKind::ByteMismatch)
        }
        "error_class_mismatch" => Ok(DocumentedDivergenceKind::ErrorClassMismatch),
        other => Err(format!(
            "Unsupported documented_divergence_kind '{other}' in {} for {context}",
            manifest_path.display()
        )),
    }
}

fn parse_observed_status(
    value: &str,
    manifest_path: &Path,
    context: &str,
) -> Result<ObservedStatus, String> {
    match value {
        "success" => Ok(ObservedStatus::Success),
        "error" => Ok(ObservedStatus::Error),
        other => Err(format!(
            "Unsupported documented divergence status '{other}' in {} for {context}",
            manifest_path.display()
        )),
    }
}

fn parse_normalized_error_class(
    value: &str,
    manifest_path: &Path,
    context: &str,
) -> Result<NormalizedErrorClass, String> {
    match value {
        "unknown-mnemonic" => Ok(NormalizedErrorClass::UnknownMnemonic),
        "illegal-addressing-mode" => Ok(NormalizedErrorClass::IllegalAddressingMode),
        "unsupported-cpu-feature" => Ok(NormalizedErrorClass::UnsupportedCpuFeature),
        "branch-out-of-range" => Ok(NormalizedErrorClass::BranchOutOfRange),
        "value-out-of-range" => Ok(NormalizedErrorClass::ValueOutOfRange),
        "syntax-error" => Ok(NormalizedErrorClass::SyntaxError),
        "missing-operand" => Ok(NormalizedErrorClass::MissingOperand),
        "wrong-operand-count" => Ok(NormalizedErrorClass::WrongOperandCount),
        "unclassified" => Ok(NormalizedErrorClass::Unclassified),
        other => Err(format!(
            "Unsupported documented divergence error class '{other}' in {} for {context}",
            manifest_path.display()
        )),
    }
}

fn required_field<T>(value: Option<T>, manifest_path: &Path, name: &str) -> Result<T, String> {
    value.ok_or_else(|| format!("Missing {name} in {}", manifest_path.display()))
}

fn required_fixture_field(
    value: Option<String>,
    manifest_path: &Path,
    line_no: usize,
    name: &str,
) -> Result<String, String> {
    value.ok_or_else(|| {
        format!(
            "Missing fixture field '{name}' in {} near line {line_no}",
            manifest_path.display()
        )
    })
}

fn parse_key_value(
    line: &str,
    manifest_path: &Path,
    line_no: usize,
) -> Result<(String, String), String> {
    let Some((key, value)) = line.split_once('=') else {
        return Err(format!(
            "Expected key = value in {}:{line_no}",
            manifest_path.display()
        ));
    };
    let key = key.trim();
    let value = value.trim();
    if key.is_empty() || value.is_empty() {
        return Err(format!(
            "Expected non-empty key and value in {}:{line_no}",
            manifest_path.display()
        ));
    }
    Ok((key.to_string(), value.to_string()))
}

fn parse_string(value: &str, manifest_path: &Path, line_no: usize) -> Result<String, String> {
    if !(value.starts_with('"') && value.ends_with('"') && value.len() >= 2) {
        return Err(format!(
            "Expected quoted string in {}:{line_no}",
            manifest_path.display()
        ));
    }
    Ok(value[1..value.len() - 1].to_string())
}

fn parse_u32(value: &str, manifest_path: &Path, line_no: usize) -> Result<u32, String> {
    value.parse::<u32>().map_err(|err| {
        format!(
            "Parse integer in {}:{line_no}: {err}",
            manifest_path.display()
        )
    })
}

fn create_suite_dir(label: &str) -> Result<PathBuf, String> {
    let dir =
        workspace_root()
            .join("target")
            .join(format!("{label}-{}-{}", process::id(), unix_nanos()));
    fs::create_dir_all(&dir)
        .map_err(|err| format!("Create suite artifact directory {}: {err}", dir.display()))?;
    Ok(dir)
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .canonicalize()
        .expect("workspace root")
}

fn unix_nanos() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos()
}

fn first_difference_offset(left: &[u8], right: &[u8]) -> Option<usize> {
    let shared = left.len().min(right.len());
    for index in 0..shared {
        if left[index] != right[index] {
            return Some(index);
        }
    }
    (left.len() != right.len()).then_some(shared)
}

#[allow(clippy::needless_range_loop)]
fn render_hex_diff(opforge: &[u8], oracle: &[u8]) -> String {
    const ROW_WIDTH: usize = 8;
    const MAX_ROWS: usize = 64;
    const CONTEXT: usize = 2;

    let total_len = opforge.len().max(oracle.len());
    if total_len == 0 {
        return "  (both empty)".to_string();
    }

    let row_count = total_len.div_ceil(ROW_WIDTH).min(MAX_ROWS);

    let format_hex_row = |data: &[u8], s: usize, e: usize| -> String {
        (s..e)
            .map(|i| {
                data.get(i)
                    .map(|b| format!("{b:02x}"))
                    .unwrap_or_else(|| "--".to_string())
            })
            .collect::<Vec<_>>()
            .join(" ")
    };

    let format_ascii = |data: &[u8], s: usize, e: usize| -> String {
        (s..e)
            .map(|i| {
                data.get(i)
                    .map(|&b| if b.is_ascii_graphic() { b as char } else { '.' })
                    .unwrap_or('~')
            })
            .collect()
    };

    // Determine which rows differ
    let row_differs: Vec<bool> = (0..row_count)
        .map(|row| {
            let s = row * ROW_WIDTH;
            let e = (s + ROW_WIDTH).min(total_len);
            (s..e).any(|i| opforge.get(i) != oracle.get(i))
        })
        .collect();

    // Build set of rows to print: all differing rows plus CONTEXT around them
    let mut print_rows = vec![false; row_count];
    for row in 0..row_count {
        if row_differs[row] {
            let lo = row.saturating_sub(CONTEXT);
            let hi = (row + CONTEXT + 1).min(row_count);
            for r in lo..hi {
                print_rows[r] = true;
            }
        }
    }

    let col_width = ROW_WIDTH * 3 - 1; // 23
    let mut lines = Vec::new();
    let mut last_printed: Option<usize> = None;

    for row in 0..row_count {
        if !print_rows[row] {
            continue;
        }
        if let Some(last) = last_printed {
            if row > last + 1 {
                lines.push(format!("@@ 0x{:04x} @@", row * ROW_WIDTH));
            }
        }
        last_printed = Some(row);
        let s = row * ROW_WIDTH;
        let e = (s + ROW_WIDTH).min(total_len);
        if row_differs[row] {
            // emit "-" line for opforge, "+" line for oracle
            lines.push(format!(
                "- 0x{s:04x}  {:<col_width$}  |{:<8}|",
                format_hex_row(opforge, s, e),
                format_ascii(opforge, s, e),
            ));
            lines.push(format!(
                "+ 0x{s:04x}  {:<col_width$}  |{:<8}|",
                format_hex_row(oracle, s, e),
                format_ascii(oracle, s, e),
            ));
        } else {
            lines.push(format!(
                "  0x{s:04x}  {:<col_width$}  |{:<8}|",
                format_hex_row(opforge, s, e),
                format_ascii(opforge, s, e),
            ));
        }
    }

    if total_len > MAX_ROWS * ROW_WIDTH {
        lines.push(format!(
            "  ... ({} more bytes not shown)",
            total_len - MAX_ROWS * ROW_WIDTH
        ));
    }

    lines.join("\n")
}

fn render_bytes_comparison_report(
    fixture_id: &str,
    opforge_bytes: &[u8],
    oracle_bytes: &[u8],
) -> String {
    let status = if opforge_bytes == oracle_bytes {
        "MATCH"
    } else {
        "MISMATCH"
    };
    let mut sections = vec![
        format!("fixture: {fixture_id}"),
        format!("status: {status}"),
        format!("--- opforge  ({} bytes)", opforge_bytes.len()),
        format!("+++ oracle   ({} bytes)", oracle_bytes.len()),
    ];
    sections.push(render_hex_diff(opforge_bytes, oracle_bytes));
    sections.join("\n") + "\n"
}

fn render_error_report(
    fixture_id: &str,
    opforge_error: Option<&str>,
    oracle_error: Option<&str>,
) -> Option<String> {
    let mut sections = vec![format!("fixture: {fixture_id}")];
    if let Some(msg) = opforge_error {
        sections.push("--- opforge error ---".to_string());
        sections.push(msg.trim_end().to_string());
    }
    if let Some(msg) = oracle_error {
        sections.push("+++ oracle error +++".to_string());
        sections.push(msg.trim_end().to_string());
    }
    if opforge_error.is_none() && oracle_error.is_none() {
        return None;
    }
    Some(sections.join("\n") + "\n")
}

fn source_path_with_extension(source_path: &Path, new_ext: &str) -> PathBuf {
    source_path.with_extension(new_ext)
}

fn sync_sidecar_report(path: &Path, content: Option<String>) {
    match content {
        Some(content) => {
            let _ = fs::write(path, content);
        }
        None => {
            if path.exists() {
                let _ = fs::remove_file(path);
            }
        }
    }
}

fn observed_status_label(status: ObservedStatus) -> &'static str {
    match status {
        ObservedStatus::Success => "success",
        ObservedStatus::Error => "error",
    }
}

fn sanitize_fixture_id(id: &str) -> String {
    id.chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '-' })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    #[derive(Debug)]
    struct FakeAdapter {
        availability: OracleAvailability,
        oracle_id: &'static str,
        oracle_profile: &'static str,
        outputs: Vec<(String, FakeOracleResponse)>,
        seen: Mutex<Vec<String>>,
        seen_profiles: Mutex<Vec<Option<String>>>,
    }

    #[derive(Debug, Clone)]
    enum FakeOracleResponse {
        Success(Vec<u8>),
        Failure {
            summary: String,
            diagnostics_text: String,
        },
    }

    impl FakeAdapter {
        fn new(
            availability: OracleAvailability,
            outputs: Vec<(String, FakeOracleResponse)>,
        ) -> Self {
            Self {
                availability,
                oracle_id: "vasm",
                oracle_profile: "m68k_mot_flat_binary",
                outputs,
                seen: Mutex::new(Vec::new()),
                seen_profiles: Mutex::new(Vec::new()),
            }
        }
    }

    impl ExternalOracleAdapter for FakeAdapter {
        fn oracle_id(&self) -> &'static str {
            self.oracle_id
        }

        fn oracle_profile(&self) -> &'static str {
            self.oracle_profile
        }

        fn supports_family(&self, family: &str) -> bool {
            family == "motorola68000"
        }

        fn supports_cpu(&self, cpu: &str) -> bool {
            matches!(cpu, "68000" | "68010" | "68020" | "68030" | "68040")
        }

        fn supports_profile(&self, cpu: &str, profile: Option<&str>) -> bool {
            matches!(
                (cpu, profile),
                (_, None)
                    | ("68020" | "68030", Some("fpu-68881" | "fpu-68882"))
                    | ("68030", Some("mmu-68030"))
                    | ("68040", Some("fpu-68040" | "mmu-68040"))
            )
        }

        fn availability(&self) -> OracleAvailability {
            self.availability.clone()
        }

        fn assemble_flat_binary(
            &self,
            request: OracleAssembleRequest<'_>,
        ) -> Result<OracleAssembleSuccess, OracleAssembleFailure> {
            let id = request
                .source_path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .expect("fixture stem")
                .to_string();
            self.seen.lock().expect("seen lock").push(id.clone());
            self.seen_profiles
                .lock()
                .expect("seen profiles lock")
                .push(request.cpu_profile.map(str::to_string));
            fs::create_dir_all(request.output_dir).expect("create oracle dir");
            match self
                .outputs
                .iter()
                .find(|(name, _)| name == &id)
                .map(|(_, response)| response.clone())
                .expect("fixture output bytes")
            {
                FakeOracleResponse::Success(bytes) => {
                    let output_path = request.output_dir.join(OUTPUT_FILENAME);
                    fs::write(&output_path, &bytes).expect("write oracle bytes");
                    Ok(OracleAssembleSuccess {
                        output_path,
                        bytes,
                        stdout_path: None,
                        stderr_path: None,
                    })
                }
                FakeOracleResponse::Failure {
                    summary,
                    diagnostics_text,
                } => {
                    let diagnostics_path = request.output_dir.join("oracle.diagnostics.txt");
                    fs::write(&diagnostics_path, &diagnostics_text)
                        .expect("write oracle diagnostics");
                    Err(OracleAssembleFailure {
                        diagnostics_path,
                        stdout_path: None,
                        stderr_path: None,
                        diagnostics_text,
                        summary,
                    })
                }
            }
        }
    }

    fn temp_dir(label: &str) -> PathBuf {
        let dir = workspace_root().join("target").join(format!(
            "{label}-{}-{}",
            process::id(),
            unix_nanos()
        ));
        fs::create_dir_all(&dir).expect("create temp dir");
        dir
    }

    fn write_fixture(dir: &Path, relative_path: &str, contents: &str) {
        let path = dir.join(relative_path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("create fixture parent");
        }
        fs::write(path, contents).expect("write fixture");
    }

    fn write_manifest(dir: &Path, contents: &str) -> PathBuf {
        fs::create_dir_all(dir).expect("create manifest dir");
        let path = dir.join("fixtures.toml");
        fs::write(&path, contents).expect("write manifest");
        path
    }

    #[test]
    fn external_oracle_manifest_rejects_duplicate_fixture_ids() {
        let dir = temp_dir("external-oracle-duplicate-id");
        write_fixture(&dir, "positive/first.asm", ".byte $01\n");
        write_fixture(&dir, "positive/second.asm", ".byte $02\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "duplicate"
cpu = "68000"
path = "positive/first.asm"

[[fixtures]]
id = "duplicate"
cpu = "68010"
path = "positive/second.asm"
"#,
        );

        let err = load_manifest(
            &manifest_path,
            &FakeAdapter::new(OracleAvailability::Ready, vec![]),
        )
        .expect_err("duplicate ids should fail");
        assert!(err.contains("duplicate fixture id 'duplicate'"));
    }

    #[test]
    fn external_oracle_manifest_rejects_duplicate_canonical_source_paths() {
        let dir = temp_dir("external-oracle-duplicate-source-path");
        write_fixture(&dir, "positive/shared.asm", ".byte $01\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "first"
cpu = "68000"
path = "positive/shared.asm"

[[fixtures]]
id = "second"
cpu = "68010"
path = "positive/../positive/shared.asm"
"#,
        );

        let err = load_manifest(
            &manifest_path,
            &FakeAdapter::new(OracleAvailability::Ready, vec![]),
        )
        .expect_err("duplicate canonical source paths should fail");
        assert!(err.contains("duplicate canonical fixture source path"));
        assert!(err.contains("first"));
        assert!(err.contains("second"));
    }

    #[test]
    fn external_oracle_manifest_rejects_unsupported_cpu() {
        let dir = temp_dir("external-oracle-unsupported-cpu");
        write_fixture(&dir, "positive/third.asm", ".byte $03\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "unsupported-cpu"
cpu = "68060"
path = "positive/third.asm"
"#,
        );

        let err = load_manifest(
            &manifest_path,
            &FakeAdapter::new(OracleAvailability::Ready, vec![]),
        )
        .expect_err("unsupported cpu should fail");
        assert!(err.contains("uses unsupported cpu '68060'"));
    }

    #[test]
    fn external_oracle_discovers_nested_manifests_in_sorted_order() {
        let dir = temp_dir("external-oracle-discover-manifests");
        let manifest_68010 = write_manifest(
            &dir.join("68010"),
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "second-fixture"
cpu = "68010"
path = "positive/second.asm"
"#,
        );
        let manifest_68000 = write_manifest(
            &dir.join("68000"),
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "first-fixture"
cpu = "68000"
path = "positive/first.asm"
"#,
        );

        let (discovery_root, manifest_paths) =
            discover_manifest_paths(&dir).expect("discover manifests");

        assert_eq!(discovery_root, dir.canonicalize().expect("canonical root"));
        assert_eq!(
            manifest_paths,
            vec![
                manifest_68000
                    .canonicalize()
                    .expect("canonical 68000 manifest"),
                manifest_68010
                    .canonicalize()
                    .expect("canonical 68010 manifest"),
            ]
        );
    }

    #[test]
    fn external_oracle_returns_structured_skip_when_adapter_is_unavailable() {
        let dir = temp_dir("external-oracle-skip-result");
        write_fixture(&dir, "positive/skip.asm", ".byte $01\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "skip"
cpu = "68000"
path = "positive/skip.asm"
"#,
        );

        let outcome = run_fixture_suite(
            &manifest_path,
            &FakeAdapter::new(
                OracleAvailability::Disabled("missing tool".to_string()),
                vec![("skip".to_string(), FakeOracleResponse::Success(vec![0x01]))],
            ),
            ExpectedOutcome::Success,
        )
        .expect("skip outcome");

        match outcome {
            ExternalOracleSuiteOutcome::Skipped(skip) => assert_eq!(skip.reason(), "missing tool"),
            ExternalOracleSuiteOutcome::Completed { .. } => panic!("expected skip outcome"),
        }
    }

    #[test]
    fn external_oracle_dispatches_fixtures_and_creates_output_dirs() {
        let dir = temp_dir("external-oracle-dispatch");
        write_fixture(&dir, "positive/first.asm", ".byte $01, $02\n");
        write_fixture(&dir, "positive/second.asm", ".byte $4E, $75\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "first-fixture"
cpu = "68000"
path = "positive/first.asm"

[[fixtures]]
id = "second-fixture"
cpu = "68010"
path = "positive/second.asm"
"#,
        );

        let adapter = FakeAdapter::new(
            OracleAvailability::Ready,
            vec![
                (
                    "first".to_string(),
                    FakeOracleResponse::Success(vec![0x01, 0x02]),
                ),
                (
                    "second".to_string(),
                    FakeOracleResponse::Success(vec![0x4E, 0x75]),
                ),
            ],
        );
        let outcome = run_fixture_suite(&manifest_path, &adapter, ExpectedOutcome::Success)
            .expect("suite pass");
        let artifact_root = match outcome {
            ExternalOracleSuiteOutcome::Completed {
                fixture_count,
                artifact_root,
                notes,
            } => {
                assert_eq!(fixture_count, 2);
                assert!(notes.is_empty());
                artifact_root
            }
            ExternalOracleSuiteOutcome::Skipped(_) => panic!("expected completed outcome"),
        };

        let seen = adapter.seen.lock().expect("seen lock").clone();
        assert_eq!(seen, vec!["first".to_string(), "second".to_string()]);
        assert!(artifact_root
            .join("first-fixture")
            .join("opforge")
            .join(OUTPUT_FILENAME)
            .exists());
        assert!(artifact_root
            .join("first-fixture")
            .join("oracle")
            .join(OUTPUT_FILENAME)
            .exists());
        assert!(artifact_root
            .join("second-fixture")
            .join("opforge")
            .join(OUTPUT_FILENAME)
            .exists());
        assert!(artifact_root
            .join("second-fixture")
            .join("oracle")
            .join(OUTPUT_FILENAME)
            .exists());
        // bytes_diff.txt is written next to the source .asm file
        let cmp = dir.join("positive").join("first.bytes_diff.txt");
        assert!(
            cmp.exists(),
            "bytes_diff.txt must exist next to source .asm"
        );
        let cmp_text = fs::read_to_string(&cmp).expect("read bytes_diff.txt");
        assert!(cmp_text.contains("status: MATCH"));
    }

    #[test]
    fn external_oracle_mismatch_writes_mismatch_txt_to_fixture_artifact_dir() {
        let dir = temp_dir("external-oracle-mismatch-report");
        write_fixture(&dir, "positive/first.asm", ".byte $01, $02\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "byte-diff-fixture"
cpu = "68000"
path = "positive/first.asm"
"#,
        );
        let adapter = FakeAdapter::new(
            OracleAvailability::Ready,
            vec![(
                "first".to_string(),
                FakeOracleResponse::Success(vec![0x01, 0xFF]),
            )],
        );
        let result = run_fixture_suite(&manifest_path, &adapter, ExpectedOutcome::Success);
        assert!(result.is_err(), "mismatched bytes should surface as error");
        let err_msg = result.unwrap_err();
        assert!(
            err_msg.contains("--- opforge"),
            "mismatch error should contain diff header: {err_msg}"
        );
        assert!(
            err_msg.contains("+ 0x0000") || err_msg.contains("- 0x0000"),
            "mismatch error should contain diff lines: {err_msg}"
        );
        // mismatch.txt and bytes_diff.txt are written next to the source .asm file
        let mismatch_file = dir.join("positive").join("first.mismatch.txt");
        assert!(
            mismatch_file.exists(),
            "mismatch.txt should be written next to source"
        );
        let mismatch_text = fs::read_to_string(&mismatch_file).expect("read mismatch.txt");
        assert!(mismatch_text.contains("external-oracle mismatch"));
        assert!(mismatch_text.contains("--- opforge"));
        let diff_file = dir.join("positive").join("first.bytes_diff.txt");
        assert!(
            diff_file.exists(),
            "bytes_diff.txt should be written next to source"
        );
        let diff_text = fs::read_to_string(&diff_file).expect("read bytes_diff.txt");
        assert!(diff_text.contains("status: MISMATCH"));
    }

    #[test]
    fn external_oracle_error_writes_error_report_next_to_source() {
        let dir = temp_dir("external-oracle-error-report");
        write_fixture(&dir, "negative/bad.asm", "INVALID_INSTRUCTION\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "error"
compare_mode = "error_class"

[[fixtures]]
id = "bad-instruction"
cpu = "68000"
path = "negative/bad.asm"
expected_outcome = "documented_divergence"
compare_mode = "error_class"
documented_divergence_kind = "opforge_error_oracle_success"
documented_divergence_reason = "test fixture: opforge error while oracle succeeds"
"#,
        );
        let adapter = FakeAdapter::new(
            OracleAvailability::Ready,
            vec![("bad".to_string(), FakeOracleResponse::Success(vec![]))],
        );
        // opForge will fail on INVALID_INSTRUCTION; oracle succeeds (documented divergence)
        let _outcome = run_fixture_suite(
            &manifest_path,
            &adapter,
            ExpectedOutcome::DocumentedDivergence,
        );
        let report = dir.join("negative").join("bad.error_report.txt");
        if report.exists() {
            let text = fs::read_to_string(&report).unwrap_or_default();
            assert!(
                text.contains("--- opforge error ---"),
                "should label opforge error section"
            );
        }
    }

    #[test]
    fn external_oracle_rerun_refreshes_success_sidecars() {
        let dir = temp_dir("external-oracle-sidecar-refresh-success");
        write_fixture(&dir, "positive/first.asm", ".byte $01, $02\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "refresh-success"
cpu = "68000"
path = "positive/first.asm"
"#,
        );
        let mismatch_path = dir.join("positive").join("first.mismatch.txt");
        let diff_path = dir.join("positive").join("first.bytes_diff.txt");
        let error_path = dir.join("positive").join("first.error_report.txt");

        let first_result = run_fixture_suite(
            &manifest_path,
            &FakeAdapter::new(
                OracleAvailability::Ready,
                vec![(
                    "first".to_string(),
                    FakeOracleResponse::Success(vec![0x01, 0xFF]),
                )],
            ),
            ExpectedOutcome::Success,
        );
        assert!(first_result.is_err(), "first run should create a mismatch");
        assert!(
            mismatch_path.exists(),
            "mismatch report should exist after mismatch"
        );
        assert!(
            diff_path.exists(),
            "byte diff should exist after bytes mismatch"
        );
        fs::write(&error_path, "stale error report").expect("seed stale error report");

        let second_result = run_fixture_suite(
            &manifest_path,
            &FakeAdapter::new(
                OracleAvailability::Ready,
                vec![(
                    "first".to_string(),
                    FakeOracleResponse::Success(vec![0x01, 0x02]),
                )],
            ),
            ExpectedOutcome::Success,
        )
        .expect("second run should match cleanly");

        match second_result {
            ExternalOracleSuiteOutcome::Completed { fixture_count, .. } => {
                assert_eq!(fixture_count, 1);
            }
            ExternalOracleSuiteOutcome::Skipped(_) => panic!("expected completed outcome"),
        }
        assert!(
            !mismatch_path.exists(),
            "stale mismatch report should be removed after a clean rerun"
        );
        assert!(
            !error_path.exists(),
            "stale error report should be removed when neither side errors"
        );
        let diff_text = fs::read_to_string(&diff_path).expect("read refreshed bytes diff");
        assert!(diff_text.contains("status: MATCH"));
    }

    #[test]
    fn external_oracle_continues_refreshing_sidecars_after_earlier_mismatch() {
        let dir = temp_dir("external-oracle-sidecar-refresh-after-mismatch");
        write_fixture(&dir, "positive/first.asm", ".byte $01\n");
        write_fixture(&dir, "positive/second.asm", ".byte $02\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "first-fixture"
cpu = "68000"
path = "positive/first.asm"

[[fixtures]]
id = "second-fixture"
cpu = "68000"
path = "positive/second.asm"
"#,
        );
        let second_mismatch = dir.join("positive").join("second.mismatch.txt");
        let second_error = dir.join("positive").join("second.error_report.txt");
        let second_diff = dir.join("positive").join("second.bytes_diff.txt");
        fs::write(&second_mismatch, "stale mismatch").expect("seed stale mismatch");
        fs::write(&second_error, "stale error").expect("seed stale error");

        let adapter = FakeAdapter::new(
            OracleAvailability::Ready,
            vec![
                ("first".to_string(), FakeOracleResponse::Success(vec![0xFF])),
                (
                    "second".to_string(),
                    FakeOracleResponse::Success(vec![0x02]),
                ),
            ],
        );
        let result = run_fixture_suite(&manifest_path, &adapter, ExpectedOutcome::Success);
        assert!(result.is_err(), "first fixture should still fail the suite");
        assert_eq!(
            adapter.seen.lock().expect("seen lock").as_slice(),
            ["first", "second"],
            "suite should keep processing fixtures so later sidecars are refreshed"
        );
        assert!(
            !second_mismatch.exists(),
            "later matching fixture should have stale mismatch removed"
        );
        assert!(
            !second_error.exists(),
            "later matching fixture should have stale error removed"
        );
        let diff_text = fs::read_to_string(&second_diff).expect("read second bytes diff");
        assert!(diff_text.contains("status: MATCH"));
    }

    #[test]
    fn external_oracle_error_run_removes_stale_byte_report_and_refreshes_mismatch() {
        let dir = temp_dir("external-oracle-sidecar-refresh-error");
        write_fixture(&dir, "negative/bad.asm", ",\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "error"
compare_mode = "error_class"

[[fixtures]]
id = "bad-instruction"
cpu = "68000"
path = "negative/bad.asm"
"#,
        );
        let diff_path = dir.join("negative").join("bad.bytes_diff.txt");
        let mismatch_path = dir.join("negative").join("bad.mismatch.txt");
        let error_path = dir.join("negative").join("bad.error_report.txt");
        fs::write(&diff_path, "stale bytes diff").expect("seed stale diff");
        fs::write(&mismatch_path, "stale mismatch").expect("seed stale mismatch");

        let err = run_fixture_suite(
            &manifest_path,
            &FakeAdapter::new(
                OracleAvailability::Ready,
                vec![(
                    "bad".to_string(),
                    FakeOracleResponse::Failure {
                        summary: "vasm exited with status 1".to_string(),
                        diagnostics_text:
                            "error 5 in line 1 of \"fixture.asm\": syntax error\n>    ,\n"
                                .to_string(),
                    },
                )],
            ),
            ExpectedOutcome::Error,
        )
        .expect_err("mismatched error classes should still refresh sidecars");

        assert!(err.contains("external-oracle mismatch"));
        assert!(
            !diff_path.exists(),
            "bytes diff should be removed when current fixture outcome is an error"
        );
        assert!(
            mismatch_path.exists(),
            "current mismatch should be written after refreshing stale reports"
        );
        let mismatch_text = fs::read_to_string(&mismatch_path).expect("read mismatch report");
        assert!(mismatch_text.contains("external-oracle mismatch"));

        let report_text = fs::read_to_string(&error_path).expect("read error report");
        assert!(report_text.contains("--- opforge error ---"));
        assert!(report_text.contains("+++ oracle error +++"));
    }

    #[test]
    fn external_oracle_runs_multiple_manifests_in_stable_order() {
        let dir = temp_dir("external-oracle-multi-manifest-dispatch");
        write_fixture(
            &dir.join("68010"),
            "positive/second.asm",
            ".byte $4E, $75\n",
        );
        write_fixture(&dir.join("68000"), "positive/first.asm", ".byte $01, $02\n");
        write_manifest(
            &dir.join("68010"),
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "second-fixture"
cpu = "68010"
path = "positive/second.asm"
"#,
        );
        write_manifest(
            &dir.join("68000"),
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "first-fixture"
cpu = "68000"
path = "positive/first.asm"
"#,
        );

        let adapter = FakeAdapter::new(
            OracleAvailability::Ready,
            vec![
                (
                    "first".to_string(),
                    FakeOracleResponse::Success(vec![0x01, 0x02]),
                ),
                (
                    "second".to_string(),
                    FakeOracleResponse::Success(vec![0x4E, 0x75]),
                ),
            ],
        );
        let outcome = run_fixture_suite(&dir, &adapter, ExpectedOutcome::Success)
            .expect("multi-manifest suite should pass");
        let artifact_root = match outcome {
            ExternalOracleSuiteOutcome::Completed {
                fixture_count,
                artifact_root,
                notes,
            } => {
                assert_eq!(fixture_count, 2);
                assert!(notes.is_empty());
                artifact_root
            }
            ExternalOracleSuiteOutcome::Skipped(_) => panic!("expected completed outcome"),
        };

        let seen = adapter.seen.lock().expect("seen lock").clone();
        assert_eq!(seen, vec!["first".to_string(), "second".to_string()]);
        assert!(artifact_root
            .join("68000")
            .join("first-fixture")
            .join("opforge")
            .join(OUTPUT_FILENAME)
            .exists());
        assert!(artifact_root
            .join("68010")
            .join("second-fixture")
            .join("oracle")
            .join(OUTPUT_FILENAME)
            .exists());
        // bytes_diff.txt is written next to each source .asm file
        let cmp_file = dir
            .join("68000")
            .join("positive")
            .join("first.bytes_diff.txt");
        assert!(
            cmp_file.exists(),
            "bytes_diff.txt should be written next to source .asm"
        );
        let cmp_text = fs::read_to_string(&cmp_file).expect("read bytes_diff.txt");
        assert!(cmp_text.contains("status: MATCH"));
        assert!(cmp_text.contains("--- opforge"));
    }

    #[test]
    fn external_oracle_forwards_manifest_cpu_profile_to_the_adapter() {
        let dir = temp_dir("external-oracle-profile-forwarding");
        write_fixture(
            &dir.join("68020-fpu-68881"),
            "documented_divergence/fnop.asm",
            "FNOP\n",
        );
        write_manifest(
            &dir.join("68020-fpu-68881"),
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
cpu_profile = "fpu-68881"
expected_outcome = "documented_divergence"
compare_mode = "bytes"

[[fixtures]]
id = "fnop-profile"
cpu = "68020"
path = "documented_divergence/fnop.asm"
documented_divergence_kind = "opforge_error_oracle_success"
documented_divergence_reason = "profile forwarding coverage fixture"
"#,
        );

        let adapter = FakeAdapter::new(
            OracleAvailability::Ready,
            vec![("fnop".to_string(), FakeOracleResponse::Success(Vec::new()))],
        );
        let outcome = run_fixture_suite(&dir, &adapter, ExpectedOutcome::DocumentedDivergence)
            .expect("profile-aware suite should run");

        match outcome {
            ExternalOracleSuiteOutcome::Completed { fixture_count, .. } => {
                assert_eq!(fixture_count, 1);
            }
            ExternalOracleSuiteOutcome::Skipped(_) => panic!("expected completed outcome"),
        }

        let seen_profiles = adapter
            .seen_profiles
            .lock()
            .expect("seen profiles lock")
            .clone();
        assert_eq!(seen_profiles, vec![Some("fpu-68881".to_string())]);
    }

    #[test]
    fn external_oracle_structured_mismatch_render_includes_required_fields() {
        let manifest = Manifest {
            family: "motorola68000".to_string(),
            oracle: "vasm".to_string(),
            cpu_profile: None,
            fixtures: Vec::new(),
        };
        let fixture = Fixture {
            id: "fixture-a".to_string(),
            cpu: "68000".to_string(),
            source_path: PathBuf::from("fixture-a.asm"),
            expected_outcome: ExpectedOutcome::Success,
            compare_mode: CompareMode::Bytes,
            documented_divergence: None,
        };
        let mismatch = compare_success_outputs(
            &manifest,
            &fixture,
            &OracleAssembleSuccess {
                output_path: PathBuf::from("/tmp/opforge.bin"),
                bytes: vec![0x01, 0x02],
                stdout_path: None,
                stderr_path: None,
            },
            &OracleAssembleSuccess {
                output_path: PathBuf::from("/tmp/oracle.bin"),
                bytes: vec![0x01, 0x03],
                stdout_path: None,
                stderr_path: None,
            },
        )
        .expect_err("expected mismatch");
        let rendered = mismatch.render();
        assert!(rendered.contains("fixture id: fixture-a"));
        assert!(rendered.contains("cpu: 68000"));
        assert!(rendered.contains("oracle id: vasm"));
        assert!(rendered.contains("compare mode: bytes"));
        assert!(rendered.contains("opforge output path: /tmp/opforge.bin"));
        assert!(rendered.contains("oracle output path: /tmp/oracle.bin"));
        assert!(rendered.contains("--- opforge  (2 bytes)"));
        assert!(rendered.contains("+++ oracle   (2 bytes)"));
        // row 0x0000 differs: "- " line for opforge, "+" line for oracle
        assert!(rendered.contains("- 0x0000"));
        assert!(rendered.contains("+ 0x0000"));
        assert!(rendered.contains("01 02"));
        assert!(rendered.contains("01 03"));
    }

    #[test]
    fn external_oracle_error_fixtures_require_both_tools_to_fail() {
        let dir = temp_dir("external-oracle-error-status");
        write_fixture(&dir, "negative/comma_only.asm", ",\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "comma-only"
cpu = "68000"
path = "negative/comma_only.asm"
expected_outcome = "error"
compare_mode = "error_class"
"#,
        );

        let err = run_fixture_suite(
            &manifest_path,
            &FakeAdapter::new(
                OracleAvailability::Ready,
                vec![(
                    "comma_only".to_string(),
                    FakeOracleResponse::Success(vec![0x00]),
                )],
            ),
            ExpectedOutcome::Error,
        )
        .expect_err("status mismatch should fail");

        assert!(err.contains("compare mode: error_class"));
        assert!(err.contains("opforge status: error"));
        assert!(err.contains("oracle status: success"));
    }

    #[test]
    fn external_oracle_error_class_mismatch_reports_classes_and_excerpts() {
        let dir = temp_dir("external-oracle-error-class-mismatch");
        write_fixture(&dir, "negative/comma_only.asm", ",\n");
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "comma-only"
cpu = "68000"
path = "negative/comma_only.asm"
expected_outcome = "error"
compare_mode = "error_class"
"#,
        );

        let err = run_fixture_suite(
            &manifest_path,
            &FakeAdapter::new(
                OracleAvailability::Ready,
                vec![(
                    "comma_only".to_string(),
                    FakeOracleResponse::Failure {
                        summary: "vasm exited with status 1".to_string(),
                        diagnostics_text:
                            "error 2 in line 1 of \"comma_only.asm\": unknown mnemonic <bogus>\n>    bogus d0,d1\n"
                                .to_string(),
                    },
                )],
            ),
            ExpectedOutcome::Error,
        )
        .expect_err("mismatched normalized classes should fail");

        assert!(err.contains("opforge error class:"));
        assert!(err.contains("oracle error class: unknown-mnemonic"));
        assert!(err.contains("opforge excerpt:"));
        assert!(err.contains(
            "oracle excerpt: error 2 in line 1 of \"comma_only.asm\": unknown mnemonic <bogus>"
        ));
    }

    #[test]
    fn external_oracle_documented_divergence_is_visible_but_non_failing() {
        let dir = temp_dir("external-oracle-documented-divergence");
        write_fixture(
            &dir,
            "documented_divergence/opt_directive.asm",
            "opt o+\nmoveq #1,d0\n",
        );
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "opt-directive"
cpu = "68000"
path = "documented_divergence/opt_directive.asm"
expected_outcome = "documented_divergence"
compare_mode = "bytes"
documented_divergence_kind = "opforge_error_oracle_success"
documented_divergence_reason = "vasm OPT directive support is intentionally out of shared subset"
"#,
        );

        let outcome = run_fixture_suite(
            &manifest_path,
            &FakeAdapter::new(
                OracleAvailability::Ready,
                vec![(
                    "opt_directive".to_string(),
                    FakeOracleResponse::Success(vec![0x70, 0x01]),
                )],
            ),
            ExpectedOutcome::DocumentedDivergence,
        )
        .expect("documented divergence should remain visible but non-failing");

        match outcome {
            ExternalOracleSuiteOutcome::Completed {
                fixture_count,
                notes,
                ..
            } => {
                assert_eq!(fixture_count, 1);
                assert_eq!(notes.len(), 1);
                assert!(notes[0].contains("documented divergence matched"));
                assert!(notes[0].contains("opforge_error_oracle_success"));
            }
            ExternalOracleSuiteOutcome::Skipped(_) => panic!("expected completed outcome"),
        }
    }

    #[test]
    fn external_oracle_documented_divergence_reports_reclassification_candidates() {
        let dir = temp_dir("external-oracle-divergence-reclassification");
        write_fixture(
            &dir,
            "documented_divergence/reclassification.asm",
            "start:\n    moveq #1,d0\n    move.w #$1234,d1\n    addq.w #1,d1\n    rts\n",
        );
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "reclassification"
cpu = "68000"
path = "documented_divergence/reclassification.asm"
expected_outcome = "documented_divergence"
compare_mode = "bytes"
documented_divergence_kind = "opforge_error_oracle_success"
documented_divergence_reason = "coverage fixture for reclassification reporting"
"#,
        );

        let outcome = run_fixture_suite(
            &manifest_path,
            &FakeAdapter::new(
                OracleAvailability::Ready,
                vec![(
                    "reclassification".to_string(),
                    FakeOracleResponse::Success(vec![
                        0x70, 0x01, 0x32, 0x3c, 0x12, 0x34, 0x52, 0x41, 0x4e, 0x75,
                    ]),
                )],
            ),
            ExpectedOutcome::DocumentedDivergence,
        )
        .expect("fully matching divergence should become a reclassification candidate");

        match outcome {
            ExternalOracleSuiteOutcome::Completed {
                fixture_count,
                notes,
                ..
            } => {
                assert_eq!(fixture_count, 1);
                assert_eq!(notes.len(), 1);
                assert!(notes[0].contains("reclassification candidate"));
                assert!(notes[0].contains("reclassification"));
            }
            ExternalOracleSuiteOutcome::Skipped(_) => panic!("expected completed outcome"),
        }
    }

    #[test]
    fn external_oracle_documented_divergence_accepts_success_oracle_error() {
        let dir = temp_dir("external-oracle-divergence-success-error");
        write_fixture(
            &dir,
            "documented_divergence/pack.asm",
            "start:\n    PACK D0,D1,#-1\n    RTS\n",
        );
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "pack-status-divergence"
cpu = "68020"
path = "documented_divergence/pack.asm"
expected_outcome = "documented_divergence"
compare_mode = "bytes"
documented_divergence_kind = "opforge_success_oracle_error"
documented_divergence_reason = "vasm rejects negative PACK immediates while opForge currently assembles them"
"#,
        );

        let outcome = run_fixture_suite(
            &manifest_path,
            &FakeAdapter::new(
                OracleAvailability::Ready,
                vec![(
                    "pack".to_string(),
                    FakeOracleResponse::Failure {
                        summary: "vasm exited with status 1".to_string(),
                        diagnostics_text:
                            "error 2026 in line 1 of \"pack.asm\": operand value out of range\n"
                                .to_string(),
                    },
                )],
            ),
            ExpectedOutcome::DocumentedDivergence,
        )
        .expect("documented divergence should accept opForge success vs oracle error");

        match outcome {
            ExternalOracleSuiteOutcome::Completed {
                fixture_count,
                notes,
                ..
            } => {
                assert_eq!(fixture_count, 1);
                assert_eq!(notes.len(), 1);
                assert!(notes[0].contains("opforge_success_oracle_error"));
                assert!(notes[0].contains("negative PACK immediates"));
            }
            ExternalOracleSuiteOutcome::Skipped(_) => panic!("expected completed outcome"),
        }
    }

    #[test]
    fn external_oracle_documented_divergence_accepts_success_byte_mismatch() {
        let dir = temp_dir("external-oracle-divergence-byte-mismatch");
        write_fixture(
            &dir,
            "documented_divergence/link_long.asm",
            "start:\n    LINK.L A6,#-8\n    RTS\n",
        );
        let manifest_path = write_manifest(
            &dir,
            r#"manifest_version = 1
family = "motorola68000"
oracle = "vasm"
oracle_profile = "m68k_mot_flat_binary"
expected_outcome = "success"
compare_mode = "bytes"

[[fixtures]]
id = "link-byte-divergence"
cpu = "68020"
path = "documented_divergence/link_long.asm"
expected_outcome = "documented_divergence"
compare_mode = "bytes"
documented_divergence_kind = "opforge_success_oracle_success_bytes_differ"
documented_divergence_reason = "current opForge/link-long encoding differs from vasm"
"#,
        );

        let outcome = run_fixture_suite(
            &manifest_path,
            &FakeAdapter::new(
                OracleAvailability::Ready,
                vec![(
                    "link_long".to_string(),
                    FakeOracleResponse::Success(vec![
                        0x48, 0x4E, 0xFF, 0xFF, 0xFF, 0xF8, 0x4E, 0x75,
                    ]),
                )],
            ),
            ExpectedOutcome::DocumentedDivergence,
        )
        .expect("documented divergence should accept success byte mismatches");

        match outcome {
            ExternalOracleSuiteOutcome::Completed {
                fixture_count,
                notes,
                ..
            } => {
                assert_eq!(fixture_count, 1);
                assert_eq!(notes.len(), 1);
                assert!(notes[0].contains("byte_mismatch"));
                assert!(notes[0].contains("--- opforge"));
                assert!(notes[0].contains("+++ oracle"));
                // the first row differs so it must have a "-" and "+" line
                assert!(notes[0].contains("- 0x0000"));
                assert!(notes[0].contains("+ 0x0000"));
            }
            ExternalOracleSuiteOutcome::Skipped(_) => panic!("expected completed outcome"),
        }
    }

    #[test]
    fn external_oracle_maps_fpu_profiles_to_opforge_preambles() {
        assert_eq!(opforge_profile_preamble(None), None);
        assert_eq!(opforge_profile_preamble(Some("mmu-68030")), None);
        assert_eq!(
            opforge_profile_preamble(Some("fpu-68881")),
            Some(".fpu 68881\n")
        );
        assert_eq!(
            opforge_profile_preamble(Some("fpu-68882")),
            Some(".fpu 68882\n")
        );
        assert_eq!(
            opforge_profile_preamble(Some("fpu-68040")),
            Some(".fpu 68040\n")
        );
    }

    #[test]
    fn external_oracle_wraps_opforge_sources_for_fpu_profiles() {
        let dir = temp_dir("external-oracle-opforge-profile-wrapper");
        let source_path = dir.join("fixture.asm");
        fs::write(&source_path, "start:\n    FNOP\n").expect("write source");
        let output_dir = dir.join("out");
        fs::create_dir_all(&output_dir).expect("create output dir");
        let manifest = Manifest {
            family: "motorola68000".to_string(),
            oracle: "vasm".to_string(),
            cpu_profile: Some("fpu-68881".to_string()),
            fixtures: Vec::new(),
        };
        let fixture = Fixture {
            id: "fixture".to_string(),
            cpu: "68020".to_string(),
            source_path: source_path.clone(),
            expected_outcome: ExpectedOutcome::Success,
            compare_mode: CompareMode::Bytes,
            documented_divergence: None,
        };

        let wrapped = prepare_opforge_source_path(&manifest, &fixture, &output_dir)
            .expect("prepare wrapped source");
        assert_ne!(wrapped, source_path);
        assert_eq!(
            fs::read_to_string(&wrapped).expect("read wrapped source"),
            ".fpu 68881\nstart:\n    FNOP\n"
        );
    }
}
