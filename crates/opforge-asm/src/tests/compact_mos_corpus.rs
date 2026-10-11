// SPDX-License-Identifier: GPL-3.0-or-later
//! Full, unmodified family/opcore corpus audits; stored references are never refreshed.
use super::*;
use crate::fs_uae_smoke::{
    compact_cli_input::assemble_cli, opforge_self_host_package_digest,
    run_prebuilt_compact_cli_breadth_from_env, run_prebuilt_compact_cli_case_from_env,
    BreadthCaseResult, FsUaeSmokeOutcome, FsUaeSmokeRun, OpforgeNativeCliGuestFile,
    OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase, OpforgeNativeCliProof,
};
use crate::native_package_build::{build_native_packages, EmbedSelection};
use crate::native_reference_parity::NATIVE_OPCORE_DIAGNOSTIC_SHARED_SUPPORT;
use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::time::Instant;

struct Case {
    name: String,
    source: PathBuf,
    reference: PathBuf,
    support: Vec<PathBuf>,
}

fn family_corpus(root: &Path, family: &str) -> Vec<Case> {
    collect_example_asm_files(&root.join("examples").join(family))
        .into_iter()
        .map(|source| Case {
            name: source
                .strip_prefix(root)
                .unwrap()
                .to_string_lossy()
                .into_owned(),
            reference: root
                .join("examples/reference")
                .join(family)
                .join(
                    source
                        .strip_prefix(root.join("examples").join(family))
                        .unwrap(),
                )
                .with_extension(""),
            source,
            support: Vec::new(),
        })
        .collect()
}

// The family instruction audit covers every top-level fixture. Nested AmigaOS
// programs and support use Hunk output/implicit CPU setup and need a separate audit.
fn motorola68000_instruction_corpus(root: &Path) -> Vec<Case> {
    family_corpus(root, "motorola68000")
        .into_iter()
        .filter(|case| case.source.parent() == Some(root.join("examples/motorola68000").as_path()))
        .collect()
}

fn corpus(root: &Path) -> Vec<Case> {
    let mut cases = family_corpus(root, "mos6502");
    cases.extend(opcore_corpus(root));
    cases.sort_by(|a, b| a.name.cmp(&b.name));
    cases
}

fn opcore_corpus(root: &Path) -> Vec<Case> {
    let mut cases = Vec::new();
    for assignment in NATIVE_OPCORE_ASSIGNMENTS {
        let NativeOpcoreRole::Root { reference_stem } = assignment.role else {
            continue;
        };
        let mut support = NATIVE_OPCORE_ASSIGNMENTS
            .iter()
            .filter_map(|other| match other.role {
                NativeOpcoreRole::Support { owner } if owner == assignment.source_path => {
                    Some(root.join(other.source_path))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        support.extend(
            NATIVE_OPCORE_DIAGNOSTIC_SHARED_SUPPORT
                .iter()
                .filter(|(owner, _)| *owner == assignment.source_path)
                .map(|(_, path)| root.join(path)),
        );
        support.sort();
        support.dedup();
        cases.push(Case {
            name: assignment.source_path.into(),
            source: root.join(assignment.source_path),
            reference: root.join("examples/reference/opcore").join(reference_stem),
            support,
        });
    }
    cases.sort_by(|a, b| a.name.cmp(&b.name));
    cases
}

struct ScratchCleanup(PathBuf);
impl Drop for ScratchCleanup {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
struct MetadataCleanup(Vec<PathBuf>);
impl Drop for MetadataCleanup {
    fn drop(&mut self) {
        for path in &self.0 {
            let _ = fs::remove_file(path);
        }
    }
}

fn scratch() -> PathBuf {
    let path = std::env::temp_dir().join(format!(
        "opforge-mos-corpus-{}-{}",
        process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&path).unwrap();
    path
}

fn equal_artifact(path: &Path, actual: &[u8], expected: &[u8]) -> bool {
    if path.extension().is_some_and(|ext| ext == "lst") {
        normalize_listing_for_reference_compare(&String::from_utf8_lossy(actual))
            == normalize_listing_for_reference_compare(&String::from_utf8_lossy(expected))
    } else {
        actual == expected
    }
}

fn reference_check(case: &Case, out: &Path) -> Result<(), String> {
    let metadata = ["opforge_6502_native_cli_smoke.lst", "meta-hex.hex"]
        .map(|name| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(name));
    if let Some(path) = metadata.iter().find(|path| path.exists()) {
        return Err(format!(
            "refusing to overwrite existing metadata artifact {}",
            path.display()
        ));
    }
    let _metadata_cleanup = MetadataCleanup(metadata.into());
    fs::create_dir_all(out).map_err(|e| e.to_string())?;
    let base = case.reference.file_name().unwrap().to_str().unwrap();
    if let Some(expected) = read_example_error_reference(&case.reference.with_extension("err"))
        .or_else(|| expected_example_error(base).map(str::to_owned))
    {
        return match assemble_example_error_in_mode(&case.source, ExecutionMode::Rust) {
            Some(actual) if actual == expected => Ok(()),
            other => Err(format!(
                "diagnostic reference mismatch: expected {expected:?}, actual {other:?}"
            )),
        };
    }
    let allow_error_outputs = has_error_example_suffix(base);
    let outputs = assemble_example_with_base_and_defines_in_mode(
        &case.source,
        out,
        base,
        allow_error_outputs,
        &[],
        ExecutionMode::Rust,
    )?;
    let ext = example_reference_payload_extension(&case.source);
    if allow_error_outputs
        && (!example_output_payload_path(out, base, ext).exists()
            || !out.join(format!("{base}.lst")).exists())
    {
        // The canonical suite permits missing partial artifacts for these
        // fixtures. An audit cannot call absent evidence reference equality.
        return Err("error-output fixture: no comparable partial artifacts (canonical suite permits omission)".into());
    }
    let mut comparisons = vec![
        (
            example_output_payload_path(out, base, ext),
            case.reference.with_extension(ext),
        ),
        (
            out.join(format!("{base}.lst")),
            case.reference.with_extension("lst"),
        ),
    ];
    for (name, _) in outputs {
        comparisons.push((out.join(&name), case.reference.parent().unwrap().join(name)));
    }
    let mut gaps = Vec::new();
    for (actual, expected) in comparisons {
        match (fs::read(&actual), fs::read(&expected)) {
            (Ok(a), Ok(b)) if equal_artifact(&expected, &a, &b) => {}
            other => gaps.push(format!(
                "{}: {}",
                expected.display(),
                match other {
                    (Err(e), _) | (_, Err(e)) => e.to_string(),
                    _ => "bytes differ".into(),
                }
            )),
        }
    }
    if gaps.is_empty() {
        Ok(())
    } else {
        Err(gaps.join("; "))
    }
}

fn save_report(
    audit: &CorpusAudit,
    kind: &str,
    selection: &Option<String>,
    total: usize,
    rows: &[Value],
) {
    let path = std::env::var_os(audit.report_env)
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            std::env::temp_dir().join(format!(
                "opforge-{}-corpus-{kind}-{}.json",
                audit.slug,
                process::id()
            ))
        });
    assert!(path.is_absolute(), "{} must be absolute", audit.report_env);
    fs::write(
        &path,
        serde_json::to_vec_pretty(&json!({
            "audit": kind, "selection": selection, "corpus_count": total,
            "attempted": rows.len(), "cases": rows,
            "cli_limitations": [
                {"limitation": "listing output unsupported", "evidence": "compact CLI argument decoder K_UNSUPPORTED for --list; source inspection, not a per-case execution claim"},
                {"limitation": "mixed CLI output kinds unsupported", "evidence": "compact CLI accepts one output kind; source inspection, not a per-case execution claim"}
            ],
        }))
        .unwrap(),
    )
    .unwrap();
    eprintln!(
        "{} audit: {} cases; report {}",
        audit.label,
        rows.len(),
        path.display()
    );
}

#[test]
#[ignore = "full Rust reference audit; existing corpus/reference regressions need classification"]
fn compact_mos_corpus_rust_references() {
    let root = workspace_root();
    run_rust_reference_audit(&MOS_AUDIT, corpus(&root));
}

#[test]
#[ignore = "full opcore Rust reference audit; existing corpus/reference regressions need classification"]
fn compact_opcore_corpus_rust_references() {
    let root = workspace_root();
    run_rust_reference_audit(&OPCORE_AUDIT, opcore_corpus(&root));
}

#[test]
#[ignore = "full Rust reference audit; existing corpus/reference regressions need classification"]
fn compact_motorola68000_corpus_rust_references() {
    let root = workspace_root();
    run_rust_reference_audit(&M68K_AUDIT, motorola68000_instruction_corpus(&root));
}

fn run_rust_reference_audit(audit: &CorpusAudit, cases: Vec<Case>) {
    let scratch = scratch();
    let _cleanup = ScratchCleanup(scratch.clone());
    let rows = cases
        .iter()
        .enumerate()
        .map(|(i, case)| {
            let result = reference_check(case, &scratch.join(i.to_string()));
            json!({"case": case.name, "rust_reference_ok": result.is_ok(), "error": result.err()})
        })
        .collect::<Vec<_>>();
    save_report(audit, "rust-references", &None, cases.len(), &rows);
    assert!(
        rows.iter().all(|r| r["rust_reference_ok"] == true),
        "Rust reference failures; inspect audit report"
    );
}

fn tree_files(dir: &Path) -> BTreeMap<PathBuf, Vec<u8>> {
    fn visit(base: &Path, dir: &Path, result: &mut BTreeMap<PathBuf, Vec<u8>>) {
        for entry in fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                visit(base, &path, result);
            } else {
                result.insert(
                    path.strip_prefix(base).unwrap().to_path_buf(),
                    fs::read(path).unwrap(),
                );
            }
        }
    }
    let mut files = BTreeMap::new();
    visit(dir, dir, &mut files);
    files
}

fn initial_cpu(source: &[u8]) -> String {
    String::from_utf8_lossy(source)
        .lines()
        .find_map(|line| {
            let mut words = line.split(';').next().unwrap_or("").split_whitespace();
            if words
                .next()
                .is_some_and(|word| word.eq_ignore_ascii_case(".cpu"))
            {
                words.next().map(|cpu| cpu.trim_matches('"').to_string())
            } else {
                None
            }
        })
        .unwrap_or_else(|| default_cpu().as_str().to_owned())
}

fn stage(case: &Case, dir: &Path) -> Result<(String, BTreeMap<PathBuf, Vec<u8>>), String> {
    fs::create_dir_all(dir).map_err(|e| e.to_string())?;
    if case.name.starts_with("examples/mos6502/")
        || case.name.starts_with("examples/motorola68000/")
    {
        let entry = case
            .source
            .file_name()
            .unwrap()
            .to_string_lossy()
            .into_owned();
        fs::copy(&case.source, dir.join(&entry)).map_err(|e| e.to_string())?;
        return Ok((entry, tree_files(dir)));
    }
    let base = workspace_root().join("examples/opcore");
    for source in std::iter::once(&case.source).chain(case.support.iter()) {
        let path = dir.join(source.strip_prefix(&base).map_err(|e| e.to_string())?);
        fs::create_dir_all(path.parent().unwrap()).map_err(|e| e.to_string())?;
        fs::copy(source, path).map_err(|e| e.to_string())?;
    }
    Ok((
        case.source
            .strip_prefix(base)
            .unwrap()
            .to_string_lossy()
            .into_owned(),
        tree_files(dir),
    ))
}

struct CorpusAudit {
    slug: &'static str,
    label: &'static str,
    cases_env: &'static str,
    report_env: &'static str,
}

const MOS_AUDIT: CorpusAudit = CorpusAudit {
    slug: "mos",
    label: "MOS/opcore",
    cases_env: "OPFORGE_MOS_CORPUS_CASES",
    report_env: "OPFORGE_MOS_CORPUS_REPORT",
};
const OPCORE_AUDIT: CorpusAudit = CorpusAudit {
    slug: "opcore",
    label: "opcore",
    cases_env: "OPFORGE_OPCORE_CORPUS_CASES",
    report_env: "OPFORGE_OPCORE_CORPUS_REPORT",
};
const M68K_AUDIT: CorpusAudit = CorpusAudit {
    slug: "m68k",
    label: "motorola68000",
    cases_env: "OPFORGE_M68K_CORPUS_CASES",
    report_env: "OPFORGE_M68K_CORPUS_REPORT",
};

#[test]
#[ignore = "full corpus fresh compact-native audit; requires configured FS-UAE"]
fn compact_mos_corpus_fs_uae() {
    let root = workspace_root();
    run_compact_corpus(&MOS_AUDIT, corpus(&root));
}

#[test]
#[ignore = "full opcore fresh compact-native audit; requires configured FS-UAE"]
fn compact_opcore_corpus_fs_uae() {
    let root = workspace_root();
    run_compact_corpus(&OPCORE_AUDIT, opcore_corpus(&root));
}

/// Audit all top-level instruction fixtures, including expected-error cases.
/// Nested AmigaOS programs/support require a separate Hunk/CPU setup audit.
#[test]
#[ignore = "top-level Motorola instruction corpus fresh compact-native audit; requires configured FS-UAE"]
fn compact_motorola68000_corpus_fs_uae() {
    let root = workspace_root();
    run_compact_corpus(&M68K_AUDIT, motorola68000_instruction_corpus(&root));
}

// Unset keeps the existing one-case timing/diagnostic runner. Any explicit
// value opts into correctness-only batching, including a batch size of one.
fn parse_corpus_batch_size(value: Option<&str>) -> Result<Option<usize>, String> {
    value
        .map(|text| {
            let size = text
                .parse::<usize>()
                .map_err(|_| "OPFORGE_CORPUS_BATCH_SIZE must be an integer in 1..=64".to_owned())?;
            if !(1..=64).contains(&size) {
                return Err("OPFORGE_CORPUS_BATCH_SIZE must be in 1..=64".into());
            }
            Ok(size)
        })
        .transpose()
}

struct PreparedCorpusCase {
    row_index: usize,
    name: String,
    bytes: Vec<u8>,
    guest: Vec<(String, Vec<u8>)>,
    command: String,
    outputs: BTreeMap<PathBuf, Vec<u8>>,
    negative: bool,
    oracle_ok: bool,
    oracle_matches_reference: bool,
}

impl PreparedCorpusCase {
    fn native_case<'a>(
        &'a self,
        files: &'a [OpforgeNativeCliGuestFile<'a>],
    ) -> OpforgeNativeCliParityCase<'a> {
        OpforgeNativeCliParityCase {
            name: &self.name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&self.bytes),
            command_template: Some(&self.command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: files,
            proof: if self.negative || !self.oracle_ok {
                OpforgeNativeCliProof::ExpectedFailureWithDiagnostic
            } else if let Some(hex) = self.outputs.get(Path::new("audit.hex")) {
                OpforgeNativeCliProof::ExactArtifact {
                    relative_path: "Work/audit.hex",
                    rust_oracle: hex,
                }
            } else {
                // Observation only; record_native_run cannot qualify absent hex.
                OpforgeNativeCliProof::SuccessfulExitContaining("")
            },
        }
    }
}

fn corpus_guest_files<'a>(
    prepared: &'a PreparedCorpusCase,
    packages: &'a [(String, &[u8])],
) -> Vec<OpforgeNativeCliGuestFile<'a>> {
    prepared
        .guest
        .iter()
        .map(|(path, bytes)| OpforgeNativeCliGuestFile {
            relative_path: path,
            bytes,
        })
        .chain(
            packages
                .iter()
                .map(|(path, bytes)| OpforgeNativeCliGuestFile {
                    relative_path: path,
                    bytes,
                }),
        )
        .collect()
}

fn record_native_run(
    row: &mut Value,
    prepared: &PreparedCorpusCase,
    run: &FsUaeSmokeRun,
) -> Result<(), String> {
    let negative = prepared.negative;
    let outputs = &prepared.outputs;
    let hex = outputs.get(Path::new("audit.hex"));
    row["native_stdout"] = json!(run.stdout);
    row["native_stderr"] = json!(run.stderr);
    row["exit_code"] = json!(run.exit_code);
    row["protocol_completed"] = json!(run.protocol_completed);
    row["native_seconds"] = json!(run.start_to_done_host_seconds);
    row["native_exact_hex_ok"] =
        json!(!negative && hex.is_some() && prepared.oracle_ok && run.success);
    row["native_negative_rejection_ok"] =
        json!(negative && run.protocol_completed && run.exit_code.is_some_and(|exit| exit != 0));
    row["diagnostic_parity"] = json!(if negative {
        "unqualified: rejection with diagnostic only"
    } else {
        "not applicable"
    });
    let gaps = if negative {
        Vec::new()
    } else {
        outputs
            .iter()
            .filter_map(|(path, expected)| {
                match run.captured_artifacts.get(&Path::new("Work").join(path)) {
                    Some(actual) if equal_artifact(path, actual, expected) => None,
                    Some(_) => Some(format!("{}: bytes differ", path.display())),
                    None => Some(format!("{}: missing", path.display())),
                }
            })
            .collect::<Vec<_>>()
    };
    row["output_gaps"] = json!(gaps);
    if !prepared.oracle_matches_reference
        || (!negative && hex.is_none())
        || (!negative && !run.success)
        || !run.protocol_completed
        || (!negative && run.exit_code != Some(0))
        || (negative && run.exit_code.is_none_or(|exit| exit == 0))
        || !gaps.is_empty()
    {
        Err("native completion/artifact parity failed".into())
    } else {
        Ok(())
    }
}

fn apply_batch_result(
    rows: &mut [Value],
    prepared: &PreparedCorpusCase,
    result: BreadthCaseResult,
) {
    let row = &mut rows[prepared.row_index];
    let evaluated = match result.run {
        Some(run) => record_native_run(row, prepared, &run),
        None => Err("native case did not complete".into()),
    };
    let error = result.error.or_else(|| evaluated.err());
    row["native_ok"] = json!(error.is_none());
    row["error"] = json!(error);
    row["native_seconds"] = Value::Null;
    row["elapsed_seconds"] = Value::Null;
}

fn run_compact_corpus(audit: &CorpusAudit, cases: Vec<Case>) {
    let batch_size_value = std::env::var("OPFORGE_CORPUS_BATCH_SIZE").ok();
    let batch_size =
        parse_corpus_batch_size(batch_size_value.as_deref()).expect("valid corpus execution mode");
    let root = workspace_root();
    let scratch = scratch();
    let _cleanup = ScratchCleanup(scratch.clone());
    let registry = engine::build_default_asm_registry();
    let build = build_native_packages(
        &registry,
        &scratch.join("native"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into()]),
    )
    .unwrap();
    // A breadth audit may freeze the executable while packages and each Rust
    // oracle remain freshly generated from the current registry and source.
    let image_path = std::env::var_os("OPFORGE_CORPUS_IMAGE").map(PathBuf::from);
    let image = match &image_path {
        Some(path) => fs::read(path)
            .unwrap_or_else(|err| panic!("read corpus executable {}: {err}", path.display())),
        None => assemble_cli(&root, &build),
    };
    let packages = tree_files(&build.output_dir.join("packages"));
    let package_guest: Vec<_> = packages
        .iter()
        .map(|(path, bytes)| (format!("packages/{}", path.display()), bytes.as_slice()))
        .collect();
    let selection = std::env::var(audit.cases_env).ok();
    let mut rows = Vec::new();
    let mut pending = Vec::new();
    for (index, case) in cases.iter().enumerate() {
        if selection
            .as_ref()
            .is_some_and(|names| !names.split(',').any(|item| case.name.contains(item.trim())))
        {
            continue;
        }
        let started = Instant::now();
        let mut row = json!({"case": case.name, "case_index": index, "rust_reference_ok": false,
            "native_ok": false, "image_digest": opforge_self_host_package_digest(&image),
            "execution_mode": if batch_size.is_some() { "batch-correctness-only" } else { "sequential" },
            "batch_size": batch_size, "native_seconds": null, "elapsed_seconds": null});
        row["image_path"] = json!(image_path.as_ref().map(|path| path.to_string_lossy()));
        let prepared = (|| -> Result<PreparedCorpusCase, String> {
            let bytes = fs::read(&case.source)
                .map_err(|error| format!("read source {}: {error}", case.source.display()))?;
            let cpu = initial_cpu(&bytes);
            let reference = reference_check(case, &scratch.join(format!("reference-{index}")));
            row["cpu"] = json!(cpu);
            row["source_digest"] = json!(opforge_self_host_package_digest(&bytes));
            row["source_bytes"] = json!(bytes.len());
            row["rust_reference_ok"] = json!(reference.is_ok());
            row["rust_reference_error"] = json!(reference.err());
            let dir = scratch.join(format!("case-{index}"));
            let (entry, inputs) = stage(case, &dir)?;
            let mut argv = vec![
                "opForge".to_string(),
                dir.join(&entry).to_string_lossy().into_owned(),
            ];
            row["original_filename"] = json!(case.source.file_name().unwrap().to_string_lossy());
            row["staged_entry"] = json!(entry);
            row["filename_adjustment"] =
                json!(entry != case.source.file_name().unwrap().to_string_lossy());
            argv.extend([
                "--hex".into(),
                dir.join("audit.hex").to_string_lossy().into_owned(),
            ]);
            for flag in ["-I", "-M"] {
                argv.extend([flag.into(), dir.to_string_lossy().into_owned()]);
            }
            row["rust_command"] = json!(argv);
            row["input_digests"] = json!(inputs
                .iter()
                .map(|(p, b)| (
                    p.to_string_lossy().into_owned(),
                    opforge_self_host_package_digest(b)
                ))
                .collect::<BTreeMap<_, _>>());
            let cli = Cli::try_parse_from(argv).map_err(|e| e.to_string())?;
            let mut config = validate_cli(&cli).map_err(|e| e.to_string())?;
            config.out_dir = Some(dir.clone());
            let oracle = run_with_validated_cli_with_context(&cli, &config);
            let base = case.reference.file_name().unwrap().to_str().unwrap();
            let error_contract = case.reference.with_extension("err").exists()
                || expected_example_error(base).is_some();
            let partial_error_fixture = has_error_example_suffix(base) && !error_contract;
            let negative = error_contract || (partial_error_fixture && oracle.is_err());
            row["expected_negative"] = json!(negative);
            row["partial_error_fixture"] = json!(partial_error_fixture);
            row["negative_expectation"] = json!(if error_contract {
                "stored or canonical diagnostic contract"
            } else if negative {
                "error-output fixture rejected by the live Rust CLI"
            } else {
                "not applicable"
            });
            row["rust_cli_diagnostic"] = json!(oracle.as_ref().err().map(|e| format!("{e:?}")));
            let oracle_matches_reference = oracle.is_err() == negative;
            row["rust_cli_matches_reference_status"] = json!(oracle_matches_reference);
            let outputs = tree_files(&dir)
                .into_iter()
                .filter(|(path, _)| !inputs.contains_key(path))
                .collect::<BTreeMap<_, _>>();
            let guest = inputs
                .iter()
                .map(|(p, b)| (p.to_string_lossy().into_owned(), b.clone()))
                .collect::<Vec<_>>();
            let command = format!(
                "--cpu {cpu} -P Work:packages -I Work: -M Work: Work:{entry} --hex Work:audit.hex"
            );
            row["native_command"] = json!(command);
            row["package_digests"] = json!(packages
                .iter()
                .map(|(p, b)| (
                    p.to_string_lossy().into_owned(),
                    opforge_self_host_package_digest(b)
                ))
                .collect::<BTreeMap<_, _>>());

            Ok(PreparedCorpusCase {
                row_index: rows.len(),
                name: case.name.clone(),
                bytes,
                guest,
                command,
                outputs,
                negative,
                oracle_ok: oracle.is_ok(),
                oracle_matches_reference,
            })
        })();
        match prepared {
            Err(error) => {
                row["error"] = json!(error);
                row["preparation_failed"] = json!(true);
            }
            Ok(prepared) if batch_size.is_some() => {
                row["native_pending"] = json!(true);
                pending.push(prepared);
            }
            Ok(prepared) => {
                let files = corpus_guest_files(&prepared, &package_guest);
                let native = prepared.native_case(&files);
                let outcome = match run_prebuilt_compact_cli_case_from_env(&root, &native, &image) {
                    Err(error) => Err(error),
                    Ok(FsUaeSmokeOutcome::Skipped(reason)) => Err(format!("skipped: {reason}")),
                    Ok(FsUaeSmokeOutcome::Completed { runs }) if runs.len() == 1 => {
                        record_native_run(&mut row, &prepared, &runs[0])
                    }
                    Ok(FsUaeSmokeOutcome::Completed { runs }) => {
                        Err(format!("expected one run, got {}", runs.len()))
                    }
                };
                row["native_ok"] = json!(outcome.is_ok());
                row["error"] = json!(outcome.err());
            }
        }
        if batch_size.is_none() {
            row["elapsed_seconds"] = json!(started.elapsed().as_secs_f64());
        }
        rows.push(row);
        save_report(audit, "compact-native", &selection, cases.len(), &rows);
    }
    if let Some(batch_size) = batch_size {
        if !pending.is_empty() {
            let guest_files: Vec<Vec<_>> = pending
                .iter()
                .map(|prepared| corpus_guest_files(prepared, &package_guest))
                .collect();
            let native_cases: Vec<_> = pending
                .iter()
                .zip(&guest_files)
                .map(|(prepared, files)| prepared.native_case(files))
                .collect();
            match run_prebuilt_compact_cli_breadth_from_env(
                &root,
                &native_cases,
                &image,
                batch_size,
            ) {
                Ok(results) => {
                    assert_eq!(
                        results.len(),
                        pending.len(),
                        "breadth API must return every requested case"
                    );
                    for (prepared, result) in pending.iter().zip(results) {
                        apply_batch_result(&mut rows, prepared, result);
                    }
                }
                Err(error) => {
                    for prepared in &pending {
                        rows[prepared.row_index]["error"] = json!(error);
                    }
                }
            }
            for prepared in &pending {
                rows[prepared.row_index]["native_pending"] = json!(false);
            }
        }
        save_report(audit, "compact-native", &selection, cases.len(), &rows);
    }
    for row in &rows {
        eprintln!("{}: {}", row["case"], row["native_ok"]);
    }
    assert!(!rows.is_empty(), "no corpus cases selected");
    assert!(
        rows.iter()
            .all(|r| r["rust_reference_ok"] == true && r["native_ok"] == true),
        "{} audit found gaps; inspect complete JSON report",
        audit.label
    );
}

#[test]
fn compact_family_corpus_staging_preserves_source_bytes() {
    let root = workspace_root();
    let scratch = scratch();
    let _cleanup = ScratchCleanup(scratch.clone());
    for family in ["mos6502", "motorola68000"] {
        let all_cases = family_corpus(&root, family);
        for case in &all_cases {
            assert!(
                case.reference.with_extension("lst").is_file()
                    || case.reference.with_extension("err").is_file(),
                "missing reference contract for {} at {}",
                case.source.display(),
                case.reference.display()
            );
        }
        let cases = if family == "motorola68000" {
            let selected = motorola68000_instruction_corpus(&root);
            let mut expected = fs::read_dir(root.join("examples/motorola68000"))
                .unwrap()
                .map(|entry| entry.unwrap().path())
                .filter(|path| path.extension().is_some_and(|ext| ext == "asm"))
                .collect::<Vec<_>>();
            expected.sort();
            assert_eq!(
                selected
                    .iter()
                    .map(|case| case.source.clone())
                    .collect::<Vec<_>>(),
                expected
            );
            selected
        } else {
            all_cases
        };
        assert!(!cases.is_empty(), "empty {family} corpus");
        for (index, case) in cases.iter().enumerate() {
            assert_eq!(case.reference.file_stem(), case.source.file_stem());
            let (entry, inputs) =
                stage(case, &scratch.join(family).join(index.to_string())).unwrap();
            assert_eq!(entry, case.source.file_name().unwrap().to_string_lossy());
            assert_eq!(inputs.len(), 1);
            assert_eq!(inputs[Path::new(&entry)], fs::read(&case.source).unwrap());
        }
    }
}

#[test]
fn compact_corpus_batch_size_is_explicit_and_bounded() {
    assert_eq!(parse_corpus_batch_size(None).unwrap(), None);
    assert_eq!(parse_corpus_batch_size(Some("1")).unwrap(), Some(1));
    assert_eq!(parse_corpus_batch_size(Some("64")).unwrap(), Some(64));
    for invalid in ["", "0", "65", "-1", "sequential"] {
        assert!(parse_corpus_batch_size(Some(invalid)).is_err(), "{invalid}");
    }
}

fn report_test_prepared(row_index: usize) -> PreparedCorpusCase {
    PreparedCorpusCase {
        row_index,
        name: "same display name".into(),
        bytes: b"actual source".to_vec(),
        guest: vec![("entry.asm".into(), b"actual source".to_vec())],
        command: "Work:entry.asm --hex Work:audit.hex".into(),
        outputs: BTreeMap::from([(PathBuf::from("audit.hex"), b"live oracle".to_vec())]),
        negative: false,
        oracle_ok: true,
        oracle_matches_reference: true,
    }
}

fn report_test_run(exit_code: i32) -> FsUaeSmokeRun {
    FsUaeSmokeRun {
        example_name: "host report adapter",
        source_path: PathBuf::from("source"),
        artifact_dir: PathBuf::from("ephemeral"),
        hunk_path: PathBuf::from("frozen"),
        stdout: "guest diagnostic".into(),
        stderr: String::new(),
        exit_code: Some(exit_code),
        protocol_completed: true,
        start_to_done_host_seconds: Some(12.0),
        native_image_digest: Some("frozen digest".into()),
        success: exit_code == 0,
        verified_output: None,
        captured_artifacts: BTreeMap::from([(
            PathBuf::from("Work/audit.hex"),
            b"live oracle".to_vec(),
        )]),
    }
}

#[test]
fn compact_corpus_batch_results_preserve_original_rows_and_host_errors() {
    // Selected original indices contain a host preparation error between two
    // native cases. Names deliberately coincide; only the stored row index maps results.
    let mut rows = vec![
        json!({"case_index": 2, "native_ok": false}),
        json!({"case_index": 7, "native_ok": false, "error": "host oracle error"}),
        json!({"case_index": 8, "native_ok": false}),
    ];
    apply_batch_result(
        &mut rows,
        &report_test_prepared(0),
        BreadthCaseResult {
            run: None,
            error: Some("timeout after isolated retry".into()),
        },
    );
    apply_batch_result(
        &mut rows,
        &report_test_prepared(2),
        BreadthCaseResult {
            run: Some(report_test_run(0)),
            error: None,
        },
    );
    assert_eq!(rows[0]["case_index"], 2);
    assert_eq!(rows[0]["native_ok"], false);
    assert_eq!(rows[0]["error"], "timeout after isolated retry");
    assert_eq!(rows[1]["error"], "host oracle error");
    assert_eq!(rows[2]["case_index"], 8);
    assert_eq!(rows[2]["native_ok"], true);
    assert_eq!(rows[2]["native_exact_hex_ok"], true);
    for index in [0, 2] {
        assert!(rows[index]["native_seconds"].is_null());
        assert!(rows[index]["elapsed_seconds"].is_null());
    }
}

#[test]
fn compact_corpus_batch_adapter_never_qualifies_failed_proof_or_oracle() {
    let mut prepared = report_test_prepared(0);
    prepared.negative = true;
    prepared.oracle_ok = false;
    let mut rows = vec![json!({"native_ok": false})];
    apply_batch_result(
        &mut rows,
        &prepared,
        BreadthCaseResult {
            run: Some(report_test_run(20)),
            error: Some("missing fresh diagnostic evidence".into()),
        },
    );
    assert_eq!(rows[0]["native_ok"], false);
    assert_eq!(rows[0]["error"], "missing fresh diagnostic evidence");
    prepared.oracle_matches_reference = false;
    rows[0]["rust_cli_matches_reference_status"] = json!(false);
    apply_batch_result(
        &mut rows,
        &prepared,
        BreadthCaseResult {
            run: Some(report_test_run(20)),
            error: None,
        },
    );
    assert_eq!(rows[0]["native_ok"], false);
    assert_eq!(rows[0]["rust_cli_matches_reference_status"], false);
    assert_eq!(
        rows[0]["diagnostic_parity"],
        "unqualified: rejection with diagnostic only"
    );
}

#[test]
fn compact_corpus_cases_borrow_current_packages_and_live_oracle_directly() {
    let prepared = report_test_prepared(0);
    let packages = vec![("packages/current.opk".into(), b"current package".as_slice())];
    let files = corpus_guest_files(&prepared, &packages);
    assert_eq!(files.len(), 2);
    assert_eq!(files[0].bytes, prepared.bytes);
    assert_eq!(files[1].bytes.as_ptr(), packages[0].1.as_ptr());
    let case = prepared.native_case(&files);
    assert_eq!(case.source_override, Some(prepared.bytes.as_slice()));
    assert_eq!(case.command_template, Some(prepared.command.as_str()));
    match case.proof {
        OpforgeNativeCliProof::ExactArtifact {
            relative_path,
            rust_oracle,
        } => {
            assert_eq!(relative_path, "Work/audit.hex");
            assert_eq!(
                rust_oracle.as_ptr(),
                prepared.outputs[Path::new("audit.hex")].as_ptr()
            );
        }
        _ => panic!("positive live oracle must require exact artifact proof"),
    }
}
