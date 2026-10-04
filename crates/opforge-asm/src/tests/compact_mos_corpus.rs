// SPDX-License-Identifier: GPL-3.0-or-later
//! Full, unmodified family/opcore corpus audits; stored references are never refreshed.
use super::*;
use crate::fs_uae_smoke::{
    compact_cli_input::assemble_cli, opforge_self_host_package_digest,
    run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliGuestFile,
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

/// Audit all top-level instruction fixtures, including expected-error cases.
/// Nested AmigaOS programs/support require a separate Hunk/CPU setup audit.
#[test]
#[ignore = "top-level Motorola instruction corpus fresh compact-native audit; requires configured FS-UAE"]
fn compact_motorola68000_corpus_fs_uae() {
    let root = workspace_root();
    run_compact_corpus(&M68K_AUDIT, motorola68000_instruction_corpus(&root));
}

fn run_compact_corpus(audit: &CorpusAudit, cases: Vec<Case>) {
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
    let image = assemble_cli(&root, &build);
    let packages = tree_files(&build.output_dir.join("packages"));
    let selection = std::env::var(audit.cases_env).ok();
    let mut rows = Vec::new();
    for (index, case) in cases.iter().enumerate() {
        if selection
            .as_ref()
            .is_some_and(|names| !names.split(',').any(|item| case.name.contains(item.trim())))
        {
            continue;
        }
        let started = Instant::now();
        let bytes = fs::read(&case.source).unwrap();
        let cpu = initial_cpu(&bytes);
        let reference = reference_check(case, &scratch.join(format!("reference-{index}")));
        let mut row = json!({"case": case.name, "cpu": cpu, "source_digest": opforge_self_host_package_digest(&bytes),
            "source_bytes": bytes.len(), "rust_reference_ok": reference.is_ok(), "rust_reference_error": reference.err(),
            "native_ok": false, "image_digest": opforge_self_host_package_digest(&image)});
        let outcome = (|| -> Result<(), String> {
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
            let mut guest = inputs
                .iter()
                .map(|(p, b)| (p.to_string_lossy().into_owned(), b.clone()))
                .collect::<Vec<_>>();
            guest.extend(
                packages
                    .iter()
                    .map(|(p, b)| (format!("packages/{}", p.display()), b.clone())),
            );
            let files = guest
                .iter()
                .map(|(p, b)| OpforgeNativeCliGuestFile {
                    relative_path: p,
                    bytes: b,
                })
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
            let hex = outputs.get(Path::new("audit.hex"));
            let native = OpforgeNativeCliParityCase {
                name: &case.name,
                cpu_override: "68020",
                extra_assembly_defines: &[],
                source_override: Some(&bytes),
                command_template: Some(&command),
                package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &files,
                proof: if negative || oracle.is_err() {
                    OpforgeNativeCliProof::ExpectedFailureWithDiagnostic
                } else if let Some(hex) = hex {
                    OpforgeNativeCliProof::ExactArtifact {
                        relative_path: "Work/audit.hex",
                        rust_oracle: hex,
                    }
                } else {
                    // Observe fresh completion, but never qualify missing hex evidence.
                    OpforgeNativeCliProof::SuccessfulExitContaining("")
                },
            };
            match run_prebuilt_compact_cli_case_from_env(&root, &native, &image)? {
                FsUaeSmokeOutcome::Skipped(reason) => Err(format!("skipped: {reason}")),
                FsUaeSmokeOutcome::Completed { runs } => {
                    if runs.len() != 1 {
                        return Err(format!("expected one run, got {}", runs.len()));
                    }
                    let run = &runs[0];
                    row["native_stdout"] = json!(run.stdout);
                    row["native_stderr"] = json!(run.stderr);
                    row["exit_code"] = json!(run.exit_code);
                    row["protocol_completed"] = json!(run.protocol_completed);
                    row["native_seconds"] = json!(run.start_to_done_host_seconds);
                    row["native_exact_hex_ok"] =
                        json!(!negative && hex.is_some() && oracle.is_ok() && run.success);
                    row["native_negative_rejection_ok"] = json!(
                        negative
                            && run.protocol_completed
                            && run.exit_code.is_some_and(|exit| exit != 0)
                    );
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
                    if !oracle_matches_reference
                        || (!negative && hex.is_none())
                        || (!negative && !run.success)
                        || !run.protocol_completed
                        || (!negative && run.exit_code != Some(0))
                        || (negative && !run.exit_code.is_some_and(|exit| exit != 0))
                        || !gaps.is_empty()
                    {
                        Err("native completion/artifact parity failed".into())
                    } else {
                        Ok(())
                    }
                }
            }
        })();
        row["native_ok"] = json!(outcome.is_ok());
        row["error"] = json!(outcome.err());
        row["elapsed_seconds"] = json!(started.elapsed().as_secs_f64());
        eprintln!("{}: {}", case.name, row["native_ok"]);
        rows.push(row);
        save_report(audit, "compact-native", &selection, cases.len(), &rows);
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
