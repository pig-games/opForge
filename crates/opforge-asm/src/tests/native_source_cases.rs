//! Correctness-only batches for source trees executed by the frozen compact CLI.
use super::native_active_includes::live_oracle;
use super::native_iterable_loops::{oracle, oracle_with_module_roots, scratch};
use super::*;

pub(super) struct Case {
    pub(super) name: String,
    pub(super) cpu: String,
    pub(super) files: Vec<(&'static str, String)>,
    /// `None` means Rust and native must both reject the staged source tree.
    pub(super) expected: Option<Vec<u8>>,
}

fn write_tree(dir: &Path, files: &[(&str, String)]) {
    for (relative, contents) in files {
        let path = dir.join(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }
}

struct SourceInputs<'a> {
    batch_size: usize,
    packages: &'a [OpforgeNativeCliGuestFile<'a>],
    command: &'a str,
    module_roots: &'a [&'a str],
    source_rejections: bool,
}

impl<'a> SourceInputs<'a> {
    fn plain(batch_size: usize) -> Self {
        Self {
            batch_size,
            packages: &[],
            command: "Work:entry.asm --bin Work:out.bin",
            module_roots: &[],
            source_rejections: false,
        }
    }
}

pub(super) fn run(root: &Path, image: &[u8], cases: &[Case]) {
    run_with_inputs(root, image, cases, SourceInputs::plain(8));
}

/// Stage current producer packages under `packages/` for targets absent from
/// the frozen executable's embedded catalog. Each source still selects its CPU.
pub(super) fn run_with_packages(
    root: &Path,
    image: &[u8],
    cases: &[Case],
    packages: &[OpforgeNativeCliGuestFile<'_>],
) {
    assert!(packages
        .iter()
        .all(|file| file.relative_path.starts_with("packages/") && !file.bytes.is_empty()));
    run_with_inputs(
        root,
        image,
        cases,
        SourceInputs {
            packages,
            command: "Work:entry.asm --bin Work:out.bin -P Work:packages",
            ..SourceInputs::plain(8)
        },
    );
}

/// Use the caller's bounded batch size while retaining each live source oracle.
pub(super) fn run_batch(root: &Path, image: &[u8], cases: &[Case], batch_size: usize) {
    run_with_inputs(root, image, cases, SourceInputs::plain(batch_size));
}

/// Source-language rejection must reach preparation, rather than fail package startup.
pub(super) fn run_source_language_batch(
    root: &Path,
    image: &[u8],
    cases: &[Case],
    batch_size: usize,
) {
    run_with_inputs(
        root,
        image,
        cases,
        SourceInputs {
            source_rejections: true,
            ..SourceInputs::plain(batch_size)
        },
    );
}

pub(super) fn run_discovery_batch(root: &Path, image: &[u8], cases: &[Case], batch_size: usize) {
    run_with_inputs(
        root,
        image,
        cases,
        SourceInputs {
            command: "Work:entry.asm --bin Work:out.bin -M Work:library",
            module_roots: &["library"],
            source_rejections: true,
            ..SourceInputs::plain(batch_size)
        },
    );
}

fn run_with_inputs(root: &Path, image: &[u8], cases: &[Case], inputs: SourceInputs<'_>) {
    let SourceInputs {
        batch_size,
        packages,
        command,
        module_roots,
        source_rejections,
    } = inputs;
    let mut package_paths = std::collections::HashSet::new();
    for package in packages {
        assert!(
            package_paths.insert(package.relative_path.to_ascii_lowercase()),
            "duplicate guest package path: {}",
            package.relative_path
        );
    }
    assert!(
        (1..=64).contains(&batch_size),
        "batch size must be in 1..=64"
    );
    assert!(!cases.is_empty(), "source-tree batch must contain cases");
    for case in cases {
        assert!(!case.files.is_empty(), "{} has no entry source", case.name);
        assert_eq!(case.files[0].0, "entry.asm", "{} entry path", case.name);
        assert!(
            case.files
                .iter()
                .all(|(path, _)| !package_paths.contains(&path.to_ascii_lowercase())),
            "{} source path collides with a staged package",
            case.name
        );
    }

    let mut oracles = Vec::with_capacity(cases.len());
    for case in cases {
        let dir = scratch();
        let live = match &case.expected {
            Some(expected) => {
                let actual = if module_roots.is_empty() {
                    live_oracle(&dir.0, &case.files)
                } else {
                    write_tree(&dir.0, &case.files);
                    oracle_with_module_roots(&dir.0, &case.files[0].1, module_roots)
                        .expect("live Rust module-search oracle")
                };
                assert_eq!(
                    &actual, expected,
                    "Rust oracle changed for {} cpu={}",
                    case.name, case.cpu
                );
                Some(actual)
            }
            None => {
                write_tree(&dir.0, &case.files);
                let source = &case.files[0].1;
                assert!(
                    if module_roots.is_empty() {
                        oracle(&dir.0, source)
                    } else {
                        oracle_with_module_roots(&dir.0, source, module_roots)
                    }
                    .is_err(),
                    "Rust accepted expected rejection {} cpu={}",
                    case.name,
                    case.cpu
                );
                None
            }
        };
        oracles.push(live);
    }

    let guest_files = cases
        .iter()
        .map(|case| {
            case.files
                .iter()
                .map(|(path, contents)| OpforgeNativeCliGuestFile {
                    relative_path: path,
                    bytes: contents.as_bytes(),
                })
                .chain(packages.iter().copied())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let parity_cases = cases
        .iter()
        .zip(&oracles)
        .zip(&guest_files)
        .map(|((case, oracle), files)| OpforgeNativeCliParityCase {
            name: &case.name,
            // The guest executable runs on 68020; source `.cpu` selects target.
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(case.files[0].1.as_bytes()),
            command_template: Some(command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: files,
            proof: match oracle {
                Some(rust_oracle) => OpforgeNativeCliProof::ExactArtifact {
                    relative_path: "Work/out.bin",
                    rust_oracle,
                },
                None => OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
            },
        })
        .collect::<Vec<_>>();

    let results = run_prebuilt_compact_cli_breadth_from_env(root, &parity_cases, image, batch_size)
        .expect("fresh source-tree correctness batch");
    let mut failures = Vec::new();
    if results.len() != cases.len() {
        failures.push(format!(
            "runner returned {} results for {} cases",
            results.len(),
            cases.len()
        ));
    }
    for (index, case) in cases.iter().enumerate() {
        let Some(result) = results.get(index) else {
            failures.push(format!("{} cpu={}: result missing", case.name, case.cpu));
            eprintln!(
                "SOURCE_TREE_CASE index={index} name={} cpu={} protocol=false exit=None artifact=missing timing=None (correctness-only)",
                case.name, case.cpu
            );
            continue;
        };
        let label = format!("{} cpu={}", case.name, case.cpu);
        let Some(run) = &result.run else {
            failures.push(format!(
                "{label}: {}",
                result
                    .error
                    .as_deref()
                    .unwrap_or("guest run missing without runner error")
            ));
            eprintln!(
                "SOURCE_TREE_CASE index={index} name={} cpu={} protocol=false exit=None artifact=failed timing=None (correctness-only)",
                case.name, case.cpu
            );
            continue;
        };
        // Captured-path inventories are not diagnostics; retain the proof
        // failure and decoded guest stdout/stderr without duplicating them.
        let proof_error = result.error.as_deref().map(|error| {
            error
                .split("; fresh captured paths:")
                .next()
                .unwrap_or(error)
        });
        if !run.protocol_completed {
            failures.push(format!("{label}: fresh guest protocol did not complete"));
        }
        match &case.expected {
            Some(_) => {
                if run.exit_code != Some(0) || !run.success || result.error.is_some() {
                    failures.push(format!(
                        "{label}: expected exact artifact and exit 0; exit={:?}, success={}, error={:?}, stdout={:?}, stderr={:?}",
                        run.exit_code, run.success, proof_error, run.stdout, run.stderr
                    ));
                }
            }
            None => {
                if source_rejections
                    && [&run.stdout, &run.stderr].iter().any(|output| {
                        output.contains("package: missing, invalid or incompatible runtime package")
                    })
                {
                    failures.push(format!(
                        "{label}: package startup failure did not exercise source rejection"
                    ));
                }
                if matches!(run.exit_code, None | Some(0))
                    || run.success
                    || (run.stdout.trim().is_empty() && run.stderr.trim().is_empty())
                    || result.error.is_some()
                {
                    failures.push(format!(
                        "{label}: expected fresh rejection with diagnostic; exit={:?}, success={}, error={:?}, stdout={:?}, stderr={:?}",
                        run.exit_code, run.success, proof_error, run.stdout, run.stderr
                    ));
                }
            }
        }
        if run.start_to_done_host_seconds.is_some() {
            failures.push(format!(
                "{label}: correctness-only batch recorded unexpected timing"
            ));
        }
        let artifact = match &case.expected {
            Some(_) if result.error.is_none() && run.exit_code == Some(0) => "exact-match",
            Some(_) => "failed",
            None if result.error.is_none() && run.exit_code.is_some_and(|code| code != 0) => {
                "expected-rejection"
            }
            None => "failed",
        };
        eprintln!(
            "SOURCE_TREE_CASE index={index} name={} cpu={} protocol={} exit={:?} artifact={} timing=None (correctness-only)",
            case.name, case.cpu, run.protocol_completed, run.exit_code, artifact
        );
    }
    assert!(
        failures.is_empty(),
        "source-tree batch failures: {failures:?}"
    );
}
