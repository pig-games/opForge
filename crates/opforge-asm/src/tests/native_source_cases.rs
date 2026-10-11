//! Correctness-only batches for source trees executed by the frozen compact CLI.
use super::native_active_includes::live_oracle;
use super::native_iterable_loops::{oracle, scratch};
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

pub(super) fn run(root: &Path, image: &[u8], cases: &[Case]) {
    run_with_inputs(
        root,
        image,
        cases,
        8,
        &[],
        "Work:entry.asm --bin Work:out.bin",
    );
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
        8,
        packages,
        "Work:entry.asm --bin Work:out.bin -P Work:packages",
    );
}

/// Use the caller's bounded batch size while retaining each live source oracle.
pub(super) fn run_batch(root: &Path, image: &[u8], cases: &[Case], batch_size: usize) {
    run_with_inputs(
        root,
        image,
        cases,
        batch_size,
        &[],
        "Work:entry.asm --bin Work:out.bin",
    );
}

fn run_with_inputs(
    root: &Path,
    image: &[u8],
    cases: &[Case],
    batch_size: usize,
    packages: &[OpforgeNativeCliGuestFile<'_>],
    command: &str,
) {
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
                let actual = live_oracle(&dir.0, &case.files);
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
                    oracle(&dir.0, source).is_err(),
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
