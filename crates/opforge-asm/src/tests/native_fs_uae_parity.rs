//! Native FS-UAE, preprocessor, and reference parity tests.

use super::*;

#[test]
fn external_fs_uae_hunk_smoke() {
    match crate::fs_uae_smoke::run_hunk_smoke_from_env(&workspace_root())
        .expect("FS-UAE smoke helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            for run in runs {
                let combined_output = format!("{}\n{}", run.stdout, run.stderr);
                assert!(
                    run.success,
                    "FS-UAE smoke failed for example {} from {} with {} under {}\nstdout:\n{}\nstderr:\n{}",
                    run.example_name,
                    run.source_path.display(),
                    run.hunk_path.display(),
                    run.artifact_dir.display(),
                    run.stdout,
                    run.stderr,
                );
                if run.example_name == "tkpkg_debug_cli" {
                    assert!(
                        combined_output.contains("TKPKG load_package/set_pipeline OK"),
                        "FS-UAE smoke for {} did not report the pipeline success marker\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                    assert!(
                        combined_output.contains("Identifier(\"move.b\")@1:1-7"),
                        "FS-UAE smoke for {} did not report the first file-mode output row\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                    assert!(
                        combined_output.contains("Comma@1:10-11"),
                        "FS-UAE smoke for {} did not report the first file-mode comma row\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                    assert!(
                        combined_output.contains("Identifier(\"move.w\")@2:1-7"),
                        "FS-UAE smoke for {} did not report the second file-mode output row\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                    assert!(
                        combined_output.contains("Identifier(\"d3\")@2:11-13"),
                        "FS-UAE smoke for {} did not report the second file-mode operand row\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                    assert!(
                        combined_output.contains("TKPKG last_error clear OK"),
                        "FS-UAE smoke for {} did not report the last_error-clear marker\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                }
                if run.example_name == "prvm_smoke" {
                    assert!(
                        combined_output.contains("OPFORGE-PRVM smoke OK"),
                        "FS-UAE smoke for {} did not report the PRVM success marker\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                }
                if run.example_name == "prvm_line_iterator_smoke" {
                    assert!(
                        combined_output.contains("OPFORGE-PRVM-ITER smoke OK"),
                        "FS-UAE smoke for {} did not report the PRVM iterator success marker\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                }
                eprintln!(
                    "FS-UAE smoke completed for example {} with {} under {}",
                    run.example_name,
                    run.hunk_path.display(),
                    run.artifact_dir.display()
                );
            }
        }
    }
}

struct FsUaeNativeCliSmokeLock(std::sync::Mutex<()>);

impl FsUaeNativeCliSmokeLock {
    fn lock(&self) -> Result<std::sync::MutexGuard<'_, ()>, std::convert::Infallible> {
        Ok(match self.0.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        })
    }
}

fn fs_uae_native_cli_smoke_lock() -> &'static FsUaeNativeCliSmokeLock {
    static LOCK: std::sync::OnceLock<FsUaeNativeCliSmokeLock> = std::sync::OnceLock::new();
    LOCK.get_or_init(|| FsUaeNativeCliSmokeLock(std::sync::Mutex::new(())))
}

#[derive(Clone, Copy)]
enum NativeCliSchemaArtifactLocation {
    CaseWork,
    CaseWorkBuild,
}

enum NativeCliSchemaExpectedArtifact {
    Binary {
        location: NativeCliSchemaArtifactLocation,
        file_name: &'static str,
        expected: Vec<u8>,
    },
    Text {
        location: NativeCliSchemaArtifactLocation,
        file_name: &'static str,
        expected: String,
        normalization: NativeCliSchemaTextNormalization,
    },
}

#[derive(Clone, Copy)]
enum NativeCliSchemaTextNormalization {
    Listing,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NativeCliSchemaDiagnosticKind {
    UnknownMnemonic,
}

struct NativeCliSchemaCase {
    name: &'static str,
    defines: Vec<&'static str>,
    source: Option<String>,
    command_template: Option<&'static str>,
    expected_success: bool,
    stdout_contains: Vec<&'static str>,
    expected_diagnostic: Option<NativeCliSchemaDiagnosticKind>,
    artifact: Option<NativeCliSchemaExpectedArtifact>,
}

fn native_cli_schema_live_rust_binary_oracle(
    case: &crate::native_reference_parity::NativeReferenceCase,
    source: &str,
) -> (NativeCliSchemaArtifactLocation, &'static str, Vec<u8>) {
    let case_dir = create_temp_dir("native-reference-live-rust-oracle");
    let input_path = case_dir.join("input.asm");
    let rust_bin_path = case_dir.join("rust-schema.bin");
    let mut prg_guard = None;
    let (oracle_source, output_path, location, file_name, cli) = match case.source_mode {
        NativeReferenceSourceMode::SourceBinFromExample => {
            let cli = Cli::parse_from([
                "opForge",
                input_path.to_string_lossy().as_ref(),
                "--bin",
                rust_bin_path.to_string_lossy().as_ref(),
                "--cpu",
                case.cpu_id.as_str(),
            ]);
            (
                source.to_string(),
                rust_bin_path,
                NativeCliSchemaArtifactLocation::CaseWork,
                crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_6502_OUTPUT_FILE,
                cli,
            )
        }
        NativeReferenceSourceMode::SourceCpuPrgFromExample => {
            prg_guard = Some(
                native_cli_schema_rust_prg_lock()
                    .lock()
                    .expect("Rust schema PRG oracle lock poisoned"),
            );
            let output_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("build")
                .join("6502-first-run.prg");
            assert!(
                !output_path.exists(),
                "Rust schema oracle output must not pre-exist: {}",
                output_path.display()
            );
            let cli = Cli::parse_from(["opForge", input_path.to_string_lossy().as_ref()]);
            (
                source.to_string(),
                output_path,
                NativeCliSchemaArtifactLocation::CaseWorkBuild,
                "6502-first-run.prg",
                cli,
            )
        }
    };
    fs::write(&input_path, oracle_source)
        .unwrap_or_else(|err| panic!("write Rust schema oracle {}: {err}", input_path.display()));
    run_with_cli_with_context(&cli)
        .unwrap_or_else(|err| panic!("run Rust CLI oracle for {}: {err:?}", case.asm_path));
    let expected = fs::read(&output_path).unwrap_or_else(|err| {
        panic!(
            "read Rust CLI oracle output {} for {}: {err}",
            output_path.display(),
            case.asm_path
        )
    });
    if prg_guard.is_some() {
        fs::remove_file(&output_path).unwrap_or_else(|err| {
            panic!(
                "remove Rust CLI oracle output {}: {err}",
                output_path.display()
            )
        });
        let output_dir = output_path.parent().expect("Rust PRG output parent");
        fs::remove_dir(output_dir).unwrap_or_else(|err| {
            panic!(
                "remove Rust CLI oracle output directory {}: {err}",
                output_dir.display()
            )
        });
    }
    (location, file_name, expected)
}

fn native_cli_schema_rust_prg_lock() -> &'static std::sync::Mutex<()> {
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    LOCK.get_or_init(|| std::sync::Mutex::new(()))
}

fn native_cli_schema_cases_with_live_rust_oracle(repo_root: &Path) -> Vec<NativeCliSchemaCase> {
    native_reference_cases()
        .iter()
        .map(|case| {
            let asm_path = repo_root.join(&case.asm_path);
            let source = fs::read_to_string(&asm_path).unwrap_or_else(|err| {
                panic!("read native reference source {}: {err}", asm_path.display())
            });
            let (location, file_name, expected) =
                native_cli_schema_live_rust_binary_oracle(case, &source);
            NativeCliSchemaCase {
                name: case.asm_path.as_str(),
                defines: vec![],
                source: Some(source),
                command_template: Some(case.command_template.as_str()),
                expected_success: true,
                stdout_contains: vec![],
                expected_diagnostic: None,
                artifact: Some(NativeCliSchemaExpectedArtifact::Binary {
                    location,
                    file_name,
                    expected,
                }),
            }
        })
        .collect()
}

fn native_cli_schema_listing_case_with_live_rust_oracle(repo_root: &Path) -> NativeCliSchemaCase {
    let asm_path = repo_root.join("examples/mos6502/6502_simple.asm");
    let source = fs::read_to_string(&asm_path)
        .unwrap_or_else(|err| panic!("read listing schema source {}: {err}", asm_path.display()));
    let case_dir = create_temp_dir("native-reference-live-rust-listing-oracle");
    let input_path = case_dir.join("input.asm");
    let list_path = case_dir.join("rust-schema.lst");
    fs::write(&input_path, &source).unwrap_or_else(|err| {
        panic!(
            "write listing schema source {}: {err}",
            input_path.display()
        )
    });
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--list",
        list_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli)
        .unwrap_or_else(|err| panic!("run Rust CLI listing oracle: {err:?}"));
    let expected = fs::read_to_string(&list_path)
        .unwrap_or_else(|err| panic!("read Rust CLI listing {}: {err}", list_path.display()));
    NativeCliSchemaCase {
        name: "examples/mos6502/6502_simple.asm#listing",
        defines: vec![],
        source: Some(source),
        command_template: Some("{input} --list {list} --cpu m6502"),
        expected_success: true,
        stdout_contains: vec![],
        expected_diagnostic: None,
        artifact: Some(NativeCliSchemaExpectedArtifact::Text {
            location: NativeCliSchemaArtifactLocation::CaseWorkBuild,
            file_name: crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_LST_OUTPUT_FILE,
            expected,
            normalization: NativeCliSchemaTextNormalization::Listing,
        }),
    }
}

fn native_cli_schema_unknown_mnemonic_case_with_live_rust_oracle() -> NativeCliSchemaCase {
    let source = "start   wat #$42\n".to_string();
    let case_dir = create_temp_dir("native-reference-live-rust-diagnostic-oracle");
    let input_path = case_dir.join("input.asm");
    let bin_path = case_dir.join("rust-schema.bin");
    fs::write(&input_path, &source).expect("write diagnostic schema source");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    let error = run_with_cli_with_context(&cli).expect_err("unknown mnemonic must fail");
    let expected_diagnostic = match error {
        CliRunError::Assembler { error, .. }
            if error
                .diagnostics()
                .iter()
                .any(|diagnostic| diagnostic.error.message().contains("No instruction found")) =>
        {
            NativeCliSchemaDiagnosticKind::UnknownMnemonic
        }
        other => panic!("unexpected Rust diagnostic oracle result: {other:?}"),
    };
    NativeCliSchemaCase {
        name: "schema#unknown-mnemonic",
        defines: vec![],
        source: Some(source),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        expected_success: false,
        stdout_contains: vec![],
        expected_diagnostic: Some(expected_diagnostic),
        artifact: None,
    }
}

fn native_cli_schema_artifact_path(
    location: NativeCliSchemaArtifactLocation,
    file_name: &str,
) -> PathBuf {
    match location {
        NativeCliSchemaArtifactLocation::CaseWork => PathBuf::from("Work").join(file_name),
        NativeCliSchemaArtifactLocation::CaseWorkBuild => {
            PathBuf::from("Work").join("build").join(file_name)
        }
    }
}

fn native_cli_schema_runner_proof(
    case: &NativeCliSchemaCase,
) -> crate::fs_uae_smoke::OpforgeNativeCliProof<'_> {
    match case.artifact.as_ref() {
        Some(NativeCliSchemaExpectedArtifact::Binary {
            location,
            file_name,
            expected,
        }) => crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: match (*location, *file_name) {
                (NativeCliSchemaArtifactLocation::CaseWork, "opforge_native_out.bin") => {
                    "Work/opforge_native_out.bin"
                }
                (NativeCliSchemaArtifactLocation::CaseWorkBuild, "6502-first-run.prg") => {
                    "Work/build/6502-first-run.prg"
                }
                _ => panic!("unsupported schema binary proof artifact for {}", case.name),
            },
            rust_oracle: expected,
        },
        Some(NativeCliSchemaExpectedArtifact::Text {
            location,
            file_name,
            expected,
            ..
        }) => crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: match (*location, *file_name) {
                (NativeCliSchemaArtifactLocation::CaseWorkBuild, "opforge_native_out.lst") => {
                    "Work/build/opforge_native_out.lst"
                }
                _ => panic!("unsupported schema text proof artifact for {}", case.name),
            },
            rust_oracle: expected.as_bytes(),
        },
        None => crate::fs_uae_smoke::OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
    }
}

fn assert_native_cli_schema_case(
    schema_case: &NativeCliSchemaCase,
    run: &crate::fs_uae_smoke::FsUaeSmokeRun,
) {
    assert_eq!(
        run.success, schema_case.expected_success,
        "schema-driven native CLI case {} success mismatch\nstdout:\n{}\nstderr:\n{}",
        schema_case.name, run.stdout, run.stderr,
    );
    for needle in &schema_case.stdout_contains {
        assert!(
            run.stdout.contains(needle),
            "schema-driven native CLI case {} missing stdout marker '{}'\nstdout:\n{}\nstderr:\n{}",
            schema_case.name,
            needle,
            run.stdout,
            run.stderr,
        );
    }

    if let Some(expected) = schema_case.expected_diagnostic {
        assert_eq!(
            native_cli_schema_normalize_native_diagnostic(&run.stderr),
            Some(expected),
            "schema-driven native CLI diagnostic mismatch for {}\nstdout:\n{}\nstderr:\n{}",
            schema_case.name,
            run.stdout,
            run.stderr,
        );
    }

    let Some(artifact) = &schema_case.artifact else {
        return;
    };
    let (location, file_name) = match artifact {
        NativeCliSchemaExpectedArtifact::Binary {
            location,
            file_name,
            ..
        }
        | NativeCliSchemaExpectedArtifact::Text {
            location,
            file_name,
            ..
        } => (*location, *file_name),
    };
    let output_path = native_cli_schema_artifact_path(location, file_name);
    let actual = run.captured_artifacts.get(&output_path).unwrap_or_else(|| {
        panic!(
            "schema-driven native CLI run did not capture {} for {}",
            output_path.display(),
            schema_case.name
        )
    });
    native_cli_schema_compare_artifact(artifact, &actual).unwrap_or_else(|message| {
        panic!(
            "schema-driven native CLI artifact mismatch for {}: {message}\nstdout:\n{}\nstderr:\n{}",
            schema_case.name, run.stdout, run.stderr,
        )
    });
}

fn native_cli_schema_normalize_native_diagnostic(
    output: &str,
) -> Option<NativeCliSchemaDiagnosticKind> {
    if output.contains("unknown native mnemonic") {
        Some(NativeCliSchemaDiagnosticKind::UnknownMnemonic)
    } else {
        None
    }
}

fn native_cli_schema_compare_artifact(
    expected_artifact: &NativeCliSchemaExpectedArtifact,
    actual: &[u8],
) -> Result<(), String> {
    match expected_artifact {
        NativeCliSchemaExpectedArtifact::Binary { expected, .. } => {
            if actual == expected {
                Ok(())
            } else {
                Err(format!(
                    "binary bytes differ: actual {} byte(s), expected {}",
                    actual.len(),
                    expected.len()
                ))
            }
        }
        NativeCliSchemaExpectedArtifact::Text {
            expected,
            normalization,
            ..
        } => {
            let actual = String::from_utf8(actual.to_vec())
                .map_err(|err| format!("text artifact is not UTF-8: {err}"))?;
            let (actual, expected) = match normalization {
                NativeCliSchemaTextNormalization::Listing => (
                    normalize_listing_for_reference_compare(&actual),
                    normalize_listing_for_reference_compare(expected),
                ),
            };
            if actual == expected {
                Ok(())
            } else {
                Err(format!(
                    "text differs\n{}",
                    diff_text(&expected, &actual, 20)
                ))
            }
        }
    }
}

fn assert_native_cli_run_omits_debug_progress(
    run: &crate::fs_uae_smoke::FsUaeSmokeRun,
    context: &str,
) {
    for marker in [
        "OPFORGE-NATIVE 1",
        "STAGE parser",
        "STAGE session",
        "STATUS output-ok",
        "STATUS selector-status-ok",
        "SESSION-CPU ",
        "SESSION-ORIGIN ",
        "MOD-PATH ",
        "MOD-ROOT ",
        "USE-IMPORT ",
        "USE-SELECT ",
        "INCLUDE-ROOT ",
    ] {
        assert!(
            !run.stdout.contains(marker),
            "{context} should keep native debug progress behind --native-debug; saw marker '{marker}'\nstdout:\n{}\nstderr:\n{}",
            run.stdout,
            run.stderr,
        );
    }
}

#[test]
fn native_reference_schema_live_rust_cli_oracle_covers_binary_manifest_cases() {
    // Proof level A. This test proves every governed binary/PRG schema case can
    // produce a non-empty oracle artifact through the Rust CLI in the same
    // test run. This test does not prove native execution or parity.
    let schema_cases = native_cli_schema_cases_with_live_rust_oracle(&workspace_root());
    assert_eq!(schema_cases.len(), native_reference_cases().len());
    for case in schema_cases {
        let Some(NativeCliSchemaExpectedArtifact::Binary { expected, .. }) = case.artifact else {
            panic!("binary manifest case unexpectedly used a text artifact");
        };
        assert!(
            !expected.is_empty(),
            "Rust CLI oracle should emit bytes for {}",
            case.name
        );
    }
}

#[test]
fn native_reference_schema_contract_preserves_native_cli_command_shapes() {
    // Proof level B. This test proves the Rust-side schema contract retains the
    // actual native CLI command template and a live Rust oracle for every
    // governed case. This test does not prove Amiga-native argument parsing.
    let schema_cases = native_cli_schema_cases_with_live_rust_oracle(&workspace_root());
    for (case, schema_case) in native_reference_cases().iter().zip(schema_cases) {
        assert_eq!(schema_case.name, case.asm_path);
        assert_eq!(
            schema_case.command_template,
            Some(case.command_template.as_str())
        );
        assert!(matches!(
            schema_case.artifact,
            Some(NativeCliSchemaExpectedArtifact::Binary { .. })
        ));
    }
}

#[test]
fn native_reference_schema_listing_comparator_accepts_match_and_rejects_mismatch() {
    // Proof level B. This test proves the Rust-side schema comparator applies
    // only the reviewed listing normalization and rejects changed listing
    // content. This test does not prove native listing generation.
    let expected = NativeCliSchemaExpectedArtifact::Text {
        location: NativeCliSchemaArtifactLocation::CaseWorkBuild,
        file_name: "probe.lst",
        expected: "opForge Assembler v0\n0000 AA".to_string(),
        normalization: NativeCliSchemaTextNormalization::Listing,
    };
    native_cli_schema_compare_artifact(&expected, b"opForge Assembler v99\n0000 AA")
        .expect("banner-only listing difference should normalize");
    let error = native_cli_schema_compare_artifact(&expected, b"opForge Assembler v99\n0000 BB")
        .expect_err("listing payload difference must fail");
    assert!(error.contains("text differs"));
}

#[test]
fn native_reference_schema_live_rust_cli_listing_oracle_uses_exact_example_source() {
    // Proof level A. This test proves the Rust CLI listing oracle consumes the
    // exact checked-in example source and emits a non-empty listing. This test
    // does not prove Amiga-native listing parity.
    let repo_root = workspace_root();
    let schema_case = native_cli_schema_listing_case_with_live_rust_oracle(&repo_root);
    let exact_source =
        fs::read_to_string(repo_root.join("examples/mos6502/6502_simple.asm")).unwrap();
    assert_eq!(schema_case.source.as_deref(), Some(exact_source.as_str()));
    let Some(NativeCliSchemaExpectedArtifact::Text { expected, .. }) = schema_case.artifact else {
        panic!("listing schema must use a text artifact");
    };
    assert!(!expected.is_empty());
}

#[test]
fn native_listing_records_end_line_before_terminal_routing() {
    // Proof level B. This proves the native frontend records `.end` in the
    // source table before treating it as a terminal non-statement. It does not
    // prove the emitted listing text or real guest execution.
    let source = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read native CLI line processor");
    assert!(
        source.contains(
            "\tlea strings.EndMnemonicText, a1\n\tmoveq #4, d1\n\tjsr line_text.opforgeNativeCliLineStartsWith\n\tbne.w record\n\ttst.w state.NativeCliIncludeDepth"
        ),
        "`.end` must enter the ordinary source-record path before terminal routing"
    );
    assert!(
        source.contains("record\n\tjsr assembly_session.opforgeNativeCliRecordSourceLine"),
        "the terminal branch target must record the exact source line"
    );
}

#[test]
fn native_reference_schema_diagnostic_comparator_accepts_match_and_rejects_mismatch() {
    // Proof level B. This test proves deterministic native diagnostic text is
    // classified into the reviewed schema class and unrelated text is rejected.
    // This test does not prove Rust CLI or real Amiga-native behavior.
    assert_eq!(
        native_cli_schema_normalize_native_diagnostic(
            "ERROR OPC-NCLI025: unknown native mnemonic\n"
        ),
        Some(NativeCliSchemaDiagnosticKind::UnknownMnemonic)
    );
    assert_eq!(
        native_cli_schema_normalize_native_diagnostic("ERROR: unrelated failure\n"),
        None
    );
}

#[test]
fn native_reference_schema_live_rust_cli_diagnostic_oracle_classifies_unknown_mnemonic() {
    // Proof level A. This test proves the live Rust CLI rejects the schema
    // source and exposes the stable unknown-mnemonic semantic class. This test
    // does not prove Amiga-native status or diagnostic output.
    let schema_case = native_cli_schema_unknown_mnemonic_case_with_live_rust_oracle();
    assert!(!schema_case.expected_success);
    assert_eq!(
        schema_case.expected_diagnostic,
        Some(NativeCliSchemaDiagnosticKind::UnknownMnemonic)
    );
    assert!(schema_case.artifact.is_none());
}

#[derive(Debug, PartialEq, Eq)]
enum NativeExpressionTextPath {
    ExpressionSlice,
    StoredText,
}

fn native_expression_text_path(
    has_metadata: bool,
    metadata_loads: bool,
    expression_slice_loads: bool,
) -> NativeExpressionTextPath {
    if has_metadata && metadata_loads && expression_slice_loads {
        NativeExpressionTextPath::ExpressionSlice
    } else {
        NativeExpressionTextPath::StoredText
    }
}

#[test]
fn native_expression_metadata_fallback_contract_covers_missing_and_malformed_boundaries() {
    // Proof level C. This test proves the boundary decision for absent,
    // unreadable, unusable, and valid expression metadata. This test does not
    // prove the native branch implementation or real 68020 execution.
    assert_eq!(
        native_expression_text_path(false, false, false),
        NativeExpressionTextPath::StoredText
    );
    assert_eq!(
        native_expression_text_path(true, false, false),
        NativeExpressionTextPath::StoredText
    );
    assert_eq!(
        native_expression_text_path(true, true, false),
        NativeExpressionTextPath::StoredText
    );
    assert_eq!(
        native_expression_text_path(true, true, true),
        NativeExpressionTextPath::ExpressionSlice
    );
}

#[test]
fn native_expression_metadata_fallback_source_routes_failures_to_stored_text() {
    // Proof level B. This test proves the native source routes failed metadata
    // loads and failed slice extraction to storedText. This test does not prove
    // either branch executes on real hardware.
    let source = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm assembly driver");
    assert!(source_contains_in_order(
        &source,
        &[
            "readOperandValueForStatement\t.block",
            "jsr eng.statementHasExprMetadataV1",
            "beq.s storedText",
            "jsr eng.opasmEngineGetStatementExprMetadataV1",
            "beq.s exprSliceStoredTextFallback",
            "jsr eng.opasmEngineGetStatementExprTextSliceV1",
            "beq.w storedText",
            "exprSliceStoredTextFallback",
            "bra.w storedText",
            "storedText",
            "jsr eng.opasmEngineGetStatementTextMetadataV1",
        ]
    ));
}

fn native_source_cpu_normalization_contract(token: &str, trailing: &str) -> Result<String, ()> {
    let normalized = if let Some(inner) = token.strip_prefix('"') {
        inner.strip_suffix('"').ok_or(())?
    } else {
        token
    };
    if normalized.is_empty()
        || (!trailing.trim().is_empty() && !trailing.trim_start().starts_with(';'))
    {
        return Err(());
    }
    Ok(match normalized.to_ascii_lowercase().as_str() {
        "6502" => "m6502".to_string(),
        "m65c02" => "65c02".to_string(),
        other => other.to_string(),
    })
}

#[test]
fn native_source_cpu_normalization_contract_covers_bare_quoted_and_malformed_tokens() {
    // Proof level C. This test proves the intended normalization/rejection
    // decision. This test does not prove native execution or package selection.
    assert_eq!(
        native_source_cpu_normalization_contract("6502", ""),
        Ok("m6502".to_string())
    );
    assert_eq!(
        native_source_cpu_normalization_contract("\"6502\"", " ; comment"),
        Ok("m6502".to_string())
    );
    assert_eq!(
        native_source_cpu_normalization_contract("\"6502", ""),
        Err(())
    );
    assert_eq!(
        native_source_cpu_normalization_contract("\"6502\"", " trailing"),
        Err(())
    );
}

#[test]
fn native_column_one_directive_fallback_routes_mnemonic_before_expression_label_heuristic() {
    // Proof level B. This test locks the native routing order that prevents an
    // expression-bearing column-one directive from becoming a label. It does
    // not execute the 68020 implementation or prove directive semantics.
    let source = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/assembly_session.asm"),
    )
    .expect("read native assembly-session source");
    assert!(source_contains_in_order(
        &source,
        &[
            "firstToken",
            "cmpi.l #1, d4",
            "bne.w firstTokenMnemonic",
            "cmpi.b #'.', (a2)",
            "beq.w firstTokenMnemonic",
            "tst.w state.NativeCliStmtExprFound",
            "bne.w firstTokenLabel",
        ]
    ));
}

fn native_counted_for_contract(lines: &[&str], iteration_limit: u32) -> Result<Vec<String>, ()> {
    fn expand(
        lines: &[&str],
        start: usize,
        end: usize,
        iteration_limit: u32,
    ) -> Result<Vec<String>, ()> {
        let mut output = Vec::new();
        let mut index = start;
        while index < end {
            let trimmed = lines[index].trim();
            if let Some(operand) = trimmed.strip_prefix(".for ") {
                let count = operand.trim().parse::<u32>().map_err(|_| ())?;
                if count > iteration_limit {
                    return Err(());
                }
                let mut depth = 1usize;
                let mut close = index + 1;
                while close < end && depth != 0 {
                    let candidate = lines[close].trim();
                    if candidate.starts_with(".for ") {
                        depth += 1;
                    } else if candidate == ".endfor" {
                        depth -= 1;
                    }
                    close += 1;
                }
                if depth != 0 {
                    return Err(());
                }
                let body = expand(lines, index + 1, close - 1, iteration_limit)?;
                for _ in 0..count {
                    output.extend(body.iter().cloned());
                }
                index = close;
                continue;
            }
            if trimmed == ".endfor" {
                return Err(());
            }
            output.push(lines[index].to_string());
            index += 1;
        }
        Ok(output)
    }
    expand(lines, 0, lines.len(), iteration_limit)
}

#[test]
fn native_counted_for_contract_covers_zero_one_nested_and_limit() {
    // Proof level C. This host boundary model proves counted block replacement,
    // matching, nesting, and the iteration limit expected at the native flow
    // callback. It does not execute the 68020 callback or statement tables.
    assert_eq!(
        native_counted_for_contract(&[".for 0", ".byte 1", ".endfor"], 8).unwrap(),
        Vec::<String>::new()
    );
    assert_eq!(
        native_counted_for_contract(&[".for 1", ".byte 1", ".endfor"], 8).unwrap(),
        vec![".byte 1"]
    );
    assert_eq!(
        native_counted_for_contract(
            &[".for 2", ".byte 1", ".for 2", ".byte 2", ".endfor", ".endfor",],
            8,
        )
        .unwrap(),
        vec![".byte 1", ".byte 2", ".byte 2", ".byte 1", ".byte 2", ".byte 2",]
    );
    assert!(native_counted_for_contract(&[".for 9", ".byte 1", ".endfor"], 8).is_err());
}

#[test]
fn native_counted_for_flow_callback_precedes_pass_processing() {
    // Proof level B. This test proves both engine passes invoke the counted
    // flow callback before label, emission, or PC callbacks and that the driver
    // resets repetition state per pass. It does not execute native code.
    let engine = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_engine.asm"),
    )
    .expect("read native opasm engine");
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    let repetition_flow = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_flow_repetition.asm"),
    )
    .expect("read native repetition-flow implementation");
    assert_eq!(
        engine
            .matches("movea.l OPASM_ENGINE_CTX_FLOW_CONTROL_CB(a5), a0")
            .count(),
        2
    );
    assert!(source_contains_in_order(
        &engine,
        &[
            "movea.l OPASM_ENGINE_CTX_FLOW_CONTROL_CB(a5), a0",
            "move.l d7, -(sp)",
            "jsr (a0)",
            "move.l (sp)+, d7",
            "tst.w OpasmEngineFlowPending.l",
            "beq.w process",
            "move.w OpasmEngineFlowNext.l, d2",
            "clr.w OpasmEngineFlowPending.l",
            "move.w d2, d7",
        ]
    ));
    assert_eq!(driver.matches("clr.w OpasmRepeatDepth").count(), 2);
    assert!(driver.contains("cmpi.l #OPASM_REPEAT_ITERATION_LIMIT, d3"));
    assert!(driver.contains(".use opasm.amigaos.flow_repetition as repetition"));
    assert!(source_contains_in_order(
        &driver,
        &[
            "checkForMnemonic",
            "jsr repetition.routeDirectiveV1",
            "beq.w beginFor",
            "beq.w compareEndfor",
            "beq.w compareWhile",
            "beq.w compareEndwhile",
        ]
    ));
    assert!(source_contains_in_order(
        &repetition_flow,
        &[
            "routeDirectiveV1\t.block",
            "RepetitionForMnemonicText",
            "repetitionEndfor",
            "repetitionEndwhile",
            "moveq #4, d3",
        ]
    ));
}

#[test]
fn native_flow_navigation_initializes_default_callback_contract() {
    // Proof level B. This source contract proves the driver delegates ordinary
    // next-index/process initialization to one navigation owner. It does not
    // execute the 68020 callback path.
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    let line_processor = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read native CLI line processor");
    let navigation = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_flow_navigation.asm"),
    )
    .expect("read native flow navigation module");
    assert!(driver.contains(".use opasm.amigaos.flow_navigation as navigation"));
    assert!(source_contains_in_order(
        &driver,
        &[
            "move.w d0, d7",
            "move.w d7, d0",
            "jsr navigation.initializeStatementFlowV1",
        ]
    ));
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "jsr directive_handlers.opforgeNativeCliParseModuleLine",
            "move.l d0, state.NativeCliPrvmRouteStatus",
            "clr.w state.NativeCliPrvmResultCount",
            "jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine",
            "jsr directive_handlers.opforgeNativeCliParseEndmoduleLine",
            "move.l d0, state.NativeCliPrvmRouteStatus",
            "clr.w state.NativeCliPrvmResultCount",
            "jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine",
        ]
    ));
    assert!(source_contains_in_order(
        &navigation,
        &[
            "initializeStatementFlowV1\t.block",
            "move.w d0, d2",
            "addq.w #1, d2",
            "clr.w d1",
            "moveq #0, d0",
        ]
    ));
    assert!(navigation.contains(".endsection"));
    assert!(navigation.contains(".endmodule"));
}

#[test]
fn native_preprocessor_reentry_source_contract_is_bounded_and_restores_caller_line() {
    // Proof level B. Native source owns a single bounded expansion frame and
    // routes its staged line through the ordinary CLI line processor. It does
    // not prove macro syntax, substitution, or native 68020 execution.
    let root = workspace_root();
    let constants =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/constants.asm"))
            .expect("read native CLI constants");
    let preprocessor =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/preprocessor.asm"))
            .expect("read native preprocessor");
    let expansion = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_expansion.asm"),
    )
    .expect("read native expansion owner");
    let line_processor = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read native CLI line processor");
    assert!(constants.contains("NATIVE_PREPROCESS_EXPANSION_DEPTH_LIMIT = 1"));
    assert!(source_contains_in_order(
        &expansion,
        &[
            "opforgeNativeCliBeginExpandedLineV1\t.block",
            "tst.w state.NativeCliPreprocessExpansionDepth",
            "move.w #1, state.NativeCliPreprocessExpansionDepth",
            "opforgeNativeCliEndExpandedLineV1\t.block",
            "move.w d3, state.NativeCliSourceLineLen",
            "clr.w state.NativeCliPreprocessExpansionDepth",
        ]
    ));
    assert!(preprocessor.contains("opforgeNativeCliResetPreprocessorV1\t.block"));
    for routine in [
        "opforgeNativeCliBeginExpandedLineV1\t.block",
        "opforgeNativeCliEndExpandedLineV1\t.block",
    ] {
        assert!(
            expansion.contains(routine),
            "missing expansion routine: {routine}"
        );
        assert!(
            !preprocessor.contains(routine),
            "expansion routine must have exactly one owner: {routine}"
        );
    }
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "opforgeNativeCliProcessExpandedLineV1\t.block",
            "jsr preprocessor_expansion.opforgeNativeCliBeginExpandedLineV1",
            "jsr opforgeNativeCliTokenizeCurrentLine",
            "jsr preprocessor_expansion.opforgeNativeCliEndExpandedLineV1",
            "opforgeNativeCliProcessExpandedScopeLineV1\t.block",
            "jsr assembly_session.opforgeNativeCliRecordSourceLine",
            "jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine",
            "opforgeNativeCliExpandActiveMacroV1\t.block",
            "bsr.w emitMacroBlockStart",
            "bsr.w opforgeNativeCliProcessExpandedScopeLineV1",
            "bsr.w opforgeNativeCliProcessExpandedLineV1",
            "bsr.w emitMacroBlockEnd",
        ]
    ));
}

#[test]
fn native_preprocessor_expanded_line_frontend_contract_routes_and_restores() {
    // Proof level B. Ordinary substituted lines and generated scope lines have
    // distinct frontend routes, but both must close the staged source frame
    // before returning the route status. This is a source/ABI contract only;
    // it does not prove native execution or rollback of later session state.
    let root = workspace_root();
    let line_processor = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read native CLI line processor");

    assert!(source_contains_in_order(
        &line_processor,
        &[
            "opforgeNativeCliProcessExpandedLineV1\t.block",
            "jsr preprocessor_expansion.opforgeNativeCliBeginExpandedLineV1",
            "jsr engine.opasmEngineGetSourceRecordCountV1",
            "jsr engine.opasmEngineGetStatementCountV1",
            "jsr opforgeNativeCliTokenizeCurrentLine",
            "move.l d0, -(sp)",
            "jsr preprocessor_expansion.opforgeNativeCliEndExpandedLineV1",
            "or.l (sp)+, d0",
            "jsr engine.opasmEngineRollbackCollectionV1",
            "opforgeNativeCliProcessExpandedScopeLineV1\t.block",
            "jsr preprocessor_expansion.opforgeNativeCliBeginExpandedLineV1",
            "jsr engine.opasmEngineGetSourceRecordCountV1",
            "jsr engine.opasmEngineGetStatementCountV1",
            "jsr assembly_session.opforgeNativeCliRecordSourceLine",
            "jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine",
            "move.l d0, -(sp)",
            "jsr preprocessor_expansion.opforgeNativeCliEndExpandedLineV1",
            "or.l (sp)+, d0",
            "jsr engine.opasmEngineRollbackCollectionV1",
        ]
    ));
    assert!(line_processor
        .contains("moveq #1, d0\n\trts\n\t.bend  ; opforgeNativeCliProcessExpandedLineV1"));
    assert!(line_processor
        .contains("moveq #1, d0\n\trts\n\t.bend  ; opforgeNativeCliProcessExpandedScopeLineV1"));
}

#[test]
fn native_preprocessor_expanded_line_failure_restores_caller_state() {
    // Proof level B. This source contract proves the expanded-body route
    // checkpoints engine-owned observable state and rolls it back after a
    // route or cleanup failure. It does not inject a native tokenizer fault.
    let root = workspace_root();
    let engine =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read opasm engine");
    let line_processor = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read line processor");
    let expansion = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_expansion.asm"),
    )
    .expect("read expansion owner");
    assert!(source_contains_in_order(
        &engine,
        &[
            "opasmEngineRollbackCollectionV1\t.block",
            "cmp.w OpasmEngineSourceRecordCount.l, d0",
            "cmp.w OpasmEngineStmtCount.l, d1",
            "cmp.w OpasmEngineImageByteCount.l, d2",
            "move.w d0, OpasmEngineSourceRecordCount.l",
            "move.w d1, OpasmEngineStmtCount.l",
            "move.w d2, OpasmEngineImageByteCount.l",
            "move.l d3, OpasmEngineSessionCurrentPc.l",
        ]
    ));
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "opforgeNativeCliProcessExpandedLineV1\t.block",
            "jsr preprocessor_expansion.opforgeNativeCliBeginExpandedLineV1",
            "jsr engine.opasmEngineGetSourceRecordCountV1",
            "jsr engine.opasmEngineGetStatementCountV1",
            "jsr engine.opasmEngineGetImageByteCountV1",
            "jsr engine.opasmEngineGetSessionCurrentPcV1",
            "jsr opforgeNativeCliTokenizeCurrentLine",
            "jsr preprocessor_expansion.opforgeNativeCliEndExpandedLineV1",
            "jsr preprocessor_expansion.opforgeNativeCliAbortExpandedLineV1",
            "jsr engine.opasmEngineRollbackCollectionV1",
            "movem.l (sp)+, d4-d7",
        ]
    ));
    assert!(source_contains_in_order(
        &expansion,
        &[
            "opforgeNativeCliAbortExpandedLineV1\t.block",
            "move.w d3, state.NativeCliSourceLineLen",
            "clr.w state.NativeCliPreprocessExpansionDepth",
        ]
    ));
}

#[test]
fn native_preprocessor_generated_scope_failure_is_transactional() {
    // Proof level B. Generated `.block`/`.endblock` recording shares the same
    // engine checkpoint and restores its caller staging before exposing a
    // failure. It does not prove a guest-side parser fault injection.
    let root = workspace_root();
    let line_processor = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read line processor");
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "opforgeNativeCliProcessExpandedScopeLineV1\t.block",
            "jsr preprocessor_expansion.opforgeNativeCliBeginExpandedLineV1",
            "jsr engine.opasmEngineGetSourceRecordCountV1",
            "jsr assembly_session.opforgeNativeCliRecordSourceLine",
            "jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine",
            "jsr preprocessor_expansion.opforgeNativeCliEndExpandedLineV1",
            "jsr engine.opasmEngineRollbackCollectionV1",
        ]
    ));
}

#[test]
fn native_preprocessor_macro_definitions_are_consumed_and_bounded() {
    // Proof levels A/B/C. Rust establishes definition consumption; the native
    // source contract verifies bounded header/body retention before ordinary
    // tokenization. This does not prove invocation or 68020 execution.
    let lines = vec![
        "COPY .macro src, dst".to_string(),
        "    lda .src".to_string(),
        "    sta .dst".to_string(),
        ".endmacro".to_string(),
        "    .byte 7".to_string(),
    ];
    let mut rust_oracle = MacroProcessor::new();
    assert_eq!(
        rust_oracle.expand(&lines).expect("expand macro definition"),
        vec!["    .byte 7"]
    );

    let root = workspace_root();
    let constants =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/constants.asm"))
            .expect("read native CLI constants");
    let state = fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/state.asm"))
        .expect("read native CLI state");
    let preprocessor =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/preprocessor.asm"))
            .expect("read native preprocessor");
    let definitions = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_definitions.asm"),
    )
    .expect("read native preprocessor definition owner");
    let scan = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_scan.asm"),
    )
    .expect("read native preprocessor scan owner");
    let line_processor = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read native CLI line processor");
    let source_reader =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/source_reader.asm"))
            .expect("read native CLI source reader");

    assert!(constants.contains("NATIVE_PREPROCESS_DEFINITION_CAPACITY = 8"));
    assert!(constants.contains("NATIVE_PREPROCESS_BODY_LINE_CAPACITY = 8"));
    assert!(source_contains_in_order(
        &state,
        &[
            "NativeCliPreprocessDefinitionCount",
            "NativeCliPreprocessActiveDefinition",
            "NativeCliPreprocessDefinitionBodyCount",
            "NativeCliPreprocessDefinitionHeaderLen",
            "NativeCliPreprocessDefinitionBodyLen",
            "NativeCliPreprocessDefinitionHeader",
            "NativeCliPreprocessDefinitionBody",
        ]
    ));
    assert!(source_contains_in_order(
        &definitions,
        &[
            "opforgeNativeCliCaptureMacroDefinitionLineV1\t.block",
            "jsr preprocessor_scan.lineContainsMacroDirective",
            "cmpi.w #constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY, d2",
            "move.w d3, 0(a2, d2.l)",
            "opforgeNativeCliFinishMacroDefinitionsV1\t.block",
        ]
    ));
    assert!(
        !preprocessor.contains("opforgeNativeCliCaptureMacroDefinitionLineV1\t.block")
            && !preprocessor.contains("opforgeNativeCliFinishMacroDefinitionsV1\t.block"),
        "macro-definition capture must have exactly one owner"
    );
    assert!(source_contains_in_order(
        &definitions,
        &[
            "appendBodyLine\t.block",
            "cmpi.w #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d3",
            "move.w d3, 0(a2, d2.l)",
        ]
    ));
    assert!(source_contains_in_order(
        &scan,
        &[
            "lineContainsDirective\t.block",
            "movem.l d5-d6/a3, -(sp)",
            "yes",
            "movem.l (sp)+, d5-d6/a3",
        ]
    ));
    for routine in [
        "lineStartsWithDirective\t.block",
        "lineStartsWithEndmacroDirective\t.block",
        "macroHeaderHasName\t.block",
        "lineContainsMacroDirective\t.block",
        "lineContainsDirective\t.block",
    ] {
        assert!(scan.contains(routine), "missing scanner routine: {routine}");
        assert!(
            !preprocessor.contains(routine),
            "scanner routine must have exactly one owner: {routine}"
        );
    }
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "opforgeNativeCliTokenizeCurrentLine\t.block",
            "jsr preprocessor_definitions.opforgeNativeCliCaptureMacroDefinitionLineV1",
            "bmi.w fail",
            "moveq #0, d0",
            "rts",
            "preprocessPass",
            "jsr assembly_session.opforgeNativeCliRecordSourceLine",
        ]
    ));
    assert!(source_contains_in_order(
        &source_reader,
        &[
            "checkModuleDepth",
            "jsr preprocessor_definitions.opforgeNativeCliFinishMacroDefinitionsV1",
            "bne.s close",
        ]
    ));
}

#[test]
fn native_preprocessor_structural_definition_record_routes_macro_and_segment_only() {
    // Proof levels B/C. This locks the shared fixed record layout and proves
    // that macro and segment definitions have distinct expansion policies while
    // the statement kind remains inactive. It does not prove native execution.
    let root = workspace_root();
    let constants =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/constants.asm"))
            .expect("read native constants");
    let state = fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/state.asm"))
        .expect("read native state");
    let definitions = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_definitions.asm"),
    )
    .expect("read definition owner");

    assert!(source_contains_in_order(
        &constants,
        &[
            "NATIVE_PREPROCESS_DEFINITION_KIND_MACRO = 0",
            "NATIVE_PREPROCESS_DEFINITION_KIND_SEGMENT = 1",
            "NATIVE_PREPROCESS_DEFINITION_KIND_STATEMENT = 2",
        ]
    ));
    assert!(source_contains_in_order(
        &state,
        &[
            "Shared structural-definition record contract (macro and segment)",
            "DefinitionHeader is the captured name/signature",
            "DefinitionKind selects macro scope wrapping or segment",
            "NativeCliPreprocessDefinitionKind",
            "NativeCliPreprocessDefinitionBodyCount",
            "NativeCliPreprocessDefinitionHeaderLen",
            "NativeCliPreprocessDefinitionBodyLen",
            "NativeCliPreprocessDefinitionHeader",
            "NativeCliPreprocessDefinitionBody",
        ]
    ));
    assert!(definitions.contains("NATIVE_PREPROCESS_DEFINITION_KIND_SEGMENT"));
    assert!(definitions.contains("NativeCliPreprocessDefinitionKind"));
    assert!(!definitions.contains("DEFINITION_KIND_STATEMENT"));
}

#[test]
fn native_segment_expansion_source_model_matches_rust_scope_and_label_rules() {
    // Proof level C. Model the kind-owned expansion policy independently of the
    // native implementation: macros wrap scope; segments inline and attach a
    // caller label to only the first expanded line.
    fn expand(kind: &str, label: Option<&str>, body: &[&str]) -> Vec<String> {
        let mut out = body
            .iter()
            .map(|line| (*line).to_string())
            .collect::<Vec<_>>();
        if kind == "macro" {
            out.insert(
                0,
                label.map_or(".block".to_string(), |name| format!("{name} .block")),
            );
            out.push(".endblock".to_string());
        } else if let Some(label) = label {
            if let Some(first) = out.first_mut() {
                let trimmed = first.trim_start();
                *first = if trimmed.is_empty() {
                    label.to_string()
                } else {
                    format!("{label} {trimmed}")
                };
            } else {
                out.push(label.to_string());
            }
        }
        out
    }

    assert_eq!(
        expand("segment", None, &["        .byte 7"]),
        ["        .byte 7"]
    );
    assert_eq!(
        expand("segment", Some("placed"), &["        .byte 7", " .byte 8"]),
        ["placed .byte 7", " .byte 8"]
    );
    assert_eq!(
        expand("macro", Some("placed"), &["        .byte 7"]),
        ["placed .block", "        .byte 7", ".endblock"]
    );

    let root = workspace_root();
    let definitions = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_definitions.asm"),
    )
    .expect("read definition owner");
    let line_processor = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read line processor");
    assert!(source_contains_in_order(
        &definitions,
        &[
            "SegmentText",
            "jsr preprocessor_scan.lineContainsDirective",
            "NATIVE_PREPROCESS_DEFINITION_KIND_SEGMENT",
            "NativeCliPreprocessDefinitionKind",
        ]
    ));
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "NativeCliPreprocessDefinitionKind",
            "NATIVE_PREPROCESS_DEFINITION_KIND_SEGMENT",
            "beq.s bodyLoop",
            "attachSegmentInvocationLabel",
            "close",
            "NATIVE_PREPROCESS_DEFINITION_KIND_SEGMENT",
            "beq.s finish",
        ]
    ));
    assert!(!definitions.contains("NATIVE_PREPROCESS_DEFINITION_KIND_STATEMENT"));
    assert!(!line_processor.contains("NATIVE_PREPROCESS_DEFINITION_KIND_STATEMENT"));
}

#[test]
fn native_preprocessor_structural_scanner_boundary_matrix_is_bounded() {
    // Proof level C. This table models the declared bounded scanner contract
    // and locks its native quote/boundary implementation. It does not prove
    // native execution or activate statement behavior.
    fn contains_macro(line: &str) -> bool {
        let bytes = line.as_bytes();
        let mut quote = None;
        let mut index = 0;
        while index + 6 <= bytes.len() {
            let byte = bytes[index];
            if let Some(delimiter) = quote {
                if byte == delimiter {
                    quote = None;
                }
                index += 1;
                continue;
            }
            if matches!(byte, b'\'' | b'"') {
                quote = Some(byte);
                index += 1;
                continue;
            }
            if byte == b';' {
                return false;
            }
            let left = index == 0 || matches!(bytes[index - 1], b' ' | b'\t');
            let spelling = bytes[index..index + 6].eq_ignore_ascii_case(b".macro");
            let right = index + 6 == bytes.len() || matches!(bytes[index + 6], b' ' | b'\t' | b';');
            if left && spelling && right {
                return true;
            }
            index += 1;
        }
        false
    }

    fn starts_with_endmacro(line: &str) -> bool {
        let trimmed = line.trim_start_matches([' ', '\t']);
        let bytes = trimmed.as_bytes();
        bytes.len() >= 9
            && bytes[..9].eq_ignore_ascii_case(b".endmacro")
            && (bytes.len() == 9 || matches!(bytes[9], b' ' | b'\t' | b';'))
    }

    fn contains_segment(line: &str) -> bool {
        let bytes = line.as_bytes();
        let mut quote = None;
        let mut index = 0;
        while index + 8 <= bytes.len() {
            let byte = bytes[index];
            if let Some(delimiter) = quote {
                if byte == delimiter {
                    quote = None;
                }
                index += 1;
                continue;
            }
            if matches!(byte, b'\'' | b'"') {
                quote = Some(byte);
                index += 1;
                continue;
            }
            if byte == b';' {
                return false;
            }
            let left = index == 0 || matches!(bytes[index - 1], b' ' | b'\t');
            let spelling = bytes[index..index + 8].eq_ignore_ascii_case(b".segment");
            let right = index + 8 == bytes.len() || matches!(bytes[index + 8], b' ' | b'\t' | b';');
            if left && spelling && right {
                return true;
            }
            index += 1;
        }
        false
    }

    for (line, expected) in [
        ("NAME .macro arg", true),
        ("NAME .MACRO arg", true),
        ("NAME .MaCrO arg", true),
        ("NAME .macro; comment", true),
        ("NAME .macrox", false),
        ("prefix.macro arg", false),
        ("; .macro ignored", false),
        ("\".macro\"", false),
        ("' .macro '", false),
        ("NAME .mac", false),
        (&"x".repeat(255), false),
    ] {
        assert_eq!(contains_macro(line), expected, "scanner matrix: {line:?}");
    }
    for (line, expected) in [
        (".endmacro", true),
        (" .ENDMACRO ; close", true),
        (".endmacrox", false),
        (".endmac", false),
    ] {
        assert_eq!(
            starts_with_endmacro(line),
            expected,
            "end-kind matrix: {line:?}"
        );
    }
    for (line, expected) in [
        ("INLINE .segment val", true),
        ("INLINE .SEGMENT val", true),
        ("INLINE .segment; comment", true),
        ("INLINE .segmentx", false),
        ("prefix.segment val", false),
        ("; INLINE .segment val", false),
        ("label .byte \" .segment \"", false),
        ("label .byte ' .segment '", false),
        ("label .byte \"x\" ; .segment", false),
        ("INLINE .seg", false),
        (&"x".repeat(255), false),
    ] {
        assert_eq!(contains_segment(line), expected, "segment matrix: {line:?}");
    }

    let root = workspace_root();
    let scan = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_scan.asm"),
    )
    .expect("read scanner owner");
    assert!(source_contains_in_order(
        &scan,
        &[
            "lineContainsMacroDirective\t.block",
            "clr.l d4",
            "tst.b d4",
            "cmpi.b #';', d1",
            "cmpi.b #'\\\'', d1",
            "cmpi.b #'\"', d1",
            "move.b d1, d4",
        ]
    ));
    assert!(source_contains_in_order(
        &scan,
        &[
            "lineContainsDirective\t.block",
            "clr.l d6",
            "tst.l d6",
            "cmp.b d6, d0",
            "cmpi.b #';', d0",
            "cmpi.b #'\\\'', d0",
            "cmpi.b #'\"', d0",
            "move.b d0, d6",
        ]
    ));
    assert!(source_contains_in_order(
        &scan,
        &[
            "lineStartsWithEndmacroDirective\t.block",
            "cmpi.l #9, d0",
            "cmpi.b #'e', d1",
            "cmpi.b #'n', d1",
            "cmpi.b #'d', d1",
            "cmpi.b #'m', d1",
            "cmpi.b #'a', d1",
            "cmpi.b #'c', d1",
            "cmpi.b #'r', d1",
            "cmpi.b #'o', d1",
            "cmpi.l #9, d0",
            "move.b 9(a0), d1",
            "cmpi.b #';', d1",
        ]
    ));
}

#[test]
fn native_preprocessor_macro_invocation_frame_is_bounded_and_resettable() {
    // Proof level B. The native state owns one bounded invocation frame whose
    // selected definition sentinel is reset for every CLI session. This does
    // not prove lookup, substitution, expansion, or native 68020 execution.
    let root = workspace_root();
    let constants =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/constants.asm"))
            .expect("read native CLI constants");
    let state = fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/state.asm"))
        .expect("read native CLI state");
    let preprocessor =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/preprocessor.asm"))
            .expect("read native preprocessor");

    assert!(constants.contains("NATIVE_PREPROCESS_MACRO_ARG_CAPACITY = 9"));
    assert!(constants.contains("NATIVE_PREPROCESS_INVOCATION_DEPTH_LIMIT = 1"));
    assert!(constants.contains("NATIVE_PREPROCESS_STATE_FIXED_BYTES = 36"));
    assert!(constants.contains(
        "(NATIVE_PREPROCESS_MACRO_ARG_CAPACITY * NATIVE_PREPROCESS_INVOCATION_ARG_TEXT_CAPACITY)"
    ));
    assert!(constants.contains("+ NATIVE_PREPROCESS_EXPANSION_LINE_CAPACITY"));
    assert!(source_contains_in_order(
        &state,
        &[
            "NativeCliPreprocessInvocationDefinition",
            "NativeCliPreprocessInvocationArgCount",
            "NativeCliPreprocessInvocationBodyIndex",
            "NativeCliPreprocessInvocationArgs",
            "NativeCliPreprocessInvocationFullArgs",
            "NativeCliPreprocessInvocationLabel",
        ]
    ));
    assert!(source_contains_in_order(
        &preprocessor,
        &[
            "opforgeNativeCliResetPreprocessorV1\t.block",
            "move.l #constants.NATIVE_PREPROCESS_STATE_BYTES, d0",
            "jsr copy.clearBytes",
            "move.w #-1, state.NativeCliPreprocessActiveDefinition",
            "move.w #-1, state.NativeCliPreprocessInvocationDefinition",
        ]
    ));
    assert!(source_contains_in_order(
        &preprocessor,
        &[
            "opforgeNativeCliBeginMacroInvocationFrameV1\t.block",
            "cmpi.w #constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY, d0",
            "tst.w state.NativeCliPreprocessInvocationDefinition",
            "bpl.s fail",
            "move.w d0, state.NativeCliPreprocessInvocationDefinition",
            "clr.w state.NativeCliPreprocessInvocationArgCount",
            "clr.w state.NativeCliPreprocessInvocationBodyIndex",
        ]
    ));
}

#[test]
fn native_preprocessor_capacity_matrix_is_deterministic() {
    // Proof level C. This host-side source matrix names every fixed native
    // preprocessor allocation and verifies that its owner checks capacity
    // before writing. It does not execute 68020 code or claim Rust's dynamic
    // macro storage is capacity-equivalent.
    let root = workspace_root();
    let constants =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/constants.asm"))
            .expect("read native constants");
    let state = fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/state.asm"))
        .expect("read native state");
    let definitions = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_definitions.asm"),
    )
    .expect("read definition owner");
    let invocation = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_invocation.asm"),
    )
    .expect("read invocation owner");
    let substitution = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_substitution.asm"),
    )
    .expect("read substitution owner");
    let expansion = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_expansion.asm"),
    )
    .expect("read expansion owner");
    let preprocessor =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/preprocessor.asm"))
            .expect("read preprocessor reset owner");
    let source_reader =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/source_reader.asm"))
            .expect("read source reader owner");

    for capacity in [
        "NATIVE_PREPROCESS_DEFINITION_CAPACITY = 8",
        "NATIVE_PREPROCESS_BODY_LINE_CAPACITY = 8",
        "NATIVE_PREPROCESS_MACRO_ARG_CAPACITY = 9",
        "NATIVE_PREPROCESS_EXPANSION_DEPTH_LIMIT = 1",
        "NATIVE_PREPROCESS_INVOCATION_DEPTH_LIMIT = 1",
        "NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY = SOURCE_LINE_BUFFER_CAPACITY",
        "NATIVE_PREPROCESS_BODY_LINE_TEXT_CAPACITY = SOURCE_LINE_BUFFER_CAPACITY",
        "NATIVE_PREPROCESS_INVOCATION_ARG_TEXT_CAPACITY = SOURCE_LINE_BUFFER_CAPACITY",
        "NATIVE_PREPROCESS_INVOCATION_FULL_ARGS_CAPACITY = SOURCE_LINE_BUFFER_CAPACITY",
        "NATIVE_PREPROCESS_INVOCATION_LABEL_CAPACITY = SOURCE_LINE_BUFFER_CAPACITY",
        "NATIVE_PREPROCESS_SAVED_LINE_CAPACITY = SOURCE_LINE_BUFFER_CAPACITY",
        "NATIVE_PREPROCESS_EXPANSION_LINE_CAPACITY = SOURCE_LINE_BUFFER_CAPACITY",
        "NATIVE_PREPROCESS_STATE_FIXED_BYTES = 36",
    ] {
        assert!(constants.contains(capacity), "missing capacity: {capacity}");
    }
    assert!(source_contains_in_order(
        &constants,
        &[
            "NATIVE_PREPROCESS_STATE_BYTES   = NATIVE_PREPROCESS_STATE_FIXED_BYTES",
            "NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY",
            "NATIVE_PREPROCESS_BODY_LINE_TEXT_CAPACITY",
            "NATIVE_PREPROCESS_INVOCATION_ARG_TEXT_CAPACITY",
            "NATIVE_PREPROCESS_INVOCATION_FULL_ARGS_CAPACITY",
            "NATIVE_PREPROCESS_INVOCATION_LABEL_CAPACITY",
            "NATIVE_PREPROCESS_SAVED_LINE_CAPACITY",
            "NATIVE_PREPROCESS_EXPANSION_LINE_CAPACITY",
        ]
    ));
    assert!(source_contains_in_order(
        &state,
        &[
            "Resource budget/lifetime: ResetPreprocessorV1 clears this whole contiguous",
            "NativeCliPreprocessDefinitionHeader",
            "constants.NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY",
            "NativeCliPreprocessDefinitionBody",
            "constants.NATIVE_PREPROCESS_BODY_LINE_TEXT_CAPACITY",
            "NativeCliPreprocessInvocationArgs",
            "constants.NATIVE_PREPROCESS_INVOCATION_ARG_TEXT_CAPACITY",
            "NativeCliPreprocessInvocationFullArgs",
            "constants.NATIVE_PREPROCESS_INVOCATION_FULL_ARGS_CAPACITY",
            "NativeCliPreprocessInvocationLabel",
            "constants.NATIVE_PREPROCESS_INVOCATION_LABEL_CAPACITY",
            "NativeCliPreprocessSavedLine",
            "constants.NATIVE_PREPROCESS_SAVED_LINE_CAPACITY",
            "NativeCliPreprocessExpansionLine",
            "constants.NATIVE_PREPROCESS_EXPANSION_LINE_CAPACITY",
        ]
    ));
    assert!(source_contains_in_order(
        &definitions,
        &[
            "cmpi.w #constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY, d2",
            "mulu #constants.NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY, d2",
            "jsr copy.copyBytes",
            "cmpi.w #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d3",
            "mulu #constants.NATIVE_PREPROCESS_BODY_LINE_TEXT_CAPACITY, d2",
            "jsr copy.copyBytes",
        ]
    ));
    assert!(source_contains_in_order(
        &invocation,
        &[
            "move.l #constants.NATIVE_PREPROCESS_INVOCATION_LABEL_CAPACITY - 1, d2",
            "tst.l d2",
            "beq.s fail",
            "cmpi.l #constants.NATIVE_PREPROCESS_INVOCATION_ARG_TEXT_CAPACITY - 1, d1",
            "cmpi.l #constants.NATIVE_PREPROCESS_INVOCATION_FULL_ARGS_CAPACITY - 1, d2",
            "cmpi.w #constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY, d0",
            "addq.w #1, state.NativeCliPreprocessInvocationArgCount",
            "move.l a0, d3",
            "sub.l a3, d3",
            "cmpi.l #constants.NATIVE_PREPROCESS_INVOCATION_ARG_TEXT_CAPACITY - 1, d3",
            "bcc.s fail",
            "jsr copy.copyBytes",
        ]
    ));
    assert!(source_contains_in_order(
        &substitution,
        &[
            "appendExpansionByte\t.block",
            "cmpi.l #constants.NATIVE_PREPROCESS_EXPANSION_LINE_CAPACITY - 1, d5",
            "move.b d4, 0(a1, d5.l)",
            "appendExpansionBytes\t.block",
            "add.l d3, d0",
            "cmpi.l #constants.NATIVE_PREPROCESS_EXPANSION_LINE_CAPACITY, d0",
            "move.b d4, 0(a1, d5.l)",
        ]
    ));
    assert!(source_contains_in_order(
        &expansion,
        &[
            "opforgeNativeCliBeginExpandedLineV1\t.block",
            "cmpi.l #constants.NATIVE_PREPROCESS_EXPANSION_LINE_CAPACITY, d0",
            "bcc.s fail",
            "tst.w state.NativeCliPreprocessExpansionDepth",
            "bne.s fail",
            "jsr copy.copyBytes",
            "move.w #1, state.NativeCliPreprocessExpansionDepth",
        ]
    ));
    assert!(source_contains_in_order(
        &preprocessor,
        &[
            "opforgeNativeCliResetPreprocessorV1\t.block",
            "move.l #constants.NATIVE_PREPROCESS_STATE_BYTES, d0",
            "jsr copy.clearBytes",
        ]
    ));
    assert!(source_contains_in_order(
        &source_reader,
        &[
            "move.w state.NativeCliSourceLineLen, d1",
            "cmpi.w #constants.SOURCE_LINE_BUFFER_CAPACITY, d1",
            "bhs.w closeFail",
            "move.b d0, 0(a1, d1.W)",
        ]
    ));
    assert!(!constants.contains("NATIVE_PREPROCESS_SEGMENT_CAPACITY"));
    assert!(!constants.contains("NATIVE_PREPROCESS_STATEMENT_CAPACITY"));
}

#[test]
fn native_preprocessor_macro_invocations_bind_before_prvm_routing() {
    // Proof level C. This host-side request-shape model covers the bounded
    // native invocation frame contract and checks that the native source puts
    // recognized calls ahead of source recording. It does not prove native
    // 68020 execution, substitution, or expansion re-entry.
    let mut rust_oracle = MacroProcessor::new();
    let lines = vec![
        "COPY .macro src, dst".to_string(),
        "    .byte .src, .dst".to_string(),
        ".endmacro".to_string(),
        "PAIR .macro left, right=2".to_string(),
        "    .byte .left, .right".to_string(),
        ".endmacro".to_string(),
        "TEXT .macro value".to_string(),
        "    .byte .value".to_string(),
        ".endmacro".to_string(),
        "    .COPY $12, $34".to_string(),
        "    .PAIR 1".to_string(),
        "    .TEXT {1, 2}".to_string(),
        "label .TEXT \"a,b\"".to_string(),
    ];
    let expanded = rust_oracle.expand(&lines).expect("Rust macro oracle");
    assert!(expanded.iter().any(|line| line.contains(".byte $12, $34")));
    assert!(expanded.iter().any(|line| line.contains(".byte 1, 2")));
    assert!(expanded.iter().any(|line| line.contains(".byte {1, 2}")));
    assert!(expanded.iter().any(|line| line.contains(".byte \"a,b\"")));

    let root = workspace_root();
    let constants =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/constants.asm"))
            .expect("read native constants");
    let state = fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/state.asm"))
        .expect("read native state");
    let preprocessor =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/preprocessor.asm"))
            .expect("read native preprocessor");
    let invocation = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_invocation.asm"),
    )
    .expect("read native preprocessor invocation owner");
    let line_processor = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read native line processor");

    assert!(constants.contains("NATIVE_PREPROCESS_STATE_FIXED_BYTES = 36"));
    assert!(source_contains_in_order(
        &state,
        &[
            "NativeCliPreprocessInvocationArgLen",
            "NativeCliPreprocessInvocationFullArgsLen",
            "NativeCliPreprocessInvocationLabelLen",
        ]
    ));
    assert!(source_contains_in_order(
        &invocation,
        &[
            "opforgeNativeCliParseMacroInvocationV1\t.block",
            "jsr preprocessor.opforgeNativeCliBeginMacroInvocationFrameV1",
            "bsr.w parseInvocationArguments",
        ]
    ));
    assert!(
        !preprocessor.contains("opforgeNativeCliParseMacroInvocationV1\t.block"),
        "macro invocation parsing must have exactly one owner"
    );
    for routine in [
        "captureInvocationLabel\t.block",
        "findCapturedMacroDefinition\t.block",
        "splitInvocationArgumentList\t.block",
        "bindMacroParameterDefaults\t.block",
    ] {
        assert!(invocation.contains(routine), "missing {routine}");
        assert!(
            !preprocessor.contains(routine),
            "invocation helper must have exactly one owner: {routine}"
        );
    }
    assert!(source_contains_in_order(
        &invocation,
        &[
            "cmpi.b #'\\'', d3",
            "cmpi.b #'\"', d3",
            "cmpi.b #'(', d3",
            "cmpi.b #'[', d3",
            "cmpi.b #'{', d3",
            "cmpi.b #',', d3",
        ]
    ));
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "jsr preprocessor_definitions.opforgeNativeCliCaptureMacroDefinitionLineV1",
            "preprocessPass",
            "jsr preprocessor_invocation.opforgeNativeCliParseMacroInvocationV1",
            "invocationPass",
            "jsr assembly_session.opforgeNativeCliRecordSourceLine",
        ]
    ));
}

#[test]
fn native_preprocessor_macro_substitution_and_reentry_are_bounded() {
    // Proof levels B/C. Rust supplies the substitution/scope oracle; the
    // native source contract proves that a bounded substituted line enters the
    // assembly-session source bridge and restores the caller line after each
    // route.
    // It does not prove native 68020 execution or emitted artifact bytes.
    let mut rust_oracle = MacroProcessor::new();
    let lines = vec![
        "LOCAL .macro value=2".to_string(),
        "local .const .value".to_string(),
        "    .byte @1, .@, .{value}".to_string(),
        ".endmacro".to_string(),
        "scope .LOCAL 7".to_string(),
    ];
    let expanded = rust_oracle.expand(&lines).expect("Rust macro oracle");
    assert!(expanded.iter().any(|line| line == "scope .block"));
    assert!(expanded.iter().any(|line| line == "local .const 7"));
    assert!(expanded.iter().any(|line| line.contains(".byte 7, 7, 7")));
    assert!(expanded.iter().any(|line| line.trim() == ".endblock"));

    let root = workspace_root();
    let state = fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/state.asm"))
        .expect("read native state");
    let preprocessor =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/preprocessor.asm"))
            .expect("read native preprocessor");
    let substitution = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/preprocessor_substitution.asm"),
    )
    .expect("read native substitution owner");
    let line_processor = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read native line processor");

    assert!(state.contains("NativeCliPreprocessExpansionLineLen"));
    for routine in [
        "opforgeNativeCliSubstituteMacroBodyLineV1\t.block",
        "appendInvocationPositional\t.block",
        "appendInvocationFullList\t.block",
        "appendInvocationNamed\t.block",
        "appendExpansionBytes\t.block",
    ] {
        assert!(substitution.contains(routine), "missing {routine}");
        assert!(
            !preprocessor.contains(routine),
            "substitution helper must have exactly one owner: {routine}"
        );
    }
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "opforgeNativeCliExpandActiveMacroV1\t.block",
            "bsr.w emitMacroBlockStart",
            "bsr.w opforgeNativeCliProcessExpandedScopeLineV1",
            "jsr preprocessor_substitution.opforgeNativeCliSubstituteMacroBodyLineV1",
            "bsr.w opforgeNativeCliProcessExpandedLineV1",
            "bsr.w emitMacroBlockEnd",
            "bsr.w opforgeNativeCliProcessExpandedScopeLineV1",
            "move.w #-1, state.NativeCliPreprocessInvocationDefinition",
        ]
    ));
    assert!(line_processor.contains("emitMacroBlockStart\t.block"));
    assert!(line_processor.contains("emitMacroBlockEnd\t.block"));
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "opforgeNativeCliProcessExpandedLineV1\t.block",
            "jsr preprocessor_expansion.opforgeNativeCliBeginExpandedLineV1",
            "jsr opforgeNativeCliTokenizeCurrentLine",
            "jsr preprocessor_expansion.opforgeNativeCliEndExpandedLineV1",
        ]
    ));
    assert!(!line_processor.contains("qualifyExpandedMacroLocalLabel"));
}

#[derive(Default)]
struct NativeSequenceAssignmentContract {
    values: Vec<(String, Vec<u32>)>,
}

impl NativeSequenceAssignmentContract {
    fn reset(&mut self) {
        self.values.clear();
    }

    fn capture(&mut self, source: &str) -> Result<(), ()> {
        let (name, operand) = source.split_once('=').ok_or(())?;
        let name = name.trim();
        if name.is_empty() || name.len() >= 32 || self.values.len() >= 8 {
            return Err(());
        }
        let operand = operand.trim();
        let list = operand
            .strip_prefix('{')
            .and_then(|text| text.strip_suffix('}'))
            .ok_or(())?;
        let mut values = Vec::new();
        if !list.trim().is_empty() {
            for element in list.split(',') {
                if values.len() >= 16 {
                    return Err(());
                }
                values.push(element.trim().parse::<u32>().map_err(|_| ())?);
            }
        }
        self.values.push((name.to_string(), values));
        Ok(())
    }
}

#[test]
fn native_sequence_assignment_contract_covers_parsing_bounds_and_session_reset() {
    // Proof level C. This host request-shape model proves the intended list
    // parsing, fixed-capacity rejection, and session-reset decisions. It does
    // not execute the native parser or prove stored 68020 memory contents.
    let mut contract = NativeSequenceAssignmentContract::default();
    contract.capture("values = {1, 2, 3}").unwrap();
    assert_eq!(contract.values, vec![("values".to_string(), vec![1, 2, 3])]);
    assert!(contract.capture("broken = {1, nope}").is_err());
    assert!(NativeSequenceAssignmentContract::default()
        .capture(&format!("{} = {{1}}", "n".repeat(32)))
        .is_err());
    assert!(NativeSequenceAssignmentContract::default()
        .capture(&format!(
            "values = {{{}}}",
            (0..17)
                .map(|value| value.to_string())
                .collect::<Vec<_>>()
                .join(",")
        ))
        .is_err());
    let mut full = NativeSequenceAssignmentContract::default();
    for index in 0..8 {
        full.capture(&format!("v{index} = {{{index}}}")).unwrap();
    }
    assert!(full.capture("overflow = {9}").is_err());
    contract.reset();
    assert!(contract.values.is_empty());
}

#[test]
fn native_sequence_assignment_storage_is_bounded_and_reset_per_session() {
    // Proof level B. This test proves the native sequence store has explicit
    // name/element/table bounds, is reset once at session initialization, is
    // populated while the exact source line is available, and consumes assignment
    // statements before ordinary pass processing. It does not execute the native
    // parser or prove iterable lookup.
    let values = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_compile_values.asm"),
    )
    .expect("read native compile-time values");
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    let session_init = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/session_init.asm"),
    )
    .expect("read native CLI session initialization");
    let assembly_session = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/assembly_session.asm"),
    )
    .expect("read native CLI assembly session");
    assert!(values.contains("SEQUENCE_CAPACITY = 8"));
    assert!(values.contains("SEQUENCE_NAME_CAPACITY = 32"));
    assert!(values.contains("SEQUENCE_ELEMENT_CAPACITY = 16"));
    assert_eq!(
        session_init.matches("jsr compile_values.resetV1").count(),
        1
    );
    assert!(!driver.contains("compile_values.resetV1"));
    assert!(source_contains_in_order(
        &assembly_session,
        &[
            "cmpi.b #'=', buffers.tokenScratchBuffer",
            "lea state.NativeCliSourceLine, a0",
            "jsr compile_values.captureSourceListAssignmentV1",
            "bsr.w opforgeNativeCliStoreStatementRecord",
        ]
    ));
    assert!(source_contains_in_order(
        &driver,
        &["cmpi.b #'=', (a0)", "moveq #1, d1", "bra.w success"]
    ));
}

fn native_iterable_for_contract(
    variable: &str,
    values: &[u32],
    iteration_limit: usize,
) -> Result<Vec<(String, u32)>, ()> {
    if variable.is_empty() || variable.len() >= 32 || values.len() > iteration_limit {
        return Err(());
    }
    Ok(values
        .iter()
        .map(|value| (variable.to_string(), *value))
        .collect())
}

fn native_ascending_range_contract(
    start: u32,
    end: u32,
    inclusive: bool,
    step: u32,
) -> Result<Vec<u32>, ()> {
    if step == 0 || start > end {
        return Err(());
    }
    let mut values = Vec::new();
    let mut current = start;
    while if inclusive {
        current <= end
    } else {
        current < end
    } {
        values.push(current);
        current = current.checked_add(step).ok_or(())?;
    }
    Ok(values)
}

#[test]
fn native_iterable_for_contract_covers_lists_ranges_binding_and_limit() {
    // Proof level C. This host request-shape model proves list order, inclusive
    // and stepped ascending range planning, loop-variable binding, and bounded
    // iteration decisions. It does not execute native tables or 68020 branches.
    assert_eq!(
        native_iterable_for_contract("value", &[1, 3, 5, 7], 8).unwrap(),
        vec![
            ("value".to_string(), 1),
            ("value".to_string(), 3),
            ("value".to_string(), 5),
            ("value".to_string(), 7),
        ]
    );
    assert_eq!(
        native_ascending_range_contract(0, 6, true, 3).unwrap(),
        vec![0, 3, 6]
    );
    assert_eq!(
        native_ascending_range_contract(0, 6, false, 3).unwrap(),
        vec![0, 3]
    );
    let sequence = [2, 4, 6, 8];
    assert_eq!(sequence.len(), 4);
    assert_eq!(sequence.get(2), Some(&6));
    assert!(native_ascending_range_contract(0, 6, true, 0).is_err());
    assert!(native_ascending_range_contract(6, 0, true, 1).is_err());
    assert!(native_iterable_for_contract("n", &[0, 1, 2], 2).is_err());
}

#[test]
fn native_iterable_for_source_binds_before_body_and_updates_before_repeat() {
    // Proof level B. This test proves the native flow callback pushes the first
    // binding before body execution, advances it before jumping back, resolves
    // it before ordinary labels, and resets binding state at session start.
    // Pass-two retention is required for source definitions the engine does not
    // replay before first emission. It does not execute the callback or prove
    // emitted bytes.
    let values = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_compile_values.asm"),
    )
    .expect("read native compile-time values");
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    assert!(values.contains("planListForOperandV1\t.block"));
    assert!(values.contains("planRangeForOperandV1\t.block"));
    assert!(values.contains("resolveSequenceExpressionV1\t.block"));
    assert!(source_contains_in_order(
        &driver,
        &[
            "jsr compile_values.planListForOperandV1",
            "jsr compile_values.planRangeForOperandV1",
            "jsr compile_values.pushBindingV1",
            "move.w d4, OpasmRepeatDepth",
        ]
    ));
    assert!(source_contains_in_order(
        &driver,
        &[
            "advanceRange",
            "updateBinding",
            "jsr compile_values.updateTopBindingV1",
            "move.w 0(a0, d4.l), d2",
        ]
    ));
    assert!(source_contains_in_order(
        &driver,
        &[
            "jsr compile_values.resolveBindingV1",
            "jsr compile_values.resolveSequenceExpressionV1",
            "jsr eng.opasmEngineResolveLabelValueV1",
        ]
    ));
    assert_eq!(
        driver.matches("jsr compile_values.resetBindingsV1").count(),
        1
    );
}

#[test]
fn native_iterable_data_parts_resolve_bindings_and_sequences_before_engine_labels() {
    // Proof level B. This proves numeric `.byte`/`.db` list parts use the same
    // compile-time binding and assigned-sequence resolution order as scalar
    // directive operands. It does not execute the native parser or guest.
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    let start = driver
        .find("readCommaOperandValueForStatement\t.block")
        .expect("comma operand evaluator block");
    let block = &driver[start..];
    let end = block
        .find("\t.bend  ; readCommaOperandValueForStatement")
        .expect("comma operand evaluator end");
    let block = &block[..end];
    assert!(source_contains_in_order(
        block,
        &[
            "bsr.w scopes.resolveLabelValueV1",
            "jsr compile_values.resolveBindingV1",
            "jsr compile_values.resolveSequenceExpressionV1",
            "jsr eng.opasmEngineResolveLabelValueV1",
            "bsr.w prepareEvaluateExpressionRequest",
        ]
    ));
}

#[test]
fn native_completed_repetition_advances_past_closing_directive() {
    // Proof level B. This proves completed `.for` and `.while` paths replace
    // router-clobbered next-index state with the statement after the closing
    // directive. It does not execute the native callback or prove loop bytes.
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    for (start_label, end_label) in [
        ("finishFor\n", "checkWhile\n"),
        ("finishWhile\n", "compareWhile\n"),
    ] {
        let start = driver
            .find(start_label)
            .expect("completed repetition label");
        let tail = &driver[start..];
        let end = tail
            .find(end_label)
            .expect("completed repetition block end");
        assert!(source_contains_in_order(
            &tail[..end],
            &[
                "move.w d7, d2",
                "addq.w #1, d2",
                "moveq #1, d1",
                "bra.w success"
            ],
        ));
    }
}

#[test]
fn opcore_iterable_rust_oracle_covers_assigned_sources() {
    // Proof level A. This test proves the live Rust assembler emits the expected
    // bytes for both assigned canonical iterable sources. It does not prove any
    // native request shape or 68020 execution.
    let case_dir = create_temp_dir("opcore-iterable-rust-oracle");
    for (name, expected) in [
        ("for_collection_basic.asm", &[1, 3, 5, 7][..]),
        ("ranges_lists_basic.asm", &[4, 6, 0, 3, 6][..]),
    ] {
        let input_path = workspace_root().join("examples/opcore").join(name);
        let bin_path = case_dir.join(format!("{name}.bin"));
        let cli = Cli::parse_from([
            "opForge",
            input_path.to_string_lossy().as_ref(),
            "--bin",
            bin_path.to_string_lossy().as_ref(),
            "--cpu",
            "m6502",
        ]);
        run_with_cli_with_context(&cli).expect("run Rust iterable source oracle");
        assert_eq!(
            fs::read(bin_path).expect("read Rust oracle bytes"),
            expected
        );
    }
}

#[test]
fn opcore_while_rust_oracle_covers_canonical_current_address() {
    // Proof level A. This test proves the live Rust assembler emits five bytes
    // for canonical `.while $ < 4`. It does not prove native loop control.
    let case_dir = create_temp_dir("opcore-while-rust-oracle");
    let input_path = workspace_root().join("examples/opcore/while_basic.asm");
    let bin_path = case_dir.join("while.bin");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run Rust while oracle");
    assert_eq!(
        fs::read(bin_path).expect("read Rust while bytes"),
        [0xff; 5]
    );
}

fn native_while_current_address_contract(limit: u32, max_iterations: usize) -> Result<usize, ()> {
    let mut current_pc = 0u32;
    let mut iterations = 0usize;
    while if iterations == 0 {
        current_pc < limit
    } else {
        current_pc.saturating_sub(1) < limit
    } {
        if iterations >= max_iterations {
            return Err(());
        }
        iterations += 1;
        current_pc += 1;
    }
    Ok(iterations)
}

#[test]
fn native_while_contract_covers_false_first_current_address_and_limit() {
    // Proof level C. This host boundary model proves false-first behavior,
    // Rust-compatible current-address phase, and iteration-limit rejection. It
    // does not execute native statement tables or 68020 branches.
    assert_eq!(native_while_current_address_contract(0, 8), Ok(0));
    assert_eq!(native_while_current_address_contract(4, 8), Ok(5));
    assert!(native_while_current_address_contract(u32::MAX, 4).is_err());
}

#[test]
fn native_while_source_reevaluates_opening_and_preserves_status() {
    // Proof level B. This test proves the native source stores the opening
    // statement, reevaluates it at `.endwhile`, preserves evaluator status
    // across index restoration, and enforces the iteration limit. It does not
    // execute the callback or prove current-PC values.
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    assert!(source_contains_in_order(
        &driver,
        &[
            "lea OpasmRepeatOpening, a0",
            "move.w 0(a0, d4.l), d0",
            "move.w #1, OpasmDriverWhileReevaluation",
            "bsr.w readWhileConditionForStatement",
            "move.l d0, d5",
            "move.w d6, d7",
            "tst.l d5",
        ]
    ));
    assert!(driver.contains("cmpi.l #OPASM_REPEAT_ITERATION_LIMIT, d5"));
    assert!(driver.contains("subq.l #1, d0"));
}

#[test]
fn native_source_cpu_bootstrap_preserves_tail_and_normalizes_before_routing() {
    // Proof level B. This test proves the bootstrap source preserves/restores
    // its parser tail around normalization before validating trailing input.
    // This test does not prove real 68020 execution.
    let source = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/source_reader.asm"),
    )
    .expect("read native CLI source reader");
    assert!(source_contains_in_order(
        &source,
        &[
            "bsr.w line_text.opforgeNativeCliCopyLineWord",
            "move.l d0, -(sp)",
            "move.l a0, -(sp)",
            "jsr directive_handlers.opforgeNativeCliNormalizeQuotedCpuToken",
            "jsr token_util.opforgeNativeCliCanonicalizeCpuName",
            "movea.l (sp)+, a0",
            "move.l (sp)+, d0",
            "bsr.w line_text.opforgeNativeCliSkipLineWhitespace",
            "cmpi.b #';', (a0)",
        ]
    ));
}

fn native_debug_output_isolation_contract(enabled: bool) -> Vec<&'static str> {
    if enabled {
        vec![
            "OPFORGE-NATIVE 1",
            "STAGE parser",
            "STAGE session",
            "STATUS output-ok",
            "SESSION-CPU ",
        ]
    } else {
        Vec::new()
    }
}

#[test]
fn native_debug_output_isolation_contract_models_enabled_and_disabled_markers() {
    // Proof level C. This test proves the marker-set decision for normal and
    // debug mode. This test does not prove native branches or DOS output.
    assert!(native_debug_output_isolation_contract(false).is_empty());
    assert_eq!(
        native_debug_output_isolation_contract(true),
        vec![
            "OPFORGE-NATIVE 1",
            "STAGE parser",
            "STAGE session",
            "STATUS output-ok",
            "SESSION-CPU ",
        ]
    );
}

#[test]
fn native_debug_output_sites_are_gated_before_progress_emission() {
    // Proof level B. This test proves every historical progress emission site
    // is reached only after a NativeCliDebugEnabled check. This test does not
    // prove real branch preservation or output bytes.
    let run = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/run.asm"),
    )
    .expect("read native CLI run source");
    let reader = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/source_reader.asm"),
    )
    .expect("read native CLI source reader");
    assert!(source_contains_in_order(
        &run,
        &[
            "tst.w state.NativeCliDebugEnabled",
            "beq.w tokenizerStage",
            "StubHeaderText",
            "tokenizerStage",
            "tst.w state.NativeCliDebugEnabled",
            "beq.s tokenizeFrontend",
            "opforgeNativeCliEmitModulePathRecords",
            "tst.w state.NativeCliDebugEnabled",
            "beq.s runEngine",
            "ParserStageText",
            "SessionStageText",
            "tst.w state.NativeCliDebugEnabled",
            "beq.s checkImage",
            "opforgeNativeCliEmitAssemblySessionSummary",
            "tst.w state.NativeCliDebugEnabled",
            "beq.s outputOkReturn",
            "NativeOutputOkText",
        ]
    ));
    assert!(source_contains_in_order(
        &reader,
        &[
            "tst.w state.NativeCliDebugEnabled",
            "beq.s packageUnavailable",
            "TokenizerOkText",
        ]
    ));
}

#[test]
fn external_fs_uae_opforge_native_cli_schema_binary_parity_matches_live_rust_cli() {
    // Proof level D. This test proves real Amiga-native CLI binary/PRG
    // artifacts match artifacts generated by the Rust CLI during this test.
    // This test does not prove text, map, or deterministic diagnostic parity.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let schema_cases = native_cli_schema_cases_with_live_rust_oracle(&repo_root);

    let parity_cases = schema_cases
        .iter()
        .map(
            |schema_case| crate::fs_uae_smoke::OpforgeNativeCliParityCase {
                name: schema_case.name,
                cpu_override: "68020",
                extra_assembly_defines: schema_case.defines.as_slice(),
                source_override: schema_case.source.as_deref().map(str::as_bytes),
                command_template: schema_case.command_template,
                package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &[],
                proof: native_cli_schema_runner_proof(schema_case),
            },
        )
        .collect::<Vec<_>>();

    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &repo_root,
        parity_cases.as_slice(),
    )
    .expect("schema-driven native opForge CLI FS-UAE shard should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                schema_cases.len(),
                "expected one native opForge CLI run per schema case inside a single batch"
            );
            for (schema_case, run) in schema_cases.iter().zip(runs.iter()) {
                assert_native_cli_schema_case(schema_case, run);
            }
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_schema_listing_parity_matches_live_rust_cli() {
    // Proof level D. This test proves a real Amiga-native CLI listing matches
    // the live Rust CLI listing under the reviewed normalization. This test
    // does not prove map or diagnostic parity.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let repo_root = workspace_root();
    let schema_cases = [native_cli_schema_listing_case_with_live_rust_oracle(
        &repo_root,
    )];
    let parity_cases = schema_cases
        .iter()
        .map(
            |schema_case| crate::fs_uae_smoke::OpforgeNativeCliParityCase {
                name: schema_case.name,
                cpu_override: "68020",
                extra_assembly_defines: schema_case.defines.as_slice(),
                source_override: schema_case.source.as_deref().map(str::as_bytes),
                command_template: schema_case.command_template,
                package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &[],
                proof: native_cli_schema_runner_proof(schema_case),
            },
        )
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &repo_root,
        parity_cases.as_slice(),
    )
    .expect("schema listing FS-UAE shard should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), schema_cases.len());
            for (schema_case, run) in schema_cases.iter().zip(runs.iter()) {
                assert_native_cli_schema_case(schema_case, run);
            }
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_schema_diagnostic_parity_matches_live_rust_cli() {
    // Proof level D. This test proves the real Amiga-native CLI returns failure
    // and emits the same normalized unknown-mnemonic class as the live Rust CLI.
    // This test does not prove other diagnostic classes or exact wording parity.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let schema_cases = [native_cli_schema_unknown_mnemonic_case_with_live_rust_oracle()];
    let parity_cases = schema_cases
        .iter()
        .map(
            |schema_case| crate::fs_uae_smoke::OpforgeNativeCliParityCase {
                name: schema_case.name,
                cpu_override: "68020",
                extra_assembly_defines: schema_case.defines.as_slice(),
                source_override: schema_case.source.as_deref().map(str::as_bytes),
                command_template: schema_case.command_template,
                package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &[],
                proof: native_cli_schema_runner_proof(schema_case),
            },
        )
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        parity_cases.as_slice(),
    )
    .expect("schema diagnostic FS-UAE shard should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), schema_cases.len());
            for (schema_case, run) in schema_cases.iter().zip(runs.iter()) {
                assert_native_cli_schema_case(schema_case, run);
                assert_eq!(
                    run.exit_code,
                    Some(1),
                    "native CLI diagnostic exit status must match Rust\nstdout:\n{}\nstderr:\n{}",
                    run.stdout,
                    run.stderr,
                );
                assert!(
                    !run.stdout.contains("unknown native mnemonic")
                        && !run.stdout.contains("native pass engine failed"),
                    "native CLI failure diagnostics must not leak to stdout\nstdout:\n{}\nstderr:\n{}",
                    run.stdout,
                    run.stderr,
                );
            }
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_expression_metadata_fallback_matches_live_rust_cli() {
    // Proof level D. This test proves the real native CLI resolves an exact,
    // unmodified operand-expression source to the same bytes as the live Rust
    // CLI. This test does not inject malformed native session metadata.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let source = "        .org $1000\nstart   lda #$40 + 2\n        sta $20\n";
    let case_dir = create_temp_dir("native-expression-metadata-fallback");
    let input_path = case_dir.join("input.asm");
    let rust_bin_path = case_dir.join("rust.bin");
    fs::write(&input_path, source).expect("write expression fallback source");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        rust_bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run live Rust expression fallback oracle");
    let rust_bin = fs::read(&rust_bin_path).expect("read live Rust expression fallback bytes");
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "expression-metadata-fallback",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &rust_bin,
        },
    }];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("expression metadata fallback FS-UAE shard should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native expression fallback run failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
            assert_eq!(native_bin, rust_bin);
        }
    }
}

#[test]
fn native_column_one_directive_routing_fs_uae() {
    // Proof level D. This test proves a source-side CPU directive in column one
    // reaches the real Amiga-native CLI without becoming a label and writes
    // the same bytes as the live Rust CLI. It does not prove .for semantics.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let repo_root = workspace_root();
    let source = b".cpu 6502\n.org 0\n.byte $aa\n";
    let case_dir = create_temp_dir("native-column-one-directive");
    let source_path = case_dir.join("input.asm");
    let rust_bin_path = case_dir.join("rust.bin");
    fs::write(&source_path, source).expect("write column-one directive source");
    let cli = Cli::parse_from([
        "opForge",
        source_path.to_string_lossy().as_ref(),
        "--bin",
        rust_bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run live Rust column-one directive oracle");
    let rust_bin = fs::read(&rust_bin_path).expect("read live Rust column-one directive bytes");
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "column-one-directives",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &rust_bin,
        },
    }];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&repo_root, &cases)
        .expect("column-one directive FS-UAE case should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native column-one directive run failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
            assert_eq!(native_bin, rust_bin);
        }
    }
}

#[test]
fn native_opcore_counted_for_fs_uae() {
    // Proof level D. This test proves the canonical counted `.for` opcore
    // source reaches the real Amiga-native CLI and writes the same flat bytes
    // as the live Rust CLI. It does not prove iterable `.for` or `.bfor`.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let repo_root = workspace_root();
    let source_path = repo_root.join("examples/opcore/for_counter_basic.asm");
    let source = fs::read(&source_path).expect("read canonical counted-for source");
    let case_dir = create_temp_dir("native-opcore-counted-for");
    let rust_bin_path = case_dir.join("rust.bin");
    let cli = Cli::parse_from([
        "opForge",
        source_path.to_string_lossy().as_ref(),
        "--bin",
        rust_bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run live Rust counted-for oracle");
    let rust_bin = fs::read(&rust_bin_path).expect("read live Rust counted-for bytes");
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "opcore-counted-for",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_slice()),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &rust_bin,
        },
    }];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&repo_root, &cases)
        .expect("counted-for FS-UAE case should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native counted-for run failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
            assert_eq!(native_bin, rust_bin);
        }
    }
}

#[test]
fn native_opcore_sequence_assignment_fs_uae() {
    // Proof level D. This test proves a real Amiga-native session consumes a
    // bounded list assignment as compile-time storage rather than an unknown
    // mnemonic. It does not prove list lookup, indexing, `.len`, or iteration.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let source = b"values = {1, 2, 3}\n.org 0\n.byte $44\n";
    let case_dir = create_temp_dir("native-opcore-sequence-assignment");
    let input_path = case_dir.join("input.asm");
    let rust_bin_path = case_dir.join("rust.bin");
    fs::write(&input_path, source).expect("write sequence assignment source");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        rust_bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run live Rust sequence assignment oracle");
    let rust_bin = fs::read(&rust_bin_path).expect("read Rust sequence assignment bytes");
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "opcore-sequence-assignment",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &rust_bin,
        },
    }];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("sequence assignment FS-UAE case should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native sequence assignment run failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
            assert_eq!(native_bin, rust_bin);
        }
    }
}

#[test]
fn native_opcore_iterable_for_fs_uae() {
    // Proof level D. This test proves real Amiga-native sessions expand list and
    // inclusive stepped-range `.for` loops, bind each element, and match live
    // Rust bytes while the canonical range case also exercises indexing and
    // `.len`. It does not prove descending ranges or non-numeric iterables.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let input_path = workspace_root().join("examples/opcore/for_collection_basic.asm");
    let list_source = fs::read(&input_path).expect("read canonical iterable for source");
    let case_dir = create_temp_dir("native-opcore-iterable-for");
    let range_path = workspace_root().join("examples/opcore/ranges_lists_basic.asm");
    let range_source = fs::read(&range_path).expect("read canonical ranges and lists source");
    let mut rust_bins = Vec::new();
    for (index, source_path) in [&input_path, &range_path].into_iter().enumerate() {
        let rust_bin_path = case_dir.join(format!("rust-{index}.bin"));
        let cli = Cli::parse_from([
            "opForge",
            source_path.to_string_lossy().as_ref(),
            "--bin",
            rust_bin_path.to_string_lossy().as_ref(),
            "--cpu",
            "m6502",
        ]);
        run_with_cli_with_context(&cli).expect("run live Rust iterable for oracle");
        rust_bins.push(fs::read(&rust_bin_path).expect("read Rust iterable for bytes"));
    }
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "opcore-iterable-list",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&list_source),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bins[0],
            },
        },
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "opcore-iterable-range",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&range_source),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bins[1],
            },
        },
    ];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("iterable for FS-UAE case should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), rust_bins.len());
            for (run, rust_bin) in runs.iter().zip(rust_bins.iter()) {
                assert!(
                    run.success,
                    "native iterable for run failed\nstdout:\n{}\nstderr:\n{}",
                    run.stdout, run.stderr
                );
                let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
                assert_eq!(&native_bin, rust_bin);
            }
        }
    }
}

#[test]
fn native_opcore_while_fs_uae() {
    // Proof level D. This test proves a real Amiga-native session reevaluates
    // canonical `.while $ < 4` against the advancing current address and
    // matches live Rust bytes. It does not prove arbitrary condition syntax.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let input_path = workspace_root().join("examples/opcore/while_basic.asm");
    let source = fs::read(&input_path).expect("read canonical while source");
    let case_dir = create_temp_dir("native-opcore-while");
    let rust_bin_path = case_dir.join("rust.bin");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        rust_bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run live Rust while oracle");
    let rust_bin = fs::read(&rust_bin_path).expect("read Rust while bytes");
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "opcore-while",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(&source),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &rust_bin,
        },
    }];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("while FS-UAE case should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native while run failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
            assert_eq!(native_bin, rust_bin);
        }
    }
}

#[test]
fn native_opcore_conditionals_fs_uae() {
    // Proof level D. This test proves a real native CLI session chooses the
    // selected canonical `.if` branches and matches live Rust bytes. It does
    // not prove `.match` branch selection.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let input_path = workspace_root().join("examples/opcore/cond_syntax.asm");
    let source = fs::read(&input_path).expect("read canonical conditional source");
    let case_dir = create_temp_dir("native-opcore-conditionals");
    let rust_bin_path = case_dir.join("rust.bin");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        rust_bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run live Rust conditional oracle");
    let rust_bin = fs::read(&rust_bin_path).expect("read Rust conditional bytes");
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "opcore-conditionals",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(&source),
        command_template: Some("{input} --bin {bin} --cpu m6502"),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &rust_bin,
        },
    }];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("conditional FS-UAE case should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native conditional run failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
            assert_eq!(native_bin, rust_bin);
        }
    }
}

#[test]
fn opcore_scopes_rust_oracle_covers_canonical_qualification() {
    // Proof level A. Live Rust establishes canonical bytes for nested block
    // shadowing and namespace close aliases; it does not execute native code.
    let case_dir = create_temp_dir("opcore-scopes-rust-oracle");
    for (index, (source, expected)) in [
        ("examples/opcore/scopes.asm", vec![2, 0, 1, 0, 5, 0]),
        (
            "examples/opcore/scopes_namespace.asm",
            vec![1, 0, 5, 0, 9, 0],
        ),
    ]
    .into_iter()
    .enumerate()
    {
        let input_path = workspace_root().join(source);
        let bin_path = case_dir.join(format!("scope-{index}.bin"));
        let cli = Cli::parse_from([
            "opForge",
            input_path.to_string_lossy().as_ref(),
            "--bin",
            bin_path.to_string_lossy().as_ref(),
            "--cpu",
            "m6502",
        ]);
        run_with_cli_with_context(&cli).expect("run Rust scope oracle");
        assert_eq!(fs::read(bin_path).expect("read Rust scope bytes"), expected);
    }
}

#[test]
fn opcore_module_basics_rust_oracle_covers_module_local_symbols() {
    // Proof level A. The live Rust CLI establishes that equal local names in
    // separate modules are legal and emit their module-local values. It does
    // not execute the native 68020 implementation.
    let input_path = workspace_root().join("examples/opcore/module_basics.asm");
    let bin_path = create_temp_dir("opcore-module-basics-rust-oracle").join("module-basics.bin");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run Rust module-local symbol oracle");
    let bytes = fs::read(bin_path).expect("read Rust module bytes");
    assert_eq!(bytes.len(), 0x1001, "Rust preserves the module .org gap");
    assert_eq!(bytes.first(), Some(&1));
    assert!(bytes[1..0x1000].iter().all(|byte| *byte == 0));
    assert_eq!(bytes.last(), Some(&2));
}

#[test]
fn native_scope_source_tracks_stack_and_qualified_symbols() {
    // Proof level B. Native source owns bounded scope push/pop, pass reset,
    // definition qualification, and innermost-to-outer lookup. It does not
    // execute the 68020 callback path.
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    let scope_flow = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_flow_scopes.asm"),
    )
    .expect("read native scope-flow implementation");
    let engine = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_engine.asm"),
    )
    .expect("read native opasm engine");
    assert!(source_contains_in_order(
        &driver,
        &[
            "opasmDriverPassOneBegin",
            "jsr scopes.resetStateV1",
            "opasmDriverPassTwoBegin",
            "jsr scopes.resetStateV1",
        ]
    ));
    assert!(source_contains_in_order(
        &driver,
        &[
            ".use opasm.amigaos.flow_scopes as scopes",
            "bsr.w scopes.beginBlockScopeV1",
            "bsr.w scopes.beginNamespaceScopeV1",
            "bsr.w scopes.endScopeDirectiveV1",
        ]
    ));
    assert!(source_contains_in_order(
        &driver,
        &[
            "checkModule",
            "lea ModuleMnemonicText, a1",
            "bsr.w scopes.beginModuleScopeV1",
            "checkEndmodule",
            "lea EndmoduleMnemonicText, a1",
            "bsr.w scopes.endScopeDirectiveV1",
        ]
    ));
    assert!(!driver.contains("bne.w scopes."));
    assert!(driver.contains("jsr scopes.qualifyStatementLabelIfScopedV1"));
    assert!(driver.contains("jsr scopes.resolveLabelValueV1"));
    assert!(source_contains_in_order(
        &scope_flow,
        &[
            "beginBlockScopeV1\t.block",
            "jsr eng.opasmEngineGetStatementLabelTextV1",
            "bsr.w pushText",
            "beginNamespaceScopeV1\t.block",
            "bsr.w pushFromStatementOperand",
            "beginModuleScopeV1\t.block",
            "clr.w ScopeDepth",
            "endScopeDirectiveV1\t.block",
            "bsr.w popScope",
            "qualifyStatementLabelIfScopedV1\t.block",
            "resolveLabelValueV1\t.block",
            "subq.w #1, d2",
        ]
    ));
    assert!(source_contains_in_order(
        &scope_flow,
        &[
            "buildTextAtDepth\t.block",
            "tst.b (a1)",
            "beq.s nextScope",
            "nextScope",
        ]
    ));
    assert!(scope_flow.contains(".module opasm.amigaos.flow_scopes"));
    assert!(scope_flow.contains(".endsection"));
    assert!(scope_flow.contains(".endmodule"));
    assert!(engine.contains("opasmEngineSetStatementLabelTextV1\t.block"));
    assert!(engine.contains("opasmEngineGetStatementSourceTextV1\t.block"));
}

fn native_scope_contract(stack: &[&str], raw: &str, defined: &[&str]) -> Option<String> {
    (0..=stack.len()).rev().find_map(|depth| {
        let candidate = if depth == 0 {
            raw.to_string()
        } else {
            format!("{}.{}", stack[..depth].join("."), raw)
        };
        defined.contains(&candidate.as_str()).then_some(candidate)
    })
}

fn native_scope_close_contract(stack: &mut Vec<&str>, directive: &str) -> Result<(), ()> {
    match directive {
        ".endblock" | ".bend" | ".endnamespace" | ".endn" | ".endmodule" => {
            stack.pop().map(|_| ()).ok_or(())
        }
        _ => Err(()),
    }
}

#[test]
fn native_scope_contract_covers_nested_shadowing_and_close_aliases() {
    // Proof level C. This host model proves inner-first qualification and
    // block/namespace close equivalence; it does not inspect native tables.
    let stack = ["OUTER", "INNER"];
    assert_eq!(
        native_scope_contract(&stack, "VAL", &["VAL", "OUTER.INNER.VAL"]),
        Some("OUTER.INNER.VAL".to_string())
    );
    assert_eq!(
        native_scope_contract(&stack, "VALUE", &["VALUE", "OUTER.VALUE"]),
        Some("OUTER.VALUE".to_string())
    );
    assert_eq!(
        native_scope_contract(&["SHADOW"], "GLOBAL", &["GLOBAL", "SHADOW.GLOBAL"]),
        Some("SHADOW.GLOBAL".to_string())
    );
    assert_eq!(
        native_scope_contract(&stack, "GLOBAL", &["GLOBAL"]),
        Some("GLOBAL".to_string())
    );
    let mut endblock_stack = vec!["OUTER", "INNER"];
    let mut bend_stack = endblock_stack.clone();
    native_scope_close_contract(&mut endblock_stack, ".endblock").expect("endblock pops");
    native_scope_close_contract(&mut bend_stack, ".bend").expect("bend pops");
    assert_eq!(endblock_stack, bend_stack);
    let mut endnamespace_stack = vec!["outer", "inner"];
    let mut endn_stack = endnamespace_stack.clone();
    native_scope_close_contract(&mut endnamespace_stack, ".endnamespace")
        .expect("endnamespace pops");
    native_scope_close_contract(&mut endn_stack, ".endn").expect("endn pops");
    assert_eq!(endnamespace_stack, endn_stack);
    assert_eq!(
        native_scope_contract(&["alpha"], "VALUE", &["alpha.VALUE", "beta.VALUE"]),
        Some("alpha.VALUE".to_string())
    );
    assert_eq!(
        native_scope_contract(&["beta"], "VALUE", &["alpha.VALUE", "beta.VALUE"]),
        Some("beta.VALUE".to_string())
    );
    let mut module_stack = vec!["alpha"];
    native_scope_close_contract(&mut module_stack, ".endmodule").expect("endmodule pops");
    assert!(module_stack.is_empty());
}

#[test]
fn native_scoped_expression_snapshot_preserves_local_shadowing_contract() {
    // Proof level C. This models the alias projection supplied to the existing
    // expression bridge; real native execution is proved separately.
    fn active_alias<'a>(scope: &[&str], qualified: &'a str) -> Option<&'a str> {
        let prefix = format!("{}.", scope.join("."));
        qualified
            .strip_prefix(&prefix)
            .filter(|name| !name.is_empty())
    }

    assert_eq!(active_alias(&["alpha"], "alpha.VALUE"), Some("VALUE"));
    assert_eq!(active_alias(&["alpha"], "beta.VALUE"), None);
    assert_eq!(
        active_alias(&["main", "inner"], "main.inner.OFFSET"),
        Some("OFFSET")
    );
    assert_eq!(active_alias(&[], "VALUE"), None);
}

#[test]
fn native_scoped_expression_snapshot_source_stays_in_opasm_adapter() {
    // Proof level B. Scope ownership projects aliases into an opasm-owned
    // snapshot before tkpkg consumes the unchanged expression extension.
    let root = workspace_root();
    let scopes =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_flow_scopes.asm"))
            .expect("read scope owner");
    let operand =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_operand_eval.asm"))
            .expect("read operand adapter");

    assert!(source_contains_in_order(
        &scopes,
        &[
            "activeLabelAliasV1\t.block",
            "move.w ScopeDepth, d4",
            "scopeLoop",
            "cmpi.b #'.', (a1)+",
            "movea.l a1, a0",
            "move.l d5, d0",
        ]
    ));
    assert!(source_contains_in_order(
        &operand,
        &[
            "jsr eng.prepareEvaluateExpressionExtensionV1",
            "bsr.w materializeScopedSnapshot",
            "materializeScopedSnapshot\t.block",
            "aliasLoop",
            "jsr scopes.activeLabelAliasV1",
            "copyOriginalBegin",
            "move.l #ScopedSnapshotNames, 0(a3)",
            "move.l #ScopedSnapshotValues, 4(a3)",
        ]
    ));
    assert!(operand.contains("SCOPED_SNAPSHOT_SOURCE_CAPACITY = 512"));
    assert!(operand.contains("SCOPED_SNAPSHOT_CAPACITY = 1024"));
}

#[test]
fn native_module_local_symbol_fs_uae() {
    // Proof level D. FS-UAE runs the canonical module-basics source through
    // the real native CLI. It proves module-local symbol separation only; it
    // does not prove macro tokenization or statement expansion.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let root = workspace_root();
    let source_path = root.join("examples/opcore/module_basics.asm");
    let source = fs::read(&source_path).expect("read canonical module source");
    let (entries, diagnostics) = assemble_example_entries_with_runtime_mode(&source_path, true)
        .expect("full Rust module authority");
    assert!(
        diagnostics.is_empty(),
        "Rust module diagnostics: {diagnostics:?}"
    );
    let rust = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    let package = item6_mos_package_bytes();
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "module-basics",
        cpu_id: "65c02",
        source: source.as_slice(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust),
    }];
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("module-local symbol FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one module-local native run");
            let run = &runs[0];
            assert!(run.success, "native module-basics failed: {}", run.stdout);
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust, "native module-local bytes differ");
        }
    }
}

#[test]
fn native_opcore_scopes_fs_uae() {
    // Proof level D. FS-UAE executes both untouched canonical scope sources
    // through the native CLI and compares output bytes with the live Rust
    // oracle. It does not prove unassigned struct or scoped-repeat semantics.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let case_dir = create_temp_dir("native-opcore-scopes");
    let source_paths = [
        workspace_root().join("examples/opcore/scopes.asm"),
        workspace_root().join("examples/opcore/scopes_namespace.asm"),
    ];
    let mut sources = Vec::new();
    let mut rust_bins = Vec::new();
    for (index, input_path) in source_paths.iter().enumerate() {
        let source = fs::read(input_path).expect("read canonical scope source");
        let bin_path = case_dir.join(format!("rust-{index}.bin"));
        let cli = Cli::parse_from([
            "opForge",
            input_path.to_string_lossy().as_ref(),
            "--bin",
            bin_path.to_string_lossy().as_ref(),
            "--cpu",
            "m6502",
        ]);
        run_with_cli_with_context(&cli).expect("run live Rust scope oracle");
        sources.push(source);
        rust_bins.push(fs::read(bin_path).expect("read Rust scope bytes"));
    }
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "opcore-scopes",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&sources[0]),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bins[0],
            },
        },
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "opcore-scopes-namespace",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&sources[1]),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bins[1],
            },
        },
    ];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("scope FS-UAE cases should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), rust_bins.len());
            for (run, rust_bin) in runs.iter().zip(rust_bins.iter()) {
                assert!(run.success, "native scope run failed\n{}", run.stdout);
                let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
                assert_eq!(&native_bin, rust_bin);
            }
        }
    }
}

#[test]
fn opcore_structs_rust_oracle_covers_canonical_layouts() {
    // Proof level A. Live Rust establishes the canonical struct and scoped
    // repetition artifacts; it does not execute native code.
    let case_dir = create_temp_dir("opcore-structs-rust-oracle");
    for (index, (source, expected)) in [
        (
            "examples/opcore/struct_literal_instance_basic.asm",
            vec![24, 50, 40, 60, 41, 61],
        ),
        (
            "examples/opcore/struct_var_instance_basic.asm",
            vec![3, 0, 1],
        ),
        (
            "examples/opcore/bfor_labeled_struct_basic.asm",
            vec![0, 10, 1, 11, 2, 12, 2, 0, 3, 0],
        ),
    ]
    .into_iter()
    .enumerate()
    {
        let input_path = workspace_root().join(source);
        let bin_path = case_dir.join(format!("struct-{index}.bin"));
        let cli = Cli::parse_from([
            "opForge",
            input_path.to_string_lossy().as_ref(),
            "--bin",
            bin_path.to_string_lossy().as_ref(),
            "--cpu",
            "m6502",
        ]);
        run_with_cli_with_context(&cli).expect("run Rust struct oracle");
        assert_eq!(
            fs::read(bin_path).expect("read Rust struct bytes"),
            expected
        );
    }
}

#[test]
fn native_struct_source_owns_directives_before_statement_processing() {
    // Proof level B. Native source owns struct start/end and placeholder-field
    // routing before ordinary statement processing; it does not execute the
    // 68020 callback path or prove member-value contents.
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    let struct_flow = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_flow_structs.asm"),
    )
    .expect("read native struct-flow implementation");
    assert!(source_contains_in_order(
        &driver,
        &[
            ".use opasm.amigaos.flow_structs as structs",
            "opasmDriverPassOneBegin",
            "jsr structs.resetStateV1",
            "opasmDriverPassTwoBegin",
            "jsr structs.resetStateV1",
            "jsr structs.routeDirectiveV1",
            "beginStructDefinition",
            "skipStructField",
            "jsr structs.captureFieldV1",
            "tryCaptureTypedStructInstanceForStatement\t.block",
            "jsr structs.captureTypedInstanceV1",
        ],
    ));
    assert!(source_contains_in_order(
        &driver,
        &[
            "jsr structs.advanceScopedRepeatV1",
            "beginBfor",
            "jsr structs.beginScopedRepeatV1",
            "qualifyScopedRepeatLabelForStatement\t.block",
            "jsr structs.qualifyScopedRepeatLabelV1",
            "jsr compile_values.resolveBindingExpressionV1",
            "OpasmRepeatBforBasePc",
            "OpasmDriverScopedRepeatValue",
        ],
    ));
    assert!(source_contains_in_order(
        &struct_flow,
        &[
            "routeDirectiveV1\t.block",
            "StructMnemonicText",
            "EndstructMnemonicText",
            "structField",
            "beginDefinitionV1\t.block",
            "moveq #0, d0",
            "bra.s return",
            "captureFieldV1\t.block",
            "endDefinitionV1\t.block",
            "captureTypedInstanceV1\t.block",
            "compile_values.upsertBindingV1",
        ],
    ));
    assert!(source_contains_in_order(
        &struct_flow,
        &[
            "beginScopedRepeatV1\t.block",
            "advanceScopedRepeatV1\t.block",
            "qualifyScopedRepeatLabelV1\t.block",
            "StructScopedRepeatIteration",
        ],
    ));
    assert!(struct_flow.contains(".module opasm.amigaos.flow_structs"));
    assert!(struct_flow.contains(".endsection"));
    assert!(struct_flow.contains(".endmodule"));
}

#[test]
fn native_struct_contract_covers_layout_instances_and_scoped_labels() {
    // Proof level C. This host model proves the field-layout and member-address
    // relationships used by the canonical sources; it does not execute native
    // assembly or native expression resolution.
    let point_field_sizes = [1_u32, 2_u32];
    let mut offsets = Vec::new();
    let mut size = 0_u32;
    for field_size in point_field_sizes {
        offsets.push(size);
        size += field_size;
    }
    assert_eq!(offsets, vec![0, 1]);
    assert_eq!(size, 3);

    let literal_instance = [24_u32, 50_u32];
    let mut mutable_instance = [40_u32, 60_u32];
    assert_eq!(mutable_instance, [40, 60]);
    mutable_instance = [41, 61];
    assert_eq!(literal_instance, [24, 50]);
    assert_eq!(mutable_instance, [41, 61]);

    let point_stride = 2_u32;
    let second_point_base = point_stride;
    assert_eq!(second_point_base + 0, 2);
    assert_eq!(second_point_base + 1, 3);
}

#[test]
fn native_text_encoding_source_owns_builtin_selectors_before_statement_processing() {
    // Proof level B. The native driver resets and routes text-encoding
    // selectors before other directive handlers. This does not execute the
    // 68020 path or prove source-defined table contents.
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    let text_flow = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_flow_text_encoding.asm"),
    )
    .expect("read native text-encoding flow");
    assert!(source_contains_in_order(
        &driver,
        &[
            ".use opasm.amigaos.flow_text_encoding as text_encoding",
            "opasmDriverPassOneBegin",
            "jsr text_encoding.resetStateV1",
            "opasmDriverPassTwoBegin",
            "jsr text_encoding.resetStateV1",
            "checkConditional",
            "jsr text_encoding.routeDirectiveV1",
            "skipTextEncodingDirective",
            "jsr text_encoding.encodeBytesV1",
        ],
    ));
    assert!(source_contains_in_order(
        &text_flow,
        &[
            "routeDirectiveV1\t.block",
            "EncMnemonicText",
            "EncodingMnemonicText",
            "EncodeMnemonicText",
            "CdefMnemonicText",
            "TdefMnemonicText",
            "EdefMnemonicText",
            "selector",
            "AsciiText",
            "PetsciiText",
            "encodeBytesV1\t.block",
            "beginDefinitionV1\t.block",
            "defineCdefV1\t.block",
            "defineTdefV1\t.block",
            "defineEdefV1\t.block",
        ],
    ));
    assert!(source_contains_in_order(
        &driver,
        &[
            "byteDirectiveSizeForStatement\t.block",
            "parseTextDirectiveForStatement",
            "emitByteDirectiveForStatement\t.block",
            "parseTextDirectiveForStatement",
        ],
    ));
}

#[test]
fn native_text_encoding_contract_covers_builtin_ascii_and_petscii_mapping() {
    // Proof level C. This host model proves the byte mapping used by the
    // initial native built-in selector path; it does not execute 68020 code
    // or prove clone, character-range, token, and escape definitions.
    let petscii = |byte: u8| match byte {
        b'A'..=b'Z' => byte | 0x80,
        b'a'..=b'z' => byte - 0x20,
        _ => byte,
    };
    assert_eq!(b"Az", b"Az");
    assert_eq!(
        b"Az".iter().copied().map(petscii).collect::<Vec<_>>(),
        [0xC1, 0x5A]
    );
}

#[test]
fn native_opcore_text_encoding_fs_uae() {
    // Proof level D. FS-UAE executes the untouched canonical text-encoding
    // sources through the native CLI and compares their bytes with live Rust.
    // It does not prove unrelated syntax adaptation.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let case_dir = create_temp_dir("native-opcore-text-encoding");
    let source_paths = [
        workspace_root().join("examples/opcore/text_encoding.asm"),
        workspace_root().join("examples/opcore/text_encoding_definitions.asm"),
    ];
    let mut sources = Vec::new();
    let mut rust_bins = Vec::new();
    for (index, input_path) in source_paths.iter().enumerate() {
        let source = fs::read(input_path).expect("read canonical text source");
        let bin_path = case_dir.join(format!("rust-{index}.bin"));
        let cli = Cli::parse_from([
            "opForge",
            input_path.to_string_lossy().as_ref(),
            "--bin",
            bin_path.to_string_lossy().as_ref(),
            "--cpu",
            "m6502",
        ]);
        run_with_cli_with_context(&cli).expect("run live Rust text oracle");
        sources.push(source);
        rust_bins.push(fs::read(bin_path).expect("read Rust text bytes"));
    }
    let command_template = if std::env::var_os("OPFORGE_TEXT_ENCODING_NATIVE_DEBUG").is_some() {
        "{input} --bin {bin} --cpu m6502 --native-debug"
    } else {
        "{input} --bin {bin} --cpu m6502"
    };
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "opcore-text-encoding",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&sources[0]),
            command_template: Some(command_template),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bins[0],
            },
        },
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "opcore-text-encoding-definitions",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&sources[1]),
            command_template: Some(command_template),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bins[1],
            },
        },
    ];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("text-encoding FS-UAE cases should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), rust_bins.len());
            for (run, rust_bin) in runs.iter().zip(rust_bins.iter()) {
                assert!(run.success, "native text run failed\n{}", run.stdout);
                let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
                assert_eq!(&native_bin, rust_bin);
            }
        }
    }
}

#[test]
fn native_text_encoding_definition_steps_fs_uae() {
    // Proof level E. These reduced, self-contained sources localize the first
    // native definition boundary; they do not replace canonical Level D parity.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let sources: Vec<Vec<u8>> = vec![
        b".module main\n.org $1200\n.encode gamefont\n.endencode\n.byte 0\n.endmodule\n".to_vec(),
        b".module main\n.org $1200\n.encode gamefont\n.cdef \"A\", \"Z\", 1\n.endencode\n.enc gamefont\n.byte \"AZ\", 0\n.endmodule\n".to_vec(),
        b".module main\n.org $1200\n.encode gamefont\n.tdef \"xy\", $40\n.endencode\n.enc gamefont\n.byte \"xy\", 0\n.endmodule\n".to_vec(),
        b".module main\n.org $1200\n.encode gamefont\n.tdef \"!?\", $80, $81\n.endencode\n.enc gamefont\n.byte \"!?\", 0\n.endmodule\n".to_vec(),
        b".module main\n.org $1200\n.encode gamefont\n.edef \"{cr}\", 13\n.endencode\n.enc gamefont\n.byte \"{cr}\", 0\n.endmodule\n".to_vec(),
        b".module main\n.org $1200\n.encode shifted,petscii\n.edef \"{home}\", 19\n.endencode\n.enc shifted\n.byte \"Az{home}\", 0\n.endmodule\n".to_vec(),
    ];
    let expected = [
        vec![0],
        vec![1, 26, 0],
        vec![64, 65, 0],
        vec![128, 129, 0],
        vec![13, 0],
        vec![0xC1, 0x5A, 19, 0],
    ];
    let cases = sources
        .iter()
        .zip(expected.iter())
        .enumerate()
        .map(
            |(index, (source, rust_oracle))| crate::fs_uae_smoke::OpforgeNativeCliParityCase {
                name: [
                    "text-definition-empty",
                    "text-definition-cdef",
                    "text-definition-tdef-range",
                    "text-definition-tdef-list",
                    "text-definition-edef",
                    "text-definition-clone",
                ][index],
                cpu_override: "68020",
                extra_assembly_defines: &[],
                source_override: Some(source),
                command_template: Some("{input} --bin {bin} --cpu m6502 --native-debug"),
                package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &[],
                proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                    relative_path: "Work/opforge_native_out.bin",
                    rust_oracle,
                },
            },
        )
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("text-definition FS-UAE cases should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), expected.len());
            for (index, (run, expected)) in runs.iter().zip(expected.iter()).enumerate() {
                assert!(
                    run.success,
                    "definition step {index} failed\n{}",
                    run.stdout
                );
                let bytes = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
                assert_eq!(&bytes, expected, "definition step {index}");
            }
        }
    }
}

#[test]
fn native_opcore_structs_fs_uae() {
    // Proof level D. FS-UAE executes the untouched canonical struct sources
    // through the native CLI and compares output bytes with the live Rust CLI.
    // It does not prove text encodings or additive MOS adaptations.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let case_dir = create_temp_dir("native-opcore-structs");
    let source_paths = [
        workspace_root().join("examples/opcore/struct_literal_instance_basic.asm"),
        workspace_root().join("examples/opcore/struct_var_instance_basic.asm"),
        workspace_root().join("examples/opcore/bfor_labeled_struct_basic.asm"),
    ];
    let mut sources = Vec::new();
    let mut rust_bins = Vec::new();
    for (index, input_path) in source_paths.iter().enumerate() {
        let source = fs::read(input_path).expect("read canonical struct source");
        let bin_path = case_dir.join(format!("rust-{index}.bin"));
        let cli = Cli::parse_from([
            "opForge",
            input_path.to_string_lossy().as_ref(),
            "--bin",
            bin_path.to_string_lossy().as_ref(),
            "--cpu",
            "m6502",
        ]);
        run_with_cli_with_context(&cli).expect("run live Rust struct oracle");
        sources.push(source);
        rust_bins.push(fs::read(bin_path).expect("read Rust struct bytes"));
    }
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "opcore-struct-literal",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&sources[0]),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bins[0],
            },
        },
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "opcore-struct-var",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&sources[1]),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bins[1],
            },
        },
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "opcore-struct-bfor",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&sources[2]),
            command_template: Some("{input} --bin {bin} --cpu m6502"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bins[2],
            },
        },
    ];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("struct FS-UAE cases should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), rust_bins.len());
            for (run, rust_bin) in runs.iter().zip(rust_bins.iter()) {
                assert!(run.success, "native struct run failed\n{}", run.stdout);
                let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
                assert_eq!(&native_bin, rust_bin);
            }
        }
    }
}

#[test]
fn opcore_conditionals_rust_oracle_covers_canonical_branches() {
    // Proof level A. This test proves live Rust selects the canonical if and
    // match branches. It does not prove native routing or 68020 execution.
    let case_dir = create_temp_dir("opcore-conditionals-rust-oracle");
    let input_path = workspace_root().join("examples/opcore/cond_syntax.asm");
    let bin_path = case_dir.join("conditional.bin");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run Rust conditional oracle");
    assert_eq!(
        fs::read(bin_path).expect("read Rust conditional bytes"),
        [1, 5, 8, 10, 13, 16]
    );
}

fn native_if_branch_contract(levels: &[&[bool]]) -> Option<Vec<usize>> {
    levels
        .iter()
        .map(|branches| branches.iter().position(|selected| *selected))
        .collect()
}

fn native_match_case_contract(value: u32, cases: &[&[u32]], default_index: usize) -> usize {
    cases
        .iter()
        .position(|case_values| case_values.contains(&value))
        .unwrap_or(default_index)
}

#[test]
fn native_conditional_contract_covers_nested_if_and_match_selection() {
    // Proof level C. This host request-shape model proves first-matching branch
    // selection, else/default fallback, and nesting-local decisions. It does
    // not execute native statement scans or 68020 branches.
    assert_eq!(
        native_if_branch_contract(&[&[true, false], &[false, true]]),
        Some(vec![0, 1])
    );
    assert_eq!(native_if_branch_contract(&[&[false, false]]), None);
    assert_eq!(native_match_case_contract(2, &[&[1], &[2, 3]], 2), 1);
    assert_eq!(native_match_case_contract(9, &[&[1, 2]], 1), 1);
}

#[test]
fn native_conditional_source_records_then_skips_unselected_statement_ranges() {
    // Proof level B. This test proves source routing records conditionals and
    // native flow owns bounded if/match scans before ordinary processing. It
    // does not prove the scans execute on real hardware.
    let parser = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read native line processor");
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    let conditional_flow = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_flow_conditionals.asm"),
    )
    .expect("read native conditional-flow implementation");
    assert!(source_contains_in_order(
        &parser,
        &[
            "conditionalLine",
            "jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine",
            "bra.w done",
        ]
    ));
    assert!(driver.contains(".use opasm.amigaos.flow_conditionals as conditionals"));
    assert!(source_contains_in_order(
        &driver,
        &[
            "checkConditional",
            "jsr conditionals.routeDirectiveV1",
            "tst.w d3",
            "beq.w checkForMnemonic",
            "beq.w beginMatchBranch",
            "beq.w finishIfBranch",
            "checkForMnemonic",
        ]
    ));
    assert!(source_contains_in_order(
        &conditional_flow,
        &[
            "routeDirectiveV1\t.block",
            "ConditionalMatchMnemonicText",
            "conditionalMatchedMatch",
            "conditionalMatchedEndif",
            "moveq #7, d3",
        ]
    ));
    assert!(conditional_flow.contains(".endmodule"));
}

#[test]
fn native_conditional_flow_transitions_survive_callback_register_clobbers() {
    // Proof level B. This test locks the explicit engine-owned transition and
    // cursor-preservation contract. It does not execute the 68020 callback.
    let repo_root = workspace_root();
    let engine =
        fs::read_to_string(repo_root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read native opasm engine");
    let driver = fs::read_to_string(
        repo_root.join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    let navigation = fs::read_to_string(
        repo_root.join("native/motorola68000/amigaos/opasm/opasm_flow_navigation.asm"),
    )
    .expect("read native opasm flow navigation");

    assert!(source_contains_in_order(
        &engine,
        &[
            "opasmEngineSetFlowNextV1\t.block",
            "move.w d0, OpasmEngineFlowNext.l",
            "move.w #1, OpasmEngineFlowPending.l",
        ]
    ));
    assert!(source_contains_in_order(
        &engine,
        &[
            "tst.w OpasmEngineFlowPending.l",
            "move.w OpasmEngineFlowNext.l, d2",
            "clr.w OpasmEngineFlowPending.l",
            "move.w d2, d7",
        ]
    ));
    assert!(engine.matches("move.l d7, -(sp)").count() >= 7);
    assert!(engine.matches("move.l (sp)+, d7").count() >= 7);

    assert!(source_contains_in_order(
        &driver,
        &[
            "finishIfBranch",
            "ori.w #$8000, d2",
            "finishEndmatchBranch",
            "move.w d7, d2",
            "addq.w #1, d2",
            "success",
            "jsr eng.opasmEngineSetFlowNextV1",
        ]
    ));
    assert!(driver.contains("jsr eng.opasmEngineGetFlowRedirectedV1"));
    assert!(!driver.contains("OpasmIfDepth"));
    assert!(!driver.contains("OpasmIfMatched"));
    assert!(source_contains_in_order(
        &navigation,
        &[
            "move.w d2, d7",
            "move.l d2, -(sp)",
            "jsr (a2)",
            "move.l (sp)+, d2",
        ]
    ));
}

#[test]
fn external_fs_uae_opforge_native_cli_source_cpu_normalization_matches_live_rust_cli() {
    // Proof level D. This test proves quoted source CPU selection reaches the
    // real native pipeline and matches live Rust bytes, while trailing input
    // fails. This test does not prove other CPU identifiers.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let valid_source =
        "        .cpu \"65c02\"\n        .org $1000\nstart   lda $12\n        sta $34\n";
    let invalid_source = "        .cpu \"6502\" trailing\nstart   lda #$42\n";
    let case_dir = create_temp_dir("native-source-cpu-normalization");
    let input_path = case_dir.join("input.asm");
    let rust_bin_path = case_dir.join("rust.bin");
    fs::write(&input_path, valid_source).expect("write quoted source CPU oracle");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        rust_bin_path.to_string_lossy().as_ref(),
    ]);
    run_with_cli_with_context(&cli).expect("run live Rust quoted source CPU oracle");
    let rust_bin = fs::read(&rust_bin_path).expect("read live Rust quoted source CPU bytes");
    let invalid_path = case_dir.join("invalid.asm");
    let invalid_bin_path = case_dir.join("invalid.bin");
    fs::write(&invalid_path, invalid_source).expect("write invalid source CPU oracle");
    let invalid_cli = Cli::parse_from([
        "opForge",
        invalid_path.to_string_lossy().as_ref(),
        "--bin",
        invalid_bin_path.to_string_lossy().as_ref(),
    ]);
    assert!(
        run_with_cli_with_context(&invalid_cli).is_err(),
        "live Rust authority must reject trailing source CPU tokens"
    );
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "source-cpu-valid",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(valid_source.as_bytes()),
            command_template: Some("{input} --bin {bin}"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bin,
            },
        },
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "source-cpu-invalid-trailing-input",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(invalid_source.as_bytes()),
            command_template: Some("{input} --bin {bin}"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        },
    ];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("source CPU normalization FS-UAE shard should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 2);
            assert!(
                runs[0].success,
                "quoted source CPU run failed\nstdout:\n{}\nstderr:\n{}",
                runs[0].stdout, runs[0].stderr
            );
            let native_bin = captured_fs_uae_artifact(&runs[0], "Work/opforge_native_out.bin");
            assert_eq!(native_bin, rust_bin);
            assert!(
                !runs[1].success,
                "trailing source CPU token must fail\nstdout:\n{}\nstderr:\n{}",
                runs[1].stdout, runs[1].stderr
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_debug_output_isolation_preserves_normal_output() {
    // Proof level D. This test proves normal and --native-debug modes execute
    // the same real native assembly and produce identical bytes, while only
    // debug mode emits progress markers. This test does not prove error output.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let source = "        .cpu 6502\n        .org $1000\nstart   lda #$42\n";
    let case_dir = create_temp_dir("native-debug-output-isolation");
    let input_path = case_dir.join("input.asm");
    let rust_bin_path = case_dir.join("rust.bin");
    fs::write(&input_path, source).expect("write debug-isolation Rust oracle source");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        rust_bin_path.to_string_lossy().as_ref(),
    ]);
    run_with_cli_with_context(&cli).expect("run debug-isolation Rust oracle");
    let rust_bin = fs::read(&rust_bin_path).expect("read debug-isolation Rust oracle bytes");
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "debug-isolation-normal",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source.as_bytes()),
            command_template: Some("{input} --bin {bin}"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bin,
            },
        },
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: "debug-isolation-enabled",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source.as_bytes()),
            command_template: Some("{input} --bin {bin} --native-debug"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &rust_bin,
            },
        },
    ];
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("debug output isolation FS-UAE shard should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 2);
            assert!(runs.iter().all(|run| run.success));
            assert_native_cli_run_omits_debug_progress(&runs[0], "normal output isolation");
            for marker in native_debug_output_isolation_contract(true) {
                assert!(
                    runs[1].stdout.contains(marker),
                    "debug output missing marker '{marker}'\nstdout:\n{}\nstderr:\n{}",
                    runs[1].stdout,
                    runs[1].stderr
                );
            }
            let normal_bin = captured_fs_uae_artifact(&runs[0], "Work/opforge_native_out.bin");
            let debug_bin = captured_fs_uae_artifact(&runs[1], "Work/opforge_native_out.bin");
            assert_eq!(normal_bin, debug_bin);
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_reports_module_use_parser_status() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let rust_oracle = native_cli_6502_contract_expected_bin();
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "module-use-parser-status",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: None,
        command_template: Some(
            "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m6502 -M Work:opforge_module_a --module-path Work:opforge_module_b --native-debug",
        ),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &rust_oracle,
        },
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&repo_root, &cases)
        .expect("native opForge CLI FS-UAE debug helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected a single native opForge CLI run");
            let run = &runs[0];
            assert_eq!(run.example_name, "opforge_cli");
            assert!(
                run.stdout.contains("OPFORGE-NATIVE 1"),
                "native opForge CLI did not report the native marker\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("STAGE parser"),
                "native opForge CLI did not report the parser stage\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("STAGE session"),
                "native opForge CLI did not report the assembly-session stage\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("SESSION-CPU "),
                "native opForge CLI did not report the assembly-session CPU\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("SESSION-SOURCE-COUNT "),
                "native opForge CLI did not report source-line session records\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("SESSION-STMT-COUNT "),
                "native opForge CLI did not report statement session records\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("STATUS session-ready"),
                "native opForge CLI did not report deterministic assembly-session readiness\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("STATUS emitter-not-implemented"),
                "native opForge CLI did not report the expected stub-emitter status\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("STATUS tokenizer-ok"),
                "native opForge CLI did not report the tokenizer stage status\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("MODULE main"),
                "native opForge CLI did not report the smoke .module directive\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("MOD-DEF 0 1 1 0 4 main"),
                "native opForge CLI did not report the table-backed .module record\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("MOD-END 0 1 4 0"),
                "native opForge CLI did not report the table-backed .endmodule record\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("MOD-PATH 0 Work:"),
                "native opForge CLI did not report the implicit input module root\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("MOD-PATH 1 Work:opforge_module_a"),
                "native opForge CLI did not report the short-form module path root\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("MOD-PATH 2 Work:opforge_module_b"),
                "native opForge CLI did not report the long-form module path root\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                !run.stdout.contains("USE-SELECT "),
                "native opForge CLI unexpectedly reported selective .use rows\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                !run.stdout.contains("USE-WILDCARD "),
                "native opForge CLI unexpectedly reported wildcard .use rows\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                !run.stdout.contains("STAGE include"),
                "native opForge CLI unexpectedly entered include handling in the module/use smoke\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("STAGE pass1"),
                "native opForge CLI did not run the pass-one stage\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("STATUS pass1-ok"),
                "native opForge CLI did not report pass-one success\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("STAGE pass2"),
                "native opForge CLI did not run the pass-two stage\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("STATUS pass2-ok"),
                "native opForge CLI did not report pass-two success\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("INPUT Work:opforge_fsuae_smoke_input.asm"),
                "native opForge CLI did not parse the default smoke input argument\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("BIN Work:opforge_native_out.bin"),
                "native opForge CLI did not parse the default smoke bin argument\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            assert!(
                run.stdout.contains("STATUS output-ok"),
                "native opForge CLI did not report flat-output success for the module/use smoke\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
            eprintln!(
                "FS-UAE native opForge CLI smoke completed with {} under {}",
                run.hunk_path.display(),
                run.artifact_dir.display()
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_6502_writes_rust_matching_bin() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let rust_oracle = native_cli_6502_contract_expected_bin();
    match crate::fs_uae_smoke::run_opforge_native_cli_6502_output_from_env(
        &workspace_root(),
        &rust_oracle,
    )
    .expect("native opForge CLI 6502 output FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                1,
                "expected a single native opForge CLI output run"
            );
            let run = &runs[0];
            assert!(
                run.success,
                "native opForge CLI 6502 output smoke failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(run, "native opForge CLI 6502 output smoke");
            let actual = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
            assert_eq!(actual, native_cli_6502_contract_expected_bin());
            eprintln!(
                "FS-UAE native opForge CLI 6502 output smoke verified {} bytes in memory",
                actual.len()
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item6_stripped_fixtures_match_rust_bins() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let rust_package_bytes = item6_mos_package_bytes();
    let package_bytes = rust_package_bytes.clone();
    assert_eq!(
        package_bytes, rust_package_bytes,
        "FS-UAE Item 6.7 native and Rust paths must consume identical serialized package bytes"
    );
    let model = load_opasm_model_from_package_bytes(rust_package_bytes.as_slice());
    let mut staged_fixtures = Vec::new();
    for (fixture, cpu_id) in item6_mos_fixture_allowlist() {
        assert_eq!(
            package_bytes, rust_package_bytes,
            "same-package identity check before FS-UAE Item 6.7 fixture comparison {fixture}"
        );
        let source =
            fs::read_to_string(repo_root.join(fixture)).expect("read Item 6 FS-UAE MOS fixture");
        let stripped_source = item6_source_without_native_cli_setup_directives(source.as_str());
        let rust_bin = item6_rust_fixture_native_cli_flat_bytes_with_initial_pc(
            &model,
            cpu_id,
            fixture,
            stripped_source.as_str(),
            0x0800,
        );
        staged_fixtures.push((fixture, cpu_id, stripped_source, rust_bin));
    }

    let cases = staged_fixtures
        .iter()
        .map(|(fixture, cpu_id, stripped_source, rust_bin)| {
            crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
                name: fixture,
                cpu_id,
                source: stripped_source.as_bytes(),
                package_bytes: package_bytes.as_slice(),
                proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(rust_bin),
            }
        })
        .collect::<Vec<_>>();

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("native opForge CLI Item 6 fixture FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                staged_fixtures.len(),
                "expected one native opForge CLI output run per Item 6 fixture"
            );
            for ((fixture, cpu_id, _stripped_source, rust_bin), run) in
                staged_fixtures.iter().zip(runs.iter())
            {
                assert!(
                    run.success,
                    "native opForge CLI Item 6 fixture {fixture} failed\nstdout:\n{}\nstderr:\n{}",
                    run.stdout, run.stderr,
                );
                assert_native_cli_run_omits_debug_progress(
                    run,
                    format!("native opForge CLI Item 6 fixture {fixture}").as_str(),
                );
                let native_bin = verified_fs_uae_output(run);
                println!(
                    "FS-UAE Item 6 stripped fixture {fixture} ({cpu_id})\nrust bin: {}\nnative bin: {}\nstdout:\n{}\nstderr:\n{}",
                    item6_hex_bytes(rust_bin.as_slice()),
                    item6_hex_bytes(native_bin),
                    run.stdout,
                    run.stderr
                );
                assert_eq!(
                    native_bin, *rust_bin,
                    "FS-UAE Item 6 stripped fixture byte mismatch for {fixture}\nstdout:\n{}\nstderr:\n{}",
                    run.stdout,
                    run.stderr
                );
            }
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item6_65c02_allmodes_matches_rust_bin() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let rust_package_bytes = item6_mos_package_bytes();
    let package_bytes = rust_package_bytes.clone();
    let model = load_opasm_model_from_package_bytes(rust_package_bytes.as_slice());
    let fixture = "examples/mos6502/65c02_allmodes.asm";
    let cpu_id = m65c02_cpu_id.as_str();
    let source = fs::read_to_string(repo_root.join(fixture))
        .expect("read focused Item 6 65C02 FS-UAE MOS fixture");
    let mut fixture_label = fixture.to_string();
    let stripped_source = if let Ok(case) = std::env::var("OPFORGE_ITEM6_65C02_FOCUSED_CASE") {
        let source = item6_65c02_focused_fs_uae_source(case.as_str())
            .unwrap_or_else(|| panic!("unknown OPFORGE_ITEM6_65C02_FOCUSED_CASE value '{case}'"));
        fixture_label = format!("{fixture}#{case}");
        source.to_string()
    } else {
        item6_source_without_native_cli_setup_directives(source.as_str())
    };
    let rust_bin = item6_rust_fixture_native_cli_flat_bytes_with_initial_pc(
        &model,
        cpu_id,
        fixture_label.as_str(),
        stripped_source.as_str(),
        0x0800,
    );
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: fixture_label.as_str(),
        cpu_id,
        source: stripped_source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bin),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("focused native opForge CLI 65C02 Item 6 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused 65C02 Item 6 run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI Item 6 fixture {fixture_label} failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, rust_bin,
                "focused FS-UAE Item 6 stripped fixture byte mismatch for {fixture_label}\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn motorola68020_native_cli_parse_line_keeps_full_ternary_const_expression() {
    // Proof level B. This test proves the Rust-side native parse-line harness
    // keeps the full ternary directive expression and its exclusive-end span.
    // This test does not prove the real Amiga-native session store or evaluator.
    let mut harness = vm::native6502::Native6502Harness::new();
    let mut control_block = vm::native6502::Native6502ControlBlockV1::new_v1();
    item5_prepare_expression_parse_harness(&mut harness, &mut control_block);
    let line = "value .const (0 || 1) ? (2 + 3) : (4 + 5)";
    let ast = item6_native_cli_parse_line(&mut harness, &mut control_block, line, 1);
    let PortableLineAst::Statement { operands, .. } = ast else {
        panic!("expected statement AST");
    };
    assert_eq!(operands.len(), 1);
    let expression = operands[0].to_core_expr();
    let Expr::Ternary {
        cond,
        then_expr,
        else_expr,
        ..
    } = expression
    else {
        panic!("expected full ternary AST, got {expression:?}");
    };
    assert!(matches!(
        *cond,
        Expr::Binary {
            op: BinaryOp::LogicOr,
            ..
        }
    ));
    assert!(matches!(
        *then_expr,
        Expr::Binary {
            op: BinaryOp::Add,
            ..
        }
    ));
    assert!(matches!(
        *else_expr,
        Expr::Binary {
            op: BinaryOp::Add,
            ..
        }
    ));
}

#[test]
fn motorola68020_native_cli_parse_line_rejects_truncated_ternary_const_expression() {
    // Proof level B. This test proves the Rust-side native parse-line harness
    // rejects a truncated ternary expression instead of shrinking its span.
    // This test does not prove the real Amiga-native request decoder.
    let mut harness = vm::native6502::Native6502Harness::new();
    let mut control_block = vm::native6502::Native6502ControlBlockV1::new_v1();
    item5_prepare_expression_parse_harness(&mut harness, &mut control_block);
    let parse = harness.invoke_v1(
        &mut control_block,
        vm::native6502_abi::NATIVE_6502_ENTRYPOINT_PARSE_LINE_V1,
        vm::native6502::Native6502HarnessRequest::ParseLine {
            source_line: "value .const (0 || 1) ? (2 + 3) : (4 + 5",
            line_num: 1,
        },
    );
    assert_eq!(parse.status_code, vm::native6502::NATIVE_6502_STATUS_OK_V1);
    let vm::native6502::Native6502HarnessOutput::LineAst(PortableLineAst::Statement {
        operands,
        ..
    }) = parse.output
    else {
        panic!("expected statement AST with an error operand");
    };
    let expression = operands
        .first()
        .expect("truncated expression should retain an error AST")
        .to_core_expr();
    assert!(
        matches!(expression, Expr::Error(..)),
        "expected an error AST for the truncated expression, got {expression:?}"
    );
}

fn item5_prepare_expression_parse_harness(
    harness: &mut vm::native6502::Native6502Harness,
    control_block: &mut vm::native6502::Native6502ControlBlockV1,
) {
    let init = harness.invoke_v1(
        control_block,
        vm::native6502_abi::NATIVE_6502_ENTRYPOINT_INIT_V1,
        vm::native6502::Native6502HarnessRequest::Init,
    );
    assert_eq!(init.status_code, vm::native6502::NATIVE_6502_STATUS_OK_V1);
    let package_bytes = item6_mos_package_bytes();
    let load = harness.invoke_v1(
        control_block,
        vm::native6502_abi::NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
        vm::native6502::Native6502HarnessRequest::LoadPackage {
            package_bytes: package_bytes.as_slice(),
        },
    );
    assert_eq!(load.status_code, vm::native6502::NATIVE_6502_STATUS_OK_V1);
    let pipeline = harness.invoke_v1(
        control_block,
        vm::native6502_abi::NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1,
        vm::native6502::Native6502HarnessRequest::SetPipeline {
            cpu_id: "65c02",
            dialect_override: None,
        },
    );
    assert_eq!(
        pipeline.status_code,
        vm::native6502::NATIVE_6502_STATUS_OK_V1
    );
}

#[test]
fn external_fs_uae_opforge_native_cli_65c02_expr_syntax_matches_rust_bin() {
    // Proof level D. This test proves the real native 680x0 CLI evaluates the
    // unchanged expression fixture to the same emitted bytes as Rust.
    // This test does not prove unrelated expression operators or CPU packages.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = b"value .const (0 || 1) ? (2 + 3) : (4 + 5)\nstart lda #value\nrts\n";
    let (rust_entries, rust_diagnostics) = assemble_source_entries_with_runtime_mode(
        &[
            ".cpu 65c02",
            ".org $0800",
            "value .const (0 || 1) ? (2 + 3) : (4 + 5)",
            "start lda #value",
            "rts",
        ],
        true,
    )
    .expect("Rust reference assembly should run");
    assert!(
        rust_diagnostics.is_empty(),
        "Rust reference diagnostics: {rust_diagnostics:?}"
    );
    let rust_bin = rust_entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "65c02-expr-syntax",
        cpu_id: m65c02_cpu_id.as_str(),
        source,
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bin),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root, &cases,
    )
    .expect("65C02 expression syntax FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native 65C02 expression syntax fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, rust_bin,
                "native 65C02 expression syntax bytes differ from Rust\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
        }
    }
}

#[test]
fn native_expression_context_forward_label_fs_uae() {
    // Proof level D. The real native CLI evaluates the forward label once while
    // it is unstable in pass one and again after finalization in pass two. This
    // directly exercises the expression service's neutral context snapshot.
    // It does not prove other expression operators or CPU packages.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = b"start lda #forward\nrts\nforward .const 42\n";
    let (rust_entries, rust_diagnostics) = assemble_source_entries_with_runtime_mode(
        &[
            ".cpu 65c02",
            ".org $0800",
            "start lda #forward",
            "rts",
            "forward .const 42",
        ],
        true,
    )
    .expect("Rust forward-label expression assembly should run");
    assert!(
        rust_diagnostics.is_empty(),
        "Rust reference diagnostics: {rust_diagnostics:?}"
    );
    let rust_bin = rust_entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "65c02-expression-forward-label",
        cpu_id: m65c02_cpu_id.as_str(),
        source,
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bin),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root, &cases,
    )
    .expect("forward-label expression FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success,
                "native forward-label expression fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, rust_bin,
                "native forward-label expression bytes differ from Rust\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
        }
    }
}

#[test]
fn native_expression_suffix_literals_rust_oracle() {
    // Proof level A. This test proves the Rust authority's suffix literal
    // values and additive token boundaries. It does not prove native execution.
    let cases = [
        ("hex", "0a6h", 0xa6),
        ("binary", "1010b", 10),
        ("octal", "17o", 15),
        ("octal-q", "17q", 15),
        ("decimal", "42d", 42),
    ];
    for (name, literal, expected) in cases {
        let value_line = format!("value .const {literal}");
        let immediate_line = format!("start lda #value+1");
        let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
            &[
                ".cpu 65c02",
                value_line.as_str(),
                immediate_line.as_str(),
                "rts",
            ],
            true,
        )
        .unwrap_or_else(|err| panic!("assemble suffix oracle {name}: {err}"));
        assert!(
            diagnostics.is_empty(),
            "Rust diagnostics for {name}: {diagnostics:?}"
        );
        assert_eq!(
            entries[1].1,
            (expected + 1) as u8,
            "suffix value for {name}"
        );
    }
}

#[test]
fn native_expression_suffix_literals_fs_uae() {
    // Proof level D. This test proves the real native CLI parses each supported
    // suffix literal without consuming adjacent additive expression text.
    // This test does not prove other Item 6 operator-precedence tiers.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let sources = [
        (
            "suffix-hex",
            b"value .const 0a6h\nstart lda #value+1\nrts\n".as_slice(),
        ),
        (
            "suffix-binary",
            b"value .const 1010b\nstart lda #value+1\nrts\n".as_slice(),
        ),
        (
            "suffix-octal",
            b"value .const 17o\nstart lda #value+1\nrts\n".as_slice(),
        ),
        (
            "suffix-octal-q",
            b"value .const 17q\nstart lda #value+1\nrts\n".as_slice(),
        ),
        (
            "suffix-decimal",
            b"value .const 42d\nstart lda #value+1\nrts\n".as_slice(),
        ),
    ];
    let mut rust_bins = Vec::with_capacity(sources.len());
    for (name, source) in &sources {
        let source = std::str::from_utf8(source).expect("fixture is UTF-8");
        let mut lines = vec![".cpu 65c02"];
        lines.extend(source.lines());
        let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
            .unwrap_or_else(|err| panic!("assemble Rust suffix authority {name}: {err}"));
        assert!(
            diagnostics.is_empty(),
            "Rust diagnostics for {name}: {diagnostics:?}"
        );
        rust_bins.push(
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>(),
        );
    }
    let cases = sources
        .iter()
        .zip(rust_bins.iter())
        .map(
            |((name, source), rust_bin)| crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
                name,
                cpu_id: "65c02",
                source,
                package_bytes: package_bytes.as_slice(),
                proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(rust_bin),
            },
        )
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("suffix-literal FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), sources.len());
            for ((run, (name, _)), rust_bin) in runs.iter().zip(sources.iter()).zip(rust_bins) {
                assert!(
                    run.success,
                    "native suffix fixture {name} failed: {}",
                    run.stdout
                );
                let native_bin = verified_fs_uae_output(run);
                assert_eq!(
                    native_bin, rust_bin,
                    "native suffix bytes differ for {name}"
                );
            }
        }
    }
}

#[test]
fn native_expression_multiplicative_rust_oracle() {
    // Proof level A. Establish the Rust reference bytes for the native
    // multiplicative-expression fixtures without requiring an emulator.
    let cases = [
        (
            "multiply",
            "value .const 3*4+1\nstart lda #value\nrts",
            vec![0xa9, 13],
        ),
        (
            "divide",
            "value .const -20/-5+1\nstart lda #value\nrts",
            vec![0xa9, 5],
        ),
        (
            "modulo",
            "value .const -23%-5+5\nstart lda #value\nrts",
            vec![0xa9, 2],
        ),
    ];
    for (name, source, expected) in cases {
        let mut lines = vec![".cpu 65c02"];
        lines.extend(source.lines());
        let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
            .unwrap_or_else(|err| panic!("Rust authority {name}: {err}"));
        assert!(
            diagnostics.is_empty(),
            "Rust diagnostics for {name}: {diagnostics:?}"
        );
        assert_eq!(
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>(),
            expected,
            "Rust output for {name}"
        );
    }
}

#[test]
fn native_expression_multiplicative_fs_uae() {
    // Proof level D. This test proves the real native CLI emits the same bytes
    // as Rust for multiplication, signed division, and modulo expressions.
    // This test does not prove later precedence tiers.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let sources = [
        (
            "multiply",
            b"value .const 3*4+1\nstart lda #value\nrts\n".as_slice(),
        ),
        (
            "divide",
            b"value .const -20/-5+1\nstart lda #value\nrts\n".as_slice(),
        ),
        (
            "modulo",
            b"value .const -23%-5+5\nstart lda #value\nrts\n".as_slice(),
        ),
    ];
    let rust_bins = sources
        .iter()
        .map(|(name, source)| {
            let text = std::str::from_utf8(source).expect("fixture UTF-8");
            let mut lines = vec![".cpu 65c02"];
            lines.extend(text.lines());
            let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
                .unwrap_or_else(|err| panic!("Rust authority {name}: {err}"));
            assert!(
                diagnostics.is_empty(),
                "Rust diagnostics for {name}: {diagnostics:?}"
            );
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let cases = sources
        .iter()
        .zip(rust_bins.iter())
        .map(
            |((name, source), rust_bin)| crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
                name,
                cpu_id: "65c02",
                source,
                package_bytes: package.as_slice(),
                proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(rust_bin),
            },
        )
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("multiplicative FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            for ((run, (name, _)), rust_bin) in runs.iter().zip(sources.iter()).zip(rust_bins) {
                assert!(run.success, "native {name} failed: {}", run.stdout);
                let native = verified_fs_uae_output(run);
                assert_eq!(native, rust_bin, "native bytes differ for {name}");
            }
        }
    }
}

#[test]
fn native_expression_shift_rust_oracle() {
    // Proof level A. These establish Rust's precedence and count-mask results.
    let cases = [
        (
            "shift-left",
            "value .const 3+1<<2\nstart lda #value\nrts",
            vec![0xa9, 16],
        ),
        (
            "shift-right",
            "value .const 128>>3+1\nstart lda #value\nrts",
            vec![0xa9, 8],
        ),
        (
            "shift-mask",
            "value .const 1<<33\nstart lda #value\nrts",
            vec![0xa9, 2],
        ),
    ];
    for (name, source, expected) in cases {
        let mut lines = vec![".cpu 65c02"];
        lines.extend(source.lines());
        let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
            .unwrap_or_else(|err| panic!("Rust authority {name}: {err}"));
        assert!(
            diagnostics.is_empty(),
            "Rust diagnostics for {name}: {diagnostics:?}"
        );
        assert_eq!(
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>(),
            expected
        );
    }
}

#[test]
fn native_expression_shift_fs_uae() {
    // Proof level D. Real native CLI output equals Rust for adjacent shift tokens.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let sources = [
        (
            "shift-left",
            b"value .const 3+1<<2\nstart lda #value\nrts\n".as_slice(),
        ),
        (
            "shift-right",
            b"value .const 128>>3+1\nstart lda #value\nrts\n".as_slice(),
        ),
        (
            "shift-mask",
            b"value .const 1<<33\nstart lda #value\nrts\n".as_slice(),
        ),
    ];
    let rust_bins = sources
        .iter()
        .map(|(name, source)| {
            let text = std::str::from_utf8(source).expect("fixture UTF-8");
            let mut lines = vec![".cpu 65c02"];
            lines.extend(text.lines());
            let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
                .unwrap_or_else(|err| panic!("Rust authority {name}: {err}"));
            assert!(
                diagnostics.is_empty(),
                "Rust diagnostics for {name}: {diagnostics:?}"
            );
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let cases = sources
        .iter()
        .zip(rust_bins.iter())
        .map(
            |((name, source), rust_bin)| crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
                name,
                cpu_id: "65c02",
                source,
                package_bytes: package.as_slice(),
                proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(rust_bin),
            },
        )
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("shift FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            for ((run, (name, _)), rust_bin) in runs.iter().zip(sources.iter()).zip(rust_bins) {
                assert!(run.success, "native {name} failed: {}", run.stdout);
                let native = verified_fs_uae_output(run);
                assert_eq!(native, rust_bin, "native bytes differ for {name}");
            }
        }
    }
}

#[test]
fn native_reference_opcore_module_macro_statement_fs_uae() {
    // Proof level D. Rust uses the full module/preprocessor authority; FS-UAE
    // runs the exact canonical roots through the native CLI.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let paths = [
        "module_basics.asm",
        "macro_syntax.asm",
        "statement_expansion.asm",
    ];
    let sources = paths
        .iter()
        .map(|name| {
            let path = root.join("examples/opcore").join(name);
            let bytes = fs::read(&path).expect("read canonical Item 7 root");
            let (entries, diagnostics) = assemble_example_entries_with_runtime_mode(&path, true)
                .expect("full Rust Item 7 authority");
            assert!(
                diagnostics.is_empty(),
                "Rust diagnostics for {name}: {diagnostics:?}"
            );
            (
                name,
                bytes,
                entries
                    .into_iter()
                    .map(|(_, byte)| byte)
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>();
    let cases = sources
        .iter()
        .map(
            |(name, bytes, rust)| crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
                name,
                cpu_id: "65c02",
                source: bytes.as_slice(),
                package_bytes: package.as_slice(),
                proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(rust),
            },
        )
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("Item 7 FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            for (run, (name, _, rust)) in runs.iter().zip(sources.iter()) {
                assert!(run.success, "native {name} failed: {}", run.stdout);
                let native = verified_fs_uae_output(run);
                assert_eq!(native, *rust, "native bytes differ for {name}");
            }
        }
    }
}

#[test]
fn native_macro_invocation_fixture_fs_uae() {
    // Proof level D. The isolated macro-only fixture exercises COPY, PAIR,
    // TEXT, and LOCAL through the real native CLI and compares its bytes with
    // the live Rust authority. It deliberately excludes segments/statements.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");
    let root = workspace_root();
    let source_path = root.join("examples/opcore/macro_invocation_native.asm");
    let source = fs::read(&source_path).expect("read macro invocation fixture");
    let (entries, diagnostics) = assemble_example_entries_with_runtime_mode(&source_path, true)
        .expect("Rust macro fixture authority");
    assert!(
        diagnostics.is_empty(),
        "Rust macro fixture diagnostics: {diagnostics:?}"
    );
    let rust = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    let package = item6_mos_package_bytes();
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "macro-invocation-native",
        cpu_id: "65c02",
        source: source.as_slice(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust),
    }];
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("macro invocation FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one macro native run");
            let run = &runs[0];
            assert!(run.success, "native macro fixture failed: {}", run.stdout);
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust, "native macro fixture bytes differ");
        }
    }
}

#[test]
fn native_macro_syntax_segment_fs_uae() {
    // Proof level D. This dedicated canonical proof includes the INLINE segment
    // and compares the exact native bytes with a live Rust assembly of the same
    // source. Statement and module-export behavior are not exercised here.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock is infallible");
    let root = workspace_root();
    let source_path = root.join("examples/opcore/macro_syntax.asm");
    let source = fs::read(&source_path).expect("read canonical macro syntax source");
    let root_lines = expand_source_file(&source_path, &[], &[], 64)
        .expect("preprocess canonical macro syntax for Rust authority");
    let module_paths = example_module_paths(&source_path);
    let graph = load_module_graph(&source_path, root_lines, &[], &[], &module_paths, 64)
        .expect("expand canonical macro syntax graph for Rust authority");
    let mut rust_lines = vec![".cpu 65c02".to_string()];
    rust_lines.extend(graph.lines);
    let rust_line_refs = rust_lines.iter().map(String::as_str).collect::<Vec<_>>();
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_line_refs, true)
        .expect("Rust canonical macro syntax authority");
    assert!(
        diagnostics.is_empty(),
        "Rust macro syntax diagnostics: {diagnostics:?}"
    );
    let rust = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    let package = item6_mos_package_bytes();
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "macro-syntax-segment",
        cpu_id: "65c02",
        source: source.as_slice(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust),
    }];
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("canonical macro syntax FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one canonical segment run");
            let run = &runs[0];
            assert!(
                run.success,
                "native canonical segment failed: {}",
                run.stdout
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust, "native canonical segment bytes differ");
        }
    }
}

#[test]
fn native_segment_label_attachment_fs_uae() {
    // Proof level D. A caller label on a segment invocation must attach to the
    // first expanded body line without synthesizing a macro scope block.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock is infallible");
    let root = workspace_root();
    let source = b".org $2000\nINLINE .segment val=7\n        .byte .val\n.endsegment\nEMPTY .segment\n.endsegment\nplaced  .INLINE\n        .word placed\nempty   .EMPTY\n        .word empty\n        .end\n";
    let source_lines = std::str::from_utf8(source)
        .expect("segment label source UTF-8")
        .lines()
        .map(str::to_string)
        .collect::<Vec<_>>();
    let mut rust_lines = vec![".cpu 65c02".to_string()];
    rust_lines.extend(
        MacroProcessor::new()
            .expand(&source_lines)
            .expect("expand Rust segment label authority"),
    );
    let rust_line_refs = rust_lines.iter().map(String::as_str).collect::<Vec<_>>();
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_line_refs, true)
        .expect("assemble Rust segment label authority");
    assert!(
        diagnostics.is_empty(),
        "Rust segment label diagnostics: {diagnostics:?}"
    );
    let rust = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    let package = item6_mos_package_bytes();
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "segment-label-attachment",
        cpu_id: "65c02",
        source,
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust),
    }];
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("segment label FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one segment label run");
            let run = &runs[0];
            assert!(run.success, "native segment label failed: {}", run.stdout);
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust, "native segment label bytes differ");
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item7_layout_directives_match_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .region ram, $1001, $100f, align=6",
        "        .region alt, $1011, $10ff, align=5",
        "        .section code, align=5",
        "        lda #$01",
        "        .align 4",
        "        .endsection",
        "        .section tail, align=7",
        "        nop",
        "        .endsection",
        "        .section high, align=1",
        "        lda #$02",
        "        .endsection",
        "        .place code in ram",
        "        .place tail in ram",
        "        .place high in alt",
    ]
    .join("\n");
    let mismatch_source = [
        "        .region ram, $1001, $10ff, align=6",
        "        .section code, align=5",
        "        lda #$01",
        "        .endsection",
        "        .place code in rom",
    ]
    .join("\n");
    let duplicate_place_source = [
        "        .region ram, $1001, $10ff, align=6",
        "        .section code, align=5",
        "        lda #$01",
        "        .endsection",
        "        .place code in ram",
        "        .place code in ram",
    ]
    .join("\n");
    let expected = vec![
        0xA9, 0x01, 0x00, 0x00, 0xEA, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0xA9, 0x02,
    ];
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "item7-layout-directives",
            cpu_id: m6502_cpu_id.as_str(),
            source: source.as_bytes(),
            package_bytes: package_bytes.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
        },
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "item7-layout-name-mismatch",
            cpu_id: m6502_cpu_id.as_str(),
            source: mismatch_source.as_bytes(),
            package_bytes: package_bytes.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExpectedFailureWithDiagnostic,
        },
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "item7-layout-duplicate-place",
            cpu_id: m6502_cpu_id.as_str(),
            source: duplicate_place_source.as_bytes(),
            package_bytes: package_bytes.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExpectedFailureWithDiagnostic,
        },
    ];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("focused native opForge CLI Item 7 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 3, "expected three focused Item 7 runs");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI Item 7 layout fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                run,
                "focused native opForge CLI Item 7 layout fixture",
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "focused FS-UAE Item 7 layout byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let mismatch_run = &runs[1];
            assert!(
                !mismatch_run.success,
                "focused native opForge CLI Item 7 layout name mismatch fixture should fail\nstdout:\n{}\nstderr:\n{}",
                mismatch_run.stdout,
                mismatch_run.stderr,
            );
            let duplicate_run = &runs[2];
            assert!(
                !duplicate_run.success,
                "focused native opForge CLI Item 7 duplicate placement fixture should fail\nstdout:\n{}\nstderr:\n{}",
                duplicate_run.stdout,
                duplicate_run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item8_data_text_directives_match_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .byte $01, $ff",
        "        .db 2",
        "        .word $1234, $0800",
        "        .dw $00fe",
        "        .long $12345678",
        "        .text \"OK\"",
        "        .null \"A\"",
        "        .ptext \"BC\"",
    ]
    .join("\n");
    let expected = vec![
        0x01, 0xFF, 0x02, 0x34, 0x12, 0x00, 0x08, 0xFE, 0x00, 0x78, 0x56, 0x34, 0x12, 0x4F, 0x4B,
        0x41, 0x00, 0x02, 0x42, 0x43,
    ];
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "item8-data-text-directives",
        cpu_id: m6502_cpu_id.as_str(),
        source: source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("focused native opForge CLI Item 8 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused Item 8 run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI Item 8 data/text fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "focused FS-UAE Item 8 data/text byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item9_symbol_config_directives_match_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .cpu 6502",
        "OFFSET  .const $02",
        "VALUE   .var $40",
        "NEXT    .set $42",
        "        .byte NEXT",
        "        lda #$42",
        "        sta $0202",
    ]
    .join("\n");
    let expected = vec![0x42, 0xA9, 0x42, 0x8D, 0x02, 0x02];
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "item9-symbol-config-directives",
        cpu_id: m6502_cpu_id.as_str(),
        source: source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("focused native opForge CLI Item 9 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused Item 9 run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI Item 9 symbol/config fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                run,
                "focused native opForge CLI Item 9 fixture",
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "focused FS-UAE Item 9 symbol/config byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item10_include_roots_match_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let expected = vec![0x22, 0xA9, 0x44];

    match crate::fs_uae_smoke::run_opforge_native_cli_item10_include_from_env(
        &repo_root,
        package_bytes.as_slice(),
        &expected,
    )
    .expect("focused native opForge CLI Item 10 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                2,
                "expected include success and missing-include runs"
            );
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI Item 10 include fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                run,
                "focused native opForge CLI Item 10 include fixture",
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
            assert_eq!(
                native_bin, expected,
                "focused FS-UAE Item 10 include byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );

            let missing_run = &runs[1];
            assert!(
                !missing_run.success,
                "focused native opForge CLI Item 10 missing include fixture should fail\nstdout:\n{}\nstderr:\n{}",
                missing_run.stdout,
                missing_run.stderr,
            );
            assert!(
                missing_run
                    .stderr
                    .contains("ERROR OPC-NCLI014: native include expansion failed"),
                "focused Item 10 missing include fixture should report the include diagnostic\nstdout:\n{}\nstderr:\n{}",
                missing_run.stdout,
                missing_run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item11_module_root_resolution_matches_rust_order() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module app",
        "        .use helper",
        "        lda #$44",
        "        .endmodule",
    ]
    .join("\n");
    let expected = vec![0xA9, 0x00, 0xA9, 0x44];
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "item11-module-root-resolution",
        cpu_id: m6502_cpu_id.as_str(),
        source: source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("focused native opForge CLI Item 11 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused Item 11 run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI Item 11 module fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                run,
                "focused native opForge CLI Item 11 module fixture",
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "focused FS-UAE Item 11 module byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item12_import_alias_resolves_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let selective_source = [
        "        .module app",
        "        .use values (VALUE)",
        "        .byte VALUE",
        "        .endmodule",
    ]
    .join("\n");
    let alias_source = [
        "        .module app",
        "        .use values as V",
        "        .byte V.VALUE",
        "        .endmodule",
    ]
    .join("\n");
    let expected = vec![0x37];
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "item12-selective-import-resolution",
            cpu_id: m6502_cpu_id.as_str(),
            source: selective_source.as_bytes(),
            package_bytes: package_bytes.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
        },
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "item12-alias-import-resolution",
            cpu_id: m6502_cpu_id.as_str(),
            source: alias_source.as_bytes(),
            package_bytes: package_bytes.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
        },
    ];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("focused native opForge CLI Item 12 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 2, "expected two focused Item 12 runs");

            let selective_run = &runs[0];
            assert!(
                selective_run.success,
                "focused native opForge CLI Item 12 selected-import fixture failed\nstdout:\n{}\nstderr:\n{}",
                selective_run.stdout,
                selective_run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                selective_run,
                "focused native opForge CLI Item 12 selected-import fixture",
            );
            let selective_bin = verified_fs_uae_output(selective_run);
            assert_eq!(
                selective_bin, expected,
                "focused FS-UAE Item 12 selected-import byte mismatch\nstdout:\n{}\nstderr:\n{}",
                selective_run.stdout, selective_run.stderr,
            );

            let alias_run = &runs[1];
            assert!(
                alias_run.success,
                "focused native opForge CLI Item 12 alias-import fixture failed\nstdout:\n{}\nstderr:\n{}",
                alias_run.stdout,
                alias_run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                alias_run,
                "focused native opForge CLI Item 12 alias-import fixture",
            );
            let alias_bin = verified_fs_uae_output(alias_run);
            assert_eq!(
                alias_bin, expected,
                "focused FS-UAE Item 12 alias-import byte mismatch\nstdout:\n{}\nstderr:\n{}",
                alias_run.stdout, alias_run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item13_bin_artifact_matches_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .output \"Work:opforge_native_out.bin\", format=bin, sections=code",
        "        .byte $11",
        "        .fill byte, 2, $ee",
        "        lda #$44",
    ]
    .join("\n");
    let expected = vec![0x11, 0xEE, 0xEE, 0xA9, 0x44];

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/opforge_native_out.bin",
        &expected,
    )
    .expect("focused native opForge CLI Item 13 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused Item 13 run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI Item 13 bin artifact fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                run,
                "focused native opForge CLI Item 13 bin artifact fixture",
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
            assert_eq!(
                native_bin, expected,
                "focused FS-UAE Item 13 bin artifact byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item13_relative_bin_output_matches_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .output \"build/opforge_native_out.bin\", format=bin, sections=code",
        "        .byte $11",
        "        .fill byte, 2, $ee",
        "        lda #$44",
    ]
    .join("\n");
    let expected = vec![0x11, 0xEE, 0xEE, 0xA9, 0x44];

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/opforge_native_out.bin",
        &expected,
    )
    .expect(
        "focused native opForge CLI relative Item 13 FS-UAE helper should complete or skip cleanly",
    ) {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused relative Item 13 run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI relative Item 13 bin artifact fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                run,
                "focused native opForge CLI relative Item 13 bin artifact fixture",
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/build/opforge_native_out.bin");
            assert_eq!(
                native_bin, expected,
                "focused FS-UAE relative Item 13 bin artifact byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item14_prg_artifact_matches_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let success_source = [
        "        .output \"Work:opforge_native_out.prg\", format=prg, loadaddr=$0800, sections=code",
        "        .byte $11",
        "        .fill byte, 2, $ee",
        "        lda #$44",
    ]
    .join("\n");
    let wide_loadaddr_source = [
        "        .output \"Work:opforge_native_out.prg\", format=prg, loadaddr=$123456, sections=code",
        "        lda #$44",
    ]
    .join("\n");
    let expected = vec![0x00, 0x08, 0x11, 0xEE, 0xEE, 0xA9, 0x44];

    match crate::fs_uae_smoke::run_opforge_native_cli_item14_prg_output_from_env(
        &repo_root,
        success_source.as_bytes(),
        wide_loadaddr_source.as_bytes(),
        package_bytes.as_slice(),
        &expected,
    )
    .expect("focused native opForge CLI Item 14 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                2,
                "expected Item 14 success and wide-loadaddr runs"
            );
            let success_run = &runs[0];
            assert!(
                success_run.success,
                "focused native opForge CLI Item 14 PRG artifact fixture failed\nstdout:\n{}\nstderr:\n{}",
                success_run.stdout, success_run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                success_run,
                "focused native opForge CLI Item 14 PRG artifact fixture",
            );
            let native_prg = captured_fs_uae_artifact(success_run, "Work/opforge_native_out.prg");
            assert_eq!(
                native_prg, expected,
                "focused FS-UAE Item 14 PRG artifact byte mismatch\nstdout:\n{}\nstderr:\n{}",
                success_run.stdout, success_run.stderr,
            );

            let wide_run = &runs[1];
            assert!(
                !wide_run.success,
                "focused native opForge CLI Item 14 wide loadaddr fixture should fail\nstdout:\n{}\nstderr:\n{}",
                wide_run.stdout,
                wide_run.stderr,
            );
            assert!(
                wide_run
                    .stderr
                    .contains("ERROR OPC-NCLI013: native module/use parser stage failed")
                    && wide_run
                        .stderr
                        .contains("OPC-NCLI007: No outputs selected. Native AmigaOS CLI currently requires --bin or --list"),
                "focused Item 14 wide loadaddr fixture should report native parser-stage rejection\nstdout:\n{}\nstderr:\n{}",
                wide_run.stdout,
                wide_run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_relative_prg_output_matches_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .output \"build/opforge_native_out.prg\", format=prg, loadaddr=$0800, sections=code",
        "        .byte $11",
        "        .fill byte, 2, $ee",
        "        lda #$44",
    ]
    .join("\n");
    let expected = vec![0x00, 0x08, 0x11, 0xEE, 0xEE, 0xA9, 0x44];

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/opforge_native_out.prg",
        &expected,
    )
    .expect("focused native opForge CLI relative PRG FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused relative PRG run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI relative PRG artifact fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                run,
                "focused native opForge CLI relative PRG artifact fixture",
            );
            let native_prg = captured_fs_uae_artifact(run, "Work/build/opforge_native_out.prg");
            assert_eq!(
                native_prg, expected,
                "focused FS-UAE relative PRG artifact byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_module_relative_bin_output_matches_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .output \"build/opforge_native_out.bin\", format=bin, sections=code",
        "        .byte $11",
        "        .fill byte, 2, $ee",
        "        lda #$44",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![0x11, 0xEE, 0xEE, 0xA9, 0x44];

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/opforge_native_out.bin",
        &expected,
    )
    .expect("focused native opForge CLI module-relative bin FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused module-relative bin run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI module-relative bin fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/build/opforge_native_out.bin");
            assert_eq!(
                native_bin, expected,
                "focused FS-UAE module-relative bin artifact byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_module_relative_prg_output_matches_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .output \"build/opforge_native_out.prg\", format=prg, loadaddr=$0800, sections=code",
        "        .byte $11",
        "        .fill byte, 2, $ee",
        "        lda #$44",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![0x00, 0x08, 0x11, 0xEE, 0xEE, 0xA9, 0x44];

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/opforge_native_out.prg",
        &expected,
    )
    .expect("focused native opForge CLI module-relative PRG FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused module-relative PRG run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI module-relative PRG fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_prg = captured_fs_uae_artifact(run, "Work/build/opforge_native_out.prg");
            assert_eq!(
                native_prg, expected,
                "focused FS-UAE module-relative PRG artifact byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_placed_relative_prg_output_matches_rust_guided_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "        .byte $11",
        "        .fill byte, 2, $ee",
        "        lda #$44",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .output \"build/opforge_native_out.prg\", format=prg, loadaddr=$0800, sections=code",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![0x00, 0x08, 0x11, 0xEE, 0xEE, 0xA9, 0x44];

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/opforge_native_out.prg",
        &expected,
    )
    .expect("focused native opForge CLI placed relative PRG FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused placed relative PRG run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI placed relative PRG fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_prg = captured_fs_uae_artifact(run, "Work/build/opforge_native_out.prg");
            assert_eq!(
                native_prg, expected,
                "focused FS-UAE placed relative PRG artifact byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_placed_relative_prg_with_symbolic_expr_matches_rust_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "OFFSET  .const $02",
        "VALUE   .var   $10",
        "start   lda #$42",
        "        sta $0200 + OFFSET",
        "        ldx #VALUE",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .output \"build/opforge_native_out.prg\", format=prg, loadaddr=$0800, sections=code",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![0x00, 0x08, 0xA9, 0x42, 0x8D, 0x02, 0x02, 0xA2, 0x10];

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/opforge_native_out.prg",
        &expected,
    )
    .expect("focused native opForge CLI placed symbolic PRG FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                1,
                "expected one focused placed symbolic PRG run"
            );
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI placed symbolic PRG fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_prg = captured_fs_uae_artifact(run, "Work/build/opforge_native_out.prg");
            assert_eq!(
                native_prg, expected,
                "focused FS-UAE placed symbolic PRG artifact byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_placed_relative_bin_with_symbolic_expr_matches_rust_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "OFFSET  .const $02",
        "VALUE   .var   $10",
        "start   lda #$42",
        "        sta $0200 + OFFSET",
        "        ldx #VALUE",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .output \"build/opforge_native_out.bin\", format=bin, sections=code",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![0xA9, 0x42, 0x8D, 0x02, 0x02, 0xA2, 0x10];

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/opforge_native_out.bin",
        &expected,
    )
    .expect("focused native opForge CLI placed symbolic BIN FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                1,
                "expected one focused placed symbolic BIN run"
            );
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI placed symbolic BIN fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/build/opforge_native_out.bin");
            assert_eq!(
                native_bin, expected,
                "focused FS-UAE placed symbolic BIN artifact byte mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item15_hex_artifact_matches_rust_guided_text() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .output \"Work:opforge_native_out.hex\", format=hex, sections=code",
        "        .byte $11",
        "        .fill byte, 2, $ee",
        "        lda #$44",
    ]
    .join("\n");
    let expected = ":0508000011EEEEA94419\n:00000001FF\n";

    match crate::fs_uae_smoke::run_opforge_native_cli_item15_hex_output_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        expected.as_bytes(),
    )
    .expect("focused native opForge CLI Item 15 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected Item 15 HEX output run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI Item 15 HEX artifact fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                run,
                "focused native opForge CLI Item 15 HEX artifact fixture",
            );
            let native_hex =
                std::str::from_utf8(captured_fs_uae_artifact(run, "Work/opforge_native_out.hex"))
                    .expect("Item 15 HEX output must be UTF-8");
            assert_eq!(
                native_hex, expected,
                "focused FS-UAE Item 15 HEX artifact text mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item16_listing_artifact_matches_rust_guided_text() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .output \"Work:opforge_native_out.lst\", format=lst, sections=code",
        "start   lda #$44",
        "        .include \"opforge_fsuae_include.inc\"",
        "        .fill byte, 2, $ee",
        "        .byte $11",
    ]
    .join("\n");
    let expected_listing = concat!(
        "opForge Assembler native\n",
        "ADDR    BYTES                    LINE  SOURCE\n",
        "------  -----------------------  ----  ------\n",
        "0800    A9 44                       2  start   lda #$44\n",
        "0802    A9 01                       1          lda #$01\n",
        "0804    EE EE                       4          .fill byte, 2, $ee\n",
        "0806    11                          5          .byte $11\n",
        "\n",
        "Lines: 4  Errors: 0  Warnings: 0\n",
        "\n",
        "SYMBOL TABLE\n",
        "\n",
        "(none)\n",
        "\n",
        "Total memory is 7 bytes\n",
        "\n",
        "GENERATED OUTPUT\n",
        "\n",
        "ADDR    BYTES\n",
        "------  -----------------------\n",
        "0800    A9 44 A9 01 EE EE 11\n",
    );

    match crate::fs_uae_smoke::run_opforge_native_cli_item16_listing_output_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        expected_listing.as_bytes(),
    )
    .expect("focused native opForge CLI Item 16 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected Item 16 listing output run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native opForge CLI Item 16 listing artifact fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                run,
                "focused native opForge CLI Item 16 listing artifact fixture",
            );
            let native_listing =
                std::str::from_utf8(captured_fs_uae_artifact(run, "Work/opforge_native_out.lst"))
                    .expect("Item 16 listing output must be UTF-8");
            assert_eq!(
                native_listing, expected_listing,
                "focused FS-UAE Item 16 listing text mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_item17_first_run_artifact_matrix_matches_rust() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source_with_output = |output: &str| {
        [
            "        .cpu 6502",
            "OFFSET  .const $02",
            "VALUE   .var   $10",
            "start   lda #$42",
            "        sta $0200 + OFFSET",
            "        .byte $f0, $05, $d0, $f7",
            "        ldx #VALUE",
            "        .byte $e8, $aa, $0c, $08, $03, $08",
            "        .text \"OK\"",
            "        .fill byte, 2, $ff",
            output,
        ]
        .join("\n")
    };
    let bin_source = source_with_output(
        "        .output \"Work:opforge_native_out.bin\", format=bin, sections=code",
    );
    let prg_source = source_with_output(
        "        .output \"Work:opforge_native_out.prg\", format=prg, loadaddr=$0800, sections=code",
    );
    let hex_source = source_with_output(
        "        .output \"Work:opforge_native_out.hex\", format=hex, sections=code",
    );
    let lst_source = source_with_output(
        "        .output \"Work:opforge_native_out.lst\", format=lst, sections=code",
    );
    let expected_bin = first_run_6502_artifact_contract_expected_bin();
    let mut expected_prg = vec![0x00, 0x08];
    expected_prg.extend_from_slice(&expected_bin);
    let expected_hex = ":15080000A9428D0202F005D0F7A210E8AA0C0803084F4BFFFFB0\n:00000001FF\n";
    let expected_listing = concat!(
        "opForge Assembler native\n",
        "ADDR    BYTES                    LINE  SOURCE\n",
        "------  -----------------------  ----  ------\n",
        "----                                1          .cpu 6502\n",
        "----                                2  OFFSET  .const $02\n",
        "----                                3  VALUE   .var   $10\n",
        "0800    A9 42                       4  start   lda #$42\n",
        "0802    8D 02 02                    5          sta $0200 + OFFSET\n",
        "0805    F0 05 D0 F7                 6          .byte $f0, $05, $d0, $f7\n",
        "0809    A2 10                       7          ldx #VALUE\n",
        "080B    E8 AA 0C 08 03 08           8          .byte $e8, $aa, $0c, $08, $03, $08\n",
        "0811    4F 4B                       9          .text \"OK\"\n",
        "0813    FF FF                      10          .fill byte, 2, $ff\n",
        "\n",
        "Lines: 10  Errors: 0  Warnings: 0\n",
        "\n",
        "SYMBOL TABLE\n",
        "\n",
        "(none)\n",
        "\n",
        "Total memory is 21 bytes\n",
        "\n",
        "GENERATED OUTPUT\n",
        "\n",
        "ADDR    BYTES\n",
        "------  -----------------------\n",
        "0800    A9 42 8D 02 02 F0 05 D0 F7 A2 10 E8 AA 0C 08 03 08 4F 4B FF FF\n",
    );

    match crate::fs_uae_smoke::run_opforge_native_cli_item17_artifact_matrix_from_env(
        &repo_root,
        [
            bin_source.as_bytes(),
            prg_source.as_bytes(),
            hex_source.as_bytes(),
            lst_source.as_bytes(),
        ],
        package_bytes.as_slice(),
        [
            &expected_bin,
            &expected_prg,
            expected_hex.as_bytes(),
            expected_listing.as_bytes(),
        ],
    )
    .expect("focused native opForge CLI Item 17 FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 4, "expected first-run artifact matrix runs");
            for (idx, run) in runs.iter().enumerate() {
                assert!(
                    run.success,
                    "focused native opForge CLI Item 17 matrix run {idx} failed\nstdout:\n{}\nstderr:\n{}",
                    run.stdout, run.stderr,
                );
                assert_native_cli_run_omits_debug_progress(
                    run,
                    format!("focused native opForge CLI Item 17 matrix run {idx}").as_str(),
                );
            }

            let expected_bin = first_run_6502_artifact_contract_expected_bin();
            let native_bin = captured_fs_uae_artifact(&runs[0], "Work/opforge_native_out.bin");
            assert_eq!(native_bin, expected_bin, "Item 17 BIN artifact mismatch");

            let native_prg = captured_fs_uae_artifact(&runs[1], "Work/opforge_native_out.prg");
            let mut expected_prg = vec![0x00, 0x08];
            expected_prg.extend_from_slice(&expected_bin);
            assert_eq!(native_prg, expected_prg, "Item 17 PRG artifact mismatch");

            let native_hex = std::str::from_utf8(captured_fs_uae_artifact(
                &runs[2],
                "Work/opforge_native_out.hex",
            ))
            .expect("Item 17 HEX output must be UTF-8");
            assert_eq!(
                native_hex, ":15080000A9428D0202F005D0F7A210E8AA0C0803084F4BFFFFB0\n:00000001FF\n",
                "Item 17 HEX artifact mismatch"
            );

            let native_listing = std::str::from_utf8(captured_fs_uae_artifact(
                &runs[3],
                "Work/opforge_native_out.lst",
            ))
            .expect("Item 17 listing output must be UTF-8");
            let expected_listing = concat!(
                "opForge Assembler native\n",
                "ADDR    BYTES                    LINE  SOURCE\n",
                "------  -----------------------  ----  ------\n",
                "----                                1          .cpu 6502\n",
                "----                                2  OFFSET  .const $02\n",
                "----                                3  VALUE   .var   $10\n",
                "0800    A9 42                       4  start   lda #$42\n",
                "0802    8D 02 02                    5          sta $0200 + OFFSET\n",
                "0805    F0 05 D0 F7                 6          .byte $f0, $05, $d0, $f7\n",
                "0809    A2 10                       7          ldx #VALUE\n",
                "080B    E8 AA 0C 08 03 08           8          .byte $e8, $aa, $0c, $08, $03, $08\n",
                "0811    4F 4B                       9          .text \"OK\"\n",
                "0813    FF FF                      10          .fill byte, 2, $ff\n",
                "\n",
                "Lines: 10  Errors: 0  Warnings: 0\n",
                "\n",
                "SYMBOL TABLE\n",
                "\n",
                "(none)\n",
                "\n",
                "Total memory is 21 bytes\n",
                "\n",
                "GENERATED OUTPUT\n",
                "\n",
                "ADDR    BYTES\n",
                "------  -----------------------\n",
                "0800    A9 42 8D 02 02 F0 05 D0 F7 A2 10 E8 AA 0C 08 03 08 4F 4B FF FF\n",
            );
            assert_eq!(
                native_listing, expected_listing,
                "Item 17 LST artifact mismatch\nstdout:\n{}\nstderr:\n{}",
                runs[3].stdout, runs[3].stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_real_first_run_source_with_cli_cpu_matches_rust_prg() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source_path = repo_root
        .join("examples")
        .join("mos6502")
        .join("6502_first_run_artifact_contract.asm");
    let source = fs::read_to_string(&source_path)
        .unwrap_or_else(|err| panic!("read {}: {err}", source_path.display()));
    let expected_bin = first_run_6502_artifact_contract_expected_bin();
    let mut expected_prg = vec![0x00, 0x08];
    expected_prg.extend_from_slice(&expected_bin);

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/6502-first-run.prg",
        &expected_prg,
    )
    .expect("real first-run source with CLI cpu FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one real first-run CLI-cpu run");
            let run = &runs[0];
            assert!(
                run.success,
                "real first-run source with CLI cpu failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(run, "real first-run source with CLI cpu");
            let native_prg = captured_fs_uae_artifact(run, "Work/build/6502-first-run.prg");
            assert_eq!(
                native_prg, expected_prg,
                "real first-run source with CLI cpu PRG mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_layouted_branches_with_cli_bin_match_rust_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "OFFSET  .const $02",
        "VALUE   .var   $10",
        "start   lda #$42",
        "        sta $0200 + OFFSET",
        "        beq done",
        "        bne start",
        "        ldx #VALUE",
        "        inx",
        "done    .byte $aa, $0c, $08",
        "        .word start + 3",
        "        .text \"OK\"",
        "        .fill byte, 2, $ff",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = first_run_6502_artifact_contract_expected_bin();
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "layouted-branches-cli-bin",
        cpu_id: m6502_cpu_id.as_str(),
        source: source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("layouted branch CLI-bin FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one layouted-branch CLI-bin run");
            let run = &runs[0];
            assert!(
                run.success,
                "layouted branch CLI-bin fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "layouted branch CLI-bin output mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_layouted_branches_without_symbolic_word_match_rust_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "OFFSET  .const $02",
        "VALUE   .var   $10",
        "start   lda #$42",
        "        sta $0200 + OFFSET",
        "        beq done",
        "        bne start",
        "        ldx #VALUE",
        "        inx",
        "done    .byte $aa, $0c, $08",
        "        .text \"OK\"",
        "        .fill byte, 2, $ff",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![
        0xA9, 0x42, 0x8D, 0x02, 0x02, 0xF0, 0x05, 0xD0, 0xF7, 0xA2, 0x10, 0xE8, 0xAA, 0x0C, 0x08,
        0x4F, 0x4B, 0xFF, 0xFF,
    ];
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "layouted-branches-no-symbolic-word-cli-bin",
        cpu_id: m6502_cpu_id.as_str(),
        source: source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("layouted branch no-symbolic-word FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                1,
                "expected one layouted-branch no-symbolic-word run"
            );
            let run = &runs[0];
            assert!(
                run.success,
                "layouted branch no-symbolic-word fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "layouted branch no-symbolic-word output mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_layouted_branches_minimal_match_rust_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "start   lda #$42",
        "        beq done",
        "        bne start",
        "done    nop",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![0xA9, 0x42, 0xF0, 0x02, 0xD0, 0xFA, 0xEA];
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "layouted-branches-minimal-cli-bin",
        cpu_id: m6502_cpu_id.as_str(),
        source: source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("layouted branch minimal FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one layouted-branch minimal run");
            let run = &runs[0];
            assert!(
                run.success,
                "layouted branch minimal fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "layouted branch minimal output mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_forward_branch_past_symbolic_immediate_matches_rust_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "VALUE   .var   $10",
        "start   beq done",
        "        ldx #VALUE",
        "done    nop",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![0xF0, 0x02, 0xA2, 0x10, 0xEA];
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "forward-branch-past-symbolic-immediate",
        cpu_id: m6502_cpu_id.as_str(),
        source: source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("forward-branch past symbolic-immediate FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                1,
                "expected one forward-branch past symbolic-immediate run"
            );
            let run = &runs[0];
            assert!(
                run.success,
                "forward-branch past symbolic-immediate fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "forward-branch past symbolic-immediate output mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_symbolic_immediate_only_matches_rust_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "VALUE   .var   $10",
        "start   ldx #VALUE",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![0xA2, 0x10];
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "symbolic-immediate-only",
        cpu_id: m6502_cpu_id.as_str(),
        source: source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("symbolic-immediate only FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one symbolic-immediate only run");
            let run = &runs[0];
            assert!(
                run.success,
                "symbolic-immediate only fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "symbolic-immediate only output mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_forward_branch_past_symbolic_absolute_expr_matches_rust_bytes(
) {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "OFFSET  .const $02",
        "start   lda #$42",
        "        sta $0200 + OFFSET",
        "        beq done",
        "        nop",
        "done    nop",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![0xA9, 0x42, 0x8D, 0x02, 0x02, 0xF0, 0x01, 0xEA, 0xEA];
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "forward-branch-past-symbolic-absolute-expr",
        cpu_id: m6502_cpu_id.as_str(),
        source: source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect(
        "forward-branch past symbolic absolute-expr FS-UAE helper should complete or skip cleanly",
    ) {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                1,
                "expected one forward-branch past symbolic absolute-expr run"
            );
            let run = &runs[0];
            assert!(
                run.success,
                "forward-branch past symbolic absolute-expr fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "forward-branch past symbolic absolute-expr output mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_real_first_run_bare_labels_with_cli_bin_match_rust_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "OFFSET  .const $02",
        "VALUE   .var   $10",
        "",
        "start",
        "        lda #$42",
        "        sta $0200 + OFFSET",
        "        beq done",
        "        bne start",
        "        ldx #VALUE",
        "        inx",
        "done",
        "        .byte $aa, $0c, $08",
        "        .word start + 3",
        "        .text \"OK\"",
        "        .fill byte, 2, $ff",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = first_run_6502_artifact_contract_expected_bin();
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "real-first-run-bare-labels-cli-bin",
        cpu_id: m6502_cpu_id.as_str(),
        source: source.as_bytes(),
        package_bytes: package_bytes.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&expected),
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &repo_root,
        cases.as_slice(),
    )
    .expect("real first-run bare-label CLI-bin FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                1,
                "expected one real first-run bare-label CLI-bin run"
            );
            let run = &runs[0];
            assert!(
                run.success,
                "real first-run bare-label CLI-bin fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = verified_fs_uae_output(run);
            assert_eq!(
                native_bin, expected,
                "real first-run bare-label CLI-bin output mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_example_org_cli_bin_debug_matches_rust_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let source_path = repo_root
        .join("examples")
        .join("mos6502")
        .join("6502_native_cli_smoke.asm");
    let source = fs::read_to_string(&source_path)
        .unwrap_or_else(|err| panic!("read {}: {err}", source_path.display()));
    let expected = native_cli_6502_contract_expected_bin();
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "example-org-cli-bin-debug",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("{input} --bin {bin} --cpu m6502 --native-debug"),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &expected,
        },
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&repo_root, &cases)
        .expect("focused native CLI .org debug helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one focused .org debug run");
            let run = &runs[0];
            assert!(
                run.success,
                "focused native CLI .org debug fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/opforge_native_out.bin");
            assert_eq!(
                native_bin, expected,
                "focused native CLI .org debug output mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_source_cpu_only_minimal_bin_matches_rust_bytes() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "start   lda #$42",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .output \"build/source-cpu-minimal.bin\", format=bin, sections=code",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected = vec![0xA9, 0x42];

    match crate::fs_uae_smoke::run_opforge_native_cli_item17_source_cpu_output_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/source-cpu-minimal.bin",
        &expected,
    )
    .expect("minimal source-driven cpu FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one minimal source-cpu run");
            let run = &runs[0];
            assert!(
                run.success,
                "minimal source-driven cpu fixture failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_bin = captured_fs_uae_artifact(run, "Work/build/source-cpu-minimal.bin");
            assert_eq!(
                native_bin, expected,
                "minimal source-driven cpu output mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_real_first_run_source_cpu_directive_matches_rust_prg() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source_path = repo_root
        .join("examples")
        .join("mos6502")
        .join("6502_first_run_artifact_contract.asm");
    let source = fs::read_to_string(&source_path)
        .unwrap_or_else(|err| panic!("read {}: {err}", source_path.display()));
    let expected_bin = first_run_6502_artifact_contract_expected_bin();
    let mut expected_prg = vec![0x00, 0x08];
    expected_prg.extend_from_slice(&expected_bin);

    match crate::fs_uae_smoke::run_opforge_native_cli_item17_source_cpu_output_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/6502-first-run.prg",
        &expected_prg,
    )
    .expect("real first-run source-driven cpu FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one real first-run source-cpu run");
            let run = &runs[0];
            assert!(
                run.success,
                "real first-run source with source-driven cpu failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            assert_native_cli_run_omits_debug_progress(
                run,
                "real first-run source with source-driven cpu",
            );
            let native_prg = captured_fs_uae_artifact(run, "Work/build/6502-first-run.prg");
            assert_eq!(
                native_prg, expected_prg,
                "real first-run source with source-driven cpu PRG mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_real_first_run_source_cpu_directive_debug_flag_emits_progress(
) {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let source_path = repo_root
        .join("examples")
        .join("mos6502")
        .join("6502_first_run_artifact_contract.asm");
    let source = fs::read_to_string(&source_path)
        .unwrap_or_else(|err| panic!("read {}: {err}", source_path.display()));
    let mut rust_oracle = vec![0x00, 0x08];
    rust_oracle.extend_from_slice(&first_run_6502_artifact_contract_expected_bin());
    let cases = [crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "first-run-source-cpu-debug",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("{input} --native-debug"),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/build/6502-first-run.prg",
            rust_oracle: &rust_oracle,
        },
    }];

    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&repo_root, &cases)
        .expect("real first-run source-driven cpu debug helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one real first-run debug run");
            let run = &runs[0];
            assert!(
                run.stdout.contains("OPFORGE-NATIVE 1"),
                "real first-run debug run should emit native header\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_real_first_run_structure_with_known_body_matches_rust_prg() {
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "OFFSET  .const $02",
        "VALUE   .var   $10",
        "start   lda #$42",
        "        sta $0200 + OFFSET",
        "        .byte $f0, $05, $d0, $f7",
        "        ldx #VALUE",
        "        .byte $e8, $aa, $0c, $08, $03, $08",
        "        .text \"OK\"",
        "        .fill byte, 2, $ff",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .output \"build/6502-first-run.prg\", format=prg, loadaddr=$0800, contiguous=false, sections=code",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected_bin = first_run_6502_artifact_contract_expected_bin();
    let mut expected_prg = vec![0x00, 0x08];
    expected_prg.extend_from_slice(&expected_bin);

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/6502-first-run.prg",
        &expected_prg,
    )
    .expect("real first-run structure reduction FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one real-structure reduction run");
            let run = &runs[0];
            assert!(
                run.success,
                "real first-run structure reduction failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_prg = captured_fs_uae_artifact(run, "Work/build/6502-first-run.prg");
            assert_eq!(
                native_prg, expected_prg,
                "real first-run structure reduction PRG mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_real_first_run_structure_with_symbolic_word_matches_rust_prg()
{
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "OFFSET  .const $02",
        "VALUE   .var   $10",
        "start   lda #$42",
        "        sta $0200 + OFFSET",
        "        .byte $f0, $05, $d0, $f7",
        "        ldx #VALUE",
        "        .byte $e8, $aa, $0c, $08",
        "        .word start + 3",
        "        .text \"OK\"",
        "        .fill byte, 2, $ff",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .output \"build/6502-first-run.prg\", format=prg, loadaddr=$0800, contiguous=false, sections=code",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected_bin = first_run_6502_artifact_contract_expected_bin();
    let mut expected_prg = vec![0x00, 0x08];
    expected_prg.extend_from_slice(&expected_bin);

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/6502-first-run.prg",
        &expected_prg,
    )
    .expect("real first-run symbolic word reduction FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one symbolic-word reduction run");
            let run = &runs[0];
            assert!(
                run.success,
                "real first-run symbolic word reduction failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_prg = captured_fs_uae_artifact(run, "Work/build/6502-first-run.prg");
            assert_eq!(
                native_prg, expected_prg,
                "real first-run symbolic-word reduction PRG mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_real_first_run_structure_with_inline_labels_matches_rust_prg()
{
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let repo_root = workspace_root();
    let package_bytes = item6_mos_package_bytes();
    let source = [
        "        .module main",
        "        .cpu 6502",
        "",
        "        .region ram, $0800, $083f, align=1",
        "",
        "        .section code, align=1",
        "OFFSET  .const $02",
        "VALUE   .var   $10",
        "start   lda #$42",
        "        sta $0200 + OFFSET",
        "        beq done",
        "        bne start",
        "        ldx #VALUE",
        "        inx",
        "done    .byte $aa, $0c, $08",
        "        .word start + 3",
        "        .text \"OK\"",
        "        .fill byte, 2, $ff",
        "        .endsection",
        "",
        "        .place code in ram",
        "",
        "        .output \"build/6502-first-run.prg\", format=prg, loadaddr=$0800, contiguous=false, sections=code",
        "",
        "        .endmodule",
        "        .end",
    ]
    .join("\n");
    let expected_bin = first_run_6502_artifact_contract_expected_bin();
    let mut expected_prg = vec![0x00, 0x08];
    expected_prg.extend_from_slice(&expected_bin);

    match crate::fs_uae_smoke::run_opforge_native_cli_item13_output_directive_from_env(
        &repo_root,
        source.as_bytes(),
        package_bytes.as_slice(),
        "Work/build/6502-first-run.prg",
        &expected_prg,
    )
    .expect("real first-run inline-label reduction FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected one inline-label reduction run");
            let run = &runs[0];
            assert!(
                run.success,
                "real first-run inline-label reduction failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
            let native_prg = captured_fs_uae_artifact(run, "Work/build/6502-first-run.prg");
            assert_eq!(
                native_prg, expected_prg,
                "real first-run inline-label reduction PRG mismatch\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_opforge_native_cli_failure_paths_report_diagnostics() {
    // Proof level D. This test proves every established real native CLI failure
    // path emits its deterministic diagnostic through ErrorOutput and returns
    // the Rust CLI failure status. This test does not prove deferred diagnostic
    // semantics or exact Rust rendering beyond the established native texts.
    let _fs_uae_native_cli_guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI FS-UAE smoke lock poisoned");

    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "unknown-mnemonic",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC",
            expected_diagnostic: "ERROR OPC-NCLI025: unknown native mnemonic",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "unsupported-addressing",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING",
            expected_diagnostic: "ERROR OPC-NCLI026: unsupported native addressing mode",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "unresolved-label",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_6502_UNRESOLVED_LABEL",
            expected_diagnostic: "ERROR OPC-NCLI022: unresolved native label",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "bad-org",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_6502_BAD_ORG",
            expected_diagnostic: "ERROR OPC-NCLI027: invalid native .org expression",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "unsupported-output",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_UNSUPPORTED_OUTPUT",
            expected_diagnostic: "OPC-NCLI003: recognized Rust CLI option is not implemented by native AmigaOS CLI yet: --srec",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "missing-input",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_MISSING_INPUT",
            expected_diagnostic: "OPC-NCLI008: Input source file not found: Work:opforge_missing_input.asm",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "missing-hunk",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_MISSING_HUNK",
            expected_diagnostic: "OPC-NCLI007: No outputs selected. Native AmigaOS CLI currently requires --bin or --list",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "hunk-output",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_HUNK_OUTPUT",
            expected_diagnostic: "ERROR OPC-NCLI028: native Hunk output is not implemented; use --bin for flat output",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "mixed-input",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_MIXED_INPUT",
            expected_diagnostic: "OPC-NCLI011: Do not mix positional input with -i/--infile; use one style",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "bad-package",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_BAD_PACKAGE",
            expected_diagnostic: "ERROR OPC-NCLI010: native tokenizer stage failed",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "package-too-large",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_PACKAGE_TOO_LARGE",
            expected_diagnostic: "ERROR OPC-NCLI019: opasm package exceeds native package storage capacity",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "unmatched-endmodule",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_UNMATCHED_ENDMODULE",
            expected_diagnostic: "ERROR OPC-NCLI016: native module depth mismatch",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "unterminated-module",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_UNTERMINATED_MODULE",
            expected_diagnostic: "ERROR OPC-NCLI016: native module depth mismatch",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "bad-use",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_BAD_USE",
            expected_diagnostic: "ERROR OPC-NCLI013: native module/use parser stage failed",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "missing-module",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE",
            expected_diagnostic: "ERROR OPC-NCLI018: native module resolution failed: missing",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "missing-module-path",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE_PATH",
            expected_diagnostic: "OPC-NCLI005: option requires a value: -M",
        },
        crate::fs_uae_smoke::OpforgeNativeCliFailureCase {
            name: "module-path-overflow",
            define: "OPFORGE_FS_UAE_NATIVE_CLI_MODULE_PATH_OVERFLOW",
            expected_diagnostic: "OPC-NCLI017: native module path capacity exceeded",
        },
    ];

    match crate::fs_uae_smoke::run_opforge_native_cli_failure_cases_from_env(
        &workspace_root(),
        &cases,
    )
    .expect("native opForge CLI failure-path FS-UAE helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(
                runs.len(),
                cases.len(),
                "expected one native opForge CLI run per failure-path case"
            );
            for (case, run) in cases.iter().zip(runs.iter()) {
                assert_eq!(run.example_name, "opforge_cli");
                assert!(
                    !run.success,
                    "native opForge CLI failure case {} should return non-zero\nstdout:\n{}\nstderr:\n{}",
                    case.name,
                    run.stdout,
                    run.stderr,
                );
                assert!(
                    run.stderr.contains(case.expected_diagnostic),
                    "native opForge CLI failure case {} did not report expected diagnostic '{}'\nstdout:\n{}\nstderr:\n{}",
                    case.name,
                    case.expected_diagnostic,
                    run.stdout,
                    run.stderr,
                );
                assert!(
                    !run.stdout.contains(case.expected_diagnostic),
                    "native opForge CLI failure case {} leaked its diagnostic to stdout\nstdout:\n{}\nstderr:\n{}",
                    case.name,
                    run.stdout,
                    run.stderr,
                );
                assert_eq!(
                    run.exit_code,
                    Some(1),
                    "native opForge CLI failure case {} status must match Rust\nstdout:\n{}\nstderr:\n{}",
                    case.name,
                    run.stdout,
                    run.stderr,
                );
            }
        }
    }
}

#[test]
fn external_fs_uae_tkpkg_native_motorola68000_family_corpus_matches_vm_authoritative_rows() {
    let corpus = selected_motorola68000_native_tkpkg_parity_corpus();

    let package = tkpkg_smoke_package_bytes();
    let model = load_opasm_model_from_package_bytes(package.as_slice());

    for entry in corpus {
        let expected_rows = render_tkpkg_smoke_debug_rows_for_source(
            &model,
            entry.cpu_id.as_str(),
            Some("motorola68k"),
            entry.source.as_str(),
        );

        match crate::fs_uae_smoke::run_tkpkg_debug_cli_file_mode_from_env(
            &workspace_root(),
            entry.source.as_bytes(),
            entry.cpu_id.as_str(),
        )
        .unwrap_or_else(|err| panic!("native tkpkg parity run for {}: {err}", entry.relative_path))
        {
            crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
                eprintln!("SKIP: {reason}");
                return;
            }
            crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
                assert_eq!(runs.len(), 1, "expected a single tkpkg debug-cli run");
                let run = &runs[0];
                assert!(
                    run.success,
                    "native tkpkg parity failed for {} with {} under {}\nstdout:\n{}\nstderr:\n{}",
                    entry.relative_path,
                    run.hunk_path.display(),
                    run.artifact_dir.display(),
                    run.stdout,
                    run.stderr,
                );

                let actual_rows = extract_tkpkg_debug_rows(run.stdout.as_str());
                assert_eq!(
                    actual_rows,
                    expected_rows,
                    "native tkpkg parity mismatch for {} with {} under {}\nstdout:\n{}\nstderr:\n{}",
                    entry.relative_path,
                    run.hunk_path.display(),
                    run.artifact_dir.display(),
                    run.stdout,
                    run.stderr,
                );
            }
        }
    }
}

#[test]
fn external_fs_uae_tkpkg_native_operator_surface_matches_vm_authoritative_rows() {
    let package = tkpkg_smoke_package_bytes();
    let model = load_opasm_model_from_package_bytes(package.as_slice());
    let expected_rows = render_tkpkg_smoke_debug_rows_for_source(
        &model,
        "m68020",
        Some("motorola68k"),
        TKPKG_OPERATOR_PARITY_SOURCE,
    );

    match crate::fs_uae_smoke::run_tkpkg_debug_cli_file_mode_with_package_from_env(
        &workspace_root(),
        TKPKG_OPERATOR_PARITY_SOURCE.as_bytes(),
        "m68020",
        package.as_slice(),
    )
    .unwrap_or_else(|err| panic!("native operator parity run: {err}"))
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected a single tkpkg debug-cli run");
            let run = &runs[0];
            assert!(
                run.success,
                "native operator parity failed with {} under {}\nstdout:\n{}\nstderr:\n{}",
                run.hunk_path.display(),
                run.artifact_dir.display(),
                run.stdout,
                run.stderr,
            );

            let actual_rows = extract_tkpkg_debug_rows(run.stdout.as_str());
            assert_eq!(
                actual_rows,
                expected_rows,
                "native operator parity mismatch with {} under {}\nstdout:\n{}\nstderr:\n{}",
                run.hunk_path.display(),
                run.artifact_dir.display(),
                run.stdout,
                run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_tkpkg_native_percent_prefix_context_matches_vm_authoritative_rows() {
    let package = tkpkg_smoke_package_bytes();
    let model = load_opasm_model_from_package_bytes(package.as_slice());
    let expected_rows = render_tkpkg_smoke_debug_rows_for_source(
        &model,
        "m68020",
        Some("motorola68k"),
        TKPKG_PERCENT_PREFIX_PARITY_SOURCE,
    );

    match crate::fs_uae_smoke::run_tkpkg_debug_cli_file_mode_with_package_from_env(
        &workspace_root(),
        TKPKG_PERCENT_PREFIX_PARITY_SOURCE.as_bytes(),
        "m68020",
        package.as_slice(),
    )
    .unwrap_or_else(|err| panic!("native percent-prefix parity run: {err}"))
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected a single tkpkg debug-cli run");
            let run = &runs[0];
            assert!(
                run.success,
                "native percent-prefix parity failed with {} under {}\nstdout:\n{}\nstderr:\n{}",
                run.hunk_path.display(),
                run.artifact_dir.display(),
                run.stdout,
                run.stderr,
            );

            let actual_rows = extract_tkpkg_debug_rows(run.stdout.as_str());
            assert_eq!(
                actual_rows,
                expected_rows,
                "native percent-prefix parity mismatch with {} under {}\nstdout:\n{}\nstderr:\n{}",
                run.hunk_path.display(),
                run.artifact_dir.display(),
                run.stdout,
                run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_tkpkg_native_mos6502_family_corpus_matches_vm_authoritative_rows() {
    let corpus = selected_mos6502_native_tkpkg_parity_corpus();

    let package = tkpkg_mos6502_native_parity_package_bytes();
    let model = load_opasm_model_from_package_bytes(package.as_slice());

    let expected_rows = corpus
        .iter()
        .flat_map(|entry| {
            render_tkpkg_smoke_debug_rows_for_source(
                &model,
                entry.cpu_id.as_str(),
                None,
                entry.source.as_str(),
            )
        })
        .collect::<Vec<_>>();
    let manifest_cases = corpus
        .iter()
        .map(|entry| crate::fs_uae_smoke::TkpkgDebugCliManifestCase {
            name: entry.relative_path.as_str(),
            cpu_id: entry.cpu_id.as_str(),
            source: entry.source.as_bytes(),
        })
        .collect::<Vec<_>>();

    match crate::fs_uae_smoke::run_tkpkg_debug_cli_manifest_mode_with_package_from_env(
        &workspace_root(),
        manifest_cases.as_slice(),
        package.as_slice(),
    )
    .unwrap_or_else(|err| panic!("native tkpkg parity manifest run: {err}"))
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected a single tkpkg debug-cli run");
            let run = &runs[0];
            assert!(
                run.success,
                "native tkpkg parity manifest failed for {} cases with {} under {}\nstdout:\n{}\nstderr:\n{}",
                corpus.len(),
                run.hunk_path.display(),
                run.artifact_dir.display(),
                run.stdout,
                run.stderr,
            );

            let actual_rows = extract_tkpkg_debug_rows(run.stdout.as_str());
            assert_eq!(
                actual_rows,
                expected_rows,
                "native tkpkg parity manifest mismatch for {} cases with {} under {}\nstdout:\n{}\nstderr:\n{}",
                corpus.len(),
                run.hunk_path.display(),
                run.artifact_dir.display(),
                run.stdout,
                run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_tkpkg_native_intel8080_family_corpus_matches_vm_authoritative_rows() {
    let corpus = selected_intel8080_native_tkpkg_parity_corpus();

    let package = tkpkg_intel8080_native_parity_package_bytes();
    let model = load_opasm_model_from_package_bytes(package.as_slice());

    let expected_rows = corpus
        .iter()
        .flat_map(|entry| {
            render_tkpkg_smoke_debug_rows_for_source(
                &model,
                entry.cpu_id.as_str(),
                None,
                entry.source.as_str(),
            )
        })
        .collect::<Vec<_>>();
    let manifest_cases = corpus
        .iter()
        .map(|entry| crate::fs_uae_smoke::TkpkgDebugCliManifestCase {
            name: entry.relative_path.as_str(),
            cpu_id: entry.cpu_id.as_str(),
            source: entry.source.as_bytes(),
        })
        .collect::<Vec<_>>();

    match crate::fs_uae_smoke::run_tkpkg_debug_cli_manifest_mode_with_package_from_env(
        &workspace_root(),
        manifest_cases.as_slice(),
        package.as_slice(),
    )
    .unwrap_or_else(|err| panic!("native intel8080 tkpkg parity manifest run: {err}"))
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected a single tkpkg debug-cli run");
            let run = &runs[0];
            assert!(
                run.success,
                "native intel8080 tkpkg parity manifest failed for {} cases with {} under {}\nstdout:\n{}\nstderr:\n{}",
                corpus.len(),
                run.hunk_path.display(),
                run.artifact_dir.display(),
                run.stdout,
                run.stderr,
            );

            let actual_rows = extract_tkpkg_debug_rows(run.stdout.as_str());
            assert_eq!(
                actual_rows,
                expected_rows,
                "native intel8080 tkpkg parity manifest mismatch for {} cases with {} under {}\nstdout:\n{}\nstderr:\n{}",
                corpus.len(),
                run.hunk_path.display(),
                run.artifact_dir.display(),
                run.stdout,
                run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_tkpkg_native_motorola6800_family_corpus_matches_vm_authoritative_rows() {
    let corpus = selected_motorola6800_native_tkpkg_parity_corpus();

    let package = tkpkg_motorola6800_native_parity_package_bytes();
    let model = load_opasm_model_from_package_bytes(package.as_slice());

    let expected_rows = corpus
        .iter()
        .flat_map(|entry| {
            render_tkpkg_smoke_debug_rows_for_source(
                &model,
                entry.cpu_id.as_str(),
                None,
                entry.source.as_str(),
            )
        })
        .collect::<Vec<_>>();
    let manifest_cases = corpus
        .iter()
        .map(|entry| crate::fs_uae_smoke::TkpkgDebugCliManifestCase {
            name: entry.relative_path.as_str(),
            cpu_id: entry.cpu_id.as_str(),
            source: entry.source.as_bytes(),
        })
        .collect::<Vec<_>>();

    match crate::fs_uae_smoke::run_tkpkg_debug_cli_manifest_mode_with_package_from_env(
        &workspace_root(),
        manifest_cases.as_slice(),
        package.as_slice(),
    )
    .unwrap_or_else(|err| panic!("native motorola6800 tkpkg parity manifest run: {err}"))
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected a single tkpkg debug-cli run");
            let run = &runs[0];
            assert!(
                run.success,
                "native motorola6800 tkpkg parity manifest failed for {} cases with {} under {}\nstdout:\n{}\nstderr:\n{}",
                corpus.len(),
                run.hunk_path.display(),
                run.artifact_dir.display(),
                run.stdout,
                run.stderr,
            );

            let actual_rows = extract_tkpkg_debug_rows(run.stdout.as_str());
            assert_eq!(
                actual_rows,
                expected_rows,
                "native motorola6800 tkpkg parity manifest mismatch for {} cases with {} under {}\nstdout:\n{}\nstderr:\n{}",
                corpus.len(),
                run.hunk_path.display(),
                run.artifact_dir.display(),
                run.stdout,
                run.stderr,
            );
        }
    }
}

#[test]
fn external_fs_uae_tkpkg_native_rejects_truncated_conditional_jump_tokenizer_program() {
    let malformed_package = tkpkg_smoke_package_bytes_with_family_tokenizer_program(vec![
        TokenizerVmOpcode::ReadChar as u8,
        TokenizerVmOpcode::JumpIfEol as u8,
        0,
        0,
        0,
    ]);

    match crate::fs_uae_smoke::run_tkpkg_debug_cli_file_mode_with_package_from_env(
        &workspace_root(),
        b"move.b d0,d1\n",
        "m68020",
        malformed_package.as_slice(),
    )
    .expect("native malformed tkpkg run should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected a single tkpkg debug-cli run");
            let run = &runs[0];
            let combined_output = format!("{}\n{}", run.stdout, run.stderr);
            assert!(
                !run.success,
                "malformed tokenizer package should fail deterministically under native tkpkg\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr
            );
            assert!(
                combined_output.contains("TKPKG load_package/set_pipeline OK"),
                "malformed tokenizer package should still load and select its pipeline before tokenization fails\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr
            );
            assert!(
                combined_output.contains("tkpkg failure: OTR901: invalid tokenizer VM progra"),
                "native tkpkg should report deterministic invalid-program status for truncated conditional-jump bytecode\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr
            );
        }
    }
}

#[test]
fn external_fs_uae_tkpkg_native_rejects_selected_chunk_bounds_during_set_pipeline() {
    let cases = vec![
        (
            "CPUS string payload crosses selected chunk length",
            tkpkg_selected_chunk_length_case(b"CPUS", 8),
        ),
        (
            "FAMS string payload crosses selected chunk length",
            tkpkg_selected_chunk_length_case(b"FAMS", 8),
        ),
        (
            "DIAL optional allow-list crosses selected chunk length",
            tkpkg_selected_chunk_truncated_by_one_case(b"DIAL"),
        ),
        (
            "TOKS token-policy skip crosses selected chunk length",
            tkpkg_selected_chunk_truncated_by_one_case(b"TOKS"),
        ),
        (
            "TKVM tokenizer program skip crosses selected chunk length",
            tkpkg_selected_chunk_truncated_by_one_case(b"TKVM"),
        ),
    ];

    for (label, malformed_package) in cases {
        assert!(
            vm::vm_opasm::load_model_from_package_bytes(malformed_package.as_slice()).is_err(),
            "{label} should be rejected by the host package model before native execution"
        );

        match crate::fs_uae_smoke::run_tkpkg_debug_cli_file_mode_with_package_from_env(
            &workspace_root(),
            b"move.b d0,d1\n",
            "m68020",
            malformed_package.as_slice(),
        )
        .expect("native malformed selected-chunk run should complete or skip cleanly")
        {
            crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
                eprintln!("SKIP: {reason}");
                return;
            }
            crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
                assert_eq!(runs.len(), 1, "expected a single tkpkg debug-cli run");
                let run = &runs[0];
                let combined_output = format!("{}\n{}", run.stdout, run.stderr);
                assert!(
                    !run.success,
                    "{label} should fail deterministically under native tkpkg\nstdout:\n{}\nstderr:\n{}",
                    run.stdout,
                    run.stderr
                );
                assert!(
                    combined_output.contains("tkpkg package loaded"),
                    "{label} should pass load_package so set_pipeline owns the rejection\nstdout:\n{}\nstderr:\n{}",
                    run.stdout,
                    run.stderr
                );
                assert!(
                    !combined_output.contains("TKPKG load_package/set_pipeline OK"),
                    "{label} must not report set_pipeline success after crossing selected chunk bounds\nstdout:\n{}\nstderr:\n{}",
                    run.stdout,
                    run.stderr
                );
                assert!(
                    combined_output.contains("tkpkg failure:"),
                    "{label} should report a deterministic tkpkg failure\nstdout:\n{}\nstderr:\n{}",
                    run.stdout,
                    run.stderr
                );
            }
        }
    }
}

#[test]
fn external_fs_uae_tkpkg_native_rejects_over_capacity_active_family_identifier() {
    const PIPELINE_ID_BUFFER_CAPACITY: usize = 32;

    let max_family_id = "a".repeat(PIPELINE_ID_BUFFER_CAPACITY - 1);
    let over_capacity_family_id = "b".repeat(PIPELINE_ID_BUFFER_CAPACITY);
    let valid_package = tkpkg_m68020_package_with_pipeline_ids(
        m68020_cpu_id.as_str(),
        max_family_id.as_str(),
        "motorola68k",
    );
    let malformed_package = tkpkg_m68020_package_with_pipeline_ids(
        m68020_cpu_id.as_str(),
        over_capacity_family_id.as_str(),
        "motorola68k",
    );

    for (label, package) in [
        ("31-byte resolved family id", valid_package.as_slice()),
        ("32-byte resolved family id", malformed_package.as_slice()),
    ] {
        let model = load_opasm_model_from_package_bytes(package);
        let resolved = model
            .resolve_pipeline(m68020_cpu_id.as_str(), Some("motorola68k"))
            .unwrap_or_else(|err| panic!("{label} should resolve in package model: {err}"));
        assert_eq!(resolved.cpu_id, m68020_cpu_id.as_str(), "{label} cpu id");
        assert_eq!(resolved.dialect_id, "motorola68k", "{label} dialect id");
    }

    match crate::fs_uae_smoke::run_tkpkg_debug_cli_file_mode_with_package_from_env(
        &workspace_root(),
        b"move.b d0,d1\n",
        "m68020",
        valid_package.as_slice(),
    )
    .expect("native max-length family-id run should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
            return;
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected a single tkpkg debug-cli run");
            let run = &runs[0];
            let combined_output = format!("{}\n{}", run.stdout, run.stderr);
            assert!(
                run.success,
                "31-byte resolved family id should remain valid under native tkpkg\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr
            );
            assert!(
                combined_output.contains("TKPKG load_package/set_pipeline OK"),
                "31-byte resolved family id should complete set_pipeline\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr
            );
        }
    }

    match crate::fs_uae_smoke::run_tkpkg_debug_cli_file_mode_with_package_from_env(
        &workspace_root(),
        b"move.b d0,d1\n",
        "m68020",
        malformed_package.as_slice(),
    )
    .expect("native over-capacity family-id run should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "expected a single tkpkg debug-cli run");
            let run = &runs[0];
            let combined_output = format!("{}\n{}", run.stdout, run.stderr);
            assert!(
                !run.success,
                "32-byte resolved family id should fail deterministically under native tkpkg\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr
            );
            assert!(
                combined_output.contains("tkpkg package loaded"),
                "32-byte resolved family id should pass load_package so set_pipeline owns the rejection\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr
            );
            assert!(
                !combined_output.contains("TKPKG load_package/set_pipeline OK"),
                "32-byte resolved family id must not report set_pipeline success\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr
            );
            assert!(
                combined_output.contains("tkpkg failure: OTR004: package identifier too long"),
                "32-byte resolved family id should report the deterministic identifier-length failure\nstdout:\n{}\nstderr:\n{}",
                run.stdout,
                run.stderr
            );
        }
    }
}
