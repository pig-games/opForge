//! Exact recorded branch cases, separated from unrelated aggregate timeouts.
use super::*;

#[test]
fn native_recorded_branch_failures_fs_uae() {
    // Level A: each selected actual source supplies a fresh in-memory Rust oracle.
    // Level D only on fresh guest completion, explicit zero exit and exact artifact.
    // Does not prove unrelated aggregate cases, timing, or a branch failure's cause.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native CLI lock");
    let root = workspace_root();
    let selection = std::env::var("OPFORGE_BRANCH_CASE").unwrap_or_else(|_| "all".into());
    assert!(matches!(
        selection.as_str(),
        "all" | "first-run" | "simple" | "allmodes" | "source-cpu"
    ));
    let names = [
        (
            "first-run",
            "examples/mos6502/6502_first_run_artifact_contract.asm",
        ),
        ("simple", "examples/mos6502/65c02_simple.asm"),
        ("allmodes", "examples/mos6502/65c02_allmodes.asm"),
    ];
    let mut owned = Vec::new();
    for (key, path) in names {
        if selection != "all" && selection != key {
            continue;
        }
        let reference = native_reference_cases()
            .iter()
            .find(|c| c.asm_path == path)
            .expect("original schema case");
        let source = fs::read_to_string(root.join(&reference.asm_path)).expect("original source");
        let (location, file_name, expected) =
            native_cli_schema_live_rust_binary_oracle(reference, &source);
        owned.push(NativeCliSchemaCase {
            name: reference.asm_path.as_str(),
            defines: vec![],
            source: Some(source),
            command_template: Some(reference.command_template.as_str()),
            expected_success: true,
            stdout_contains: vec![],
            expected_diagnostic: None,
            artifact: Some(NativeCliSchemaExpectedArtifact::Binary {
                location,
                file_name,
                expected,
            }),
        });
    }
    if selection == "all" || selection == "source-cpu" {
        let source = "        .cpu \"6502\"\n        .org $1000\n        lda #$11\n        .cpu M6502\n        sta $20\n        .cpu 65C02\n        bra done\n        .byte $ff\ndone    lda #$22\n";
        let expected = live_rust_cpu_name_oracle(source, None, "native-source-cpu-package-aliases")
            .expect("original source CPU Rust oracle");
        owned.push(NativeCliSchemaCase {
            name: "source-cpu-package-aliases",
            defines: vec![],
            source: Some(source.into()),
            command_template: Some("{input} --bin {bin}"),
            expected_success: true,
            stdout_contains: vec![],
            expected_diagnostic: None,
            artifact: Some(NativeCliSchemaExpectedArtifact::Binary {
                location: NativeCliSchemaArtifactLocation::CaseWork,
                file_name: "opforge_native_out.bin",
                expected,
            }),
        });
    }
    let cases = owned
        .iter()
        .map(|case| crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: case.name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: case.source.as_deref().map(str::as_bytes),
            command_template: case.command_template,
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: native_cli_schema_runner_proof(case),
        })
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &cases)
        .expect("recorded branch cases attempted with original proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), owned.len());
            for (case, run) in owned.iter().zip(&runs) {
                assert_native_cli_schema_case(case, run);
                eprintln!("BRANCH_CASE_PASS {}", case.name);
            }
        }
    }
}
