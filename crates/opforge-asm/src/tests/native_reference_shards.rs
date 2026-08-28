//! Native reference-corpus accounting tests.

use super::*;

#[test]
fn native_reference_manifest_seed_matches_current_mos_item6_slice() {
    // Proof level A. This test proves the governed manifest retains every
    // original stripped-source MOS seed case. This test does not prove native
    // execution or semantic parity for those cases.
    let manifest_paths = native_reference_cases()
        .iter()
        .filter(|case| case.source_mode == NativeReferenceSourceMode::SourceBinFromExample)
        .map(|case| (case.asm_path.as_str(), case.cpu_id.as_str()))
        .collect::<Vec<_>>();
    let item6_paths = item6_mos_fixture_allowlist()
        .into_iter()
        .collect::<Vec<_>>();

    for entry in item6_paths {
        assert!(
            manifest_paths.contains(&entry),
            "expected stripped-bin manifest slice to retain Item 6 seed case {:?}",
            entry
        );
    }
    assert!(
        manifest_paths.len() >= 5,
        "expected stripped-bin manifest slice to be at least the original Item 6 size"
    );
}

#[test]
fn native_reference_manifest_carries_current_focused_non_seed_cases() {
    // Proof level A. This test proves focused non-seed cases retain their
    // intended CPU and source-mode metadata. This test does not prove the
    // command template or native output is correct.
    let current_focused_cases = [
        (
            "examples/mos6502/6502_first_run_artifact_contract.asm",
            "m6502",
            NativeReferenceSourceMode::SourceCpuPrgFromExample,
        ),
        (
            "examples/mos6502/65c02_simple.asm",
            "65c02",
            NativeReferenceSourceMode::SourceBinFromExample,
        ),
        (
            "examples/mos6502/65c02_allmodes.asm",
            "65c02",
            NativeReferenceSourceMode::SourceBinFromExample,
        ),
    ];

    for (asm_path, cpu_id, source_mode) in current_focused_cases {
        let case = native_reference_cases()
            .iter()
            .find(|case| case.asm_path == asm_path)
            .unwrap_or_else(|| panic!("missing focused native reference case {asm_path}"));
        assert_eq!(case.cpu_id, cpu_id);
        assert_eq!(case.source_mode, source_mode);
    }
}

#[test]
fn native_reference_manifest_accounts_for_current_example_corpus() {
    // Proof level A. This test proves every currently checked-in example is
    // represented by a case, an exact opcore shard assignment, or a non-empty
    // reviewed exclusion. This test does not prove any native CLI path executed.
    let repo_root = workspace_root();
    let examples_dir = repo_root.join("examples");
    let asm_files = collect_example_asm_files(&examples_dir);
    let mut case_count = 0usize;
    let mut opcore_count = 0usize;
    let mut motorola68000_count = 0usize;
    let mut exclusion_count = 0usize;

    for asm_path in asm_files {
        let relative_path = asm_path
            .strip_prefix(&repo_root)
            .unwrap_or_else(|_| panic!("strip repo root for {}", asm_path.display()))
            .display()
            .to_string();
        match account_native_reference_path(relative_path.as_str())
            .unwrap_or_else(|err| panic!("account native reference path {relative_path}: {err}"))
        {
            NativeReferenceAccounting::Case(case) => {
                case_count += 1;
                assert_eq!(case.asm_path, relative_path);
            }
            NativeReferenceAccounting::Opcore(assignment) => {
                opcore_count += 1;
                assert_eq!(assignment.source_path, relative_path);
            }
            NativeReferenceAccounting::Motorola68000Reference => {
                motorola68000_count += 1;
                assert_eq!(
                    Path::new(&relative_path).parent(),
                    Some(Path::new("examples/motorola68000"))
                );
            }
            NativeReferenceAccounting::Excluded(rule) => {
                exclusion_count += 1;
                assert!(
                    !rule.reason.trim().is_empty(),
                    "native reference exclusion reason must be concrete for {relative_path}"
                );
            }
        }
    }

    assert_eq!(case_count, native_reference_cases().len());
    assert!(
        opcore_count > 0,
        "native reference completeness guard should exercise opcore assignments"
    );
    assert!(
        exclusion_count > 0,
        "native reference completeness guard should exercise explicit exclusions"
    );
    assert_eq!(
        motorola68000_count, 43,
        "every current top-level Motorola 68000 source must be reference-owned"
    );
}

#[test]
fn native_reference_motorola68000_manifest_accounts_for_complete_top_level_corpus() {
    // Proof level B. The discovered source set and every top-level .srec/.lst/
    // .err artifact must have one exact owner. This proves complete accounting,
    // not execution by a native guest.
    let cases =
        crate::native_reference_parity::native_motorola68000_reference_cases(&workspace_root())
            .expect("complete Motorola 68000 reference inventory");
    assert_eq!(cases.len(), 43);
    let binary_count = cases
        .iter()
        .filter(|case| {
            matches!(
                case.outcome,
                crate::native_reference_parity::NativeMotorola68000ReferenceOutcome::Binary { .. }
            )
        })
        .count();
    let diagnostic_count = cases.len() - binary_count;
    assert_eq!(binary_count, 36);
    assert_eq!(diagnostic_count, 7);
    for case in &cases {
        assert!(matches!(
            account_native_reference_path(&case.asm_path),
            Ok(NativeReferenceAccounting::Motorola68000Reference)
        ));
    }
}
