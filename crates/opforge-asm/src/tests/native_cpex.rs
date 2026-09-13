// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

#[test]
fn native_cpex_harness_uses_real_loader_selection_and_getters() {
    // Level B: the focused executable uses the production loader, alias-aware
    // pipeline selector, and all public runtime-context property getters. Expected
    // property values remain absent from native source.
    let source = fs::read_to_string(
        workspace_root()
            .join("native/motorola68000/amigaos/test-harnesses/tkpkg/tkpkg_cpex_harness.asm"),
    )
    .expect("read CPEX harness");
    assert!(source_contains_in_order(
        &source,
        &[
            "jsr package_loader.tkpkgPackageLoaderLoadStagedV1",
            "lea CpuAliasA, a1",
            "bsr.w selectCpu",
            "bsr.w captureProperties",
            "lea CpuCanonicalB, a1",
            "bsr.w selectCpu",
            "bsr.w captureProperties",
        ]
    ));
    assert!(source.contains("jsr pipeline.tkpkgPipelineSetActiveV1"));
    assert!(source_contains_in_order(
        &source,
        &[
            "captureProperties\t.block",
            "jsr runtime_context.getCpuWordSizeBytesV1",
            "move.l d1, (a3)+",
            "jsr runtime_context.getCpuMaxProgramAddressV1",
            "move.l d1, (a3)+",
        ]
    ));
    assert!(!source.contains("$01ffffff"));
    assert!(!source.contains("#3, d1"));
    assert!(
        !source
            .split_whitespace()
            .collect::<String>()
            .contains(",0,0"),
        "absent dialect must end at the first separator"
    );
}

#[test]
fn native_cpex_failed_commit_invalidates_selection_and_properties() {
    // Level B: commit invalidates the selected-pipeline bit and CPEX presence
    // before any fallible locator mutation, so a later commit failure cannot
    // expose getters from the preceding successful CPU selection.
    let pipeline = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm"),
    )
    .expect("read native pipeline owner");
    let commit = pipeline
        .split("commitActiveSelectionV1\t.block")
        .nth(1)
        .and_then(|tail| tail.split(".bend  ; commitActiveSelectionV1").next())
        .expect("active selection commit body");
    assert!(source_contains_in_order(
        commit,
        &[
            "bclr #1, buffers.PackageStateFlags",
            "clr.b buffers.ActiveCpuExecutionPresent",
            "bsr.w copyLocatorToBufferV1",
            "bne.w commitDone",
            "move.b buffers.PendingCpuExecutionPresent, d0",
            "move.b d0, buffers.ActiveCpuExecutionPresent",
            "bset #1, buffers.PackageStateFlags",
        ]
    ));
}

#[test]
fn native_cpex_staged_length_and_request_wire_contract() {
    // Level B: staged length survives the clear loop's D0 counter, and
    // native requests use the ABI's little-endian fields on a big-endian host.
    let loader = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/tkpkg/tkpkg_package_loader.asm"),
    )
    .expect("read package loader");
    let count = loader
        .split("cpexCountCanonicalCpusV1\t.block")
        .nth(1)
        .expect("count helper")
        .split("\t.bend  ; cpexCountCanonicalCpusV1")
        .next()
        .unwrap();
    assert!(
        source_contains_in_order(
            count,
            &[
                "move.l d6, d0",
                "movem.l (sp)+, d2-d7/a0-a6",
                "tst.l d1",
                "rts",
            ]
        ),
        "return flags must reflect status, not the nonzero CPU count"
    );
    assert!(source_contains_in_order(
        &loader,
        &[
            "tkpkgPackageLoaderLoadStagedV1\t.block",
            "move.l d0, -(sp)",
            "bsr.w clearLoadedState",
            "move.l (sp)+, d0",
            "bsr.w validateStagedPackageV1",
        ]
    ));
    let harness = fs::read_to_string(
        workspace_root()
            .join("native/motorola68000/amigaos/test-harnesses/tkpkg/tkpkg_cpex_harness.asm"),
    )
    .expect("read harness");
    assert!(source_contains_in_order(
        &harness,
        &[
            "move.b d0, abi.CB_INPUT_PTR(a0)",
            "lsr.w #8, d0",
            "move.b d0, abi.CB_INPUT_PTR + 1(a0)",
            "move.b d3, abi.CB_INPUT_LEN(a0)",
            "lsr.w #8, d3",
            "move.b d3, abi.CB_INPUT_LEN + 1(a0)",
            "jsr pipeline.tkpkgPipelineSetActiveV1",
        ]
    ));
}

#[test]
fn native_cpex_harness_fs_uae() {
    // Level D: separate fresh positive and legacy-negative guests load real
    // packages. The positive artifact must equal values decoded from those
    // same package bytes; the legacy request must fail explicitly.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let (cpex_package, legacy_package, oracle) = cpex_harness_packages_and_oracle();
    match crate::fs_uae_smoke::run_tkpkg_cpex_harness_from_env(
        &workspace_root(),
        &cpex_package,
        &legacy_package,
        &oracle,
    )
    .expect("CPEX native harness")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 2);
            assert!(runs[0].protocol_completed);
            assert!(runs[0].success);
            assert_eq!(runs[0].exit_code, Some(0));
            assert_eq!(
                captured_fs_uae_artifact(&runs[0], "Work/build/cpex-values.bin"),
                oracle
            );
            assert!(runs[1].protocol_completed);
            assert!(!runs[1].success);
            assert_ne!(runs[1].exit_code, Some(0));
            assert!(format!("{}\n{}", runs[1].stdout, runs[1].stderr)
                .contains("CPEX property unavailable"));
        }
    }
}

fn cpex_harness_packages_and_oracle() -> (Vec<u8>, Vec<u8>, Vec<u8>) {
    let base = tkpkg_smoke_package_bytes();
    let mut chunks = package::decode_hierarchy_chunks(&base).expect("decode smoke package");
    let properties = chunks
        .cpu_execution_properties
        .as_mut()
        .expect("smoke package CPEX");
    for property in properties.iter_mut() {
        if property.cpu_id == "m68000" {
            property.word_size_bytes = 3;
            property.max_program_address = 0x01ff_ffff;
            property.data_little_endian = true; // Deliberately differs from registry.
        } else if property.cpu_id == "m68020" {
            property.word_size_bytes = 2;
            property.max_program_address = 0xffff;
            property.data_little_endian = false;
        }
    }
    let cpex_package =
        package::encode_hierarchy_chunks_from_chunks(&chunks).expect("encode focused CPEX package");
    let model = load_opasm_model_from_package_bytes(&cpex_package);
    assert_eq!(
        model.canonical_cpu_id_for_input("68000").as_deref(),
        Some("m68000")
    );
    let mut oracle = Vec::with_capacity(24);
    for cpu in ["68000", "m68020"] {
        let property = model
            .cpu_execution_properties(cpu)
            .expect("resolve CPEX CPU")
            .expect("CPEX property present");
        oracle.extend_from_slice(&property.word_size_bytes.to_be_bytes());
        oracle.extend_from_slice(&property.max_program_address.to_be_bytes());
        oracle.extend_from_slice(&u32::from(!property.data_little_endian).to_be_bytes());
    }
    assert_eq!(
        oracle,
        [0, 0, 0, 3, 1, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 255, 255, 0, 0, 0, 1]
    );

    chunks.cpu_execution_properties = None;
    let legacy_package = package::encode_hierarchy_chunks_from_chunks(&chunks)
        .expect("encode focused legacy package");
    (cpex_package, legacy_package, oracle)
}
