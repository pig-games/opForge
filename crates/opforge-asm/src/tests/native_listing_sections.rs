// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

const LISTING_SECTION_SOURCE: &[u8] = b".module main\n.cpu 6502\n.region rom, $8000, $80ff\n.section code, align=1\n; inside comment\n\n.byte $11,$22,$33\n.endsection\n.place code in rom\n.endmodule\n";

#[test]
fn native_listing_sections_hunk_lea_symbol_addend_discriminator() {
    // Level B: execute the real Rust assembler/Hunk path used to build native
    // code and inspect both address forms in its emitted CODE payload.
    let source = b".module listing.bound.probe\n.cpu 68020\n.section code, kind=code\n\tlea ProbeBuffer+24000,a4\n\tlea ProbeBuffer.l,a5\n\tadda.l #24000,a5\n\trts\nreserveProbe:\n\tmove.l d1,-(sp)\n\tmove.l a4,d1\n\tsub.l a2,d1\n\tcmp.l d0,d1\n\tmovem.l (sp)+,d1\n\trts\npointerProbe:\n\tcmpa.l a4,a2\n\tcmpa.l a6,a5\n\ttst.b (a1)+\n\tbsr.w privateWrapper\n\trts\nprivateWrapper:\n\trts\n.endsection\n.section bss, kind=bss\n.res byte,16098\nProbeBuffer: .res byte,24001\n.endsection\n.output \"build/probe.hunk\", format=hunk, sections=code,bss\n.endmodule\n";
    let oracle_dir = create_temp_dir("native-listing-lea-addend");
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _oracle_guard = OracleDir(oracle_dir.clone());
    fs::create_dir_all(oracle_dir.join("build")).expect("create Hunk probe directory");
    let input = oracle_dir.join("input.asm");
    fs::write(&input, source).expect("write Hunk probe source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m68020".to_string(),
    ]);
    let mut config = validate_cli(&cli).expect("validate Hunk probe CLI");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("assemble Hunk probe");
    let hunk = fs::read(oracle_dir.join("build/probe.hunk")).expect("read Hunk probe");
    let expected_code = [
        0x49, 0xf9, 0x00, 0x00, 0x9c, 0xa2, 0x4b, 0xf9, 0x00, 0x00, 0x3e, 0xe2, 0xdb, 0xfc, 0x00,
        0x00, 0x5d, 0xc0, 0x4e, 0x75,
    ];
    assert!(
        hunk.windows(expected_code.len())
            .any(|bytes| bytes == expected_code),
        "Hunk CODE must retain the nonzero BSS offset and full $9CA2 LEA addend: {hunk:02X?}"
    );
    let expected_reserve = [
        0x2f, 0x01, 0x22, 0x0c, 0x92, 0x8a, 0xb2, 0x80, 0x4c, 0xdf, 0x00, 0x02, 0x4e, 0x75,
    ];
    assert!(
        hunk.windows(expected_reserve.len())
            .any(|bytes| bytes == expected_reserve),
        "Hunk CODE must preserve reserve subtraction/CMP CCR through MOVEM restore: {hunk:02X?}"
    );
    let expected_pointer_loop = [
        0xb5, 0xcc, 0xbb, 0xce, 0x4a, 0x19, 0x61, 0x00, 0x00, 0x04, 0x4e, 0x75, 0x4e, 0x75,
    ];
    assert!(
        hunk.windows(expected_pointer_loop.len())
            .any(|bytes| bytes == expected_pointer_loop),
        "Hunk CODE must retain CMPA/TST pointer forms and the resolved private BSR.W: {hunk:02X?}"
    );
    let words = hunk
        .chunks_exact(4)
        .map(|chunk| u32::from_be_bytes(chunk.try_into().expect("Hunk word")))
        .collect::<Vec<_>>();
    let expected_reloc32 = [0x0000_03ec, 2, 1, 2, 8, 0];
    assert!(
        words
            .windows(expected_reloc32.len())
            .any(|group| group == expected_reloc32),
        "Hunk RELOC32 must relocate both LEA extension offsets 2 and 8 from CODE to BSS: {words:08X?}"
    );
}

#[test]
fn native_listing_sections_live_rust_annotations_and_boundary_label_ownership() {
    // Level A: live Rust fixes exact BIN/listing output and independently
    // proves labels on section boundaries retain their pre-dispatch ownership.
    let oracle = build_listing_section_oracle();
    assert_eq!(oracle.bin, [0x11, 0x22, 0x33]);
    let listing = std::str::from_utf8(&oracle.listing).expect("listing UTF-8");
    let lines = listing.lines().collect::<Vec<_>>();
    assert!(lines
        .iter()
        .find(|line| line.contains(".section code, align=1"))
        .expect("opening section listing row")
        .ends_with("; [section code]"));
    assert!(lines
        .iter()
        .find(|line| line.contains("; inside comment"))
        .expect("inside comment listing row")
        .ends_with("; [section code]"));
    assert!(lines
        .iter()
        .find(|line| line.contains(".byte $11,$22,$33"))
        .expect("inside data listing row")
        .ends_with("; [section code]"));
    let comment_index = lines
        .iter()
        .position(|line| line.contains("; inside comment"))
        .expect("inside comment row index");
    assert!(lines[comment_index + 1].ends_with("; [section code]"));
    assert!(!lines
        .iter()
        .find(|line| line.contains(".endsection"))
        .expect("closing section listing row")
        .contains("; [section "));

    let assembler = run_passes(&[
        ".module main",
        ".cpu 6502",
        ".region rom, $8000, $80ff",
        "open_label: .section code, align=1",
        ".byte $11,$22,$33",
        "close_label: .endsection",
        ".place code in rom",
        ".endmodule",
    ]);
    let symbols = assembler.symbols();
    let symbol_value = |name: &str| {
        symbols
            .entries()
            .iter()
            .find(|entry| entry.name.eq_ignore_ascii_case(name))
            .map(|entry| entry.val)
    };
    assert_eq!(symbol_value("main.open_label"), Some(0));
    assert_eq!(symbol_value("main.close_label"), Some(0x8003));
}

#[test]
fn native_listing_sections_uses_monotonic_post_state_projection() {
    // Level B: the listing projection reads the existing pre-dispatch table;
    // it neither allocates nor rewrites semantic statement ownership.
    let root = workspace_root();
    let engine =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read native engine owner");
    let accessor = engine
        .split("opasmEngineGetStatementSourceRecordIndexV1\t.block")
        .nth(1)
        .and_then(|tail| {
            tail.split(".bend  ; opasmEngineGetStatementSourceRecordIndexV1")
                .next()
        })
        .expect("statement source-record accessor body");
    assert!(source_contains_in_order(
        accessor,
        &[
            "cmp.l OpasmEngineStmtCount.l, d0",
            "lea OpasmEngineStmtSourceRecordIndexTable.l, a0",
            "move.l 0(a0, d0.l), d0",
            "invalid",
            "moveq #-1, d0",
        ]
    ));

    let layout =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_layout.asm"))
            .expect("read native layout owner");
    let projection = layout
        .split("getListingSectionNameV1\t.block")
        .nth(1)
        .and_then(|tail| tail.split(".bend  ; getListingSectionNameV1").next())
        .expect("listing section projection body");
    assert!(source_contains_in_order(
        projection,
        &[
            "cmpi.l #-1, d0",
            "lea OpasmLayoutStatementSectionIndices.l, a0",
            "move.w 0(a0, d0.l), d5",
            "finalState",
            "move.w OpasmLayoutActiveSectionIndex.l, d5",
            "bsr.w getSectionNameV1",
        ]
    ));
    assert!(!projection.contains("move.w d5, 0(a0, d0.l)"));

    let output = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opasm/opasm_output_artifacts.asm"),
    )
    .expect("read native output-artifact owner");
    let listing = output
        .split("opasmOutputBuildListingArtifactV1\t.block")
        .nth(1)
        .and_then(|tail| {
            tail.split(".bend  ; opasmOutputBuildListingArtifactV1")
                .next()
        })
        .expect("native listing writer body");
    assert!(source_contains_in_order(
        listing,
        &[
            "lea OpasmListingArtifactBuffer.l, a2",
            "movea.l a2, a4",
            "adda.l #OPASM_OUTPUT_LISTING_BUFFER_CAPACITY, a4",
            "suba.l a5, a5",
            "jsr engine.opasmEngineGetStatementCountV1",
            "movea.l d0, a6",
            "listingSuccessor",
            "cmpa.l a6, a5",
            "jsr engine.opasmEngineGetStatementSourceRecordIndexV1",
            "cmp.l d7, d0",
            "addq.l #1, a5",
            "listingSuccessorFound",
            "jsr layout.getListingSectionNameV1",
            "lea OpasmListingSectionPrefix.l, a0",
            "bsr.w opasmListingAppendCString",
            "bne.w listingCapacityFail",
            "move.l d3, d0",
            "addq.l #1, d0",
            "bsr.w opasmListingReserve",
            "bcs.w listingCapacityFail",
            "listingSectionCopy",
            "move.b #']', (a2)+",
            "listingNewline",
            "cmpa.l a4, a2",
            "move.b #10, (a2)+",
            "footer",
            "lea OpasmListingLinesPrefix.l, a0",
            "bsr.w opasmListingAppendCString",
            "bne.w listingCapacityFail",
        ]
    ));
    assert!(source_contains_in_order(
        listing,
        &[
            "jsr engine.opasmEngineGetSourceRecordTextV1",
            "tst.l d0",
            "bsr.w opasmListingReserve",
            "bcs.w listingCapacityFail",
            "move.l d0, d3",
            "sourceLoop",
            "move.b (a0)+, (a2)+",
        ]
    ));
}

#[test]
fn native_listing_sections_checked_append_exact_fit_and_overflow_contract() {
    // Level C: the production helper's 68020 CMP/BCS contract accepts an
    // exact-fit span and rejects a one-byte overflow. This models only CCR;
    // Level D remains the proof that the complete native writer executes.
    let output = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_output_artifacts.asm"),
    )
    .expect("read native output-artifact owner");
    let reserve = output
        .split("opasmListingReserve\t.block")
        .nth(1)
        .and_then(|tail| tail.split(".bend  ; opasmListingReserve").next())
        .expect("listing reserve helper body");
    assert!(source_contains_in_order(
        reserve,
        &[
            "move.l a4, d1",
            "sub.l a2, d1",
            "cmp.l d0, d1",
            "movem.l (sp)+, d1",
            "rts",
        ]
    ));
    let cmp_sets_carry = |requested: u32, available: u32| available < requested;
    assert!(!cmp_sets_carry(24_000, 24_000), "exact fit is accepted");
    assert!(
        cmp_sets_carry(24_001, 24_000),
        "one-byte overflow is rejected"
    );

    let append_cstring = output
        .split("opasmListingAppendCString\t.block")
        .nth(1)
        .and_then(|tail| tail.split(".bend  ; opasmListingAppendCString").next())
        .expect("checked C-string appender body");
    assert!(source_contains_in_order(
        append_cstring,
        &[
            "scan",
            "tst.b (a1)+",
            "addq.l #1, d0",
            "reserve",
            "bsr.w opasmListingReserve",
            "bcs.s fail",
            "bsr.w opasmOutputAppendCString",
        ]
    ));
}

#[test]
fn native_listing_sections_bin_and_listing_fs_uae() {
    // Level D: one fresh guest reproduces the exact live Rust BIN and listing.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let root = workspace_root();
    let oracle = build_listing_section_oracle();
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let expected = [
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/listing.bin",
            rust_oracle: &oracle.bin,
        },
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/listing.lst",
            rust_oracle: &oracle.listing,
        },
    ];
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "listing-section-annotations",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(LISTING_SECTION_SOURCE),
        command_template: Some(
            "{input} --bin {guest_work_dir}build/listing.bin --list {guest_work_dir}build/listing.lst --cpu m6502 --opasm-package {package}",
        ),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifacts(&expected),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &[case])
        .expect("listing-section FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one listing-section run");
            let run = &runs[0];
            assert!(run.protocol_completed);
            assert!(run.success);
            assert_eq!(run.exit_code, Some(0));
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/listing.bin"),
                oracle.bin
            );
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/listing.lst"),
                oracle.listing
            );
        }
    }
}

#[test]
fn native_listing_sections_capacity_failure_fs_uae() {
    // Level D negative: a complete valid source whose live Rust listing exceeds
    // native's fixed capacity must fail freshly at the native artifact builder.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let root = workspace_root();
    let source = oversized_listing_source();
    let oracle = build_listing_oracle_from_source(
        source.as_bytes(),
        "native-listing-capacity-oracle",
        "capacity",
    );
    assert_eq!(oracle.bin, [0xaa]);
    assert!(oracle.listing.len() > 24_000);
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "listing-capacity-failure",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(
            "{input} --bin {guest_work_dir}build/capacity.bin --list {guest_work_dir}build/capacity.lst --cpu m6502 --opasm-package {package}",
        ),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExpectedFailureContaining(
            "ERROR OPC-NCLI044: native output artifact build failed",
        ),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &[case])
        .expect("listing capacity FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one listing-capacity run");
            let run = &runs[0];
            assert!(run.protocol_completed);
            assert!(!run.success);
            assert!(run.exit_code.is_some_and(|code| code != 0));
            assert!(format!("{}\n{}", run.stdout, run.stderr)
                .contains("ERROR OPC-NCLI044: native output artifact build failed"));
        }
    }
}

struct ListingSectionOracle {
    bin: Vec<u8>,
    listing: Vec<u8>,
}

fn build_listing_section_oracle() -> ListingSectionOracle {
    build_listing_oracle_from_source(
        LISTING_SECTION_SOURCE,
        "native-listing-sections-oracle",
        "listing",
    )
}

fn oversized_listing_source() -> String {
    let mut source = String::from(
        ".module main\n.cpu 6502\n.region rom, $8000, $80ff\n.section code, align=1\n.byte $aa\n",
    );
    for index in 0..600 {
        source.push_str(&format!(
            "; capacity row {index:03} keeps the complete listing source visible\n"
        ));
    }
    source.push_str(".endsection\n.place code in rom\n.endmodule\n");
    source
}

fn build_listing_oracle_from_source(
    source: &[u8],
    temp_label: &str,
    output_stem: &str,
) -> ListingSectionOracle {
    let oracle_dir = create_temp_dir(temp_label);
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _oracle_guard = OracleDir(oracle_dir.clone());
    fs::create_dir_all(oracle_dir.join("build")).expect("create listing oracle directory");
    let input_path = oracle_dir.join("input.asm");
    let bin_path = oracle_dir.join(format!("build/{output_stem}.bin"));
    let list_path = oracle_dir.join(format!("build/{output_stem}.lst"));
    fs::write(&input_path, source).expect("write listing oracle source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input_path.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin_path.to_string_lossy().into_owned(),
        "--list".to_string(),
        list_path.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m6502".to_string(),
    ]);
    let mut config = validate_cli(&cli).expect("validate listing-section Rust CLI");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("run listing-section live Rust CLI");
    ListingSectionOracle {
        bin: fs::read(bin_path).expect("read listing-section Rust BIN"),
        listing: fs::read(list_path).expect("read listing-section Rust listing"),
    }
}
