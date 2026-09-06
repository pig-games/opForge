// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

const VISIBILITY_LISTING_SOURCE: &[u8] = b".module main\n.cpu 6502\n.region rom, $8000, $80ff\n.section code, align=1\n.pub\n.byte $11\n.priv\n.byte $22\n.endsection\n.place code in rom\n.endmodule\n";

#[test]
fn native_listing_visibility_source_rows_live_rust_order_and_state() {
    // Level A: live Rust owns the exact listing rows/BIN, while a separate
    // labeled source proves that both consumed directives still change symbol
    // visibility without becoming output-producing statements.
    let oracle = build_visibility_listing_oracle();
    assert_eq!(oracle.bin, [0x11, 0x22]);
    let listing = std::str::from_utf8(&oracle.listing).expect("listing UTF-8");
    let rows = listing.lines().collect::<Vec<_>>();
    let public_row = rows
        .iter()
        .position(|row| row.contains(".pub"))
        .expect("public visibility source row");
    let first_byte_row = rows
        .iter()
        .position(|row| row.contains(".byte $11"))
        .expect("first byte source row");
    let private_row = rows
        .iter()
        .position(|row| row.contains(".priv"))
        .expect("private visibility source row");
    let second_byte_row = rows
        .iter()
        .position(|row| row.contains(".byte $22"))
        .expect("second byte source row");
    assert!(
        public_row < first_byte_row
            && first_byte_row < private_row
            && private_row < second_byte_row
    );
    assert_eq!(rows.iter().filter(|row| row.contains(".pub")).count(), 1);
    assert_eq!(rows.iter().filter(|row| row.contains(".priv")).count(), 1);
    for index in [public_row, first_byte_row, private_row, second_byte_row] {
        assert!(rows[index].ends_with("; [section code]"));
    }

    let assembler = run_passes(&[
        ".module main",
        ".cpu 6502",
        ".region rom, $8000, $80ff",
        ".section code, align=1",
        ".pub",
        "public_label: .byte $11",
        ".priv",
        "private_label: .byte $22",
        ".endsection",
        ".place code in rom",
        ".endmodule",
    ]);
    let symbols = assembler.symbols();
    let public = symbols
        .entries()
        .iter()
        .find(|entry| entry.name.eq_ignore_ascii_case("main.public_label"))
        .expect("public label");
    let private = symbols
        .entries()
        .iter()
        .find(|entry| entry.name.eq_ignore_ascii_case("main.private_label"))
        .expect("private label");
    assert_eq!(public.visibility, SymbolVisibility::Public);
    assert_eq!(private.visibility, SymbolVisibility::Private);
}

#[test]
fn native_listing_visibility_source_rows_consumed_path_records_once() {
    // Level B: only the successful ordinary visibility-consumed path records
    // the original line, propagates recording failure, and returns without
    // statement parsing or package routing. Definition preprocessing remains
    // ahead of this ordinary path.
    let source = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read native line processor");
    let consumed = source
        .split("; Consumed metadata still owns a listing row")
        .nth(1)
        .and_then(|tail| tail.split("\nvisibilityPass").next())
        .expect("visibility-consumed path");
    assert!(source_contains_in_order(
        consumed,
        &[
            "jsr assembly_session.opforgeNativeCliRecordSourceLine",
            "bne.w fail",
            "moveq #0, d0",
            "rts",
        ]
    ));
    assert_eq!(
        consumed
            .matches("assembly_session.opforgeNativeCliRecordSourceLine")
            .count(),
        1
    );
    assert!(!consumed.contains("opforgeNativeCliParseStatementInvocationV1"));
    assert!(!consumed.contains("opforgeNativeCliProcessPackageLineV1"));
    assert!(source_contains_in_order(
        &source,
        &[
            "tst.w state.NativeCliPreprocessActiveDefinition",
            "bpl.s visibilityPass",
            "jsr preprocessor.opforgeNativeCliTrackVisibilityV1",
        ]
    ));
}

#[test]
fn native_listing_visibility_source_rows_bin_and_listing_fs_uae() {
    // Level D: one fresh guest must reproduce both complete live-Rust artifacts
    // and return an explicit zero exit.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let root = workspace_root();
    let oracle = build_visibility_listing_oracle();
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let expected = [
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/visibility.bin",
            rust_oracle: &oracle.bin,
        },
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/visibility.lst",
            rust_oracle: &oracle.listing,
        },
    ];
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "listing-visibility-source-rows",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(VISIBILITY_LISTING_SOURCE),
        command_template: Some(
            "{input} --bin {guest_work_dir}build/visibility.bin --list {guest_work_dir}build/visibility.lst --cpu m6502 --opasm-package {package}",
        ),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifacts(&expected),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &[case])
        .expect("listing visibility FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(run.protocol_completed);
            assert!(run.success);
            assert_eq!(run.exit_code, Some(0));
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/visibility.bin"),
                oracle.bin
            );
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/visibility.lst"),
                oracle.listing
            );
        }
    }
}

struct VisibilityListingOracle {
    bin: Vec<u8>,
    listing: Vec<u8>,
}

fn build_visibility_listing_oracle() -> VisibilityListingOracle {
    let oracle_dir = create_temp_dir("native-listing-visibility-oracle");
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _oracle_guard = OracleDir(oracle_dir.clone());
    fs::create_dir_all(oracle_dir.join("build")).expect("create listing oracle directory");
    let input = oracle_dir.join("input.asm");
    let bin = oracle_dir.join("build/visibility.bin");
    let listing = oracle_dir.join("build/visibility.lst");
    fs::write(&input, VISIBILITY_LISTING_SOURCE).expect("write listing oracle source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin.to_string_lossy().into_owned(),
        "--list".to_string(),
        listing.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m6502".to_string(),
    ]);
    let mut config = validate_cli(&cli).expect("validate listing visibility Rust CLI");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("run listing visibility Rust CLI");
    VisibilityListingOracle {
        bin: fs::read(bin).expect("read listing visibility Rust BIN"),
        listing: fs::read(listing).expect("read listing visibility Rust listing"),
    }
}
