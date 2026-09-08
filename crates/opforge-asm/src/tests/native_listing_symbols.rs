// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

const REPRESENTATIVE_SOURCE: &[u8] = b".module main\n.cpu 68020\n.org 0\n.byte $aa\n.region low, $1000, $10ff\n.section symbols, kind=bss\n.priv\na_private_name_longer_than_fifteen:  .res byte,1 ; private: note\n.pub\nalphaPublicNameLongerThanFifteen:.res byte,1\n.endsection\n.place symbols in low\n.endmodule\n";

const PLACED_ADDRESS_SOURCE: &[u8] = b".module main\n.cpu 68020\n.org 0\n.byte $aa\n.region low, $1000, $10ff\n.region middle, $010000, $0100ff\n.region high, $12345678, $12345777\n.section low_symbols, kind=bss\n.priv\nzetaLowPrivateNameLongerThanFifteen: .res byte,1\n.endsection\n.section high_symbols, kind=bss\n.priv\nz_highPrivateNameLongerThanFifteen: .res byte,1\n.endsection\n.section middle_symbols, kind=bss\n.pub\nalphaMiddlePublicNameLongerThanFifteen: .res byte,1\n.endsection\n.place low_symbols in low\n.place middle_symbols in middle\n.place high_symbols in high\n.endmodule\n";

#[test]
fn native_listing_symbols_live_rust_normalizes_only_leading_label_colons() {
    // Level A: exercise the actual Rust ListingWriter normalization boundary.
    for (source, expected_source) in [
        ("  label: .byte 1", "  label .byte 1"),
        ("label:.byte 1", "label .byte 1"),
        ("label:", "label"),
        ("label:; comment: value", "label; comment: value"),
        ("a_.$: .byte 1", "a_.$ .byte 1"),
        (".byte \"quoted:operand\"", ".byte \"quoted:operand\""),
        ("; comment: value", "; comment: value"),
        ("# directive: value", "# directive: value"),
    ] {
        let mut output = Vec::new();
        ListingWriter::new(&mut output, false)
            .write_line(crate::listing::ListingLine {
                addr: 0,
                bytes: &[],
                status: LineStatus::Ok,
                aux: 0,
                line_num: 1,
                source,
                section: None,
                cond: None,
            })
            .expect("write normalized listing source row");
        let output = std::str::from_utf8(&output).expect("listing UTF-8");
        assert!(
            output.ends_with(&format!("  1  {expected_source}\n")),
            "unexpected source normalization for {source:?}: {output:?}"
        );
    }
}

#[test]
fn native_listing_symbols_live_rust_canonical_footer_and_addresses() {
    // Level A: complete live Rust sources own exact BIN/listing output. These
    // assertions isolate canonical long names, uppercase-key order, visibility,
    // and the minimum-width 4/6/8-digit address formatting.
    for (name, source, expected) in [
        (
            "representative",
            REPRESENTATIVE_SOURCE,
            vec![
                ("main.alphaPublicNameLongerThanFifteen", "1001", "pub"),
                ("main.a_private_name_longer_than_fifteen", "1000", "prv"),
            ],
        ),
        (
            "placed-addresses",
            PLACED_ADDRESS_SOURCE,
            vec![
                (
                    "main.alphaMiddlePublicNameLongerThanFifteen",
                    "010000",
                    "pub",
                ),
                ("main.zetaLowPrivateNameLongerThanFifteen", "1000", "prv"),
                ("main.z_highPrivateNameLongerThanFifteen", "12345678", "prv"),
            ],
        ),
    ] {
        let oracle = build_listing_symbol_oracle(name, source);
        assert_eq!(oracle.bin, [0xaa]);
        let listing = std::str::from_utf8(&oracle.listing).expect("listing UTF-8");
        let actual = listing
            .lines()
            .filter(|line| line.starts_with("main."))
            .collect::<Vec<_>>();
        let expected = expected
            .iter()
            .map(|(symbol, value, visibility)| {
                format!("{symbol:<15}  {value:<8}  {visibility:<3}  {:<4}", "lbl")
            })
            .collect::<Vec<_>>();
        assert_eq!(
            actual,
            expected.iter().map(String::as_str).collect::<Vec<_>>()
        );
    }
}

#[test]
fn native_listing_symbols_representative_fs_uae() {
    run_listing_symbol_case("listing-symbols-representative", REPRESENTATIVE_SOURCE);
}

#[test]
fn native_listing_symbols_placed_addresses_fs_uae() {
    run_listing_symbol_case("listing-symbols-placed-addresses", PLACED_ADDRESS_SOURCE);
}

fn run_listing_symbol_case(name: &'static str, source: &'static [u8]) {
    // Level D: each complete source has its own live Rust oracle and fresh guest.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let root = workspace_root();
    let oracle = build_listing_symbol_oracle(name, source);
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let expected = [
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/symbols.bin",
            rust_oracle: &oracle.bin,
        },
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/symbols.lst",
            rust_oracle: &oracle.listing,
        },
    ];
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source),
        command_template: Some(
            "{input} --bin {guest_work_dir}build/symbols.bin --list {guest_work_dir}build/symbols.lst --cpu 68020 --opasm-package {package}",
        ),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifacts(&expected),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &[case])
        .expect("listing-symbol FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(run.protocol_completed);
            assert!(run.success);
            assert_eq!(run.exit_code, Some(0));
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/symbols.bin"),
                oracle.bin
            );
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/symbols.lst"),
                oracle.listing
            );
        }
    }
}

struct ListingSymbolOracle {
    bin: Vec<u8>,
    listing: Vec<u8>,
}

fn build_listing_symbol_oracle(name: &str, source: &[u8]) -> ListingSymbolOracle {
    let oracle_dir = create_temp_dir(&format!("native-listing-symbols-{name}"));
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _guard = OracleDir(oracle_dir.clone());
    fs::create_dir_all(oracle_dir.join("build")).expect("create listing-symbol directory");
    let input = oracle_dir.join("input.asm");
    let bin = oracle_dir.join("build/symbols.bin");
    let listing = oracle_dir.join("build/symbols.lst");
    fs::write(&input, source).expect("write listing-symbol source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin.to_string_lossy().into_owned(),
        "--list".to_string(),
        listing.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "68020".to_string(),
    ]);
    let mut config = validate_cli(&cli).expect("validate listing-symbol Rust CLI");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("run listing-symbol Rust CLI");
    ListingSymbolOracle {
        bin: fs::read(bin).expect("read listing-symbol BIN"),
        listing: fs::read(listing).expect("read listing-symbol listing"),
    }
}
