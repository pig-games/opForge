// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

const SYMBOL_ORDER: [u8; 36] = [
    17, 0, 35, 2, 33, 4, 31, 6, 29, 8, 27, 10, 25, 12, 23, 14, 21, 16, 19, 18, 1, 20, 3, 22, 5, 24,
    7, 26, 9, 28, 11, 30, 13, 32, 15, 34,
];
const PUBLIC_POSITIONS: [usize; 2] = [1, 33];

#[test]
fn native_map_symbols_live_rust_complete_order_and_visibility() {
    // Level A: live Rust fixes canonical names, stable lexical order, values,
    // and public filtering for labels below and above native index 32.
    let oracle = build_map_symbols_oracle();
    assert_eq!(oracle.all, expected_map(false));
    assert_eq!(oracle.public, expected_map(true));
    assert_eq!(oracle.bin, (0_u8..36).collect::<Vec<_>>());
}

#[test]
fn native_map_symbols_canonical_successor_and_export_contract() {
    // Level B: bind the complete-table successor traversal and the in-place
    // owner-prefix/raw-export comparison, without a 32-bit visited bitmap.
    let native = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opforge-cli/source_artifacts.asm"),
    )
    .expect("read native source-artifact owner");
    let append = native
        .split("mapAppendSymbolsV1\t.block")
        .nth(1)
        .and_then(|tail| tail.split(".bend  ; mapAppendSymbolsV1").next())
        .expect("native map-symbol writer body");
    assert!(source_contains_in_order(
        append,
        &[
            "jsr engine.opasmEngineGetLabelCountV1",
            "moveq #-1, d5",
            "nextSymbol",
            "scan",
            "move.l d5, d0",
            "jsr engine.opasmEngineGetLabelNameV1",
            "bsr.w compareFoldedNamesV1",
            "cmp.l d5, d4",
            "move.l d7, d5",
            "move.l d7, d0",
            "jsr engine.opasmEngineGetLabelNameV1",
            "bsr.w mapAppendCStringV1",
            "bra.w nextSymbol",
        ]
    ));
    assert!(!append.contains("btst"));
    assert!(!append.contains("bset"));
    assert!(!append.contains("appendRootModuleNameV1"));

    let visibility = native
        .split("labelIsPublicV1\t.block")
        .nth(1)
        .and_then(|tail| tail.split(".bend  ; labelIsPublicV1").next())
        .expect("native map visibility body");
    assert!(source_contains_in_order(
        visibility,
        &[
            "jsr engine.opasmEngineGetLabelNameV1",
            "lea state.NativeCliOrdinaryExportOwnerTable, a0",
            "lea state.NativeCliModuleNameTable, a1",
            "ownerCharacter",
            "cmp.b d2, d3",
            "ownerEnd",
            "cmpi.b #'.', (a0)+",
            "lea state.NativeCliOrdinaryExportNameOffsetTable, a1",
            "lea state.NativeCliOrdinaryExportNamePool, a1",
            "bsr.w namesEqualFoldedV1",
        ]
    ));
}

#[test]
fn native_map_symbols_all_and_public_fs_uae() {
    // Level D: one fresh guest reproduces both exact live Rust maps, including
    // canonical names and the public label whose engine index exceeds 32.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let root = workspace_root();
    let source = map_symbols_source();
    let oracle = build_map_symbols_oracle_from_source(source.as_bytes());
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let expected = [
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/symbols.bin",
            rust_oracle: &oracle.bin,
        },
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/symbols-all.map",
            rust_oracle: &oracle.all,
        },
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/symbols-public.map",
            rust_oracle: &oracle.public,
        },
    ];
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "map-symbols-all-public-36",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(
            "{input} --bin {guest_work_dir}build/symbols.bin --cpu m6502 --opasm-package {package}",
        ),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifacts(&expected),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &[case])
        .expect("map-symbol FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one map-symbol run");
            let run = &runs[0];
            assert!(run.protocol_completed);
            assert!(run.success);
            assert_eq!(run.exit_code, Some(0));
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/symbols.bin"),
                oracle.bin
            );
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/symbols-all.map"),
                oracle.all
            );
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/symbols-public.map"),
                oracle.public
            );
        }
    }
}

struct MapSymbolsOracle {
    bin: Vec<u8>,
    all: Vec<u8>,
    public: Vec<u8>,
}

fn map_symbols_source() -> String {
    let mut source = String::from(".module main\n.cpu 6502\n.org $8000\n");
    for (position, symbol) in SYMBOL_ORDER.iter().enumerate() {
        if PUBLIC_POSITIONS.contains(&position) {
            source.push_str(".pub\n");
        }
        source.push_str(&format!("sym{symbol:02}: .byte ${position:02x}\n"));
        if PUBLIC_POSITIONS.contains(&position) {
            source.push_str(".priv\n");
        }
    }
    source.push_str(".mapfile \"build/symbols-all.map\", symbols=all\n");
    source.push_str(".mapfile \"build/symbols-public.map\", symbols=public\n");
    source.push_str(".endmodule\n");
    source
}

fn expected_map(public_only: bool) -> Vec<u8> {
    let mut text = String::from(
        "Regions\nname start end used free align\n\nSections\nname base size kind region\n",
    );
    text.push_str("\nSymbols\nname value visibility\n");
    for symbol in 0_u8..36 {
        let position = SYMBOL_ORDER
            .iter()
            .position(|candidate| *candidate == symbol)
            .expect("symbol permutation is complete");
        let is_public = PUBLIC_POSITIONS.contains(&position);
        if !public_only || is_public {
            let visibility = if is_public { "public" } else { "private" };
            text.push_str(&format!(
                "main.sym{symbol:02} {:04X} {visibility}\n",
                0x8000 + position
            ));
        }
    }
    text.into_bytes()
}

fn build_map_symbols_oracle() -> MapSymbolsOracle {
    let source = map_symbols_source();
    build_map_symbols_oracle_from_source(source.as_bytes())
}

fn build_map_symbols_oracle_from_source(source: &[u8]) -> MapSymbolsOracle {
    let oracle_dir = create_temp_dir("native-map-symbols-oracle");
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _oracle_guard = OracleDir(oracle_dir.clone());
    fs::create_dir_all(oracle_dir.join("build")).expect("create map-symbol oracle directory");
    let input_path = oracle_dir.join("input.asm");
    fs::write(&input_path, source).expect("write map-symbol oracle source");
    let bin_path = oracle_dir.join("build/symbols.bin");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input_path.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin_path.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m6502".to_string(),
    ]);
    let mut config = validate_cli(&cli).expect("validate map-symbol Rust CLI");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("run map-symbol live Rust CLI");
    MapSymbolsOracle {
        bin: fs::read(bin_path).expect("read map-symbol Rust BIN"),
        all: fs::read(oracle_dir.join("build/symbols-all.map")).expect("read all-symbol Rust map"),
        public: fs::read(oracle_dir.join("build/symbols-public.map"))
            .expect("read public-symbol Rust map"),
    }
}
