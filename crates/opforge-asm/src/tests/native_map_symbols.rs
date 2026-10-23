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
