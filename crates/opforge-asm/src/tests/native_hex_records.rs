// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

// Keep each source line within the documented 64-token native limit.
// The second contiguous span remains 33 bytes across two directives.
const HEX_RECORD_SOURCE: &[u8] = b".module main\n.cpu 6502\n.org $8000\n.byte 1,2,3,4,5\n.org $8006\n.byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15\n.byte 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32\n.meta\n.output\n.hex \"build/hex-records\"\n.endoutput\n.endmeta\n.endmodule\n";

const EXPECTED_BIN: &[u8] = &[
    1, 2, 3, 4, 5, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
];

const EXPECTED_HEX: &[u8] = b":0580000001020304056C\n:20800600000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F6A\n:018026002039\n:00000001FF\n";

#[test]
fn native_hex_records_gap_and_thirty_three_byte_rust_oracle() {
    // Level A: the live Rust CLI is authoritative for both sparse Intel HEX
    // segmentation and the flat BIN representation of the same source.
    let oracle = build_hex_record_oracle();
    assert_eq!(oracle.bin, EXPECTED_BIN);
    assert_eq!(oracle.hex, EXPECTED_HEX);
}

struct HexRecordOracle {
    bin: Vec<u8>,
    hex: Vec<u8>,
}

fn build_hex_record_oracle() -> HexRecordOracle {
    let oracle_dir = create_temp_dir("native-hex-records-oracle");
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _oracle_guard = OracleDir(oracle_dir.clone());
    fs::create_dir_all(oracle_dir.join("build")).expect("create HEX oracle directory");
    let input_path = oracle_dir.join("input.asm");
    fs::write(&input_path, HEX_RECORD_SOURCE).expect("write HEX oracle source");
    let bin_path = oracle_dir.join("build/hex-records.bin");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input_path.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin_path.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m6502".to_string(),
    ]);
    let mut config = validate_cli(&cli).expect("validate HEX-record Rust CLI");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("run HEX-record live Rust CLI");
    HexRecordOracle {
        bin: fs::read(bin_path).expect("read HEX-record Rust BIN oracle"),
        hex: fs::read(oracle_dir.join("build/hex-records.hex"))
            .expect("read HEX-record Rust HEX oracle"),
    }
}
