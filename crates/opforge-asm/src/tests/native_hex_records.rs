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
fn native_hex_records_writer_presence_cap_and_bounds_contract() {
    // Level B: bind the native writer to presence-driven record boundaries,
    // the 32-byte cap, pre-write capacity reservation, and balanced A4 saves.
    let native = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_output_artifacts.asm"),
    )
    .expect("read native output-artifact owner");
    let writer = native
        .split("opasmOutputBuildHexArtifactV1\t.block")
        .nth(1)
        .and_then(|tail| tail.split(".bend  ; opasmOutputBuildHexArtifactV1").next())
        .expect("native HEX writer body");
    assert!(source_contains_in_order(
        writer,
        &[
            "movem.l d2-d7/a2-a4, -(sp)",
            "jsr engine.opasmEngineGetImagePresentBufferPtrV1",
            "movea.l a0, a4",
            "recordLoop",
            "tst.b (a4)",
            "addq.l #1, a4",
            "measureRecord",
            "cmpi.l #32, d7",
            "tst.b 0(a4, d7.l)",
            "haveRecordLen",
            "move.l a2, d0",
            "lea OpasmHexArtifactBuffer.l, a0",
            "move.l a0, d2",
            "sub.l d2, d0",
            "add.l d7, d0",
            "add.l d7, d0",
            "addi.l #24, d0",
            "cmpi.l #OPASM_OUTPUT_HEX_BUFFER_CAPACITY, d0",
            "bhi.w fail",
            "move.b #':', (a2)+",
        ]
    ));
    assert_eq!(writer.matches("movem.l (sp)+, d2-d7/a2-a4").count(), 2);
    assert!(source_contains_in_order(
        writer,
        &["fail", "moveq #1, d0", "movem.l (sp)+, d2-d7/a2-a4", "rts"]
    ));
}

#[test]
fn native_hex_records_gap_and_thirty_three_byte_rust_oracle() {
    // Level A: the live Rust CLI is authoritative for both sparse Intel HEX
    // segmentation and the flat BIN representation of the same source.
    let oracle = build_hex_record_oracle();
    assert_eq!(oracle.bin, EXPECTED_BIN);
    assert_eq!(oracle.hex, EXPECTED_HEX);
}

#[test]
fn native_hex_records_gap_and_thirty_three_byte_fs_uae() {
    // Level D: one fresh guest must reproduce both exact live Rust artifacts.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let root = workspace_root();
    let oracle = build_hex_record_oracle();
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let expected = [
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/hex-records.bin",
            rust_oracle: &oracle.bin,
        },
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/hex-records.hex",
            rust_oracle: &oracle.hex,
        },
    ];
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "hex-records-gap-and-33",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(HEX_RECORD_SOURCE),
        command_template: Some(
            "{input} --bin {guest_work_dir}build/hex-records.bin --cpu m6502 --opasm-package {package}",
        ),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifacts(&expected),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &[case])
        .expect("HEX record FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one HEX record run");
            let run = &runs[0];
            assert!(run.protocol_completed);
            assert!(run.success);
            assert_eq!(run.exit_code, Some(0));
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/hex-records.bin"),
                oracle.bin
            );
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/hex-records.hex"),
                oracle.hex
            );
        }
    }
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
