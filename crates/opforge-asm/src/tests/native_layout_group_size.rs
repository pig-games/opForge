// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

const LAYOUT_SOURCE: &[u8] = b".module main\n.cpu 6502\n.region rom, $8000, $80ff, align=1\n.section code, align=1\n.byte 1,2,3,4,5\n.endsection\n.section data, align=2\n.byte 6,7,8\n.endsection\n.section zero, kind=bss, align=1\n.res byte, 4\n.endsection\n.pack in rom : code, data, zero\n.output \"build/layout.bin\", format=bin, contiguous=false, sections=code,data\n.mapfile \"build/layout.map\", symbols=none\n.endmodule\n";

#[test]
fn native_layout_group_size_accumulator_survives_table_pointer_helpers() {
    // Level B: bind the native accumulator register across helpers documented
    // to clobber D2, and bind the live Rust three-section placement result.
    let native = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_layout.asm"),
    )
    .expect("read native layout owner");
    let place_section = native
        .split("placeSectionV1\t.block")
        .nth(1)
        .and_then(|tail| tail.split(".bend  ; placeSectionV1").next())
        .expect("native place-section routine body");
    assert!(source_contains_in_order(
        place_section,
        &[
            "moveq #0, d3",
            "groupSizeLoop",
            "jsr wordTablePtrV1",
            "jsr longTablePtrV1",
            "add.l (a0), d3",
            "bcs.w fail",
            "groupSizeReady",
            "tst.l d3",
            "add.l d3, d0",
            "bcs.w fail",
        ]
    ));

    let lines = std::str::from_utf8(LAYOUT_SOURCE)
        .expect("layout source UTF-8")
        .lines()
        .collect::<Vec<_>>();
    let assembler = run_passes(&lines);
    let regions = assembler.regions();
    let rom = regions.get("rom").expect("ROM region");
    assert_eq!(rom.cursor, 0x800d);
    let sections = assembler.sections();
    assert_eq!(
        sections.get("code").and_then(|section| section.base_addr),
        Some(0x8000)
    );
    assert_eq!(
        sections.get("data").and_then(|section| section.base_addr),
        Some(0x8006)
    );
    assert_eq!(
        sections.get("zero").and_then(|section| section.base_addr),
        Some(0x8009)
    );
}

#[test]
fn native_layout_group_size_multi_member_structural_name_group() {
    // Level A: Rust's layout authority merges both concrete members with the
    // same structural section name and reserves their complete byte sum.
    let assembler = run_passes(&[
        ".module main",
        ".cpu 6502",
        ".region ram, $4000, $40ff",
        ".section code, align=1",
        ".byte 1,2",
        ".endsection",
        ".section code, align=1",
        ".byte 3,4,5",
        ".endsection",
        ".place code in ram",
        ".endmodule",
    ]);
    let code = assembler
        .sections()
        .get("code")
        .expect("merged code section");
    assert_eq!(code.base_addr, Some(0x4000));
    assert_eq!(code.bytes, [1, 2, 3, 4, 5]);
    assert_eq!(
        assembler.regions().get("ram").expect("RAM region").cursor,
        0x4005
    );
}

#[test]
fn native_layout_group_size_map_and_bin_fs_uae() {
    // Level D: one fresh guest proves three differently aligned section sizes
    // produce the exact live Rust BIN and symbol-free map artifacts.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let root = workspace_root();
    let oracle_dir = create_temp_dir("native-layout-group-size-oracle");
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _oracle_guard = OracleDir(oracle_dir.clone());
    fs::create_dir_all(oracle_dir.join("build")).expect("create layout oracle directory");
    let input_path = oracle_dir.join("input.asm");
    fs::write(&input_path, LAYOUT_SOURCE).expect("write layout oracle source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input_path.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m6502".to_string(),
    ]);
    let mut config = validate_cli(&cli).expect("validate layout Rust CLI");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("run layout live Rust CLI oracle");
    let rust_bin =
        fs::read(oracle_dir.join("build/layout.bin")).expect("read layout Rust BIN oracle");
    let rust_map =
        fs::read(oracle_dir.join("build/layout.map")).expect("read layout Rust map oracle");
    assert_eq!(rust_bin, [1, 2, 3, 4, 5, 6, 7, 8]);
    let map_text = std::str::from_utf8(&rust_map).expect("layout map UTF-8");
    assert!(map_text.contains("rom 8000 80FF 13 243 1\n"));
    assert!(map_text.contains("code 8000 5 code rom\n"));
    assert!(map_text.contains("data 8006 3 code rom\n"));
    assert!(map_text.contains("zero 8009 4 bss rom\n"));
    assert!(!map_text.contains("Symbols\n"));

    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let expected = [
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/layout.bin",
            rust_oracle: &rust_bin,
        },
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/layout.map",
            rust_oracle: &rust_map,
        },
    ];
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "layout-group-size",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(LAYOUT_SOURCE),
        command_template: Some("{input} --cpu m6502 --opasm-package {package}"),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifacts(&expected),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &[case])
        .expect("layout group-size FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one layout group-size run");
            let run = &runs[0];
            assert!(run.protocol_completed);
            assert!(run.success);
            assert_eq!(run.exit_code, Some(0));
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/layout.bin"),
                rust_bin
            );
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/layout.map"),
                rust_map
            );
        }
    }
}
