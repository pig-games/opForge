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
