// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

#[test]
fn placed_section_symbols_rebase_once_during_68020_family_stabilization() {
    for cpu in ["68020", "68030", "68040"] {
        let lines = vec![
            ".module main".to_string(),
            format!(".cpu {cpu}"),
            ".region codeRam, $2000, $20ff".to_string(),
            ".region dataRam, $3000, $30ff".to_string(),
            ".region bssRam, $4000, $40ff".to_string(),
            ".section code, kind=code".to_string(),
            "code_label: rts".to_string(),
            "    .long data_label, bss_label, code_label".to_string(),
            ".endsection".to_string(),
            ".section data, kind=data".to_string(),
            "data_label: .long code_label, bss_label".to_string(),
            ".endsection".to_string(),
            ".section bss, kind=bss".to_string(),
            "    .res byte, 8".to_string(),
            "bss_label: .res byte, 4".to_string(),
            ".endsection".to_string(),
            ".place code in codeRam".to_string(),
            ".place data in dataRam".to_string(),
            ".place bss in bssRam".to_string(),
            ".output \"build/placed.hunk\", format=hunk, sections=code,data,bss".to_string(),
            ".endmodule".to_string(),
        ];

        let mut assembler = Assembler::new();
        let pass1 = assembler.pass1(&lines);
        assert_eq!(pass1.errors, 0, "{cpu}: {:?}", assembler.diagnostics);
        for (name, expected) in [
            ("main.code_label", 0x2000),
            ("main.data_label", 0x3000),
            ("main.bss_label", 0x4008),
        ] {
            assert_eq!(
                assembler.symbols.entry(name).map(|entry| entry.val),
                Some(expected),
                "{cpu} {name} after layout stabilization"
            );
        }

        let mut listing_out = Vec::new();
        let mut listing = ListingWriter::new(&mut listing_out, false);
        let pass2 = assembler.pass2(&lines, &mut listing).expect("pass2");
        assert_eq!(pass2.errors, 0, "{cpu}: {:?}", assembler.diagnostics);
        let code = &assembler.sections()["code"];
        let data = &assembler.sections()["data"];
        assert_eq!(
            &code.bytes[2..14],
            &[0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0],
            "{cpu}"
        );
        assert_eq!(&data.bytes[..8], &[0, 0, 0, 0, 0, 0, 0, 8], "{cpu}");
        assert_eq!(code.output_fixups.len(), 3, "{cpu}");
        assert_eq!(data.output_fixups.len(), 2, "{cpu}");
        build_linker_output_payload(
            &assembler.root_metadata.linker_outputs[0],
            assembler.sections(),
        )
        .expect("placed Hunk output");
    }
}

// Forward constants can grow an ordinary section after its initial placement.
// Sizing retries must place subsequent sections from the newly measured extent.
#[test]
fn section_placement_retries_forward_fill_extent() {
    let assembler = run_passes(&[
        ".module main",
        ".cpu 68020",
        ".region ram,0,$ffff",
        ".section code,kind=code",
        ".fill byte,COUNT,$aa",
        ".endsection",
        ".section data,kind=data",
        ".word $1122",
        ".endsection",
        ".place code in ram",
        ".place data in ram",
        "COUNT .const 8",
        ".output \"build/grown.bin\",format=bin,sections=code,data",
        ".output \"build/grown.hunk\",format=hunk,sections=code,data",
        ".endmodule",
    ]);
    assert_eq!(assembler.sections()["code"].base_addr, Some(0));
    assert_eq!(assembler.sections()["data"].base_addr, Some(8));
    let mut expected = vec![0xaa; 8];
    expected.extend_from_slice(&[0x11, 0x22]);
    let outputs = &assembler.root_metadata.linker_outputs;
    let binary = build_linker_output_payload(&outputs[0], assembler.sections())
        .expect("adjacent binary sections");
    assert_eq!(binary, expected);
    build_linker_output_payload(&outputs[1], assembler.sections())
        .expect("Hunk sections after sizing retry");
}

// A library can place shared concrete sections before its importer contributes
// more bytes. Reserve the complete stabilized extent, not just the prefix at .place.
#[test]
fn section_placement_reserves_contributions_after_place() {
    let assembler = run_passes(&[
        ".module provider",
        ".cpu 68020",
        ".pub",
        ".region ram,0,$ffff",
        ".section code,kind=code",
        "entry .block",
        ".byte $a5,$5a",
        ".bend",
        ".endsection",
        ".section data,kind=data",
        "payload .byte $33",
        ".endsection",
        ".place code in ram",
        ".place data in ram",
        ".endmodule",
        ".module main",
        ".cpu 68020",
        ".use provider",
        ".section code,kind=code",
        ".long provider.entry,provider.payload",
        ".endsection",
        ".section data,kind=data",
        ".byte $55",
        ".endsection",
        ".output \"build/late.bin\",format=bin,sections=code,data",
        ".output \"build/late.hunk\",format=hunk,sections=code,data",
        ".endmodule",
    ]);
    assert_eq!(assembler.sections()["code"].base_addr, Some(0));
    assert_eq!(assembler.sections()["data"].base_addr, Some(10));
    assert_eq!(
        assembler
            .symbols
            .entry("provider.payload")
            .map(|entry| entry.val),
        Some(10)
    );
    let code = &assembler.sections()["code"];
    assert!(code.output_fixups.iter().any(|fixup| fixup.offset == 6
        && fixup.target.section_name() == Some("data")
        && fixup.encoded_addend == 0));
    let outputs = &assembler.root_metadata.linker_outputs;
    assert_eq!(
        build_linker_output_payload(&outputs[0], assembler.sections()).unwrap(),
        [0xa5, 0x5a, 0, 0, 0, 0, 0, 0, 0, 0, 0x33, 0x55]
    );
    build_linker_output_payload(&outputs[1], assembler.sections()).expect("placed Hunk");
}

#[test]
fn section_placement_rechecks_late_contributions_against_region_bound() {
    for cpu in ["68020", "6502"] {
        let lines = [
            &format!(".cpu {cpu}"),
            ".region ram,0,5",
            ".section code,kind=code",
            ".word 0",
            ".endsection",
            ".section data,kind=data",
            ".byte 1",
            ".endsection",
            ".place code in ram",
            ".place data in ram",
            ".section code,kind=code",
            ".long 0",
            ".endsection",
            ".section data,kind=data",
            ".byte 2",
            ".endsection",
        ]
        .map(str::to_string);
        let mut assembler = Assembler::new();
        let pass = assembler.pass1(&lines);
        assert!(pass.errors > 0);
        assert!(
            assembler.diagnostics.iter().any(|diag| diag
                .error
                .message()
                .contains("Section placement overflows region")),
            "{:?}",
            assembler.diagnostics
        );
    }
}
#[test]
fn section_placement_hints_follow_shrinking_completed_extents() {
    let assembler = run_passes(&[
        ".cpu 68020",
        ".region ram,0,$ffff",
        ".section code,kind=code",
        ".if FINAL",
        ".fill byte,4,$aa",
        ".else",
        ".fill byte,8,$aa",
        ".endif",
        ".endsection",
        ".section data,kind=data",
        ".byte $55",
        ".endsection",
        ".place code in ram",
        ".place data in ram",
        "FINAL .const 1",
    ]);
    assert_eq!(assembler.sections()["code"].max_pc, 4);
    assert_eq!(assembler.sections()["data"].base_addr, Some(4));
}

#[test]
fn section_placement_retries_late_data_contributions_without_cpu_relaxation() {
    let assembler = run_passes(&[
        ".cpu 6502",
        ".region ram,0,$ffff",
        ".section code,kind=code",
        ".byte 1",
        ".endsection",
        ".section data,kind=data",
        ".byte 2",
        ".endsection",
        ".place code in ram",
        ".place data in ram",
        ".section code,kind=code",
        ".byte 3,4",
        ".endsection",
        ".output \"build/late-6502.bin\",format=bin,sections=code,data",
    ]);
    assert_eq!(assembler.sections()["code"].max_pc, 3);
    assert_eq!(assembler.sections()["data"].base_addr, Some(3));
    let bytes = build_linker_output_payload(
        &assembler.root_metadata.linker_outputs[0],
        assembler.sections(),
    )
    .unwrap();
    assert_eq!(bytes, [1, 3, 4, 2]);
}
