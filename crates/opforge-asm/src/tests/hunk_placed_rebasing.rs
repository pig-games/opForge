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
