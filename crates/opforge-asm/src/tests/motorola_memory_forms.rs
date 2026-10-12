// SPDX-License-Identifier: GPL-3.0-or-later
//! Legal baseline Motorola effective addresses and section pointer relocation.
use super::*;

#[test]
fn motorola_memory_numeric_absolute_destination_is_legal() {
    for cpu in ["68000", "68020"] {
        let source = format!(
            concat!(
                ".module main\n",
                ".cpu {cpu}\n",
                ".section entry,kind=code\n",
                "start .block\n",
                " move.l (a0),$1000\n",
                " cmpa.l #$1000,a0\n",
                " rts\n",
                ".bend\n",
                ".endsection\n",
                ".output \"forms.hunk\",format=hunk,sections=entry\n",
                ".endmodule\n"
            ),
            cpu = cpu
        );
        let assembler = run_passes(&source.lines().collect::<Vec<_>>());
        assert_eq!(
            assembler.sections()["entry"].bytes,
            [0x21, 0xd0, 0x10, 0, 0xb1, 0xfc, 0, 0, 0x10, 0, 0x4e, 0x75],
            "{cpu}"
        );
    }
}
#[test]
fn motorola_memory_symbol_operands_retain_pointer_relocations() {
    for cpu in ["68000", "68020"] {
        let source = format!(
            concat!(
                ".module main\n",
                ".cpu {cpu}\n",
                ".section entry,kind=code\n",
                " move.l (a0),Cell\n",
                " cmpa.l #Cell,a0\n",
                " move.l d0,Cell\n",
                " rts\n",
                ".endsection\n",
                ".section bss,kind=bss\n",
                "Cell .res long,1\n",
                ".endsection\n",
                ".output \"forms.hunk\",format=hunk,sections=entry,bss\n",
                ".endmodule\n"
            ),
            cpu = cpu
        );
        let assembler = run_passes(&source.lines().collect::<Vec<_>>());
        let entry = &assembler.sections()["entry"];
        assert_eq!(
            entry.bytes,
            [0x23, 0xd0, 0, 0, 0, 0, 0xb1, 0xfc, 0, 0, 0, 0, 0x23, 0xc0, 0, 0, 0, 0, 0x4e, 0x75,],
            "{cpu}"
        );
        assert_eq!(entry.output_fixups.len(), 3);
        for (fixup, offset) in entry.output_fixups.iter().zip([2, 8, 14]) {
            assert_eq!(fixup.offset, offset);
            assert_eq!(fixup.target_section_name(), Some("bss"));
        }
    }
}

#[test]
fn motorola_memory_absolute_destination_update_and_width_controls() {
    for cpu in ["68000", "68020"] {
        let source = format!(
            concat!(
                ".cpu {cpu}\n",
                " move.w (a1),$7fff\n",
                " move.b (a2)+,$8000\n",
                " move.l -(a3),$123456\n",
                " move.l (a0),($1000).l\n",
                " move.l (a0),(-1).w\n"
            ),
            cpu = cpu
        );
        let assembler = run_passes(&source.lines().collect::<Vec<_>>());
        let mut entries = assembler.image().entries().unwrap();
        entries.sort_by_key(|(address, _)| *address);
        assert_eq!(
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>(),
            [
                0x31, 0xd1, 0x7f, 0xff, 0x13, 0xda, 0, 0, 0x80, 0, 0x23, 0xe3, 0, 0x12, 0x34, 0x56,
                0x23, 0xd0, 0, 0, 0x10, 0, 0x21, 0xd0, 0xff, 0xff,
            ],
            "{cpu}"
        );
    }
}
#[test]
fn motorola_memory_absolute_word_refuses_out_of_range_explicit_width() {
    for cpu in ["68000", "68020"] {
        let mut assembler = Assembler::new();
        let result = assembler.pass1(&[format!(".cpu {cpu}"), " move.l (a0),($8000).w".into()]);
        assert_eq!(result.errors, 1, "{cpu}");
        assert!(assembler.image().entries().unwrap().is_empty(), "{cpu}");
    }
}
#[test]
fn motorola_memory_absolute_long_obeys_cpu_address_width() {
    let mut assembler = Assembler::new();
    let result = assembler.pass1(&[".cpu 68000".into(), " move.l (a0),$1000000".into()]);
    assert_eq!(result.errors, 1);
    let assembler = run_passes(&[".cpu 68020", " move.l (a0),$1000000"]);
    let mut entries = assembler.image().entries().unwrap();
    entries.sort_by_key(|(address, _)| *address);
    assert_eq!(
        entries
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>(),
        [0x23, 0xd0, 1, 0, 0, 0]
    );
}
#[test]
fn motorola_address_alu_long_pointer_immediates_share_relocation_policy() {
    for cpu in ["68000", "68020"] {
        let source = format!(
            concat!(
                ".module main\n",
                ".cpu {cpu}\n",
                ".section entry,kind=code\n",
                " adda.l #Cell,a0\n",
                " suba.l #Cell,a1\n",
                " cmpa.l #Cell,a2\n",
                ".endsection\n",
                ".section bss,kind=bss\n",
                "Cell .res long,1\n",
                ".endsection\n",
                ".output \"forms.hunk\",format=hunk,sections=entry,bss\n",
                ".endmodule\n"
            ),
            cpu = cpu
        );
        let assembler = run_passes(&source.lines().collect::<Vec<_>>());
        let entry = &assembler.sections()["entry"];
        assert_eq!(
            entry.bytes,
            [0xd1, 0xfc, 0, 0, 0, 0, 0x93, 0xfc, 0, 0, 0, 0, 0xb5, 0xfc, 0, 0, 0, 0]
        );
        assert_eq!(entry.output_fixups.len(), 3);
        for (fixup, offset) in entry.output_fixups.iter().zip([2, 8, 14]) {
            assert_eq!(fixup.offset, offset);
            assert_eq!(fixup.target_section_name(), Some("bss"));
        }
    }
}
