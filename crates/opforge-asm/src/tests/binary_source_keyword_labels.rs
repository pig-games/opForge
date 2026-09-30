//! Bare core-directive spellings must bind as scoped source symbols.
use super::*;

fn branch_source() -> String {
    let mut source = String::from(".cpu m68020\n.org 0\n");
    for (index, label) in ["item", "word", "byte", "long", "end", "word"]
        .iter()
        .enumerate()
    {
        source.push_str(&format!(
            "scope{index} .block\n{label}\n nop\n bra.w {label}\n.bend\n"
        ));
    }
    source.push_str(" .word $1234\n move.w #8,d0\n.end\n");
    source
}

const DATA_SOURCE: &str = r#".cpu m6502
.org 0
first .block
word=7
long=9
 .byte word,long
 .word word
.bend
second .block
word=11
long=13
 .byte word,long
 .word long
.bend
 .byte 17
.end
"#;

const RESERVATION_SOURCE: &str = r#".module keyword_reservation
.cpu m68020
.section code,kind=code
 rts
.endsection
.section data,kind=data
 .byte 17
.endsection
.section bss,kind=bss
storage .block
word=2
 .res word,word
 .res long,word
.bend
.endsection
.output "build/sections.hunk",format=hunk,sections=code,bss,data
.endmodule
"#;

const STRUCT_EXTENT_SOURCE: &str = r#".cpu m68020
.org 0
scope .block
MAX_RECORDS=3
RECORD_BYTES=4
word=2
Frame .struct
Stage .res MAX_RECORDS * RECORD_BYTES
Tail .res word
.endstruct
 .word Frame.Stage,Frame.Tail,Frame
.bend
.end
"#;

const MODULE_STRUCT_EXTENT_SOURCE: &str = r#".module prvm.amigaos.macro_descriptors
.cpu m68020
MAX_RECORDS=64
RECORD_BYTES=32
State .struct
Record .res 32
Stage .res MAX_RECORDS * RECORD_BYTES
.endstruct
 .long State.Stage,State
.endmodule
"#;

fn oracle(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust keyword-label oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn compact_keyword_labels_rust_oracle() {
    let mut expected = [0x4e, 0x71, 0x60, 0, 0xff, 0xfc].repeat(6);
    expected.extend([0x12, 0x34, 0x30, 0x3c, 0, 8]);
    assert_eq!(oracle(&branch_source()), expected);
}

#[test]
fn compact_keyword_data_rust_oracle() {
    assert_eq!(oracle(DATA_SOURCE), [7, 9, 7, 0, 11, 13, 13, 0, 17]);
    let hunk = hunk_sections::rust_hunk_source(RESERVATION_SOURCE);
    assert_eq!(hunk::allocation(&hunk).unwrap().bss, 12);
    assert_eq!(oracle(STRUCT_EXTENT_SOURCE), [0, 0, 0, 12, 0, 14]);
    assert_eq!(
        oracle(MODULE_STRUCT_EXTENT_SOURCE),
        [0, 0, 0, 32, 0, 0, 8, 32]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; scoped core-keyword branch labels"]
fn compact_keyword_labels_fs_uae() {
    assert_binary_source(branch_source(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; generic keyword data expressions"]
fn compact_keyword_data_fs_uae() {
    assert_binary_source(DATA_SOURCE.into(), "m6502".into());
    hunk_sections::native_hunk_source(RESERVATION_SOURCE);
    assert_binary_source(MODULE_STRUCT_EXTENT_SOURCE.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; block-local constant capture for struct extents"]
fn compact_keyword_scoped_struct_fs_uae() {
    assert_binary_source(STRUCT_EXTENT_SOURCE.into(), "m68020".into());
}

// A measured convergence gap, not an unsupported source-language form.
// Keep the positive Rust/native probe above when removing this readiness check.
#[test]
#[ignore = "requires configured FS-UAE; current block-local constant capture gap"]
fn compact_keyword_scoped_struct_known_gap_fs_uae() {
    assert_native_files_rejection(
        &[("input.asm", STRUCT_EXTENT_SOURCE)],
        "m68020",
        Some("[file 00000001, line 00000008]"),
    );
}
