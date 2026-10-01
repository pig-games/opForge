//! A macro name may also name a structural declaration or ordinary label.
use super::*;

fn hunk_source(code: &str) -> String {
    format!(
        ".module macro_name_probe\n.cpu m68020\n.section code,kind=code\n{code}.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n.section data,kind=data\n.byte $aa\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n"
    )
}

fn evaluator_source() -> String {
    hunk_source(
        ".priv\nEVALUATE .macro saved\n movem.l .saved,-(sp)\n moveq #7,d0\n movem.l (sp)+,.saved\n tst.l d0\n rts\n.endmacro\n.pub\nevaluate .block\n .EVALUATE d3-d7/a1-a6\n.bend\nevaluateWithSymbols .block\n .eVaLuAtE d3/d5-d7/a1-a6\n.bend\n",
    )
}

fn ordinary_label_source() -> String {
    hunk_source(
        ".priv\nENTRY .macro value\n.byte .value,$5a\n.endmacro\n.pub\nentry nop\n .ENTRY $a5\n rts\n",
    )
}

fn renamed_evaluator_source() -> String {
    evaluator_source()
        .replace("EVALUATE .macro", "EVALUATE_BODY .macro")
        .replace(" .EVALUATE ", " .EVALUATE_BODY ")
        .replace(" .eVaLuAtE ", " .EVALUATE_BODY ")
}

#[test]
fn compact_macro_name_block_collision_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&evaluator_source());
    let sections = hunk::segments(&oracle).expect("valid evaluator macro Hunk");
    assert_eq!(
        sections[0].payload,
        [
            0x48, 0xe7, 0x1f, 0x7e, 0x70, 7, 0x4c, 0xdf, 0x7e, 0xf8, 0x4a, 0x80, 0x4e, 0x75, 0x48,
            0xe7, 0x17, 0x7e, 0x70, 7, 0x4c, 0xdf, 0x7e, 0xe8, 0x4a, 0x80, 0x4e, 0x75,
        ]
    );
    assert!(sections[0].relocations.is_empty());
}

#[test]
fn compact_macro_name_ordinary_label_collision_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&ordinary_label_source());
    let sections = hunk::segments(&oracle).expect("valid ordinary label macro Hunk");
    assert_eq!(
        sections[0].payload,
        [0x4e, 0x71, 0xa5, 0x5a, 0x4e, 0x75, 0, 0]
    );
    assert!(sections[0].relocations.is_empty());
}

#[test]
fn compact_macro_name_renamed_evaluator_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&renamed_evaluator_source());
    let sections = hunk::segments(&oracle).expect("valid renamed evaluator macro Hunk");
    assert_eq!(sections[0].payload.len(), 28);
    assert_eq!(oracle, hunk_sections::rust_hunk_source(&evaluator_source()));
}

#[test]
#[ignore = "requires configured FS-UAE; public block name also names a private macro"]
fn compact_macro_name_block_collision_fs_uae() {
    hunk_sections::native_hunk_source(&evaluator_source());
}

#[test]
#[ignore = "requires configured FS-UAE; ordinary instruction label also names a private macro"]
fn compact_macro_name_ordinary_label_collision_fs_uae() {
    hunk_sections::native_hunk_source(&ordinary_label_source());
}

#[test]
#[ignore = "requires configured FS-UAE; evaluator macro renamed while routine stays evaluate"]
fn compact_macro_name_renamed_evaluator_fs_uae() {
    hunk_sections::native_hunk_source(&renamed_evaluator_source());
}
