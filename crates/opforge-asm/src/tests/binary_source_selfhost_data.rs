//! Isolated DATA probes for the self-host source's quoted bytes and TKVM demo.
use super::*;

const ALPHABET: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";
const DEMO: &str =
    include_str!("../../../../native/motorola68000/amigaos/tkvm/tkvm_demo_program.asm");

fn quoted_flat_source() -> String {
    format!(".cpu m68020\n.org 0\n.byte \"{ALPHABET}\"\n.end\n")
}

fn data_hunk_source(import: &str, data: &str, dependency: &str) -> String {
    format!(
        ".module selfhost_data_probe\n.cpu m68020\n{import}.section code,kind=code\n nop\n rts\n.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n{data}.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n{dependency}"
    )
}

fn quoted_hunk_source() -> String {
    data_hunk_source(
        "",
        &format!(".section data,kind=data\n.byte \"{ALPHABET}\"\n.endsection\n"),
        "",
    )
}

fn demo_hunk_source() -> String {
    data_hunk_source(".use tkvm.amigaos.demo_program\n", "", DEMO)
}

fn root_demo_hunk_source() -> String {
    DEMO.replace(
        "\t.endmodule",
        ".section code,kind=code\n nop\n rts\n.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule",
    )
}

fn macro_data_hunk_source(inside_data: bool) -> String {
    let definition = "emit .macro value\n.byte .value\n.endmacro\n";
    let (outside, inside) = if inside_data {
        ("", definition)
    } else {
        (definition, "")
    };
    format!(
        ".module macro_data_probe\n.cpu m68020\n{outside}.section code,kind=code\n nop\n rts\n.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n.section data,kind=data\n{inside}.emit $5a\n.byte $a5\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n"
    )
}

fn imported_macro_dce_source(labeled_call: bool, referenced: bool) -> String {
    // DATA avoids the implicit output root at the first CODE Hunk byte.
    let reference = if referenced {
        ".long dep.routine\n"
    } else {
        ""
    };
    let routine = if labeled_call {
        "routine .OUTER $22\n"
    } else {
        "routine .block\n.OUTER $22\n.bend\n"
    };
    format!(
        ".module macro_dce_probe\n.cpu m68020\n.use macro_dep (routine) as dep\n.section code,kind=code\n.byte $11\n{reference}.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n.section data,kind=data\n.byte $a5\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n.module macro_dep\n.cpu m68020\n.pub\nINNER .macro value\n.byte .value\n.endmacro\nOUTER .macro value\n.INNER .value\n.byte $33\n.endmacro\n.section data,kind=data\n{routine}.byte $44\n.endsection\n.endmodule\n"
    )
}

fn assert_imported_macro_dce(labeled_call: bool, referenced: bool) {
    let oracle =
        hunk_sections::rust_hunk_source(&imported_macro_dce_source(labeled_call, referenced));
    let sections = hunk::segments(&oracle).expect("valid macro reachability Hunk");
    let expected: &[u8] = if referenced {
        &[0x11, 0, 0, 0, 0, 0, 0, 0]
    } else {
        &[0x11, 0, 0, 0]
    };
    assert_eq!(
        sections[0].payload, expected,
        "labeled invocation: {labeled_call}, referenced: {referenced}"
    );
    assert_eq!(
        sections[0].relocations,
        if referenced { vec![(1, 2)] } else { vec![] }
    );
    assert_eq!(
        sections[2].payload,
        if referenced {
            [0x22, 0x33, 0x44, 0xa5]
        } else {
            [0x44, 0xa5, 0, 0]
        }
    );
}

#[test]
fn compact_selfhost_data_quoted_flat_rust_oracle() {
    let source = quoted_flat_source();
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live quoted DATA oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(ALPHABET.len(), 63);
    assert_eq!(
        entries
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>(),
        ALPHABET.as_bytes()
    );
}

#[test]
fn compact_selfhost_data_quoted_hunk_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&quoted_hunk_source());
    let sections = hunk::segments(&oracle).expect("valid quoted DATA Hunk");
    assert_eq!(sections[0].payload, [0x4e, 0x71, 0x4e, 0x75]);
    assert_eq!(sections[2].kind, 0x3ea);
    let mut expected = ALPHABET.as_bytes().to_vec();
    expected.push(0);
    assert_eq!(sections[2].payload, expected);
    assert!(sections[2].relocations.is_empty());
}

#[test]
fn compact_selfhost_data_demo_hunk_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&demo_hunk_source());
    let sections = hunk::segments(&oracle).expect("valid imported TKVM DATA Hunk");
    assert_eq!(sections[0].payload, [0x4e, 0x71, 0x4e, 0x75]);
    assert_eq!(sections[2].kind, 0x3ea);
    let data = sections[2].payload;
    // ABI marker (21), state entry (4), bytecode (200), lexemes (50), length (4).
    assert_eq!(data.len(), 280, "279 DATA bytes plus Hunk word padding");
    assert_eq!(&data[..21], b"OPFORGE-TOKVM-ABI-V1\0");
    assert_eq!(&data[21..25], &[0; 4]);
    assert_eq!(&data[161..224], ALPHABET.as_bytes());
    assert_eq!(data[224], 0, "DemoProgram ends at byte 225");
    let lexemes = [
        ".", "$", "#", "?", "@", "[", "]", "{", "}", ",", ":", "(", ")", "+", "-", "*", "**", "/",
        "~", "==", "!=", "!", "&", "|", "&&", "||", "^", "^^", "<", "<=", ">", ">=", "<<", ">>",
        "%", "..", "..=",
    ]
    .concat();
    assert_eq!(&data[225..275], lexemes.as_bytes());
    assert_eq!(&data[275..279], &200_u32.to_be_bytes());
    assert_eq!(data[279], 0);
    assert!(sections[2].relocations.is_empty());
}

#[test]
fn compact_selfhost_data_root_demo_hunk_rust_oracle() {
    assert_eq!(
        hunk_sections::rust_hunk_source(&root_demo_hunk_source()),
        hunk_sections::rust_hunk_source(&demo_hunk_source()),
        "root-local and imported demo must emit the same complete Hunk"
    );
}

#[test]
fn compact_selfhost_data_macro_sections_rust_oracle() {
    for inside_data in [false, true] {
        let oracle = hunk_sections::rust_hunk_source(&macro_data_hunk_source(inside_data));
        let sections = hunk::segments(&oracle).expect("valid macro DATA Hunk");
        assert_eq!(sections[0].payload, [0x4e, 0x71, 0x4e, 0x75]);
        assert_eq!(sections[2].kind, 0x3ea);
        assert_eq!(
            sections[2].payload,
            [0x5a, 0xa5, 0, 0],
            "macro defined inside DATA: {inside_data}"
        );
        assert!(sections[2].relocations.is_empty());
    }
}

#[test]
fn compact_selfhost_data_unused_block_macro_rust_oracle() {
    assert_imported_macro_dce(false, false);
}

#[test]
fn compact_selfhost_data_referenced_block_macro_rust_oracle() {
    assert_imported_macro_dce(false, true);
}

#[test]
fn compact_selfhost_data_labeled_macro_dce_rust_oracle() {
    for referenced in [false, true] {
        assert_imported_macro_dce(true, referenced);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; exact 63-byte quoted alphabet"]
fn compact_selfhost_data_quoted_flat_fs_uae() {
    assert_binary_source(quoted_flat_source(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; quoted alphabet in Hunk DATA"]
fn compact_selfhost_data_quoted_hunk_fs_uae() {
    hunk_sections::native_hunk_source(&quoted_hunk_source());
}

#[test]
#[ignore = "requires configured FS-UAE; complete imported TKVM demo DATA"]
fn compact_selfhost_data_demo_hunk_fs_uae() {
    hunk_sections::native_hunk_source(&demo_hunk_source());
}

#[test]
#[ignore = "requires configured FS-UAE; root demo and macro DATA placement controls"]
fn compact_selfhost_data_root_and_macro_sections_fs_uae() {
    let selected = std::env::var("OPFORGE_SELFHOST_DATA_CASE").ok();
    let cases = [
        ("root_demo", root_demo_hunk_source()),
        ("macro_outside", macro_data_hunk_source(false)),
        ("macro_inside", macro_data_hunk_source(true)),
    ];
    assert!(
        selected
            .as_deref()
            .is_none_or(|selected| cases.iter().any(|(name, _)| *name == selected)),
        "unknown self-host DATA case: {selected:?}"
    );
    for (name, source) in cases {
        if selected.as_deref().is_none_or(|selected| name == selected) {
            eprintln!("SELFHOST_DATA_CASE={name}");
            hunk_sections::native_hunk_source(&source);
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; unused imported block with nested anonymous macro calls"]
fn compact_selfhost_data_unused_block_macro_fs_uae() {
    hunk_sections::native_hunk_source(&imported_macro_dce_source(false, false));
}

#[test]
#[ignore = "requires configured FS-UAE; referenced imported block retains nested macro bytes"]
fn compact_selfhost_data_referenced_block_macro_fs_uae() {
    hunk_sections::native_hunk_source(&imported_macro_dce_source(false, true));
}

#[test]
#[ignore = "requires configured FS-UAE; explicitly labeled macro call reachability"]
fn compact_selfhost_data_labeled_macro_dce_fs_uae() {
    for referenced in [false, true] {
        hunk_sections::native_hunk_source(&imported_macro_dce_source(true, referenced));
    }
}
