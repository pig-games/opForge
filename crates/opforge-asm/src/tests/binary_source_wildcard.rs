//! Imported blocks remain reference-driven under wildcard availability.
use super::*;

const WILDCARD_IMPORT: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (*)\n.word entry\n.endmodule\n.end\n",
    ),
    (
        "library/dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\nentry .block\n.byte $11\n.bend\nunused .block\n.byte $99\n.bend\n.endmodule\n.end\n",
    ),
];

const WILDCARD_ENTRY_SIBLING: &[(&str, &str)] = &[(
    "main.asm",
    ".module dep\n.cpu m6502\n.org $1000\n.pub\nentry .block\n.byte $11\n.bend\nunused .block\n.byte $99\n.bend\n.endmodule\n.module main\n.cpu m6502\n.use dep (*)\n.word entry\n.endmodule\n.end\n",
)];

const WILDCARD_ENTRY_SIBLING_FORWARD: &[(&str, &str)] = &[(
    "main.asm",
    ".module main\n.cpu m6502\n.use dep (*)\n.word entry\n.endmodule\n.module dep\n.cpu m6502\n.org $1000\n.pub\nentry .block\n.byte $11\n.bend\nunused .block\n.byte $99\n.bend\n.endmodule\n.end\n",
)];

#[test]
fn binary_graph_wildcard_import_rust_oracle() {
    assert_eq!(oracle(SELECTED_UNUSED).unwrap(), [0xaa]);
    assert_eq!(
        oracle_with_roots(WILDCARD_IMPORT, &["library"]).unwrap(),
        [0x11, 0x00, 0x10]
    );
    assert_eq!(oracle(WILDCARD_ENTRY_SIBLING).unwrap(), [0x11, 0x00, 0x10]);
    assert_eq!(
        oracle(WILDCARD_ENTRY_SIBLING_FORWARD).unwrap(),
        [0x11, 0x00, 0x10]
    );
}

#[test]
fn binary_graph_import_parameters_are_inert() {
    let main = WILDCARD_IMPORT[0]
        .1
        .replace(".use dep (*)", ".use dep (*) with (FEATURE=1)");
    assert_eq!(
        oracle_with_roots(&[("main.asm", &main), WILDCARD_IMPORT[1]], &["library"]).unwrap(),
        oracle_with_roots(WILDCARD_IMPORT, &["library"]).unwrap()
    );
}

#[test]
#[ignore = "requires configured FS-UAE; direct wildcard import"]
fn compact_cli_wildcard_import_fs_uae() {
    let expected = oracle_with_roots(WILDCARD_IMPORT, &["library"]).unwrap();
    compact_cli(WILDCARD_IMPORT, &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; imported entry-file blocks remain reference-driven"]
fn compact_cli_wildcard_entry_sibling_fs_uae() {
    let expected = oracle(WILDCARD_ENTRY_SIBLING).unwrap();
    compact_cli(WILDCARD_ENTRY_SIBLING, &[], &[], Some(&expected), false);
    let forward = oracle(WILDCARD_ENTRY_SIBLING_FORWARD).unwrap();
    compact_cli(
        WILDCARD_ENTRY_SIBLING_FORWARD,
        &[],
        &[],
        Some(&forward),
        false,
    );
}
