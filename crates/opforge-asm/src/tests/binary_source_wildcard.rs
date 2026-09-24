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

const INERT_PARAMETERS: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (*) with (FEATURE=1, OFFSET=(2+3), MODE=3)\n.word entry\n.endmodule\n.end\n",
    ),
    WILDCARD_IMPORT[1],
];

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
    assert_eq!(
        oracle_with_roots(INERT_PARAMETERS, &["library"]).unwrap(),
        oracle_with_roots(WILDCARD_IMPORT, &["library"]).unwrap()
    );
    let quoted = INERT_PARAMETERS[0].1.replace("MODE=3", "MODE=\"fast\"");
    assert_eq!(
        oracle_with_roots(&[("main.asm", &quoted), INERT_PARAMETERS[1]], &["library"]).unwrap(),
        [0x11, 0x00, 0x10]
    );
    let mapped = EXPLICIT_MAPPED_SECTION[0]
        .1
        .replace(" as d map", " as d with (FEATURE=1) map");
    assert_eq!(
        oracle_with_roots(
            &[("main.asm", &mapped), EXPLICIT_MAPPED_SECTION[1]],
            &["library"]
        )
        .unwrap(),
        oracle_with_roots(EXPLICIT_MAPPED_SECTION, &["library"]).unwrap()
    );
    let empty = INERT_PARAMETERS[0]
        .1
        .replace("with (FEATURE=1, OFFSET=(2+3), MODE=3)", "with ()");
    assert!(oracle_with_roots(&[("main.asm", &empty), INERT_PARAMETERS[1]], &["library"]).is_err());
}

#[test]
#[ignore = "requires configured FS-UAE; inert wildcard import parameters"]
fn compact_cli_inert_import_parameters_fs_uae() {
    let expected = oracle_with_roots(INERT_PARAMETERS, &["library"]).unwrap();
    compact_cli(INERT_PARAMETERS, &["library"], &[], Some(&expected), false);
    let mapped = EXPLICIT_MAPPED_SECTION[0]
        .1
        .replace(" as d map", " as d with (FEATURE=1) map");
    let files = &[("main.asm", mapped.as_str()), EXPLICIT_MAPPED_SECTION[1]];
    let expected = oracle_with_roots(files, &["library"]).unwrap();
    compact_cli(files, &["library"], &[], Some(&expected), false);
    let empty = INERT_PARAMETERS[0]
        .1
        .replace("with (FEATURE=1, OFFSET=(2+3), MODE=3)", "with ()");
    compact_cli(
        &[("main.asm", &empty), INERT_PARAMETERS[1]],
        &["library"],
        &[],
        None,
        false,
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
