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

const UNUSED_PARAMETERS: &[(&str, &str)] = &[
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
fn binary_graph_unused_import_parameters_preserve_output() {
    assert_eq!(
        oracle_with_roots(UNUSED_PARAMETERS, &["library"]).unwrap(),
        oracle_with_roots(WILDCARD_IMPORT, &["library"]).unwrap()
    );
    let quoted = UNUSED_PARAMETERS[0].1.replace("MODE=3", "MODE=\"fast\"");
    assert!(
        oracle_with_roots(&[("main.asm", &quoted), UNUSED_PARAMETERS[1]], &["library"]).is_err()
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
    let empty = UNUSED_PARAMETERS[0]
        .1
        .replace("with (FEATURE=1, OFFSET=(2+3), MODE=3)", "with ()");
    assert!(
        oracle_with_roots(&[("main.asm", &empty), UNUSED_PARAMETERS[1]], &["library"]).is_err()
    );
}

#[test]
#[ignore = "requires configured FS-UAE; unused configured values preserve output"]
fn compact_cli_unused_import_parameters_fs_uae() {
    let expected = oracle_with_roots(UNUSED_PARAMETERS, &["library"]).unwrap();
    compact_cli(UNUSED_PARAMETERS, &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; empty parameter list is invalid"]
fn compact_cli_rejects_empty_import_parameters_fs_uae() {
    let empty = UNUSED_PARAMETERS[0]
        .1
        .replace("with (FEATURE=1, OFFSET=(2+3), MODE=3)", "with ()");
    compact_cli(
        &[("main.asm", &empty), UNUSED_PARAMETERS[1]],
        &["library"],
        &[],
        None,
        false,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; literal parameter with mapped alias"]
fn compact_cli_literal_parameter_with_map_fs_uae() {
    let mapped = EXPLICIT_MAPPED_SECTION[0]
        .1
        .replace(" as d map", " as d with (FEATURE=1) map");
    let files = &[("main.asm", mapped.as_str()), EXPLICIT_MAPPED_SECTION[1]];
    let expected = oracle_with_roots(files, &["library"]).unwrap();
    compact_cli(files, &["library"], &[], Some(&expected), false);
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
