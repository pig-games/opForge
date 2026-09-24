//! Parent-relative include resolution in selected module files.
use super::*;

const PARENT_INCLUDE: &[(&str, &str)] = &[
    (
        "project/main.asm",
        ".module main\n.cpu m6502\n.use dep as d\n.org $1001\n.byte d.value\n.endmodule\n.end\n",
    ),
    (
        "project/library/dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\n.include \"./../common/part.inc\"\n.endmodule\n.end\n",
    ),
    ("project/common/part.inc", "value = 7\n.byte value\n"),
];
const NORMALIZED_INCLUDE_CYCLE: &[(&str, &str)] = &[
    PARENT_INCLUDE[0],
    (
        "project/library/dep.asm",
        ".module dep\n.cpu m6502\n.include \"parts/loop.inc\"\n.endmodule\n.end\n",
    ),
    (
        "project/library/parts/loop.inc",
        ".include \"../parts/loop.inc\"\n",
    ),
];

#[test]
fn binary_discovery_parent_include_rust_oracle() {
    assert_eq!(
        oracle_with_search_roots(PARENT_INCLUDE, &["project/library"], &["project"]).unwrap(),
        [7, 7]
    );
}

#[test]
fn binary_discovery_parent_include_requires_allowed_root_rust_oracle() {
    assert!(oracle_with_roots(PARENT_INCLUDE, &["project/library"])
        .unwrap_err()
        .contains("INCLUDE file not found"));
}

#[test]
#[ignore = "requires configured FS-UAE; normalized parent-relative include"]
fn compact_cli_parent_include_fs_uae() {
    let expected =
        oracle_with_search_roots(PARENT_INCLUDE, &["project/library"], &["project"]).unwrap();
    compact_cli(
        PARENT_INCLUDE,
        &["project/library"],
        &["project"],
        Some(&expected),
        false,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; parent include requires an allowed root"]
fn compact_cli_parent_include_without_root_fs_uae() {
    compact_cli(PARENT_INCLUDE, &["project/library"], &[], None, false);
}

#[test]
fn binary_discovery_parent_include_cycle_rust_oracle() {
    let error = oracle_with_search_roots(
        NORMALIZED_INCLUDE_CYCLE,
        &["project/library"],
        &["project/library"],
    )
    .unwrap_err();
    assert!(
        error.contains("cycle") || error.contains("recursive"),
        "{error}"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; normalized include cycle rejection"]
fn compact_cli_parent_include_cycle_fs_uae() {
    compact_cli(
        NORMALIZED_INCLUDE_CYCLE,
        &["project/library"],
        &["project/library"],
        None,
        false,
    );
}
