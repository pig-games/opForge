//! File-derived module identity and discovery parity.
use super::*;

const IMPLICIT_FILE_MODULE: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep as d\n.org $1001\n.byte d.value\n.endmodule\n.end\n",
    ),
    (
        "library/dep.asm",
        ".cpu m6502\n.org $1000\n.pub\nvalue = 7\n.byte value\n",
    ),
];
const IMPLICIT_ROOT: &str = ".cpu m6502\n.use dep as d\n.org $1001\n.byte d.value\n.end\n";

#[test]
fn binary_discovery_implicit_file_module_rust_oracle() {
    assert_eq!(
        oracle_with_roots(IMPLICIT_FILE_MODULE, &["library"]).unwrap(),
        [7, 7]
    );
    let root_implicit = [("main.asm", IMPLICIT_ROOT), IMPLICIT_FILE_MODULE[1]];
    assert_eq!(
        oracle_with_roots(&root_implicit, &["library"]).unwrap(),
        [7, 7]
    );
    let candidate_with_end = format!("{}.end\n", IMPLICIT_FILE_MODULE[1].1);
    assert_eq!(
        oracle_with_roots(
            &[
                IMPLICIT_FILE_MODULE[0],
                ("library/dep.asm", &candidate_with_end),
            ],
            &["library"],
        )
        .unwrap(),
        [7, 7]
    );
}

#[test]
fn binary_discovery_implicit_name_precedence_rust_oracle() {
    let duplicate = [
        IMPLICIT_FILE_MODULE[0],
        IMPLICIT_FILE_MODULE[1],
        ("other/dep.asm", ".cpu m6502\n.pub\nvalue = 8\n"),
    ];
    assert!(oracle_with_roots(&duplicate, &["library", "other"])
        .unwrap_err()
        .contains("Ambiguous module"));

    let explicit_other = [
        IMPLICIT_FILE_MODULE[0],
        (
            "library/dep.asm",
            ".module other\n.cpu m6502\n.pub\nvalue = 7\n.endmodule\n",
        ),
    ];
    assert!(oracle_with_roots(&explicit_other, &["library"])
        .unwrap_err()
        .contains("unknown module"));
}

#[test]
#[ignore = "requires configured FS-UAE; implicit module ID from filename"]
fn compact_cli_implicit_file_module_fs_uae() {
    let expected = oracle_with_roots(IMPLICIT_FILE_MODULE, &["library"]).unwrap();
    compact_cli(
        IMPLICIT_FILE_MODULE,
        &["library"],
        &[],
        Some(&expected),
        false,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; implicit entry module ID from filename"]
fn compact_cli_implicit_root_fs_uae() {
    let files = [("main.asm", IMPLICIT_ROOT), IMPLICIT_FILE_MODULE[1]];
    let expected = oracle_with_roots(&files, &["library"]).unwrap();
    compact_cli(&files, &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; .end closes a file-derived module"]
fn compact_cli_implicit_dependency_end_fs_uae() {
    let dependency = format!("{}.end\n", IMPLICIT_FILE_MODULE[1].1);
    let files = [
        IMPLICIT_FILE_MODULE[0],
        ("library/dep.asm", dependency.as_str()),
    ];
    let expected = oracle_with_roots(&files, &["library"]).unwrap();
    compact_cli(&files, &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; ambiguous implicit basenames reject"]
fn compact_cli_implicit_duplicate_rejection_fs_uae() {
    let files = [
        IMPLICIT_FILE_MODULE[0],
        IMPLICIT_FILE_MODULE[1],
        ("other/dep.asm", ".cpu m6502\n.pub\nvalue = 8\n"),
    ];
    compact_cli(&files, &["library", "other"], &[], None, false);
}

#[test]
#[ignore = "requires configured FS-UAE; explicit module suppresses filename fallback"]
fn compact_cli_explicit_module_precedence_fs_uae() {
    let files = [
        IMPLICIT_FILE_MODULE[0],
        (
            "library/dep.asm",
            ".module other\n.cpu m6502\n.pub\nvalue = 7\n.endmodule\n",
        ),
    ];
    compact_cli(&files, &["library"], &[], None, false);
}
