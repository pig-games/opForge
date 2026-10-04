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

const NUMERIC_MODULES: &[(&str, &str)] = &[
    (
        "6502_main.asm",
        ".cpu m6502\n.use _6502_dep as d\n.org $1001\n.byte d.value\n.end\n",
    ),
    (
        "library/6502_dep.asm",
        ".cpu m6502\n.org $1000\n.pub\nvalue = 7\n.byte value\n",
    ),
];

fn numeric_collision_files() -> Vec<(&'static str, &'static str)> {
    vec![
        NUMERIC_MODULES[0],
        NUMERIC_MODULES[1],
        ("other/_6502_DEP.asm", ".cpu m6502\n.pub\nvalue = 8\n"),
    ]
}

#[test]
fn binary_discovery_numeric_file_modules_rust_oracle() {
    assert_eq!(
        oracle_with_roots(NUMERIC_MODULES, &["library"]).unwrap(),
        [7, 7]
    );
    assert!(
        oracle_with_roots(&numeric_collision_files(), &["library", "other"])
            .unwrap_err()
            .contains("Ambiguous module")
    );
    // An explicit declaration suppresses the numeric filename fallback.
    let explicit = [
        NUMERIC_MODULES[0],
        (
            "library/6502_dep.asm",
            ".module other\n.cpu m6502\n.pub\nvalue = 7\n.endmodule\n",
        ),
    ];
    assert!(oracle_with_roots(&explicit, &["library"])
        .unwrap_err()
        .contains("unknown module"));
}

const NUMERIC_EXAMPLES: &[(&str, &str, &str)] = &[
    (
        "6502_simple.asm",
        include_str!("../../../../examples/mos6502/6502_simple.asm"),
        "m6502",
    ),
    (
        "68000_basic_moves.asm",
        include_str!("../../../../examples/motorola68000/68000_basic_moves.asm"),
        "m68000",
    ),
];

#[test]
fn binary_discovery_numeric_examples_match_neutral_rust_oracles() {
    for &(filename, source, _) in NUMERIC_EXAMPLES {
        let original = oracle(&[(filename, source)]).unwrap();
        let neutral = oracle(&[("input.asm", source)]).unwrap();
        assert!(!original.is_empty());
        assert_eq!(original, neutral, "filename changed output for {filename}");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; original numeric corpus filenames and neutral controls"]
fn compact_cli_numeric_examples_fs_uae() {
    for &(filename, source, cpu) in NUMERIC_EXAMPLES {
        let expected = oracle(&[(filename, source)]).unwrap();
        assert_eq!(expected, oracle(&[("input.asm", source)]).unwrap());
        for name in [filename, "input.asm"] {
            eprintln!("IMPLICIT_NUMERIC_EXAMPLE cpu={cpu} entry={name}");
            compact_cli_cpu(&[(name, source)], &[], &[], Some(&expected), false, cpu);
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; prefixed implicit root and dependency identity"]
fn compact_cli_numeric_file_modules_fs_uae() {
    let expected = oracle_with_roots(NUMERIC_MODULES, &["library"]).unwrap();
    compact_cli(NUMERIC_MODULES, &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; normalized numeric names remain ambiguous"]
fn compact_cli_numeric_collision_fs_uae() {
    let files = numeric_collision_files();
    assert!(oracle_with_roots(&files, &["library", "other"]).is_err());
    compact_cli(&files, &["library", "other"], &[], None, false);
}

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
