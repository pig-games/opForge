//! Shared scalar string leaves must behave identically before and after capture.
use super::*;

const SUBTRACTION: &str = " addi.w #'0'-1,d0\n";
const ADDITION: &str = " moveq #'0'+11,d0\n";
const SINGLE: &str = " move.b #'S',(a1)+\n moveq #\"S\",d0\n";
const PAIR: &str = " move.w #'AB'+1,d0\n move.w #\"AB\"+1,d1\n";
const SCALARS: &str = "Single = '0'-1\nPair = \"AB\"+1\n.word Single,Pair\n.word '0'-1,\"AB\"+1\n.word ('0'-1),(\"AB\"+1)\n";
const STRINGS: &str = ".byte 'S',\"S\",'AB',\"AB\",'text',\"text\"\n";
const INVALID: &[&str] = &[
    "Empty = ''\n.word Empty\n",
    "Long = 'ABC'\n.word Long\n",
    " move.w #'',d0\n",
    " move.l #\"ABC\",d0\n",
];
const CASES: &[(&str, &str, &[u8])] = &[
    ("subtraction", SUBTRACTION, &[0x06, 0x40, 0, 0x2f]),
    ("addition", ADDITION, &[0x70, 0x3b]),
    ("single", SINGLE, &[0x12, 0xfc, 0, 0x53, 0x70, 0x53]),
    (
        "pair",
        PAIR,
        &[0x30, 0x3c, 0x41, 0x43, 0x32, 0x3c, 0x41, 0x43],
    ),
    (
        "scalars",
        SCALARS,
        &[
            0, 0x2f, 0x41, 0x43, 0, 0x2f, 0x41, 0x43, 0, 0x2f, 0x41, 0x43,
        ],
    ),
    ("strings", STRINGS, b"SSABABtexttext"),
];

fn source(body: &str) -> String {
    format!(".module app\n.cpu m68020\n{body}.endmodule\n")
}

fn combined_body() -> String {
    CASES.iter().map(|(_, body, _)| *body).collect()
}

#[test]
fn compact_character_literals_rust_oracles() {
    for (name, body, expected) in CASES {
        let source = source(body);
        assert_eq!(
            oracle_with_roots(&[("main.asm", &source)], &[]).unwrap(),
            *expected,
            "{name}"
        );
    }
    for body in INVALID {
        assert!(
            oracle_with_roots(&[("main.asm", &source(body))], &[]).is_err(),
            "{body}"
        );
    }
}

fn native(body: &str) {
    let source = source(body);
    let expected = oracle_with_roots(&[("main.asm", &source)], &[]).unwrap();
    // The shared harness honors OPFORGE_COMPARE_NATIVE_ROOT and selects its
    // relocated, source-poisoning replay entry only through the explicit define.
    assert_binary_files(&[("main.asm", &source)], "m68020", expected);
}

#[test]
#[ignore = "requires configured FS-UAE; direct or captured character subtraction"]
fn compact_character_literal_subtraction_fs_uae() {
    native(SUBTRACTION);
}

#[test]
#[ignore = "requires configured FS-UAE; direct or captured character addition"]
fn compact_character_literal_addition_fs_uae() {
    native(ADDITION);
}

#[test]
#[ignore = "requires configured FS-UAE; both quote forms in single-byte instruction leaves"]
fn compact_character_literal_single_fs_uae() {
    native(SINGLE);
}

#[test]
#[ignore = "requires configured FS-UAE; two-byte scalar leaves pack big-endian"]
fn compact_character_literal_pair_fs_uae() {
    native(PAIR);
}

#[test]
#[ignore = "requires configured FS-UAE; assignments and shared data scalar expressions"]
fn compact_character_literal_scalars_fs_uae() {
    native(SCALARS);
}

#[test]
#[ignore = "requires configured FS-UAE; plain quoted data strings preserve byte emission"]
fn compact_character_literal_strings_fs_uae() {
    native(STRINGS);
}

#[test]
#[ignore = "requires configured FS-UAE; all scalar and data string forms, direct or captured"]
fn compact_character_literals_combined_fs_uae() {
    native(&combined_body());
}

#[test]
#[ignore = "requires configured FS-UAE; graph capture rejects empty and longer scalar strings"]
fn compact_character_literals_cli_rejection_fs_uae() {
    for body in &INVALID[2..] {
        let text = source(body);
        assert!(oracle_with_roots(&[("main.asm", &text)], &[]).is_err());
        compact_cli_cpu(&[("main.asm", &text)], &[], &[], None, false, "m68020");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; graph capture and dependency-ordered replay"]
fn compact_character_literals_cli_fs_uae() {
    let text = source(&combined_body());
    let expected = oracle_with_roots(&[("main.asm", &text)], &[]).unwrap();
    compact_cli_cpu(
        &[("main.asm", &text)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68020",
    );
}
