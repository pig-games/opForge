//! Numeric CPU aliases are package names only in the shared `.cpu` operand.
use super::*;

const CASES: &[(&str, &str, &[u8])] = &[
    (
        "m68020",
        ".cpu 68020\n.long 68020\nmark .cpu 68020\n.long mark\nother: .cpu 68020\n.long other\n.end\n",
        &[0, 1, 9, 180, 0, 0, 0, 4, 0, 0, 0, 8],
    ),
    (
        "m6502",
        ".cpu 6502\n.word 6502\nmark .cpu 6502\n.word mark\nother: .cpu 6502\n.word other\n.end\n",
        &[102, 25, 2, 0, 4, 0],
    ),
];

#[test]
fn binary_cpu_numeric_alias_oracles() {
    for (_, source, expected) in CASES {
        assert_eq!(
            graph::oracle_with_roots(&[("input.asm", source)], &[]).unwrap(),
            *expected
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; package-owned numeric aliases and ordinary literals"]
fn binary_cpu_numeric_alias_fs_uae() {
    for (cpu, source, _) in CASES {
        assert_binary_source((*source).into(), (*cpu).into());
    }
}

#[test]
#[ignore = "requires configured FS-UAE; CPU aliases must match the selected package"]
fn binary_cpu_numeric_alias_mismatch_fs_uae() {
    assert_native_rejection(".cpu 6502\n.long 1\n.end\n", "m68020");
}
