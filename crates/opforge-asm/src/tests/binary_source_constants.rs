//! First breadth increment: immutable numeric constants and practical routines.
use super::*;

const PAGE_COPY: &str = include_str!("../../fixtures/binary-source/page-copy.asm");
const REVERSE_BYTE: &str = include_str!("../../fixtures/binary-source/reverse-byte.asm");
const RANGE_CHECK: &str = include_str!("../../fixtures/binary-source/range-check.asm");
const REGISTER_PAIR: &str = ".cpu m68000\n.org $1000\n move.l d0,d1\n.end\n";

fn bytes(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust assembly");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn binary_constants_routine_oracles() {
    assert_eq!(bytes(REGISTER_PAIR), [0x22, 0]);
    assert_eq!(
        bytes(REVERSE_BYTE),
        [0xa2, 8, 0x46, 0x20, 0x26, 0x21, 0xca, 0xd0, 0xf9, 0x60, 0x20, 0, 0x21, 0, 10, 0]
    );
    assert_eq!(
        bytes(PAGE_COPY),
        [
            0xa2, 0, 0xbd, 0, 0x20, 0x9d, 0, 0x21, 0xe8, 0xd0, 0xf7, 0x60, 0, 0x20, 0, 0x21, 0, 1,
            12, 0
        ]
    );
    // Independent instruction/data contract, including the negative constant.
    assert_eq!(
        bytes(RANGE_CHECK),
        [
            0x90, 0xbc, 0, 0, 0, 32, 0xb0, 0xbc, 0, 0, 0, 94, 0x62, 4, 0x70, 0, 0x4e, 0x75, 0x70,
            0xff, 0x4e, 0x75, 0, 32, 0, 126, 0, 22, 0xff, 0xff, 0xff, 0xff
        ]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; indexed package-selection gap"]
fn binary_constants_indexed_gap_fs_uae() {
    // The package's direct_x/direct_y predicates are not yet lowered into the
    // numeric view. Retain the real case for that next increment; do not erase
    // the register distinction by mapping both to an unchecked operand pair.
    assert_native_rejection(PAGE_COPY, "m6502");
}

#[test]
#[ignore = "requires configured FS-UAE; complete byte-reversal routine"]
fn binary_constants_reverse_byte_fs_uae() {
    assert_binary_source(REVERSE_BYTE.into(), "m6502".into());
}

#[test]
#[ignore = "requires configured FS-UAE; complete range-check routine"]
fn binary_constants_range_check_fs_uae() {
    assert_binary_source(RANGE_CHECK.into(), "m68000".into());
}

fn arithmetic_source(cpu: &str) -> String {
    format!(
        ".cpu {cpu}\nbase_value = 7\nnegative = -base_value\n\
         product = (base_value+3)*2\n.org $1000\nfirst:\n\
         .byte product,base_value,negative+8\nlast:\n\
         distance = last-first\nposition = $\n\
         .word distance,position\n.long negative\n.end\n"
    )
}

#[test]
fn binary_constants_arithmetic_oracles() {
    assert_eq!(
        bytes(&arithmetic_source("m6502")),
        [20, 7, 1, 3, 0, 3, 16, 249, 255, 255, 255]
    );
    assert_eq!(
        bytes(&arithmetic_source("m68000")),
        [20, 7, 1, 0, 3, 16, 3, 255, 255, 255, 249]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; generic constant semantics"]
fn binary_constants_arithmetic_fs_uae() {
    assert_binary_source(arithmetic_source("m6502"), "m6502".into());
}

// Each case gets its own fresh native invocation. The boolean records current
// Rust acceptance, not a declaration that the program is semantically valid.
// In particular Rust currently accepts this cycle through provisional pass-one
// values; native rejects it rather than reproducing that convergence defect.
const REJECTIONS: &[(&str, &str, bool)] = &[
    ("forward", "early = later+1\nlater = 4\n.byte early", true),
    ("cycle", "left = right+1\nright = left+1\n.byte left", true),
    ("duplicate", "value = 1\nvalue = 2\n.byte value", false),
    ("label_collision", "value:\nvalue = 2\n.byte value", false),
    (
        "constant_collision",
        "value = 2\nvalue:\n.byte value",
        false,
    ),
    ("missing", "value = missing+1\n.byte value", false),
    ("trailing", "value = 1,2\n.byte value", false),
];

fn rejection_source(body: &str) -> String {
    format!(".cpu m6502\n.org $1000\n{body}\n.end\n")
}

#[test]
fn binary_constants_rejection_oracles() {
    for (name, body, rust_accepts) in REJECTIONS {
        let source = rejection_source(body);
        let result =
            assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true);
        let accepted = matches!(result, Ok((_, ref diagnostics)) if diagnostics.is_empty());
        assert_eq!(accepted, *rust_accepts, "live Rust domain for {name}");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; package rejection-predicate gap"]
fn binary_constants_register_pair_gap_fs_uae() {
    assert_eq!(bytes(REGISTER_PAIR), [0x22, 0]);
    assert_native_rejection(REGISTER_PAIR, "m68000");
}

#[test]
#[ignore = "requires explicit OPFORGE_CONSTANT_REJECTION and configured FS-UAE"]
fn binary_constants_rejection_fs_uae() {
    let case = std::env::var("OPFORGE_CONSTANT_REJECTION").unwrap();
    let (_, body, _) = REJECTIONS
        .iter()
        .find(|(name, _, _)| *name == case)
        .unwrap();
    assert_native_rejection(&rejection_source(body), "m6502");
}
