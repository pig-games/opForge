//! F2: package-controlled operand matching and semantic/TABL composition.
use super::*;

const INDEXED: &str = include_str!("../../fixtures/binary-source/indexed-boundaries.asm");
const DEFERRED_INDEX: &str = ".cpu m6502\n.org $1000\n lda target,x\ntarget:\n .byte 0\n.end\n";
const REGISTERS: &str = include_str!("../../fixtures/binary-source/register-predicates.asm");
const SELF_HOST_MOVEM: &str = ".cpu m68020\n.org 0\n movem.l d2-d7/a2-a6, -(sp)\n.end\n";
const SELF_HOST_MOVEA_SHORT: &str = ".cpu m68020\n.org 0\n movea.l 4.w,a6\n.end\n";
const NUMERIC_MEMBER_VARIANTS: &str = ".cpu m68020\n.org 0\n lea 8.w,a0\n.end\n";
const MOVEM_WORD_LIST: &str = ".cpu m68020\n.org 0\n movem.w d0/a7, -(a7)\n.end\n";
const MOVEM_SINGLE: &str = ".cpu m68020\n.org 0\n movem.l d2, -(sp)\n.end\n";
const MOVEM_DUPLICATE: &str = ".cpu m68020\n.org 0\n movem.l d2/d2, -(sp)\n.end\n";
const MOVEM_CROSS_CLASS: &str = ".cpu m68020\n.org 0\n movem.l d2-a2, -(sp)\n.end\n";
const SCALAR_SUBTRACTION: &str = ".cpu m6502\n.org $1000\nVALUE=5\n lda VALUE-1\n.end\n";

fn oracle_bytes(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust assembly");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn binary_selection_positive_oracles() {
    assert_eq!(oracle_bytes(DEFERRED_INDEX), [0xbd, 3, 0x10, 0]);
    assert_eq!(
        oracle_bytes(INDEXED),
        [
            0xb5, 0, 0xb5, 0xff, 0xbd, 0, 1, 0xbd, 0xff, 0xff, 0xb6, 0, 0xb6, 0xff, 0xbe, 0, 1,
            0xbe, 0xff, 0xff, 0xb9, 0x20, 0, 0x9d, 0, 0x20, 0x99, 0, 0x21,
        ]
    );
    assert_eq!(
        oracle_bytes(REGISTERS),
        [0x22, 0, 0x30, 7, 0x2e, 0, 0x7e, 0x7f, 0x70, 0x80]
    );
}

#[test]
fn binary_selection_self_host_movem_rust_oracle() {
    assert_eq!(oracle_bytes(SELF_HOST_MOVEM), [0x48, 0xe7, 0x3f, 0x3e]);
    assert_eq!(oracle_bytes(MOVEM_SINGLE).len(), 4);
    assert_eq!(oracle_bytes(MOVEM_WORD_LIST).len(), 4);
    assert_eq!(oracle_bytes(MOVEM_DUPLICATE), oracle_bytes(MOVEM_SINGLE));
    assert_rust_rejection(MOVEM_CROSS_CLASS);
    assert_eq!(oracle_bytes(SCALAR_SUBTRACTION), [0xa5, 4]);
}

#[test]
fn binary_selection_self_host_movea_short_rust_oracle() {
    assert_eq!(oracle_bytes(SELF_HOST_MOVEA_SHORT), [0x2c, 0x78, 0, 4]);
    assert_eq!(oracle_bytes(NUMERIC_MEMBER_VARIANTS), [0x41, 0xf8, 0, 8]);
}

#[test]
#[ignore = "requires configured FS-UAE; numeric member wrapper parity"]
fn binary_selection_self_host_movea_short_native_parity_fs_uae() {
    assert_binary_source(SELF_HOST_MOVEA_SHORT.into(), "m68020".into());
    assert_binary_source(NUMERIC_MEMBER_VARIANTS.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; package-owned packed register-list fragment"]
fn binary_selection_self_host_movem_native_parity_fs_uae() {
    assert_eq!(oracle_bytes(SELF_HOST_MOVEM), [0x48, 0xe7, 0x3f, 0x3e]);
    assert_binary_source(SELF_HOST_MOVEM.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; package mask classes and register lists"]
fn binary_selection_movem_variants_native_parity_fs_uae() {
    for source in [MOVEM_WORD_LIST, MOVEM_SINGLE, MOVEM_DUPLICATE] {
        assert_binary_source(source.into(), "m68020".into());
    }
}

#[test]
#[ignore = "requires configured FS-UAE; mixed-class range rejected"]
fn binary_selection_movem_cross_class_native_rejection_fs_uae() {
    assert_rust_rejection(MOVEM_CROSS_CLASS);
    assert_native_files_rejection(
        &[("input.asm", MOVEM_CROSS_CLASS)],
        "m68020",
        Some("[file 00000001, line 00000003]"),
    );
}

#[test]
#[ignore = "requires configured FS-UAE; name subtraction remains scalar"]
fn binary_selection_scalar_subtraction_fs_uae() {
    assert_eq!(oracle_bytes(SCALAR_SUBTRACTION), [0xa5, 4]);
    assert_binary_source(SCALAR_SUBTRACTION.into(), "m6502".into());
}

#[test]
#[ignore = "requires configured FS-UAE; indexed X/Y and address-width boundaries"]
fn binary_selection_indexed_fs_uae() {
    assert_binary_source(INDEXED.into(), "m6502".into());
}

#[test]
#[ignore = "requires configured FS-UAE; register class and signed-range endpoints"]
fn binary_selection_registers_fs_uae() {
    assert_binary_source(REGISTERS.into(), "m68000".into());
}

// One selector chooses one bounded, fresh native invocation. Host tests check
// every oracle; native coverage is only claimed for individually completed runs.
const REJECTIONS: &[(&str, &str, &str)] = &[
    ("index_accumulator", "m6502", "lda $20,a"),
    ("index_wrong_x", "m6502", "ldx $20,x"),
    ("index_wrong_y", "m6502", "ldy $20,y"),
    ("index_narrow_overflow", "m6502", "stx $100,y"),
    ("index_wide_overflow", "m6502", "lda $10000,x"),
    ("banked_register", "m68000", "move.l b0,d0"),
    ("banked_leading_zero", "m68000", "move.l b00,d0"),
    ("extended_register", "m68000", "move.l e0,d0"),
    ("banked_destination", "m68000", "move.l d0,b0"),
    ("register_class", "m68000", "moveq #1,a0"),
    ("signed_upper", "m68000", "moveq #128,d0"),
    ("signed_lower", "m68000", "moveq #-129,d0"),
];

fn source(cpu: &str, instruction: &str) -> String {
    format!(".cpu {cpu}\n.org $1000\n {instruction}\n.end\n")
}

fn assert_rust_rejection(source: &str) {
    let result =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true);
    assert!(
        !matches!(result, Ok((_, ref diagnostics)) if diagnostics.is_empty()),
        "negative case must be rejected by live Rust: {source}"
    );
}

#[test]
fn binary_selection_rejection_oracles() {
    for (_, cpu, instruction) in REJECTIONS {
        assert_rust_rejection(&source(cpu, instruction));
    }
}

#[test]
#[ignore = "requires explicit OPFORGE_SELECTION_REJECTION and configured FS-UAE"]
fn binary_selection_rejection_fs_uae() {
    let case = std::env::var("OPFORGE_SELECTION_REJECTION").unwrap();
    let (_, cpu, instruction) = REJECTIONS
        .iter()
        .find(|(name, _, _)| *name == case)
        .unwrap();
    let source = source(cpu, instruction);
    assert_rust_rejection(&source);
    assert_native_rejection(&source, cpu);
}

// Full Rust layout resolves this; the fixed-pass native subset must not guess
// zero-page width from an unresolved value and move subsequent labels.
#[test]
#[ignore = "requires configured FS-UAE; unsupported width convergence fails closed"]
fn binary_selection_deferred_index_fs_uae() {
    assert_eq!(oracle_bytes(DEFERRED_INDEX), [0xbd, 3, 0x10, 0]);
    assert_native_rejection(DEFERRED_INDEX, "m6502");
}
