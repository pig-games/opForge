//! Native scalar logical-expression parity proofs.

use super::*;

const LOGICAL_SOURCE: &[u8] = b"and_true .const 2 && 3\
\nand_false .const 2 && 0\
\nor_true .const 0 || 3\
\nor_false .const 0 || 0\
\nxor_true .const 0 ^^ 3\
\nxor_false .const 2 ^^ 3\
\nand_precedence .const 1 || 0 && 0\
\nor_xor_left .const 1 || 1 ^^ 1\
\nbitwise_first .const 2 && 1 | 2\
\n        .byte and_true,and_false,or_true\
\n        .byte or_false,xor_true,xor_false\
\n        .byte and_precedence,or_xor_left,bitwise_first\n";

fn rust_logical_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(LOGICAL_SOURCE).expect("logical fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust logical authority");
    assert!(
        diagnostics.is_empty(),
        "Rust logical diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_logical_rust_oracle() {
    // Proof level A. This proves live Rust truthiness, logical-AND precedence,
    // shared left-associative OR/XOR precedence, and bitwise-before-logical
    // behavior. It does not prove native parser or evaluator execution.
    assert_eq!(rust_logical_bytes(), [1, 0, 1, 0, 1, 0, 1, 0, 1]);
}
