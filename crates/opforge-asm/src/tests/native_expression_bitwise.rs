//! Native scalar bitwise-expression parity proofs.

use super::*;

const BITWISE_SOURCE: &[u8] = b"bit_and .const $f0 & $0f\
\nbit_or .const $f0 | $0f\
\nbit_xor .const $f0 ^ $0f\
\nprecedence .const 1 | 2 ^ 3 & 1\
\ncompare_first .const 2 == 2 & 1\
\nlogical_yield .const 0 || 3\
\nsuffix_and .const 23H & 0FH\
\nsuffix_or_xor .const 23H | 0FH ^ 0FFH\
\n        .byte bit_and,bit_or,bit_xor,precedence,compare_first,logical_yield,suffix_and,suffix_or_xor\n";

fn rust_bitwise_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(BITWISE_SOURCE).expect("bitwise fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust bitwise authority");
    assert!(
        diagnostics.is_empty(),
        "Rust bitwise diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_bitwise_rust_oracle() {
    // Proof level A. This proves live Rust values and the comparison/AND/XOR/OR
    // precedence ladder, including yielding `||` to the logical tier. It does
    // not prove native parser or evaluator execution.
    assert_eq!(rust_bitwise_bytes(), [0, 0xff, 0xff, 3, 1, 1, 3, 0xf3]);
}
