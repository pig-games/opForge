//! Native scalar comparison parity proofs.

use super::*;

const COMPARISON_SOURCE: &[u8] = b"eq .const 3 == 3\
\nne .const 3 != 4\
\nne_alt .const 3 <> 4\
\nle .const 3 <= 4\
\nlt .const 3 < 4\
\nge .const 4 >= 3\
\ngt .const 4 > 3\
\nsigned .const -2 < -1\
\nprecedence .const 1 << 2 == 4\
\nfalse_value .const 4 < 3\
\n        .byte eq,ne,ne_alt,le,lt,ge,gt,signed,precedence,false_value\n";

fn rust_comparison_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(COMPARISON_SOURCE).expect("comparison fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust comparison authority");
    assert!(
        diagnostics.is_empty(),
        "Rust comparison diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_comparison_rust_oracle() {
    // Proof level A. This proves live Rust values for every retained spelling,
    // signed ordering, false projection, and shift/comparison precedence. It
    // does not prove native parser or evaluator execution.
    assert_eq!(rust_comparison_bytes(), [1, 1, 1, 1, 1, 1, 1, 1, 1, 0]);
}
