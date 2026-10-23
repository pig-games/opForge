//! Native quoted `.word` string-data parity proofs.

use super::*;

const WORD_STRING_SOURCE: &[u8] =
    b".org 0\n.word $4142\n.word 'AB'\n.word \"AB\"\n.word 'A'*256 | 'B'\n";

fn rust_word_string_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(WORD_STRING_SOURCE).expect("word-string fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust word-string authority");
    assert!(
        diagnostics.is_empty(),
        "Rust word-string diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_word_string_data_rust_oracle() {
    // Proof level A. This proves live Rust emits pure quoted `.word` operands
    // in decoded source order while numeric forms retain little-endian word
    // packing. It does not prove native routing or execution.
    assert_eq!(
        rust_word_string_bytes(),
        vec![0x42, 0x41, 0x41, 0x42, 0x41, 0x42, 0x42, 0x41]
    );
}
