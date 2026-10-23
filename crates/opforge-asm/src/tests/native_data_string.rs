//! Native string-data directive parity proofs.

use super::*;

const DATA_STRING_SOURCE: &[u8] = b".org 0\
\n        .byte 'abcd'\
\n        .byte \"Labels can be up to 32 characters && must start with an alpha.\"\
\n        .byte \"\\r\\n\\t\\x2a\\x2B\\0\"\
\n        .byte '\\\\','\\\'',\"'\"\
\n        .byte 3,\"red\",4,'blue'\
\n        rts\n";

fn rust_data_string_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(DATA_STRING_SOURCE).expect("data-string fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust data-string authority");
    assert!(
        diagnostics.is_empty(),
        "Rust data-string diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

fn expected_data_string_bytes() -> Vec<u8> {
    let mut expected = Vec::new();
    expected.extend_from_slice(b"abcd");
    expected.extend_from_slice(b"Labels can be up to 32 characters && must start with an alpha.");
    expected.extend_from_slice(&[13, 10, 9, 0x2a, 0x2b, 0]);
    expected.extend_from_slice(b"\\''");
    expected.extend_from_slice(&[3]);
    expected.extend_from_slice(b"red");
    expected.extend_from_slice(&[4]);
    expected.extend_from_slice(b"blue");
    expected.push(0x60);
    expected
}

#[test]
fn native_data_string_rust_oracle() {
    // Proof level A. This proves the live Rust byte contract for both quote
    // styles, an exactly-64-byte operand, escapes, mixed lists, and final RTS.
    // It does not prove native parsing or guest execution.
    assert_eq!(rust_data_string_bytes(), expected_data_string_bytes());
}
