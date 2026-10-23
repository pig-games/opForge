//! Native scalar unary-expression parity proofs.

use super::*;

const UNARY_SOURCE: &[u8] = b"base .const $1234\
\nstart lda #>base\
\n      lda #<base\
\n      lda #!0\
\n      lda #!7\
\n      lda #(~$f0)&$ff\
\n      lda #(--5)\
\n      lda #(+1+2)\
\n      lda #>$1200 + $34\
\n      lda #<$12ff + 2\
\n      lda #-2 ** 2\
\n      lda #~2 ** 2\
\n      lda #!2 ** 0\
\n      lda #(<$1201)\
\n      lda #1 ? >$1234 : <$1234\
\n      lda #0 ? >$1234 : <$1234\
\n      rts\n";

fn rust_unary_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(UNARY_SOURCE).expect("unary fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust unary authority");
    assert!(
        diagnostics.is_empty(),
        "Rust unary diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_unary_rust_oracle() {
    // Proof level A. This proves live Rust recursive unary behavior, outer
    // high/low precedence, unary-before-power precedence, ternary-arm entry,
    // and the complete instruction byte sequence. It does not prove native
    // execution.
    assert_eq!(
        rust_unary_bytes(),
        [
            0xa9, 0x12, 0xa9, 0x34, 0xa9, 0x01, 0xa9, 0x00, 0xa9, 0x0f, 0xa9, 0x05, 0xa9, 0x03,
            0xa9, 0x12, 0xa9, 0x01, 0xa9, 0x04, 0xa9, 0x09, 0xa9, 0x01, 0xa9, 0x01, 0xa9, 0x12,
            0xa9, 0x34, 0x60,
        ]
    );
}
