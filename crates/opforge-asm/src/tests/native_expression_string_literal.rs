//! Native scalar string-literal expression parity proofs.

use super::*;

const STRING_LITERAL_SOURCE: &[u8] = b"single .const 'A'\
\npair .const 'AB'\
\nnewline .const '\\n'\
\ncarriage .const '\\r'\
\ntab .const '\\t'\
\nzero .const '\\0'\
\nhex_char .const '\\x43'\
\ndouble_quote .const \"D\"\
\nstart lda #single\
\n      lda #<pair\
\n      lda #>pair\
\n      lda #newline\
\n      lda #carriage\
\n      lda #tab\
\n      lda #zero\
\n      lda #'\\x43'\
\n      lda #\"D\"\
\n      .word pair\
\n      rts\n";

fn rust_string_literal_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(STRING_LITERAL_SOURCE).expect("string-literal fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust string-literal authority");
    assert!(
        diagnostics.is_empty(),
        "Rust string-literal diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_string_literal_rust_oracle() {
    // Proof level A. This proves live Rust one/two-byte packing, both quote
    // styles, escape decoding, direct selected-operand use, and final bytes. It
    // does not prove native compilation or guest execution.
    assert_eq!(
        rust_string_literal_bytes(),
        [
            0xa9, 0x41, 0xa9, 0x42, 0xa9, 0x41, 0xa9, 0x0a, 0xa9, 0x0d, 0xa9, 0x09, 0xa9, 0x00,
            0xa9, 0x43, 0xa9, 0x44, 0x42, 0x41, 0x60,
        ]
    );
    for source in ["value .const ''", "value .const 'ABC'", "value .const 'A"] {
        let (_, diagnostics) = assemble_source_entries_with_runtime_mode(
            &[".cpu 65c02", source, "start lda #value"],
            true,
        )
        .expect("invalid Rust string literal should report diagnostics");
        assert!(
            !diagnostics.is_empty(),
            "Rust must reject malformed scalar literal in `{source}`"
        );
    }
}
