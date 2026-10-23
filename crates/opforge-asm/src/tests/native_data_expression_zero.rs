//! Native zero-valued data-expression parity proofs.

use super::*;

const ZERO_DATA_SOURCE: &[u8] = b".org 0\
\nBASE .const $2000\
\n.byte (BASE & $ff), $7f, (0 || 0), (BASE >> 8)\
\n.byte $11, (BASE & $ff)\
\n        rts\n";

fn rust_zero_data_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(ZERO_DATA_SOURCE).expect("zero-data fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust zero-data authority");
    assert!(
        diagnostics.is_empty(),
        "Rust zero-data diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_data_expression_zero_rust_oracle() {
    // Proof level A. This proves live Rust accepts zero-valued expressions in
    // the first, middle, and later positions of a numeric byte list and fixes
    // their exact output. It does not prove native evaluation or guest output.
    assert_eq!(
        rust_zero_data_bytes(),
        [0x00, 0x7f, 0x00, 0x20, 0x11, 0x00, 0x60]
    );

    let (_, diagnostics) = assemble_source_entries_with_runtime_mode(
        &[".cpu 65c02", ".byte (MISSING & $ff), 1"],
        true,
    )
    .expect("unresolved Rust data expression should report diagnostics");
    assert!(
        !diagnostics.is_empty(),
        "Rust must reject an unresolved numeric data expression"
    );
}
