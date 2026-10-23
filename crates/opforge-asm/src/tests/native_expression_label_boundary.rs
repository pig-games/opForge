//! Native parenthesized label-term boundary parity proofs.

use super::*;

const LABEL_BOUNDARY_SOURCE: &[u8] = b"        .org $1000\njump_tab .byte 1,2,3\nentries .const ($-jump_tab) / 3\n        .byte entries\n        .word (jump_tab)\n";

fn rust_label_boundary_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(LABEL_BOUNDARY_SOURCE).expect("label-boundary fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust label-boundary authority");
    assert!(
        diagnostics.is_empty(),
        "Rust label-boundary diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_label_boundary_rust_oracle() {
    // Proof level A. This proves live Rust evaluates current-address
    // subtraction from an underscore-bearing label inside a group, continues
    // with division, and accepts a directly parenthesized label term. It does
    // not prove native token consumption or guest execution.
    assert_eq!(
        rust_label_boundary_bytes(),
        vec![0x01, 0x02, 0x03, 0x01, 0x00, 0x10]
    );
}
