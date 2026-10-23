//! Native non-emitting `.ds` reservation parity proofs.

use super::*;

const DS_RESERVATION_SOURCE: &[u8] = b".org 0\n.byte 1\ngap .ds 3\nafter .byte 2\n.word after\n";

fn rust_ds_reservation_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(DS_RESERVATION_SOURCE).expect("ds fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust ds authority");
    assert!(
        diagnostics.is_empty(),
        "Rust ds diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_ds_reservation_rust_oracle() {
    // Proof level A. This proves live Rust omits the reserved gap from its
    // emitted entries while the following label retains address 4. It does
    // not prove native sizing, emission, or guest execution.
    assert_eq!(rust_ds_reservation_bytes(), vec![0x01, 0x02, 0x04, 0x00]);
}
