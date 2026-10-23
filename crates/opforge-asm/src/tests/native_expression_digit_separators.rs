//! Native scalar digit-separator parity proofs.

use super::*;

#[test]
fn native_expression_digit_separators_rust_oracle() {
    // Proof level A. This proves Rust accepts the retained separator forms and
    // preserves their scalar values. It does not prove native execution.
    let cases = [
        ("decimal", "1_0", 10),
        ("hex-prefix", "$F_F", 0xff),
        ("binary-prefix", "%1010_1010", 0xaa),
        ("binary-suffix", "1010_1010b", 0xaa),
    ];
    for (name, literal, expected) in cases {
        let value_line = format!("value .const {literal}");
        let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
            &[".cpu 65c02", value_line.as_str(), "start lda #value", "rts"],
            true,
        )
        .unwrap_or_else(|err| panic!("assemble digit-separator oracle {name}: {err}"));
        assert!(
            diagnostics.is_empty(),
            "Rust diagnostics for {name}: {diagnostics:?}"
        );
        assert_eq!(entries[1].1, expected, "separator value for {name}");
    }
    let (_, invalid_diagnostics) = assemble_source_entries_with_runtime_mode(
        &[".cpu 65c02", "value .const $___+1", "start lda #value"],
        true,
    )
    .expect("invalid Rust source should report diagnostics without panicking");
    assert!(
        !invalid_diagnostics.is_empty(),
        "underscore-only prefixed literal must remain invalid"
    );
}
