//! Native scalar exponentiation parity proofs.

use super::*;

const POWER_SOURCE: &[u8] = b"pow1 .const 2 ** 3\
\npow2 .const 3 ** 2 ** 2\
\npow3 .const 2 * 3 ** 2\
\npow4 .const 2 ** 3 * 2\
\npow5 .const 0 ** 0\
\n        .byte pow1,pow2,pow3,pow4,pow5\n";

fn rust_power_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(POWER_SOURCE).expect("power fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust power authority");
    assert!(
        diagnostics.is_empty(),
        "Rust power diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_power_rust_oracle() {
    // Proof level A. This proves the live Rust expression implementation's
    // right associativity, precedence, and zero-exponent behavior. It does not
    // prove native parser or evaluator execution.
    assert_eq!(rust_power_bytes(), [8, 81, 18, 16, 1]);

    let (_, diagnostics) = assemble_source_entries_with_runtime_mode(
        &[".cpu 65c02", "bad .const 2 ** -1", "        .byte bad"],
        true,
    )
    .expect("negative power source should report diagnostics without panicking");
    assert!(
        !diagnostics.is_empty(),
        "Rust must reject a negative integer exponent"
    );
}
