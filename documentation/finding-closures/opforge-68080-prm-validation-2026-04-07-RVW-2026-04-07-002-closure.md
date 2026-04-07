# Finding Closure Report

## Finding

- ID: RVW-2026-04-07-002
- Original summary: `MOVIW.L` was Apollo-gated by default and always emitted the restricted Line-A compatibility opcode instead of the regular 68080 PRM encoding.

## Claimed Fix

- Plan item: WI-2
- Implementation slice or commit: `10f5c52` (`Made regular MOVIW the default 68080 encoding.`)
- Changed files:
  - `crates/opforge-families/src/m68080/handler.rs`
  - `crates/opforge-asm/src/tests.rs`
  - `documentation/opForge-m68000-family-68080-full-extension-spec-v0_1.md`
  - `examples/ab/motorola68000/vasm/68080/fixtures.toml`
  - `examples/motorola68000/68080_apollo_gate_error.asm`
  - `examples/motorola68000/68080_integer_addressing_matrix.asm`
  - `examples/reference/motorola68000/68080_apollo_gate_error.err`
  - `examples/reference/motorola68000/68080_full_additional_surface.hex`
  - `examples/reference/motorola68000/68080_full_additional_surface.lst`
  - `examples/reference/motorola68000/68080_integer_addressing_matrix.hex`
  - `examples/reference/motorola68000/68080_integer_addressing_matrix.lst`

## Validation Evidence

- Command or check: `cargo test -p asm moviw`
- Result: PASS. The focused regressions prove plain `.cpu 68080` `MOVIW.L` assembles without `.apollo on`, emits the regular 68080 encoding, and still leaves Line-A-only `MOVS` under Apollo gating.
- Command or check: `cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo audit && cargo test --workspace`
- Result: PASS.

## Closure Status

- Status: fixed
- Residual risk: Low. No deprecated Line-A `MOVIW` compatibility selector is currently exposed, so compatibility-only behavior is intentionally absent rather than silently ambiguous.

## Notes

- This closure is tied to the WI-2 plan-compliance PASS that cleared commit `10f5c52`.