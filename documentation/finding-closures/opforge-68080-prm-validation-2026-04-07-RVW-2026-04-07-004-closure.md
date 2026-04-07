# Finding Closure Report

## Finding

- ID: RVW-2026-04-07-004
- Original summary: The 68080 `MOVEC` surface exposed control-register code `$00C` only as `STH`, while the latest PRM uses `IEP3` as the canonical name and treats `STH` as legacy wording.

## Claimed Fix

- Plan item: WI-4
- Implementation slice or commit: `5e6a78b` (`Canonicalized 68080 MOVEC IEP3 naming.`)
- Changed files:
  - `crates/opforge-families/src/m68k/operand.rs`
  - `crates/opforge-families/src/m68k/handler.rs`
  - `crates/opforge-families/src/m68080/handler.rs`
  - `crates/opforge-asm/src/tests.rs`
  - `documentation/opForge-m68000-family-68080-full-extension-spec-v0_1.md`

## Validation Evidence

- Command or check: `cargo test -p asm movec`
- Result: PASS. The targeted MOVEC regression proves `MOVEC IEP3,Dn`, `MOVEC Dn,IEP3`, and the legacy alias `MOVEC Dn,STH` all encode as control-register code `$00C` on `m68080`.
- Command or check: `cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo audit && cargo test --workspace`
- Result: PASS.

## Closure Status

- Status: fixed
- Residual risk: Low. The canonical naming surface is now aligned with the PRM, and alias compatibility remains explicit rather than implicit.

## Notes

- This closure is tied to the WI-4 plan-compliance PASS that cleared commit `5e6a78b`.