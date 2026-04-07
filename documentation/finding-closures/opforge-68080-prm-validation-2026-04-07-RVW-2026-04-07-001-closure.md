# Finding Closure Report

## Finding

- ID: RVW-2026-04-07-001
- Original summary: AMMX line-F instructions on `m68080` were incorrectly rejected unless `.apollo on` was enabled, even though the PRM reserves Apollo gating for the Line-A subset rather than AMMX.

## Claimed Fix

- Plan item: WI-1
- Implementation slice or commit: `d697a55` (`Removed incorrect Apollo gating from 68080 AMMX instructions.`)
- Changed files:
  - `crates/opforge-families/src/m68080/handler.rs`
  - `crates/opforge-families/src/m68080/module.rs`
  - `crates/opforge-asm/src/tests.rs`

## Validation Evidence

- Command or check: `cargo test -p asm 68080`
- Result: PASS. The WI-1 regression coverage proves default-off `LOADI` and `PAND` assemble on plain `.cpu 68080`, while `MOVS.B` remains Apollo-gated.
- Command or check: `cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo audit && cargo test --workspace`
- Result: PASS.

## Closure Status

- Status: fixed
- Residual risk: Low. Remaining 68080 validation risk is limited to unrelated opcode families, not the AMMX Apollo-gating boundary corrected by this slice.

## Notes

- This closure is tied to the WI-1 plan-compliance PASS that cleared commit `d697a55`.