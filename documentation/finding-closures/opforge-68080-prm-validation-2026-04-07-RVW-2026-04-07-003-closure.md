# Finding Closure Report

## Finding

- ID: RVW-2026-04-07-003
- Original summary: Plain `.cpu 68080` defaulted the runtime FPU target to disabled, so documented 68080 FPU mnemonics failed unless `.fpu 68080` was set explicitly.

## Claimed Fix

- Plan item: WI-3
- Implementation slice or commit: `160f58b` (`Enabled the integrated 68080 FPU by default.`)
- Changed files:
  - `crates/opforge-families/src/m68k/state.rs`
  - `crates/opforge-asm/src/tests.rs`
  - `documentation/opForge-m68000-family-68080-full-extension-spec-v0_1.md`

## Validation Evidence

- Command or check: `cargo test -p asm m68080_fpu`
- Result: PASS. The targeted FPU regressions prove plain `.cpu 68080` assembles the documented legacy 68080 FPU surface, while `.fpu none` still disables that surface deterministically and illegal target pairings remain rejected.
- Command or check: `cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo audit && cargo test --workspace`
- Result: PASS.

## Closure Status

- Status: fixed
- Residual risk: Low. The closure addresses the default integrated-FPU contract specifically; any remaining 68080 FPU work would be in deeper opcode-surface validation rather than the default-state mismatch.

## Notes

- This closure is tied to the WI-3 plan-compliance PASS that cleared commit `160f58b`.