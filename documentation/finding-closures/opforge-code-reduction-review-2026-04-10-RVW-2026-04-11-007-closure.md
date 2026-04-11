# Finding Closure Report

## Finding

- ID: `RVW-2026-04-11-007`
- Original summary: runtime-model bootstrap policy was implemented in parallel in `engine` and `asm`, including artifact-path resolution, load-from-path behavior, package-byte fallback, and artifact persistence.

## Claimed Fix

- Plan item: Work item 8 - consolidate runtime-model bootstrap policy into one shared module.
- Implementation slice or commit: pre-commit Work item 8 remediation slice on `main`
- Changed files:
  - `crates/opforge-vm/src/runtime_bootstrap.rs`
  - `crates/opforge-vm/src/lib.rs`
  - `crates/opforge-asm/src/runtime_model.rs`
  - `crates/opforge-asm/src/tests.rs`
  - `crates/opforge-engine/src/lib.rs`

## Validation Evidence

- Command or check: `cargo test -p asm --features vm-runtime-opasm-artifact vm_runtime_artifact_ -- --nocapture`
- Result: PASS; assembler-side runtime bootstrap tests passed for shared artifact-path parity, shared load-from-path parity, fallback-byte persistence, and the existing runtime determinism gate.
- Command or check: `cargo test -p engine --features vm-runtime-only,vm-runtime-opasm-artifact runtime_model -- --nocapture`
- Result: PASS; engine-side runtime bootstrap tests passed for shared artifact-path parity plus artifact miss, load, recovery, and invalidation behavior.
- Command or check: `cargo fmt --all`
- Result: PASS.
- Command or check: `cargo clippy --all-targets --all-features -- -D warnings`
- Result: PASS.
- Command or check: `cargo audit`
- Result: PASS with the existing allowed `registry` unmaintained warning (`RUSTSEC-2025-0026`).
- Command or check: `cargo test --workspace`
- Result: PASS; the full workspace unit, integration, and doc-test suites completed without failures.

## Closure Status

- Status: `fixed`
- Residual risk: low; the bootstrap policy now lives behind one shared helper, and the remaining crate-local code is limited to caller-specific gating and engine cache behavior rather than duplicated artifact/bootstrap policy.

## Notes

- The new `vm::runtime_bootstrap` module owns default artifact-path resolution, package-byte loading, artifact loading, and fallback persistence.
- `asm::runtime_model` and `engine` now delegate through that shared helper instead of carrying separate artifact/bootstrap implementations.
- Focused parity assertions now bind the crate-local callers back to the shared helper so future drift is caught at the seam rather than after the two crates diverge.