# Finding Closure Report

## Finding

- ID: `RVW-2026-04-11-006`
- Original summary: The FFI boundary manually mirrored the portable syntax model in several places, including expression-tree flattening in `push_expr`, token and expression text reconstruction, and token-kind remapping.

## Claimed Fix

- Plan item: Work item 10 - add a portable-schema adapter layer inside the FFI crate.
- Implementation slice or commit: pre-commit Work item 10 remediation slice on `main`
- Changed files:
  - `crates/opforge-ffi/src/lib.rs`
  - `crates/opforge-ffi/src/portable_adapter.rs`

## Validation Evidence

- Command or check: `cargo test -p ffi ffi_opforge_opcore_expr_group_ -- --nocapture`
- Result: PASS; the focused expression-group slice validated portable node-kind, text, span, and child-edge parity through the FFI surface.
- Command or check: `cargo test -p ffi ffi_opforge_opcore_tokenize_group_ -- --nocapture`
- Result: PASS; the focused tokenize-group slice validated token-kind, token-text, and span parity through the FFI surface.
- Command or check: `cargo fmt --all`
- Result: PASS.
- Command or check: `cargo clippy --all-targets --all-features -- -D warnings`
- Result: PASS.
- Command or check: `cargo audit`
- Result: PASS with the existing allowed warnings for `registry` unmaintained (`RUSTSEC-2025-0026`) and `rand` via `proptest` (`RUSTSEC-2026-0097`).
- Command or check: `cargo test --workspace`
- Result: PASS; the full workspace unit, integration, and doc-test suites completed without failures.

## Closure Status

- Status: `fixed`
- Residual risk: low; portable token and expression surface mapping now live behind one internal adapter, so future portable schema changes have one FFI seam to update and focused parity tests to catch drift.

## Notes

- The new `portable_adapter` module now owns portable token-kind mapping, token text rendering, expression node metadata, expression display text, and expression-child traversal for the FFI crate.
- `crates/opforge-ffi/src/lib.rs` now routes `push_expr`, `portable_token_text`, `portable_expr_text`, and `map_portable_token_kind` through that adapter without changing the public ABI or report layout.
- The new `ffi_opforge_opcore_expr_group_` and `ffi_opforge_opcore_tokenize_group_` parity tests lock the adapter-backed projection to the existing FFI boundary behavior.