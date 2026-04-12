# Finding Closure Report

## Finding

- ID: `RVW-2026-04-11-004`
- Original summary: The LSP layer duplicated `Content-Length` framing logic between the production stdio protocol path and the integration test client, so framing changes could drift between the two implementations.

## Claimed Fix

- Plan item: Work item 13 - share one LSP framing implementation between production and the integration client.
- Implementation slice or commit: pre-commit Work item 13 remediation slice on `main`
- Changed files:
  - `crates/opforge-lsp/src/framing.rs`
  - `crates/opforge-lsp/src/lib.rs`
  - `crates/opforge-lsp/src/protocol.rs`
  - `crates/opforge-lsp/tests/common/lsp_client.rs`

## Validation Evidence

- Command or check: `cargo test -p lsp protocol -- --nocapture`
- Result: PASS; the focused protocol suite validated shared header parsing, missing-header rejection, oversize rejection, JSON decode rejection, and frame writing through the extracted helper.
- Command or check: `cargo test -p lsp -- --nocapture`
- Result: PASS; the full LSP crate suite, including the integration client tests, stayed green after both production and test-client framing paths were rewired to the shared module.
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
- Residual risk: low; the server protocol path and the integration client now share one framing implementation, so future framing-rule changes have one source of truth and production-side tests that directly exercise it.

## Notes

- The new `crates/opforge-lsp/src/framing.rs` module now owns the shared message-size cap plus the `read_lsp_message` and `write_lsp_message` framing helpers.
- `crates/opforge-lsp/src/protocol.rs` now uses that helper for production stdio framing, and its protocol tests still cover the shared seam directly, including frame writing.
- `crates/opforge-lsp/tests/common/lsp_client.rs` now imports the same helper for integration-client reads and writes, removing the stale local framing copy that the review flagged.