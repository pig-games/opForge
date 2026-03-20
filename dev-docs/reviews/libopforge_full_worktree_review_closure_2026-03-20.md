# Finding Closure Report

## Finding

- ID: `RVW-2026-03-20-001`
- Original summary: The FFI layer relied on large amounts of handwritten boilerplate for tokenize-report accessors, repeated report accessors, and paired high-level assemble/check entrypoints, making ABI-surface evolution drift-prone.

## Claimed Fix

- Plan item: `Item 2` and `Item 3` in `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Implementation slice or commit: `1e7beab` (`Refactor shared tokenize-report ffi accessors`) and `0dd5552` (`Extend shared ffi report helper rollout`)
- Changed files: `crates/opforge-ffi/src/lib.rs`, `crates/opforge-ffi/tests/abi_contract.rs`, `crates/opforge-ffi/tests/release_panic_boundary.rs`

## Validation Evidence

- Command or check: `cargo test -q -p ffi`
- Result: PASS; current HEAD rerun passed the `ffi` crate suite, including the migrated accessor and release-profile panic-boundary coverage (`55` crate tests plus the `4` release-panic-boundary tests).
- Command or check: `make test`
- Result: PASS; current HEAD full test rerun completed successfully, including the integrated FFI consumers.
- Command or check: `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`
- Result: PASS for clippy; `cargo audit` reported only the accepted baseline warning `RUSTSEC-2025-0026` for `registry`.

## Closure Status

- Status: fixed
- Residual risk: No finding-specific residual risk remains.

## Notes

- The March 20 remediation first proved the helper pattern on the tokenize-report family, then extended the same internal generation layer to the remaining targeted report accessors and paired assemble/check entrypoints called out in the review.

---

## Finding

- ID: `RVW-2026-03-20-002`
- Original summary: `AssemblerBuilder<'a>` and `AssemblerSessionBuilder` duplicated the same fluent builder surface and behavior instead of routing through one shared internal mutation layer.

## Claimed Fix

- Plan item: `Item 1` in `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Implementation slice or commit: `fd6f1e6` (`Refactor shared libopforge builder mutations`)
- Changed files: `crates/opforge-lib/src/lib.rs`, `crates/opforge-lsp/tests/lsp_client_integration.rs`

## Validation Evidence

- Command or check: `cargo test -q -p libopforge`
- Result: PASS; current HEAD rerun passed all `64` `libopforge` tests, including the regression coverage for borrowed and owned builder paths.
- Command or check: `cargo clippy --workspace --all-targets -- -D warnings`
- Result: PASS; the workspace clippy rerun is clean, including the previously noted `crates/opforge-lsp/tests/lsp_client_integration.rs` blocker that Item 1 had to clear.
- Command or check: `make test`
- Result: PASS; current HEAD full test rerun completed successfully.

## Closure Status

- Status: fixed
- Residual risk: No finding-specific residual risk remains.

## Notes

- The borrowed and owned high-level builders now act as thin frontends over one private config-mutating implementation path, matching the review’s requested direction without changing the public API contract.

---

## Finding

- ID: `RVW-2026-03-20-003`
- Original summary: The workflow error wrappers duplicated the same detail storage and identical impl blocks across `InvalidArgumentError`, `InvalidRequestError`, `HostIoError`, and `InternalErrorReport`.

## Claimed Fix

- Plan item: `Item 4` in `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Implementation slice or commit: `77b3c55` (`Share libopforge workflow error details`)
- Changed files: `crates/opforge-lib/src/lib.rs`

## Validation Evidence

- Command or check: `cargo test -q -p libopforge`
- Result: PASS; current HEAD rerun passed all `64` `libopforge` tests, including the focused workflow-error regression coverage added in the remediation slice.
- Command or check: `make test`
- Result: PASS; current HEAD full test rerun completed successfully.

## Closure Status

- Status: fixed
- Residual risk: No finding-specific residual risk remains.

## Notes

- The public wrapper types keep their existing constructors, accessors, `Display`, and `Error` behavior while reusing one shared private detail implementation.

---

## Finding

- ID: `RVW-2026-03-20-004`
- Original summary: The CLI build-profile banner strings were authored three times across `BUILD_PROFILE_SUMMARY`, `LONG_VERSION`, and `HELP_BUILD_PROFILE` instead of deriving the longer strings from one canonical profile label.

## Claimed Fix

- Plan item: `Item 5` in `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Implementation slice or commit: `e6d2b4a` (`Canonicalize cli build profile strings`)
- Changed files: `crates/opforge-cli-core/src/cli.rs`

## Validation Evidence

- Command or check: `cargo test -q -p cli-core`
- Result: PASS; current HEAD rerun passed all `56` library tests and `22` additional package tests for `cli-core`, covering the unchanged build-profile banners.
- Command or check: `make test`
- Result: PASS; current HEAD full test rerun completed successfully.

## Closure Status

- Status: fixed
- Residual risk: No finding-specific residual risk remains.

## Notes

- The cfg matrix is now authored once and reused to derive the longer version/help strings, removing the exact drift trap called out by the review while keeping the emitted text unchanged.

---

## Finding

- ID: `RVW-2026-03-20-005`
- Original summary: `handle_did_open` and `handle_did_change` duplicated the document upsert, derived-state refresh, workspace-index refresh, and validation dispatch flow.

## Claimed Fix

- Plan item: `Item 6` in `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Implementation slice or commit: `223ffda` (`Share LSP open/change document upsert flow`)
- Changed files: `crates/opforge-lsp/src/session.rs`, `crates/opforge-lsp/tests/lsp_client_integration.rs`, `crates/opforge-ffi/tests/release_panic_boundary.rs`

## Validation Evidence

- Command or check: `cargo test -q -p lsp`
- Result: PASS; current HEAD rerun passed the `lsp` test suites, including the integration regression that proves `didChange` refreshes open-document symbols without forcing a rooted rebuild.
- Command or check: `make test`
- Result: PASS; current HEAD full test rerun completed successfully, including `did_change_refreshes_open_document_symbols_without_rooted_rebuild`.
- Command or check: `cargo clippy --workspace --all-targets -- -D warnings`
- Result: PASS; the workspace clippy rerun is clean after the LSP-slice follow-up work.

## Closure Status

- Status: fixed
- Residual risk: No finding-specific residual risk remains. The extra `crates/opforge-ffi/tests/release_panic_boundary.rs` change in the same commit serialized an existing parent-test race discovered during required validation and does not reopen the LSP duplication issue.

## Notes

- The notification-specific parameter decoding remains local, but both open/change notifications now share one document upsert path as requested by the review.

---

## Workflow Gate Evidence

- Review source: `documentation/reviews/full-worktree-code-review-2026-03-20.md`
- Plan source: `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Current focused reruns: PASS via `cargo test -q -p libopforge`, `cargo test -q -p ffi`, `cargo test -q -p cli-core`, and `cargo test -q -p lsp` on current `HEAD`
- Current broader reruns: PASS via `cargo fmt --all --check`, `cargo clippy --workspace --all-targets -- -D warnings`, `cargo audit` with accepted baseline warning `RUSTSEC-2025-0026`, and `make test` on current `HEAD`# Finding Closure Report

## Finding

- ID: `RVW-2026-03-20-001`
- Original summary: The FFI layer relied on large amounts of handwritten boilerplate for tokenize-report accessors, repeated report accessors, and paired high-level assemble/check entrypoints, making ABI-surface evolution drift-prone.

## Claimed Fix

- Plan item: `Item 2` and `Item 3` in `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Implementation slice or commit: `1e7beab` (`Refactor shared tokenize-report ffi accessors`) and `0dd5552` (`Extend shared ffi report helper rollout`)
- Changed files: `crates/opforge-ffi/src/lib.rs`, `crates/opforge-ffi/tests/abi_contract.rs`, `crates/opforge-ffi/tests/release_panic_boundary.rs`

## Validation Evidence

- Command or check: `cargo test -q -p ffi`
- Result: PASS; current HEAD rerun passed the `ffi` crate suite, including the migrated accessor and release-profile panic-boundary coverage (`55` crate tests plus the `4` release-panic-boundary tests).
- Command or check: `make test`
- Result: PASS; current HEAD full test rerun completed successfully, including the integrated FFI consumers.
- Command or check: `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`
- Result: PASS for clippy; `cargo audit` reported only the accepted baseline warning `RUSTSEC-2025-0026` for `registry`.

## Closure Status

- Status: fixed
- Residual risk: No finding-specific residual risk remains. A fresh chained `cargo fmt --all --check && ...` gate is currently blocked by unrelated rustfmt drift in `crates/opforge-asm/src/tests.rs`, outside this remediation slice.

## Notes

- The March 20 remediation first proved the helper pattern on the tokenize-report family, then extended the same internal generation layer to the remaining targeted report accessors and paired assemble/check entrypoints called out in the review.

---

## Finding

- ID: `RVW-2026-03-20-002`
- Original summary: `AssemblerBuilder<'a>` and `AssemblerSessionBuilder` duplicated the same fluent builder surface and behavior instead of routing through one shared internal mutation layer.

## Claimed Fix

- Plan item: `Item 1` in `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Implementation slice or commit: `fd6f1e6` (`Refactor shared libopforge builder mutations`)
- Changed files: `crates/opforge-lib/src/lib.rs`, `crates/opforge-lsp/tests/lsp_client_integration.rs`

## Validation Evidence

- Command or check: `cargo test -q -p libopforge`
- Result: PASS; current HEAD rerun passed all `64` `libopforge` tests, including the regression coverage for borrowed and owned builder paths.
- Command or check: `cargo clippy --workspace --all-targets -- -D warnings`
- Result: PASS; the workspace clippy rerun is clean, including the previously noted `crates/opforge-lsp/tests/lsp_client_integration.rs` blocker that Item 1 had to clear.
- Command or check: `make test`
- Result: PASS; current HEAD full test rerun completed successfully.

## Closure Status

- Status: fixed
- Residual risk: No finding-specific residual risk remains. Current chained full-gate replay is blocked only by unrelated rustfmt drift outside the builder slice.

## Notes

- The borrowed and owned high-level builders now act as thin frontends over one private config-mutating implementation path, matching the review’s requested direction without changing the public API contract.

---

## Finding

- ID: `RVW-2026-03-20-003`
- Original summary: The workflow error wrappers duplicated the same detail storage and identical impl blocks across `InvalidArgumentError`, `InvalidRequestError`, `HostIoError`, and `InternalErrorReport`.

## Claimed Fix

- Plan item: `Item 4` in `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Implementation slice or commit: `77b3c55` (`Share libopforge workflow error details`)
- Changed files: `crates/opforge-lib/src/lib.rs`

## Validation Evidence

- Command or check: `cargo test -q -p libopforge`
- Result: PASS; current HEAD rerun passed all `64` `libopforge` tests, including the focused workflow-error regression coverage added in the remediation slice.
- Command or check: `make test`
- Result: PASS; current HEAD full test rerun completed successfully.

## Closure Status

- Status: fixed
- Residual risk: No finding-specific residual risk remains.

## Notes

- The public wrapper types keep their existing constructors, accessors, `Display`, and `Error` behavior while reusing one shared private detail implementation.

---

## Finding

- ID: `RVW-2026-03-20-004`
- Original summary: The CLI build-profile banner strings were authored three times across `BUILD_PROFILE_SUMMARY`, `LONG_VERSION`, and `HELP_BUILD_PROFILE` instead of deriving the longer strings from one canonical profile label.

## Claimed Fix

- Plan item: `Item 5` in `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Implementation slice or commit: `e6d2b4a` (`Canonicalize cli build profile strings`)
- Changed files: `crates/opforge-cli-core/src/cli.rs`

## Validation Evidence

- Command or check: `cargo test -q -p cli-core`
- Result: PASS; current HEAD rerun passed all `56` library tests and `22` additional package tests for `cli-core`, covering the unchanged build-profile banners.
- Command or check: `make test`
- Result: PASS; current HEAD full test rerun completed successfully.

## Closure Status

- Status: fixed
- Residual risk: No finding-specific residual risk remains.

## Notes

- The cfg matrix is now authored once and reused to derive the longer version/help strings, removing the exact drift trap called out by the review while keeping the emitted text unchanged.

---

## Finding

- ID: `RVW-2026-03-20-005`
- Original summary: `handle_did_open` and `handle_did_change` duplicated the document upsert, derived-state refresh, workspace-index refresh, and validation dispatch flow.

## Claimed Fix

- Plan item: `Item 6` in `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Implementation slice or commit: `223ffda` (`Share LSP open/change document upsert flow`)
- Changed files: `crates/opforge-lsp/src/session.rs`, `crates/opforge-lsp/tests/lsp_client_integration.rs`, `crates/opforge-ffi/tests/release_panic_boundary.rs`

## Validation Evidence

- Command or check: `cargo test -q -p lsp`
- Result: PASS; current HEAD rerun passed the `lsp` test suites, including the integration regression that proves `didChange` refreshes open-document symbols without forcing a rooted rebuild.
- Command or check: `make test`
- Result: PASS; current HEAD full test rerun completed successfully, including `did_change_refreshes_open_document_symbols_without_rooted_rebuild`.
- Command or check: `cargo clippy --workspace --all-targets -- -D warnings`
- Result: PASS; the workspace clippy rerun is clean after the LSP-slice follow-up work.

## Closure Status

- Status: fixed
- Residual risk: No finding-specific residual risk remains. The extra `crates/opforge-ffi/tests/release_panic_boundary.rs` change in the same commit serialized an existing parent-test race discovered during required validation and does not reopen the LSP duplication issue.

## Notes

- The notification-specific parameter decoding remains local, but both open/change notifications now share one document upsert path as requested by the review.

---

## Workflow Gate Evidence

- Review source: `documentation/reviews/full-worktree-code-review-2026-03-20.md`
- Plan source: `documentation/libopforge-full-worktree-review-remediation-plan-v0_1.md`
- Current focused reruns: PASS via `cargo test -q -p libopforge`, `cargo test -q -p ffi`, `cargo test -q -p cli-core`, and `cargo test -q -p lsp` on current `HEAD`
- Current broader reruns: PASS via `cargo clippy --workspace --all-targets -- -D warnings`, `cargo audit` with accepted baseline warning `RUSTSEC-2025-0026`, and `make test` on current `HEAD`
- Current chained gate replay note: `cargo fmt --all --check && cargo clippy --workspace --all-targets -- -D warnings && cargo audit && make test` stops at `cargo fmt --all --check` because of unrelated rustfmt drift in `crates/opforge-asm/src/tests.rs`; this drift is outside RVW-2026-03-20-001 through RVW-2026-03-20-005 and does not reproduce any of the March 20 review findings