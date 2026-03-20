# libopforge Full Worktree Review Remediation Plan v0.1

## Metadata

- Source: `documentation/reviews/full-worktree-code-review-2026-03-20.md`
- Mode: remediation
- Owner: active implementation agent for `feature/libopforge-lib`

## Objective

Land the maintainability remediations called out in the full worktree review
without widening scope beyond RVW-2026-03-20-001 through RVW-2026-03-20-005 or
changing public behavior.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- One work item is active at a time and each item must end in exactly one new
  commit.
- The plan stays inside RVW-2026-03-20-001 through RVW-2026-03-20-005; no
  unrelated cleanup or public API redesign is allowed.
- Public Rust API behavior, C ABI behavior, diagnostics, and output strings must
  remain unchanged unless a work item explicitly calls for a compatibility-safe
  internal refactor.
- Reference outputs or host-facing docs should change only if a planned refactor
  forces a user-visible wording update.

## Work Items

- [x] Item 1: Extract a shared internal mutation layer for borrowed and owned high-level builders
  - Source requirement or finding IDs: RVW-2026-03-20-002 (expected full closure)
  - Expected files: `crates/opforge-lib/src/lib.rs`; `crates/opforge-lsp/tests/lsp_client_integration.rs` for the required validation-unblocking clippy fix discovered during full-gate execution
  - Validation: `cargo test -q -p libopforge`; confirm borrowed and owned builder paths still cover `build()`, `prepare()`, `assemble()`, and `check()`; full gates passed after fixing the existing `manual_contains` clippy blocker in `crates/opforge-lsp/tests/lsp_client_integration.rs`
  - Full quality gates: `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the shared builder layer, touched file, and full gate evidence before commit
  - Commit outcome: planned: one commit that makes borrowed and owned builders thin frontends over one private config-mutating implementation
  - Definition of done: duplicated builder setter behavior is routed through one internal implementation path while public borrowed and owned APIs stay behaviorally identical

- [x] Item 2: Introduce a private FFI helper layer and migrate one cohesive tokenize-report family
  - Source requirement or finding IDs: RVW-2026-03-20-001 (expected partial closure)
  - Expected files: `crates/opforge-ffi/src/lib.rs`; `crates/opforge-ffi/tests/abi_contract.rs`
  - Validation: `cargo test -q -p ffi`; verify the migrated tokenize-report family keeps the same null handling, status values, and exported signatures
  - Full quality gates: `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the new helper layer, migrated report family, and full gate evidence before commit
  - Commit outcome: planned: one commit that proves the generation/helper pattern on `opforge_opcore_*` and `opforge_opasm_*` tokenize-report wrappers
  - Definition of done: one cohesive FFI wrapper family is derived from shared internal helpers or macros, the ABI stays unchanged, and RVW-2026-03-20-001 is partially closed with a reusable pattern in place

- [x] Item 3: Extend the FFI helper layer to the remaining targeted report accessors and paired high-level entrypoints
  - Source requirement or finding IDs: RVW-2026-03-20-001 (expected full closure)
  - Expected files: `crates/opforge-ffi/src/lib.rs`; `crates/opforge-ffi/tests/abi_contract.rs`; `crates/opforge-ffi/tests/release_panic_boundary.rs`
  - Validation: `cargo test -q -p ffi`; verify assembled/check entrypoints and migrated accessor groups preserve panic boundaries, invalid-request behavior, and ownership rules
  - Full quality gates: `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the expanded helper adoption, touched tests, and full gate evidence before commit
  - Commit outcome: planned: one commit that rolls the helper layer out to the repeated report accessor and paired assemble/check clusters called out by the review
  - Definition of done: the targeted repetitive FFI accessor and paired entrypoint clusters are derived from one shared internal layer, handwritten duplication is materially reduced, and RVW-2026-03-20-001 is fully closed

- [x] Item 4: Replace repeated workflow error wrapper storage and impls with one private shared detail type
  - Source requirement or finding IDs: RVW-2026-03-20-003 (expected full closure)
  - Expected files: `crates/opforge-lib/src/lib.rs`
  - Validation: `cargo test -q -p libopforge`; verify the public workflow error variants still expose the same codes, summaries, and trait behavior
  - Full quality gates: `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the shared detail type or macro, touched file, and full gate evidence before commit
  - Commit outcome: planned: one commit that removes four near-identical wrapper impl blocks without changing the public error types
  - Definition of done: `InvalidArgumentError`, `InvalidRequestError`, `HostIoError`, and `InternalErrorReport` reuse one private implementation path while their public behavior remains unchanged

- [ ] Item 5: Canonicalize build-profile strings in CLI core
  - Source requirement or finding IDs: RVW-2026-03-20-004 (expected full closure)
  - Expected files: `crates/opforge-cli-core/src/cli.rs`
  - Validation: `cargo test -q -p cli-core`; verify `BUILD_PROFILE_SUMMARY`, `LONG_VERSION`, and `HELP_BUILD_PROFILE` still report the same strings for each cfg case
  - Full quality gates: `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the single-source profile matrix, touched file, and full gate evidence before commit
  - Commit outcome: planned: one commit that defines each cfg-specific profile label once and derives the longer banner/help strings from it
  - Definition of done: the cfg matrix for build-profile text is authored once, duplicate literal maintenance is removed, and CLI output stays unchanged

- [ ] Item 6: Extract shared document upsert flow for LSP open/change notifications
  - Source requirement or finding IDs: RVW-2026-03-20-005 (expected full closure)
  - Expected files: `crates/opforge-lsp/src/session.rs`; `crates/opforge-lsp/tests/lsp_client_integration.rs`
  - Validation: `cargo test -q -p lsp`; verify `didOpen` and `didChange` still refresh derived state, update the workspace index, and preserve force-validate behavior
  - Full quality gates: `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the shared upsert helper, touched files, and full gate evidence before commit
  - Commit outcome: planned: one commit that moves common document-state construction and refresh logic behind one helper used by both notification handlers
  - Definition of done: `handle_did_open` and `handle_did_change` share one document upsert path, notification-specific parameter decoding stays local, and behavior remains unchanged

## Milestones

- [x] Milestone 1: public `opforge-lib` facade duplication is reduced by completing Items 1 and 4
- [ ] Milestone 2: FFI repetition is reduced through the shared helper rollout in Items 2 and 3
- [ ] Milestone 3: tooling-layer duplication is reduced by completing Items 5 and 6

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
