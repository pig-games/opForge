# libopforge LSP review remediation plan - 2026-03-17

## Metadata

- Source: `dev-docs/reviews/libopforge_full_review_2026-03-17.md` at commit `93a0c11d2a6b0c853ae0223e423d177511a7f457`
- Mode: `remediation`
- Owner: GitHub Copilot

## Objective

Close review findings `RVW-2026-03-17-001` through `RVW-2026-03-17-003` without widening scope beyond the reviewed `opforge-lsp` validation and indexing defects. The work must harden overlay validation against broad or symlinked filesystem traversal, refresh diagnostics when validation-relevant settings change, and preserve rooted workspace symbols when an editor tab closes.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at all times during plan execution.
- One active work item at a time; no overlapping implementation across items.
- Each work item ends in exactly one new commit before the next one begins.
- Scope is limited to `RVW-2026-03-17-001` through `RVW-2026-03-17-003`; no unrelated LSP cleanup, validator redesign, or source-graph refactoring.
- Prefer the smallest production-code slice that closes each finding, with focused LSP integration coverage added only where needed to prove the fix.
- No fixture or reference regeneration is allowed unless an intentional behavior change later proves it is required.
- The plan does not become active until the `Plan Quality Reviewer` returns `PASS` for this artifact against the active worktree `AGENTS.md` and the source review.
- Full quality gates and `plan-compliance-reviewer` `PASS` evidence are required before each commit.

## Work Items

- [x] Item 1: Constrain overlay root selection to the active project boundary.
  - Source requirement or finding IDs: `RVW-2026-03-17-001` (expected partial closure).
  - Expected files: `crates/opforge-lsp/src/session.rs` and `crates/opforge-lsp/tests/lsp_client_integration.rs` only.
  - Validation: add a focused integration test proving unrelated open documents cannot widen overlay rooting, then run `cargo test --locked -p lsp`.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the overlay-root slice.
  - Commit outcome: overlay creation chooses only a configured workspace root or a narrow active-file project root and never falls back to the common parent of unrelated open documents.
  - Definition of done: validation for an open file cannot escape into a broader ancestor solely because other files from different projects are open in the editor.

- [x] Item 2: Replace recursive overlay copying with minimal, symlink-safe staging.
  - Source requirement or finding IDs: `RVW-2026-03-17-001` (expected full closure, completing Item 1).
  - Expected files: `crates/opforge-lsp/src/session.rs`, `crates/opforge-lsp/src/validation_runner.rs` only if rebasing needs a narrow helper change, and `crates/opforge-lsp/tests/lsp_client_integration.rs`.
  - Validation: add focused tests for symlinked-directory refusal and for staging only the active, open, and dependency files needed by validation, then run `cargo test --locked -p lsp`.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the overlay-staging slice.
  - Commit outcome: validation overlays stop recursively copying entire trees, refuse or tightly bound symlink descent, and materialize only the files needed for the active validation run.
  - Definition of done: the reviewed denial-of-service and trust-boundary path is closed because overlay creation no longer performs whole-tree copies or follows symlinked directories.

- [x] Item 3: Refresh validation state when configuration changes affect validator inputs.
  - Source requirement or finding IDs: `RVW-2026-03-17-002` (expected full closure).
  - Expected files: `crates/opforge-lsp/src/session.rs` and `crates/opforge-lsp/tests/lsp_client_integration.rs` only unless a narrow validation-runner helper change is strictly required.
  - Validation: add an integration test that changes `opforgeLsp.validation.onSave` after files are open and proves diagnostics refresh without another edit or save, relying on the shared implementation path that invalidates all validation generations for validator-input configuration changes, then run `cargo test --locked -p lsp`.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the configuration-refresh slice.
  - Commit outcome: `workspace/didChangeConfiguration` invalidates stale validation generations, refreshes stored diagnostic contributions as needed, and schedules fresh validation for open roots when validator-affecting settings change.
  - Definition of done: diagnostics shown after a validator-path, include-path, module-path, define, CPU, or validation-setting change always reflect the new configuration without requiring a follow-up edit or save.

- [x] Item 4: Rehydrate rooted on-disk symbols when a document closes.
  - Source requirement or finding IDs: `RVW-2026-03-17-003` (expected full closure).
  - Expected files: `crates/opforge-lsp/src/session.rs`, `crates/opforge-lsp/src/workspace_index.rs`, and `crates/opforge-lsp/tests/lsp_client_integration.rs` only.
  - Validation: add an integration test that opens a rooted file, closes it, and proves both `workspace/symbol` and `textDocument/definition` still resolve against the on-disk file, then run `cargo test --locked -p lsp`.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the didClose reindex slice.
  - Commit outcome: closing a file under a configured root reloads or rebuilds the rooted on-disk document state immediately so workspace navigation keeps seeing the file after the editor tab closes.
  - Definition of done: `didClose` no longer removes rooted symbols from workspace-wide symbol, definition, reference, or completion results while the file still exists on disk under a configured root.

- [x] Item 5: Finish closure evidence and plan bookkeeping for the reviewed findings.
  - Source requirement or finding IDs: `RVW-2026-03-17-001`, `RVW-2026-03-17-002`, and `RVW-2026-03-17-003` (expected closure confirmation only).
  - Expected files: this plan for checkbox updates, one finding-closure artifact for `dev-docs/reviews/libopforge_full_review_2026-03-17.md`, and `crates/opforge-lsp/src/session.rs` only if a formatter-only adjustment is required to satisfy the full quality gate on the split tip.
  - Validation: run `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/lsp/libopforge_lsp_review_remediation_plan_2026-03-17.md`, the full workspace quality gates, and the `Finding Closure Reviewer` plus final `plan-compliance-reviewer` gates.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: final `PASS` from `plan-compliance-reviewer` confirming the last active checkbox, changed files, validation evidence, and bookkeeping are consistent with the completed remediation state.
  - Commit outcome: the plan, closure report, and review traceability accurately reflect the final resolved state for the three findings.
  - Definition of done: each reviewed finding has explicit closure evidence, required workflow gates have passed, and the plan state matches the actual implemented and validated remediation work.

## Milestones

- [x] Milestone 1: Overlay rooting is constrained to the active project boundary (`Item 1` complete and committed).
- [x] Milestone 2: Overlay staging is minimal and symlink-safe (`Item 2` complete and committed).
- [x] Milestone 3: Validation refreshes immediately after configuration changes (`Item 3` complete and committed).
- [x] Milestone 4: Closing rooted files preserves workspace navigation results (`Item 4` complete and committed).
- [x] Milestone 5: Closure evidence and bookkeeping are complete (`Item 5` complete and committed).

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan execution before the `Plan Quality Reviewer` returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping