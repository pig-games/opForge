# libopforge full review remediation plan - 2026-03-16

## Metadata

- Source: `dev-docs/reviews/libopforge_full_review_2026-03-16.md` at commit `ac20a05`
- Mode: remediation
- Owner: Codex

## Objective

Close the current full-review findings `RVW-2026-03-16-001` through
`RVW-2026-03-16-004` without widening scope beyond the reviewed issues. The
plan focuses on the LSP path and validation defects plus the source-graph
bootstrap defect identified in the latest full-worktree review.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- One active work item at a time; no overlapping implementation across items.
- Each work item ends in exactly one new commit before the next item starts.
- Scope is limited to `RVW-2026-03-16-001` through `RVW-2026-03-16-004`; do not
  widen into unrelated LSP cleanup, URI refactors, or module-graph redesign.
- Prefer the smallest vertical remediation slice that fully closes each
  finding, with focused regression coverage added only where needed to prove the
  fix.
- Full quality gates and `plan-compliance-reviewer` `PASS` evidence are
  required before each commit.

## Work Items

- [x] Item 1: Make LSP file URI conversion platform-correct.
  - Source requirement or finding IDs: `RVW-2026-03-16-001` (expected full closure).
  - Expected files: `crates/opforge-lsp/src/session.rs` and `crates/opforge-lsp/tests/lsp_client_integration.rs` or nearby unit tests only if narrowly required for Windows and UNC path coverage.
  - Validation: focused tests for URI round-trip and request-path behavior, including Windows drive-letter and UNC cases; `cargo test --locked -p lsp`.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the URI-conversion slice.
  - Commit outcome: LSP file-URI parsing and formatting preserve Windows drive letters and UNC authorities for all file-backed request paths.
  - Definition of done: the broken ad-hoc URI conversion path is removed or corrected, focused coverage proves the fix on Windows-shaped inputs, and no unrelated LSP behavior is changed.

- [x] Item 2: Re-root LSP overlays and relative module/include resolution to workspace scope.
  - Source requirement or finding IDs: `RVW-2026-03-16-002` (expected full closure).
  - Expected files: `crates/opforge-lsp/src/session.rs`, `crates/opforge-lsp/src/validation_runner.rs`, `crates/opforge-lsp/src/workspace_index.rs`, and focused LSP integration tests only where needed for sibling-root and relative-path coverage.
  - Validation: focused tests for validation overlays with sibling directories and relative `module_paths` or `include_paths`; `cargo test --locked -p lsp`; any new targeted LSP integration case added for overlay remapping and definition resolution.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the overlay-rooting slice.
  - Commit outcome: validation overlays, CLI invocation paths, and module-target lookup all operate from a consistent workspace or configured root instead of the active file's parent directory.
  - Definition of done: unsaved sibling edits and relative module or include roots are visible to validation and definition flows, and the fix is proven with focused regression coverage.

- [x] Item 3: Stop dropping the newest LSP validation request under concurrency pressure.
  - Source requirement or finding IDs: `RVW-2026-03-16-003` (expected full closure).
  - Expected files: `crates/opforge-lsp/src/session.rs` and `crates/opforge-lsp/tests/lsp_client_integration.rs` only if needed for backpressure coverage.
  - Validation: focused validation-backpressure test that saturates the concurrency cap and proves the latest version is eventually validated; `cargo test --locked -p lsp --test lsp_client_integration overlapping_validations_publish_only_newest_version_results`; `cargo test --locked -p lsp`.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the rescheduling slice.
  - Commit outcome: validation work that arrives while the worker cap is saturated is queued or marked pending and is replayed automatically once capacity returns.
  - Definition of done: the latest document version is never left permanently unvalidated because of the concurrency cap, and regression coverage proves the new replay behavior.

- [x] Item 4: Make root-module and import bootstrap discovery conditional-aware.
  - Source requirement or finding IDs: `RVW-2026-03-16-004` (expected full closure).
  - Expected files: `crates/opforge-engine/src/source_graph.rs`, `crates/opforge-engine/src/lib.rs`, and focused source-graph or engine tests only where needed for inactive-conditional coverage; `crates/opforge-asm/src/asmline_directives_metadata.rs` only if a narrow root-module contract adjustment is required.
  - Validation: focused tests for inactive `.if` branches containing `.module` or `.use`, multi-module expanded roots, and root-only `.meta` behavior after bootstrap selection; `cargo test --locked -p engine`; `cargo test --locked -p asm`.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the conditional-aware bootstrap slice.
  - Commit outcome: root-module identification and dependency discovery are derived from the active root-module context instead of the raw expanded line list.
  - Definition of done: dead-branch `.module` and `.use` directives can no longer perturb bootstrap selection, and focused tests prove the source-graph behavior matches assembler conditional semantics.

- [x] Item 5: Capture closure evidence and finish remediation bookkeeping.
  - Source requirement or finding IDs: `RVW-2026-03-16-001` through `RVW-2026-03-16-004` (expected closure confirmation only).
  - Expected files: this plan for checkbox updates and one or more finding-closure artifacts required by the active workflow for the resolved findings.
  - Validation: `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-16.md`; full workspace quality gates; `finding-closure-reviewer` `PASS` for the closure artifact set.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: final `PASS` from `plan-compliance-reviewer` confirming the active checkbox, changed files, validation evidence, and progress bookkeeping are consistent with the completed remediation state.
  - Commit outcome: the plan, closure artifacts, and review traceability all reflect the final resolved state for the four findings.
  - Definition of done: each reviewed finding has closure evidence, required workflow gates have passed, and plan bookkeeping matches the actual completed remediation work.

## Milestones

- [x] Milestone 1: LSP file URI handling is correct for Windows and UNC paths (`Item 1` complete and committed).
- [x] Milestone 2: LSP overlay validation and module-target resolution are workspace-root correct (`Item 2` complete and committed).
- [x] Milestone 3: LSP validation always converges on the newest document version under load (`Item 3` complete and committed).
- [x] Milestone 4: Source-graph bootstrap obeys active conditional semantics for module and import discovery (`Item 4` complete and committed).
- [x] Milestone 5: Closure artifacts and plan bookkeeping are complete (`Item 5` complete and committed).

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- no finding may be marked fixed before `finding-closure-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping

## Execution Notes

- 2026-03-16: Implemented `RVW-2026-03-16-001` through
  `RVW-2026-03-16-004` in the LSP and engine crates with focused regression
  coverage for Windows drive-letter URIs, UNC URIs, workspace-rooted overlay
  validation, relative module-path rebasing, validation replay after
  concurrency backpressure, inactive conditional `.use` handling, and root-only
  bootstrap scanning.
- 2026-03-16 validation:
  - `cargo fmt --all --check`
  - `cargo clippy -- -D warnings`
  - `cargo audit`
  - `cargo test --locked -p lsp`
  - `cargo test --locked -p engine`
  - `cargo test --locked`
- Remaining workflow work:
  - None. The remediation slice now includes the closure artifact, the
    reviewer-pass evidence, and the final bookkeeping needed for commit
    closure.
