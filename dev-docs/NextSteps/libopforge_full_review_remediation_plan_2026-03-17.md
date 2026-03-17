# libopforge full review remediation plan - 2026-03-17

## Metadata

- Source: `dev-docs/reviews/libopforge_full_review_2026-03-17.md` at commit `1c233224974969089db41e4595687bf46cbbb4a7`
- Mode: `remediation`
- Owner: GitHub Copilot

## Objective

Close review findings `RVW-2026-03-17-004` and `RVW-2026-03-17-005`
without widening scope beyond the reviewed release-workflow integrity gap and
the `opforge-lsp` validation-overlay correctness gap. The work must make manual
release uploads build exactly the requested tag, preserve the release-ffi smoke
coverage already required for shipped artifacts, resolve validator config roots
from the real workspace, and stage include or module dependencies so editor
validation sees the same source set as normal assembly.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- One active work item at a time; no overlapping implementation across items.
- Each work item ends in exactly one new commit before the next one begins.
- Scope is limited to `RVW-2026-03-17-004` and `RVW-2026-03-17-005`; no
  unrelated release automation cleanup, workflow redesign, LSP indexing
  cleanup, or preprocessor refactoring.
- Prefer the smallest production-code slice that closes each finding, with
  focused regression coverage added only where needed to prove the fix.
- No fixture or reference regeneration is allowed unless an intentional
  behavior change later proves it is required.
- The plan does not become active until the `Plan Quality Reviewer` returns
  `PASS` for this artifact against the active worktree `AGENTS.md` and the
  source review.
- Full quality gates and `plan-compliance-reviewer` `PASS` evidence are
  required before each commit.

## Work Items

- [x] Item 1: Make manual release checkout resolve and use the requested tag.
  - Source requirement or finding IDs: `RVW-2026-03-17-004` (expected partial closure).
  - Expected files: `.github/workflows/release-binaries.yml` only.
  - Validation: run the shipped release smoke test with `cargo test --locked -p ffi release_profile_loads_and_assembles_smoke`, then verify the workflow diff shows `workflow_dispatch` uploads resolving and checking out `refs/tags/<tag>` before any build step and failing when the tag does not exist.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the checkout-integrity slice.
  - Commit outcome: manual upload runs derive a concrete tag ref, check out that tag before building, and stop immediately if the requested tag cannot be resolved.
  - Definition of done: no manual upload path can build the branch tip while publishing artifacts under a different free-form tag name.

- [x] Item 2: Derive packaged and uploaded release tags from the resolved checkout.
  - Source requirement or finding IDs: `RVW-2026-03-17-004` (expected full closure, completing Item 1).
  - Expected files: `.github/workflows/release-binaries.yml` and release-workflow documentation only if a narrow operator note is required by the changed workflow contract.
  - Validation: run `cargo test --locked -p ffi release_profile_loads_and_assembles_smoke`, then verify the workflow diff shows packaging and `softprops/action-gh-release` consuming the resolved checked-out tag rather than the raw dispatch input.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the release-tag-plumbing slice.
  - Commit outcome: release package naming and uploaded release assets are derived from the checked-out tag identity, so verify-only runs and upload runs cannot diverge on the source or published tag.
  - Definition of done: the reviewed release-integrity defect is closed because the workflow’s build source, package names, and uploaded tag all come from the same resolved tag ref.

- [ ] Item 3: Rebase validator config paths from the original workspace root.
  - Source requirement or finding IDs: `RVW-2026-03-17-005` (expected partial closure).
  - Expected files: `crates/opforge-lsp/src/validation_runner.rs`, `crates/opforge-lsp/src/session.rs`, and focused LSP integration tests only if needed for relative-root coverage.
  - Validation: add a focused regression test proving relative `includePaths` and `modulePaths` continue to resolve when validation runs from the temporary overlay, then run `cargo test --locked -p lsp`.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the config-root-rebasing slice.
  - Commit outcome: validator configuration passed to the CLI resolves relative include and module roots from the real workspace root instead of the temporary overlay root.
  - Definition of done: projects that assemble correctly from disk do not start failing editor validation solely because relative validator roots are rewritten into the overlay directory.

- [ ] Item 4: Stage ordinary include dependencies into the validation overlay.
  - Source requirement or finding IDs: `RVW-2026-03-17-005` (expected full closure, completing Item 3).
  - Expected files: `crates/opforge-lsp/src/session.rs`, `crates/opforge-lsp/src/workspace_index.rs`, `crates/opforge-lsp/src/validation_runner.rs` only if narrowly required for staging metadata flow, and `crates/opforge-lsp/tests/lsp_client_integration.rs`.
  - Validation: add focused regression coverage proving unopened files reached through ordinary `INCLUDE` directives and `.use` imports are staged into the overlay and validated successfully, then run `cargo test --locked -p lsp`.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the include-staging slice.
  - Commit outcome: overlay construction follows the same reachable dependency set as real assembly for both `.use` imports and ordinary include files required by the active validation target.
  - Definition of done: editor validation sees the same dependency files as disk assembly even when include dependencies are closed in the editor and live outside the temporary overlay root.

- [ ] Item 5: Capture closure evidence and finish remediation bookkeeping.
  - Source requirement or finding IDs: `RVW-2026-03-17-004` (expected full closure confirmation after Items 1-2) and `RVW-2026-03-17-005` (expected full closure confirmation after Items 3-4).
  - Expected files: this plan for checkbox updates and one finding-closure artifact for `dev-docs/reviews/libopforge_full_review_2026-03-17.md`.
  - Validation: run `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-17.md`, the full workspace quality gates, and the `Finding Closure Reviewer` plus final `plan-compliance-reviewer` gates.
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: final `PASS` from `plan-compliance-reviewer` confirming the last active checkbox, changed files, validation evidence, and bookkeeping are consistent with the completed remediation state.
  - Commit outcome: the plan, closure report, and review traceability accurately reflect the final resolved state for both findings.
  - Definition of done: each reviewed finding has explicit closure evidence, required workflow gates have passed, and the plan state matches the actual implemented and validated remediation work.

## Milestones

- [x] Milestone 1: Manual release uploads are pinned to a resolved tag checkout (`Item 1` complete and committed).
- [x] Milestone 2: Release packaging and upload tag selection derive from the resolved checkout (`Item 2` complete and committed).
- [ ] Milestone 3: Validator config roots remain workspace-correct inside the overlay (`Item 3` complete and committed).
- [ ] Milestone 4: Validation overlays stage ordinary include and module dependencies (`Item 4` complete and committed).
- [ ] Milestone 5: Closure evidence and bookkeeping are complete (`Item 5` complete and committed).

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan execution before the `Plan Quality Reviewer` returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping