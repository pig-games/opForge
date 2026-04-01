# Full Codebase Review Remediation Plan

## Metadata

- Source: `documentation/reviews/full-codebase-review-2026-04-01.md`
- Mode: remediation
- Owner: GPT-5.4 reviewer of record

## Objective

Fully remediate `RVW-2026-04-01-001`, `RVW-2026-04-01-002`, and `RVW-2026-04-01-003` from the full-worktree review without widening scope beyond the reviewed issues and their directly coupled validation updates.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at all times during plan execution.
- Scope is limited to the three recorded review findings plus the minimum directly coupled tests or workflow-document updates needed to validate each fix.
- Only one work item may be active at a time.
- Each work item must end in exactly one new commit before the next item begins.
- No plan-driven commit is allowed until all quality gates pass and `plan-compliance-reviewer` returns `PASS`.
- No finding may be marked fixed until `finding-closure-reviewer` returns `PASS` for the claimed closure.
- Do not introduce unrelated cleanup, refactors, or workflow expansion while executing this plan.

## Work Items

- [x] Item 1 - Restore span-aware invalid-source diagnostics in the m68020 FPU encoder.
  - Source requirement or finding IDs: `RVW-2026-04-01-001` — expected to fully close this finding.
  - Expected files: `crates/opforge-families/src/m68020/handler.rs`
  - Validation: add or update one focused inline regression test in `crates/opforge-families/src/m68020/handler.rs` that exercises an invalid source effective address for each affected encode path, then run `cargo test -p families m68020` and `cargo test --quiet`.
  - Full quality gates: targeted `families` regression coverage passes, workspace tests stay green, `plan-compliance-reviewer` returns `PASS` before commit, and `finding-closure-reviewer` returns `PASS` before the finding is marked fixed.
  - Plan-compliance review evidence: capture the item-specific `PASS` result together with the exact changed file list, validation commands, and updated checkbox state.
  - Commit outcome: one commit removes the unreachable generic-error branches and preserves only the span-aware diagnostic path for the affected FPU source validation cases.
  - Definition of done: invalid source effective addresses for the affected m68020 FPU forms now produce span-aware diagnostics, and the focused regression coverage fails before the fix and passes after it.

- [x] Item 2 - Reject duplicate canonical source paths in external-oracle manifests before fixture execution.
  - Source requirement or finding IDs: `RVW-2026-04-01-002` — expected to fully close this finding.
  - Expected files: `crates/opforge-asm/src/external_oracle.rs`
  - Validation: add one focused manifest-loading regression test in `crates/opforge-asm/src/external_oracle.rs` covering two fixtures that resolve to the same canonical source path, then run `cargo test -p asm external_oracle_` and `cargo test --quiet`.
  - Full quality gates: targeted `asm` external-oracle coverage passes, workspace tests stay green, `plan-compliance-reviewer` returns `PASS` before commit, and `finding-closure-reviewer` returns `PASS` before the finding is marked fixed.
  - Plan-compliance review evidence: capture the item-specific `PASS` result together with the exact changed file list, validation commands, and updated checkbox state.
  - Commit outcome: one commit adds canonical-source-path duplicate rejection during manifest validation so conflicting suites fail before any sidecar refresh or fixture dispatch occurs.
  - Definition of done: manifests that reuse the same canonical fixture source path fail deterministically during validation, and same-source fixtures can no longer overwrite each other's sidecar reports because those manifests are rejected up front.

- [x] Item 3 - Replace contributor-specific absolute references in `AGENTS.md` with repository-relative paths.
  - Source requirement or finding IDs: `RVW-2026-04-01-003` — expected to fully close this finding.
  - Expected files: `AGENTS.md`
  - Validation: update the referenced paths, run `rg -n '/Users/erik/(\\.codex/worktrees/7175/opForge|Code/Retro/opForge)' AGENTS.md` expecting no matches, and spot-check that every changed relative link target exists in the current worktree.
  - Full quality gates: the `AGENTS.md` path audit is clean, `plan-compliance-reviewer` returns `PASS` before commit, and `finding-closure-reviewer` returns `PASS` before the finding is marked fixed.
  - Plan-compliance review evidence: capture the item-specific `PASS` result together with the exact changed file list, validation commands, and updated checkbox state.
  - Commit outcome: one commit converts the broken absolute workflow references in `AGENTS.md` to portable repository-relative paths without changing unrelated workflow rules.
  - Definition of done: `AGENTS.md` contains no contributor-specific absolute repository paths, and every updated reference resolves within this worktree.

## Milestones

- [x] Milestone 1 - `RVW-2026-04-01-001` is closed with focused regression coverage and a dedicated commit.
- [ ] Milestone 2 - `RVW-2026-04-01-002` and `RVW-2026-04-01-003` are closed in their own commit-sized slices with closure evidence recorded for each finding.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- no finding may be marked fixed before `finding-closure-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
