# opForge Motorola 68000 Family 68080 PRM Remediation Plan (v0.1)

## Metadata

- Source: review report at `dev-docs/reviews/opforge_68080_prm_validation_2026-04-07.md`
- Mode: remediation
- Owner: GitHub Copilot (GPT-5.4)
- Status: execution in progress; WI-1 has passed `plan-compliance-reviewer` and landed as its own commit, and WI-2 through WI-4 are being reconstructed from the saved pre-split diff into one-slice commit states

## Objective

Bring the live 68080 assembler behavior back into conformance with the latest
official PRM for the four remaining review findings: AMMX Apollo over-gating,
default `MOVIW` encoding/gating, integrated 68080 FPU default state, and the
canonical `MOVEC IEP3` control-register name.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to review findings `RVW-2026-04-07-001` through
  `RVW-2026-04-07-004`, plus the minimum documentation and closure artifacts
  needed to make those fixes reviewable and traceable.
- No work item may silently widen scope beyond the cited findings.
- Existing non-68080 behavior and existing 68080 behavior not named by the
  findings must remain unchanged unless a narrower fix is impossible and the
  plan item records that justification.
- One active work item at a time; each work item ends in exactly one new commit
  before the next item begins.

## Execution Rules From AGENTS.md

- Production code first for each remediation slice. Do not spend a slice on
  cleanup, renames, formatting-only edits, or test-harness expansion unless the
  change is required to land the finding fix safely.
- Stay inside the smallest viable vertical slice. Each work item must make one
  specific PRM mismatch stop reproducing.
- Tests are supporting work, not the main work. Add the minimum targeted
  validation that proves the corrected behavior, then run the full quality
  gates.
- Refactoring is allowed only when it directly enables the current slice. If a
  slice requires refactoring, record why a narrower change was not sufficient in
  the implementation update for that slice.
- If blocked, stop peripheral work and record the exact blocker, affected file
  or interface, attempted approach, needed decision, and smallest next step.
- Because this is review-driven work, each finding must also end with a closure
  artifact and a `finding-closure-reviewer` PASS before it is marked fixed.

## Execution Protocol

1. Run `plan-quality-reviewer` on this plan and the source review report.
  Prefer `Plan Quality Orchestrator` when the environment can launch its
  nested reviewers. Implementation does not start until plan-quality review
  returns `PASS` and the result is saved to
  `documentation/plans/opForge-m68000-family-68080-prm-remediation-plan-v0_1.md.quality-gate.txt`.
2. Select the next unchecked work item only after the previous item is fully
   committed and its checkbox state is updated.
3. Implement only the current slice and update this plan as mandatory
   bookkeeping.
4. Run the full quality gates for the slice: `cargo fmt --all`,
   `cargo clippy --all-targets --all-features -- -D warnings`, `cargo audit`,
   and `cargo test --workspace`.
5. Run the slice-specific validation listed in the active work item.
6. Run `plan-compliance-reviewer` with the active `AGENTS.md`, this plan, the
   current slice summary, changed files, and validation evidence.
7. If `plan-compliance-reviewer` returns `PASS`, create exactly one new commit
   for that work item, update the checkbox state, and then continue.
8. After the implementation items are complete, write one closure report per
   finding under `documentation/finding-closures/` and run
   `finding-closure-reviewer` before any finding is marked fixed.
9. If traceability across findings, commits, and validations becomes hard to
   inspect, run `artifact-traceability-reviewer` before final close-out.
10. If any quality gate or reviewer loop fails three times for one item, stop
    and ask the user to resolve the blockage before continuing.

## Current Execution Status

- WI-1: complete. `plan-compliance-reviewer` PASS and committed as
  `d697a55` (`Removed incorrect Apollo gating from 68080 AMMX instructions.`).
- WI-2: slice reconstructed and fully validated in the working tree. Default
  `MOVIW` now emits the regular PRM form without `.apollo on`, the deprecated
  Line-A `MOVIW` compatibility form is not exposed, and the slice is being
  advanced through `plan-compliance-reviewer` before commit.
- A previously validated pre-split working tree exists for WI-2 through WI-4,
  but none of those items is complete for workflow purposes until each slice is
  reconstructed as its own commit candidate and passes `plan-compliance-reviewer`.
- WI-5: draft closure artifacts are being prepared so the final closure pass can
  record the validated fixes after commit-level traceability exists.
- Outstanding workflow work: run one `plan-compliance-reviewer` pass per slice,
  create one commit per work item, run `finding-closure-reviewer` on the
  closure reports, and then update the checkboxes to their final closed state.

## Work Items

- [x] WI-1: Remove Apollo over-gating from AMMX line-F instructions.
  - Source requirement or finding IDs: `RVW-2026-04-07-001` (expected closure:
    full)
  - Validation: `cargo test -p asm 68080`
  - Definition of done: representative AMMX line-F instructions such as
    `LOADI`, `STOREI`, and `PAND` assemble on plain `.cpu 68080` without
    `.apollo on`; the documented Line-A Apollo subset continues to enforce its
    existing gate until its dedicated slice changes it; targeted regression
    tests prove both the newly legal AMMX path and the still-gated Line-A path.
  - Expected files: `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-families/src/m68080/module.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this slice only narrows
    Apollo gating for PRM-defined AMMX line-F instructions and adds boundary
    regression coverage.
  - Commit outcome: one commit removing AMMX Apollo over-gating and landing the
    targeted regression tests.

- [ ] WI-2: Make the regular `MOVIW` encoding the default 68080 path and isolate
      the restricted Line-A compatibility form.
  - Source requirement or finding IDs: `RVW-2026-04-07-002` (expected closure:
    full)
  - Validation: `cargo test -p asm moviw`
  - Definition of done: plain `.cpu 68080` `MOVIW.L` assembles without
    `.apollo on` and emits the regular PRM encoding; diagnostics no longer
    describe default `MOVIW` as Apollo-gated; if the deprecated Line-A form
    still needs to assemble, this slice uses exactly one explicit compatibility
    selector for it and documents/tests that selector separately from the
    default mnemonic contract.
  - Expected files: `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `documentation/opForge-m68000-family-68080-full-extension-spec-v0_1.md`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this slice is limited to
    default `MOVIW` encoding/gating, the explicit compatibility path if needed,
    and the related documentation/tests.
  - Commit outcome: one commit rebinding default `MOVIW` to the PRM form and
    isolating any deprecated compatibility encoding behind an explicit request.

- [ ] WI-3: Default `.cpu 68080` to the integrated 68080 FPU target.
  - Source requirement or finding IDs: `RVW-2026-04-07-003` (expected closure:
    full)
  - Validation: `cargo test -p asm m68080_fpu`
  - Definition of done: documented 68080 FPU instructions assemble under plain
    `.cpu 68080` without an extra `.fpu 68080` directive; explicit override or
    disable states still behave deterministically; capability and diagnostic
    tests reflect the integrated-default 68080 FPU contract.
  - Expected files: `crates/opforge-families/src/m68k/state.rs`,
    `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/normalization.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `documentation/opForge-m68000-family-68080-full-extension-spec-v0_1.md`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this slice only changes
    the 68080 FPU default-state contract, its deterministic diagnostics, and
    the required regression coverage.
  - Commit outcome: one commit making the integrated 68080 FPU default active
    under `.cpu 68080` and updating the supporting tests/docs.

- [ ] WI-4: Canonicalize `IEP3` for `MOVEC` code `$00C` while preserving the
      legacy `STH` alias.
  - Source requirement or finding IDs: `RVW-2026-04-07-004` (expected closure:
    full)
  - Validation: `cargo test -p asm movec`
  - Definition of done: `MOVEC IEP3,...` and `MOVEC ...,IEP3` assemble on
    68080 as the canonical spelling for control-register code `$00C`; legacy
    `STH` remains available only as an explicit compatibility alias if existing
    compatibility coverage still requires it; tests and docs use `IEP3` as the
    primary name.
  - Expected files: `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `documentation/opForge-m68000-family-68080-full-extension-spec-v0_1.md`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this slice is restricted
    to the control-register naming surface, backwards-compatible aliasing, and
    the required tests/docs.
  - Commit outcome: one commit making `IEP3` the canonical 68080 name for code
    `$00C` and retaining `STH` only as compatibility surface if needed.

- [ ] WI-5: Produce closure artifacts and final review evidence for findings
      `RVW-2026-04-07-001` through `RVW-2026-04-07-004`.
  - Source requirement or finding IDs: `RVW-2026-04-07-001`,
    `RVW-2026-04-07-002`, `RVW-2026-04-07-003`, `RVW-2026-04-07-004`
    (expected closure: full verification)
  - Validation: `finding-closure-reviewer` PASS for each closure artifact, plus
    `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opForge-m68000-family-68080-prm-remediation-plan-v0_1.md`
  - Definition of done: one closure report per finding exists under
    `documentation/finding-closures/`, each report cites the implementing plan
    item, commit or slice, changed files, and validation evidence; no finding is
    marked fixed until `finding-closure-reviewer` returns `PASS`; if the
    mapping from findings to commits becomes unclear, `artifact-traceability-reviewer`
    is run before the final close-out commit.
  - Expected files: `documentation/finding-closures/opforge-68080-prm-validation-2026-04-07-RVW-2026-04-07-001-closure.md`,
    `documentation/finding-closures/opforge-68080-prm-validation-2026-04-07-RVW-2026-04-07-002-closure.md`,
    `documentation/finding-closures/opforge-68080-prm-validation-2026-04-07-RVW-2026-04-07-003-closure.md`,
    `documentation/finding-closures/opforge-68080-prm-validation-2026-04-07-RVW-2026-04-07-004-closure.md`,
    `documentation/plans/opForge-m68000-family-68080-prm-remediation-plan-v0_1.md`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming the closure artifacts are
    traceable to the implemented slices and that no finding was prematurely
    marked fixed.
  - Commit outcome: one commit containing the closure reports, final plan
    checkbox updates, and any required traceability evidence.

## Milestones

- [ ] Milestone 1: Apollo boundary and `MOVIW` default behavior corrected
      (WI-1 through WI-2).
- [ ] Milestone 2: Integrated FPU default and `IEP3` naming corrected
      (WI-3 through WI-4).
- [ ] Milestone 3: Closure artifacts and review evidence complete (WI-5).

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan execution begins before `plan-quality-reviewer` returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- no review finding is marked fixed before `finding-closure-reviewer` returns
  `PASS`
- checkbox updates are mandatory bookkeeping