# libopforge rust facade error-surface coverage plan - 2026-03-18

## Metadata

- Source: `dev-docs/NextSteps/libopforge_rust_facade_upgrade_spec_2026-03-17_rev7.md`, `dev-docs/reviews/libopforge_full_review_closure_2026-03-18.md`, and the scoped user instruction for this follow-up
- Mode: `remediation`
- Owner: Codex

## Source

- User instruction: `implement this plan following this worktree's AGENTS.md instructions`
- Governing worktree rules: `/Users/erik/Code/Retro/opForge/AGENTS.md`
- Upstream facade spec: `dev-docs/NextSteps/libopforge_rust_facade_upgrade_spec_2026-03-17_rev7.md`
- Established remediation baseline: `dev-docs/reviews/libopforge_full_review_closure_2026-03-18.md`

## Objective

Narrow this follow-up plan to the issues that still appear genuinely open after the latest check. The current worktree now appears to contain direct facade-local coverage for the named public error surfaces and the main guide/error-contract examples, so this plan should no longer try to re-land broad docs/tests coverage that already exists.

The remaining work for this artifact is therefore:

- audit the current Rust and VM paths for the touched public error surfaces
- check how well the current lockstep option and existing tests actually validate parity for those paths
- extend the regression suite only where that audit finds real blind spots
- record whether any broader VM parity debt should move into a separate dedicated plan

The touched public error surfaces for this narrower follow-up remain:

- `libopforge::opcore::CoreError`
- `libopforge::opcore::CoreErrorKind`
- `libopforge::processing::ProcessorError`
- `libopforge::processing::ProcessorErrorKind`
- `libopforge::processing::ProcessorFailureDetail`
- `libopforge::asm::AssemblerWorkflowError`
- `libopforge::diagnostics::AsmRunError` as the payload carried by `AssemblerWorkflowError::Assemble`

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at all times during plan execution.
- This plan is intentionally separate from `dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-18.md` and must not patch, widen, or replace that plan.
- Do not begin overlapping execution on shared files until the current March 18 remediation plan has fully settled or the user explicitly re-sequences the two plans.
- Scope is limited to parity audit, test-adequacy review, targeted regression hardening, and follow-up traceability for the touched public error surfaces listed above.
- Do not broaden the task into FFI redesign, formatter-taxonomy redesign, generic processing-boundary redesign beyond the already-reviewed fixes, or unrelated documentation cleanup.
- Existing docs/examples/tests that already cover the touched public error surfaces should be treated as current state to audit and reuse, not as missing work to be recreated.
- This plan does not attempt to solve all Rust/VM parity questions in the repository. If the required audit finds broader parity debt beyond the touched facade error surfaces, that broader work must be captured in a separate dedicated VM parity plan rather than being silently absorbed here.
- One active work item at a time; no overlapping implementation across items.
- Each work item ends in exactly one new commit before the next one begins.
- The plan does not become active until the `Plan Quality Reviewer` returns `PASS` for this artifact against the active worktree `AGENTS.md` and the cited source artifacts.
- Full quality gates and `plan-compliance-reviewer` `PASS` evidence are required before each commit.

## Follow-up requirements

- `FUP-VM-001`: a deep analysis must document the current Rust and VM paths for the touched facade error surfaces and the current parity expectations for those paths.
- `FUP-VM-002`: the current test suite and lockstep option must be checked explicitly to determine whether they already exercise those touched paths adequately.
- `FUP-VM-003`: the regression suite must be extended only where the audit finds real blind spots affecting the touched public error surfaces.
- `FUP-VM-004`: the landed audit and regression changes must be traceable back to the rev7 validation expectations and the original facade-plan obligations.
- `FUP-VM-005`: if the audit finds broader VM parity debt beyond this narrowed scope, the repository must record that a separate dedicated VM parity plan is required.

## Work Items

- [x] Item 1: Land the smallest live-path parity regression needed to make the narrowed audit concrete.
  - Definition of done: one concrete touched-surface parity regression is landed on the live public path and the remaining audit scope is narrowed accordingly.
  - Source requirement or finding IDs: `FUP-VM-001`, `FUP-VM-002`, `FUP-VM-003`; rev7 `Validation Expectations` bullets covering `CoreError` and `CoreErrorKind`, `ProcessorError`, `ProcessorErrorKind`, and `ProcessorFailureDetail`, `AssemblerWorkflowError`, `AsmRunError`, and the required engine-to-assembler mapping behavior; March 18 closure findings `RVW-2026-03-18-001` through `RVW-2026-03-18-003` as established shipped baseline rather than reopened work.
  - Expected files: the narrowest regression location on the live path, expected to be `crates/opforge-lib/src/lib.rs` unless the audit proves a smaller existing parity test location is better, plus only the smallest supporting audit note if needed to explain the exact blind spot the regression closes.
  - Validation: inspect the current Rust and VM code paths for the touched facade error surfaces just enough to choose one real blind spot on the live path, implement the minimum regression that closes it, and run the relevant targeted parity or lockstep tests plus `cargo test --locked -p libopforge` to prove the slice.
  - Completed slice: `public_asm_workflow_lockstep_failed_assembly_preserves_assemble_category` is landed in `crates/opforge-lib/src/lib.rs`; targeted validation and full quality gates passed, and the slice is at the Item 1 commit boundary.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`.
  - Plan-compliance review evidence: commit gate requires `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the first parity-regression slice.
  - Commit outcome: the narrowed follow-up starts with one concrete parity regression on the shipped public path rather than an audit-only artifact, and the exact live-path gap it closes is recorded for the remaining audit work.
  - Detailed definition of done: the repository contains one concrete regression covering a real touched-surface parity gap on the live public path, and the remaining audit work is narrowed to what that regression does not cover.

- [x] Item 2: Audit the remaining Rust/VM parity surface and lockstep test adequacy for the touched public error contracts.
  - Definition of done: the remaining touched Rust/VM paths and current coverage are documented clearly enough to justify either more narrow regression work or explicit deferral.
  - Source requirement or finding IDs: `FUP-VM-003`; rev7 `Validation Expectations` bullets covering stable inspection of `CoreError` and `CoreErrorKind`, stable inspection and kind mapping of `ProcessorError`, `ProcessorErrorKind`, and `ProcessorFailureDetail`, stable `AsmRunError` accessors and `AssemblerWorkflowError` category behavior, and proof that genuinely core-local failures may cross the engine boundary as `CoreError` while processor-originated failures map through the expected assembler workflow variants.
  - Expected files: one analysis artifact under `dev-docs/NextSteps/` or `dev-docs/reviews/`, and only the smallest supporting regression adjustments still required after Item 1 if the audit finds another non-deferred blind spot.
  - Validation: inspect the current Rust and VM code paths for the touched facade error surfaces, inventory the current tests/examples/checks that cover them, record whether the existing lockstep mode and parity-oriented tests exercise those paths adequately, and identify any remaining regression additions required after Item 1; run the relevant currently existing targeted parity or lockstep tests plus `cargo test --locked -p libopforge`, and `cargo test --locked -p opforge-engine` if engine-local parity coverage is added.
  - Completed slice: `dev-docs/reviews/libopforge_error_surface_remaining_parity_audit_2026-03-18.md` records the remaining touched paths after Item 1, confirms that no further narrow regression is justified in this slice, and defers the remaining gaps to broader VM parity follow-up.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`.
  - Plan-compliance review evidence: commit gate requires `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the parity-audit slice.
  - Commit outcome: maintainers have a checked-in audit of the remaining touched Rust/VM paths, the current lockstep-test adequacy for those paths, the blind spots that still affect this narrowed follow-up work, and any broader parity debt that must be deferred to a separate dedicated VM parity plan.
  - Detailed definition of done: the repository contains a concrete analysis of the remaining touched Rust/VM paths and current coverage after Item 1, including an explicit statement on whether the lockstep option is being used well enough for those surfaces and a precise list of any further regression additions still required.

- [x] Item 3: Publish traceability evidence and record whether broader VM parity work needs a separate plan.
  - Definition of done: the narrowed follow-up is traceable end to end and the broader VM parity follow-up decision is explicitly recorded.
  - Source requirement or finding IDs: `FUP-VM-004`, `FUP-VM-005`; rev7 `Validation Expectations` for the touched public error surfaces and engine-to-assembler mapping behavior; March 18 closure findings `RVW-2026-03-18-001` through `RVW-2026-03-18-003` as the already-landed remediation baseline that this narrower parity follow-up audits rather than reimplements.
  - Expected files: this plan for checkbox updates and one follow-up traceability or closure artifact under `dev-docs/NextSteps/` or `dev-docs/reviews/` that links the audit and landed regression additions back to the cited spec and plan obligations, including the final recommendation on broader VM parity work.
  - Validation: run `python3 /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/scripts/workflow/check_plan_checkboxes.py /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/dev-docs/NextSteps/libopforge_rust_facade_error_surface_coverage_plan_2026-03-18_rev1.md`, the full workspace quality gates, `plan-compliance-reviewer`, and `artifact-traceability-reviewer` for the final closure state; include explicit traceability for the audit findings, the resulting regression additions, and the final decision on whether a separate dedicated VM parity plan is needed.
  - Completed slice: `dev-docs/reviews/libopforge_error_surface_coverage_closure_2026-03-18.md` links the Item 1 and Item 2 commits back to the plan and rev7 obligations and records that broader VM parity work must proceed as a separate dedicated plan.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`.
  - Plan-compliance review evidence: final commit gate requires `PASS` from `plan-compliance-reviewer` confirming the last active checkbox, changed files, validation evidence, and bookkeeping are consistent with the completed narrowed remediation state.
  - Commit outcome: future readers can see exactly which parity questions were audited for the touched public error surfaces, which regression checks were added, and whether any broader VM parity debt was intentionally deferred into a separate dedicated plan.
  - Detailed definition of done: the narrowed follow-up work is traceable, the required workflow gates have passed, the final plan state matches the implemented audit and regression hardening, and the repository contains a clear decision on any broader VM parity follow-up.

## Milestones

- [x] Milestone 1: One concrete live-path parity regression for the touched public error surfaces is landed (`Item 1` complete).
- [x] Milestone 2: The current touched Rust/VM paths and current lockstep-test adequacy are documented in a checked-in audit (`Item 2` complete).
- [x] Milestone 3: Follow-up traceability evidence is complete and the broader VM parity follow-up decision is recorded (`Item 3` complete).

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no execution overlap with any reopened March 18 remediation work on shared files unless the user explicitly re-sequences both efforts
- no plan execution before the `Plan Quality Reviewer` returns `PASS`
- no execution of Items 2 and 3 before the Item 1 regression slice is committed
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- no commit on a touched Rust/VM dual-mode path before the audit-derived parity checks for that path have been executed and recorded
- each work item or phase must end in exactly one new commit before the next one starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
