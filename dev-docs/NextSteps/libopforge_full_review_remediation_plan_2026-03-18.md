# libopforge full review remediation plan - 2026-03-18

## Metadata

- Source: `dev-docs/reviews/libopforge_full_review_2026-03-18.md` at commit `dd69bbaabf267ccd605869d6544acef9464d377c`
- Mode: `remediation`
- Owner: GitHub Copilot

## Objective

Close review findings `RVW-2026-03-18-001` through `RVW-2026-03-18-003` without widening scope beyond the public `libopforge::processing` boundary defects and the developer-guide contract mismatch identified in the review. The work must make the routed module-item helper publish the same neutral error contract as the rest of the processing facade, preserve truthful `CoreErrorKind` classification when core-owned failures cross the processing boundary, and bring the high-level Rust guide text back into sync with the shipped `AssemblerWorkflowError` API.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at all times during plan execution.
- One active work item at a time; no overlapping implementation across items.
- Each work item ends in exactly one new commit before the next one begins.
- Scope is limited to `RVW-2026-03-18-001` through `RVW-2026-03-18-003` plus the closure bookkeeping needed to prove those findings are resolved.
- Prefer the smallest production-code slice that closes each finding, with focused `libopforge` and engine validation added only where needed to prove the fix.
- Do not broaden the task into FFI outcome-object work, assembler workflow redesign, or unrelated guide cleanup outside the reviewed contract mismatches.
- No fixture or reference regeneration is allowed unless an intentional behavior change later proves it is required.
- The plan does not become active until the `Plan Quality Reviewer` returns `PASS` for this artifact against the active worktree `AGENTS.md` and the source review.
- Full quality gates and `plan-compliance-reviewer` `PASS` evidence are required before each commit.

## Work Items

- [x] Item 1: Repair the public `route_module_item_line` processing contract.
  - Source requirement or finding IDs: `RVW-2026-03-18-001` (expected full closure).
  - Expected files: `crates/opforge-engine/src/processing.rs`, `crates/opforge-engine/src/lib.rs` only if a narrow re-export adjustment is required, and `crates/opforge-lib/src/lib.rs`.
  - Validation: add focused facade tests proving the default `route_module_item_line(...)` helper now returns the same neutral `EngineError` split as `editor_route_line(...)`, and that the facade exports a model-driven `route_module_item_line_with_model(...)` path for hosts that provide an explicit runtime model; then run `cargo test --locked -p engine` if engine-local coverage is added and `cargo test --locked -p libopforge`.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the processing-helper contract slice.
  - Commit outcome: the stable facade stops surfacing runtime-model-unavailable and unsupported-processor-return failures as `ParseError`, and the model-backed module-item routing helper is available through the published `libopforge::processing` surface.
  - Definition of done: a host using the named public module-item routing helpers can distinguish core-owned failures from processor/orchestration failures without treating runtime-model setup problems as source parse errors.

- [x] Item 2: Preserve specific `CoreErrorKind` classification across the processing boundary.
  - Source requirement or finding IDs: `RVW-2026-03-18-002` (expected full closure).
  - Expected files: `crates/opforge-engine/src/processing.rs`, `crates/opforge-types/src/processing.rs` only if a narrow structured routing-error carrier is required, and `crates/opforge-lib/src/lib.rs`.
  - Validation: add focused tests proving routed generic core failures such as conditional or module-item failures retain their specific public `CoreErrorKind` values when surfaced through `processing::EngineError::Core`, then run `cargo test --locked -p engine` if engine-local coverage is added and `cargo test --locked -p libopforge`.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the core-classification slice.
  - Commit outcome: engine-side routing no longer flattens core-owned failures to `ParseError` before the facade maps them back into `CoreError`, so the public processing boundary preserves the truthful concern taxonomy promised by the completed facade plan.
  - Definition of done: a routed conditional, module, `.use`, import, macro, namespace, scope, preprocess, struct, or segment failure that reaches the stable processing facade still reports the same specific `CoreErrorKind` a direct `opcore` caller would observe.

- [ ] Item 3: Update the developer guide and guide validation for the high-level workflow error contract.
  - Source requirement or finding IDs: `RVW-2026-03-18-003` (expected full closure).
  - Expected files: `documentation/libopforge-developer-guide.md` and `crates/opforge-lib/src/lib.rs` only if a narrow guide assertion update is required.
  - Validation: update guide checks so they assert that high-level Rust assembly prose names `AssemblerWorkflowError` as the failure contract and describes `AsmRunError` only as the `Assemble` payload, then run `cargo test --locked -p libopforge`.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the guide-sync slice.
  - Commit outcome: the shipped developer guide matches the current facade API by documenting `AssemblerWorkflowError` as the top-level Rust assembly failure surface and `AsmRunError` as the assemble-path payload.
  - Definition of done: a Rust host following the main developer guide will handle the same top-level error type that the current `Assembler` and `AssemblerSession` APIs actually return.

- [ ] Item 4: Finish closure evidence and remediation bookkeeping for the reviewed findings.
  - Source requirement or finding IDs: `RVW-2026-03-18-001`, `RVW-2026-03-18-002`, and `RVW-2026-03-18-003` (expected closure confirmation only).
  - Expected files: this plan for checkbox updates and one closure artifact for `dev-docs/reviews/libopforge_full_review_2026-03-18.md`.
  - Validation: run `python3 /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/scripts/workflow/check_plan_checkboxes.py /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-18.md`, the full workspace quality gates, and the `Finding Closure Reviewer` plus final `plan-compliance-reviewer` gates.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`.
  - Plan-compliance review evidence: final `PASS` from `plan-compliance-reviewer` confirming the last active checkbox, changed files, validation evidence, and bookkeeping are consistent with the completed remediation state.
  - Commit outcome: the remediation plan, closure report, and final finding traceability accurately reflect the resolved state for all three March 18 review findings.
  - Definition of done: each finding has explicit closure evidence, required workflow gates have passed, and the final plan state matches the implemented and validated remediation work.

## Milestones

- [ ] Milestone 1: The public module-item routing helper exposes the neutral processing boundary promised by the facade (`Item 1` complete and committed).
- [ ] Milestone 2: Routed generic core failures preserve truthful concern classification through the processing facade (`Item 2` complete and committed).
- [ ] Milestone 3: The main developer guide matches the shipped high-level Rust workflow error contract (`Item 3` complete and committed).
- [ ] Milestone 4: Closure evidence and bookkeeping are complete for all March 18 findings (`Item 4` complete and committed).

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan execution before the `Plan Quality Reviewer` returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping