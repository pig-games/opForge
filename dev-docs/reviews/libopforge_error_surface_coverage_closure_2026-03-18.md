# libopforge facade error-surface coverage closure - 2026-03-18

## Scope

This artifact closes `dev-docs/NextSteps/libopforge_rust_facade_error_surface_coverage_plan_2026-03-18_rev1.md` after the Item 1 and Item 2 commits on `feature/libopforge-lib`.

Relevant commits:

- Item 1: `0d3bed25d3225bbd21b59c3e254d05f883c8fbb0` `Added lockstep workflow parity regression.`
- Item 2: `a73a161` `Documented remaining parity audit scope.`

## Traceability

### Item 1 trace

- Plan item: Item 1 in `dev-docs/NextSteps/libopforge_rust_facade_error_surface_coverage_plan_2026-03-18_rev1.md`
- Supporting artifact: `dev-docs/reviews/libopforge_error_surface_parity_audit_2026-03-18.md`
- Landed regression: `public_asm_workflow_lockstep_failed_assembly_preserves_assemble_category` in `crates/opforge-lib/src/lib.rs`
- Requirements covered: `FUP-VM-001`, `FUP-VM-002`, `FUP-VM-003`
- Rev7 linkage: preserves high-level `AssemblerWorkflowError` category behavior while keeping `AsmRunError` as the `AssemblerWorkflowError::Assemble` payload on the live lockstep path.

### Item 2 trace

- Plan item: Item 2 in `dev-docs/NextSteps/libopforge_rust_facade_error_surface_coverage_plan_2026-03-18_rev1.md`
- Supporting artifact: `dev-docs/reviews/libopforge_error_surface_remaining_parity_audit_2026-03-18.md`
- Requirements covered: `FUP-VM-003`
- Rev7 linkage: confirms the remaining touched public error surfaces continue to satisfy the rev7 expectations for `CoreError`, `ProcessorError`, `AssemblerWorkflowError`, and `AsmRunError` without requiring another narrow regression in this follow-up.

## Requirement closure summary

| Requirement | Closure |
| --- | --- |
| `FUP-VM-001` document current Rust and VM paths | Closed by the Item 1 blind-spot note and the Item 2 remaining-surface audit. |
| `FUP-VM-002` check current test and lockstep adequacy | Closed by the Item 1 lockstep blind-spot analysis plus the Item 2 adequacy audit. |
| `FUP-VM-003` extend regressions only where a real blind spot exists | Closed by the single Item 1 regression on the live lockstep failed-assembly path and the Item 2 conclusion that no further narrow regression is justified. |
| `FUP-VM-004` trace landed audit and regressions back to rev7 and the plan | Closed by this closure artifact. |
| `FUP-VM-005` record whether broader VM parity work needs a separate plan | Closed: broader VM parity work is required and must proceed as a separate dedicated VM parity plan. |

### FUP-VM-004 trace

- Rev7 source: `dev-docs/NextSteps/libopforge_rust_facade_upgrade_spec_2026-03-17_rev7.md`
- March 18 baseline: `dev-docs/reviews/libopforge_full_review_closure_2026-03-18.md`
- Follow-up plan: `dev-docs/NextSteps/libopforge_rust_facade_error_surface_coverage_plan_2026-03-18_rev1.md`
- Item 1 linkage: the live lockstep failed-assembly regression and blind-spot note preserve the rev7 workflow-category and payload expectations without reopening the already-closed March 18 findings `RVW-2026-03-18-001` through `RVW-2026-03-18-003`.
- Item 2 linkage: the remaining-surface audit records where the touched public facade error contracts already satisfy the rev7 validation bullets and where broader VM parity still needs its own future plan.
- Closure result: this artifact closes `FUP-VM-004` by tying the narrowed follow-up back to the rev7 obligations, the March 18 baseline remediation, the Item 1 implementation commit, and the Item 2 audit commit.

## Broader VM parity follow-up decision

The narrowed follow-up is complete, but broader VM parity work remains.

A separate dedicated VM parity plan is required for the following questions:

- whether module-item routing should gain a truthful Rust or lockstep parity path
- whether high-level workflow testing needs a live-path harness for `ProcessorErrorKind::Internal` and `ProcessorErrorKind::ProcessorDiagnostic`
- whether additional parity expectations should be added below the touched public facade error surfaces addressed here

This closure artifact records that requirement. It does not expand into that broader plan.

## Validation evidence

Validation executed across the completed narrowed follow-up:

- Item 1 targeted regression and `cargo test --locked -p libopforge`
- Item 2 targeted parity tests and `cargo test --locked -p libopforge`
- `cargo fmt --all`
- `cargo clippy --workspace -- -D warnings`
- `cargo audit`
- `cargo test --locked --workspace`
- `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/libopforge_rust_facade_error_surface_coverage_plan_2026-03-18_rev1.md`
- Final `artifact-traceability-reviewer` re-review for the completed closure state: PASS with no findings.
- Final `plan-compliance-reviewer` re-review for the completed closure state: PASS with no findings and commit allowed.

## Conclusion

The narrowed facade error-surface coverage follow-up is complete. Item 1 added the only remaining live-path regression that was narrow enough to justify implementation work inside this slice, Item 2 documented the remaining touched Rust/VM parity surface and showed that no further narrow regression is justified here, and this closure artifact records that any remaining VM parity debt now belongs to a separate dedicated follow-up plan.