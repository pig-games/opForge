# Definition Of Done Matrix

## Spec

Done when:

- goals and non-goals exist
- acceptance criteria are testable
- unresolved questions are explicit

## Plan

Done when:

- source and mode are explicit
- work items are ordered and commit-sized
- risk-matched focused validation is defined per sub-item
- high-level phase/epic closure checkpoints and their full quality gates are
  explicit
- `plan-compliance-reviewer` is required before each commit
- each work item or phase is expected to end in a new commit
- progress checkboxes exist

## Implementation slice

Done when:

- behavior advanced for the target slice
- the slice's focused behavior, invariant, formatter, architecture, inventory,
  staged native, and external proof gates passed as applicable
- if the slice is a named high-level closure checkpoint, the full Rust gate
  (`scripts/workflow/run_rust_quality_gate.sh` or `make quality-gate`) and every
  other closure gate passed
- `plan-compliance-reviewer` passed before commit
- the slice is ready to land as the commit for its active work item or phase
- progress state was updated

## Finding closure

Done when:

- the original finding ID is cited
- closure evidence exists
- closure status is justified
