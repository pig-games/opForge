# Plan: <title>

## Metadata

- Source:
- Mode:
- Owner:
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.

## Goal

## Version Impact

- Affected component(s):
- Impact class:
- Owned contract:
- Rationale:

## Work Items

- [ ] Item 1: <commit-sized slice>
  - Source requirement or finding IDs:
  - Expected files:
  - Gate tier: focused sub-item | high-level closure
  - Required focused gates:
  - Full quality gates (required at a named high-level checkpoint; otherwise state the checkpoint to which they are deferred):
  - Plan-compliance review evidence:
  - Commit outcome:
  - Definition of done:

## Blocking Rules

- no commit before the active sub-item's focused gates pass
- no advancing beyond a named high-level closure checkpoint before its full
  gates pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`
