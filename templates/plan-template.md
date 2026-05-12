# Plan Title

## Metadata

- Source:
- Mode:
- Owner:

## Objective

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.

## Version Impact

- Affected component(s):
- Impact class:
- Owned contract:
- Rationale:

## Work Items

- [ ] Item 1
  - Source requirement or finding IDs:
  - Expected files:
  - Full quality gates:
  - Plan-compliance review evidence:
  - Commit outcome:
  - Definition of done:

## Milestones

- [ ] Milestone 1

## Completion Archive

- When every checkbox in this plan is complete and the plan is no longer the
  active execution artifact, archive it with
  `scripts/workflow/archive_completed_plan.sh`.
- The archived filename must move to `documentation/plans/completed/` and end
  in `-completed-YYYY-MM-DDTHHMMSSZ.md`.
- Move the companion quality-gate sidecar with the same timestamped basename.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
