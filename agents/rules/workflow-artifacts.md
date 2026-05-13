# Workflow Artifact Rule Pack

Load this for specs, plans, review reports, closure reports, workflow templates,
skills, workflow agents, CI gates, and workflow validators.

## Artifact types

Canonical artifact types:

- specification
- plan
- review report
- finding closure report

Preferred templates:

- `templates/spec-template.md`
- `templates/plan-template.md`
- `templates/review-report-template.md`
- `templates/finding-closure-report-template.md`

## Provenance

Governed artifacts should state:

- artifact type
- source instruction/spec/review/finding
- relevant skill used
- relevant workflow wrapper or validator
- validation status

## Plan rules

- Every plan must explicitly state that the active worktree `AGENTS.md` remains
  binding during execution.
- Every work item or phase should be commit-sized.
- Every work item or phase should end in a new commit before the next begins.
- Plan-compliance review must pass before plan-driven commits.

## Archive completed plans

When a plan is complete and no longer active:

- Move it to `documentation/plans/completed/`.
- Append UTC completion timestamp: `-completed-YYYY-MM-DDTHHMMSSZ.md`.
- Move the quality-gate sidecar with the same timestamped basename.
- Prefer `scripts/workflow/archive_completed_plan.sh`.
- Do not archive unclear/incomplete plans as completed.

## Validator rule

Before committing workflow artifact changes, run `make workflow-gate` or the
specific `scripts/workflow/check_*.py` validators and record the result.
