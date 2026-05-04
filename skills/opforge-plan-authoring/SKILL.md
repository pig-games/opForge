---
name: opforge-plan-authoring
description: Create execution-ready opForge plans from specs, reviews, or explicitly scoped user requests. Use when implementation or remediation work needs ordered, commit-sized slices with checkboxes, validations, and definition-of-done rules.
---

# opForge Plan Authoring

## Overview

Write one kind of plan for all executable work. The input may differ, but the
plan quality bar stays the same.

## Plan modes

Valid plan modes:

- `implementation`
- `remediation`
- `migration`
- `cleanup-only` when explicitly approved

## Workflow

1. Identify the plan source.
2. Record the plan mode.
3. Convert the source into ordered work items.
4. Keep each item commit-sized.
5. Require each item or phase to end in a new commit.
6. Define full quality-gate validation, plan-compliance evidence, and done
   criteria per item.
7. Add progress checkboxes.
8. State explicitly that the active worktree `AGENTS.md` rules remain binding
   throughout execution.

## Required structure

Use [templates/plan-template.md](/Users/erik/Code/Retro/opForge/templates/plan-template.md).

For remediation plans, each work item must also list:

- the finding IDs it addresses,
- whether it is expected to fully or partially close them.

## Guardrails

- One active work item at a time.
- Small commits only.
- Every work item or phase must produce a new commit before the next item
  begins.
- Full quality gates are mandatory before each commit.
- Rust code changes should list `scripts/workflow/run_rust_quality_gate.sh` (or
  `make quality-gate`) as the full Rust gate, plus any focused tests required
  by the specific slice.
- `plan-compliance-reviewer` must pass before each commit.
- Checkbox updates are mandatory bookkeeping.
- Every generated plan must state that the active worktree `AGENTS.md`
  workflow and execution rules remain binding at all times.
- If the plan is derived from a spec or review, do not silently widen scope.

## References

- `../../references/workflow/plan-slice-rules.md`
- `../../references/workflow/definition-of-done-matrix.md`
- `../../references/workflow/plan-modes-guide.md`
- `../../references/workflow/traceability-guide.md`
- `../../templates/plan-template.md`

## Helper scripts

- `../../scripts/workflow/new_artifact_from_template.sh`
- `../../scripts/workflow/check_plan_checkboxes.py`
- `../../scripts/workflow/run_rust_quality_gate.sh`
