---
name: opforge-plan-authoring
description: Create execution-ready opForge plans from specs, reviews, or explicitly scoped user requests. Use when implementation or remediation work needs ordered, commit-sized slices with checkboxes, validations, and definition-of-done rules.
---

# opForge Plan Authoring

## Use when

- creating an implementation, remediation, migration, or explicitly approved cleanup plan
- converting a spec or review into executable work items
- preparing plan-driven commits

## Required output

Use `templates/plan-template.md` without changing its validator-facing headings.

A valid plan includes:

- source, mode, objective, constraints, version impact, work items, milestones, and blocking rules
- an explicit statement that active `AGENTS.md` rules remain binding
- ordered commit-sized checkbox work items
- source requirements or finding IDs per work item
- expected files, full gates, plan-compliance evidence, commit outcome, and done criteria per item

For remediation plans, list finding IDs and whether each item fully or partially closes them.

## Guardrails

- One active item at a time.
- Do not silently widen scope beyond the source.
- Each work item or phase must end in a new commit before the next begins.
- Full quality gates and `plan-compliance-reviewer` are mandatory before plan-driven commits.
- Rust items should name `scripts/workflow/run_rust_quality_gate.sh` or `make quality-gate`.
- Native 68000 items should load `agents/rules/native-68000.md` and name the native formatter gate.

## Validate with

- `scripts/workflow/check_plan_checkboxes.py`
- `scripts/workflow/check_workflow_artifact_bundle.py`
- `scripts/workflow/run_plan_workflow.sh`
