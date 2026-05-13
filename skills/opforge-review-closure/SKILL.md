---
name: opforge-review-closure
description: Verify and document that claimed review fixes actually close the original opForge findings. Use when remediation work claims to resolve prior review items and closure evidence must be recorded.
---

# opForge Review Closure

## Use when

- remediation work claims to close review findings
- closure evidence must be recorded
- follow-up review needs fixed, partial, deferred, superseded, or not-fixed status

## Required output

Use `templates/finding-closure-report-template.md` without changing its validator-facing headings.

Each closure report includes:

- original finding ID and summary
- plan item, implementation slice or commit, and changed files
- validation command/check and result
- closure status, residual risk, and closure rationale

## Guardrails

- Stable finding ID is mandatory.
- "Looks fixed" is not evidence.
- Closure must tie to validation, reproduction, or direct code inspection.
- If the original issue still reproduces, status is not `fixed`.

## Validate with

- `scripts/workflow/check_closure_report.py`
- `scripts/workflow/check_workflow_artifact_bundle.py`
- `scripts/workflow/run_closure_workflow.sh`
