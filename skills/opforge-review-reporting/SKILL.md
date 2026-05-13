---
name: opforge-review-reporting
description: Create normalized opForge review reports with findings-first structure, stable finding IDs, evidence-based severity, and explicit testing gaps. Use when a code review needs to be written, cleaned up, or merged from multiple reviewers.
---

# opForge Review Reporting

## Use when

- writing or normalizing a code review report
- merging multiple reviewer outputs
- preparing remediation input

## Required output

Use `templates/review-report-template.md` without changing its validator-facing headings.

Each material finding includes:

- stable `RVW-YYYY-MM-DD-NNN` finding ID
- severity, file, issue, why it matters, and one fix direction
- testing gap or residual risk when relevant

## Guardrails

- Findings first.
- No style-only nits unless they affect behavior, safety, maintainability, or future correctness.
- If no material findings exist, say so and note residual risk.
- Do not leave unresolved open questions in the final artifact.
- If competing fix paths remain, ask the user to choose before finalizing.

## Validate with

- `scripts/workflow/allocate_review_finding_id.py`
- `scripts/workflow/check_review_report.py`
- `scripts/workflow/check_workflow_artifact_bundle.py`
- `scripts/workflow/run_review_workflow.sh`
