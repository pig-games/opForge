---
name: opforge-review-reporting
description: Create normalized opForge review reports with findings-first structure, stable finding IDs, evidence-based severity, and explicit testing gaps. Use when a code review needs to be written, cleaned up, or merged from multiple reviewers.
---

# opForge Review Reporting

## Overview

Produce review reports that are usable as direct inputs to remediation planning
and closure tracking.

## Workflow

1. Review the requested scope.
2. Record only material findings.
3. Assign a stable finding ID to each finding.
4. Order findings by severity.
5. Resolve material clarification questions before finalizing the review.
6. Write the final review as a Markdown artifact using the local template.

## Required structure

Use [templates/review-report-template.md](/Users/erik/Code/Retro/opForge/templates/review-report-template.md).

Each finding must include:

- finding ID,
- severity,
- file reference,
- why it matters,
- one concrete fix direction.

## Guardrails

- Findings first.
- No style-only nits unless they affect behavior, safety, or maintainability.
- Stable finding IDs are mandatory for follow-up planning and closure.
- Do not leave an `Open Questions` section in the final review artifact.
- If clarification is required, ask the user first and only then finalize the
  review artifact.
- Do not present multiple competing fix options in the final review artifact.
- If multiple materially different fix paths remain viable, ask the user to
  resolve the choice before finalizing the review.
- Guided review iteration should not continue indefinitely; after 3 failed
  review/gate correction cycles, stop and ask the user how to resolve the
  blockage.
- If no material findings exist, say so explicitly and still note residual risk.

## References

- `../../references/workflow/review-severity-guide.md`
- `../../templates/review-report-template.md`

## Helper scripts

- `../../scripts/workflow/check_review_report.py`
- `../../scripts/workflow/run_review_workflow.sh`
