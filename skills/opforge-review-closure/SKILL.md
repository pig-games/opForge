---
name: opforge-review-closure
description: Verify and document that claimed review fixes actually close the original opForge findings. Use when remediation work claims to resolve prior review items and closure evidence must be recorded.
---

# opForge Review Closure

## Overview

Track review findings through implementation so follow-up reviews can tell the
difference between:

- fixed,
- partially fixed,
- not fixed,
- superseded,
- deferred.

## Workflow

1. Start from the original review finding ID.
2. Identify the plan item and implementation slice that claim closure.
3. Record the targeted validation evidence.
4. Assign a closure status.
5. Note residual risk if closure is partial or deferred.

## Required structure

Use [templates/finding-closure-report-template.md](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/templates/finding-closure-report-template.md).

## Guardrails

- Stable finding ID is mandatory.
- “Looks fixed” is not evidence.
- Closure must be tied to a validation result or a reproduction check.
- If the original issue still reproduces, status is not `fixed`.

## References

- `../../references/workflow/finding-closure-rules.md`
- `../../templates/finding-closure-report-template.md`

