---
name: opforge-spec-authoring
description: Write implementation-ready opForge specifications with explicit goals, non-goals, invariants, acceptance criteria, and validation expectations. Use when behavior needs to be designed before planning or coding begins.
---

# opForge Spec Authoring

## Use when

- behavior needs design before planning or coding
- scope, invariants, or acceptance criteria are unclear
- later plan derivation should be mechanical

## Required output

Use `templates/spec-template.md` without changing its validator-facing headings.

A valid spec states:

- summary, problem, goals, non-goals, invariants, behavioral contract, boundary cases, acceptance criteria, validation expectations, and open questions
- checkbox goals and checkbox acceptance criteria
- unresolved decisions explicitly, without hidden assumptions

## Guardrails

- Non-goals are mandatory.
- Acceptance criteria must be concrete and testable.
- Do not smuggle implementation plans into the spec.
- Do not broaden scope beyond the user request.

## Validate with

- `scripts/workflow/check_workflow_artifact_bundle.py`
- `scripts/workflow/run_spec_workflow.sh`
