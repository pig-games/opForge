# opForge Workflow Enforcement Plan v0.1

## Summary

This document proposes a concrete enforcement layer for the branch-local agent
workflow.

The goal is to stop relying on agents remembering prose rules and instead make
the most important workflow expectations:

- structured,
- checkable,
- CI-visible,
- blocking when violated.

## Problem

The current workflow has better rules than before, but several important
failures still happen in practice:

- plans omit required execution fields,
- reviews still drift from the intended shape,
- checks are claimed but not actually evidenced,
- plan-compliance review is sometimes skipped,
- commits happen without the intended per-phase discipline,
- fixes are claimed, but later reviews reopen the same issue.

The core problem is that too many rules still exist only as prose in:

- `AGENTS.md`,
- skills,
- reviewer definitions,
- workflow proposal docs.

That helps conscientious agents, but it does not reliably block drift.

## Goals

- [x] Define a concrete enforcement architecture for plans, reviews, closure
  reports, and version-impact declarations.
- [x] Define exact scripts to add for machine-checkable workflow validation.
- [x] Define exact artifact fields that those scripts should require.
- [x] Define CI hook points where those scripts should run.
- [x] Define blocking behavior for failures.
- [x] Keep the system incremental so it can be adopted in phases.

## Non-Goals

- [x] This plan does not implement the scripts yet.
- [x] This plan does not require all historical artifacts to be backfilled.
- [x] This plan does not replace human code review.
- [x] This plan does not attempt to prove semantic correctness of production
  code.
- [x] This plan does not require a specific external release automation tool.

## Invariants / Constraints

- The active worktree [AGENTS.md](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/AGENTS.md)
  rules remain binding.
- Enforcement should prefer small deterministic scripts over large opaque
  validators.
- A failed workflow check must block the next workflow step.
- Artifact structure should be tightened only where that produces real blocking
  value.
- CI should validate artifact integrity, but not become dependent on hidden
  local state.

## Behavioral Contract

## 1. Enforcement model

Every binding workflow rule should exist in four places:

1. `AGENTS.md` as the repository rule,
2. a template as the expected artifact shape,
3. a reviewer/sub-agent as the human/LLM gate,
4. a script as the machine gate.

If a rule exists only in prose, it should be treated as advisory, not enforced.

## 2. Artifact classes to enforce

The enforcement layer should validate these artifacts:

- plan
- review report
- finding closure report
- spec
- version-impact declaration

The first three should be mandatory. Spec and version-impact validation can be
added in phase 2.

In addition to validators, the workflow should provide guided runner scripts for
common end-to-end artifact flows where agents regularly forget required steps.

## 3. Scripts to add

### 3.1 `scripts/workflow/check_plan_structure.py`

Purpose:

- validate that a plan has all mandatory fields and no obviously missing
  execution requirements.

Required checks:

- `## Metadata`, `## Objective`, `## Constraints`, `## Work Items`,
  `## Blocking Rules` exist
- every work item has:
  - checkbox
  - source requirement or finding IDs
  - expected files
  - full quality gates
  - plan-compliance review evidence
  - commit outcome
  - definition of done
- the plan explicitly states:
  - `AGENTS.md` rules remain binding
  - all quality gates must pass before commit
  - `plan-compliance-reviewer` must pass before commit
  - each work item or phase must end in a new commit

Failure mode:

- non-zero exit
- short structured error list

### 3.2 `scripts/workflow/check_review_report.py`

Purpose:

- validate that a review report is structurally usable for planning and closure.

Required checks:

- Markdown artifact
- `## Scope`, `## Findings`, `## Testing Gaps`, `## Residual Risks`,
  `## Brief Summary` exist
- each finding has:
  - stable finding ID
  - severity
  - file
  - why it matters
  - exactly one fix direction field
- forbidden content:
  - `## Open Questions`
  - unresolved “option A / option B” fix menus
  - missing finding IDs

Heuristic checks should be narrow and explicit. The script should reject only
clear patterns such as:

- `Option 1`, `Option 2`
- `Either ... or ...`
- `One approach would be`
- `Alternatively`

when they appear inside `Fix direction`.

### 3.2a `scripts/workflow/run_review_workflow.sh`

Purpose:

- scaffold and drive the branch-local review workflow from start to finish.

Required behavior:

- create the review artifact from the template if needed,
- print the exact branch-local review instructions,
- require the review artifact to pass `check_review_report.py`,
- require a companion `review-report-quality-reviewer` result file,
- allow at most 3 failed correction cycles,
- halt and ask the user to resolve the blockage if the retry limit is reached,
- otherwise loop until both the structural validator and the reviewer gate pass.

### 3.3 `scripts/workflow/check_closure_report.py`

Purpose:

- validate that closure claims are structurally complete before a finding is
  considered closed.

Required checks:

- original finding ID exists
- claimed status exists
- evidence section exists
- changed files or commit refs exist
- validation evidence exists
- closure rationale exists

### 3.4 `scripts/workflow/check_spec_structure.py`

Purpose:

- validate that a spec is complete enough to feed planning.

Required checks:

- problem
- goals
- non-goals
- invariants/constraints
- behavioral contract
- acceptance criteria
- validation expectations
- open questions section

### 3.5 `scripts/workflow/check_version_impact.py`

Purpose:

- validate that release-bearing plans and release-prep artifacts explicitly
  classify version impact.

Required checks:

- affected component(s)
- impact class
- owned contract
- rationale

## 4. Artifact field upgrades

### 4.1 Plan artifact

Plans should keep the current structure, but each work item should be treated as
an execution record, not just a task label.

Required per-item fields:

- `Source requirement or finding IDs`
- `Expected files`
- `Full quality gates`
- `Plan-compliance review evidence`
- `Commit outcome`
- `Definition of done`

Recommended `Commit outcome` format:

- `planned: one commit`
- `actual: <hash>` once complete

### 4.2 Review report

Required per-finding fields:

- stable finding ID
- severity
- file
- why it matters
- one decisive fix direction

Forbidden review states:

- unresolved open questions
- unresolved competing fix menus

### 4.3 Finding closure report

Required fields:

- original finding ID
- claimed closure status
- implementing commit(s)
- validation evidence
- closure rationale

### 4.4 Version-impact declaration

This can initially live inline in plans and release-prep docs.

Required shape:

```md
## Version Impact

- Affected component(s):
- Impact class:
- Owned contract:
- Rationale:
```

## 5. Reviewer gate alignment

Scripts should not replace the existing reviewer sub-agents. They should
reinforce them.

Desired alignment:

- `plan-quality-reviewer`
  - checks conceptual soundness
  - script checks structural completeness
- `plan-compliance-reviewer`
  - checks actual slice evidence and plan adherence
  - script checks that required fields exist before/after execution
- `review-report-quality-reviewer`
  - checks review quality
  - script checks artifact shape and forbidden patterns
- `finding-closure-reviewer`
  - checks closure reasoning
  - script checks closure artifact completeness

## 6. CI hooks

### 6.1 New workflow job: `workflow-artifacts`

Add a dedicated CI job in `.github/workflows/quality-gate.yml` or a separate
workflow file.

Responsibilities:

- run workflow artifact validators
- fail fast on invalid plan/review/closure/spec artifacts
- keep enforcement visible and separate from code build/test lanes

Suggested commands:

```sh
python3 scripts/workflow/check_plan_structure.py documentation/*.md dev-docs/**/*.md
python3 scripts/workflow/check_review_report.py dev-docs/reviews/*.md
python3 scripts/workflow/check_closure_report.py dev-docs/reviews/*closure*.md
python3 scripts/workflow/check_spec_structure.py documentation/*spec*.md
python3 scripts/workflow/check_version_impact.py documentation/*.md dev-docs/**/*.md
```

The exact glob scope can be narrowed in implementation to avoid historical
document churn.

### 6.2 Narrow initial scope

To avoid failing CI on legacy documents, phase 1 should only validate:

- newly created branch-local workflow artifacts,
- docs under a dedicated workflow-governed directory,
- files touched in the current diff where practical.

### 6.3 Release hook

Before any release-tagging or release-note prep workflow:

- `check_version_impact.py` must pass
- plan/review/closure scripts must be green for the relevant artifacts

## 7. Git hook support

Add optional local hooks under a tracked hooks directory, for example:

- `githooks/pre-commit`
- `githooks/commit-msg`

Recommended `pre-commit` responsibilities:

- run `check_plan_structure.py` on modified plan files
- run `check_review_report.py` on modified review files
- run `check_closure_report.py` on modified closure files

This should be optional locally but mandatory in CI.

## 8. Blocking rules

The following should become hard blockers:

- plan cannot become active if `check_plan_structure.py` fails
- review cannot be considered complete if `check_review_report.py` fails
- finding cannot be marked fixed if `check_closure_report.py` fails
- release-prep cannot proceed if `check_version_impact.py` fails
- plan-driven commit cannot proceed if:
  - full quality gates are not recorded,
  - `plan-compliance-reviewer` has not passed,
  - the current work item does not produce a new commit

## 9. Rollout phases

### Phase 1: structural enforcement

Implement:

- `check_plan_structure.py`
- `check_review_report.py`
- `check_closure_report.py`

Update:

- `AGENTS.md`
- templates
- CI workflow with a `workflow-artifacts` lane

Success criteria:

- new workflow artifacts are structurally validated in CI
- obvious drift stops passing silently

### Phase 2: spec and semver enforcement

Implement:

- `check_spec_structure.py`
- `check_version_impact.py`

Update:

- spec workflow
- release-prep workflow
- semver policy docs

Success criteria:

- plans cannot be derived from structurally incomplete specs
- release-bearing work cannot skip version-impact classification

### Phase 3: diff-aware enforcement

Implement optional smarter behavior:

- validate only changed artifact files by default
- detect whether a changed plan item has matching commit evidence
- detect whether a changed review finding has matching closure artifacts

Success criteria:

- lower CI noise
- better signal-to-noise on real workflow drift

## Boundary Cases

- Historical documents that predate the workflow:
  - should be excluded or grandfathered until touched
- Tiny one-file fixes with no explicit plan:
  - still follow `AGENTS.md`, but phase-1 artifact enforcement may not apply
    unless the work is explicitly plan-driven
- Review with no findings:
  - still requires valid structure, but no finding IDs
- Docs-only planning/proposal docs:
  - should not be mistaken for active executable plans unless placed in the
    governed plan location or explicitly marked active

## Acceptance Criteria

- [x] The proposal defines exact scripts to add.
- [x] The proposal defines what each script should validate.
- [x] The proposal defines exact artifact fields required for enforcement.
- [x] The proposal defines CI hook points.
- [x] The proposal defines blocking behavior.
- [x] The proposal defines a phased rollout path.

## Validation Expectations

This plan is ready to convert into an execution plan if the next step:

- picks a governed artifact scope for phase 1,
- adds the three initial validator scripts,
- adds one CI `workflow-artifacts` lane,
- updates `AGENTS.md` and templates only where needed to match the scripts.

## Open Questions

- Which exact directories should be treated as the governed artifact scope in
  phase 1:
  - only new branch-local workflow docs,
  - `documentation/` plus `dev-docs/reviews/`,
  - or only changed files in the current diff?

This should be resolved before implementing the CI hook, because it affects how
noisy the first rollout will be.
