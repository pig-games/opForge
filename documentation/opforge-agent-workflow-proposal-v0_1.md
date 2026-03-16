# opForge Agent Workflow Proposal v0.1

## Purpose

This document proposes a repository-native agent workflow system that improves:

- review quality,
- review remediation planning,
- review finding closure verification,
- specification quality,
- spec-to-plan derivation,
- plan execution discipline,
- consistency of artifacts across agents.

The goal is to reduce repeated prompting such as:

- "follow the planning guardrails",
- "use the review format we used last time",
- "make sure the fix plan has checkboxes and validation",
- "make sure the spec is actually implementation-ready".

Instead, the repository should provide those expectations directly through:

- top-level agent rules,
- reusable skills,
- dedicated sub-agents,
- document templates,
- small supporting references.

## Current State

The current repository already has useful building blocks:

- a strong top-level [AGENTS.md](/Users/erik/Code/Retro/opForge/AGENTS.md),
- a plan execution gate in [agents/plan-compliance-reviewer.md](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/agents/plan-compliance-reviewer.md),
- a multi-model code-review stack in
  - [.github/agents/review-triple-orchestrator.agent.md](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/.github/agents/review-triple-orchestrator.agent.md),
  - [.github/agents/review-correctness.agent.md](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/.github/agents/review-correctness.agent.md),
  - [.github/agents/review-security.agent.md](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/.github/agents/review-security.agent.md),
  - [.github/agents/review-tests-quality.agent.md](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/.github/agents/review-tests-quality.agent.md).

The main missing pieces are:

- reusable instructions for creating good review reports,
- reusable instructions for creating high-quality plans from specs, reviews, or other approved work definitions,
- a mechanism to prove that a claimed review fix actually addressed the original finding,
- reusable instructions for writing specs that are implementable,
- gates that validate those artifacts before implementation begins.

## Design Principles

The system should follow these principles:

1. Artifact quality should not depend on the user remembering hidden rules.
2. Each major artifact type should have both:
   - a creation workflow,
   - a review gate.
3. Review findings should have stable identities so remediation can be tracked across review rounds.
4. Code review and process review should stay separate.
5. The repository should prefer small, composable agents over one giant "do everything" agent.
6. Templates should reduce formatting churn, not replace judgment.
7. Every gate should have a narrow job and a binary pass/fail outcome.

## Proposed System

The recommended system has four layers:

1. `AGENTS.md`
   Defines repository-wide workflow law and routing.
2. `skills/`
   Helps agents create high-quality artifacts.
3. `agents/`
   Provides sub-agent gates that validate those artifacts.
4. `templates/` and `references/workflow/`
   Give consistent shape and low-cost guidance.

## Layer 1: AGENTS.md

The top-level [AGENTS.md](/Users/erik/Code/Retro/opForge/AGENTS.md) should remain the authority for repository-wide behavior, but it should be extended with explicit workflow sections.

Recommended additions:

### 1. Artifact contract section

Define the canonical artifact types:

- specification,
- plan,
- review report,
- finding closure report,
- compliance check result,
- release/doc sync note.

For each artifact, define:

- required sections,
- expected level of detail,
- when it is needed,
- which skill should create it,
- which sub-agent should review it.

### 2. Routing section

Add explicit routing rules such as:

- if the user asks for a code review, use the triple review stack
- if the user asks for any implementation or remediation plan, use the plan-authoring skill
- if the user asks for a new design/spec, use the spec-authoring skill
- if the user is executing a plan, run the plan-compliance reviewer before commit

### 3. Gate section

Add a repository-wide gate matrix:

- no plan is active until the plan-quality reviewer passes
- no plan-driven commit is allowed until all quality gates pass
- no plan-driven commit is allowed until plan-compliance passes
- every plan work item or phase must end in a new commit before the next one begins
- no review is considered complete until the required review workflow ran
- no review finding may be marked fixed until a finding-closure reviewer passes

### 4. Templates section

State that agents should prefer repository templates for:

- specs,
- plans,
- review reports,
- finding closure reports.

This avoids repeated ad hoc structures.

## Layer 2: Skills

Add a repository `skills/` directory with a small set of workflow skills.

These skills should be short and procedural. They should point into template and reference files rather than carrying large policy text themselves.

### Skill A: opforge-review-reporting

Purpose:

- produce findings-first review reports with stable structure and severity discipline.

Use when:

- the user asks for a review,
- a sub-agent needs a normalized report format,
- an existing review needs cleanup into repo standard form.

Responsibilities:

- enforce findings-first output,
- require clarification questions to be resolved before final review finalization,
- require one decisive fix direction per finding,
- require user clarification before finalization if multiple materially
  different fix paths remain viable,
- separate findings from testing gaps and residual risks,
- discourage style-only noise,
- require concrete fix direction for each finding.

References:

- `references/workflow/review-severity-guide.md`
- `templates/review-report-template.md`

### Skill B: opforge-plan-authoring

Purpose:

- create execution-ready plans from approved inputs such as specs, reviews, or scoped user requests.

Use when:

- the user asks for an implementation plan,
- the user asks for a remediation plan,
- a spec has been accepted and needs an execution plan,
- a review has findings that need to be addressed.

Responsibilities:

- normalize different plan sources into one plan structure,
- convert source requirements or findings into ordered work items,
- define validation per work item,
- require full quality gates before each commit,
- require `plan-compliance-reviewer` before each commit,
- require checkbox tracking,
- require commit boundaries,
- require each work item or phase to end in a new commit,
- require clear "done means" rules,
- require every generated plan to restate that the active `AGENTS.md` rules
  remain binding throughout execution,
- record the plan mode, such as:
  - implementation,
  - remediation,
  - migration,
  - cleanup-only if explicitly approved.

References:

- `references/workflow/plan-slice-rules.md`
- `references/workflow/definition-of-done-matrix.md`
- `templates/plan-template.md`

### Skill C: opforge-spec-authoring

Purpose:

- write specifications that are implementation-ready rather than aspirational.

Use when:

- the user asks for a spec,
- the work involves behavior design,
- a plan is being requested from intended behavior that is not yet concretely defined.

Responsibilities:

- include goals and non-goals,
- define invariants,
- identify acceptance criteria,
- specify boundary behavior,
- specify diagnostics and failure conditions when applicable,
- identify validation expectations.

References:

- `references/workflow/spec-quality-checklist.md`
- `templates/spec-template.md`

### Skill D: opforge-review-closure

Purpose:

- prove that review findings claimed as fixed are actually resolved.

Use when:

- a remediation slice claims to close one or more review findings,
- a follow-up review should distinguish new issues from reopened old ones,
- the team wants stable evidence that a finding has really been addressed.

Responsibilities:

- require stable finding IDs in review reports,
- map each claimed fix to one or more original findings,
- require concrete evidence for closure,
- record whether the finding is:
  - fixed,
  - partially fixed,
  - not fixed,
  - superseded,
  - intentionally deferred.

References:

- `references/workflow/finding-closure-rules.md`
- `templates/finding-closure-report-template.md`

## Layer 3: Sub-agents

The repository should expand `agents/` with narrow gates. Each gate should be short, opinionated, and binary.

### Existing sub-agent to keep

- [agents/plan-compliance-reviewer.md](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/agents/plan-compliance-reviewer.md)

This should remain the execution-phase gate.

### New sub-agent A: spec-quality-reviewer.md

Purpose:

- decide whether a specification is ready for implementation or plan derivation.

Checks:

- goals are explicit,
- non-goals exist,
- boundary conditions are covered,
- acceptance criteria are testable,
- unresolved decisions are surfaced explicitly,
- validation expectations are defined.

Output:

- `PASS` only if the spec is implementation-ready
- `FAIL` if ambiguity or missing decision-making would cause drift

### New sub-agent B: review-report-quality-reviewer.md

Purpose:

- validate the quality of a produced review report.

Checks:

- findings come first,
- severity makes sense,
- findings are evidence-based,
- file references exist,
- style-only noise is not dominating,
- testing gaps are called out separately from hard findings.

Output:

- `PASS` if the review is usable as a remediation input
- `FAIL` if it is vague, padded, or structurally weak

### New sub-agent C: plan-quality-reviewer.md

Purpose:

- validate that any plan is executable and not hand-wavy.

Checks:

- the plan source is explicit,
- the plan mode is explicit,
- source requirements or findings map to one or more work items,
- ordering is sensible,
- items are commit-sized,
- validation is defined per item,
- checkbox tracking exists,
- "done" is concrete,
- no fake completeness claims exist.

Output:

- `PASS` only if the plan can be executed slice by slice
- `FAIL` if the plan is too broad, skips validation, or hides coupling

### New sub-agent D: finding-closure-reviewer.md

Purpose:

- verify that a claimed review fix actually closes the original finding.

Checks:

- the original finding is identified by a stable ID,
- the remediation slice explicitly claims that finding,
- the changed code touches the relevant execution path,
- the original failure mode no longer reproduces, or an equivalent targeted validation proves closure,
- the closure status is accurate:
  - fixed,
  - partially fixed,
  - not fixed,
  - superseded,
  - deferred.

Output:

- `PASS` only if the finding closure claim is supported by evidence
- `FAIL` if the claim is vague, incomplete, or unsupported

### Optional new sub-agent E: artifact-traceability-reviewer.md

Purpose:

- ensure traceability between spec, plan, implementation, and review.

Use only when:

- the work is large,
- multiple contributors or agents are involved,
- scope drift has been a recurring problem.

Checks:

- spec requirement -> plan item
- plan item -> implementation slice
- implementation slice -> validation
- review finding -> remediation item

This is optional because it is high-value for larger efforts and unnecessary for tiny fixes.

## Layer 4: Templates and References

Add templates:

- `templates/spec-template.md`
- `templates/review-report-template.md`
- `templates/plan-template.md`
- `templates/finding-closure-report-template.md`

Add references:

- `references/workflow/spec-quality-checklist.md`
- `references/workflow/review-severity-guide.md`
- `references/workflow/plan-slice-rules.md`
- `references/workflow/definition-of-done-matrix.md`
- `references/workflow/finding-closure-rules.md`

These should be concise and operational.

They should answer questions like:

- what makes a spec actionable,
- what severity levels mean,
- what "small commit" means,
- what completion evidence is required before checking a box.

## Triple Review Relationship

The existing triple-review system should remain the code-review layer.

Its role is:

- inspect the implementation change,
- find correctness, security, and testing risks,
- produce findings.

It should not be overloaded to:

- write the remediation plan,
- decide whether a spec is good,
- decide whether a plan is executable.

Recommended division of labor:

- triple review stack: code review
- review-report-quality-reviewer: review artifact quality
- plan-quality-reviewer: plan quality for both implementation and remediation workflows
- finding-closure-reviewer: proof that a claimed fix actually closes a prior finding
- plan-compliance-reviewer: execution compliance

## Recommended End-to-End Workflow

### A. Specification-driven feature work

1. Write spec with `opforge-spec-authoring`
2. Run `spec-quality-reviewer`
3. Build plan with `opforge-plan-authoring` in implementation mode
4. Run `plan-quality-reviewer`
5. Execute slices
6. Before each commit run all quality gates and `plan-compliance-reviewer`
7. Ensure each plan work item or phase lands as its own new commit
8. After implementation run triple review if requested

### B. Review-driven remediation work

1. Run triple review
2. Normalize output with `opforge-review-reporting`
3. Run `review-report-quality-reviewer`
4. Build plan with `opforge-plan-authoring` in remediation mode
5. Run `plan-quality-reviewer`
6. Execute slices with full quality gates and `plan-compliance-reviewer` before
   each commit
7. Ensure each plan work item or phase lands as its own new commit
8. For each claimed resolved finding, produce a closure note with `opforge-review-closure`
9. Run `finding-closure-reviewer`

### Review finding identity rule

To make closure verification work, each review finding should have a stable ID.

Recommended format:

- `RVW-2026-03-16-001`
- `RVW-2026-03-16-002`

The exact format can vary, but the key requirement is stability across:

- remediation plans,
- implementation commits,
- closure reports,
- follow-up reviews.

Each remediation plan item should list:

- the finding IDs it claims to address,
- the validation that proves closure,
- whether the slice is expected to fully or partially close the finding.

Each closure report should record:

- finding ID,
- original summary,
- implementation slice or commit,
- validation evidence,
- closure status,
- residual risk if not fully fixed.

### C. Small bug-fix work

Not every tiny bug needs the full chain.

For small work:

- code review may be skipped if the user does not ask for it
- spec-writing may be skipped if behavior is already obvious
- plan-compliance still applies if an explicit plan is being executed

This keeps the workflow strong without becoming ceremonial.

## Minimal First Rollout

To avoid process overload, the first rollout should be small.

Recommended phase 1:

1. update [AGENTS.md](/Users/erik/Code/Retro/opForge/AGENTS.md) with artifact routing and gate rules
2. add four skills:
   - `opforge-review-reporting`
   - `opforge-plan-authoring`
   - `opforge-spec-authoring`
   - `opforge-review-closure`
3. add four sub-agents:
   - `spec-quality-reviewer.md`
   - `plan-quality-reviewer.md`
   - `review-report-quality-reviewer.md`
   - `finding-closure-reviewer.md`
4. add four templates:
   - spec
   - review report
   - plan
   - finding closure report

This gives a strong workflow chain without too much initial maintenance cost.

## Recommended Second Rollout

Phase 2:

1. refine plan-mode guidance and traceability references
2. add optional traceability reviewer
3. add helper scripts for opening artifact templates and validating checklist discipline

This second phase is worth doing once the first phase has proven useful.

## Example Repository Layout

Suggested final layout:

```text
agents/
  artifact-traceability-reviewer.md
  plan-compliance-reviewer.md
  finding-closure-reviewer.md
  plan-quality-reviewer.md
  review-report-quality-reviewer.md
  spec-quality-reviewer.md

skills/
  README.md
  opforge-review-closure/
    SKILL.md
    references/
  opforge-review-reporting/
    SKILL.md
    references/
  opforge-plan-authoring/
    SKILL.md
    references/
  opforge-spec-authoring/
    SKILL.md
    references/

templates/
  finding-closure-report-template.md
  plan-template.md
  review-report-template.md
  spec-template.md

references/
  workflow/
    definition-of-done-matrix.md
    finding-closure-rules.md
    plan-modes-guide.md
    plan-slice-rules.md
    review-severity-guide.md
    spec-quality-checklist.md
    traceability-guide.md

scripts/
  workflow/
    check_plan_checkboxes.py
    new_artifact_from_template.sh
```

## Decision Summary

The recommended workflow architecture is:

- `AGENTS.md` for law,
- skills for creation,
- sub-agents for gates,
- templates for structure,
- references for concise guidance,
- triple review for code-review depth,
- stable finding IDs and closure reports for anti-regression discipline.

That combination gives:

- better default artifact quality,
- less repeated prompting,
- less planning churn,
- better traceability,
- stronger gates without one monolithic workflow agent.

## Recommended Next Step

Implement phase 1 first:

1. add the core workflow skills
2. add the new review/gate sub-agents
3. add the templates and references
4. update [AGENTS.md](/Users/erik/Code/Retro/opForge/AGENTS.md) to route tasks into them

That is the highest-value improvement with the lowest process overhead.
