# opForge Multi-Subagent Candidate Evaluation v0.1

## Purpose

This note evaluates the existing non-triple-review subagents as candidates for
an orchestrated multi-subagent workflow similar to
`review-triple-orchestrator.agent.md`.

The question is not whether these agents are useful. They already are.
The question is whether a parent orchestrator that runs multiple independent
subagents and then performs a final adjudication would materially improve the
decision quality enough to justify the extra workflow cost.

## Evaluation Criteria

A subagent is a good candidate for multi-subagent treatment when most of the
following are true:

- the task is judgment-heavy rather than mostly deterministic
- independent reviewers can catch materially different failure modes
- disagreement between reviewers is informative rather than just noise
- the output can be normalized into one final artifact or pass/fail decision
- the workflow is important enough that extra latency and cost are justified
- the task is not run so frequently that orchestration overhead becomes a drag

A subagent is a poor candidate when most of the following are true:

- the task is a narrow binary gate against explicit repository rules
- there is little room for legitimate expert disagreement
- repeated use makes latency and overhead matter more than marginal insight
- multiple reviewers would mostly restate the same checklist
- the repository benefits from one clear authority rather than consensus

## Summary Ranking

### Strong candidates

- `Artifact Traceability Reviewer`
- `Plan Quality Reviewer`
- `Spec Quality Reviewer`

### Conditional candidates

- `Finding Closure Reviewer`
- `Review Report Quality Reviewer`

### Poor candidates

- `Plan Compliance Reviewer`

### Already the leaves of a multi-agent stack

- `Review GPT-5.4`
- `Review Claude Opus 4.6`
- `Review Gemini 3.1 Pro`

These three are already the parallel leaf reviewers used by the triple-review
workflow. They are not new candidates for a similar treatment; they are the
current treatment.

## Per-Agent Evaluation

### Artifact Traceability Reviewer

Recommendation: strong candidate.

Why it fits:

- Traceability checks span several artifacts, which creates real room for
  reviewers to miss different broken links.
- Independent passes are likely to surface different mapping failures between
  source artifact, plan item, implementation slice, validation evidence, and
  closure record.
- The existing agent is already scoped as an optional gate for larger efforts,
  so the extra orchestration cost is acceptable.

Why not default everywhere:

- Small changes do not need multi-agent traceability review.
- The value appears only once there are enough artifacts and handoffs for drift
  to become plausible.

Recommended shape:

- one orchestrator
- two or three read-only traceability reviewers
- one final adjudicator that emits a single `PASS` or `FAIL`

Best use:

- multi-round remediation
- large implementation plans
- efforts with reopened findings or weak plan-to-code traceability

### Plan Quality Reviewer

Recommendation: strong candidate.

Why it fits:

- Plan quality is not purely mechanical. Sequencing, slice sizing, validation
  sufficiency, source-to-work-item mapping, and hidden scope growth all involve
  judgment.
- Independent reviewers can catch different weaknesses, especially around
  oversized slices, missing validation, and mismatches between source artifact
  and planned work.
- A final adjudicator can merge findings into one executable plan gate result
  without much ambiguity.

Main risk:

- Multiple reviewers may try to "improve" the plan beyond the source scope.
- The orchestrator would need strong instructions to reject gold-plating and to
  preserve the repository's implementation-first bias.

Recommended shape:

- one orchestrator
- two or three plan-quality reviewers with the same plan and source artifact
- one final merged `PASS` or `FAIL` result plus a deduplicated issue list

Best use:

- major implementation plans
- remediation plans derived from review findings
- plans that will drive multiple commits or multiple agents over time

### Spec Quality Reviewer

Recommendation: strong candidate.

Why it fits:

- Specification quality is highly judgment-heavy. Different reviewers are likely
  to notice different missing invariants, hidden ambiguity, or weak acceptance
  criteria.
- The repository already treats specification readiness as a gate before
  planning, so improving confidence here has downstream value.
- A merged result can still remain binary: ready or not ready.

Main risk:

- Specs attract opinionated expansion. Without strict scope discipline,
  multiple reviewers can turn a narrow spec into a broader design exercise.

Recommended shape:

- one orchestrator
- one reviewer biased toward clarity and scope discipline
- one reviewer biased toward invariants and boundary behavior
- optionally one reviewer biased toward validation and acceptance criteria
- one final adjudicator that keeps only implementation-relevant gaps

Best use:

- new behavioral specs
- CPU-family extension specs
- any spec that will feed a large downstream plan

### Finding Closure Reviewer

Recommendation: conditional candidate.

Why it might fit:

- Closure verification can be subtle when evidence is partial or when a fix only
  addresses part of the original failure mode.
- Independent reviewers can be useful for distinguishing `fixed` from
  `partially fixed`, especially when the implementation touches the right area
  but the validation evidence is thin.

Why it is not a top-tier fit:

- Closure review should stay tightly anchored to one original finding ID, one
  claimed closure report, and one implementation slice.
- Too much reviewer discretion here can produce unnecessary disagreement on what
  should remain a disciplined evidence check.

Recommended shape:

- do not make this the default path
- use a dual-review or "escalated closure" mode only for disputed or high-risk
  findings
- keep the final output binary and tied to the original finding ID

Best use:

- security findings
- regressions that have reopened before
- closure claims with incomplete validation evidence

### Review Report Quality Reviewer

Recommendation: conditional candidate.

Why it might fit:

- Review artifact quality involves judgment about severity discipline,
  decisiveness of fix direction, and whether the report is actionable for
  planning and closure.
- Independent passes can catch weak or ambiguous writeups that one reviewer may
  accept too easily.

Why it is only conditional:

- This is a meta-review of a review artifact. Running several reviewers here can
  add noticeable process overhead without improving the underlying code review.
- If the repository already has a high-quality triple-review output, the extra
  gain from multi-agent review-of-review is limited.

Recommended shape:

- reserve for major review artifacts that will drive significant remediation
- prefer two reviewers, not three
- keep one final adjudicator responsible for the gate result

Best use:

- full-worktree reviews
- release-blocking reviews
- reviews that will become the source for a remediation plan

### Plan Compliance Reviewer

Recommendation: poor candidate.

Why it does not fit:

- This agent is a per-commit execution gate, so latency matters a lot.
- Its job is primarily to check adherence to explicit plan and repository rules,
  not to solicit diverse perspectives.
- The repository benefits from one clear authority on whether the current slice
  is allowed to proceed.
- Multi-agent compliance review would likely produce duplicated checklist output
  and slow down the exact loop that is supposed to stay tight.

Best treatment instead:

- keep it single-authority
- improve prompt precision and required inputs rather than adding more agents
- optionally pair it with `Artifact Traceability Reviewer` for large efforts,
  but as a separate escalation path rather than a bundled consensus workflow

## Recommended Rollout Order

If the repository wants to expand beyond triple review, the best order is:

1. `Spec Quality Reviewer` orchestrator
2. `Plan Quality Reviewer` orchestrator
3. `Artifact Traceability Reviewer` orchestrator
4. optional escalated `Finding Closure Reviewer` dual-review mode
5. optional `Review Report Quality Reviewer` dual-review mode

`Plan Compliance Reviewer` should remain single-agent.

## Proposed Design Rule

Use multi-subagent orchestration only for artifact-quality or cross-artifact
judgment problems where independent blind-spot detection is valuable.

Do not use it for high-frequency execution gates whose main job is to enforce
explicit repository law.

That rule preserves the current logic of the repository:

- triple review for implementation-risk discovery
- single-agent compliance for commit-time discipline
- optional multi-agent escalation for high-value artifact and traceability gates