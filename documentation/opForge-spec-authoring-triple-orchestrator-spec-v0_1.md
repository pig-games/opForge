# opForge Triple-Model Spec Authoring Orchestrator Specification (v0.1)

## Summary

Define a repository-native spec authoring orchestrator that uses three
model-specific subagents to help produce one final specification document. The
orchestrator is the sole writer of record and is responsible for normalization,
conflict resolution, coherence, and final readiness gating.

## Problem

The repository already has multi-model review and spec-quality orchestration,
but it does not yet have a disciplined multi-model authoring workflow for new
specifications.

Without an explicit authoring orchestrator, multi-model spec work tends to
degrade into one of these failure modes:

- three independent drafts with conflicting scope or terminology
- a stitched-together document with duplicated or incompatible sections
- silent drift between the user request, the template, and the final artifact
- speculative scope growth because no single agent owns final adjudication

The repository needs a multi-model spec authoring workflow that benefits from
three model perspectives while still producing one coherent implementation-ready
specification.

## Goals

- [ ] Produce exactly one final specification artifact from a three-model authoring workflow.
- [ ] Keep one orchestrator agent as the sole writer of the final specification document.
- [ ] Require all three leaf authoring agents to work from the same normalized problem statement and user constraints.
- [ ] Give each leaf authoring agent a distinct writing bias so the collaboration is complementary rather than redundant.
- [ ] Ensure the final specification follows the local spec template and remains suitable for direct plan derivation.
- [ ] Resolve or escalate contradictions before finalizing the artifact rather than writing competing statements into the spec.
- [ ] Prevent silent degraded execution when the requested three-model authoring set cannot actually run.

## Non-Goals

- [ ] Generating three independent spec artifacts for later manual comparison.
- [ ] Allowing leaf authoring agents to write files directly.
- [ ] Replacing the existing spec-quality review workflow.
- [ ] Smuggling implementation planning into the spec authoring workflow.
- [ ] Expanding the workflow into a general-purpose multi-document drafting system in v0.1.

## Invariants / Constraints

- The active worktree `AGENTS.md` rules remain binding throughout authoring.
- The orchestrator is the only agent allowed to create or update the final spec
  artifact.
- The orchestrator must load the active `AGENTS.md`, the local spec template,
  the normalized user request, and any source materials required by the
  requested spec before launching subagents.
- All three leaf agents must receive the same normalized scope and the same
  explicit user constraints.
- Leaf agents are read-only contributors. They may return proposed section
  content, conflicts, ambiguities, and missing information, but they must not
  edit files.
- The final artifact must follow
  [templates/spec-template.md](/Users/erik/.codex/worktrees/7175/opForge/templates/spec-template.md).
- The orchestrator must not silently continue with fewer than three leaf agents.
  If the configured three-model set cannot run, it must stop and report the
  blockage unless the user explicitly approves degraded execution.
- If material ambiguity would lead to contradictory spec content, the
  orchestrator must ask the user the smallest necessary clarifying question
  before finalization.
- The workflow must prefer repository-local scope discipline over broader or
  more speculative model suggestions.

## Behavioral Contract

The proposed agent set is:

- `Spec Authoring Orchestrator` as the GPT-5.4 writer and adjudicator of record
- `Spec Author GPT-5.4` as the structure and scope-discipline leaf author
- `Spec Author Claude Opus 4.6` as the ambiguity, invariants, and boundary-case
  leaf author
- `Spec Author Gemini 3.1 Pro` as the acceptance-criteria and validation leaf
  author


The orchestrator must execute this workflow:

1. Normalize the user request into one explicit specification scope, one target
   artifact path, and one set of explicit constraints.
2. Load the active `AGENTS.md`, the local spec template, and any required input
   documents before launching leaf agents.
3. If the target spec path already exists and the user did not explicitly ask
   for in-place update behavior, derive a new sibling spec path so the authoring
   run is auditable.
4. Launch the three leaf authoring agents in parallel using the same normalized
   scope, the same constraints, and the same required return format.
5. Run an orchestrator-local GPT-5.4 authoring pass rather than merely averaging
   the leaf outputs.
6. Deduplicate overlaps, reject speculative scope growth, reconcile
   disagreements, and keep one consistent terminology set across the final spec.
7. Ask the user for clarification if unresolved contradictions would otherwise
   become conflicting statements in the written artifact.
8. Write one final specification artifact using the local spec template.
9. Run the local `spec-quality-reviewer` after writing. For high-value or
   high-ambiguity specs, prefer escalating to the existing
   `spec-quality-orchestrator`.
10. If the quality gate fails, revise the spec and retry. Do not exceed the
    repository’s correction-loop limit.
11. Return a concise completion message naming the actual spec artifact path,
    the leaf agent set used, whether the quality gate passed, and any remaining
    user decisions that intentionally stayed out of scope.

The required leaf-agent return format is:

1. `Spec Summary`
2. `Proposed Section Content`
   Each item must be labeled by the target spec-template section.
3. `Conflicts or Ambiguities`
4. `Missing Information`
5. `Risks of Mis-Specification`

The leaf agents must not return full-file rewrites, implementation plans, or
file edits.

The required leaf-agent role biases are:

- `Spec Author GPT-5.4`
  Focus on problem framing, goals, non-goals, scope discipline, and overall
  structure quality.
- `Spec Author Claude Opus 4.6`
  Focus on hidden ambiguity, invariants, boundary cases, diagnostics, and
  internal consistency risks.
- `Spec Author Gemini 3.1 Pro`
  Focus on acceptance criteria, validation expectations, completeness of
  behavioral coverage, and whether the spec is directly plannable.

## Boundary Cases

- If any required context is missing, the orchestrator must stop and ask for it
  before launching leaf agents.
- If one or more configured leaf agents fail to launch, the orchestrator must
  not silently degrade to a two-model or one-model authoring pass.
- If two leaf agents propose materially different scope boundaries, the
  orchestrator must choose one coherent boundary or ask the user to resolve it.
- If a leaf agent proposes content outside the normalized request, the
  orchestrator must reject or explicitly quarantine that material rather than
  blending it into the final spec.
- If the user asks to update an existing spec, the orchestrator must preserve
  continuity with that spec while still normalizing conflicting new
  constraints. Unless the user explicitly requests in-place update behavior,
  the orchestrator must derive a new sibling artifact path for auditability.
- If the final draft passes internal synthesis but fails the post-write
  spec-quality gate, the spec is not complete and must return to revision.
- If no material open questions remain, the final artifact should say so
  explicitly rather than inventing placeholder uncertainty.

## Acceptance Criteria

- [ ] The orchestrator launches exactly three model-specific leaf authoring agents for normal execution.
- [ ] All leaf authoring agents receive the same normalized scope and explicit user constraints.
- [ ] Only the orchestrator writes the final specification artifact.
- [ ] The final artifact follows the local spec template and contains one coherent set of goals, non-goals, constraints, and acceptance criteria.
- [ ] The final artifact does not contain contradictory scope statements or duplicate competing sections from different leaf agents.
- [ ] The orchestrator reports the exact leaf agent set used in its final confirmation.
- [ ] The orchestrator fails fast or explicitly escalates when the requested three-model authoring workflow cannot actually be executed.
- [ ] The orchestrator runs a post-write spec quality gate and does not declare success on a failed gate.
- [ ] The workflow produces a spec that is suitable for direct plan derivation without requiring a separate normalization pass.

## Validation Expectations

- Create a pilot agent file set for the orchestrator and the three leaf authors
  under `agents/`.
- Run the workflow on at least one new spec request and one in-place spec update
  request.
- Confirm the final spec follows the local template and remains singular rather
  than multi-draft.
- Derive a plan from at least one produced spec artifact without performing an
  extra normalization pass or asking clarifying questions about existing-target
  path behavior.
- Run the branch-local `spec-quality-reviewer` on the produced artifact.
- For high-value pilot runs, also run the branch-local
  `spec-quality-orchestrator` to validate that the authoring workflow produced a
  gate-ready spec.
- Verify that simulated leaf-agent disagreement is resolved through
  adjudication or user clarification rather than contradiction in the final
  document.
- Verify that a missing-leaf-agent scenario fails clearly instead of degrading
  silently.

## Open Questions

- Whether the first implementation should write a companion audit file that
  stores the three leaf-agent outputs alongside the final merged spec.
- Whether a future version should support configurable existing-target write
  policies beyond the v0.1 default of sibling-path creation unless the user
  explicitly requests in-place update behavior.
