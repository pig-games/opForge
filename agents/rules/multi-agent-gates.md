# Multi-Agent Gate Rule Pack

Load this only for triple-review or multi-agent quality-gate work.

## General rules

- Use local branch/worktree agent definitions when available.
- Reviewer subagents are read-only.
- The orchestrator/adjudicator is responsible for the final verdict.
- Do not vote-count blindly; adjudicate based on evidence.
- If configured subagents cannot launch, fail fast. Do not silently replace a
  required multi-agent review with a single-model review.
- Final output must make subagent usage auditable by naming which reviewers ran
  and summarizing each result.

## Review reports

- Triple-review outputs must become a `.md` review artifact following the active
  worktree `AGENTS.md` and review-report template.
- Final review artifacts must not leave unresolved open-question sections.
- Each material finding must give one decisive fix direction.
- If materially different fix paths remain viable, ask the user to resolve that
  choice before finalizing.
- Guided correction loops must not run indefinitely. After 3 failed correction
  cycles, stop and report the blockage.

## Spec and plan gates

- No high-value/high-ambiguity spec should be promoted to planning until the
  spec-quality gate passes.
- No high-value/high-ambiguity plan should become active until the plan-quality
  gate passes.
- No plan-driven commit should proceed until plan-compliance and required
  quality gates pass.
