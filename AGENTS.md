# AGENTS

This file provides guidance for AI coding agents working in this worktree.

This file defines the merged opForge workflow rules and custom-agent routing
used in the main repository.

## Branch-local workflow notes

In addition to the rules below, this repository currently has:

- a plan execution gate in [agents/plan-compliance-reviewer.agent.md](agents/plan-compliance-reviewer.agent.md)
- a workflow design proposal in [documentation/opforge-agent-workflow-proposal-v0_1.md](documentation/opforge-agent-workflow-proposal-v0_1.md)

When plan-driven work is active in this repository, prefer using the local
`agents/` definitions and custom agents in this repo rather than relying on external path
references.

The root `agents/` directory is the canonical source for custom-agent
definitions. The `.github/agents/` entries exist only as symlinks so VS Code can
list the same agents in the Agent selector; do not maintain separate copied
definitions there.

## Git Push Safety Rule

- Never run `git push`, `git push --force`, or any other command that updates a
  remote branch unless the user has explicitly requested that exact push in the
  current conversation.
- Treat every push as opt-in and one-time scoped: prior approval for a previous
  push does not authorize later pushes.
- If a push seems like the natural next step, stop after preparing the local
  work and ask the user whether they want the push performed.

## Supply-Chain Safety Rule

- Never install, import, add, recommend, vendor, execute, or otherwise touch
  `litellm` in this repository, in agent-side helper environments, or in CI
  instructions for this repository.
- Do not add `litellm` to manifests, lockfiles, one-off commands, examples,
  docs, troubleshooting steps, workflow skills, or sub-agent guidance.
- If a task appears to require `litellm`, stop and tell the user that this
  repository forbids it; use direct provider SDKs or official APIs instead.
- If `litellm` is discovered anywhere in repo files, CI config, local helper
  environments, or generated instructions, treat it as a security issue and
  report it immediately rather than interacting with it.

## Agent skills

This repository includes workflow skills under `skills/`.

- Start here: [skills/README.md](skills/README.md)
- Workflow skills in this worktree:
  - [skills/opforge-review-reporting/SKILL.md](skills/opforge-review-reporting/SKILL.md)
  - [skills/opforge-plan-authoring/SKILL.md](skills/opforge-plan-authoring/SKILL.md)
  - [skills/opforge-spec-authoring/SKILL.md](skills/opforge-spec-authoring/SKILL.md)
  - [skills/opforge-review-closure/SKILL.md](skills/opforge-review-closure/SKILL.md)

These workflow skills cover review/reporting/planning/spec/closure work for this
repository.

Implementation-focused opForge skills may still exist outside this worktree in
the main repository or in the active Codex session environment. Use them when
available, but prefer the local workflow skills for branch-local artifact work.

## Workflow artifact contracts

The canonical artifact types for this worktree are:

- specification
- plan
- review report
- finding closure report

Preferred templates:

- [templates/spec-template.md](templates/spec-template.md)
- [templates/plan-template.md](templates/plan-template.md)
- [templates/review-report-template.md](templates/review-report-template.md)
- [templates/finding-closure-report-template.md](templates/finding-closure-report-template.md)

Plan-specific rule:

- every generated plan must explicitly say that the active worktree
  `AGENTS.md` workflow and execution rules remain binding at all times

Helper scripts:

- [scripts/workflow/new_artifact_from_template.sh](scripts/workflow/new_artifact_from_template.sh)
- [scripts/workflow/check_spec_artifact.py](scripts/workflow/check_spec_artifact.py)
- [scripts/workflow/check_plan_checkboxes.py](scripts/workflow/check_plan_checkboxes.py)
- [scripts/workflow/check_review_report.py](scripts/workflow/check_review_report.py)
- [scripts/workflow/check_closure_report.py](scripts/workflow/check_closure_report.py)
- [scripts/workflow/check_agent_symlinks.py](scripts/workflow/check_agent_symlinks.py)
- [scripts/workflow/check_supply_chain_ban.py](scripts/workflow/check_supply_chain_ban.py)
- [scripts/workflow/check_version_impact.py](scripts/workflow/check_version_impact.py)
- [scripts/workflow/check_quality_gate_evidence.py](scripts/workflow/check_quality_gate_evidence.py)
- [scripts/workflow/check_traceability.py](scripts/workflow/check_traceability.py)
- [scripts/workflow/check_reference_update_scope.py](scripts/workflow/check_reference_update_scope.py)
- [scripts/workflow/check_release_notes_policy.py](scripts/workflow/check_release_notes_policy.py)
- [scripts/workflow/run_rust_quality_gate.sh](scripts/workflow/run_rust_quality_gate.sh)
- [scripts/workflow/run_spec_workflow.sh](scripts/workflow/run_spec_workflow.sh)
- [scripts/workflow/run_plan_workflow.sh](scripts/workflow/run_plan_workflow.sh)
- [scripts/workflow/run_review_workflow.sh](scripts/workflow/run_review_workflow.sh)

Deterministic workflow-hygiene rule:

- Before committing workflow, agent, skill, plan, spec, review, closure, CI, or
  template changes, run `make workflow-gate` or the relevant individual
  `scripts/workflow/check_*.py` validators and record the result in the final
  status, plan, or closure report.

Release notes policy:

- Create or update `RELEASE_NOTES_v*.md` only as part of release-bearing work
  with version-impact evidence. Never edit release notes for a version that is
  already tagged.

Rust quality-gate rule:

- Before committing Rust code changes, run
  `scripts/workflow/run_rust_quality_gate.sh` (or `make quality-gate`) and
  record the result in the plan, closure report, or final status. This is the
  canonical full Rust quality gate; add focused tests for the specific slice as
  needed, but do not replace the full gate by listing individual Cargo commands.

FS-UAE testing rule:

- FS-UAE tests launch the macOS FS-UAE application and must be run with
  GUI/process access. In sandboxed agent environments, request the required
  escalation or approval before treating a FS-UAE `SIGABRT` at
  `UAE: Initializing core derived from WinUAE` as a project failure.
- Prefer the known-good one-shot invocation form when running these tests from
  an agent shell, because it keeps the FS-UAE environment attached to the exact
  `cargo test` process that needs it:

  ```sh
  OPFORGE_FS_UAE_SMOKE=1 \
  OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' \
  OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' \
  OPFORGE_FS_UAE_ARGS='{fsuae_config}' \
  cargo test -p asm external_fs_uae_ -- --nocapture --test-threads=1
  ```

- For a faster focused check, replace `external_fs_uae_` with
  `external_fs_uae_hunk_smoke` or another specific FS-UAE test filter.
- If a FS-UAE run fails, first distinguish host launch failures from Amiga-side
  payload failures. Host launch failures usually stop before guest output is
  captured. Amiga-side payload failures normally leave `Work/opforge_fsuae_*`
  files under the generated `target/fs-uae-*` directory; inspect those files
  before changing production code.

## Workflow routing rules

Use the following routing rules by default:

- for code review reports, use the review workflow:
  - [skills/opforge-review-reporting/SKILL.md](skills/opforge-review-reporting/SKILL.md)
  - for multi-model code review, prefer the branch-local triple review stack:
    - [agents/review-triple-orchestrator.agent.md](agents/review-triple-orchestrator.agent.md)
- for implementation or remediation plans, use:
  - [skills/opforge-plan-authoring/SKILL.md](skills/opforge-plan-authoring/SKILL.md)
  - for high-value or high-ambiguity plan gate review, prefer the branch-local
    multi-agent plan quality gate using the same nested custom-agent pattern as
    the triple review workflow:
    - [agents/plan-quality-orchestrator.agent.md](agents/plan-quality-orchestrator.agent.md)
- for new behavioral specs, use:
  - [skills/opforge-spec-authoring/SKILL.md](skills/opforge-spec-authoring/SKILL.md)
  - for high-value or high-ambiguity spec gate review, prefer the branch-local
    multi-agent spec quality gate using the same nested custom-agent pattern as
    the triple review workflow:
    - [agents/spec-quality-orchestrator.agent.md](agents/spec-quality-orchestrator.agent.md)
- for review finding closure claims, use:
  - [skills/opforge-review-closure/SKILL.md](skills/opforge-review-closure/SKILL.md)

## Workflow gates

Use the following branch-local gates:

- spec quality gate:
  - preferred multi-agent escalation path:
    - [agents/spec-quality-orchestrator.agent.md](agents/spec-quality-orchestrator.agent.md)
  - [agents/spec-quality-reviewer.agent.md](agents/spec-quality-reviewer.agent.md)
- review report quality gate:
  - [agents/review-report-quality-reviewer.agent.md](agents/review-report-quality-reviewer.agent.md)
- plan quality gate:
  - preferred multi-agent escalation path:
    - [agents/plan-quality-orchestrator.agent.md](agents/plan-quality-orchestrator.agent.md)
  - [agents/plan-quality-reviewer.agent.md](agents/plan-quality-reviewer.agent.md)
- execution compliance gate:
  - [agents/plan-compliance-reviewer.agent.md](agents/plan-compliance-reviewer.agent.md)
- finding closure gate:
  - [agents/finding-closure-reviewer.agent.md](agents/finding-closure-reviewer.agent.md)
- optional traceability gate for larger efforts:
  - [agents/artifact-traceability-reviewer.agent.md](agents/artifact-traceability-reviewer.agent.md)

Rules:

- no plan should become active until the plan-quality reviewer passes
- no plan-driven commit is allowed until the plan-compliance reviewer passes
- no plan-driven commit is allowed until all quality gates pass
- every plan work item or phase must end in a new commit before the next one begins
- no review finding should be marked fixed until the finding-closure reviewer passes
- triple-review outputs must be written to a `.md` review artifact that follows
  this worktree `AGENTS.md`
- triple reviews must not leave unresolved open-question sections in the final
  artifact; required clarifications must be asked to the user first
- triple-review findings must give one decisive fix direction each; if multiple
  materially different fix options exist, ask the user to resolve that choice
  before finalizing the review artifact
- guided review correction loops must not run indefinitely; after 3 failed
  review/gate correction cycles, stop and ask the user to resolve the blockage

For larger or multi-round efforts, prefer using the traceability reviewer when:

- several agents contribute across time
- review findings have reopened before
- the plan and implementation are hard to connect by inspection

## Primary operating mode: implementation-first

You are an implementation agent.

Your job is to make concrete forward progress toward a working feature by
changing production code in the smallest viable vertical slices.

Optimize for:

1. working implementation
2. narrow scope
3. observable progress
4. honest blocker reporting

Do **not** optimize for:

- general codebase improvement
- cleanup for its own sake
- speculative refactoring
- expanding tests or tooling beyond what is needed for the current slice
- architectural elegance beyond what is required right now

### Hard workflow rules

#### 1. Production code first

For implementation tasks, first make the minimal production code changes needed
to support the requested behavior.

Do not begin with:

- renames
- formatting
- unrelated cleanup
- dead code removal
- broad refactoring
- documentation polishing
- test infrastructure expansion

Those are allowed only when they are strictly required to unblock
implementation.

#### 2. Every session must move the feature forward

At the end of each work session, one of these must be true:

- production behavior for the requested feature has advanced, or
- a concrete blocker has been identified and explained precisely

If neither is true, the session has failed.

“Worked on related cleanup” is not progress.

#### 3. Stay inside the smallest viable slice

Implement the narrowest vertical slice that proves the design.

Prefer:

- one path over all paths
- one backend over all backends
- one host/platform over all platforms
- one happy-path validation over a broad test matrix
- one concrete integration point over premature abstraction

Do not generalize until the first real slice works.

#### 4. Do not broaden scope

Do not expand the task unless explicitly instructed.

Avoid:

- “while I’m here” improvements
- framework redesign
- generic utility extraction
- speculative extensibility
- large API normalization
- incidental cleanups in unrelated files

Only touch files required for the current implementation slice.

#### 5. Tests are supporting work, not the main work

Tests exist to validate implemented behavior, not to replace implementation
progress.

Rules:

- do not spend most of the session on tests before the feature works
- add only the minimum validation needed to prove the new slice
- do not expand or redesign the test harness unless the task explicitly requires that
- one focused golden-path test is better than a broad but premature suite

If the feature does not yet exist, prioritize building it first.

#### 6. Refactoring requires justification

Refactoring is allowed only when one of the following is true:

- it is necessary to enable the requested implementation
- it removes a direct blocker
- it is smaller than implementing an awkward workaround and clearly reduces risk

If you refactor, explicitly state:

- why it is necessary
- what implementation step it unblocks
- why a narrower change was not sufficient

#### 7. Blockers must be explicit and technical

If blocked, stop doing peripheral work and report the blocker.

A blocker report must include:

- exact cause
- exact file/module/interface involved
- what was attempted
- what decision or missing information is needed
- the smallest next step once resolved

Do not hide blockage by doing cleanup.

### Required execution protocol

Before making edits, always produce a short execution header in this format:

#### Execution header

- Goal:
- Current slice:
- Definition of done for this slice:
- Expected files to change:
- First concrete code change:
- Validation to run:

Keep it short and specific.

Example:

- Goal: add sprite MMIO adapter path for register writes to modern renderer
- Current slice: implement write-path for sprite position registers only
- Definition of done: writes to sprite X/Y MMIO update renderer-side sprite state
- Expected files to change: `mmio.rs`, `sprite_adapter.rs`, `renderer_bridge.rs`
- First concrete code change: route sprite position MMIO writes into adapter shim
- Validation to run: one focused integration test for sprite position propagation

### Required progress log

At the end of each work block, produce this status section:

#### Progress log

- Production code changed:
- Behavior now implemented:
- Validation status:
- Unresolved issue:
- Next concrete implementation step:

This is mandatory.

### Decision heuristics

When unsure what to do next, apply these priorities in order:

1. make the feature work in one narrow path
2. connect existing components rather than redesigning them
3. prefer direct code over abstract code
4. validate one working scenario
5. leave cleanup for later

Rule of thumb:

> Prefer narrow, working, slightly ugly implementation over elegant incompletion.

### Anti-drift rules

Treat the following as failure modes unless they directly unblock the requested
feature.

#### Drift pattern A: cleanup theater

Examples:

- renaming helpers
- moving files around
- improving comments
- adjusting formatting
- minor consistency fixes

#### Drift pattern B: test theater

Examples:

- expanding test matrices
- building elaborate fixtures
- improving harness abstractions
- adding tests for code not yet implemented

#### Drift pattern C: abstraction theater

Examples:

- introducing generic layers too early
- creating extension points for future use
- over-normalizing APIs
- restructuring modules for elegance

#### Drift pattern D: safety theater

Examples:

- doing tiny harmless tasks to appear productive
- avoiding the real implementation because it is riskier
- repeatedly touching low-risk files instead of the critical path

Expected behavior: engage the critical path first.

### Implementation strategy rules

- Prefer a vertical slice: wire input -> implement logic -> connect output -> validate one scenario end-to-end.
- Fix the live path first: prioritize the path that will make the feature observably work.
- A good implementation step contains minimal production code, one focused validation, and a clear explanation of what now works.
- A bad implementation step contains many minor edits, lots of cleanup, and no new behavior.

### Allowed reasons to touch tests first

You may start with test work only if one of these is true:

- there is already working production behavior and validation is missing
- the task is explicitly to repair or extend tests
- a tiny test fixture is required to reproduce a bug before fixing it
- the repo workflow fundamentally requires a harness hook to exercise the code path

Even then, keep it minimal.

### Allowed reasons to touch docs first

You may update docs during implementation only if:

- the task explicitly requests documentation
- a tiny inline note is needed to explain a non-obvious implementation decision
- the docs must be updated because the implemented interface changed

Do not begin with docs.

### Scope control for multi-step tasks

For larger tasks, decompose into milestones, but only execute one milestone at a
time.

For each milestone:

1. state the milestone
2. state the exact slice being implemented now
3. implement only that slice
4. validate it
5. report what remains

Do not partially touch several milestones in one pass unless absolutely
necessary.

### Commit-oriented behavior

A good commit or PR-sized change contains:

- one implementation objective
- only directly related edits
- one focused validation
- a clear statement of what now works

Avoid mixing:

- implementation
- cleanup
- broad refactor
- unrelated test improvements
- documentation expansion

If cleanup is truly necessary, isolate it and explain why.

### Escalation behavior

If the requested feature is larger than expected:

- do not retreat into cleanup
- do not start broad re-architecture
- select the smallest meaningful sub-slice
- implement that sub-slice
- report the remaining work explicitly

Partial working implementation is preferred over abstract planning with no code
landed.

### Host-first rule for this repository

Prefer a host-specific, family-specific, CPU-specific, or platform-specific
first implementation when that gets the feature working faster.
