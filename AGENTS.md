# opForge Agent Operating Contract

This file is the always-loaded contract for AI coding agents working in this
worktree. Keep it short. Load task-specific rule packs only when the current
work requires them.

## Non-negotiable safety rules

- Do not run `git push`, `git push --force`, or any command that updates a remote
  branch unless the user explicitly asks for that exact push in the current
  conversation.
- Treat every push as opt-in and one-time scoped: prior approval for a previous
  push does not authorize later pushes.
- Do not use, launch, automate, recommend, or rely on GitKraken in this worktree.
- Use non-interactive `git` commands and repository-local workflow scripts
  instead of GUI git tools.
- Do not install, import, add, recommend, vendor, execute, or otherwise touch
  `litellm` in repo files, helper environments, docs, examples, CI, or generated
  instructions. If discovered, report it as a security issue.

## Repository workflow anchors

- The root `agents/` directory is the canonical source for custom-agent
  definitions.
- `.github/agents/` entries exist only as symlinks so VS Code can list the same
  agents in the Agent selector; do not maintain separate copied definitions
  there.
- Prefer local workflow skills under `skills/` for branch-local workflow
  artifact work.
- Implementation-focused opForge skills may exist outside this worktree in the
  active Codex environment. Use them when available for implementation work.

## Primary operating mode

You are an implementation agent. Make concrete forward progress toward working
behavior in the smallest viable vertical slice.

Optimize for:

1. working implementation
2. narrow scope
3. observable progress
4. honest blocker reporting

Do not optimize for cleanup, speculative refactoring, broad test expansion, or
architectural elegance unless required to unblock the requested slice.

## Implementation-first rules

- For implementation tasks, change production code before tests, docs, renames,
  formatting, or cleanup.
- Stay inside the smallest viable slice: one path, one backend, one platform, or
  one happy-path validation is preferred over broad coverage too early.
- Do not broaden scope without explicit user direction.
- Refactor only when it directly unblocks the current slice, removes a blocker,
  or is smaller and safer than an awkward workaround.
- Tests support implemented behavior; they do not replace implementation
  progress.
- Report blockers precisely instead of hiding them with peripheral cleanup.
- Prefer a host-specific or platform-specific first implementation when that
  gets the feature working faster, but do not place target
  CPU/family/dialect-specific semantics in generic VM/native implementation
  paths. CPU-specific behavior belongs in package VM/family/dialect
  definitions.

## Required execution header

Before edits, provide a short header:

```md
#### Execution header

- Goal:
- Current slice:
- Definition of done for this slice:
- Expected files to change:
- First concrete code change:
- Validation to run:
```

## Required progress log

At the end of each work block, report:

```md
#### Progress log

- Production code changed:
- Behavior now implemented:
- Validation status:
- Unresolved issue:
- Next concrete implementation step:
```

## Git staging and commits

- Do not chain `git status`, `git add`, and `git commit` in one ad hoc command.
- Prefer `scripts/workflow/stage_and_commit.sh` when staging an explicit file set
  and creating one focused commit.
- Keep workflow artifacts out of code commits unless the artifact itself changed
  for a clear reason.
- Every plan work item or phase should end in a focused commit before the next
  item begins.

## Validation baseline

- Before committing Rust code changes, run `scripts/workflow/run_rust_quality_gate.sh`
  or `make quality-gate`, plus focused tests for the slice when useful.
- For workflow, agent, skill, plan, spec, review, closure, CI, or template
  changes, run `make workflow-gate` or the relevant `scripts/workflow/check_*.py`
  validator and record the result.

## CPU-specific architecture boundary

Generic opForge Rust VM, Native VM, workflow, and CLI implementation paths must
not grow CPU/family/dialect/register/addressing-mode/instruction-specific logic.

CPU-specific behavior belongs in package VM definitions, family/dialect
packages, fixtures, examples, tests, or documentation.

Current deterministic enforcement is scoped to architecture-neutral core,
shared type, root `src/`, workflow implementation paths, and `native/**`.
Native assembly is scanned structurally for implementation-owned identifiers and
metadata such as labels, macro names, constant names, and section/module names.
The guard intentionally does not treat assembler instruction syntax or ordinary
directives as violations. The workflow also reports a warning-only scan over
selected broader Rust implementation crates so future promotion candidates stay
visible without failing the gate. Rust test files are excluded from both scopes.

The deterministic guard is:

```sh
python3 scripts/workflow/check_cpu_specific_arch_boundary.py
```

This guard is run by the Rust quality gate and workflow gate. If it fails,
prefer:

1. moving the logic into package VM/family/dialect definitions,
2. renaming accidental generic identifiers,
3. adding a narrow reviewed allowlist entry with a concrete reason.

## Workflow skills

Use workflow skills only for workflow artifact work, not ordinary feature coding:

- `skills/opforge-spec-authoring/SKILL.md` for behavior specifications
- `skills/opforge-plan-authoring/SKILL.md` for implementation/remediation plans
- `skills/opforge-review-reporting/SKILL.md` for code review reports
- `skills/opforge-review-closure/SKILL.md` for closure evidence

Generated plans must state that this active `AGENTS.md` remains binding during
execution.

## Workflow artifact contract

Governed spec, plan, review, and closure artifacts must carry workflow
provenance showing the matching local skill and workflow wrapper entrypoint.
Artifact bundle validation may reject governed artifacts that lack matching
workflow provenance.

Preferred templates:

- `templates/spec-template.md`
- `templates/plan-template.md`
- `templates/review-report-template.md`
- `templates/finding-closure-report-template.md`

Plan-driven work must not commit until the plan-compliance reviewer passes and
all required quality gates pass. No review finding should be marked fixed until
the finding-closure reviewer passes.

## Task-specific rule packs

Load only the rule packs relevant to the current task:

- `agents/rules/native-68000.md` when touching `native/motorola68000/**/*.asm`
- `agents/rules/fs-uae.md` when running or debugging FS-UAE tests
- `agents/rules/release-notes.md` for release-bearing work
- `agents/rules/reference-refresh.md` for golden/reference updates
- `agents/rules/workflow-artifacts.md` for specs, plans, reviews, closure reports,
  templates, skills, workflow agents, CI gates, or workflow validators
- `agents/rules/multi-agent-gates.md` for triple-review or multi-agent quality gates

Optional helper:

```sh
python3 scripts/workflow/workflow_scope_hint.py <changed paths...>
```

## Decision heuristic

When unsure, make one narrow path work, validate it, and report exactly what
remains. Prefer narrow working implementation over elegant incompletion.
