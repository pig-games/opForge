# opForge Agent Operating Contract

Deliver meaningful states Erik can inspect and test quickly, at low total cost.
The [workflow guide](documentation/workflow/README.md) is the canonical reference
for working in this repository. The current user request defines the assignment;
historical documents do not activate work or grant authority.

## Safety and existing engineering boundaries

- Preserve unrelated and uncommitted work. Use an isolated worktree for cleanup
  and experiments that would disrupt another task. Avoid destructive Git operations.
- Do not push or update remote branches unless Erik explicitly asks for that exact
  push in the current conversation. Each authorization covers one push only.
- Use non-interactive Git. Do not use, launch, automate, recommend or rely on GitKraken.
- Do not install, import, add, recommend, vendor, execute or otherwise touch
  `litellm`. If discovered, report it as a security issue.
- CPU/family/dialect semantics belong in package definitions, fixtures and their
  specialized implementation boundaries, never in generic VM, native, workflow
  or CLI paths. Preserve the existing architecture-boundary checks.
- Generic directives such as `.byte`, `.word` and `.org` belong to shared core
  processing. They must not dispatch through CPU/family-specific operand parsers
  or instruction encoders. Dot-prefixed statement names are resolved by shared
  directive/macro/segment handling; unresolved names must not fall through to
  instruction processing.
- Before 1.0, retain only the latest supported state of each bytecode/VM contract.
  Version identifiers detect mismatches; they do not require legacy executors.
  Migrate affected package generation and Rust/native execution together, removing
  superseded program versions rather than adding compatibility paths.
- Production behavior must not be selected by test or benchmark identity, fixture
  or output paths, self-host generation, or expected output. Do not weaken tests or substitute stale results for proof.
- Preserve the approved native instrumentation and fresh-run parity safeguards.
  A timeout, crash, partial capture or launcher success is not native completion.

## Working method

- Understand the affected responsibilities and surrounding behavior before choosing
  a solution. Aim for the smallest coherent outcome, not the smallest local diff.
- Prefer compact, clear code. Address accumulating responsibilities and repeated
  special cases while working in an area; discuss consequential redesigns.
  Large or rapidly growing files are a warning to investigate, not a reason to
  split code mechanically or compress it to satisfy a line count.
- Within agreed scope, investigate, edit, validate and commit autonomously.
  Discuss significant tradeoffs, scope changes and unresolved intent. Respect
  requests for interactive work without turning routine actions into approvals.
- Use plans, specifications and independent reviews when they help the task.
  They are not prerequisites for ordinary implementation or commits.
- Automate recurring deterministic work. Delegate bounded tasks to the least
  expensive capable model when savings exceed briefing, coordination, verification
  and rework. Keep small, context-heavy tasks local. The coordinator owns integration.
  Explicitly select GPT-6 Luna (`gpt-6-luna`) for straightforward bounded tasks
  and GPT-6 Sol (`gpt-6-sol`) for implementation, debugging, review and routine
  coordination. Reserve GPT-6 Astra (`gpt-6-astra`) for genuinely complex problems
  and high-level architectural design and planning.
  Follow the workflow guide's delegation guidance; keep tool/model selection
  configurable and account for availability and demonstrated task performance.
- For experiments, establish a hypothesis, baseline, correctness comparison and
  success/stop conditions. Preserve a working reference path while evaluating a
  replacement. Performance claims require reproducible comparative measurements.
- Before integration, inspect the combined result, remove superseded experiments
  and duplication where appropriate, and explain any remaining compromises.

## Commits and validation

- Commit useful recovery points and coherent changes, including documentation.
  Checkpoint commits may be incomplete or failing: identify known limitations.
  Commits do not imply approval, completion or readiness for integration.
- Inspect the staged diff and keep unrelated work out. The optional
  `scripts/workflow/stage_and_commit.sh` stages explicit paths only and refuses
  a pre-existing staged change outside those paths.
- Run focused checks while developing. Qualify coherent changes with affected
  subsystem checks and relevant engineering guards before declaring integration
  readiness. Run broad qualification at meaningful completion boundaries or
  when risk or failures warrant it, independently of commit count.
- Report the scope and result of validation honestly. A checkpoint is not a
  qualification claim. See the workflow guide for available commands.

## Communication and artifact lifecycle

- Briefly explain intended work, meaningful progress, results and remaining
  uncertainty. Use the format that helps Erik inspect the outcome; no fixed forms.
- Erik and agents can add useful observations to the shared [workflow notebook](documentation/workflow/NOTES.md).
  Revisit it at natural work boundaries or when friction repeats; use it to discuss
  improvements, not as a source of new rules or automatic tasks.
- Keep only active work artifacts and current reference documentation in the tree.
  Before deleting completed or superseded artifacts, preserve enduring facts in
  current documentation. Git history is the archive; do not create archive trees.
- At meaningful completion boundaries, identify and update affected user and
  technical documentation. Validate relevant commands, examples, diagrams, links
  and referenced paths before declaring the work complete. Experimental behavior
  needs brief provisional notes, not continuously polished product documentation.
- Stop for an actual blocker, missing authorization or consequential unresolved
  choice. Do not stop solely because a procedure expects an approval record.

## Task-specific engineering guidance

Read only what applies:

- [Native assembly](agents/rules/native-68000.md): assembly style and calling rules.
- [Native parity](agents/rules/native-rust-parity-porting.md): reference boundaries
  and the authoritative real-native proof contract.
- [Failure triage](agents/rules/native-parity-failure-triage.md): native failures.
- [Instrumentation](agents/rules/native-68000-safe-instrumentation.md): debug changes.
- [FS-UAE](agents/rules/fs-uae.md): emulator execution and environment.
- [References](agents/rules/reference-refresh.md): generated fixtures and goldens.
- [Release notes](agents/rules/release-notes.md): release-bearing work.

Optional artifact-writing skills are indexed in [skills/README.md](skills/README.md).
