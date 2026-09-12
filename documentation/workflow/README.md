# Working on opForge

[AGENTS.md](../../AGENTS.md) defines the operating contract. This guide explains
its practical use. The workflow aims to shorten time to inspectable, testable
results, reduce total token cost and support coherent changes within agreed scope.
It makes no new decisions about opForge architecture or future project plans.

## Agree on outcomes, then work

Establish the requested outcome, scope, constraints and how to recognize success.
Use a brief conversation for ordinary work; write a plan or specification when it
resolves complexity or when requested. No fixed document, file prediction, edit
order, checkbox progression, sidecar or independent approval is required.

Examine the affected responsibilities before implementing. A coherent outcome may
span related modules. Discuss consequential tradeoffs and scope changes; use
judgment for routine details. Deliver a state that can be inspected or tested,
not just a list of helpers completed. Interactive requests set discussion points.

## Recoverable commits

Commit when a recovery point is useful, not because a plan item changed state.
A checkpoint may preserve incomplete work, a failed experiment or a useful baseline
for code, documentation or planning. An optional `checkpoint:` subject makes its
purpose visible; describe known failures in the body. No full gate is required.

An integration commit captures a coherent change intended for the maintained
codebase. Validate that claim with relevant checks. A feature or release completion
claim may reference such a commit and carries its own qualification requirements.
A commit alone is neither approval nor a claim of completion.

Inspect staged changes. The optional explicit-path staging helper refuses staged
changes outside its selected paths. Preserve checkpoints until a replacement is
verified. Use another worktree to inspect old states when resetting would disturb
uncommitted work. Remote pushes require separate explicit authorization.

## Validation follows risk

Choose the affected scope explicitly. Existing engineering checks remain available;
none of these command names promises coverage outside the selected checks.

| Point in work | Expected evidence | Commands |
|---|---|---|
| Development | Fast checks for changed behavior and relevant invariants | `make dev-check` for workflow changes; targeted `cargo test -p <crate> <filter>`, compile or assembly commands for product changes |
| Integration | Affected subsystem tests, engineering guards, focused differential/native checks when relevant | `make tranche-check` for workflow integration; selected subsystem commands for product changes |
| Qualification | Broad checks appropriate to the completed capability | `make milestone-check` runs the existing Rust quality gate; select native/corpus/self-host commands separately when applicable |

`make dev-check` checks workflow links, the existing dependency ban and CPU boundary.
`make tranche-check` adds the workflow Python test suite. `make workflow-gate` is
its CI alias. These commands do not compile or qualify product code.
`make -n milestone-check` inspects wiring without launching qualification.
The native deterministic gate remains available as
`python3 scripts/workflow/run_native_porting_quality_gate.py`; real-native proof
is governed by the [native parity contract](../../agents/rules/native-rust-parity-porting.md).

Run focused checks repeatedly while developing. Broaden for cross-cutting changes,
unresolved failures and meaningful completion boundaries. Retain engineering CI;
local token efficiency is not grounds to disable useful integration checks.
Report what ran, what passed and material gaps. Do not repeatedly run an unchanged
broad suite after successful validation without a reason.

## Spend effort where it changes the result

Script stable, repeated mechanics with clear inputs, output and failure behavior.
Scripts must not hide externally consequential actions or bypass authorization.
Keep substantial judgment with a capable coordinator. Delegate bounded work when
briefing, coordination, verification and likely rework cost less than direct work.
Use the cheapest capable configured model; escalate when uncertainty erases savings.
Return compact deliverables and evidence, not repeated conversation histories.
Avoid default reviewer chains or parallel agents with overlapping assignments.

## Keep a current working tree

Keep active artifacts and maintained references, tests, fixtures and benchmark tools.
Delete completed reports, superseded plans and transient receipts once enduring
facts have been incorporated into current documentation. Git preserves previous
states. Do not move obsolete files into an archive or regenerate historical receipts.
Check callers before deletion: a historical-looking file may still be runtime,
test or validation input. Preserve its engineering function in a maintained location.
Do not absorb uncommitted or unrelated work into cleanup.

Maintain documentation whose truth changes. A historical plan is not an instruction
for new work merely because it mentions unfinished tasks. The current user request
and an explicitly selected active artifact determine what is being worked on.

Existing `@opforge-slice` comments in native source are historical provenance,
not required inputs or active instructions. Their referenced plans are in Git
history. Remove those comments naturally when the associated source is next
changed; this cleanup preserves native source bytes.
