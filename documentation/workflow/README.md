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

## Controlled experiments and integration

Before investing in an experiment, state the hypothesis, the baseline it will be
compared with, a focused correctness comparison, and what would count as success
or a reason to stop. Use a short note or conversation; no prescribed artifact is
required. Keep the work isolated and retain a working reference path while the
replacement is evaluated. Temporary duplication and incomplete prototypes are
acceptable within the agreed scope. Mark experimental interfaces as non-public.

For performance work, use reproducible before/after measurements with identified
inputs, build settings and environment. Distinguish measured gains from predictions
and incomplete diagnostic runs. Choose a useful result before investing further;
retain, revise or abandon the experiment according to the evidence. Discuss a
material change of direction, rather than automatically extending an unsuccessful
approach. There is no extra approval gate for routine experimental steps.

Before declaring integration readiness, inspect the combined diff and resulting
responsibilities, not just each intermediate patch. Remove superseded experiments,
dead switches and duplication where appropriate. Explain retained fallback paths
and unresolved compromises. Verify the integrated behavior and, for performance
work, measure the integrated result. A successful prototype alone is not the final
maintained implementation.

At a meaningful completion boundary, identify affected user and technical docs,
update descriptions whose truth changed, and validate relevant commands, examples,
diagrams, cross-links and paths. Maintain stable interfaces as they become ready;
keep experimental notes provisional. Update affected documents, not every document.

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
| Development | Fast checks for changed behavior and relevant invariants | `make workflow-check` for workflow changes; targeted `cargo test -p <crate> <filter>`, compile or assembly commands for product changes |
| Integration | Affected subsystem tests, engineering guards, focused differential/native checks when relevant | `make workflow-test` for workflow integration; selected subsystem commands for product changes |
| Qualification | Broad checks appropriate to the completed capability | `make quality-gate` runs the existing Rust quality gate; select native/corpus/self-host commands separately when applicable |

`make workflow-check` checks workflow links, the dependency ban, CPU boundary and
known benchmark selectors in production source. `make workflow-test` adds the
workflow Python test suite. `make workflow-gate` is its CI alias. These commands
do not compile or qualify product code. `make -n quality-gate` inspects Rust
qualification wiring without launching it. Product checks retain their existing
specific command names; select the affected scope explicitly.

`check_benchmark_selectors.py` is a narrow tripwire for known B01–B10 identities
and performance-fixture paths in production Rust/native source. Test files,
harnesses and performance tools are excluded; inline test code in production
files is still scanned. It ignores comments and native hexadecimal constants.
This lexical check does not detect arbitrary output-path or self-host-generation
switches, constructed identities or every form of test detection. Review remains
responsible for the broader prohibition in AGENTS.md.
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

## Learn as we work

[NOTES.md](NOTES.md) is a shared notebook for recurring friction, promising
approaches not yet incorporated, and ideas worth trying. Erik and agents add brief
notes when useful. No required fields, scores or entry after every task.

Revisit it together at natural checkpoints, such as starting substantial work,
finishing a coherent change or encountering repeated friction. Decide whether to
try an idea, improve the workflow, scripts or automation, or drop it. Entries are
observations and suggestions, not instructions or authorization to create tasks
or automation. Remove notes once incorporated or obsolete; Git keeps the history.

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
