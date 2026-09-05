<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->

# opForge Rust-First VM and Native AmigaOS Performance Optimization Plan v0.1

## Metadata

- Source: original Rust-first performance instruction and companion baseline; 2026-09-01 activation, 2026-09-02 bounded-corpus direction, 2026-09-04 diagnostic-entry and terminal-gate amendments; 2026-09-05 user instruction to reorder for the cheapest largest early gains, faster subsequent work, token efficiency, and increasing certainty toward completion.
- LSP source amendment (2026-09-05): user explicitly permits LSP to remain broken until the very end; all LSP repair and final qualification belong to Step 25 / Item LSP-close.
- Step13 scheduling amendment (2026-09-05): under the user’s authorization to reorder the plan, independently reviewed language changes focused provisional qualification only; final gates remain unchanged.
- Mode: implementation plan with measured experiments and explicit rejection decisions.
- Owner: opForge maintainers and implementing Codex tasks.
- AGENTS binding: The active worktree `AGENTS.md`, applicable rule packs, and explicit conversation instructions remain binding during execution. This plan does not override a mandatory gate in that contract.
- Worktree: `/Users/erik/Code/Retro/opForge-wt-rust-vm-native-performance`, branch `codex/rust-vm-native-performance`; activation checkpoint `68cc693c40fd27e30bed11e08974d3263d6cb6f6`, inspected HEAD `8fd492904b090e80a3cdedc7f4dfc6531ddc6ce1`.
- Amendment scope: scheduling and evidence thresholds only. No runtime change or performance success is claimed. Existing uncommitted Item 0f code/results remain owned by that unfinished slice. The LSP deferral amendment has its own focused workflow-scope commit with no product/runtime change.
- Workflow: `skills/opforge-plan-authoring/SKILL.md`, `scripts/workflow/run_plan_workflow.sh`; independent quality/compliance receipt in the adjacent `.quality-gate.txt`.

## Goal

First reduce the cost paid by every native test and assembly, then use the faster
product to investigate remaining hotspots. Rust remains the discovery and first
implementation authority for generic VM changes. Native platform work does not
wait for exhaustive Rust opcode instrumentation. Finish with reproducible
performance evidence, complete correctness gates and unchanged gen0 → gen1 →
gen2 self-host proof.

The prior plan put schema promotion, thirteen executor instrumentation commits,
and repeated broad gates in front of cheap native improvements. This amendment
replaces that critical path with: preserve the investigation → classify relevant
failures → faster same-range startup clear → one measured Rust VM improvement
and transfer decision → eliminate the largest safely unused region → buffer
measured module reads → close early throughput qualification.
Every experiment can be rejected after a bounded comparison. No speedup is
promised from a snapshot or static byte count.

## Version Impact

- Affected component(s): native runtime, Rust VM/package runtime, internal performance tools and qualification evidence.
- Impact class: patch
- Owned contract: package-owned semantics, artifacts, diagnostics, exits, capacity/lifecycle behavior, portable interpreter fallback and fail-closed parity.
- Rationale: implementation cost changes only; any new public CLI/profile contract requires explicit review and versioning before release.

## Inputs and Evidence

Paths below are relative to this worktree. Recorded evidence is immutable history;
its then-current scheduling instructions are superseded by this plan, not its
measurements or failure statuses. Some current Item 0f reports are uncommitted;
Item 0f must persist and identify them before implementation starts.

| Evidence | What it supports | What it does not support |
|---|---|---|
| `documentation/performance/results/opforge-native-item0f-attribution-decision-2026-09-04.md` and `opforge-native-b10-repeatability-2026-09-04.md` | Exact-input B10 setup snapshots localize the pending 41,221,928-byte session clear at about 60.8s; later snapshots at about 100.7s show it completed | No calibrated full-run time share, no native B10 success, no hardware speed claim |
| `documentation/performance/results/opforge-native-io-counter-calibration-2026-09-04.md` | 7 module candidates, 23,865 reads returning 23,858 bytes; source audit corroborates byte reads | DOS calls are not physical disk accesses; no evidence of a whole index scan for each `.use` |
| `documentation/performance/results/opforge-native-common-boundary-controls-2026-09-04.md` | B03 ABBA has identical shared work; 0.203s mean boundary difference | Aborted controls are not complete corpus assembly timings or a universal overhead correction |
| `documentation/performance/results/opforge-item0f-closure-gates-2026-09-05.md` | Native 38/51 groups passed, 13 failed; 49,852.97s summed group time (13.85h); 29 timeouts, four branch rejections, one diagnostic mismatch. Rust assembler 1,586 passed, LSP 34 passed / 14 failed | Gate duration is not product throughput; failures are not yet classified as baseline versus introduced |
| `documentation/performance/results/opforge-corpus-v1-rust-baseline-2026-09-04.json` | Seven release runs/case: B10 median 157.246ms, range 156.763–158.714ms; B01–B08 roughly 133–140ms | Native comparison, current Rust VM opcode attribution or isolated setup cost |
| `documentation/performance/opforge-rust-vm-native-amigaos-performance-baseline-v0_1.md` | F1–F12 code audit; 30.8MB statement arena, 368,278-byte embedded copy, existing layout/final-emission split | Static costs and historical Rust wins are not fresh optimization acceptance |
| `documentation/performance/opforge-production-corpus-v1.md`, frozen manifest and native status ledger under `documentation/performance/results/` | Frozen B01–B10 inputs, commands, package and explicit failed/incomplete native cases | Failed native baselines are never successful timing samples |

The 30.8MB statement arena is about 74.7% of the 41.2MB session allocation by
static size. That makes it a higher potential work-elimination target than
starting with smaller source regions; it is not a measured 74.7% time saving.
The embedded package is under 1% of that clear volume and has a wider lifetime
contract, so it is not on the first critical path. Later pass, symbol,
expression, representation and AOT costs remain hypotheses.

## Scope and architecture

Keep Track R (Rust generic VM), Track N (native platform/work elimination) and
Track T (transfer of successful Rust mechanisms). Preserve current bulk package
reads/artifact writes, once-per-invocation module index, tokenizer fast paths,
and the already existing layout-only/final-emission split. Do not rebuild that
split merely because an old finding proposed it.

No CPU/family/dialect semantics may enter generic VM/native/CLI paths. Native
slices load the native parity and 68000 rule packs; instrumentation uses only the
approved framework. FS-UAE is confirmation, with proof levels labelled. Fresh
challenge, exact START/DONE, explicit guest exit (zero for successful assembly),
live Rust oracle, exact artifact/diagnostic comparison, attempt-all recovery and
ephemeral cleanup remain mandatory. Negative tests require their exact expected
exit and diagnostic. Timeout, partial counter, reduced probe or launcher success
is never Level D proof. Do not use full self-hosting for routine profiling.

## Execution and measurement policy

One active implementation item and one focused commit at a time. Record base,
branch, absolute worktree, files, required evidence, result commit and disposition
in the existing performance ledger. Use the integration worktree for narrow
sequential changes; create a child worktree for lifecycle/representation risk or
isolation from unfinished work. Never modify `main`, unrelated dirty work, push,
or merge remotely as part of this plan. Checked historical items remain checked;
open findings do not become fixed through a scheduling amendment.

The sequential **Step NN** labels are the personal tracking order; stable
**Item** IDs remain unchanged for dependencies and evidence references. Steps
01–07 are completed history, retained at the end for auditability; the active
Work Items run from Step 08 onward. Historical prerequisites and phase gates do
not reactivate the superseded sequence. When inserting a reviewed sub-item,
renumber subsequent Step labels to keep the sequence contiguous while retaining
all Item IDs. Include both labels in progress updates and ledger entries.

**Current step: Step 15 of 25 · Item A-triage — choose the next action from
post-win evidence after the focused Step14 commit.** Update this pointer and total when the active item or plan changes.

### Evidence sufficient to try versus evidence sufficient to keep

- **Try:** a named measured operation or sampled owner, source/ownership audit,
  a plausible cost reduction, a focused oracle and rollback are sufficient for
  a bounded experiment. The existing clear and module-read evidence authorize
  Items 17c and 14m respectively after their stated prerequisites. A global
  wall-time share or full profiler inventory is not required to try them.
- **Keep provisionally:** exact focused correctness; expected deterministic
  mechanism change; at least three matched completed control/candidate pairs
  on the affected mechanism case using the real CLI. Alternate order; match
  CPU/JIT, executable identity, package/input digests, flags, output mode and
  timing boundary. Record median/range and gate cost. Let noise be the larger
  relative range of the two timing sets; require median improvement greater
  than `max(5%, noise)`. This is a conservative engineering threshold, not a
  statistical confidence interval. If inconclusive, add at most two pairs,
  then reject/defer or approve a narrowly named foundation exception.
- **B10:** attempt one fixed-budget unchanged B10 control/candidate comparison
  per early candidate. If either cannot complete, record failed/incomplete and
  no B10 speed/non-regression claim. A focused CLI win can integrate provisionally
  through Phase A while B10 is blocked. A reproducible newly introduced error
  blocks even provisional acceptance. Phase A closure requires completed B10
  exact parity and repeated completed candidate evidence. When the historical
  control still times out at the unchanged bound but the candidate completes,
  report a censored baseline and completion improvement, never a median speedup
  or a passed control. Require five completed candidate runs, exact live Rust
  parity and focused mechanism/control proof; freeze that first complete native
  B10 result as the baseline for subsequent candidates. Otherwise use matched
  completed B10 non-regression checks. Terminal acceptance never relies on
  partial profiles. Do not shrink/relabel B10 to manufacture success.
- **Step13 foundation exception:** an incomplete full/live B03 attempt at the
  unchanged fixed 120s bound may be retained provisionally for Step13 only when
  an independent review approves the exception and all of the following are
  already complete: three stable B01 matched control/candidate pairs with exact
  native parity, distinct mode-image identities, and improvement greater than
  `max(5%, noise)`; a fresh live 513-statement Level D capacity PASS; a fresh
  early-error Level D PASS; the existing all-24-field host proof and native
  guards; the full non-LSP Rust gate; and a final independent compliance PASS.
  The B03 fixture is 256 trivial NOPs, while the 513-statement case exercises
  the actual CLI/store/pass/emission path with more live rows. This exception
  preserves every attempted B03/B10 failure as unresolved and makes no B03/B10
  speed or parity claim. The unchanged-bound completed B03 requirement and the
  full B01–B10 A-close qualification remain mandatory and unchanged.
- Use existing counters with I/O detail disabled for compute timing and separate
  all-counter structural I/O runs. Use observer-off completed timings where
  possible; do not pool different observer/CPU configurations. Pin 68020 as the
  acceptance baseline; label 68040/68080 and physical A6000 evidence separately.
- A primitive speedup leaves bytes cleared unchanged; reduced loop iterations
  and completed timing are its mechanism evidence. Live reset reduces cleared
  bytes; buffering reduces actual DOS calls. Do not demand the wrong counter
  reduction or infer elapsed share from count ratios.
- New profiling work must name one decision it will unblock, the minimum added
  observation, and its stop condition. Do not repeat the stopped 60/100s B10
  sampling loop, collect every histogram, or build a remote runner first.
- Keep a balanced queue: after the first cheap native startup trial, perform the
  first Rust hotspot/implementation decision before another broad native change.
  Thereafter C0 compares Rust, native platform and eligible transfer candidates
  together by measured return; a track has no guaranteed quota of speculative
  work. A blocked native case must not force repeated native profiling when a
  supported Rust candidate can proceed through a reviewed reorder.
- Every user-facing results report must explain how the last work contributed
  to performance: measured runtime improvement, faster development/validation,
  or enabling evidence. Distinguish these explicitly from unmeasured expectations.
- Track developer throughput as well as product cost: elapsed focused test,
  native build and gate time, guest startup where separable, and proof status.
  Estimate payback as expected seconds saved per invocation × remaining
  invocations, divided by implementation/review/validation time. Record
  assumptions; re-rank after each accepted early win instead of inventing ROI.

### Gate tiers and increasing certainty

Every item requires plan-compliance PASS and its listed focused evidence before
commit. Native production changes require formatter, architecture/inventory,
staged native gate, risk-matched Level A–C evidence and focused authoritative
Level D confirmation. Workflow artifacts require `make workflow-gate` plus plan
validators. The user's explicit LSP deferral supersedes the earlier requirement
to block interim work on LSP failures. Every otherwise-required Rust gate through
Item 30 now runs `bash scripts/workflow/run_rust_quality_gate.sh --defer-lsp`:
all native guards, formatting, audit and non-LSP packages remain required;
only package `lsp` clippy/tests are excluded. Its receipt explicitly says
non-LSP and deferred, never full-workspace PASS. Do not repair, suppress or
reclassify LSP failures in Phase A. Retain the 34/14 integration failure receipt
for Item LSP-close, which runs the default unfiltered gate at the very end.
Other failures still block their required commits and checkpoints.

Full broad qualification checkpoints are **A-close**, **B-close**, **28** and
**30**, not each small native experiment. A-close/B-close use the non-LSP Rust gate
and `run_native_existing_parity_completion.sh --verify-phase-zero` (all 51
nonterminal groups in the inspected 53-group inventory); check the current
inventory for growth. The two named full-product and two-generation groups stay
mandatory in the complete `--verify` gates at 28/30. This explicitly extends the
2026-09-04 nonterminal gate scope to these intermediate checkpoints; it changes
no test or proof protocol. No advancement beyond a checkpoint until it passes.

The failed 2026-09-05 suite is retained as a diagnostic baseline. Its complete
rerun moves to A-close after early throughput work and bounded failure repair;
Item 0f now closes its observation deliverable with its own required focused
checks and any mandatory Rust gate. That is an explicit gate scheduling change,
not a passing Phase 0 receipt. Related/unclassified failures require the E0
classification before a candidate; unrelated classified failures remain open
and block A-close, except explicitly deferred LSP failures owned by LSP-close.
Never suppress other tests, relax timeouts, bless changed output or
claim a baseline failure is harmless without evidence. Cross-cutting changes or
focused regression escalate validation immediately.

### Token and delegation budget

Use the coordinator for ranking, architecture tradeoffs and adjudication. Use a
small model for bounded read-only measurement extraction, exact failure-log
classification or a checklist review; pass paths and a precise question instead
of full conversation history. A single implementation owner avoids merge churn.
Use an independent capable reviewer for lifecycle/proof/architecture risk and
required plan-quality/compliance decisions; do not repeatedly commission broad
reviews of unchanged material. Delegate only when saved work exceeds context,
launch and reconciliation cost. Persist concise decision/receipt tables instead
of repeating raw logs or narrative investigation histories in this plan.

## Requirement and finding identifiers

| ID | Requirement |
|---|---|
| SR-EARLY | Cheapest high-confidence throughput gains first; bounded experiments; re-rank after wins |
| SR-WT | Dedicated worktree, one active item, explicit focused commits and preserved unrelated work |
| SR-RF | Rust evidence and successful implementation precede generic native VM transfer |
| SR-MEAS | Frozen real CLI corpus, provenance, matched completed timing, no invented claims |
| SR-PAR | Exact semantics, lifecycle, failure paths and fail-closed native proof |
| SR-ARCH | Package ownership and portable interpreter fallback |
| SR-ID | Versioned stable Rust/native identities and bounded attributable profiling |
| SR-ACC | Eligibility/signature checks, disabled/generic/accelerated/dual modes, explicit decisions |
| SR-TERM | Full qualification, budgets, cleanup and fresh gen0 → gen1 → gen2 proof |

F1–F12 retain their meanings in the companion baseline. A partial mechanism
improvement does not close a whole finding; closure review is still required.

## Work Items

### Phase A — native startup, Rust VM gain, then larger native work elimination

- [x] Step 08 · Item 0f — Close the bounded investigation deliverable; retain failed qualification
  - Completion evidence (2026-09-05): `documentation/performance/results/opforge-item0f-completion-gates-2026-09-05.md` records explicit zero exits for the non-LSP Rust gate (1,586 assembler tests, 1,881.30s) and staged workflow gate (134 tests, 238 formatter files), 27 verified raw receipt hashes and the independently reviewed first-pause fix. Native qualification and LSP debt remain open under their named later owners. This checkbox is part of the focused Step 08 commit, not a claim of completed Phase A.
  - Source requirement or finding IDs: SR-MEAS, SR-WT, SR-PAR, F12.
  - Expected files: existing Item 0f host tools/tests, corpus adapter, wrapper and results/ledger; no native optimization.
  - Dependencies/worktree: current active unfinished slice; preserve all dirty work and establish its reviewed commit before E0.
  - Steps and boundaries: Stop sampling. Audit and persist the already pending work as the observation slice, with exact raw hashes and corrected input/observer provenance. Preserve both failed full-gate receipts and every unresolved case. Record the new A-close owner of broad qualification; update historical reports only to point to this scheduling amendment, never alter results.
  - Before/after and stop/go: no speed claim; do not repeat the 13.85h native suite to close an observation report. If its required non-LSP gate still fails, classify/repair the exact non-LSP blocker within this slice and review it; do not mark 0f complete on failure.
  - Gate tier: focused sub-item.
  - Required focused gates: focused capture/control/corpus/wrapper and static-contract tests, workflow gate; non-LSP Rust gate for the pending Rust changes; common execution/gate policy applies.
  - Full quality gates: deferred to A-close, except mandatory per-commit contract gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): close bounded attribution with tracked qualification debt`; exactly one focused commit before the next item.
  - Definition of done: observation work committed with honest proof statuses; A-close remains open and no claim of passed Phase 0 or corpus parity.

- [x] Step 09 · Item E0 — Classify failures relevant to the first native experiments
  - Completion evidence (2026-09-05): `documentation/performance/results/opforge-step09-early-failure-ledger-2026-09-05.md` assigns all 13 failed groups, B01–B10 statuses, LSP and the six-byte range discrepancy to explicit owners. Fresh strict B01 Level D parity passed at `47009f6c` with guest/host exit 0 and exact live Rust output. No defect is claimed fixed, no performance ratio is inferred, and 17c preserves the existing range. This checkbox closes only with the focused E0 commit after workflow/compliance PASS.
  - Source requirement or finding IDs: SR-EARLY, SR-PAR.
  - Expected files: performance failure ledger and one focused reproducer/oracle where missing.
  - Dependencies/worktree: Item 0f commit; integration worktree, read-only classification first.
  - Steps and boundaries: Use existing logs first. Reproduce one representative completed branch rejection and the wrong negative diagnostic against the recorded base and candidate boundary as needed; identify affected contracts and distinguish the 29 timeouts from explicit exits. Check the reported 92-byte versus 98-byte header issue against the exact init range; do not silently repair it inside the bulk-clear experiment. Any required semantic repair becomes its own reviewed checkbox/commit before the affected optimization. Do not expand into a full suite rerun.
  - Before/after and stop/go: classify regressions, baseline defects, unresolved and timeout-only separately. If unknown could invalidate a candidate oracle, that candidate blocks; choose an unaffected exact CLI oracle or investigate the first divergent boundary.
  - Gate tier: focused sub-item.
  - Required focused gates: risk-matched host checks and focused exact-source native confirmations if run; workflow gate for ledger; common execution/gate policy applies.
  - Full quality gates: deferred to A-close, except mandatory per-commit contract gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): classify early optimization correctness boundaries`; exactly one focused commit before the next item.
  - Definition of done: ledger links all failed groups/corpus statuses to owners and retains LSP failures under Item LSP-close; first candidate has a trustworthy focused oracle and no hidden relevant failure.

- [x] Step 10 · Item 17c — Speed the exact existing session clear at one call site
  - Completion evidence (2026-09-05): `documentation/performance/results/opforge-step10-session-clear-2026-09-05.md` and the six-run B01 JSON record 40.62% median case-time improvement (75.5636s to 44.8694s), 24.00% lower whole-test cost, exact same-range memory/ABI checks, strict native parity and capacity/error confirmations. Both B10 builds retain their unchanged 120s timeout failures; no B10 gain or Phase A closure is claimed. Non-LSP Rust passed (1,588 assembler tests) and staged workflow passed. This checkbox closes only with final independent compliance and the focused 17c commit; retention remains provisional through A-close.
  - Source requirement or finding IDs: SR-EARLY, SR-PAR, SR-ARCH, F2, F3.
  - Expected files: native opasm session init/clear helper, primitive harness and focused tests/results.
  - Dependencies/worktree: E0; narrow integration slice or isolated child if unfinished work remains.
  - Steps and boundaries: Add a 68020-safe aligned longword clear with byte alignment/tail handling at initSessionV1 only; preserve the exact existing 41,221,928-byte range, register/flag ABI, capacities and initialization semantics. Keep the byte reference switch. No copy primitive, lifecycle redesign, instrumentation framework or global call-site migration. Check zero/odd/small/large lengths and guard bytes; do not use a 16-bit loop count that truncates the large arena.
  - Before/after and stop/go: existing exact-input snapshots authorize this experiment, not retention. Apply matched-pair threshold to completed B01; clear-byte total must stay identical and bulk-loop work fall. Attempt bounded B10. Stop/revert after the bounded trial if no useful win; leave deep lifecycle work independent.
  - Gate tier: focused sub-item.
  - Required focused gates: native formatter/staged/architecture/inventory; primitive Level A–C memory/ABI oracle and focused fresh Level D real CLI B01 plus capacity/early-error case; common execution/gate policy applies.
  - Full quality gates: deferred to A-close, except mandatory per-commit contract gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `perf(native): accelerate same-range session initialization`; exactly one focused commit before the next item.
  - Definition of done: one startup call site is faster with exact clear semantics and focused CLI parity, or a documented rejected trial; speed is provisional until A-close.

- [x] Step 11 · Item R0 — Find one Rust cost with existing phase timing and host sampling
  - Completion evidence (2026-09-05): `documentation/performance/results/opforge-step11-rust-cost-decision-2026-09-05.md` selects the explicit-package early return. One transient timer measures fallback construction at B01 100.479ms and B10 101.305ms, exact artifacts and overhead recorded; both sampler attempts had empty stacks. Production source restored exactly. This checkbox closes only with the focused R0 commit after workflow/compliance PASS; no runtime gain or Phase A closure claimed.
  - Source requirement or finding IDs: SR-RF, SR-MEAS, F11.
  - Expected files: existing phase_profile/runtime profile paths only if needed; scripts/performance and decision report.
  - Dependencies/worktree: 17c accepted/rejected decision commit; integration worktree.
  - Steps and boundaries: Use current release real CLI corpus and available host sampler first. B10 total is about 157ms against 133–140ms small cases; investigate fixed costs without asserting the difference is setup attribution. Use bounded repeated invocations for sampling, retaining individual-run identity. Add only one missing owner/helper counter needed to distinguish the leading alternatives. Select one removable repeated check/decode/allocation/clone cost; no full opcode inventory prerequisite.
  - Before/after and stop/go: record owner evidence, coverage and overhead. After one bounded sampling session and at most one targeted counter slice, either select a supported candidate or report no-go and proceed to dispositions.
  - Gate tier: focused sub-item.
  - Required focused gates: exact Rust artifacts; focused tests for any new counter; workflow for report; mandatory non-LSP Rust gate if Rust changes; common execution/gate policy applies.
  - Full quality gates: A-close; mandatory per-commit contract gates still apply.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): select first measured Rust cost`; exactly one focused commit before the next item.
  - Definition of done: a concrete Rust candidate has trustworthy attribution, baseline and rollback, or an explicit no-go.

- [x] Step 12 · Item 10 — Remove the selected Rust cost and measure immediately
  - Completion evidence (2026-09-05): `documentation/performance/results/opforge-step12-explicit-package-performance-2026-09-05.md` records exact matched B01/B10 gains of 72.7808%/63.9820% from skipping discarded fallback construction for explicit packages. Focused feature/error checks, 1,589 assembler tests and the full non-LSP Rust/workflow gates PASS. No direct native counterpart; proceed to16a. This checkbox closes only with the focused Item10 commit after final compliance PASS; no Phase A closure claimed.
  - Source requirement or finding IDs: SR-RF, SR-MEAS, SR-PAR, SR-ACC, F11.
  - Expected files: exact owner and tests selected by R0; record paths in ledger before edits.
  - Dependencies/worktree: R0 decision commit. On a positive decision implement the selected cost; on an R0 no-go close Item 10 through one documentation-only disposition commit, explicitly marked rejected/unimplemented.
  - Steps and boundaries: Implement only the selected repeated-work elimination. Keep generic reference and off/generic/accelerated/dual modes as applicable; bounded differential comparisons, eligibility/invalidation/fallback counters. No new execution IR or broad VM refactor without a separately approved candidate.
  - Before/after and stop/go: matched complete production B10 and affected cases; policy improvement threshold, zero mismatch, setup/code/memory and fallback coverage. Reject local-bucket wins that regress the integrated path.
  - Gate tier: focused sub-item.
  - Required focused gates: focused exact artifact/diagnostic/error and differential tests; non-LSP Rust gate for Rust commit; common execution/gate policy applies.
  - Full quality gates: A-close plus mandatory non-LSP Rust before Rust commit.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `perf(vm): remove measured repeated runtime work`, or `docs(perf): record Rust candidate no-go` for the rejection path; exactly one focused commit before the next item.
  - Definition of done: one measured Rust win or honest rejection is committed; generic semantics remain unchanged. Record an immediate transfer disposition: native counterpart applicability, measured native owner/cost, expected reuse, eligibility and proof effort. If a positive Rust win plus native evidence outranks 16a/14m, insert one concrete reviewed 25.x native port and cleanup owner immediately after Item 10; otherwise explicitly defer. A Rust rejection never authorizes native speculation.

- [x] Step 13 · Item 16a — Eliminate unused statement-arena clearing before smaller regions
  - Completion evidence (2026-09-05; final focused commit and independent compliance receipt pending): completed B01 full/live matched evidence records a 17.2050% observed gain above the 5% threshold with exact parity and stable distinct native FNV identities; fresh 513-statement capacity and early-error Level D proofs pass; private/full and live native images are byte-identical to their prior artifacts; all six B03 and both B10 fixed-bound attempts remain unresolved timeouts and are mandatory A-close debt. See `documentation/performance/results/opforge-step13-native-statement-performance-2026-09-05.md` and `opforge-step13-native-statement-comparison-2026-09-05.json`.
  - Source requirement or finding IDs: SR-EARLY, SR-PAR, F2, F7.
  - Expected files: opasm session/statement insertion and access paths, debug poison/assert harness, results.
  - Dependencies/worktree: Item 10 decision commit; high-risk child worktree. Old Item 16 source reset is not a prerequisite.
  - Steps and boundaries: Audit all statement zero dependencies and byte-region boundaries. Reset authoritative live counts and fully initialize rows on insertion; omit only the proven-unused 30.8MB statement range from startup clear. Keep all other regions and layout resets initially unchanged. Retain statement-only legacy reset and poison unused records. A generation-map redesign or record compaction is outside this item; schedule separately only if needed.
  - Before/after and stop/go: target work reduction is 30.8MB minus any required initialized live bytes, not a promised timing percentage. Apply B01/B03 matched timings and bounded B10. If full/live B03 remains incomplete at the unchanged fixed 120s bound, an independently reviewed narrow foundation exception may retain Step13 provisionally only after three stable B01 matched control/candidate pairs show exact native parity with distinct mode-image identities and improvement greater than `max(5%, noise)`, fresh live 513-statement capacity Level D and early-error Level D PASS, existing all-24-field host proof/native guards, full non-LSP Rust gate, and final independent compliance PASS. B03 is 256 trivial NOPs; the 513 case is the actual CLI/store/pass/emission path with more live rows. Preserve every B03/B10 attempted failure as unresolved and make no speed/parity claim. The unchanged-bound completed B03 requirement and full B01–B10 A-close remain mandatory. If this exception cannot be justified, record defer and proceed to module buffering; do not broaden to all-region redesign.
  - Gate tier: focused sub-item.
  - Required focused gates: native gates, Level A–C poison/reuse/capacity/early-error oracles; focused Level D source/statement/layout CLI parity; common execution/gate policy applies.
  - Full quality gates: deferred to A-close, except mandatory per-commit contract gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `perf(native): initialize statement storage by live state`; exactly one focused commit before the next item.
  - Definition of done: unused statement capacity is neither cleared nor read with exact focused parity, or explicit bounded defer; a narrowly reviewed foundation exception may retain this item provisionally under the stated B01/live-513/early-error/host-proof/gate conditions while B03/B10 failures remain unresolved; F2 remains partial while other regions clear. Step13 closes only after its provisional-retention or bounded-defer decision is committed; B03/B10 debt remains open at A-close.

- [x] Step 14 · Item 14m — Buffer the measured module-candidate scan first
  - Retention evidence (2026-09-05; final compliance and focused commit pending): four complete matched pairs show 65.255084583s byte-reader versus 49.944847938s buffered medians (23.4621% improvement), exact live-Rust module output, and stable image identities. Minimal platform counters confirm 16,541 to 8 module DOS reads for identical 16,538 logical bytes and three candidates. The isolated buffered timeout was not reproduced in the two final bounded pairs; all five candidate attempts remain recorded (four complete, one timeout). The independent reviewer approves provisional retention only; both bounded B10 timeouts and the failed full-counter compositions remain explicit debt, with no B10 or Phase A completion claim. See `documentation/performance/results/opforge-step14-native-module-performance-2026-09-05.md` and its comparison JSON. The final A1 clobber comment correction rebuilds to the identical host native image. Full non-LSP Rust passed (1,590 assembler tests, 1,855.83s suite); workflow, staged native and focused host gates passed. This checkbox closes only with final independent compliance and the focused commit; all remaining A-close debt stays open.
  - Source requirement or finding IDs: SR-EARLY, SR-PAR, F1.
  - Expected files: opforge-cli DOS/reader and module_discovery paths, reader harness/tests/results.
  - Dependencies/worktree: 16a decision commit; existing 23,865/23,858 module read evidence authorizes trial; no shared schema or root consolidation prerequisite.
  - Steps and boundaries: Introduce one bounded generic buffered byte reader (start at 8KiB; change only with evidence) and migrate module candidate scanning only. Preserve candidate enumeration, declarations, EOF, short/error reads, CR/LF and index ownership. Keep reference reader. Do not combine root-scan consolidation, range seeking or duplicate-root elimination with the refill change.
  - Before/after and stop/go: with full reads expect sum of ceil(file_bytes/buffer_bytes) plus required EOF probes; explicitly count extra short-read refills, never silently assume full DOS reads. Completed B08 or an independently complete module CLI oracle, bounded B10 and policy timing threshold; if affected baseline diagnostic fails, repair in a separate prerequisite slice.
  - Gate tier: focused sub-item.
  - Required focused gates: native gates; refill/boundary/short/error/empty cases Level A–C; focused Level D complete module CLI and exact diagnostics; common execution/gate policy applies.
  - Full quality gates: deferred to A-close, except mandatory per-commit contract gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `perf(native): buffer module candidate input`; exactly one focused commit before the next item.
  - Definition of done: measured module DOS reads block-scale with unchanged logical bytes and focused parity, or rejected trial.

- [ ] Step 15 · Item A-triage — Choose the next action from post-win evidence
  - Source requirement or finding IDs: SR-EARLY, SR-MEAS, SR-PAR.
  - Expected files: performance ledger and this plan only.
  - Dependencies/worktree: 14m decision commit; integration worktree.
  - Steps and boundaries: Compare current completed B01/B03/B08/B10 evidence and focused gate duration with the original configuration. If B10 still cannot finish, localize its next owner using existing counters or one bounded approved observation. Insert one concrete fix or measurement item before A-close with exact files, oracle, budget and independent plan-quality approval; repeat only after its commit and fresh evidence. Likewise split remaining baseline correctness repairs into one invariant per checkbox. No generic VM first implementation in native. Do not schedule every timeout as a separate speculative optimization.
  - Before/after and stop/go: first discriminate branch/diagnostic correctness from timeout-only work. No identical broad gate rerun until changed behavior can plausibly resolve its known failures. If no supported next step exists, record exact blocker rather than a performance claim.
  - Gate tier: focused sub-item.
  - Required focused gates: workflow/plan validators; audit raw provenance and unresolved failure mapping; common execution/gate policy applies.
  - Full quality gates: deferred to A-close, except mandatory per-commit contract gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): rerank after native throughput experiments`; exactly one focused commit before the next item.
  - Definition of done: one explicit route to completed B10 and remaining gate repair is selected, or the phase remains honestly blocked; any inserted items are individually reviewable.

- [ ] Step 16 · Item A-close — Qualify early wins and clear the recorded failure debt
  - Source requirement or finding IDs: SR-PAR, SR-MEAS, SR-TERM.
  - Expected files: qualification results, failure ledger and plan receipts.
  - Dependencies/worktree: A-triage and all inserted repair/optimization commits; no Phase B starts before PASS.
  - Steps and boundaries: After focused repairs are green, run completed frozen B01–B10 controls/candidates, non-LSP Rust and all current nonterminal native groups, preserving attempt-all behavior. Resolve every non-LSP prior failure with proof or remain blocked; LSP remains deferred to LSP-close. Do not extend timeout or suppress a test to turn it green. Report provisional accepted/reverted candidates and development-loop cost.
  - Before/after and stop/go: completed B10 repeated non-regression: candidate median may not exceed matched control by more than max(2%, measured noise); high variance over 5% requires investigation. For a censored historical control use the five-run completed-candidate qualification rule above and retain the original failure; never compute a speedup ratio from it. Mechanism wins remain provisional until this gate.
  - Gate tier: high-level closure.
  - Required focused gates: all applicable focused contracts plus workflow gate; exact corpus artifacts/diagnostics/exits; common execution/gate policy applies.
  - Full quality gates: non-LSP Rust quality gate, all current --verify-phase-zero groups, native formatter/staged/architecture/inventory, workflow and independent compliance PASS.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): qualify native throughput foundation`; exactly one focused commit before the next item.
  - Definition of done: non-LSP Rust and 51-group/current nonterminal gate pass; B01–B10 parity and B10 matched non-regression or explicitly censored-baseline completion qualification are established; all non-LSP failure debt is closed with required review; LSP debt remains assigned to LSP-close.

### Phase B — residual measured work and complete profiling

- [ ] Step 17 · Item C0 — Dispose of the old roadmap and select only residual winners
  - Source requirement or finding IDs: SR-EARLY, SR-RF, SR-MEAS, SR-ACC, F1–F11.
  - Expected files: decision ledger, this plan and baseline annotations if findings change.
  - Dependencies/worktree: A-close PASS and Item 10 decision; inserted candidates run serially from reviewed integration commits.
  - Steps and boundaries: For every row in the legacy mapping below record retained, rejected, deferred with reason, or selected with positive current evidence. Re-rank using remaining cost, implementation/proof effort, expected repeated-run savings and memory risk. Insert one concrete checkbox per selected experiment and its cleanup before B-close; independent plan-quality PASS before execution. A native VM transfer requires a successful Rust implementation record, not a Rust profile alone. No standing obligation to build all speculative optimizations.
  - Before/after and stop/go: no new evidence means defer, not implement. Native-only primitive/layout/lifecycle changes may use native evidence; generic execution representation is Rust-first. Each candidate uses the shared bounded trial and exact proof policy.
  - Gate tier: focused sub-item.
  - Required focused gates: workflow/plan gates; review numeric evidence and exact prerequisite/rollback/cleanup for selected candidates; common execution/gate policy applies.
  - Full quality gates: B-close; mandatory per-commit gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): select residual work by measured return`; exactly one focused commit before the next item.
  - Definition of done: every original pending optimization has an explicit disposition and each selected candidate has a commit-sized executable item; deferred items remain labelled unimplemented.

- [ ] Step 18 · Item P0 — Finish profile foundations after the early product wins
  - Source requirement or finding IDs: SR-ID, SR-MEAS, SR-ACC, F11.
  - Expected files: profile schema/catalog/exporter, Rust executor/service owners, native bridge adapters, bounded report tests.
  - Dependencies/worktree: C0 and selected candidate decisions; focused profiling may be pulled forward only for one named blocked decision via reviewed amendment.
  - Steps and boundaries: Inventory existing hooks before adding any. Complete stable versioned identities, bounded off/counters/sampled/trace modes and Rust/native correlation using existing records. Split implementation into reviewed one-owner or one-schema checkbox commits before P-close; never one thirteen-executor mega-commit. Start with hot owners. Required final coverage is every VM/service owner with attributable invocation/work totals or an explicit unsupported counterpart reason; detailed opcode/PC/sequence/helper/allocation/lookup/high-water/cache/accelerator profiles must be available where needed to answer remaining hotspot questions. Record cold-owner coverage without unbounded histograms.
  - Before/after and stop/go: calibrate control/counters/sampled/trace overhead and ranking. Reuse native bridge counters; do not reimplement 0a–0e as 6/6a/6b duplicates. Inventory gaps block P-close, not already proven native platform experiments.
  - Gate tier: focused sub-item.
  - Required focused gates: schema corruption/overflow/identity/disabled overhead checks; focused executor oracles and mandatory Rust/native gates for each inserted implementation; common execution/gate policy applies.
  - Full quality gates: B-close; mandatory per-commit gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): scope remaining profile coverage`; exactly one focused commit before the next item.
  - Definition of done: reviewed concrete instrumentation sub-items exist for actual gaps; no unimplemented profile contract is called complete.

- [ ] Step 19 · Item P-close — Verify profile coverage and final hotspot decisions
  - Source requirement or finding IDs: SR-ID, SR-MEAS, SR-ACC, F11.
  - Expected files: profile inventory and machine-readable result/decision reports.
  - Dependencies/worktree: all P0 sub-items committed.
  - Steps and boundaries: Verify all inserted P0 items committed; run frozen corpus with overhead controls, owner inventory, stable cross-runtime IDs and bounded trace/counters. Publish remaining hotspots, rejected ideas and limits; retain interpreters/fallbacks. Convert missing high-value attribution into a specific prerequisite rather than guessing an optimization.
  - Before/after and stop/go: all owner rows attributable; overflow never accepted as complete; no general overhead factor inferred from B03 aborts.
  - Gate tier: focused sub-item.
  - Required focused gates: coverage/identity/overflow/known-program oracles and production artifact parity; common execution/gate policy applies.
  - Full quality gates: B-close; mandatory per-commit gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): verify profiling coverage and decisions`; exactly one focused commit before the next item.
  - Definition of done: stable profiles, calibrated overhead and complete owner inventory exist and every candidate has evidence/disposition.

- [ ] Step 20 · Item 27 — Install regression budgets for accepted mechanisms
  - Source requirement or finding IDs: SR-MEAS, SR-ACC, SR-TERM.
  - Expected files: existing performance checks/results and optional CI lane.
  - Dependencies/worktree: P-close and all accepted candidate commits.
  - Steps and boundaries: Reuse deterministic operation formulas from accepted slices, with small regression sentinels added alongside each win when cheap. Here formalize bounded tolerances for clears, reads, prepare/reuse and accelerator hit/fallback/mismatch; separate hardware-dependent timing lane. Detect an injected regression. Do not add CI framework work before first wins.
  - Before/after and stop/go: document CI runtime cost and stable tolerance justification; no performance proof by absolute cross-machine wall time.
  - Gate tier: focused sub-item.
  - Required focused gates: budget positive/negative tests and workflow gate; exact representative corpus; common execution/gate policy applies.
  - Full quality gates: B-close; mandatory per-commit gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `test(perf): enforce accepted mechanism budgets`; exactly one focused commit before the next item.
  - Definition of done: accepted mechanisms have reproducible affordable regression detection.

- [ ] Step 21 · Item B-close — Qualify residual optimizations and observability
  - Source requirement or finding IDs: SR-PAR, SR-MEAS, SR-ID.
  - Expected files: results/ledger/plan receipts.
  - Dependencies/worktree: 27 and all inserted Phase B items.
  - Steps and boundaries: Run full intermediate qualification on the exact integrated production shape; compare original and Phase A baselines and report retained/rejected/deferred work.
  - Before/after and stop/go: complete repeated B10 non-regression and zero differential mismatches; code/memory/overhead reported.
  - Gate tier: high-level closure.
  - Required focused gates: all focused contracts, exact B01–B10 and workflow gate; common execution/gate policy applies.
  - Full quality gates: non-LSP Rust quality gate, current --verify-phase-zero, native gates and workflow/compliance PASS.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): qualify measured optimization program`; exactly one focused commit before the next item.
  - Definition of done: all required intermediate gates pass and terminal qualification has no hidden implementation dependency.

### Phase C — broad certainty, cleanup and terminal proof

- [ ] Step 22 · Item 28 — Qualify optimized and reference modes before cleanup
  - Source requirement or finding IDs: SR-PAR, SR-ACC, SR-TERM.
  - Expected files: raw qualification reports and temporary-path inventory.
  - Dependencies/worktree: B-close PASS.
  - Steps and boundaries: Run B01–B10 all output/listing variants, required generic/reference/optimized/dual comparisons, complete native --verify, and unchanged full-product and gen0→gen1→gen2 proof. Baseline CPU is 68020; separate reliable optional CPU/hardware results. Inventory only temporary paths actually introduced and map each to one cleanup checkbox.
  - Before/after and stop/go: compare original, post-A and current medians/ranges plus code/memory/coverage; no terminal waiver or stale proof.
  - Gate tier: high-level closure.
  - Required focused gates: full artifact/diagnostic/state/layout/fixup/exit checks; fresh guest protocol and exact self-host artifacts; common execution/gate policy applies.
  - Full quality gates: non-LSP Rust, complete native --verify including both terminal groups, native/workflow and compliance gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): qualify performance and terminal parity`; exactly one focused commit before the next item.
  - Definition of done: all complete pre-cleanup gates and terminal proof pass; every actual temporary path has a concrete cleanup owner.

- [ ] Step 23 · Item 29 — Schedule and execute only cleanup that actually exists
  - Source requirement or finding IDs: SR-PAR, SR-ACC, SR-TERM.
  - Expected files: exact temporary paths named by 28, associated tests/results and plan.
  - Dependencies/worktree: 28 PASS; all inserted cleanup items before 30.
  - Steps and boundaries: Insert one focused 29.x checkbox per actual reference/comparison representation, with files, invariants and validation. Remove prototype duplicate state and temporary legacy native resets/readers; retain portable interpreters and supported production fallback/mode contracts. Do not create ten no-op cleanup commits for rejected candidates. Each cleanup is separately reviewed and committed before the next.
  - Before/after and stop/go: mechanism counts and code/memory do not regress; any mismatch blocks cleanup.
  - Gate tier: focused sub-item.
  - Required focused gates: focused mechanism/ownership/parity gates per inserted item, no stale references; non-LSP Rust for Rust changes; common execution/gate policy applies.
  - Full quality gates: 30; mandatory per-commit gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): scope qualification-backed cleanup`; exactly one focused commit before the next item.
  - Definition of done: every actual temporary path maps to one concrete reviewed 29.x cleanup item, or is explicitly identified as a supported permanent contract; this scoping commit claims no removal. Each 29.x item requires actual removal and its focused proof before commit; Item 30 depends on all those commits.

- [ ] Step 24 · Item 30 — Prove the final performance production shape
  - Source requirement or finding IDs: SR-MEAS, SR-PAR, SR-TERM.
  - Expected files: final result report, ledger and plan receipts.
  - Dependencies/worktree: all cleanup commits; performance integration worktree.
  - Steps and boundaries: After all 29.x commits repeat complete B01–B10, all output/listing variants, final profiles, full native suite and fresh gen0→gen1→gen2 proof on exact final binaries. Compare artifacts and report retained wins, rejected/deferred ideas, Rust-only wins, native transfers, residual hotspots, machine configuration and gate costs. Only then resume/close parked Item 40 and Milestone 8 through their own required review.
  - Before/after and stop/go: compare original and pre-cleanup performance; failures/timeouts block completion regardless of earlier proof.
  - Gate tier: high-level closure.
  - Required focused gates: exact post-cleanup parity and complete terminal protocol; final plan-compliance and required finding-closure reviews; common execution/gate policy applies.
  - Full quality gates: non-LSP Rust, complete native --verify including both terminal groups, native/workflow and final review gates.
  - Plan-compliance review evidence: independent PASS citing this item, exact diff, required gate receipts, evidence limitations and rollback/disposition.
  - Commit outcome: `docs(perf): close measured performance program`; exactly one focused commit before the next item.
  - Definition of done: complete final correctness/performance gates pass, temporary paths resolved, reproducible reports published; leave the plan active for the final LSP-close item; archive only after every active checkbox and required closure review passes.

- [ ] Step 25 · Item LSP-close — Repair and qualify LSP at the very end
  - Source requirement or finding IDs: 2026-09-05 explicit user LSP deferral; SR-PAR, SR-TERM.
  - Expected files: `crates/opforge-lsp/src/**`, its integration harness/tests only as the actual failure requires, retained failure/closure report and plan receipts.
  - Dependencies/worktree: Item 30 and every earlier implementation/cleanup commit; this is the last planned work.
  - Steps and boundaries: Reproduce the retained LSP failures on the final code, find their actual cause and repair the earliest broken production or harness boundary without relaxing waits/assertions to hide failures. Insert one reviewed commit-sized LSP repair checkbox per distinct invariant immediately before this closure item, renumbering Step labels while preserving Item IDs. This closure checkbox itself records final qualification after those repair commits; no mixed multi-fix commit.
  - Before/after and stop/go: retain original 34 passed / 14 failed integration evidence as history; require all current LSP tests and the unfiltered workspace gate to pass. A timeout or missing response is never a success.
  - Gate tier: high-level closure.
  - Required focused gates: exact failing request/diagnostic tests, complete LSP integration suite, workflow/plan validators; changes to shared VM/native behavior also rerun their affected correctness/performance proof.
  - Full quality gates: `bash scripts/workflow/run_rust_quality_gate.sh` with no deferral flag, plus all gates affected by actual repair changes. No LSP exception remains at final program closure.
  - Plan-compliance review evidence: independent PASS on final diff, repair commits, LSP results, unfiltered gate and preservation of Item 30 production proof.
  - Commit outcome: `docs(perf): close deferred LSP qualification`; one focused closure commit after all repair commits.
  - Definition of done: every current LSP test and the unfiltered Rust gate pass; all previous items remain proven and the plan can be archived. No earlier phase is blocked solely by LSP.

## Legacy roadmap disposition map

These are retained options, not a serial implementation checklist. C0 must give
every row a reasoned disposition. Selected work receives a new concrete item
before implementation; deferred/rejected work is explicitly not implemented.
Original requirement IDs and baseline findings remain traceable in each decision.

| Legacy items | Current ownership / priority |
|---|---|
| 1, 0a–0e, 2 | Completed history below; preserve original receipts |
| 0f | Observation handoff now; failed broad qualification owned by A-close |
| 3–6b, 7a–7n, 8–9 | R0 minimum decision-driven profile now; P0/P-close finish shared schema, bounded modes and complete owner attribution later |
| 10 | First evidence-selected Rust elimination immediately after 17c, before native lifecycle/buffering; immediate native-transfer decision |
| 11 | Prepared/cache/predecoded Rust candidate only if residual evidence supports it |
| 12–13 | Portable IR/threading/superinstructions/generated/AOT: late, conditional; include setup, code/cache, verification and coverage cost |
| 14, 15a | 14m takes measured module scanning first; normal source buffering and seekable bounded ranges are separate optional follow-ups |
| 15 | Root preflight/existence/CPU/output scan consolidation follows buffering only when measured gain exceeds semantic risk |
| 16, 16a, 16b | 16a takes largest unused statement region first; source/text, layout-map, label/image live resets remain separate measured options |
| 17 | 17c isolates startup clear first; additional generic copies/clears only for measured remaining call sites |
| 17a | Embedded package in-place lifetime/validation change deferred behind larger observed work |
| 18–18b | Audit already-present layout-only/final-emission split; remove residual presence clears only with pass evidence, never rebuild existing separation |
| 19–20a | Prepared directives/flow/expression lifecycle only after repeated-work evidence; generic VM mechanism needs positive Rust implementation |
| 21–22b | Hot/cold statement rows, interned owners/operands/names are high-risk late options; unused clearing does not require compaction |
| 23–24 | STVM decode/index and symbol probes only when measured; native generic transfer requires positive Rust result |
| 25/25.x | One concrete native transfer per successful Rust candidate, with eligibility, shared IDs, coverage, generic fallback and paired cleanup |
| 26 | Native-only ABI/register/alignment/cache tuning after larger residual work; 68020 first, optional later CPUs separately justified |
| 27 | Small deterministic sentinels accompany wins; consolidated budgets before B-close |
| 28, 28a–28d, 29a–29k.x, 30 | Pre-cleanup terminal qualification, actual-path-only 29.x cleanup, fresh post-cleanup terminal proof |

## Milestones

| Milestone | Closure checkpoint | Required certainty |
|---|---|---|
| Bounded investigation recorded | 0f | Evidence integrity and required focused/contract gates; broad failures remain open |
| Faster iteration and balanced first wins | A-close | Completed corpus/B10 parity, repeated measurements, all nonterminal/Rust gates green |
| Measured final candidates and profiles | B-close | Complete owner coverage, bounded calibrated profiles, budgets, all intermediate gates green |
| Qualified before cleanup | 28 | All modes/outputs, full native suite and terminal self-host proof |
| Performance product | 30 | Fresh post-cleanup native/non-LSP proof and reported performance; no provisional wins |
| Complete program including LSP | LSP-close | All LSP repairs and the default unfiltered Rust gate pass at the very end |

## Blocking Rules

- `plan-compliance-reviewer` must return `PASS` before every commit.
- One active item; no next implementation item before its focused commit. A
  failed required gate blocks that item; change scope only through reviewed
  amendment, never silently proceed.
- No optimization before 0f/E0. No generic native VM acceleration without a
  positive Rust implementation decision. Native clear/I/O experiments need
  their named evidence, not the entire Rust instrumentation backlog.
- No advancement beyond A-close, B-close, 28 or 30 without its required PASS;
  the named LSP exclusion applies until the final LSP-close checkpoint.
  Unresolved baseline defects cannot be reclassified as successes by deferral.
- A regression in an affected correctness contract blocks the candidate even
  during provisional early integration. Incomplete timings never justify a
  completed-run speedup or final non-regression claim.
- All semantic repairs, profiler expansions, residual candidates and cleanup
  require concrete commit-sized checkboxes before execution. A decision to
  defer can close a decision item, never a behavior implementation claim.
- Required active AGENTS gates, architecture rules, proof protocol, finding
  closure reviews and no-push restrictions remain binding.

## Definition of Done

All active items and inserted sub-items have focused commits, independent
compliance receipts and applicable passing gates. Every historical candidate has
an explicit implemented/rejected/deferred disposition; no speculative feature
is mandatory merely because it appeared in the old queue. Profiling ownership,
shared identities, bounded modes and regression budgets are complete. Accepted
optimizations have exact production parity, completed repeated measurements,
coverage/setup/code/memory costs and rollback/cleanup decisions. The complete
native suite and fresh final gen0→gen1→gen2 proof pass after cleanup; parked
self-host work closes only with its required evidence. Publish remaining limits
and archive using the repository workflow only after final LSP-close passes.

## Commit Outcome

This LSP deferral amendment: one focused commit containing this plan, its
sidecar, `scripts/workflow/run_rust_quality_gate.sh` and
`scripts/workflow/tests/test_rust_quality_gate_scope.py` on
`codex/rust-vm-native-performance`. Preserve pending implementation files and
failed evidence. Subsequent implementation: one focused commit per active item,
including reasoned no-go decisions. No remote update is authorized.

## Historical completed receipts (non-operative chronology)

The original checked receipt bodies below are preserved for auditability;
only their item headings have gained sequential tracking labels.
Their old dependency order, future gates and contemporaneous pending notes are
superseded by the active Work Items above. They do not certify Item 0f, A-close
or any new optimization. Current open Item 0f observations and failed gate
results remain in the linked 2026-09-04/05 evidence reports; the pre-amendment
working plan had SHA-256
`57ab229e3083ba844e441e119368b365d473bf0bb59dcb44d5e517444fcc095b`.

- [x] Step 01 · Item 1 — Activate on the latest remote self-host parking checkpoint and publish the performance ledger
  - Source requirement or finding IDs: SR-ACT, SR-WT, SR-MEAS, SR-TERM, SR-NOGUESS, F12.
  - Rationale/mechanism: start from the exact committed six-hour parking state while preserving the open fail-closed terminal proof and preventing stale anchors.
  - Architectural boundaries: no production change; no history rewrite; no terminal-proof success claim; primary checkout and its unrelated files remain untouched.
  - Expected files: this plan, baseline, quality-gate sidecar, and `documentation/performance/results/opforge-native-performance-activation-ledger-2026-09-01.md`.
  - Steps: fetch origin; verify `origin/main`; create the integration worktree/branch from that exact commit; merge the reviewed planning history; record base/branch/path; re-read rules; diff relevant paths from `94e23e2`; revalidate F1-F12 anchors and commands; amend/re-review changed evidence.
  - Dependencies/worktree: first active item; user-authorized parking checkpoint `68cc693c`; long-lived `/Users/erik/Code/Retro/opForge-wt-rust-vm-native-performance` on `codex/rust-vm-native-performance`.
  - Before/after metrics: no speed claim; capture exact host/toolchain/package/corpus/native runner configuration, static counts, Rust 29.14-second checkpoint, and native two-/six-hour fail-closed observations.
  - Correctness/parity/failure validation: workflow validators; verify Item 40/Milestone 8 remain unchecked; confirm primary checkout status is unchanged except its pre-existing unrelated files.
  - Rollback/kill/reference strategy: retain `68cc693c` as immutable reference; abandon only the new performance branch/worktree if activation evidence is invalid; never rewrite the checkpoint.
  - Effort/risk and stop/go: S/Low; stop if fetched remote differs from the recorded checkpoint, drift changes a semantic contract without plan amendment, or the terminal proof is accidentally represented as complete.
  - Full quality gates: plan workflow, `make workflow-gate`, plan-quality review, and plan-compliance review.
  - Plan-compliance review evidence: `PASS` — the independent reviewer confirmed the focused four-file documentation activation, exact fetched `origin/main`/merge-base `68cc693c`, reviewed planning merge `940a9e0d`, open Item 40/Milestone 8, exact 41,221,928-byte arena formula, partial F4 drift, Item-1-first sequencing, unchanged primary checkout, and absence of production changes or push.
  - Activation evidence (2026-09-01): `git fetch origin` resolved `origin/main` to `68cc693c`; the dedicated branch/worktree was created from that commit and the reviewed planning history merged at `940a9e0d`; the activation ledger records toolchain, package digest, static counts, two-/six-hour fail-closed evidence, and F1-F12 drift. The checkbox validator, plan workflow, plan-quality review, `make workflow-gate`, diff checks, and plan-compliance review passed. Item 40 and Milestone 8 remain unchecked.
  - Commit outcome: `docs(perf): activate from native self-host parking checkpoint`.
  - Definition of done: exact remote base and isolation are recorded; drift and evidence are current; Item 40/Milestone 8 remain open; gates pass; and the activation commit exists.

#### Native attribution foundation — profile bounded production workloads without optimizing

These measurement items are the mandatory post-activation foundation. Items
0a-0e run sequentially after Item 1, Item 2 then freezes B01-B10, and Item 0f
publishes attribution from that corpus, including explicitly failed/incomplete
native baselines under the approved diagnostic entry gate. An incomplete profile is useful
localization evidence but never a completed assembly or Level D result.

- [x] Step 02 · Item 0a — Add bounded native progress and coarse phase timing
  - Source requirement or finding IDs: SR-BRIDGE, SR-PROG, SR-NOGUESS, SR-WT, SR-PAR, F12.
  - Rationale/mechanism: motivated by the historical multi-hour observation, make bounded B01-B10 runs observable and distinguish forward progress, repeated passes, and a local stall before choosing a fix.
  - Architectural boundaries: approved native debug/assert framework only; bounded memory; no CPU semantics, optimization, per-operation I/O, output change, or proof relaxation.
  - Expected files: one generic native profile/progress module, `opasm/opasm_engine.asm` phase/pass/statement boundaries, native test harness, host decoder/report tests, and a bridge result-schema note.
  - Steps: define a versioned memory-resident block with fresh run ID, complete/incomplete state, phase, pass/layout round, current/last-completed/total statement, statement visits, source/module and VM/service/program IDs, flow/backward redirects, last-progress tick, elapsed phase ticks, saturation/overflow; time only startup/package/source ingest/statement build/pass one/layout rounds/final/artifacts; add default-off low-frequency approved heartbeat and graceful abort export with `complete=false`.
  - Dependencies/worktree: Item 1 activation commit; integration worktree or a dedicated child slice, never primary/`main`.
  - Before/after metrics: disabled/counters wall time, code bytes, block/report bytes, heartbeat frequency and overhead; no speed claim.
  - Correctness/parity/failure validation: Level A-C deterministic phase/progress/saturation/abort tests; exact artifact/diagnostic/exit parity when allowed to complete; incomplete records rejected as proof; one focused fresh-challenge FS-UAE confirmation.
  - Rollback/kill/reference strategy: build/runtime off switch; keep the pre-bridge binary as reference; disable heartbeat if perturbation exceeds the recorded investigation budget.
  - Effort/risk and stop/go: M/High; stop on semantic output change, unbounded storage, per-event I/O, or inability to distinguish incomplete from complete.
  - Full quality gates: native format, staged native porting gate, Rust quality gate, focused FS-UAE confirmation, and workflow gate for schema/report docs.
  - Plan-compliance review evidence: `PASS` — the independent reviewer verified the exact staged Item 0a slice after remediation: Level-B checks lock both saturation paths; the deterministic FS-UAE harness executes them and checks their overflow bits; the decoder rejects unknown flag/overflow bits and complete-plus-abort; instrumentation remains fixed-size, default-off, release-gated, semantics-preserving, and proof-limited; and no preflight cleanup or optimization is mixed into the slice.
  - Completion evidence (2026-09-02): the decoder, source contract, deterministic harness, native format/staged-porting gates, fresh FS-UAE harness, and fresh release/counters exact-artifact CLI cases pass. The full Rust gate passes all 1,563 library tests and remaining workspace/doc tests. Counters add 1,672 Hunk bytes over the cleaned release; heartbeat adds 16 more. One end-to-end FS-UAE perturbation check measured 48.46s release versus 48.55s counters (+0.19%), explicitly not a vintage-runtime estimate.
  - Commit outcome: `feat(native-perf): expose bounded assembly progress`.
  - Definition of done: bounded production-path runs expose trustworthy phase/pass/statement progress and a complete or explicitly incomplete report with measured overhead.

- [x] Step 03 · Item 0b — Count native pass, statement, flow, and layout multiplication
  - Source requirement or finding IDs: SR-BRIDGE, SR-PROG, SR-NOGUESS, SR-MEAS, SR-PAR, F4, F6, F12.
  - Rationale/mechanism: determine whether elapsed time comes from repeated whole-input visits, control-flow rescans, backward redirects, layout retries, or repeated emission.
  - Architectural boundaries: observe generic opasm lifecycle only; no route caching, pass reduction, emission suppression, or changed convergence behavior.
  - Expected files: native profile module, `opasm/opasm_engine.asm`, `opasm/opasm_assembly_driver.asm`, native flow/navigation modules, decoder fixtures/tests.
  - Steps: count statement visits by pass, layout rounds and `LayoutChanged` reasons, convergence/final image bytes, directive classifications, flow rows, forward/backward redirects, and bounded current/max progress; export through Item 0a's envelope.
  - Dependencies/worktree: Item 0a commit; next sequential bridge slice in the same dedicated worktree lineage.
  - Before/after metrics: deterministic counts and counter slopes at fixed snapshot intervals; disabled/counters overhead and added bytes; no speed claim.
  - Correctness/parity/failure validation: Level A-C exact counter oracles for stable, unstable, forward/backward-flow, and final-emission cases; focused Level D parity; aborted record stays incomplete.
  - Rollback/kill/reference strategy: independently gate the counter group and retain Item 0a progress-only mode.
  - Effort/risk and stop/go: M/High; stop if instrumentation changes pass decisions, PC/layout, flow, images, or exceeds bounded storage.
  - Full quality gates: native format, staged native porting gate, Rust quality gate, focused FS-UAE confirmation.
  - Plan-compliance review evidence: `PASS` — the independent reviewer verified that `OFWM` is fixed-size, saturating, correlated fail-closed to `OFPR`, and independently gated; passive routines preserve registers/CCR/stack, flow sites restore D3, and pass-end restores zero status; counters observe existing pass/flow/layout/classification/image work without changing decisions or artifacts; Level-D claims remain fresh-protocol/exact-oracle bounded; B01-B10 slopes remain deferred to Items 2/0f; full self-hosting remains excluded from profiling; inventory updates validate exactly; and no optimization is included.
  - Completion evidence (2026-09-02): a separately gated, correlated 128-byte `OFWM` companion now counts pass-mode statement visits, layout rounds/reasons, flow direction/spans, retained classifications, and convergence/final image bytes with saturating groups and visible overflow. Deterministic decoder/source/harness tests pass; fresh focused and whole-CLI FS-UAE runs complete with explicit zero guest exit and exact Rust artifact parity; release and progress-only Hunk digests remain unchanged from Item 0a. The staged native gate, workflow gate, and final single-thread canonical Rust gate pass, including all 1,567 assembly-library tests. The bounded perturbation triplet measured 47.95s release, 48.32s progress-only, and 48.57s work-enabled, explicitly as end-to-end host/emulator observations rather than vintage-runtime estimates. B01-B10 attribution remains deferred to Items 2/0f; the full self-host remains terminal correctness/scalability proof only.
  - Commit outcome: `feat(native-perf): count assembly work multiplication`.
  - Definition of done: bounded reports show whether statement, flow, layout, or image work grows and at what rate without changing results.

- [x] Step 04 · Item 0c — Count native symbol and expression work
  - Source requirement or finding IDs: SR-BRIDGE, SR-PROG, SR-NOGUESS, SR-MEAS, SR-PAR, F5, F8, F12.
  - Rationale/mechanism: test the statically plausible full-label scans and repeated expression lifecycle against dynamic call/candidate/byte counts.
  - Architectural boundaries: no index, cache, interning, prepared expression, lookup-order, ambiguity, or diagnostic change.
  - Expected files: native profile module, `opasm/opasm_engine.asm`, `opasm/opasm_flow_scopes.asm`, operand-evaluation paths, `opcore/opcore_expr_bridge.asm`, decoder fixtures/tests.
  - Steps: count exact/scoped/imported/final-component lookup calls, label candidates, compared string bytes, bounded probe/chain histogram/maxima, expression-snapshot candidates, and expression parse/compile/bind/evaluate calls; preserve lookup class and phase identity.
  - Dependencies/worktree: Item 0b commit; next sequential bridge slice.
  - Before/after metrics: calls, candidates, compared bytes, expression lifecycle ratios, counter slopes, disabled/counters overhead and report bytes; no speed claim.
  - Correctness/parity/failure validation: Level A-C exact counts for exact/scoped/imported/suffix/ambiguous/missing symbols and expression success/failure; focused Level D parity; overflow visible.
  - Rollback/kill/reference strategy: independently gate histograms and detailed byte counts while retaining aggregate calls if overhead is excessive.
  - Effort/risk and stop/go: M/High; stop if lookup ordering, ambiguity, source position, diagnostics, or expression result changes.
  - Gate tier and required focused gates: focused sub-item — native format, architecture/instrumentation/inventory guards, staged native porting gate, focused decoder/source/harness/full-CLI Rust tests, and focused fresh FS-UAE counter plus exact-artifact confirmations.
  - Full quality gates: deferred to the Phase 0 closure at Item 0f unless focused evidence escalates this item.
  - Plan-compliance review evidence: `PASS` — the independent reviewer verified that staged Item 0c covers every exact/scoped/imported/final-component and expression lifecycle boundary, including the assembly-driver import callback, while preserving disabled and callback semantics. `OFSE` remains fixed, separately gated, observation-only, architecture-compliant, saturating, and fail-closed on overflow; focused Rust/native gates, refreshed contracts/inventory, fresh FS-UAE counter/artifact parity, and corrected size/digest evidence pass under the approved Item 0f full-gate deferral.
  - Completion evidence (2026-09-02): a correlated 256-byte `OFSE` record counts lookup calls/outcomes and expression lifecycle work; optional detail mode adds actual candidates, compared bytes, probe distribution/maxima, chain depth, and expression-snapshot scans without changing lookup or expression behavior. Eight decoder tests reject malformed, incomplete, uncorrelated, falsely populated aggregate, and any overflowing proof record. The focused Item 0c Rust/native group passes 4/4, the full-product capacity group passes 6/6, boundary contract/inventory and `make workflow-gate` pass, and the exact staged native porting gate passes all instrumentation, ownership, architecture/no-growth, proof-contract, and 238-file formatter checks. After closing the reviewer's missed assembly-driver imported-callback finding, fresh detail and progress-only production CLI guests completed challenged protocols with explicit zero exit and byte-for-byte live-Rust artifact equality in 46.22s and 46.35s; the focused deterministic OFSE guest had already completed in 17.04s. Same-tool Hunks are release 554,500 bytes (`17a2b255…`), Item 0b work-only 556,972 (`88265708…`), aggregate 558,064 (`d0d5bc7d…`), and detail 558,432 (`ae423af0…`). The single-run -0.13s/-0.28% delta is noise, not a speed or vintage-hardware claim. Full Rust and complete native wrapper gates remain mandatory at Phase 0 closure Item 0f.
  - Commit outcome: `feat(native-perf): count symbol and expression work`.
  - Definition of done: a bounded report can confirm or reject lookup/expression multiplication by call, candidate, byte, phase, and pass.

- [x] Step 05 · Item 0d — Count coarse native VM and service execution
  - Source requirement or finding IDs: SR-BRIDGE, SR-PROG, SR-NOGUESS, SR-ID, SR-MEAS, SR-PAR, F10-F12.
  - Rationale/mechanism: determine whether VM/service execution dominates before considering threaded dispatch, AOT translation, superinstructions, or another portable execution design.
  - Architectural boundaries: invocation and aggregate-opcode counts only; provisional CPU-neutral IDs later map to Item 3; no per-opcode timing, VM rewrite, accelerator, target semantics, or changed ABI.
  - Expected files: native profile/catalog module and TKVM, PRVM, ExprVM, expression, selection, encoding, operand, state, branch, fixup, and value service boundaries; decoder/catalog tests.
  - Steps: count invocations and total executed opcodes by VM/program/phase plus selector/encoding candidates; capture current VM/service/program in the progress block; fail visibly on unknown/overflow; defer high-cardinality opcode/PC histograms to the shared profiler.
  - Dependencies/worktree: Item 0c commit; next sequential bridge slice.
  - Before/after metrics: invocation/opcode/candidate totals and slopes, catalog/report bytes, disabled/counters overhead; no speed claim.
  - Correctness/parity/failure validation: Level A-C service-boundary counter oracles, unknown-ID and saturation tests, focused Level D artifact/diagnostic/exit parity.
  - Rollback/kill/reference strategy: retain phase/service invocation counts if opcode totals materially perturb the run; runtime/build disable remains available.
  - Effort/risk and stop/go: M/High; stop if identifiers encode CPU-specific semantics, addresses, or unstable ordering, or instrumentation alters VM state.
  - Gate tier and required focused gates: focused sub-item — native format, affected guards/inventories, staged native porting gate, focused VM/service counter tests, and focused FS-UAE confirmation.
  - Full quality gates: deferred to the Phase 0 closure at Item 0f unless focused evidence escalates this item.
  - Plan-compliance review evidence: `PASS` — the independent reviewer verified that Item 0d remains CPU-neutral and observation-only, that the four-entry VM/program nesting stack restores enclosing context without changing VM state, that Level B/C/D proofs and inventory/format/staged-native/workflow gates match the focused sub-item, and that full quality gates remain deferred to Item 0f.
  - Completion evidence (2026-09-03): a separately gated 192-byte `OFVE` companion counts TKVM/PRVM/EXVM/ExprVM and program invocations/opcodes, eight coarse services, selector/encoder candidates, and marginal phase totals. Nested VM/program and service stacks restore enclosing IDs in `OFPR`. Decoder, source-contract, harness, and full-CLI assemble tests pass; native format, inventory, staged native porting, and `make workflow-gate` pass. Fresh focused FS-UAE completed in 17.23s; all-counter CLI exact-artifact completed in 48.79s; progress-only control completed in 47.88s, each with explicit zero guest exit. Same-tool Hunks are release 554,500 (`17a2b255…`), Item 0c detail 558,432 (`ae423af0…`), and Item 0d combined 560,176 (`04318972…`). The +1.90% single-run delta is noise, not a speed or vintage-hardware claim. B01-B10 ranking remains Item 0f; the full self-host remains excluded from profiling.
  - Commit outcome: `feat(native-perf): count coarse runtime execution`.
  - Definition of done: the report ranks native VM/service aggregate work sufficiently to decide whether deeper VM profiling is warranted.

- [x] Step 06 · Item 0e — Count native platform I/O, clear, and copy work
  - Source requirement or finding IDs: SR-BRIDGE, SR-PROG, SR-NOGUESS, SR-MEAS, SR-PAR, F1-F3, F12.
  - Rationale/mechanism: quantify drive-active startup and fixed memory work across B01-B10 so the bridge report can compare them with steady-state compute rather than dismiss or overstate them based on the historical self-host observation.
  - Architectural boundaries: observe platform operations only; no buffering, read consolidation, clear elimination, package in-place use, bulk primitive, or changed DOS/error behavior.
  - Expected files: native profile module, DOS/source/module/package read boundaries, arena clear/copy call sites, decoder fixtures/tests.
  - Steps: count DOS opens/reads/read bytes/seeks/writes/write bytes/closes by source/bootstrap/module/package/artifact class; count clear/copy calls, requested/completed bytes, and range class; record source bytes/logical lines/module candidates; export through Item 0a's bounded envelope.
  - Dependencies/worktree: Item 0d commit; next sequential bridge slice.
  - Before/after metrics: operations, bytes, average bytes/call, clear/copy bytes by phase, slopes and share of elapsed phase, disabled/counters overhead; no speed claim.
  - Correctness/parity/failure validation: Level A-C exact success/short-read/EOF/error/range counter oracles, clear/copy saturation tests, focused Level D artifact/diagnostic/exit parity, incomplete abort stays explicit.
  - Rollback/kill/reference strategy: independently gate platform counter groups; retain coarser phase/progress evidence if call-site detail perturbs the run.
  - Effort/risk and stop/go: M/High; stop if DOS return/error behavior, memory contents, artifacts, diagnostics, or boundedness changes.
  - Gate tier and required focused gates: focused sub-item — native format, affected guards/inventories, staged native porting gate, focused platform-counter tests, and focused FS-UAE confirmation.
  - Full quality gates: deferred to the Phase 0 closure at Item 0f unless focused evidence escalates this item.
  - Plan-compliance review evidence: reviewer verifies complete F1-F3 observation coverage and absence of work elimination.
  - Commit outcome: `feat(native-perf): count platform io and memory work`.
  - Closure evidence (2026-09-04): final five-mode host build matrix passed (163.06s), four fresh guest subgroup oracles passed (69.45s total), final exact live-Rust CLI/export/formula proof passed (49.76s), and exported negative proof passed. Decoder 16/16, staged native gate, inventory/architecture, explicit new-module formatting/safety, plan bundle, Rust formatting and diff checks pass. Independent `platform_coverage_review` final verdict PASS confirms named F1-F3 scope, explicit exclusions, sealed export safety, proof honesty and release identity. Item 2 may begin after this focused commit; Item 0f full Rust/native gates and corpus attribution remain mandatory.
  - Definition of done: bounded records distinguish startup I/O, clears, and copies from steady-state compute with exact operation and byte counts.
  - Coverage/export continuation (2026-09-04): audited F1-F3 bulk boundaries, explicit residual-memory exclusions, I/O/bulk kill switches, zero-seek rejection, and fixed terminal exports now have focused evidence. All four subgroup guest oracles pass; a real 106-byte/eight-line CLI case exports five correlated records with exact Rust artifact parity, while an output-open failure exports an explicitly incomplete record. Repeated full-counter runs give identical work counts and 66–67 ticks versus one observer-disabled 65-tick run; this is coarse calibration, not a reliable overhead percentage. The current emulator template is 68040 despite the 68020 assembly target; Item 0f must pin the measured CPU. Final staged gate/review and commit remain before this checkbox closes. The range/phase report and three raw JSON captures record exact scope and provenance.
  - In-progress recovery checkpoint (2026-09-04): resumed the uncommitted intervening-agent draft after completed Item 0d `edcb6466`; repaired damaged conditional/decoder insertions, duplicate DOS operations, and source-reader control flow. Independent/combined profile builds, focused native counter execution, bounded CLI artifact parity, and exact disabled release identity are now checked. Item 0e remains unchecked and uncommitted: complete range/phase breakdowns, audited nested I/O/line attribution, platform subgroup gates/export, and remaining error/diagnostic proofs are still required. See `documentation/performance/results/opforge-native-platform-item0e-recovery-2026-09-04.md`; do not use the provisional counters for corpus attribution or start Item 2 yet.
  - Range/phase continuation (2026-09-04): implemented the fixed 528-byte OFIO schema 2 with separate bulk request/completion hooks, five range and nine phase rows, one-shot range selection, package-state and per-pass presence clears, startup-before-clear accounting, per-operation I/O classes, and EOF line visits. Fifteen decoder tests, independent/combined builds, the actual buffer/CCR guest oracle, exact real-CLI artifact parity, and a real output-open negative proof pass; disabled release identity is unchanged. This supersedes those recovery gaps, not Item 0e's checkbox. Separate subgroup gates, remaining inline-memory/seek coverage audit, representative-value/error/overhead evidence, staged gate, and review remain before commit. See `documentation/performance/results/opforge-native-platform-range-phase-2026-09-04.md`.

- [x] Step 07 · Item 2 — Freeze the production-path benchmark corpus and result protocol
  - Completion evidence (2026-09-04, diagnostic entry only): frozen manifest `documentation/performance/results/opforge-corpus-v1-manifest.json` has SHA-256 `fece2121b487b37e1217b4854b74308366399938e26520e06d124ed63559aed9`; the adjacent current-format Rust baseline records seven retained exact-artifact runs for all ten cases, verified Cargo-selected release identity, commands and package provenance. `opforge-corpus-v1-native-status-2026-09-04.md` retains every failed/incomplete case and historical proof limitation. Eight corpus/schema tests, baseline validation, six capacity checks, host staging/name-map/oracle checks, fresh runtime/platform counter confirmations (five guests), Rust formatting, inventory/runtime-boundary guards, and the actual staged workflow/native gate pass. Independent plan-compliance review passed the amended Item 2 boundary. Full Rust gate remains failed/incomplete and the full native wrapper remains unrun; neither is waived, and both remain required at Item 0f closure. This supersedes only the unfrozen/pending-review status below, never its native failures.
  - In-progress evidence (2026-09-04): deterministic B01-B10 generation, independent B01-B08 BIN contracts, a verified locked-release runner, fail-closed JSON ledger, and a live-oracle native adapter are implemented. All ten current cases pass seven retained Rust invocations; workflow validation passes. Initial native B01/B04/B06/B07 complete, B02/B03/B05 exceed the 120-second cutoff, and nested-include B08/B10 drafts exposed the native one-level include limit. B08/B10 now use sibling includes and a transitive module graph: corrected B08 matches BIN bytes with zero exit but fails the empty-stdout contract; enlarged 256-group B10 times out. B09 completes with a real unresolved-label error and is not parity proof. Focused counter confirmation passes; staging/capacity repairs pass focused host checks without increasing allocation limits. See `documentation/performance/opforge-production-corpus-v1.md`. The user approved diagnosis of these explicitly failed/incomplete cases before successful native parity. The corpus remains unfrozen pending the Item 2 manifest/result ledger, review, and commit; no Item 0f completion is implied.
  - Source requirement or finding IDs: SR-MEAS, SR-PAR, SR-REMOTE, F1-F12.
  - Rationale/mechanism: make the attribution report and every later mechanism/end-to-end result comparable.
  - Architectural boundaries: fixtures traverse real CLI/package paths; synthetic cases isolate mechanisms but cannot replace the bounded representative integrated B10 workload. Neither B10 nor any reduced case is terminal self-host proof.
  - Expected files: `crates/opforge-asm/tests` or existing benchmark-fixture area, `scripts/performance/**`, `documentation/performance/results/**`.
  - Steps: map/reuse B01-B10; define B10's integrated composition, coverage, digest, and bounded observation window; add deterministic generators/manifests only for gaps; record commands, outputs, cold/warm policy, seven-run default, median/range/p95, and comparison schema; add artifact/diagnostic checks. Freeze exact inputs and a per-case native status ledger even when native runs fail or time out; mark physical-A6000 runtime and successful native completion unvalidated where evidence is absent.
  - Dependencies/worktree: Item 0e after Item 1; integration worktree unless fixture generation merits a child slice.
  - Before/after metrics: establish unprofiled Rust release wall times and sizes; capture native wall time only for corpus cases with reliable completion.
  - Correctness/parity/failure validation: generated/reused corpora produce expected artifacts/diagnostics; malformed-result and missing-artifact checks fail closed.
  - Rollback/kill/reference strategy: fixture manifests are versioned; remove synthetic cases that perturb the target or duplicate real coverage.
  - Effort/risk and stop/go: M/Medium; stop if B10's inputs/composition are not representative and reproducible or its observation is unbounded. Native timeout/failure does not block diagnostic attribution; it blocks correctness/performance acceptance and stays an open finding. Do not shrink away a failing mechanism to obtain a green corpus.
  - Gate tier and required focused gates: focused sub-item — focused corpus/schema tests and workflow gate for report/schema docs.
  - Full quality gates: deferred to the Phase 0 closure at Item 0f unless focused evidence escalates this item.
  - Plan-compliance review evidence: reviewer checks all ten matrix categories, B10 coverage/observation bounds, real-path commands, explicit failed/incomplete statuses, and absence of inferred native completion or physical runtime.
  - Commit outcome: `test(perf): establish reproducible production benchmark corpus`.
  - Definition of done: B01-B10 inputs/commands/package are frozen and digestible, Rust artifacts repeat, the result protocol is validated, and each native attempt has an explicit passed/failed/incomplete status with evidence provenance. Successful native parity is not required for Item 0f diagnostic entry; failures remain open, never comparison-eligible successful timings, and all final correctness gates are unchanged. Review and a focused commit precede Item 0f.
