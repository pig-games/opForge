<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->

# opForge Rust-First VM and Native AmigaOS Performance Optimization Plan v0.1

## Metadata

- Source: `dev-docs/NextSteps/opforge-native-performance-plan-codex-prompt-v4-rust-first-separate-worktree.md` (uncommitted read-only task input in the invoking checkout), user activation clarification, the 2026-09-01 A6000 multi-hour self-assembly observation and instruction to profile before optimizing, the 2026-09-02 direction that full self-host assembly is not the performance/profiling workload, and `documentation/performance/opforge-rust-vm-native-amigaos-performance-baseline-v0_1.md`
- Mode: implementation plan with evidence-gated performance remediation
- Owner: opForge maintainers and implementing Codex threads
- AGENTS binding statement: the active root `AGENTS.md` and every applicable nested rule pack remain binding during execution. Native slices must load `agents/rules/native-rust-parity-porting.md`, `native-68000.md`, `native-68000-safe-instrumentation.md` when instrumentation changes, `fs-uae.md` for emulator work, `native-parity-failure-triage.md` on failures, and `multi-agent-gates.md` where its gates apply.

## Goal

Build a reproducible Rust-first performance program that identifies and improves
real VM/package-runtime hotspots without weakening the generic interpreter,
package ownership, or parity discipline; independently remove verified native
AmigaOS platform waste; and transfer only positively proven Rust VM ideas into
native accelerators. Finish with regression budgets and the unchanged native
generation-zero to generation-one to generation-two proof.

No optimization candidate is selected or reordered until a native compute-
attribution report and the relevant Rust hotspot report expose its measured
operation share. The first response to the observed multi-hour native run is
progress and multiplication instrumentation, not an assumed I/O, lookup, VM,
or representation fix.

Routine profiling and performance acceptance use B01-B09 mechanism cases plus
the bounded representative integrated B10 workload. The full opForge
generation-zero to generation-one to generation-two self-host remains a
separate terminal correctness/scalability proof only: it is not used for
hotspot ranking, profiler-overhead calibration, repeated before/after timing,
or optimization acceptance.

This plan is active from remote checkpoint
`68cc693c40fd27e30bed11e08974d3263d6cb6f6`. That commit deliberately parks
Item 40 and Milestone 8 of
`documentation/plans/opforge-native-amigaos-680x0-full-support-self-hosting-plan-v0_1.md`
after CPU-active native generation-one runs produced no completion, exit, or
artifacts at two and six hours. The user's 2026-09-01 activation direction
supersedes the former requirement to wait for terminal-plan finalization; it
does not mark either checkbox complete or reinterpret the missing proof.

Items 0a-0e, Item 2, and Item 0f are now the first post-activation sequence. They add
bounded progress/counters, coarse timing, symbolized sampling support, and
incomplete abort reports before any optimization. They may not change output
behavior, relax proof requirements, or turn incomplete evidence into proof.

## Version Impact

- Versioning impact: Patch while profiler controls and optimizations remain
  internal and behavior-compatible. Any user-visible CLI/profile contract must
  be explicitly approved and versioned before release.
- Affected component(s): Rust VM/package runtime, native AmigaOS/680x0 runtime,
  internal performance tooling, benchmark evidence, and CI performance budgets.
- Impact class: patch
- Owned contract: package-driven VM semantics, CLI/artifact/diagnostic behavior,
  and fail-closed native parity/self-hosting proof remain unchanged.
- Rationale: the program changes implementation cost and internal observability;
  it does not intentionally add user-visible assembler behavior or output-format
  contracts.

## Existing Behavior

- Rust already has broad phase and dynamic path timing, prepared source/routes,
  package-driven VMs, and full semantic/reference tests, but lacks stable
  VM/program/opcode/PC/helper/allocation attribution and machine-readable profile
  output.
- Historical Rust work improved listing, stabilization, parsing, token reuse,
  routing, tokenizer validation, and one exact tokenizer path; an attempted
  parser fast path improved a local bucket but regressed end-to-end and was
  reverted.
- Native startup byte-clears a 41,221,928-byte capacity arena; source/bootstrap
  and module discovery use one-byte DOS reads; the 368,278-byte embedded package
  is copied before use; pass-two convergence can repeat image work; expressions
  are compiled per evaluation; directive/flow/state/symbol work is repeatedly
  decoded or scanned; statement rows reserve 308 bytes each.
- A 2026-09-01 A6000 self-assembly ran beyond 4 hours 45 minutes. Drive activity
  was heavy only for the first minutes and then silent for hours. At the reported
  roughly 120 MIPS, this is on the order of 2.052 trillion instruction-
  equivalents, about 41.0 million per roughly 50,000 source statements or 4.56
  million per statement-pass visit if all nine possible visits occur. These are
  scale estimates, not attribution, but they make an unmeasured algorithmic
  multiplier or spin a first-class concern.
- Good native choices already present—bulk external package reads and artifact
  writes, one module index per invocation, TKVM jump-table/fused scanner paths,
  and package-owned CPU semantics—must remain.

## Target Behavior

- Stable, versioned, machine-readable Rust profiles rank all VM and service work
  by phase, owner, program, opcode, PC, sequence, helper, allocation/clone,
  lookup, high-water, cache, and accelerator outcome with measured overhead.
- Native long runs expose bounded memory-resident progress, phase/pass/statement
  position, multiplier counters, coarse timing, and an explicitly incomplete
  diagnostic snapshot on graceful abort. Five-to-ten-minute counter slopes and
  symbolized PC samples can localize work without being misrepresented as proof.
- Generic VM optimizations start in Rust, retain the portable interpreter, and
  support disabled, generic-only, accelerated, and bounded dual-compare modes.
- Native input reads scale with blocks; unused arena capacity is not cleared;
  immutable package bytes are not redundantly copied without a measured reason;
  convergence emits no final bytes and final emission happens once; repeated
  immutable routes/expressions/state are prepared safely; compact records reduce
  bytes per live object.
- Native generic-VM accelerators exist only for positive Rust decision records;
  native-only ABI/cache/alignment tuning is separately profiled on 68020 first.
- VM bytecode may remain the canonical portable semantic/distribution format
  while measured hot programs are lowered through a validated portable execution
  IR to predecoded, threaded, superinstruction, source-generated, or target-
  assembled forms. Every derived form is signature-bound and retains the
  interpreter as oracle/fallback.
- Every accepted slice has mechanism counters, production-path before/after
  results, exact artifact/diagnostic/exit parity, rollback controls, and a focused
  commit in an isolated performance worktree.
- B10 exercises the integrated production path within a repeatable profiling
  window; terminal self-hosting is run separately only at the plan's explicit
  terminal proof gates.

## Inputs and Evidence

- Baseline: `documentation/performance/opforge-rust-vm-native-amigaos-performance-baseline-v0_1.md`
- Prerequisite: `documentation/plans/opforge-native-amigaos-680x0-full-support-self-hosting-plan-v0_1.md`
- Historical Rust work: `documentation/plans/completed/opforge-vm-runtime-performance-refactor-plan-v0_1.md`
- Rust entrypoints: `crates/opforge-cli-core/src/run.rs`,
  `crates/opforge-engine/src/{lib.rs,source_graph.rs}`,
  `crates/opforge-asm/src/{engine.rs,phase_profile.rs,runtime_model.rs}`,
  `crates/opforge-vm/src/**`, and `crates/opforge-core/src/expr_vm.rs`
- Native entrypoints: `native/motorola68000/amigaos/{opforge-cli,opasm,tkpkg,tkvm,prvm,exprvm,opcore}/**/*.asm`
- Validation: `scripts/workflow/run_rust_quality_gate.sh`,
  `scripts/workflow/run_native_porting_quality_gate.py`,
  `scripts/workflow/run_native_existing_parity_completion.sh`, Makefile targets,
  and `crates/opforge-asm/src/tests/native_fs_uae_parity.rs`
- Related infrastructure, read-only: opFoundryCore
  `docs/planning/11_Amiga_Remote_Test_Execution_Architecture_Design.md`
- Historical measurements and static calculations are transcribed in the
  baseline; all performance accept/reject decisions use fresh measurements.
- Maintainer field evidence: A6000 self-assembly over 4:45, startup-only drive
  activity followed by hours of apparent compute, and explicit direction to
  instrument/profile before choosing an optimization.

## Scope

### In Scope

- Track R: profiler foundations, full Rust VM characterization, evidence-gated
  generic improvements and accelerators, differential modes, and decision records.
- Track N: native buffered I/O, scan consolidation, lifecycle/reset, bulk memory,
  package copy removal, layout/emission separation, prepared state, compact
  representation, and measured index improvements.
- Track T: shared native correlation, transfer decisions, ports of successful
  Rust mechanisms, profile-gated portable-IR/target-backend generation,
  native-only 68020 ABI/dispatch/cache tuning, optional later 68040/68060/68080
  variants, budgets, and terminal proof.
- Reproducible real-CLI benchmark corpus, machine-readable results, profile
  overhead calibration, exact parity, and failure paths; separately, unchanged
  terminal self-hosting proof.
- The activated native-attribution foundation: progress, operation counts,
  coarse timing, bounded abort snapshots, and symbolized sampling before any
  optimization.

### Out of Scope

- Claiming the parked self-hosting plan, Item 40, or Milestone 8 is complete;
  terminal proof resumes after measured performance work.
- Discovering or prototyping generic VM semantics first in 680x0 assembly.
- CPU/family/dialect semantics in generic Rust/native implementation paths.
- Benchmark-, path-, fixture-, generation-, source-text-, or expected-output-
  specific shortcuts; hidden fallback; reduced proof; per-event I/O.
- Making AMMX/68080 the baseline, permanently keeping comparison scaffolding, or
  replacing the portable interpreter as compatibility fallback.
- Modifying opFoundryCore or blocking Rust work on remote-runner availability.
- Broad cleanup, unrelated refactors, or output/diagnostic contract changes.
- Treating an incomplete/aborted profile, emulator sample, timeout, or progress
  heartbeat as Level D evidence or terminal self-hosting completion.
- Using the full self-host assembly as the routine profiler, performance
  comparison, or optimization acceptance workload.

## Worktree and execution policy

Use the long-lived sibling performance integration worktree created from remote
checkpoint `68cc693c` on branch `codex/rust-vm-native-performance`. Use a sibling child worktree/branch for a
high-risk or independently reviewable slice; lower-risk sequential slices may
run directly in the performance integration worktree. Never execute a slice in
the primary checkout or on `main`, and never push without explicit current-turn
authorization.

Item 1 activates and records the checkpoint before Items 0a-0e, Item 2, and
Item 0f run sequentially
in this integration worktree or reviewed child slices. They never run in the
primary checkout. The primary checkout's unrelated untracked files are outside
this plan and remain untouched.

Before each item, record in the performance ledger: prerequisite integration
commit, slice base, branch, absolute sibling worktree path, active item, expected
commit, and integration dependency. After the item, record validation evidence,
result commit, and accepted/rejected/reverted status. Exactly one item is active.
Each item ends in one new focused commit before another starts. A stopped or
rejected experiment still ends in a decision-record/revert commit if it changed
the integration branch.

Every item below requires a `plan-compliance-reviewer` PASS before commit.
Commit-sized sub-items use risk-matched focused Rust tests plus the affected
formatter, architecture, inventory, staged native, and external proof gates.
They do not rerun the complete Rust workspace by default. Each `### Phase`
boundary is a high-level group; its final item is the closure checkpoint and
must run `scripts/workflow/run_rust_quality_gate.sh`, the complete current native
wrapper where native behavior changed, and the other phase gates before the
next phase begins. Phase 0's closure checkpoint is Item 0f after Items 0a-0e and
Item 2. A cross-cutting change or focused failure can escalate any sub-item to a
full gate when the plan-compliance reviewer finds focused evidence insufficient.
Workflow/plan/report changes run `make workflow-gate`. Each non-closure item's
`Full quality gates` field names the closure checkpoint to which that suite is
deferred; closure items list the mandatory full suite. No checkbox is marked
complete before its applicable gates pass and its commit exists.

## Requirement and finding identifiers

| ID | Requirement |
|---|---|
| SR-ACT | Begin from the latest fetched remote parking checkpoint; audit drift and preserve the open terminal proof. |
| SR-BRIDGE | Run reviewed observation instrumentation immediately after activation; no optimization or proof weakening. |
| SR-WT | Dedicated performance worktrees; record base, branch, path, commit, dependency. |
| SR-RF | Rust profiles and implements generic VM changes before native transfer. |
| SR-ID | Stable shared IDs and machine-readable Rust/native-correlatable output. |
| SR-MEAS | Real CLI corpus, repeated/control runs, overhead calibration, no invented claims. |
| SR-ACC | Signature/capability eligibility, generic fallback, four rollout modes, decision record. |
| SR-NATIVE | Eliminate native work before instruction-level tuning; 68020 baseline. |
| SR-PAR | Exact artifact, diagnostic, state/layout/fixup, exit, and failure-path parity. |
| SR-ARCH | Package ownership, CPU-neutral generic layers, and future portability. |
| SR-REMOTE | Optional OFTB/OFTR-style remote automation without blocking early work. |
| SR-TERM | Preserve gen0 -> gen1 -> gen2 terminal self-hosting proof. |
| SR-PROG | Long native runs expose bounded progress and explicitly incomplete diagnostic snapshots. |
| SR-NOGUESS | Every optimization item cites a positive relevant Item 0f/Phase 0/Phase 1 measured threshold; otherwise stop for a reviewed no-go/reorder amendment. |

Finding IDs F1-F12 refer to the companion baseline.

## Workstream and phase map

| Phase | Primary track | Role |
|---|---|---|
| Activation/native attribution | Track N/T measurement only | checkpoint activation, native progress and compute attribution; no optimization |
| 0 | Track R foundation plus Track N/T correlation foundation | activation, corpus, shared identities, Rust profiler, native platform/correlation counters |
| 1 | Track R | complete Rust VM instrumentation and authoritative hotspot report |
| 2 | Track R | initial evidence-gated Rust optimization experiments and decisions |
| 3 | Track N | native input buffering and scan elimination |
| 4 | Track N | native session lifecycle, bulk operations, and package-copy elimination |
| 5 | Track N | native layout-only convergence and one final emission |
| 6 | Track N informed by Track R | prepared directive, flow, and expression state |
| 7 | Track N | deep native statement/string representation changes |
| 8 | Track N informed by Track R | prepared STVM and measured symbol indexing |
| 9 | Track T | explicit Rust-to-native transfer decisions, concrete ports, then native-only tuning |
| 10 | Tracks R/N/T closure | budgets, qualification, removal of temporary rollout paths, and terminal proof |

Track R is the only discovery and first-implementation track for generic VM
optimization. Track N can remove independently proven platform work after Phase
0. Track T cannot schedule a generic native accelerator until Track R has a
positive decision record and the plan has been amended with one concrete slice.

Every optimization work item below inherits SR-NOGUESS even where its finding
list does not repeat that ID. Its plan-compliance receipt must cite the raw
Item 0f/Phase 0/Phase 1 measurement, the numeric acceptance threshold, and the
observed value that passed it. Missing, ambiguous, or below-threshold evidence
produces a reviewed no-go/reorder amendment, not implementation.

## Work Items

### Phase 0 — activation, profiler architecture, baselines, and reproducibility

- [x] Item 1 — Activate on the latest remote self-host parking checkpoint and publish the performance ledger
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
publishes attribution from that corpus. An incomplete profile is useful
localization evidence but never a completed assembly or Level D result.

- [x] Item 0a — Add bounded native progress and coarse phase timing
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

- [x] Item 0b — Count native pass, statement, flow, and layout multiplication
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

- [x] Item 0c — Count native symbol and expression work
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

- [x] Item 0d — Count coarse native VM and service execution
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

- [x] Item 0e — Count native platform I/O, clear, and copy work
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

- [ ] Item 2 — Freeze the production-path benchmark corpus and result protocol
  - Source requirement or finding IDs: SR-MEAS, SR-PAR, SR-REMOTE, F1-F12.
  - Rationale/mechanism: make the attribution report and every later mechanism/end-to-end result comparable.
  - Architectural boundaries: fixtures traverse real CLI/package paths; synthetic cases isolate mechanisms but cannot replace the bounded representative integrated B10 workload. Neither B10 nor any reduced case is terminal self-host proof.
  - Expected files: `crates/opforge-asm/tests` or existing benchmark-fixture area, `scripts/performance/**`, `documentation/performance/results/**`.
  - Steps: map/reuse B01-B10; define B10's integrated composition, coverage, digest, and repeatable A6000 runtime envelope; add deterministic generators/manifests only for gaps; record commands, outputs, cold/warm policy, seven-run default, median/range/p95, and comparison schema; add artifact/diagnostic checks.
  - Dependencies/worktree: Item 0e after Item 1; integration worktree unless fixture generation merits a child slice.
  - Before/after metrics: establish unprofiled Rust release wall times and sizes; capture native wall time only for corpus cases with reliable completion.
  - Correctness/parity/failure validation: generated/reused corpora produce expected artifacts/diagnostics; malformed-result and missing-artifact checks fail closed.
  - Rollback/kill/reference strategy: fixture manifests are versioned; remove synthetic cases that perturb the target or duplicate real coverage.
  - Effort/risk and stop/go: M/Medium; stop if B10 is not representative, bounded, and reproducible; fix the corpus before attribution.
  - Gate tier and required focused gates: focused sub-item — focused corpus/schema tests and workflow gate for report/schema docs.
  - Full quality gates: deferred to the Phase 0 closure at Item 0f unless focused evidence escalates this item.
  - Plan-compliance review evidence: reviewer checks all ten matrix categories, B10 coverage/runtime envelope, and real-path commands.
  - Commit outcome: `test(perf): establish reproducible production benchmark corpus`.
  - Definition of done: B01-B10 are named, digestible, repeatable, parity-checked, and emit a comparable result ledger before Item 0f consumes them.

- [ ] Item 0f — Publish the bounded native compute-attribution report
  - Source requirement or finding IDs: SR-BRIDGE, SR-PROG, SR-NOGUESS, SR-MEAS, SR-PAR, F1-F12.
  - Rationale/mechanism: replace competing guesses with measured slopes, a ranked owner/mechanism decision, and the smallest justified next profiling or optimization slice.
  - Architectural boundaries: investigation/report only; a timed-out or aborted run remains localization evidence; no terminal-proof checkbox closes without actual fresh completion and explicit zero guest exit.
  - Expected files: `documentation/performance/results/**` raw manifests/reports, symbol/map and sampling command manifest, this plan/baseline only if measured evidence changes priorities.
  - Steps: run the frozen B01-B10 corpus under reproducible bounded FS-UAE configurations and retain targeted symbolized PC samples; run the same representative cases on the physical A6000 where feasible; compare statement/pass/lookup/expression/flow/image/VM/DOS/clear/copy slopes; distinguish progress from spin; rank measured owners and publish stop/go evidence without implementing a fix. Do not run or require the full self-host assembly for this report.
  - Dependencies/worktree: Item 2 commit after Item 0e and access to its frozen representative native B01-B10 workloads; same sequential bridge lineage; physical runs may be collected manually into the reviewed result envelope.
  - Before/after metrics: elapsed and progress deltas, operations/second and operations/statement-visit, phase shares, PC sample shares, counter overhead/control comparison, overflow/completeness; no speed claim.
  - Correctness/parity/failure validation: manifests/digests/configuration, repeated bounded snapshots, decoder integrity, explicit incomplete state, exact artifacts/diagnostics/exit only for completed runs; Level D rules unchanged.
  - Rollback/kill/reference strategy: if counters perturb ranking, rerun with successively coarser groups and PC sampling; if attribution remains ambiguous, amend a narrower instrumentation item rather than guess.
  - Effort/risk and stop/go: M/Medium; proceed to Item 3 only with a reviewed attribution report, or after a reviewed amendment naming the missing measurement—not an assumed fix.
  - Gate tier and required focused gates: Phase 0 high-level closure — focused report/schema validation, `make workflow-gate`, native format/staged porting gate, and focused confirmations accumulated by the phase.
  - Full quality gates: authoritative full Rust quality gate and the complete current native FS-UAE wrapper; earlier focused production receipts remain supporting evidence but do not replace this closure gate.
  - Plan-compliance review evidence: reviewer verifies raw evidence, field/emulator distinction, completeness flags, overhead, ranked mechanisms, and absence of an optimization claim.
  - Commit outcome: `docs(perf): attribute native assembly compute work`.
  - Definition of done: bounded representative runs provide a reproducible progress diagnosis and ranked measured owners; the historical multi-hour self-host observation remains motivation, not the profiling workload or proof.

- [ ] Item 3 — Define shared profile IDs and versioned result schema
  - Source requirement or finding IDs: SR-ID, SR-RF, SR-ARCH, F11.
  - Rationale/mechanism: stable identities make Rust hotspot reports and later native correlation meaningful.
  - Architectural boundaries: registry is CPU-neutral; program identity derives from package/version/owner/table/offset/length/digest, never source path or benchmark identity.
  - Expected files: `crates/opforge-types/src/**` and/or `crates/opforge-vm/src/**`, schema/report module, focused tests, profile-format documentation.
  - Steps: enumerate VM/phase/helper/accelerator IDs; define metadata/catalog/counter/timing/histogram/terminal records; choose versioned JSONL logical schema; validate uniqueness, unknown IDs, integrity, and deterministic ordering.
  - Dependencies/worktree: Item 0f after Item 2; child slice worktree because schema affects both tracks.
  - Before/after metrics: measure disabled-path code/time and catalog/export size; no optimization claim.
  - Correctness/parity/failure validation: golden logical records, malformed/unknown/collision/version rejection, cross-platform endian tests for compact native records.
  - Rollback/kill/reference strategy: schema version gate; no consumer accepts an incompatible version silently.
  - Effort/risk and stop/go: M/Medium; stop if IDs depend on addresses or unstable iteration order.
  - Full quality gates: focused schema tests and Rust quality gate; workflow gate for format docs.
  - Plan-compliance review evidence: reviewer verifies complete VM inventory and CPU-neutral ownership.
  - Commit outcome: `feat(perf): define shared versioned profile identity schema`.
  - Definition of done: every baseline VM/service maps to stable IDs and deterministic validated records.

- [ ] Item 4 — Implement zero/low-overhead Rust deterministic counters
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism: deterministic counts reveal coverage and repeated work without timer noise.
  - Architectural boundaries: off mode effectively zero-cost; no per-event I/O; bounded/saturating storage; semantics unchanged.
  - Expected files: `crates/opforge-vm/src/**profile**`, `crates/opforge-asm/src/phase_profile.rs`, VM entrypoints, focused tests.
  - Steps: add off/counters modes; nested phase context; VM/program/opcode/PC, branch, helper, high-water, lookup, allocation/clone, cache and accelerator events; export once; bridge existing phase buckets.
  - Dependencies/worktree: Item 3; child slice.
  - Before/after metrics: control versus counters wall time/code size/memory on B01-B10; event-count determinism across repeated runs.
  - Correctness/parity/failure validation: exact artifacts/diagnostics; counter overflow and nested-context tests; no output when disabled.
  - Rollback/kill/reference strategy: compile-time feature plus runtime off switch; remove/reshape counters whose overhead exceeds Phase 0 budget.
  - Effort/risk and stop/go: L/Medium; go only if counters are deterministic and overhead is documented and acceptable for investigation/CI subsets.
  - Full quality gates: focused profiler/VM tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer inspects hot-path gating and absence of event I/O.
  - Commit outcome: `feat(perf): add deterministic Rust VM counters`.
  - Definition of done: all inventory rows expose required counts with calibrated disabled/counters overhead.

- [ ] Item 5 — Add Rust sampled timing, bounded trace, and report tooling
  - Source requirement or finding IDs: SR-ID, SR-MEAS, F11.
  - Rationale/mechanism: distinguish dispatch from helpers and reconstruct hot sequences without timing every opcode.
  - Architectural boundaries: deterministic sampling configuration; bounded ring; explicit overflow; derived human reports never replace raw evidence.
  - Expected files: Rust profile collector/exporter, `scripts/performance/**`, report tests/docs.
  - Steps: add sampled/timed and targeted trace modes; inclusive/exclusive scopes; filters; JSONL export; ranked report/diff command; integrate `xctrace` command capture; calibrate timer/sample resolution.
  - Dependencies/worktree: Item 4; child slice.
  - Before/after metrics: control/counters/sampled/trace overhead and variance on B01/B03/B10; raw/report sizes and dropped events.
  - Correctness/parity/failure validation: bounded overflow, corrupt/truncated report, nesting, deterministic trace-filter, and artifact parity tests.
  - Rollback/kill/reference strategy: mode switches; reject high-perturbation timing from decision evidence and use host sampling/counters instead.
  - Effort/risk and stop/go: L/Medium; stop if profile perturbation changes hotspot ordering without a lower-overhead alternative.
  - Full quality gates: focused collector/tool tests and Rust quality gate; workflow gate for format documentation.
  - Plan-compliance review evidence: reviewer verifies all four modes and calibration evidence.
  - Commit outcome: `feat(perf): add sampled Rust VM profiles and bounded traces`.
  - Definition of done: machine/human reports answer phase/program/opcode/PC/sequence/helper questions with measured overhead.

- [ ] Item 6 — Promote the bridge profiler into the shared schema and add DOS/source counters
  - Source requirement or finding IDs: SR-ID, SR-NATIVE, SR-MEAS, SR-PAR, SR-PROG, F1, F3, F12.
  - Rationale/mechanism: retain the proven bridge observability, map it into the stable shared schema, and quantify native platform work for later work-elimination decisions.
  - Architectural boundaries: approved native debug/assert framework only; bounded memory; explicit complete/incomplete envelope; one terminal export on completed proof runs; no semantic behavior change.
  - Expected files: bridge native profile module, shared-ID catalog/schema adapters, DOS/source/copy call sites, Rust host decoder/tests, native harness.
  - Steps: promote Items 0a-0e records to Item 3 stable IDs/logical JSONL; preserve progress and incomplete-abort decoding; extend DOS/source/copy classifications where the full B01-B10 corpus requires them; validate old provisional bridge records fail or migrate explicitly. Detailed lifecycle and correlation extensions remain Items 6a and 6b.
  - Dependencies/worktree: Items 0a-0f, 2, 3, and 5; child worktree after Item 5 commits. No item is active in parallel.
  - Before/after metrics: native off/counters overhead, buffer footprint, export size, repeatability; no speed claim.
  - Correctness/parity/failure validation: Level A-C counter tests plus focused Level D fresh-challenge case; corrupt/overflow/missing-report failure paths.
  - Rollback/kill/reference strategy: build flag and runtime off; omit detailed opcode counters until later if memory/overhead fails budget.
  - Effort/risk and stop/go: L/High; stop if instrumentation violates memory headroom or Level D protocol.
  - Full quality gates: native format, staged native porting gate, Rust quality gate, focused FS-UAE confirmation.
  - Plan-compliance review evidence: reviewer checks approved instrumentation and shared-ID fidelity.
  - Commit outcome: `feat(native-perf): add bounded platform and correlation counters`.
  - Definition of done: the bridge collector uses stable shared identities and emits progress plus DOS/source/memory-operation records with explicit completeness/overflow and exact parity.

- [ ] Item 6a — Add native opasm lifecycle, pass, emission, and high-water counters
  - Source requirement or finding IDs: SR-ID, SR-NATIVE, SR-MEAS, SR-PAR, F2-F4, F6-F9.
  - Rationale/mechanism: quantify arena reset, live capacity, pass rounds, flow, and image work without mixing VM identity hooks into the collector commit.
  - Architectural boundaries: counters observe generic opasm state only; bounded memory and one terminal export; no per-event I/O or CPU semantics.
  - Expected files: native profile module, `opasm/opasm_engine.asm`, layout/flow/image call sites, Rust decoder fixtures/tests.
  - Steps: promote and extend Items 0b-0c pass/layout/convergence/flow/symbol records with bytes/ranges cleared and used/peak source/statement/label/image/scratch counters; retain measured bridge semantics in the shared schema/export.
  - Dependencies/worktree: Item 6; next sequential child slice from its integration commit.
  - Before/after metrics: off/counters overhead, buffer/report size, deterministic B01/B03-B10 counts.
  - Correctness/parity/failure validation: saturation/overflow and missing/corrupt-record tests, Level A-C counter oracles, focused Level D parity.
  - Rollback/kill/reference strategy: build/runtime off; disable individual high-cardinality counter groups if boundedness fails.
  - Effort/risk and stop/go: M/High; stop if memory headroom or Level D evidence is compromised.
  - Full quality gates: native format, staged native gate, Rust quality gate, focused FS-UAE confirmation.
  - Plan-compliance review evidence: reviewer verifies this commit only instruments opasm/platform state.
  - Commit outcome: `feat(native-perf): count opasm lifecycle and pass work`.
  - Definition of done: F2-F4/F6-F9 native mechanism counts and high-water values are deterministic and parity-neutral.

- [ ] Item 6b — Add coarse native VM/program/phase correlation IDs
  - Source requirement or finding IDs: SR-ID, SR-RF, SR-PAR, F5, F9-F11.
  - Rationale/mechanism: correlate selected native VM/service coverage to Rust identities without using native as generic-hotspot discovery authority.
  - Architectural boundaries: shared stable IDs only; no native opcode optimization or new semantics; detailed native counters are bounded and optional.
  - Expected files: native profile catalog, tkpkg/opasm service boundaries, Rust decoder/catalog tests.
  - Steps: replace Item 0d provisional IDs with Item 3 IDs for native TKVM/PRVM/ExprVM/selection/encoding/operand/state/branch/fixup/value owners; retain invocation/opcode totals by phase/program; validate unknown IDs fail visibly.
  - Dependencies/worktree: Item 6a; next sequential child slice.
  - Before/after metrics: mapped invocation coverage, off/counters overhead, catalog/report bytes.
  - Correctness/parity/failure validation: Rust/native catalog round-trip, unknown/version failures, focused Level D exact parity.
  - Rollback/kill/reference strategy: retain platform-only profiles when VM detail exceeds memory/overhead budget.
  - Effort/risk and stop/go: M/High; stop at invocation-level correlation if per-opcode storage is not justified.
  - Full quality gates: native format, staged native gate, Rust quality gate, focused FS-UAE confirmation.
  - Plan-compliance review evidence: reviewer verifies IDs match Rust catalog and no optimization is introduced.
  - Commit outcome: `feat(native-perf): correlate native runtime identities`.
  - Definition of done: every native counterpart has a tested shared identity or explicit unsupported reason.

### Phase 1 — serious Rust VM hotspot characterization

Items 7a-7m are deliberately one-executor commits. Each has the same complete
slice contract: generic CPU-neutral hooks only; a child worktree based on the
previous integrated 7-series commit; invocation/opcode/PC/pair/triple/branch and
relevant high-water counters; control/counters overhead plus representative-case
coverage before/after; known-program, malformed-program, error-path, and exact
normal-output validation; independently disableable high-cardinality histograms;
M/Medium effort/risk with a stop when boundedness or overhead fails; focused
executor tests plus the Rust quality gate; plan-compliance review of the named
executor only; and one focused commit. Each item is done only when its stable IDs,
deterministic counts, failure behavior, and overhead are recorded.

- [ ] Item 7a — Instrument TKVM as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism and boundaries: attribute tokenizer program/opcode/PC, scanner, token, scratch, and branch cost without changing token semantics.
  - Expected files: `runtime_model_core.rs`, `execution_model/tokenizer_bridge.rs`, and TKVM tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Items 4-5; use the common 7-series child-worktree, control/counters, coverage, and overhead contract.
  - Correctness/parity/failure validation: common 7-series exact-output and malformed-program cases.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused TKVM tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks TKVM-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust TKVM`.
  - Definition of done: TKVM attribution and evidence are complete.

- [ ] Item 7b — Instrument PRVM v2 as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism and boundaries: attribute parser dispatch, checkpoints, token/lexeme work, resumes, and high-water without changing parse semantics.
  - Expected files: `execution_model/parser_vm_v2.rs`, `vm_opasm.rs`, and PRVM tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7a; common 7-series contract.
  - Correctness/parity/failure validation: parse diagnostics and rollback/checkpoint failures under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused PRVM tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks PRVM-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust PRVM`.
  - Definition of done: PRVM attribution and evidence are complete.

- [ ] Item 7c — Instrument EXVM as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F5, F11.
  - Rationale/mechanism and boundaries: attribute expression-parser bytecode, helpers, allocations, and emitted ExprVM program shape without changing frontend contracts.
  - Expected files: `exvm_v2_runtime.rs`, `vm_opcore.rs`, and EXVM tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7b; common 7-series contract.
  - Correctness/parity/failure validation: step/stack/retired-delegate/missing-END failures under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused EXVM tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks EXVM-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust EXVM`.
  - Definition of done: EXVM attribution and evidence are complete.

- [ ] Item 7d — Instrument ExprVM as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F5, F11.
  - Rationale/mechanism and boundaries: separate compile, bind, evaluation, symbol, current-PC, stack, and operator costs while preserving portable expression semantics.
  - Expected files: `crates/opforge-core/src/expr_vm.rs`, `vm_opcore.rs`, and expression tests.
  - Steps: wire shared hooks and deterministic lifecycle/counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7c; common 7-series contract.
  - Correctness/parity/failure validation: undefined/overflow/current-PC/failure diagnostics under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused ExprVM tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks ExprVM-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust ExprVM`.
  - Definition of done: ExprVM attribution and evidence are complete.

- [ ] Item 7e — Instrument MSEL/TABL selection as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism and boundaries: count selector/table identity, candidate scans, predicates, choices, and high-water with package-owned selection unchanged.
  - Expected files: `selector_vm.rs`, `runtime_model_core.rs`, and selector tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7d; common 7-series contract.
  - Correctness/parity/failure validation: no-match/ambiguity/malformed selection failures under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused selector tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks selector-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust selector VM`.
  - Definition of done: selection attribution is complete.

- [ ] Item 7f — Instrument SEMV semantic bytecode as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism and boundaries: attribute fixed semantic opcode/operand/helper work without moving family semantics into the interpreter.
  - Expected files: `bytecode.rs`, `runtime_model_core.rs`, and semantic VM tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7e; common 7-series contract.
  - Correctness/parity/failure validation: malformed/truncated/missing-operand behavior under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused SEMV tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks SEMV-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust semantic VM`.
  - Definition of done: SEMV attribution is complete.

- [ ] Item 7g — Instrument the encoding VM as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism and boundaries: attribute selection-to-encoding opcodes, output sizes, and helpers while preserving package-owned bytes.
  - Expected files: `encoding_vm.rs`, its runtime owner, and encoding tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7f; common 7-series contract.
  - Correctness/parity/failure validation: overflow/malformed/output failures under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused encoding VM tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks encoding-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust encoding VM`.
  - Definition of done: encoding attribution is complete.

- [ ] Item 7h — Instrument structured encoding as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism and boundaries: attribute record decode, candidates, helpers, and result high-water without changing structured contracts.
  - Expected files: `structured_encoding_vm.rs`, its runtime owner, and tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7g; common 7-series contract.
  - Correctness/parity/failure validation: missing-record/malformed/fuzz failures under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused structured-encoding tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks structured-encoding-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust structured encoding`.
  - Definition of done: structured-encoding attribution is complete.

- [ ] Item 7i — Instrument OPRD operand-record execution as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism and boundaries: attribute record decode, candidates, field access, and result sizes without target semantics in generic code.
  - Expected files: `operand_record_vm.rs`, its runtime owner, and OPRD tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7h; common 7-series contract.
  - Correctness/parity/failure validation: missing/invalid record and composite failures under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused OPRD tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks OPRD-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust operand VM`.
  - Definition of done: OPRD attribution is complete.

- [ ] Item 7j — Instrument STVM state execution as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F9, F11.
  - Rationale/mechanism and boundaries: attribute decode, reset, profile/key lookup, directive scans, and invalidation while package data owns state semantics.
  - Expected files: `state_vm.rs`, its runtime owner, and STVM tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7i; common 7-series contract.
  - Correctness/parity/failure validation: invalid transition/profile/directive failures under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused STVM tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks STVM-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust state VM`.
  - Definition of done: STVM attribution is complete.

- [ ] Item 7k — Instrument BRVM branch execution as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism and boundaries: attribute form choices, branch outcomes, stability helpers, and results without family semantics in generic code.
  - Expected files: `branch_vm.rs`, its runtime owner, and BRVM tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7j; common 7-series contract.
  - Correctness/parity/failure validation: range/stability/malformed failures under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused BRVM tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks BRVM-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust branch VM`.
  - Definition of done: BRVM attribution is complete.

- [ ] Item 7l — Instrument FXVM fixup execution as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism and boundaries: attribute version, opcode, helper, target, and result/error work without changing fixup ownership.
  - Expected files: `fixup_vm.rs`, its runtime owner, and FXVM tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7k; common 7-series contract.
  - Correctness/parity/failure validation: missing-target/version/malformed/range failures under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused FXVM tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks FXVM-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust fixup VM`.
  - Definition of done: FXVM attribution is complete.

- [ ] Item 7m — Instrument VALU execution as one executor slice
  - Source requirement or finding IDs: SR-RF, SR-ID, SR-MEAS, F11.
  - Rationale/mechanism and boundaries: attribute value opcodes, stack, helpers, and result sizes without changing portable value semantics.
  - Expected files: `value_vm.rs`, its runtime owner, and VALU tests.
  - Steps: wire shared hooks and deterministic counter oracles for this executor only.
  - Dependencies/worktree and metrics: Item 7l; common 7-series contract.
  - Correctness/parity/failure validation: malformed/missing-input/overflow failures under the common contract.
  - Rollback/kill/reference strategy: common independent histogram/mode switch.
  - Full quality gates: focused VALU tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks VALU-only scope and evidence.
  - Commit outcome: `feat(perf): instrument Rust value VM`.
  - Definition of done: VALU attribution is complete.

- [ ] Item 7n — Enforce complete executor-to-profile coverage
  - Source requirement or finding IDs: SR-RF, SR-ID, F11.
  - Rationale/mechanism: prevent a new executor or VM-like service from silently escaping the profiler after the one-executor slices.
  - Architectural boundaries: registry validation only; CALS remains classified as compact alias data/service, not an opcode VM.
  - Expected files: central executor/profile registry and inventory tests/documentation.
  - Steps: enumerate all RuntimeModelCore/execution-model entrypoints; require a stable mapping or reviewed non-executor exemption; compare registry to baseline inventory.
  - Dependencies/worktree: Item 7m; sequential integration-worktree documentation/test slice.
  - Before/after metrics: coverage goes to 100% mapped or explicit exempt; negligible runtime impact.
  - Correctness/parity/failure validation: deliberately unmapped test executor fails; normal suite remains exact.
  - Rollback/kill/reference strategy: no runtime fallback; update registry and schema version deliberately for new executors.
  - Effort/risk and stop/go: S/Low; Phase 1 report blocks unless coverage is complete.
  - Full quality gates: focused registry test and Rust quality gate; workflow gate if inventory documentation changes.
  - Plan-compliance review evidence: reviewer checks inventory completeness and exemptions.
  - Commit outcome: `test(perf): enforce complete VM profile coverage`.
  - Definition of done: registry tests mechanically cover every current executor.

- [ ] Item 8 — Attribute helpers, allocations, clones, lookups, caches, and expression lifecycle
  - Source requirement or finding IDs: SR-RF, SR-MEAS, F5, F8, F9, F11.
  - Rationale/mechanism: separate interpreter dispatch from work done behind helpers and service boundaries.
  - Architectural boundaries: observe existing ownership; do not replace allocators, caches, or symbol semantics in this item.
  - Expected files: `opforge-vm`, `opforge-core`, `opforge-asm` runtime/line/symbol/cache paths and profile tests.
  - Steps: classify helper/service crossings; count alloc/clone sites, symbol probes/string compares, expression parse/compile/bind/evaluate, cache hits/misses/invalidation, callbacks, and temporary high-water.
  - Dependencies/worktree: Item 7n; child slice.
  - Before/after metrics: attribution completeness and incremental counters-mode overhead.
  - Correctness/parity/failure validation: focused counter-oracle tests and full artifact/diagnostic parity.
  - Rollback/kill/reference strategy: aggregate expensive cardinalities by stable helper class; retain a targeted trace filter for deeper diagnosis.
  - Effort/risk and stop/go: L/Medium; stop adding granularity when it no longer changes ranking and record the blind spot.
  - Full quality gates: focused instrumentation tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer verifies requested lifecycle and lookup categories.
  - Commit outcome: `feat(perf): attribute Rust VM helper and allocation costs`.
  - Definition of done: reports distinguish dispatch/check from semantic/helper/allocation/lookup/service costs.

- [ ] Item 9 — Run the Rust corpus and publish the Phase 1 hotspot decision report
  - Source requirement or finding IDs: SR-RF, SR-MEAS, SR-ACC, F5, F9-F11.
  - Rationale/mechanism: convert raw profiles into ranked, coverage-qualified implementation decisions.
  - Architectural boundaries: no optimization code; historical numbers remain separately labelled.
  - Expected files: `documentation/performance/results/**phase-1-rust-hotspots**`, raw profile manifests/digests, plan status.
  - Steps: run B01-B10 in control/counters/sampled and targeted trace modes; perform `xctrace` samples; rank VMs/programs/opcodes/PCs/sequences/helpers; quantify candidate coverage/setup/code-size/memory risk; set evidence thresholds.
  - Dependencies/worktree: Items 2, 5, 7n, 8; integration worktree.
  - Before/after metrics: this item establishes the authoritative Rust baseline—median/range/p95 and all mechanism distributions.
  - Correctness/parity/failure validation: identical artifacts/diagnostics/exits across modes; raw-integrity and rerun reproducibility checks.
  - Rollback/kill/reference strategy: discard high-perturbation series; rerun with counters/host sampling and document exclusions.
  - Effort/risk and stop/go: M/Medium; Phase 2 is blocked until report names candidates with material real-workload coverage and bounded risk.
  - Full quality gates: result-schema verifier, corpus parity suite, Rust quality gate, workflow gate.
  - Plan-compliance review evidence: reviewer confirms all nine required hotspot questions are answered or explicitly unresolved.
  - Commit outcome: `docs(perf): publish ranked Rust VM hotspot decisions`.
  - Definition of done: positive, deferred, and rejected candidates have cited raw evidence and thresholds.

### Phase 2 — initial Rust VM optimization experiments and decisions

- [ ] Item 10 — Remove the highest-ranked repeated Rust check/decode/allocation/clone/lookup cost
  - Source requirement or finding IDs: SR-RF, SR-ACC, SR-PAR, F11 plus the selected Phase 1 candidate ID.
  - Rationale/mechanism: take the smallest portable measured win before changing dispatch or adding specialization.
  - Architectural boundaries: candidate must remain in its current generic owner; no CPU leakage; generic behavior remains available.
  - Expected files: only the selected VM/helper path, focused tests, profiler counters, one decision record.
  - Steps: freeze before profile; implement one elimination/cache/hoist; add generic-only/enabled/dual compare; measure isolated and B10; classify transfer.
  - Dependencies/worktree: Item 9 positive candidate; dedicated child worktree.
  - Before/after metrics: selected event count, component time, end-to-end median/p95, setup/code/memory, coverage/fallback/mismatch.
  - Correctness/parity/failure validation: exact output/diagnostic/state/fixup/error parity in all modes and full corpus.
  - Rollback/kill/reference strategy: runtime/feature kill switch; revert if B10 regresses beyond Phase 1 noise or maintenance cost exceeds benefit; remove accepted experiment-only comparison scaffolding in Item 28a.
  - Effort/risk and stop/go: M/Medium; stop after one mechanism and decide before another candidate.
  - Full quality gates: focused differential tests, corpus suite, Rust quality gate.
  - Plan-compliance review evidence: reviewer checks positive evidence, narrow diff, and decision record.
  - Commit outcome: `perf(vm): eliminate measured <candidate> overhead` or focused revert/decision commit.
  - Definition of done: before/after evidence supports acceptance or clean rejection and transfer class is recorded.

- [ ] Item 11 — Trial measured prepared metadata, route cache, or predecoded micro-ops
  - Source requirement or finding IDs: SR-RF, SR-ACC, SR-ARCH, F5/F9/F11 as selected by Item 9.
  - Rationale/mechanism: move immutable validation/version/route/decode work outside a demonstrably hot loop.
  - Architectural boundaries: identity includes package/program contract and invalidation inputs; portable bytecode remains authoritative.
  - Expected files: selected Rust VM and package metadata/cache owner, tests, decision record.
  - Steps: define immutable key/lifetime; prepare once; count prepare/hit/miss/invalidate; implement four modes and bounded comparison; measure cold/startup and warm B10 integrated workload.
  - Dependencies/worktree: Item 9 and, if related, Item 10; child worktree.
  - Before/after metrics: preparation/setup, hit rate, avoided decodes/checks, memory/code size, isolated and B10 time.
  - Correctness/parity/failure validation: stale identity/version/pipeline/scope/error cases plus corpus parity.
  - Rollback/kill/reference strategy: bypass on validation miss and retain generic path; remove cache if immaterial; remove accepted experiment-only comparison scaffolding in Item 28b.
  - Effort/risk and stop/go: M/Medium; do not combine unrelated VMs in one commit.
  - Full quality gates: selected-VM differential tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer validates complete key/invalidation and evidence.
  - Commit outcome: `perf(vm): prepare measured <program> execution metadata`.
  - Definition of done: no stale reuse, measured benefit/coverage, and transfer class recorded.

- [ ] Item 12 — Trial one measured portable execution representation
  - Source requirement or finding IDs: SR-RF, SR-ACC, SR-ARCH, F10-F11 and Phase 1 candidate.
  - Rationale/mechanism: reduce dispatch/decode/check frequency only where PC/sequence evidence predicts material coverage, while deciding the smallest portability-preserving representation.
  - Architectural boundaries: canonical portable bytecode and package semantics remain versioned and authoritative; derived execution IR contains no target semantics; no handcrafted CPU behavior.
  - Expected files: one Rust VM, one validated portable execution-IR/predecode/threading/superinstruction experiment, tests, and decision record.
  - Steps: compare decoded micro-ops, generated superinstructions, handler threading, and a two-level bytecode-to-portable-IR-to-target-backend design on coverage/setup/code/RAM/relocation/cache criteria; implement only the smallest positive Rust-side representation with four modes; measure B10; record whether a later target cross-assembler is justified.
  - Dependencies/worktree: Item 9; Item 11 if predecode is prerequisite; child worktree.
  - Before/after metrics: dispatches retired, sequence coverage, component/end-to-end time, setup, code/memory, fallback/mismatch.
  - Correctness/parity/failure validation: differential random/fixture programs, malformed bytecode, exact diagnostics/state/fixups and corpus parity.
  - Rollback/kill/reference strategy: signature/version gate and kill switch; reject if synthetic-only or B10 neutral/regressive; remove accepted experiment-only comparison scaffolding in Item 28c.
  - Effort/risk and stop/go: M/High; one VM/sequence per item; add another plan item only after decision.
  - Full quality gates: focused VM/differential/corpus tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer verifies measured sequence coverage, canonical-bytecode authority, invalidation, and semantic ownership.
  - Commit outcome: `perf(vm): trial measured <vm> execution representation` or revert/decision commit.
  - Definition of done: positive/rejected representation, resource tradeoffs, and native/backend transfer class are explicit.

- [ ] Item 13 — Trial one exact-program/signature-bound generated or AOT accelerator if still justified
  - Source requirement or finding IDs: SR-RF, SR-ACC, SR-PAR, SR-ARCH, F11 and positive Phase 1 evidence.
  - Rationale/mechanism: accelerate an exceptionally hot stable portable program only when generic improvements leave material cost, including testing bytecode as an intermediate input to generated code.
  - Architectural boundaries: eligibility uses validated package capability/program signature; never paths/text/fixture/generation/output; canonical bytecode and generic interpreter remain oracle/fallback; target lowering may not own package semantics.
  - Expected files: correct Rust VM ownership layer, validated portable execution IR and signature catalog/generator if warranted, one host backend experiment, tests, decision record.
  - Steps: document contract and coverage; lower bytecode to a deterministic validated portable execution IR; generate or implement the smallest exact host handler; add four modes/counters; dual-run outputs, state, fixups, diagnostics and failures; measure generation/setup/code/cache/RAM/B10 integrated workload; record target-backend relocation and verification requirements.
  - Dependencies/worktree: Items 9-12; requires positive threshold and explanation why portable superinstruction is insufficient; child worktree.
  - Before/after metrics: eligibility/hit/fallback/mismatch, covered execution share, component/B10 speed, startup/code/memory.
  - Correctness/parity/failure validation: signature near-miss, corrupt program, all semantic and failure outputs, and corpus differential proof; terminal self-host proof remains a separate final gate.
  - Rollback/kill/reference strategy: default-disabled rollout, kill switch, generic fallback; removal criterion stated in the decision record and experiment-only scaffolding removed in Item 28d.
  - Effort/risk and stop/go: L/High; omit this item entirely when Item 9 evidence is below threshold.
  - Full quality gates: focused differential/fuzz/property tests as applicable and Rust quality gate.
  - Plan-compliance review evidence: reviewer checks all accelerator-policy fields and absence of test identity.
  - Commit outcome: `perf(vm): add validated <program> Rust accelerator` or documented no-go commit.
  - Definition of done: accepted accelerator has complete decision record and transfer class, or no-go evidence closes the candidate.

### Phase 3 — native buffered input and scan consolidation

- [ ] Item 14 — Introduce a shared bounded native buffered reader
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F1.
  - Rationale/mechanism: make DOS reads scale as `ceil(bytes/buffer_size)` plus bounded refill overhead rather than bytes.
  - Architectural boundaries: preserve CR/LF, EOF, error, line/column, range-end, and diagnostic semantics; 68020 baseline.
  - Expected files: `opforge-cli/dos.asm`, `source_reader.asm`, new generic reader module, harness/tests.
  - Steps: add DOS Seek and 8-16 KiB buffered state; implement `readByte`, `readLine`, bounded range/refill; migrate normal source path first; retain reference mode.
  - Dependencies/worktree: Items 1, 2, 6; child native slice; does not wait for Phase 2.
  - Before/after metrics: opens/reads/bytes/seeks, logical lines, buffer high-water, B01/B02/B08/B10 native wall time.
  - Correctness/parity/failure validation: EOF at boundaries, CR/LF splits, short/error reads, range ends, exact diagnostics/artifacts, focused Level D.
  - Rollback/kill/reference strategy: retain the build/runtime reference reader through Item 28 qualification; remove it only in Item 29h.
  - Effort/risk and stop/go: L/High; go only when operation count is block-scaled and parity is exact.
  - Full quality gates: native format, staged native gate, Rust quality gate, focused FS-UAE confirmation.
  - Plan-compliance review evidence: reviewer verifies no unbounded buffer or per-event I/O.
  - Commit outcome: `perf(native): buffer production source input`.
  - Definition of done: normal source reads are block-scaled with failure/line parity.

- [ ] Item 15 — Consolidate root existence, output, CPU, and processing scans
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F1.
  - Rationale/mechanism: remove redundant root existence/output/CPU opens/scans while keeping one buffered processing stream.
  - Architectural boundaries: directive precedence and root selection remain unchanged; module scanning is separate Item 15a.
  - Expected files: `run.asm`, `source_reader.asm`, buffered reader, tests.
  - Steps: make processing open authoritative; combine/cache `.output`/`.cpu` preflight metadata in one buffered pass or eliminate it where processing can own discovery; remove existence-only open; retain root reference mode through Item 28.
  - Dependencies/worktree: Item 14; child slice.
  - Before/after metrics: root opens/reads/full scans, B02/B10 wall time and buffer memory.
  - Correctness/parity/failure validation: missing root, directive order/absence/duplicates, mixed line endings, exact diagnostics and focused Level D.
  - Rollback/kill/reference strategy: root legacy scan switch; removal Item 29h.
  - Effort/risk and stop/go: L/High; stop if scan consolidation changes diagnostics—retain one buffered scan per semantic phase instead.
  - Full quality gates: native format/staged gate, Rust quality gate, focused then current complete FS-UAE wrapper.
  - Plan-compliance review evidence: reviewer checks root-only scope and operation formula.
  - Commit outcome: `perf(native): consolidate root source scans`.
  - Definition of done: no existence-only root open and root scans/reads meet the recorded block-scaled formula with parity.

- [ ] Item 15a — Migrate module indexing and bounded ranges to buffered seekable input
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F1.
  - Rationale/mechanism: make candidate indexing block-scaled and reach known module offsets by seek rather than byte consumption.
  - Architectural boundaries: once-per-invocation module index, declaration/range semantics, ambiguity and diagnostics remain unchanged.
  - Expected files: `module_discovery.asm`, module-range source path, DOS Seek/buffered reader, tests.
  - Steps: migrate candidate scanner; seek to retained starts; enforce bounded ends through reader state; count files/reads/seeks/ranges; retain module reference mode.
  - Dependencies/worktree: Item 15; next sequential child slice.
  - Before/after metrics: module opens/reads/seeks/scanned bytes, rows indexed, B08/B10.
  - Correctness/parity/failure validation: missing/duplicate/ambiguous modules, range and CR/LF edges, short/error reads, complete native parity.
  - Rollback/kill/reference strategy: module byte-reader/range-skip switch; removal Item 29h.
  - Effort/risk and stop/go: L/High; stop on any index/range/diagnostic mismatch.
  - Full quality gates: native format/staged gate, Rust quality gate, focused and complete module FS-UAE.
  - Plan-compliance review evidence: reviewer checks once-only indexing and module-only migration.
  - Commit outcome: `perf(native): buffer and seek module source ranges`.
  - Definition of done: module reads block-scale, known offsets seek, bounded ranges and diagnostics match exactly.

### Phase 4 — native session lifecycle and bulk memory operations

- [ ] Item 16 — Establish lifecycle invariants and stop clearing unused source capacity
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F2.
  - Rationale/mechanism: make scalar counts authoritative and eliminate the source-record/text-pool share of the 41,221,928-byte clear first.
  - Architectural boundaries: every valid source record is fully initialized; capacities, source diagnostics, and errors are unchanged.
  - Expected files: `opasm/opasm_engine.asm` session/source insertion paths, approved debug assertions/tests.
  - Steps: audit source-region zero reads; reset source scalars/used ranges; fully initialize insertion; poison unused source capacity in debug; retain a source-only legacy-clear switch.
  - Dependencies/worktree: Items 2 and 6a; high-risk child worktree.
  - Before/after metrics: source bytes/ranges cleared, B01/B10, live/peak source rows/text, debug poison cost.
  - Correctness/parity/failure validation: reuse, capacity, early error, stale-read poison, exact source diagnostics/artifacts and focused Level D.
  - Rollback/kill/reference strategy: source-only legacy clear and invariant comparison; named removal is Item 29a.
  - Effort/risk and stop/go: M/High; stop on any poison read or unexplained output difference.
  - Full quality gates: native format/staged gate, Rust quality gate, focused source/session FS-UAE.
  - Plan-compliance review evidence: reviewer checks the source zero-dependency audit and one-region scope.
  - Commit outcome: `perf(native): reset source storage by live state`.
  - Definition of done: unused source capacity is not cleared or read and source parity is exact.

- [ ] Item 16a — Stop clearing unused statement and layout capacity
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F2, F7.
  - Rationale/mechanism: remove the 30.8 MB statement share and the separate 100,000-row layout-map reset from fixed startup.
  - Architectural boundaries: statement/layout validity and pass behavior remain exact; no record compaction in this item.
  - Expected files: statement creation/access and `opasm_layout.asm`, approved debug assertions/tests.
  - Steps: audit zero dependencies; reset counts; fully initialize statement rows; use touched/generation state for layout mapping; poison unused rows; retain statement-only reference reset.
  - Dependencies/worktree: Item 16; next sequential child slice.
  - Before/after metrics: statement/layout bytes cleared, touches/generation wraps, B01/B03/B10, live/peak rows.
  - Correctness/parity/failure validation: session reuse, 100,000 boundary, layout retries, early failure, poison and exact Level D parity.
  - Rollback/kill/reference strategy: statement/layout legacy reset switch; named removal is Item 29a.
  - Effort/risk and stop/go: L/High; stop on generation-wrap ambiguity or stale layout state.
  - Full quality gates: native format/staged gate, Rust quality gate, focused capacity/layout FS-UAE.
  - Plan-compliance review evidence: reviewer checks only statement/layout lifecycle changed.
  - Commit outcome: `perf(native): reset statement layout by live state`.
  - Definition of done: unused statement/layout capacity is not cleared or read and parity is exact.

- [ ] Item 16b — Stop clearing unused label and image capacity
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F2, F4, F8.
  - Rationale/mechanism: finish live-state session reset for label tables and image/presence buffers.
  - Architectural boundaries: label hash semantics, sparse image presence, capacities, and overlap errors remain exact.
  - Expected files: label/image lifecycle in `opasm_engine.asm`, approved debug assertions/tests.
  - Steps: audit zero dependencies; reset label/hash scalar state and touched image ranges; fully initialize valid rows; poison unused entries; retain label/image reference resets.
  - Dependencies/worktree: Item 16a; next sequential child slice.
  - Before/after metrics: label/image bytes cleared, touched ranges, B01/B04/B09/B10, live/peak labels/images.
  - Correctness/parity/failure validation: collisions, sparse images, overlaps, capacity/early error/reuse, poison and exact Level D parity.
  - Rollback/kill/reference strategy: label/image legacy reset switches; named removal is Item 29a.
  - Effort/risk and stop/go: L/High; stop if sparse-image semantics depend on untracked zero state.
  - Full quality gates: native format/staged gate, Rust quality gate, focused symbol/image FS-UAE.
  - Plan-compliance review evidence: reviewer checks label/image-only lifecycle scope.
  - Commit outcome: `perf(native): reset label and image storage by live state`.
  - Definition of done: the unconditional 41,221,928-byte clear is gone behind a temporary reference mode and all regions have poison proof.

- [ ] Item 17 — Add measured generic 68020 bulk copy and clear primitives
  - Source requirement or finding IDs: SR-NATIVE, SR-ARCH, SR-PAR, F3.
  - Rationale/mechanism: speed only unavoidable transfers after copy/clear sites and sizes are counted.
  - Architectural boundaries: generic 68020 interface, alignment/tail and overlap contract explicit; no AMMX/68080 path.
  - Expected files: `opforge-cli/copy.asm`, selected large call sites, primitive tests.
  - Steps: inventory/size call sites; implement alignment-aware longword/unrolled copy/clear with byte tails; route only measured large sites; benchmark versus byte reference.
  - Dependencies/worktree: Items 6 and 16b; child slice.
  - Before/after metrics: bytes/calls by site and size/alignment, B01/B09/B10, code size.
  - Correctness/parity/failure validation: zero/tail/alignment/overlap-boundary cases and exact artifacts.
  - Rollback/kill/reference strategy: byte reference primitive and per-call-site switch until Item 28; removal or no-go cleanup is Item 29g.
  - Effort/risk and stop/go: M/Medium; retain only sites with measured benefit and no B10/code-cache regression.
  - Full quality gates: native format/staged gate, Rust quality gate, focused memory/artifact FS-UAE.
  - Plan-compliance review evidence: reviewer checks one primitive contract and explicit call-site set.
  - Commit outcome: `perf(native): add measured 68020 bulk memory primitives`.
  - Definition of done: selected unavoidable copies/clears use proven primitives with exact behavior and measured value.

- [ ] Item 17a — Validate and execute the embedded package in place
  - Source requirement or finding IDs: SR-NATIVE, SR-ARCH, SR-PAR, F3.
  - Rationale/mechanism: eliminate the full 368,278-byte immutable embedded-package copy rather than merely accelerate it.
  - Architectural boundaries: package validator/service uses one explicit active base; external packages retain mutable bulk-read storage; lifetime/alignment/version checks exact.
  - Expected files: `package_pipeline.asm`, package service/control block/base accessors, package tests.
  - Steps: audit all package-base writers/readers; point embedded mode at immutable `incbin`; validate/activate from that base; keep external mode separate; count copies and active-base transitions.
  - Dependencies/worktree: Item 17; next sequential child slice.
  - Before/after metrics: embedded copy bytes/calls become zero, startup B01/B10, active storage/headroom/code size.
  - Correctness/parity/failure validation: embedded/external/corrupt/oversize/version cases, exact package identity and artifacts.
  - Rollback/kill/reference strategy: copied-package mode through Item 28; remove it or record the measured no-go exception in Item 29g.
  - Effort/risk and stop/go: M/Medium; stop if immutable lifetime/alignment or validator contract cannot be proven.
  - Full quality gates: native format/staged gate, Rust quality gate, focused package FS-UAE and complete parity.
  - Plan-compliance review evidence: reviewer checks active-base lifetime and external-path isolation.
  - Commit outcome: `perf(native): execute embedded package in place`.
  - Definition of done: embedded copy count is zero or a measured no-go decision closes the item.

### Phase 5 — verify and finish the checkpoint's layout/final-emission split

- [ ] Item 18 — Audit the existing native layout and final-emission modes
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F4.
  - Rationale/mechanism: checkpoint `68cc693c` already established layout-only retries plus one final pass; verify that boundary and add missing observability rather than reimplementing it.
  - Architectural boundaries: current callbacks and bytes remain unchanged in this slice; retry limit and diagnostics exact; the checkpoint implementation is the reference.
  - Expected files: `opasm_engine.asm`, layout/runtime mode constants/context, tests.
  - Steps: audit `OpasmEngineFinalEmission` and output-disable propagation through pass/callback context; count layout/final modes; record stable selected size/form/fixup-shape metadata; assert the checkpoint mode behaves identically with instrumentation off/on.
  - Dependencies/worktree: Items 6a and 16b; child worktree.
  - Before/after metrics: mode counts, record bytes, B05/B09/B10 non-regression; no speed claim.
  - Correctness/parity/failure validation: all pass/error/output variants and focused Level D.
  - Rollback/kill/reference strategy: retain checkpoint mode unchanged; revert only new assertions/counters if they perturb behavior; cleanup is Item 29b.
  - Effort/risk and stop/go: M/High; stop if mode propagation changes any callback/result.
  - Full quality gates: native format/staged gate, Rust quality gate, focused stability/output FS-UAE.
  - Plan-compliance review evidence: reviewer checks this is mode/metadata only.
  - Commit outcome: `test(native-perf): verify layout and emission modes`.
  - Definition of done: every pass callback has an explicit tested mode and stable metadata is available.

- [ ] Item 18a — Verify zero convergence materialization and remove residual presence clears
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F4.
  - Rationale/mechanism: prove the checkpoint already makes convergence image writes zero, then remove the still-static 1 MiB image-presence clear from layout-only rounds if counters show it remains material.
  - Architectural boundaries: addresses, selected forms, sizes, branch stability, sections, and diagnostics remain exact.
  - Expected files: opasm image/output callbacks, encoder bridge, image-presence reset, tests.
  - Steps: count and assert zero layout-round image appends; measure per-round 1 MiB presence clears; if above the Item 0f threshold, bypass only that clear in layout mode while retaining authoritative size/fixup-shape results; dual-trace checkpoint/new rounds.
  - Dependencies/worktree: Item 18; next sequential child slice.
  - Before/after metrics: convergence image bytes and presence clears become zero; callback/encode counts and B05/B09/B10.
  - Correctness/parity/failure validation: stable/unstable branches, regions/overlap, retry exhaustion, no-output/all-output and focused Level D.
  - Rollback/kill/reference strategy: checkpoint layout mode and trace compare; if the clear is not material, close with a measured no-go decision; cleanup Item 29b.
  - Effort/risk and stop/go: L/High; stop on any layout trace or diagnostic mismatch.
  - Full quality gates: native format/staged gate, Rust quality gate, focused then complete native parity.
  - Plan-compliance review evidence: reviewer checks only convergence materialization is suppressed.
  - Commit outcome: `perf(native): remove measured layout presence clears` or measured no-go decision commit.
  - Definition of done: operation counters prove zero convergence image bytes with exact layout parity.

- [ ] Item 18b — Execute exactly one asserted final emission
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, SR-TERM, F4.
  - Rationale/mechanism: verify the checkpoint's scheduled final pass materializes images/fixups/artifacts exactly once and prove encoded sizes match stable layout.
  - Architectural boundaries: all Hunk/S-record/BIN/PRG/listing/map/metadata/fixup behavior and errors remain exact.
  - Expected files: final pass orchestration, image/fixup/artifact callbacks, assertions/tests.
  - Steps: count the existing scheduled final mode; assert every encoded size/form/fixup shape; compare checkpoint/instrumented final traces and artifacts; change orchestration only if evidence disproves exactly-one behavior.
  - Dependencies/worktree: Item 18a; next sequential child slice.
  - Before/after metrics: final emission count exactly one, assertion failures zero, B05/B09/B10 median/p95.
  - Correctness/parity/failure validation: all-output/no-listing/listing, unresolved/overflow/retry errors, bounded B05/B09/B10 comparisons, and complete focused Level D validation; terminal self-host remains a separate final gate.
  - Rollback/kill/reference strategy: checkpoint pass-two orchestration retained as reference through Item 28 qualification; removal Item 29b.
  - Effort/risk and stop/go: L/High; any size/fixup assertion blocks and must be fixed, never bypassed.
  - Full quality gates: native format/staged gate, Rust quality gate, targeted and complete native parity wrapper.
  - Plan-compliance review evidence: reviewer verifies exactly-one emission and assertion coverage.
  - Commit outcome: `test(native-perf): prove one final emission` or focused correction commit if counters disprove it.
  - Definition of done: counters/assertions and complete parity prove one final materialization.

### Phase 6 — prepared immutable execution state

- [ ] Item 19 — Prepare native directive routes
  - Source requirement or finding IDs: SR-NATIVE, SR-ARCH, SR-PAR, F6 and prior Rust prepared-route evidence.
  - Rationale/mechanism: replace repeated sequential directive classification with one prepared route per statement.
  - Architectural boundaries: package/directive semantics and diagnostic locations remain authoritative; no control-flow map in this item.
  - Expected files: `opasm_directive_router.asm`, statement route metadata, opasm pass consumers, tests.
  - Steps: profile classifications; define source/pipeline-sensitive route identity; build once; consume in passes; count hit/miss/invalidate; dual-compare decisions.
  - Dependencies/worktree: Items 6a, 9, and 18b; positive native frequency; child worktree.
  - Before/after metrics: classify calls, route bytes, hits/misses/invalidation, B07/B10.
  - Correctness/parity/failure validation: all directives, unknown/malformed/inactive cases, exact diagnostics and focused Level D.
  - Rollback/kill/reference strategy: legacy classifier and bounded compare; removal Item 29c.
  - Effort/risk and stop/go: M/High; reject if setup/memory outweighs B10 benefit.
  - Full quality gates: native format/staged gate, Rust quality gate, focused directive FS-UAE.
  - Plan-compliance review evidence: reviewer checks route identity and directive-only scope.
  - Commit outcome: `perf(native): prepare directive routes`.
  - Definition of done: each valid statement route is prepared once and exact decisions/diagnostics match.

- [ ] Item 19a — Prepare native control-flow boundary maps
  - Source requirement or finding IDs: SR-NATIVE, SR-ARCH, SR-PAR, F6.
  - Rationale/mechanism: precompute IF/MATCH/repetition matching boundaries rather than scan rows on every traversal.
  - Architectural boundaries: route semantics from Item 19, nesting, inactive behavior, and source diagnostics remain exact.
  - Expected files: `opasm_flow_navigation.asm`, boundary metadata/build step, pass consumers, tests.
  - Steps: record scan baseline; build bounded maps after parse; consume them in every pass; count lookup/fallback/invalidation; dual-compare targets.
  - Dependencies/worktree: Item 19; next sequential child slice.
  - Before/after metrics: rows scanned becomes setup-only, map bytes/high-water, lookups, B07/B10.
  - Correctness/parity/failure validation: nested/malformed IF/MATCH/repetition, inactive branches, exact diagnostics and complete native parity.
  - Rollback/kill/reference strategy: legacy forward scanner and bounded compare; removal Item 29c.
  - Effort/risk and stop/go: L/High; reject if map memory/setup exceeds measured reuse benefit.
  - Full quality gates: native format/staged gate, Rust quality gate, focused and complete control-flow FS-UAE.
  - Plan-compliance review evidence: reviewer checks boundary-map-only scope and diagnostics.
  - Commit outcome: `perf(native): prepare control-flow boundaries`.
  - Definition of done: matching scans occur once at preparation and all traversals/diagnostics match.

- [ ] Item 20 — Cache compiled native expression programs by validated identity
  - Source requirement or finding IDs: SR-RF, SR-NATIVE, SR-ARCH, SR-PAR, F5 and a positive Rust prepared-expression decision record.
  - Rationale/mechanism: compile immutable expression text once for reuse across layout/final rounds before attempting symbol binding.
  - Architectural boundaries: EXVM/ExprVM contract, text diagnostics, current-PC flag, scope/pipeline/state/version identity, and fallback remain exact.
  - Expected files: `opcore_expr_bridge.asm`, expression program storage/metadata, runtime context, tests.
  - Steps: quantify repeat rate; define complete prepared key; cache program offset/length and flags; count compile/hit/miss/invalidation/fallback; dual-evaluate cached versus compile-on-eval.
  - Dependencies/worktree: Item 9, a positive relevant Item 11 decision, and Item 18b; high-risk child worktree.
  - Before/after metrics: parse/compile/eval and reuse/invalidation, cache bytes/setup, B06/B10.
  - Correctness/parity/failure validation: current-PC, scope/state/pipeline switches, malformed/overflow/undefined expressions and exact diagnostics/fixups.
  - Rollback/kill/reference strategy: compile-on-eval reference and visible per-expression fallback; removal Item 29d.
  - Effort/risk and stop/go: L/High; no implementation without positive Rust record and material native repeat rate.
  - Full quality gates: native format/staged gate, Rust quality gate, focused expression differential/FS-UAE.
  - Plan-compliance review evidence: reviewer checks identity/invalidation and positive Rust decision.
  - Commit outcome: `perf(native): cache compiled expression programs`.
  - Definition of done: each valid immutable identity compiles at most once and every fallback/mismatch is counted.

- [ ] Item 20a — Bind prepared native expressions to stable symbols and dependencies
  - Source requirement or finding IDs: SR-RF, SR-NATIVE, SR-ARCH, SR-PAR, F5, F8 and the positive Rust binding decision record.
  - Rationale/mechanism: replace repeated name lookup with stable symbol IDs/dependency metadata after compile reuse is independently proven.
  - Architectural boundaries: forward/unstable references, current PC, local scope, state/pipeline/version and diagnostics remain exact; constant folding only if separately proven by Rust.
  - Expected files: expression metadata, symbol interface/IDs, ExprVM bridge/runtime context, tests.
  - Steps: define binding/dependency key; bind names once; mark unstable/current-PC cases; invalidate precisely; count bind/direct/fallback; dual-compare evaluation/state/fixups.
  - Dependencies/worktree: Item 20 and a positive Rust binding implementation decision; next sequential child slice.
  - Before/after metrics: binds/name lookups/direct IDs/dependency checks, metadata bytes, B04/B06/B10.
  - Correctness/parity/failure validation: forward/local/undefined/redefined symbols, pipeline/scope changes, exact values/diagnostics/fixups and complete parity.
  - Rollback/kill/reference strategy: name-lookup reference and per-expression fallback; removal Item 29d.
  - Effort/risk and stop/go: L/High; reject if invalidation or metadata cost erases B10 integrated-workload value.
  - Full quality gates: native format/staged gate, Rust quality gate, focused and complete expression FS-UAE.
  - Plan-compliance review evidence: reviewer checks positive Rust binding record and dependency matrix.
  - Commit outcome: `perf(native): bind prepared expressions to symbol ids`.
  - Definition of done: eligible expressions use validated stable IDs, unsafe cases visibly fall back, and parity is exact.

### Phase 7 — compact native statement and symbol representation

- [ ] Item 21 — Publish the native statement access profile and compact-record design
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, SR-ARCH, F2, F7.
  - Rationale/mechanism: choose a hot/cold layout from measured reads/writes/lifetimes before changing the 308-byte record.
  - Architectural boundaries: design only; 100,000 capacity, source/owner/operand diagnostics, and future portability remain fixed contracts.
  - Expected files: `documentation/performance/results/**statement-access-design**`, raw counter manifest, plan status.
  - Steps: profile every field by phase; quantify string duplication/lifetime; specify hot scalar record, cold storage, accessors, migration order, byte budgets, and failure semantics.
  - Dependencies/worktree: Items 16b, 19a, 20a; integration worktree.
  - Before/after metrics: current bytes/live statement, field access frequencies, duplicate bytes, projected layout/pool cost (clearly projected).
  - Correctness/parity/failure validation: traceability review against all statement consumers; no production behavior change.
  - Rollback/kill/reference strategy: no migration if projected savings are immaterial or lifetime cannot be proven.
  - Effort/risk and stop/go: M/Low; plan amendment/re-review if design changes contracts.
  - Full quality gates: result verifier, workflow gate, plan-compliance review.
  - Plan-compliance review evidence: reviewer verifies complete consumer/access inventory.
  - Commit outcome: `docs(perf): design compact native statement records`.
  - Definition of done: one reviewed fixed layout and byte/high-water acceptance budget exists.

- [ ] Item 21a — Add a dual-written compact hot statement record
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F7 and Item 21 design.
  - Rationale/mechanism: isolate frequently accessed scalar fields from cold text while retaining the legacy record as oracle.
  - Architectural boundaries: producers still populate both; no consumer cutover and no string migration in this item.
  - Expected files: statement record constants/allocation, creation path, cross-check tests.
  - Steps: allocate bounded hot rows; dual-write scalar/offset/flag fields; cross-check after creation/pass updates; count bytes and mismatches.
  - Dependencies/worktree: Item 21; high-risk child worktree.
  - Before/after metrics: hot bytes/live row, dual-write overhead, mismatch zero, B03/B10 non-regression.
  - Correctness/parity/failure validation: capacity/early error/reuse and exact artifacts/diagnostics.
  - Rollback/kill/reference strategy: legacy record remains sole reader; remove hot rows if design fails.
  - Effort/risk and stop/go: M/High; stop on any dual-write mismatch.
  - Full quality gates: native format/staged gate, Rust quality gate, focused capacity FS-UAE.
  - Plan-compliance review evidence: reviewer checks producer-only scope.
  - Commit outcome: `perf(native): dual-write compact hot statements`.
  - Definition of done: every hot field is dual-written and mechanically equal.

- [ ] Item 21b — Cut statement scalar consumers over to compact hot records
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F7 and Item 21 design.
  - Rationale/mechanism: realize cache/memory benefits after dual-write proof.
  - Architectural boundaries: text fields and legacy record remain for cold/reference reads; consumer semantics unchanged.
  - Expected files: opasm statement consumers/accessors and cross-check tests.
  - Steps: route all scalar readers through accessors/hot rows; bounded dual-read comparison; measure each phase; retain legacy cold fields.
  - Dependencies/worktree: Item 21a; next sequential child slice.
  - Before/after metrics: hot/legacy reads, bytes/live row, B03/B05/B07/B10 time.
  - Correctness/parity/failure validation: all passes, flow/layout/error/capacity paths, exact complete Level D.
  - Rollback/kill/reference strategy: accessor switch to legacy; removal of duplicate scalar arrays Item 29e.
  - Effort/risk and stop/go: L/High; stop if B10/access cost regresses beyond noise.
  - Full quality gates: native format/staged gate, Rust quality gate, focused and complete native parity.
  - Plan-compliance review evidence: reviewer checks consumer list matches Item 21 inventory.
  - Commit outcome: `perf(native): read compact hot statement records`.
  - Definition of done: all scalar consumers use hot accessors with zero comparison mismatches.

- [ ] Item 22 — Intern native statement owner/module identities
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F7.
  - Rationale/mechanism: replace repeated 64-byte owner strings with bounded stable IDs as the smallest cold-text migration.
  - Architectural boundaries: full scoped ownership, case and diagnostic spelling remain exact.
  - Expected files: owner/module pool and statement owner accessors, tests.
  - Steps: measure duplication; add bounded interning; dual-store/compare ID-to-text; cut owner consumers to accessor; count pool bytes/hits.
  - Dependencies/worktree: Item 21b; child worktree.
  - Before/after metrics: owner duplicates/unique bytes, pool overhead, bytes/live statement, B08/B10.
  - Correctness/parity/failure validation: nested modules/scopes, 107-byte names, pool exhaustion, exact diagnostics/artifacts.
  - Rollback/kill/reference strategy: inline owner reference field and accessor switch; removal Item 29e.
  - Effort/risk and stop/go: M/High; reject if pool/setup is not materially smaller.
  - Full quality gates: native format/staged gate, Rust quality gate, focused module/scope FS-UAE.
  - Plan-compliance review evidence: reviewer checks owner-only scope and bounded failure.
  - Commit outcome: `perf(native): intern statement owners`.
  - Definition of done: owner IDs round-trip exactly and measured storage falls materially.

- [ ] Item 22a — Move native operand text to validated source slices or a bounded pool
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F7.
  - Rationale/mechanism: replace repeated 64-byte inline operands using the already-proven long-operand slice concept.
  - Architectural boundaries: immutable source lifetime, normalized/inline-text cases, and diagnostic columns remain exact.
  - Expected files: operand storage/accessors, source pool/slices, expression/encoding consumers, tests.
  - Steps: classify slice-safe versus synthesized operands; dual-store offsets/length or pool IDs; cut consumers via accessor; count bytes/fallback.
  - Dependencies/worktree: Item 22; next sequential child slice.
  - Before/after metrics: slice/pool/inline counts and bytes, fallback, bytes/live statement, B03/B06/B10.
  - Correctness/parity/failure validation: long/synthesized/macro/normalized operands, lifetime, columns, exhaustion and exact artifacts.
  - Rollback/kill/reference strategy: inline operand accessor fallback; removal Item 29e.
  - Effort/risk and stop/go: L/High; retain hybrid form when synthesis prevents safe slicing.
  - Full quality gates: native format/staged gate, Rust quality gate, focused expression/operand FS-UAE.
  - Plan-compliance review evidence: reviewer checks lifetime and diagnostic-column proof.
  - Commit outcome: `perf(native): store operands as slices or pooled text`.
  - Definition of done: eligible operands avoid inline copies, fallbacks are explicit, and parity is exact.

- [ ] Item 22b — Share native statement-label and symbol-name storage
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F7-F8.
  - Rationale/mechanism: remove duplicate 108-byte statement label and 108-byte symbol name copies and support stable IDs.
  - Architectural boundaries: spelling, case, scope, longest-name, ambiguity, and diagnostics remain exact.
  - Expected files: statement label accessors, symbol name/pool/index storage, tests.
  - Steps: bind statement labels to stable symbol/name pool identities; dual-compare all reads; measure fragmentation/high-water; cut consumers.
  - Dependencies/worktree: Items 20a and 22a; next sequential child slice.
  - Before/after metrics: duplicate/unique label bytes, pool/index overhead, bytes/live statement/symbol, B04/B10.
  - Correctness/parity/failure validation: collisions, case/scope/final component, long names, exhaustion, exact diagnostics and complete parity.
  - Rollback/kill/reference strategy: inline label/name accessors and dual comparison; removal Item 29e.
  - Effort/risk and stop/go: L/High; keep a bounded hybrid if sharing regresses lookup or memory.
  - Full quality gates: native format/staged gate, Rust quality gate, focused and complete symbol FS-UAE.
  - Plan-compliance review evidence: reviewer checks one label/name ownership migration.
  - Commit outcome: `perf(native): share statement and symbol names`.
  - Definition of done: shared names are exact, bounded, materially smaller, and all consumers pass dual comparison.

### Phase 8 — native package-state and symbol-index optimization

- [ ] Item 23 — Decode selected native STVM metadata once and reset scalar state
  - Source requirement or finding IDs: SR-RF, SR-NATIVE, SR-ARCH, F9 and a positive Rust STVM prepared-state implementation decision.
  - Rationale/mechanism: reuse validated profile/key/default/override metadata and make reset scalar rather than reparse serialized records.
  - Architectural boundaries: package owns state semantics; selection/pipeline/version invalidation exact; no directive index in this item.
  - Expected files: `tkpkg_state_service.asm`, runtime context/package validation, tests.
  - Steps: cite positive Rust before/after record; profile native decode/reset; prepare selected immutable metadata once; reset active scalars; count prepare/reuse/invalidate/fallback; dual-compare state.
  - Dependencies/worktree: Items 6b and 9 plus a positive completed Rust implementation decision (not profile insight alone); child worktree.
  - Before/after metrics: decoded records/reset, prepared bytes, reuse/invalidation, B07/B10.
  - Correctness/parity/failure validation: profiles/defaults/overrides, pipeline/version changes, malformed records, exact state/diagnostics and focused Level D.
  - Rollback/kill/reference strategy: serialized reset fallback and dual state compare; removal Item 29f.
  - Effort/risk and stop/go: M/High; skip if no positive Rust implementation record or native repeat rate is immaterial.
  - Full quality gates: native format/staged gate, Rust quality gate, focused state FS-UAE.
  - Plan-compliance review evidence: reviewer verifies the positive Rust decision and reset-only scope.
  - Commit outcome: `perf(native): prepare STVM reset metadata`.
  - Definition of done: eligible state reset reuses one validated decode with zero comparison mismatches.

- [ ] Item 23a — Index native STVM directives and arguments
  - Source requirement or finding IDs: SR-RF, SR-NATIVE, SR-ARCH, F9 and a positive Rust STVM directive-index implementation decision.
  - Rationale/mechanism: replace repeated serialized directive/string scans with bounded prepared lookup after reset reuse is proven.
  - Architectural boundaries: package strings/arguments, case rules, transitions, and diagnostics remain authoritative.
  - Expected files: `tkpkg_state_service.asm`, prepared directive index/runtime context, tests.
  - Steps: cite positive Rust index result; record native scans/compares; build validated bounded index once; use it for apply; count hits/misses/fallback/invalidation; dual-compare transitions.
  - Dependencies/worktree: Item 23 and a positive Rust directive-index implementation decision; next sequential child slice.
  - Before/after metrics: directive records scanned/string compares, index bytes/setup, B07/B10.
  - Correctness/parity/failure validation: known/unknown/malformed directives/arguments, all transitions and pipeline changes, exact diagnostics and complete parity.
  - Rollback/kill/reference strategy: serialized directive scan fallback and dual compare; removal Item 29f.
  - Effort/risk and stop/go: M/High; skip if hit-adjusted benefit is immaterial or index memory dominates.
  - Full quality gates: native format/staged gate, Rust quality gate, focused and complete state FS-UAE.
  - Plan-compliance review evidence: reviewer checks positive Rust implementation and directive-only scope.
  - Commit outcome: `perf(native): index STVM directives`.
  - Definition of done: eligible directives use bounded indexed lookup with exact transitions/diagnostics.

- [ ] Item 24 — Improve native symbol indexing from measured probe distributions
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F8.
  - Rationale/mechanism: reduce chains and full string comparisons only if B04/B10 data identifies them as material.
  - Architectural boundaries: scope/final-component ambiguity and diagnostics unchanged; prepared direct symbol IDs preferred where available.
  - Expected files: `opasm_engine.asm` symbol hash/index, symbol metadata, tests.
  - Steps: report probe/chain/compare distributions; model bucket/hash/metadata costs; add stored hash/length and/or secondary final-component index; dual-lookup compare; measure memory.
  - Dependencies/worktree: Items 6a, 20a, and 22b; positive threshold; child slice.
  - Before/after metrics: average/p50/p95/max probes and comparisons, index bytes, B04/B10 time; distinguish observed from theoretical capacity.
  - Correctness/parity/failure validation: collisions, long names, case/scope/final-component ambiguity, capacity/exhaustion, exact diagnostics.
  - Rollback/kill/reference strategy: legacy lookup switch through Item 28; reject if immaterial, otherwise remove the comparator in Item 29i.
  - Effort/risk and stop/go: M/Medium; skip if prepared IDs already remove the hot lookup share.
  - Full quality gates: native format/staged gate, Rust quality gate, focused symbol FS-UAE and complete wrapper.
  - Plan-compliance review evidence: reviewer checks measured distribution and memory tradeoff.
  - Commit outcome: `perf(native): improve measured symbol lookup path` or no-go decision commit.
  - Definition of done: lookup distribution is bounded/improved with exact parity, or evidence closes the idea.

### Phase 9 — transfer proven Rust mechanisms and tune native-only costs

- [ ] Item 25 — Publish native-transfer decisions and add one item per positive candidate
  - Source requirement or finding IDs: SR-RF, SR-ACC, SR-ARCH, F10-F11 and accepted Phase 2 decision records.
  - Rationale/mechanism: prevent automatic or speculative native ports.
  - Architectural boundaries: no native code in this decision item; candidates cite exact portable/package contracts and preserve generic native interpreter fallback.
  - Expected files: `documentation/performance/results/**native-transfer-decisions**` and an amended/re-reviewed plan.
  - Steps: classify each Rust result; for execution-representation candidates compare interpreter, predecoded tape, threaded handlers, selected superinstructions, and portable-IR-to-68020 cross-assembly on generation/setup/relocation/code/RAM/cache/coverage/verification cost; choose port/adapt/leave-Rust/reject; amend this plan with one concrete checkbox Item 25.x and one paired cleanup Item 29k.x per approved candidate, including exact symbols/files/tests and a new plan-quality PASS.
  - Dependencies/worktree: completed Phase 2 reports and native correlation from Items 6b and 18b-24; integration worktree.
  - Before/after metrics: projected native coverage and break-even from measured counts; no projected speedup reported as achieved.
  - Correctness/parity/failure validation: traceability/schema checks and plan-quality review.
  - Rollback/kill/reference strategy: default decision is no transfer; plan amendment can remove a candidate without code churn.
  - Effort/risk and stop/go: M/Low; no candidate proceeds without positive Rust result, native coverage, and plausible 68020 model.
  - Full quality gates: workflow gate and plan-quality reviewer PASS.
  - Plan-compliance review evidence: reviewer checks every port cites a complete positive Rust decision.
  - Commit outcome: `docs(perf): decide Rust VM optimization transfers`.
  - Definition of done: every accepted Rust result has one explicit transfer disposition; each approved port exists as a newly reviewed, concrete, single-candidate Item 25.x rather than a repeatable generic checkbox.

#### Contract for concrete amended Item 25.x transfer slices

Item 25 does not itself authorize implementation. Each approved candidate must
add a specific checkbox with its Rust decision-record ID, exact portable contract
and validated signature/capability key, one mapped native VM/service and exact
symbols/files, one child worktree/commit, disabled/generic/enabled/bounded-dual
modes, eligibility/hit/fallback/mismatch counters, 68020 setup/coverage/component/
B10/code/memory measurements, malformed/near-miss/fallback and complete Level D
parity, a kill switch and generic fallback, a material-benefit stop/go rule, and
a paired Item 29k.x that removes only its temporary comparison machinery after
qualification. The amended plan and quality sidecar must pass the full plan
workflow before that new item starts.

If the candidate uses VM bytecode as an intermediate assembler format, the
25.x slice must additionally name the canonical-bytecode-to-portable-IR
validator, deterministic target backend, relocation/calling-convention contract,
generated-code memory and cache budget, stale-signature rejection, regeneration
test, and proof that the backend contains lowering mechanics rather than
CPU/family/dialect assembler semantics.

- [ ] Item 26 — Profile and tune one truly native-only 68020 overhead
  - Source requirement or finding IDs: SR-NATIVE, SR-ARCH, SR-PAR, F10.
  - Rationale/mechanism: address ABI/register-save/dispatch/alignment/cache cost after higher-level work is removed.
  - Architectural boundaries: 68020 baseline interface; semantics stay in package/generic owner; no Rust-first requirement only when mechanism is inherently native-only.
  - Expected files: one measured native runtime/ABI/dispatch path, counters/tests/decision record.
  - Steps: use handler symbols/maps and native profile; select one cost; model alternatives; implement smallest trusted-entry/dispatch/register/alignment change; measure generic and real workload; optionally model later CPU variants.
  - Dependencies/worktree: Items 18b-25 and all concrete approved Item 25.x slices (or an explicit Item 25 no-transfer decision), plus a positive native-only profile; child slice.
  - Before/after metrics: calls/dispatches/instructions or sampler share, component/B10 time, code size/cache/memory, 68020 first.
  - Correctness/parity/failure validation: ABI preservation, invalid program/error path, generic comparison, exact artifacts and complete Level D.
  - Rollback/kill/reference strategy: old entry/dispatch switch through Item 28 and cleanup Item 29j; no 68080 code retained without separate reliable results and one interface.
  - Effort/risk and stop/go: M/High; stop after one candidate; reject micro-wins that regress code size/cache or B10.
  - Full quality gates: native format/staged gate, Rust quality gate, focused and complete FS-UAE wrapper.
  - Plan-compliance review evidence: reviewer verifies the cost is native-only and measured.
  - Commit outcome: `perf(native): tune measured 68020 <abi-or-dispatch> cost` or revert.
  - Definition of done: a single native-only decision has reproducible evidence and exact parity.

### Phase 10 — regression budgets and terminal proof

- [ ] Item 27 — Install deterministic performance regression budgets
  - Source requirement or finding IDs: SR-MEAS, SR-ACC, SR-NATIVE, SR-PAR, F1-F12.
  - Rationale/mechanism: catch mechanism regressions in CI without relying on noisy wall time.
  - Architectural boundaries: deterministic counters are normative; wall-time lanes are optional isolated signals; budgets do not weaken correctness gates.
  - Expected files: performance verifier/scripts, CI/Makefile integration, saved representative profiles, documentation.
  - Steps: select stable Rust VM counts and native platform/accelerator counts; set reviewed tolerances; save schema-versioned profiles; gate coverage/fallback/mismatch and operation formulas; add optional benchmark lane.
  - Dependencies/worktree: accepted Phases 0-9; integration worktree.
  - Before/after metrics: budget stability across repeated runs and CI overhead; mismatch budget exactly zero.
  - Correctness/parity/failure validation: deliberate regression tests, stale schema/profile rejection, normal quality/parity suites.
  - Rollback/kill/reference strategy: update budgets only with reviewed mechanism explanation and fresh baseline; noisy wall time cannot block deterministic correctness.
  - Effort/risk and stop/go: M/Medium; omit unstable counters rather than encode broad tolerances.
  - Full quality gates: performance verifier tests, Rust/native gates, workflow gate, plan-compliance review.
  - Plan-compliance review evidence: reviewer checks each budget maps to an accepted mechanism and corpus.
  - Commit outcome: `ci(perf): enforce deterministic Rust and native budgets`.
  - Definition of done: representative regressions fail deterministically, accepted paths stay green, and CI cost is documented.

- [ ] Item 28 — Qualify all optimized and reference modes before cleanup
  - Source requirement or finding IDs: SR-MEAS, SR-PAR, SR-REMOTE, SR-TERM, SR-ACC, all findings.
  - Rationale/mechanism: prove end-to-end value and semantic identity while temporary reference/dual modes still permit direct comparison.
  - Architectural boundaries: unchanged fail-closed Level D protocol, exact guest completion/zero exit, attempt-all recovery, ephemeral artifacts, and generation proof.
  - Expected files: pre-cleanup qualification report and raw manifests/digests; documentation only unless a proof bug is separately planned.
  - Steps: run B01-B10 generic/reference/accepted/dual modes; all output/listing variants; Rust profile; 68020 and separately 68080 where reliable; full native suite; gen0->gen1->gen2 comparisons; list every temporary path and its exact cleanup item.
  - Dependencies/worktree: Item 27 and every accepted optimization/concrete Item 25.x integrated; performance integration worktree.
  - Before/after metrics: repeated median/range/p95, mechanism counts, profile overhead, code/memory/startup, accelerator coverage/fallback/mismatch; no unsupported aggregate claim.
  - Correctness/parity/failure validation: full Rust gate, full native gates, every artifact/diagnostic/exit comparison, fresh terminal self-hosting proof.
  - Rollback/kill/reference strategy: any mismatch or terminal failure blocks closure; bisect/revert the owning slice, never waive or rename evidence.
  - Effort/risk and stop/go: L/High; stop and triage the corrected invariant if any failure moves or times out.
  - Full quality gates: authoritative Rust quality gate, native formatting/staged gate, complete FS-UAE wrapper, workflow gate, final plan-compliance review.
  - Plan-compliance review evidence: reviewer cites all commits, reports, gates, and terminal artifacts.
  - Commit outcome: `docs(perf): qualify optimized paths before cleanup`.
  - Definition of done: exact parity and terminal proof pass in reference/optimized modes and every temporary path maps to Items 28a-28d, Items 29a-29j, or a concrete candidate Item 29k.x.

- [ ] Item 28a — Remove Item 10 Rust experiment comparison scaffolding
  - Source requirement or finding IDs: SR-RF, SR-ACC, SR-PAR, F11 and Item 10 decision record.
  - Rationale/mechanism: remove candidate-specific duplicate execution/comparison buffers after qualification while retaining the generic implementation, accepted optimized path, shared bounded diagnostic comparison facility, and compatibility fallback.
  - Architectural boundaries: no semantic, eligibility, profile-ID, or fallback change; a rejected Item 10 produces a documented no-op cleanup.
  - Expected files: exact Item 10 candidate module/tests/decision record.
  - Steps: remove temporary prototype wiring and duplicate buffers; retain supported modes through shared facilities; prove no candidate-specific dead path remains.
  - Dependencies/worktree: Item 28; sequential integration-worktree cleanup commit.
  - Before/after metrics: B10/code/memory plus eligibility/hit/fallback/mismatch remain within accepted Item 10 budgets.
  - Correctness/parity/failure validation: Item 10 differential/error corpus and exact Rust artifacts/diagnostics/state/fixups.
  - Rollback/kill/reference strategy: revert cleanup on mismatch/regression; portable interpreter and compatibility fallback remain.
  - Effort/risk and stop/go: S/Medium; stop if shared bounded comparison mode would be weakened.
  - Full quality gates: focused candidate tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer verifies only temporary Item 10 scaffolding is removed.
  - Commit outcome: `perf(vm): retire item 10 experiment scaffolding` or focused no-op decision.
  - Definition of done: no temporary Item 10 duplicate machinery remains and supported rollout modes still work.

- [ ] Item 28b — Remove Item 11 Rust prepared-path comparison scaffolding
  - Source requirement or finding IDs: SR-RF, SR-ACC, SR-PAR, Item 11 decision record.
  - Rationale/mechanism: remove candidate-specific dual buffers/reference plumbing after qualification while retaining generic interpretation, accepted preparation, shared diagnostic comparison, and fallback.
  - Architectural boundaries: preparation key/invalidation and semantics unchanged; rejected Item 11 yields a no-op cleanup.
  - Expected files: exact Item 11 VM/cache module, tests, and decision record.
  - Steps: delete temporary duplicate preparation/execution state; retain shared modes/counters; prove no stale/dead path.
  - Dependencies/worktree: Item 28a; next sequential cleanup commit.
  - Before/after metrics: setup/hit/miss/invalidation, B10/code/memory remain within Item 11 budgets.
  - Correctness/parity/failure validation: stale identity/version/pipeline/scope/error and full Rust corpus parity.
  - Rollback/kill/reference strategy: revert cleanup on mismatch/regression; generic fallback remains.
  - Effort/risk and stop/go: S/Medium; stop if eligibility/invalidation observability is lost.
  - Full quality gates: focused prepared-path tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer verifies only temporary Item 11 scaffolding is removed.
  - Commit outcome: `perf(vm): retire item 11 comparison scaffolding` or focused no-op decision.
  - Definition of done: no candidate-specific Item 11 duplicate machinery remains.

- [ ] Item 28c — Remove Item 12 Rust execution-representation experiment scaffolding
  - Source requirement or finding IDs: SR-RF, SR-ACC, SR-PAR, Item 12 decision record.
  - Rationale/mechanism: remove prototype-only comparison state after qualification while retaining canonical portable bytecode, the accepted predecode/threading/superinstruction representation, shared bounded dual mode, and generic fallback.
  - Architectural boundaries: bytecode version/signature and semantic ownership unchanged; rejected Item 12 yields a no-op cleanup.
  - Expected files: exact Item 12 VM/compiler/decoder tests and decision record.
  - Steps: remove candidate-specific duplicate dispatch/trace scaffolding; retain stable counters/modes; prove no dead opcode path.
  - Dependencies/worktree: Item 28b; next sequential cleanup commit.
  - Before/after metrics: dispatch/coverage/setup/B10/code/memory remain within Item 12 budgets.
  - Correctness/parity/failure validation: differential/malformed bytecode and full Rust corpus parity.
  - Rollback/kill/reference strategy: revert cleanup on mismatch/regression; portable generic path remains.
  - Effort/risk and stop/go: S/Medium; stop if stable diagnostic dual mode or fallback is weakened.
  - Full quality gates: focused VM/differential tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer verifies only temporary Item 12 scaffolding is removed.
  - Commit outcome: `perf(vm): retire item 12 experiment scaffolding` or focused no-op decision.
  - Definition of done: no prototype-only Item 12 duplicate machinery remains.

- [ ] Item 28d — Remove Item 13 exact-program accelerator experiment scaffolding
  - Source requirement or finding IDs: SR-RF, SR-ACC, SR-PAR, SR-ARCH, Item 13 decision record.
  - Rationale/mechanism: remove prototype-specific duplicate buffers/traces after qualification while retaining validated signature eligibility, generic interpreter fallback, shared bounded dual mode, and accelerator counters.
  - Architectural boundaries: package capability/signature contract and fallback remain; rejected/omitted Item 13 yields a no-op cleanup.
  - Expected files: exact Item 13 accelerator/catalog/tests and decision record.
  - Steps: remove candidate-specific prototype comparison machinery; retain shared modes and eligibility/hit/fallback/mismatch counters; prove no test-identity branch.
  - Dependencies/worktree: Item 28c; next sequential cleanup commit.
  - Before/after metrics: coverage/setup/B10/code/memory remain within Item 13 budgets; mismatches remain zero.
  - Correctness/parity/failure validation: signature near-miss/corrupt program/failure/full corpus differential parity.
  - Rollback/kill/reference strategy: revert cleanup on mismatch/regression; portable interpreter remains compatibility oracle.
  - Effort/risk and stop/go: S/Medium; stop if the required shared rollout modes cannot be retained.
  - Full quality gates: focused accelerator/differential tests and Rust quality gate.
  - Plan-compliance review evidence: reviewer verifies only temporary Item 13 scaffolding is removed.
  - Commit outcome: `perf(vm): retire item 13 experiment scaffolding` or focused no-op decision.
  - Definition of done: no prototype-specific Item 13 duplicate machinery remains and the supported accelerator lifecycle is intact.

Items 29a-29j are explicit one-subsystem cleanup commits. Each uses the
performance integration worktree sequentially from the prior cleanup commit,
changes no semantic contract, measures optimized-mode B10/code/memory before and
after, runs subsystem-focused failure/parity tests plus native format, staged
native gate, Rust quality gate, and the complete FS-UAE wrapper, obtains
plan-compliance PASS, and stops/reverts on any mismatch or material regression.
Each is estimated M effort/Medium risk; an item that removes high-risk assembly
state is treated as High risk by its reviewer when the exact diff warrants it.
The portable interpreter and supported generic compatibility fallback are not
temporary and are never removed by these items.

- [ ] Item 29a — Remove temporary session-lifecycle legacy clears and comparisons
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F2 and Items 16-16b.
  - Rationale/mechanism and boundaries: remove only source/statement/layout/label/image legacy-clear switches after Item 28 proof; retain poison/assert support.
  - Expected files: session lifecycle flags/comparison blocks and tests.
  - Steps: delete dead clear paths, prove no references, and rerun clear counters.
  - Dependencies/worktree and metrics: Item 28d; common cleanup contract; full-session clear remains zero and code/memory do not regress.
  - Correctness/parity/failure validation: common cleanup contract for reuse/capacity/early-error behavior.
  - Rollback/kill/reference strategy: revert this commit if any parity or budget fails.
  - Full quality gates: common cleanup Rust/native/complete FS-UAE gates.
  - Plan-compliance review evidence: reviewer verifies only lifecycle legacy paths are removed.
  - Commit outcome: `perf(native): retire legacy session clears`.
  - Definition of done: no legacy full-arena path remains and all gates pass.

- [ ] Item 29b — Remove temporary layout/emission audit comparisons
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, SR-TERM, F4 and Items 18-18b.
  - Rationale/mechanism and boundaries: remove only Item 18 audit/dual-trace scaffolding; retain the checkpoint's layout/final modes plus accepted counters and final-size/fixup assertions.
  - Expected files: Item 18 audit switches, comparison traces, and tests.
  - Steps: delete temporary comparison machinery and re-prove convergence bytes zero/final emission one.
  - Dependencies/worktree and metrics: Item 29a; common cleanup contract; B05/B09/B10 and emission counters remain accepted.
  - Correctness/parity/failure validation: common cleanup contract with stability/all-output/retry failures.
  - Rollback/kill/reference strategy: revert this commit on any mismatch or budget failure.
  - Full quality gates: common cleanup Rust/native/complete FS-UAE gates.
  - Plan-compliance review evidence: reviewer verifies no checkpoint production mode is removed.
  - Commit outcome: `perf(native): retire layout emission audit scaffolding`.
  - Definition of done: the checkpoint's layout-only plus one-final mode remains without temporary audit machinery.

- [ ] Item 29c — Remove temporary directive and flow scanners used only for comparison
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F6 and Items 19-19a.
  - Rationale/mechanism and boundaries: remove duplicate sequential route/boundary scans after qualification; retain explicit fallback only for documented ineligible/unprepared errors, not silent normal execution.
  - Expected files: directive-router/flow-navigation switches, comparison code, and tests.
  - Steps: delete dead normal-path scanners and verify prepared coverage/fallback counts.
  - Dependencies/worktree and metrics: Item 29b; common cleanup contract; B07/B10 route/scan counts remain accepted.
  - Correctness/parity/failure validation: common cleanup contract plus malformed/nested control-flow diagnostics.
  - Rollback/kill/reference strategy: revert this commit on any mismatch or budget failure.
  - Full quality gates: common cleanup Rust/native/complete FS-UAE gates.
  - Plan-compliance review evidence: reviewer verifies only prepared-route comparison paths are removed.
  - Commit outcome: `perf(native): retire prepared-route comparison paths`.
  - Definition of done: normal valid statements never rescan routes/boundaries.

- [ ] Item 29d — Remove temporary expression cache/binding comparison machinery
  - Source requirement or finding IDs: SR-RF, SR-NATIVE, SR-PAR, F5 and Items 20-20a.
  - Rationale/mechanism and boundaries: remove bounded dual-evaluation instrumentation after proof while retaining compile/name-lookup fallback for explicitly ineligible identities.
  - Expected files: expression bridge mode, comparison buffers, and tests.
  - Steps: delete duplicate execution while retaining eligibility/fallback/mismatch observability required by the portable contract.
  - Dependencies/worktree and metrics: Item 29c; common cleanup contract; B06/B10 compile/bind/reuse counts stay accepted.
  - Correctness/parity/failure validation: common cleanup contract plus forward/current-PC/scope/state/failure parity.
  - Rollback/kill/reference strategy: revert this commit on any mismatch or budget failure.
  - Full quality gates: common cleanup Rust/native/complete FS-UAE gates.
  - Plan-compliance review evidence: reviewer verifies only duplicate expression comparison execution is removed.
  - Commit outcome: `perf(native): retire expression dual execution`.
  - Definition of done: no production duplicate expression evaluation remains.

- [ ] Item 29e — Remove obsolete statement/string representation arrays and comparisons
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F7-F8 and Items 21a-22b.
  - Rationale/mechanism and boundaries: realize memory savings by deleting only legacy scalar/text arrays after every consumer is accessor-backed and Item 28 passed.
  - Expected files: opasm BSS/constants/accessors and comparison tests.
  - Steps: prove no legacy readers/writers, delete arrays, recalculate exact session size/capacity, and update evidence.
  - Dependencies/worktree and metrics: Item 29d; common cleanup contract; bytes/live statement plus total BSS/pool high-water must meet Item 21 budget.
  - Correctness/parity/failure validation: common cleanup contract with maximum strings, exhaustion, capacity, scopes, diagnostics, and all artifacts.
  - Rollback/kill/reference strategy: revert this commit on any mismatch or memory/time budget failure.
  - Full quality gates: common cleanup Rust/native/complete FS-UAE gates.
  - Plan-compliance review evidence: reviewer verifies every deleted array has no consumer.
  - Commit outcome: `perf(native): retire legacy statement text arrays`.
  - Definition of done: exact new static/live sizes are recorded and old arrays are absent.

- [ ] Item 29f — Remove temporary STVM prepared-state comparison machinery
  - Source requirement or finding IDs: SR-RF, SR-NATIVE, SR-PAR, F9 and Items 23-23a.
  - Rationale/mechanism and boundaries: remove duplicate serialized reset/directive scans used only for dual comparison; retain explicit malformed/ineligible fail-closed handling.
  - Expected files: state-service switches, comparison buffers, and tests.
  - Steps: delete duplicate normal execution and retain version/invalidation checks and counters.
  - Dependencies/worktree and metrics: Item 29e; common cleanup contract; B07/B10 state decode/scan counts remain accepted.
  - Correctness/parity/failure validation: common cleanup contract across every profile, override, transition, pipeline switch, and malformed case.
  - Rollback/kill/reference strategy: revert this commit on any mismatch or budget failure.
  - Full quality gates: common cleanup Rust/native/complete FS-UAE gates.
  - Plan-compliance review evidence: reviewer verifies only STVM comparison scans are removed.
  - Commit outcome: `perf(native): retire STVM comparison scans`.
  - Definition of done: valid prepared state no longer executes the serialized reference in production.

- [ ] Item 29g — Remove temporary bulk-memory and copied-package reference modes
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F3 and Items 17-17a.
  - Rationale/mechanism and boundaries: remove byte-primitive comparison routing and copied embedded-package mode only after Item 28; retain external mutable package storage and primitive contract tests.
  - Expected files: copy/package switches, dead copied storage path, comparison tests.
  - Steps: remove reference routing/storage and prove embedded copy zero plus exact active-base behavior.
  - Dependencies/worktree and metrics: Item 29f; common cleanup contract; B01/B09/B10 copy counts/code/memory remain accepted.
  - Correctness/parity/failure validation: common cleanup contract with alignment/tails and embedded/external/corrupt package failures.
  - Rollback/kill/reference strategy: revert this commit on any mismatch or budget failure; use no-op evidence if Items 17/17a were rejected.
  - Full quality gates: common cleanup Rust/native/complete FS-UAE gates.
  - Plan-compliance review evidence: reviewer verifies only package/memory reference paths are removed or no-go is documented.
  - Commit outcome: `perf(native): retire package and memory reference paths` or a focused no-go documentation commit.
  - Definition of done: production retained paths have no temporary comparison mode and the embedded-copy disposition is final.

- [ ] Item 29h — Remove temporary byte-reader and duplicate-scan reference modes
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F1 and Items 14-15a.
  - Rationale/mechanism and boundaries: delete old byte-at-a-time root/module readers and scan switches after qualification; retain byte-oriented buffered API semantics and DOS errors.
  - Expected files: reader/root/module switches, dead loops, and tests.
  - Steps: remove byte-reader/scan reference code and prove no one-byte DOS source reads or redundant root existence scan remain.
  - Dependencies/worktree and metrics: Item 29g; common cleanup contract; B02/B08/B10 DOS formulas remain accepted.
  - Correctness/parity/failure validation: common cleanup contract with CR/LF, range, EOF, short/error read, missing/ambiguous module cases.
  - Rollback/kill/reference strategy: revert this commit on any mismatch or budget failure.
  - Full quality gates: common cleanup Rust/native/complete FS-UAE gates.
  - Plan-compliance review evidence: reviewer verifies all production input callers migrated before deletion.
  - Commit outcome: `perf(native): retire byte-scaled source readers`.
  - Definition of done: all production source paths use buffered/seekable input.

- [ ] Item 29i — Remove temporary legacy symbol-index comparison path
  - Source requirement or finding IDs: SR-NATIVE, SR-PAR, F8 and Item 24 when accepted.
  - Rationale/mechanism and boundaries: delete only the old lookup comparator after Item 28; make a documented no-op decision when Item 24 was rejected.
  - Expected files: symbol lookup switch/comparison tests or the Item 24 no-go record.
  - Steps: remove the legacy comparator or prove no cleanup is required; verify probe budgets and exact ambiguity behavior.
  - Dependencies/worktree and metrics: Item 29h; common cleanup contract; B04/B10 probe/time/memory remain accepted.
  - Correctness/parity/failure validation: common cleanup contract with collisions, long names, scopes, final-component ambiguity and exhaustion.
  - Rollback/kill/reference strategy: revert on mismatch/budget failure; a rejected Item 24 yields a focused no-op decision.
  - Full quality gates: common cleanup Rust/native/complete FS-UAE gates.
  - Plan-compliance review evidence: reviewer verifies symbol-index acceptance/no-go traceability.
  - Commit outcome: `perf(native): retire symbol lookup comparison path` or `docs(perf): confirm symbol-index no cleanup required`.
  - Definition of done: no unowned legacy symbol comparator remains.

- [ ] Item 29j — Remove temporary native-only tuning comparison path
  - Source requirement or finding IDs: SR-NATIVE, SR-ARCH, SR-PAR, F10 and Item 26 when accepted.
  - Rationale/mechanism and boundaries: delete the old ABI/dispatch/alignment comparison entry after Item 28 while retaining one semantic interface; make a no-op decision if Item 26 was rejected.
  - Expected files: exact Item 26 switch/tests or its no-go record.
  - Steps: remove the native-only comparator or prove no cleanup is required; verify ABI, invalid-program, code-size/cache, and B10 budgets.
  - Dependencies/worktree and metrics: Item 29i; common cleanup contract; Item 26 native-only metrics remain accepted.
  - Correctness/parity/failure validation: common cleanup contract with ABI preservation, invalid/error paths, and exact complete Level D.
  - Rollback/kill/reference strategy: revert on mismatch/budget failure; a rejected Item 26 yields a focused no-op decision.
  - Full quality gates: common cleanup Rust/native/complete FS-UAE gates.
  - Plan-compliance review evidence: reviewer verifies Item 26 acceptance/no-go traceability.
  - Commit outcome: `perf(native): retire native tuning comparison path` or `docs(perf): confirm native tuning no cleanup required`.
  - Definition of done: no temporary native-only comparator remains.

Any approved concrete Item 25.x must have its own concrete Item 29k.x cleanup
checkbox inserted here by the Item 25 plan amendment. Item 30 is blocked until
Items 29a-29j and all candidate-specific cleanup items pass and commit.

- [ ] Item 30 — Run post-cleanup terminal proof and publish the final report
  - Source requirement or finding IDs: SR-MEAS, SR-PAR, SR-REMOTE, SR-TERM, SR-ACC, all findings.
  - Rationale/mechanism: prove retained performance and semantic identity on the exact production shape after temporary comparison paths are gone.
  - Architectural boundaries: unchanged fail-closed Level D protocol, exact guest completion/zero exit, attempt-all recovery, ephemeral artifacts, generation proof, and generic portable fallbacks.
  - Expected files: final Rust/native result report, raw manifests/digests, updated plan checkboxes/receipts; documentation only unless a proof bug becomes a separately reviewed item.
  - Steps: rerun B01-B10, all output/listing variants, final Rust profile, 68020 and separate reliable 68080 results, complete native suite, gen0->gen1->gen2 and required artifact comparisons; classify remaining/rejected/Rust-only/transferred work.
  - Dependencies/worktree: Items 29a-29j and all amended Item 29k.x cleanup commits; performance integration worktree.
  - Before/after metrics: compare post-cleanup to Item 28 and original Phase 0 median/range/p95, mechanism counts, code/memory/startup, coverage/fallback/mismatch.
  - Correctness/parity/failure validation: full Rust/native/workflow gates, every artifact/diagnostic/exit comparison, fresh terminal self-hosting proof.
  - Rollback/kill/reference strategy: any mismatch or terminal failure blocks closure; revert the owning cleanup/optimization and rerun, never waive evidence.
  - Effort/risk and stop/go: L/High; triage the corrected invariant if any failure moves or times out.
  - Full quality gates: authoritative Rust quality gate, native formatting/staged gate, complete FS-UAE wrapper, workflow gate, final plan-compliance review.
  - Plan-compliance review evidence: reviewer cites all commits, reports, cleanup proof, gates, and terminal artifacts.
  - Commit outcome: `docs(perf): close Rust-first VM and native performance program`.
  - Definition of done: post-cleanup exact parity and terminal proof pass, results are reproducible, and the final report closes every item.

## Phase gates and quantitative acceptance

Targets below are mechanism contracts. Overall speed ambitions remain provisional
until Item 9 establishes current distributions.

| Phase | Mechanism success | End-to-end success/stop rule |
|---|---|---|
| Activated native attribution | Bounded native progress plus pass/flow/layout/symbol/expression/VM/platform counters and coarse timing; explicit complete/incomplete records | A repeated bounded FS-UAE/A6000 investigation attributes progress and ranks measured owners; no optimization or proof claim |
| 0 | Complete stable IDs, four Rust modes, promoted shared native counters, B01-B10, measured overhead | Control results repeat within documented variance and bridge attribution is retained; no optimization starts before gate |
| 1 | 100% executor inventory attribution; ranked programs/opcodes/PCs/sequences/helpers | B10 hotspot report answers required questions; otherwise instrument more, do not guess |
| 2 | Candidate event/dispatch/setup count changes as predicted; mismatches = 0 | Retain only within noise-safe non-regression and material B10/coverage evidence; foundations may be retained when explicitly required later |
| 3 | DOS reads scale with blocks, known ranges seek, redundant root opens/scans removed | Exact corpus parity; B02/B08/B10 non-regressive and improvement reported, not preclaimed |
| 4 | Full arena clear = 0; embedded full copy = 0 or measured exception; clears/copies proportional to live/used work | B01 fixed startup improves materially from baseline and B10 does not regress |
| 5 | Convergence image bytes = 0; final emission count = 1; size assertions all pass | B05/B09/B10 improve or foundational split is explicitly justified; exact all-output parity |
| 6 | Immutable expression compiles <=1 per valid identity; flow boundaries/routes prepared once; invalidation explicit | B06/B07/B10 improvement exceeds variance or candidate is rejected |
| 7 | Materially lower measured bytes/live statement plus pool overhead; obsolete capacity arrays removed only after proof | B03/B10 non-regressive; memory high-water materially lower, exact diagnostics preserved |
| 8 | State decode reuse and symbol probe p95/max distributions measured and improved | B04/B07/B10 benefit justifies index memory/code; otherwise revert/skip |
| 9 | Every port cites positive Rust record; native eligible/hit/fallback measured; mismatch = 0 | Retain only material native B10 benefit after setup/code/cache cost; 68020 result authoritative |
| 10 | Deterministic budgets detect injected regressions; all accepted mismatch counters = 0 | Exact B01-B10 parity, complete gates, and fresh gen0->gen1->gen2 proof |

Every generic VM hotspot investigation begins with Rust profile evidence. Every
Rust accelerator reports exact workload coverage, generic/accelerated/dual
results, setup, code size, memory, isolated and B10 change. Every native transfer
references the positive Rust decision record and repeats parity against both the
Rust oracle and native generic path. No synthetic-only win is accepted.

Provisional ambitions to calibrate, not commitments: counters-only overhead
should be low enough for routine targeted CI use; sampled mode should preserve
hotspot ordering; trace memory must be bounded; the native reader should use an
8-16 KiB candidate buffer selected from measured memory/time tradeoffs. Item 9
and Item 6 replace these ambitions with reviewed numeric budgets. Items 0a-0e,
Item 2, and Item 0f set only the bounded B01-B10 investigation and attribution
foundation; the historical multi-hour self-host observation is motivation only,
and no optimization is pre-approved.

## Validation and Quality Gates

- Rust sub-item: focused unit/integration/differential tests and the affected
  corpus/invariant guards. The full workspace gate is not repeated by default.
- Native assembly sub-item: `make native-68000-format-check`,
  `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`,
  proof-level-labelled focused Rust/native tests, affected architecture,
  inventory, and instrumentation guards, then the focused authoritative FS-UAE
  confirmation required by the loaded native rule packs.
- High-level phase closure: `scripts/workflow/run_rust_quality_gate.sh` (or the
  current equivalent full Rust gate), every accumulated workflow/native gate,
  and the complete current FS-UAE wrapper where the phase changed native
  behavior. A phase cannot close or hand work to the next phase without this
  receipt.
- Workflow/plan/report item: focused schema/report verifier and
  `make workflow-gate`.
- Before every commit: `plan-compliance-reviewer` PASS against the active item,
  diff, and validation receipts. A finding is not declared closed without the
  required finding-closure process when one applies.
- FS-UAE evidence is valid only with fresh challenge, exact guest start and
  completion, explicit zero guest exit, expected output/diagnostics, ephemeral
  artifacts, and attempt-all recovery. Launcher success is not proof.
- Optional opFoundry automation must preserve those semantics and produce
  traceable OFTB/OFTR-style job IDs, configuration, durations, checksums, exit,
  and retrieved profile artifacts.

## Blocking Rules

- Item 1 activates only from fetched remote checkpoint `68cc693c`; it does not
  complete or waive the parked self-hosting plan.
- Items 0a-0e follow Item 1 and may instrument only; Item 2 freezes the corpus;
  Item 0f then reports attribution without optimizing.
- One work item is active at a time. A blocked item prevents the next item until
  the plan is explicitly amended and reviewed.
- No F1-F10 optimization, candidate reordering, or generic VM optimization claim
  precedes Item 0f; after that report, every optimization item must cite a
  positive relevant measured threshold from Item 0f, Phase 0, or Phase 1, or
  stop for a reviewed no-go/reorder amendment. No generic VM optimization claim
  precedes Items 1-9. No
  native generic-VM accelerator precedes a positive Rust decision record and
  Item 25 transfer approval.
- Track N Items 14-18 may begin after Phase 0 without waiting for all Phase 2
  experiments, but must integrate through recorded commits and shared counters.
- A moved failure, reduced fixture, prefix scan, smoke run, timeout, stale marker,
  launcher exit, or missing artifact is not a fix or proof.
- A progress heartbeat, PC sample, partial counter snapshot, graceful abort, or
  `complete=false` report is localization evidence only and never Level D or
  terminal self-hosting proof.
- B01-B10 are the performance/profiling corpus. The full gen0 -> gen1 -> gen2
  self-host is excluded from routine profiling and performance acceptance and
  runs only at explicitly named terminal proof gates.
- Any mismatch, profiler overflow used as if complete, unstable identity,
  unexplained overhead, architecture-boundary violation, or B10 integrated-workload
  regression blocks acceptance.
- Do not stage or commit unrelated files. Do not push, merge to `main`, remove
  worktrees, or alter the primary checkout as part of this plan.

## Definition of Done

- Items 0a-0f, Items 1-30, and any approved 25.x transfer/29k.x cleanup items have focused commits,
  plan-compliance PASS receipts, green full gates, and recorded worktree/base/
  branch/integration relationships.
- Frozen B01-B10 workloads have bounded FS-UAE and physical-A6000 progress/
  counter evidence where feasible, measured instrumentation overhead, and a
  ranked attribution report. The historical multi-hour self-host observation
  remains motivation only, and terminal proof runs remain explicitly separate.
- Stable Rust/native-correlatable profiles and deterministic budgets exist;
  all VMs/services in the inventory are attributable; profiler overhead is known.
- Accepted Rust accelerators retain generic fallback and have complete decision
  records; every native transfer has positive Rust and native evidence.
- Native input, reset, copy, convergence, prepared-state, representation, and
  index mechanisms meet their recorded operation/memory criteria or have explicit
  measured no-go decisions.
- B01-B10 exact artifacts, diagnostics, states, layouts, fixups, and exits match;
  the complete Level D suite and fresh gen0->gen1->gen2 comparisons pass after
  performance work resumes parked Item 40 and Milestone 8.
- Temporary comparison/reference representations are removed where named;
  portable interpreters and supported fallback contracts remain.
- Final documentation lists retained wins, rejected/reverted ideas, Rust-only
  wins, native transfers, residual hotspots, configurations, and raw evidence.

## Commit Outcome

- Planning artifact commit: one documentation-only commit on
  `codex/rust-vm-native-performance-plan` containing this plan, its quality-gate
  sidecar, and the companion baseline.
- Execution outcome: one focused commit per checked work item on the recorded
  performance slice/integration branch. No combined mega-commit and no remote
  update without explicit user authorization.
