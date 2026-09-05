# Item 0f: bounded attribution decision

## Current scheduling amendment (2026-09-05)

The active performance plan now assigns broad nonterminal native qualification
to Step 16 / Item A-close. The user subsequently deferred all LSP work until
the final Item LSP-close. Step 08 / Item 0f requires its focused observation
checks and the explicit `run_rust_quality_gate.sh --defer-lsp` gate; it does not
rerun the 51-group native gate or repair LSP. Older scheduling statements below
are historical and superseded by that reviewed plan. All recorded failures,
proof limitations and raw observations remain unchanged. Raw B03/B10 receipt
SHA-256 identities and byte counts are inventoried in
`documentation/performance/results/opforge-item0f-observation-inventory-2026-09-05.json`.


This is the consolidated decision for the current investigation, not a native
parity oracle or a high-level gate receipt. Active `AGENTS.md` remains binding.
The plan-authoring workflow keeps Item 0f open until its required closure checks
and plan-compliance review pass. No optimization is implemented by this report.

## Stop/go

**Stop repeating the same B10 60/100-second experiment.** It has established
early progress and exposed limitations of the observer; additional identical
snapshots are not a prerequisite for writing down that outcome.

**Go to closure verification for this bounded investigation, not directly to
native optimization.** The full Rust quality gate and approved 51-group Phase 0
nonterminal native gate are required. Passing focused tests is not a substitute. If
closure is blocked, name the exact failed check and handle it as a bounded
remediation rather than restart the profiling loop.

**Do not claim the multi-hour self-hosting cause is solved.** These measurements
cover setup and part of the frontend. They do not explain later passes or
calibrate the A6000. The full self-host remains excluded from routine profiling.

## What the evidence establishes

| Mechanism | Measured evidence | Decision and limit |
|---|---|---|
| Whole-session initialization | A 41,221,928-byte clear is pending in uniquely bound native loop snapshots and completed in later exact-input B10 snapshots | Strongest localized early compute candidate; preserve lifecycle/capacity correctness in any future change. It is not proven to explain complete B10 or hardware runtime. |
| Frontend progress | Two usable no-I/O B10 pairs reach frontend at about 100.7s, with 20,837/20,985 VM opcodes and 2,193/2,215 copies | Progress, not a stationary loop in those intervals. Opcode and copy counts do not establish where elapsed frontend time is spent. |
| Source/module I/O | Corrected all-counter B10 records 7 module candidates, 23,865 reads returning 23,858 bytes; source audit predicts these totals | Byte reads and overlapping roots are real; no repeated whole-index scan per `.use` is established in this interval. Native DOS calls are not physical-drive access counts. |
| Observer perturbation | Exact-input B10 advances farther with I/O counters disabled; full-B03 ABBA controls have identical shared work and a 0.203s mean START-to-DONE difference | Use no-I/O mode provisionally for compute/bulk observations and separate all-counter structural-I/O runs. No general overhead factor or product speedup. |
| Later passes, symbols and expressions | No engine statement visits in the accepted B10 later snapshots; earlier controlled B03 aborts show some pass-one progress | No B10 backend, lookup or expression hotspot conclusion. Generic VM/AOT acceleration remains unproven and must follow the Rust-first plan. |

This orders measurement confidence, not a calibrated wall-time ranking. The
startup clear is the first specifically localized native compute mechanism;
frontend cost remains unresolved between I/O, VM execution and other work.
Any reordering of the existing optimization plan requires an explicit reviewed
amendment. Items 16/17 are relevant to clearing; Item 14 concerns buffered I/O;
neither is activated by this report or by an uncalibrated count alone.

## Evidence integrity and failed observations

The original instrumentation flags accidentally staged a duplicate main source.
The host selector is corrected and all subsequent receipts check the exact
frozen discoverable source inventory. Earlier receipts remain qualified; they
are not rewritten as exact-input measurements.

B10 repeat r2 supplied two valid pause-local snapshots. r3 supplied only its
early snapshot, then reached the coordinator timeout without a later frame.
The unchanged r4 retry supplied no frames and ended at the sampler deadline,
followed by launcher/proof rejection. Both failures are retained. Neither
missing observation is zero work, a confirmed native stall or proof of an
emulator-input defect. The intended three usable pairs were not collected.

The host sampler now records foreground PID observations separately from an
acknowledged fresh full register frame and command prompt. It verifies the
launched PID immediately before its single key request; it never retries a
toggle blindly. A missing acknowledgement is `debugger-prompt-timeout`, not a
generic successful launch or a sampled PC. The same-run memory binding,
pause-local raw decoder, exact guest protocol and timeout bounds are unchanged.
This corrects an observability gap; it does not retroactively diagnose r3/r4.

## Supporting reports

- `opforge-native-item0f-first-captures-2026-09-04.md`: controlled aborts and
  native setup-loop binding, with the earlier input-graph qualification.
- `opforge-native-observer-controls-2026-09-04.md`: observer launch controls.
- `opforge-native-io-counter-calibration-2026-09-04.md`: exact-input correction,
  module-read accounting and paired observer evidence.
- `opforge-native-common-boundary-controls-2026-09-04.md`: four B03 abort controls.
- `opforge-native-b10-repeatability-2026-09-04.md`: raw identities, usable repeat
  and retained missing-frame attempts.
- `opforge-corpus-v1-native-status-2026-09-04.md`: unresolved native corpus
  correctness/completion statuses. Diagnostic observations close none of them.

## Single debugger-entry confirmation

`opforge-b10-debugger-ack-confirmation-2026-09-04.json`, SHA-256
`58bd21e5ec0eef858baa6e553b0cd6410b7b75c3e500cb2b98ca327f469b09b4`,
contains two accepted snapshots. Both entry receipts identify the launched PID
81,910 as foreground immediately before and after the key request; fresh full
frame/prompt acknowledgements occur at 60.917254 and 100.799463 seconds after
START. The sampled frames follow at 60.918029 and 100.799554 seconds.

Both pause-local raw dumps independently reproduce all five stored profiles
with active run ID 3,789,605,646 and zero overflow. The first remains in setup
with the pending session clear; the second is frontend with that clear complete,
20,963 VM opcodes and 2,209 copies / 114,265 completed copied bytes. Native
executable, symbols, driver and exact source inventory match the earlier mode.
The sampler now has SHA-256
`cbe58a7c0d317d0d18076db405eb66a8b91a9f1df7606bdf6e62ba58c577bedc`;
this changed observer is not pooled into the old same-tool repeatability range.

The guest is intentionally stopped without DONE/exit. Test exit 101 and launcher
exit 1 remain failed proof, not completed assembly. All guest trees are removed.
This single confirmation verifies the new observations on a real capture; it
does not establish a reliability distribution or explain the earlier failures.
No further B10 repeat is scheduled by this investigation.

Twelve focused capture tests pass. `make workflow-gate` passes 128 host tests
and the 238-file native format check. Its staged-only checks see an empty index,
not a prepared commit. The independent reviewer verifies both raw frames,
counter records, focus receipts and cleanup. These are Level A/B host-contract
checks and Level E observations, not Level D native parity.

## Remaining closure obligations

1. The bounded debugger-entry confirmation is recorded above; this obligation
   is satisfied for the investigation, not a claim of fixed input reliability.
2. The authoritative full Rust and approved Phase 0 native gates have completed
   and failed. Their exact outcomes and transcript identities are recorded in
   `opforge-item0f-closure-gates-2026-09-05.md`.
3. Classify and remediate the gate failures before final plan-compliance review.
   A focused Item 0f commit follows only when required gates pass.

Additional frontend timing, if still needed for an optimization decision,
belongs in one narrowly specified approved instrumentation follow-up with its
own question and stopping criterion—not an indefinite prerequisite of this
already-consolidated evidence report. This does not waive any current gate or
authorize advancing beyond the active plan item.

### User-approved native gate-scope resolution

Before the approved amendment, Item 0f required the **complete current native wrapper**, but the plan's scope
and Blocking Rules reserve full self-hosting for explicitly named terminal
gates. `scripts/workflow/run_native_existing_parity_completion.sh --verify`
includes both `external_fs_uae_native_opforge_full_product_artifact_parity`
(one guest assembling the complete native product) and
`external_fs_uae_native_opforge_two_generation_self_host_parity` (gen0→1→2).
The latter is unequivocally a terminal self-host gate; the former is a separate
full-product correctness test whose deferral also needs an explicit decision.

The user explicitly approved separating Phase 0 and terminal gates, including
both named tests. `--verify-phase-zero` now retains every other group (51 of the
current 53), attempts all selected groups despite failures, and rejects nonzero,
skipped or empty execution. It identifies itself as a nonterminal gate, never
the complete native wrapper. Seven focused host wrapper tests pass, including
exact inventory and failure/skip/empty-run propagation.

The existing complete `--verify` and `--verify-generation-two-first` modes
retain all 53 groups and their proof requirements. Both deferred tests remain
mandatory at terminal Items 28 and 30. No test is counted as passed by deferral.
The plan explicitly records this Phase 0-only exception; all other phase gates
and full Rust validation remain unchanged. Item 0f stays open until its actual
required gates and review pass.

### Rust closure remediation

The full Rust gate exposed four static source-contract tests whose expectations
predated the committed coarse runtime counters (`edcb6466`):

- `motorola68020_opcore_expr_bridge_owns_first_run_scalar_expr_path`: the exact
  allowed import list omitted `debug.amigaos.runtime_profile`.
- `motorola68020_tkpkg_expression_service_has_one_implementation_owner` and
  `motorola68020_tkpkg_service_writes_little_endian_control_block_bytes`: exact
  expression-service sequences omitted conditional observer entry/leave; the
  latter also omitted the selection-service observer sequence.
- `motorola68020_tokvm_interpreter_records_operand_aware_vm_failures`: the
  dispatch/handler expectation used the old `opcodeEmitDiag` label rather than
  `opcodeEmitFailure`; the failure-kind and operand contract is unchanged.

Only the exact expectations in `crates/opforge-asm/src/tests.rs` are updated.
They now include the existing compile guards, register save/restore and observer
calls rather than stripping instrumentation or loosening sequence checks.
All four focused Level A/B tests pass; native assembly is unchanged.

The first already-failed full run and an intermediate run compiled before the
last sequence correction were stopped, not counted as completed gates. Their
logs are preserved at `target/workflow-logs/rust-quality-gate-item0f-stale-contracts-20260904.log`
and `target/workflow-logs/rust-quality-gate-item0f-service-expectation-20260904.log`.
The final full gate completed from the corrected source. Its assembler suite
passed all 1,586 tests, but the overall gate failed when the LSP client
integration suite reported 34 passed / 14 failed. The native nonterminal gate
also completed and failed 13 of 51 groups. Exact identities, counts, durations,
failure classes, transcript hashes and cleanup status are recorded in
`opforge-item0f-closure-gates-2026-09-05.md`. Neither result is a passed closure
gate.

### Native closure blocker: first group

The first Phase 0 group completed all seven attempted cases and failed after
1,461.64 seconds. The wrapper then attempted every remaining group; the final
result is 38 groups passed and 13 failed.
The first group's retained host transcript reports four rejected cases:

| Exact source | Result |
|---|---|
| `examples/mos6502/6502_allmodes.asm` | 300,000ms timeout waiting for completion/process exit, with empty partial guest streams; no completion proof. |
| `examples/mos6502/6502_first_run_artifact_contract.asm` | Explicit guest exit 1; pass 1 succeeds, then pass 2 rejects `beq done` with `OPC-NCLI026`, followed by `OPC-NCLI020`. |
| `examples/mos6502/65c02_simple.asm` | Explicit guest exit 1; pass 1 succeeds, then pass 2 rejects `bra skip` with the same diagnostics. |
| `examples/mos6502/65c02_allmodes.asm` | Explicit guest exit 1; pass 1 succeeds, then pass 2 rejects `bra skip1` with the same diagnostics. |

These are failed Level D confirmation attempts, not performance results. The
group did not reach its final case-by-case artifact assertions, so this report
does not independently promote the other three cases to parity success.
The host runner returned the captured error evidence and removed the per-case
trees before the wrapper advanced. No guest instrumentation, timeout extension,
native correction, or retry is introduced in response.

| ID | Hypothesis | Evidence for | Evidence against / limit | Status | Next discriminator |
|---|---|---|---|---|---|
| G1 | A pass-2 request/selection mismatch causes the branch failures | Three completed cases reach pass 2 and reject branch instructions | The exact earliest divergent boundary is not yet established; the diagnostic alone does not identify a selector or package bug | open | Compare the unchanged Rust reference and native request/selection boundaries for one exact failing source before proposing remediation |
| G2 | The all-modes timeout shares that cause | Same schema group and instruction family | No returned guest diagnostic or completion; elapsed timeout is not a root cause | open | Keep this failure distinct until boundary evidence connects it; do not infer a stall or repeat profiling |

Independent whole-Item 0f review found no additional implementation or evidence
blocker before these gate results: retained raw profiles, pause-local captures,
control comparisons, diagnostic/parity separation and cleanup were rechecked.
The latest workflow gate passes 130 host tests and 238 native-format checks.
The completed native and Rust failures block closure. Item 0f remains unchecked
and uncommitted. The sampling investigation is stopped, not reopened by these
correctness failures.

## Step 08 review correction (2026-09-05)

Independent review found that the first debugger request could acknowledge a
pre-request frame already present in the accumulated host transcript. The
observer now records its transcript offset before each key request, including
the first. A focused regression rejects a stale complete frame followed by a
new bare prompt and accepts a subsequent full fresh frame. All twelve capture
tests pass. This is Level A/B host observation integrity, not native parity.

The reviewer checked every retained debugger transcript: none contains a
`Next PC` or full D0 register frame before its first `Activated debugger` marker;
the missing-frame r4 receipt has no activation or frame. Thus this edge does
not invalidate the retained observations. No repeat B10 sampling is required
or scheduled, and no new Level D proof is claimed. The independently verified
27-entry raw inventory matches every path, byte count and SHA-256.

The fresh non-LSP Rust and staged workflow gates now pass with explicit host
exit 0; see `opforge-item0f-completion-gates-2026-09-05.md` for exact transcript
identities and scope. This closes no native or LSP failure. Final compliance
review and the focused Step 08 commit are recorded in the plan and its sidecar.
