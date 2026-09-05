# Item 0f: observer controls and later B10 capture — in progress

Subsequent source-inventory audit found an extra discoverable main-source alias
in these profiled runs. Their counters describe that staged workload, not the
exact frozen discovery graph. See
`opforge-native-io-counter-calibration-2026-09-04.md` for the harness correction,
scope of this qualification and corrected-input reruns. Earlier receipts are
retained without turning incomplete runs into parity or timing acceptance.

Level E diagnostic evidence only. No completed-assembly, native-parity,
optimization-acceptance, physical-A6000 or Phase 0 closure claim is made.
The active plan and `AGENTS.md` remain binding; the plan-authoring skill preserves
the current Item 0f boundary and the full closure gates.

## Matched control protocol

Use the unchanged complete frozen B03 case with abort-after-one-visit and all
five existing counter groups. Each run builds the native executable from the
same source/defines, obtains the actual case's live Rust oracle and uses a fresh
challenge. The deliberate guest exit is 1 with `OPC-NCLI020`, not success.

The three host modes are app launch with console disabled, direct PTY launch
with console disabled, and direct PTY launch with console enabled but never
entered. No control sends debugger commands. Exact START, exact matching DONE
and explicit exit are observed before cleanup with a monotonic host clock and
100 ms polling. The normal coordinator independently checks guest protocol,
exit, diagnostic and correlated abort/counter fields. A host timing observation
alone cannot make a control comparable, and no stored report is parity authority.
START-to-DONE includes guest executable loading and terminal record export,
not only the assembler engine interval. Host build/boot/teardown are excluded.

The corpus command is `diagnose --case B03 --abort-visits 1 --control-mode MODE`
with the same opt-in FS-UAE environment/config as the first-captures report.
Control waits stop at 115 seconds after START; the coordinator still caps guest
execution at 120 seconds. An eight-second grace after DONE applies only to the
opt-in observer, letting it clean up and print its receipt before the existing
termination fallback. That grace is not included in the measured interval.
Every case's binary, labels, config, protocol files, logs and outputs remain
ephemeral. Raw JSON records observations, never reusable executable/oracle data.

## First-round outcomes

`opforge-b03-control-app-r1-2026-09-04.json` has a valid controlled-abort capture
and host receipt: START-to-DONE 76.605660 seconds, explicit guest exit 1, no
debugger commands. Native executable SHA-256 is
`b69242f0106a86c86c5861e94ec43bfe5f09d0a6578cb39e5c13e0c9415c50fe`.
The profile reports 2,464 guest-clock ticks; these are not host seconds.

`opforge-b03-control-pty-r1-2026-09-04.json` and
`opforge-b03-control-console-r1-2026-09-04.json` both validate the deliberate
guest abort and counters, but have no host receipt. The coordinator's existing
500 ms TERM-to-KILL grace can kill the observer before its bounded child cleanup
and final report. No missing time is inferred from total test runtime. The
diagnostic-only post-DONE grace addresses this evidence-collection path without
changing guest correctness or execution deadlines. Retries use new filenames;
the earlier observations are retained.

This is a preliminary control comparison, not repeated-run calibration. It does
not measure counter-disabled overhead, the cost of entering/pausing the debugger,
late-stage workload behavior, or physical hardware throughput. Ordinary host
scheduling and run variance remain uncontrolled; no small percentage difference
is accepted as significant.

## Usable matched controls

The diagnostic-only cleanup grace allowed both retries to retain their host
receipts. All three rows below have the exact fresh protocol/explicit exit,
validated abort/counters, identical native SHA-256 above, and identical config
apart from the ephemeral Work mount and intentional console switch:

| Mode / raw receipt suffix | START-to-DONE host seconds | Total guest ticks |
|---|---:|---:|
| app-r1 | 76.605660 | 2,464 |
| pty-r2 | 76.500040 | 2,464 |
| console-r2 | 76.535441 | 2,464 |

Raw filenames are `opforge-b03-control-MODE-rN-2026-09-04.json`. Independent
revalidation matched each sampler START/DONE/exit to that same run's actual
captured guest protocol and verified the config digest and executable identity.
After excluding run identity and clock fields, the complete decoded work
profiles are exactly equal. Phase clocks are not identical: pass-one is 62,
61 and 62 ticks respectively. No clock discrepancy is discarded as a work count.

The observed spread is about 0.106 seconds; one usable sample per mode cannot
establish a precise overhead or variance estimate. This set shows no large
launcher/console effect on the bounded B03 control. It supports proceeding to
one later B10 localization sample, not accepting a performance baseline or
claiming that observer effects are absent. The first-round missing receipts
remain excluded from host-time comparisons. Host parser/teardown code changed
between attempts; sampler hashes identify those revisions, while actual native
bytes and work counts stayed unchanged.

## Later B10: primary PC binding limitation

`opforge-b10-live100-2026-09-04.json` retained a fresh 100-second frame with
Next PC `0x00F81754` in system ROM. The normal matcher found zero matching Hunk
CODE locations, so the receipt has no code binding or live counter records.
This is a real Level E register observation, not evidence of a reached assembler
phase, a particular system service, or a completed assembly. The guest remained
without the required completion proof, and its run tree was removed.

The follow-up capture requests one explicit D6 code-address candidate because
the first frame contained `0x078E15D8` there. It reads the register again in the
fresh run, never reuses that address or the earlier relocation base, and accepts
only a unique actual-Hunk CODE match. Even a successful register anchor is not
attribution of the sampled PC. Existing fixed-size records can then be read and
strictly decoded while the guest is paused. No memory/register write or stepping
command is introduced; the 120-second coordinator ceiling stays unchanged.

The fresh retry, `opforge-b10-live100-bound-2026-09-04.json`, also sampled ROM
(Next PC `0x00F8265E`). D6 was now `7`; disassembling that one candidate produced
no Hunk match. The tool correctly refused a binding and returned no counter
records. The displayed invalid instructions at that candidate were only read
and disassembled, never executed. Both 100-second runs remain failed/incomplete
guest cases (`capture_ok: false`, test 101/wrapper 1), even though register
frames were observed and cleanup completed. Neither proves frontend progress,
a wait/spin condition, or the cause of the original timeout.

No additional candidate guessing is justified by these results. The proposed next
capture should acquire the record addresses during early setup, retain the
verified mapping within that same guest process, resume with a bounded read-only
debugger command, then resample the records later with run-ID/state validation.
The continuation below implements and exercises that next step.
No mapping from an earlier run may be reused. The current sampler additionally
stops on fresh guest completion instead of interpreting a finished process as
an active sample; no post-completion memory reading is required for controls.

## Same-guest resumed B10 continuation

Raw receipt: `opforge-b10-live60-resume100-2026-09-04.json`.
Command: `diagnose --case B10 --abort-visits 1 --sample-after-seconds 60
--resample-after-seconds 100`, with the same explicit FS-UAE/automation opt-ins
and pinned configuration as above. Native binary and symbol-map hashes match
the preceding captures; the complete frozen B10 case remains unchanged.

The sampler uniquely bound early CODE (68 disassembly bytes, 56 non-relocated
bytes checked), read all five fixed records, and required active, correlated,
nonzero run identity with zero overflow before sending one bare `g`. At the
later pause it re-read the same getters, checked their opcodes and exact
pointers, and decoded only the new pause's memory. No earlier transcript bytes
could satisfy missing later reads. Completion was checked at each debugger
prompt before sampling. Initial binding attribution stays inside snapshot 0;
snapshot 1 explicitly refers to that same-process mapping rather than claiming
a fresh PC binding. Two-stop reports have no ambiguous top-level counter aliases.

| Observation | First pause | Later pause |
|---|---:|---:|
| Host seconds after fresh START | 60.981 | 100.726 |
| Captured Next PC | `0x078E0C6E` | `0x078DF11E` |
| Progress phase | package/setup | frontend |
| Session clear bytes requested | 41,221,928 | 41,221,928 |
| Session clear bytes completed | 0 | 41,221,928 |
| Module candidate attempts | 0 | 6 |
| Module reads | 0 | 31,573 |
| Module bytes returned | 0 | 31,568 |
| Logical source lines | 0 | 2 |
| VM opcodes | 0 | 113 |
| Statement visits | 0 | 0 |

Both snapshots have run ID 3,789,383,205, active state and zero overflow in all
five groups. Tokenizer/parser opcodes at the later stop are 56/57; expression
and encoding work have not begun. Package loading has returned 368,278 bytes
in one read. Source-class reads returned 23 bytes in 23 calls. Module opens/
closes are 6/5, so the module scan is not complete. These are observed counts,
not accepted throughput. A logical source-line counter is not the count of
candidate lines inspected during module discovery.

This establishes real progress past the large setup clear into module
discovery within this bounded run. It does not establish how long every later
phase takes or explain the original multi-hour self-hosting run. The 39.746
host seconds between frames include the remainder of setup and debugger
interruption; they cannot be assigned solely to module reads. The guest tick
values 26/2,070 are phase checkpoints, not elapsed time sampled continuously.

Static corroboration: `opforge-cli/module_discovery.asm`'s
`scanCandidateFile.candidateReadLoop` explicitly requests one byte from
`dos.readInput` per call. `opforge-cli/dos.asm` dispatches that request to
AmigaDOS `Read` and invokes the optional read counter afterward. This explains
the observed near-one-byte-per-call pattern, but does not establish physical
drive accesses: filesystem caching can make DOS calls invisible as drive I/O.
The later frame's disassembly is consistent with the platform profiler's
`classOffset` calculation (class 3 and A5 equal to the OFIO record address).
This is source/disassembly corroboration, not an additional independent symbol
binding or a sample-share estimate. Per-read observer overhead remains a real
confounder that the earlier B03 controls did not calibrate.

The next diagnostic slice should compare this module-heavy interval with the
existing I/O-counter kill switch, retaining coarse progress/other counters and
the frozen input, then quantify module-candidate scan repetition. No buffered
reader, module-index optimization or native semantic correction is implemented
or accepted by this receipt.

The sampler reports two accepted snapshots and complete cleanup, but the guest
was intentionally stopped without DONE or an explicit guest exit. Accordingly
`capture_ok`, `complete`, `comparison_eligible` and `parity_passed` remain false;
the Rust test exits 101 and the diagnostic wrapper exits 1. Neither snapshot's
active-record zero exit field is a guest exit. The exact ephemeral run tree was
removed and no emulator/sampler process remained.

Independent host audit split the raw transcript at its two debugger entries,
reconstructed every byte at each snapshot's record addresses, and reproduced
every stored decoded field with the existing strict decoders. It also checked
the two register frames, identical record locations/run identity, single `g`,
capture bounds, unchanged native identity, config/sampler/driver digests and
explicit incomplete status. This is Level E capture integrity, not Level D
native parity. Focused host tests are Level A/B tooling checks only.

## Static arena accounting, independently checked against emitted symbols

The actual all-counter build's symbol map gives these section-local spans:

| Arena group | Bytes |
|---|---:|
| Emitted header | 98 |
| Source tables and packed source text | 5,194,304 |
| Statement tables | 30,800,000 |
| Label tables/hash heads | 2,081,792 |
| Tail fields | 12 |
| Three image buffers | 3,145,728 |
| Full emitted arena | 41,221,934 |

All boundaries here belong to `opasm.amigaos.engine`'s declared BSS arena, not
an unscoped nearest-label comparison. The map gives start offset 408,024, source
tables 408,122, statement tables 5,602,426, label tables 36,402,426, tail
38,484,218, images 38,484,230 and end 41,629,958. The source constants and all
non-header groups agree with these emitted spans. Statement capacity is
100,000 entries at 308 bytes each, whether or not the current input needs them.

The declared header constant is 92, whereas the emitted header span is 98.
Consequently `OPASM_ENGINE_ASSEMBLY_SESSION_BYTES` requests 41,221,928 bytes,
six fewer than the emitted arena. The live clear-request observation matches
the constant. This confirms a count/span discrepancy, not a demonstrated
observable output failure. Its possible reinitialization/presence-tail impact
needs a focused invariant test before a fix claim. No guest code is changed
in this diagnostic item.

## Open questions

| Question | Status | Next discriminator |
|---|---|---|
| Does the console-enabled launcher materially alter the early workload? | no large effect observed in three usable matched controls; calibration remains open | Repeat controls and measure counter-disabled/entry effects |
| Where does B10 spend work after the setup clear? | same-guest snapshot reaches frontend module discovery with 31,573 reads; later phases and time shares remain open | Calibrate per-read counter overhead and quantify candidate scanning before choosing an optimization |
| Does the six-byte short clear affect reinitialization behavior? | confirmed count mismatch; behavior impact open | Explicitly scoped reset/presence-tail invariant test before any correction |

Item 0f remains uncommitted and incomplete. Required full Rust/native closure
gates, corpus-wide attribution, counter-disabled controls and final review are
not replaced by these controls.

## Validation receipt for this bounded continuation

- Focused sampler/corpus Python tests: 15 passed.
- Host coordinator tests (`fs_uae_smoke::tests`): 40 passed.
- `make workflow-gate`: passed after the final completion-at-prompt guard,
  including 111 workflow tests and 238 native files checked with no formatting
  changes. Its staged checks saw an empty index; this is not a staged commit
  receipt. Architecture warning-only findings remain advisory.
- Plan bundle validation, Rust formatting check and whitespace diff check:
  passed. No native assembly was changed.
- Required reviewer: interim PASS for the bounded diagnostic/control slice,
  including the exact completion check after debugger pause and before sampling
  commands. This is explicitly not Item 0f closure or commit authorization.
- Emulator/sampler processes were stopped and ephemeral run trees removed.
  The full Rust quality gate and complete native FS-UAE closure wrapper were
  not run for this continuation; no full-gate or parity success is claimed.

Same-guest continuation validation: 35 focused sampler/driver/decoder tests
passed, including changed identity, missing later bytes, overflow, phase/state
mismatch, bounded delays, environment isolation and rejected-second-stop
reporting. `make workflow-gate` passed with 115 host tests and 238 native files
unchanged by formatting; staged checks still saw an empty index. Interim
plan-compliance review passed before the bounded guest probe. Plan bundle and
diff checks pass; Item 0f and full closure gates remain open and uncommitted.
The same read-only reviewer (`platform_coverage_review`, Hume) subsequently
reconstructed both pause-local snapshots and returned interim PASS for the
result, report and plan. This remains explicitly not closure or commit approval.
