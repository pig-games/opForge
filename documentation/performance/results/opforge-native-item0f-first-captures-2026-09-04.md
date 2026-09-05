# Item 0f: first bounded incomplete captures — in progress

Later qualification (2026-09-04): the profiled harness staged an unintended
second main-source filename. These receipts remain observations of that staged
workload, not exact-frozen-discovery measurements. The source-inventory cause,
harness correction and reruns are documented in
`opforge-native-io-counter-calibration-2026-09-04.md`. No earlier failure is
reclassified as successful assembly or parity.

This is Level E diagnostic evidence, not completed assembly, native parity,
profiler calibration, an accepted optimization threshold, or Item 0f closure.
The user-approved diagnostic entry amendment is `8c0f6b60`; frozen Item 2 inputs
and their explicit native failures are committed at `8fd49290`.

## Exact experiment

Use the complete frozen B03 input: 257 lines, 3,083 source bytes, one origin
directive and 256 no-operand instructions. No prefix or source truncation is
used. Source/command case digest is
`be04f9426a0ffb55278cdb05d1d604f5895e85a2cee0b6894ac43c9df1359acf`;
the corpus/package digests are recorded in each raw result and the frozen
manifest. Each invocation obtains the live Rust oracle from the actual input
and package, although an intentionally aborted assembly cannot pass parity.

The existing approved `OPFORGE_PROGRESS_ABORT_VISITS` control stops at a pass
statement boundary. All five counter groups and the existing terminal exporter
are enabled. No native assembly or guest debug routine was changed for these
captures. The new opt-in host adapter requests a known nonzero engine failure,
requires fresh protocol completion, and checks the abort flag, exact visit
limit, correlated incomplete records and zero overflow. It is separate from the
ordinary exact-artifact parity entry point.

Invocation policy: opt-in-allowed only for this user-authorized local diagnosis.
The wrapper uses `--test-threads=1`, leaves bounded guest execution/cleanup with
the existing coordinator, and never treats launcher exit as guest completion.

```sh
OPFORGE_FS_UAE_SMOKE=1 \
OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' \
OPFORGE_FS_UAE_CONFIG_TEMPLATE=/tmp/opforge-performance-68020-max-20260904.fs-uae \
OPFORGE_FS_UAE_ARGS='{fsuae_config}' \
python3 scripts/performance/production_corpus.py diagnose --case B03 \
  --abort-visits 1 --output /tmp/b03-abort1.json
```

Repeat with another visit limit and a new output filename. The temporary config
is generated with the corpus `fs-uae-config` command; its text is retained in
each capture's `CORPUS_CONFIG` transcript. Actual CPU is 68020, JIT off, speed
max, FS-UAE 3.1.66, inherited A4000 boot and the runner's 64 MiB Zorro III RAM.
The post-start ceiling stays 120 seconds. No physical A6000 run is represented.

## Captured observations

Raw report files include exact fixed-record bytes, decoded fields, challenge
messages and command/define metadata:

- `opforge-b03-abort1-2026-09-04.json`
- `opforge-b03-abort16-2026-09-04.json`
- `opforge-b03-abort64-2026-09-04.json`

All three captures returned guest exit 1 with `OPC-NCLI020`, the requested abort
flag/limit and no record overflow. Their case/decoder trees were removed by the
runner. The raw record groups were independently decoded again with
`decode_native_progress.py`'s functions and exactly reproduced the reported
profiles; recorded START/DONE challenge strings and exit fields also matched.
This audits the diagnostic report, not reusable proof authority.

| Recorded field | Abort at visit 1 | Abort at visit 16 | Abort at visit 64 |
|---|---:|---:|---:|
| Total statements retained | 257 | 257 | 257 |
| Last completed statement (zero-based) | none | 14 | 62 |
| Total elapsed ticks | 2,464 | 2,546 | 2,828 |
| Startup ticks | 27 | 27 | 27 |
| Package/setup ticks | 2,043 | 2,043 | 2,043 |
| Frontend ticks | 332 | 332 | 332 |
| Pass-one ticks | 62 | 144 | 426 |
| Source reads | 3,084 | 3,084 | 3,084 |
| Package reads | 1 | 1 | 1 |
| Tracked completed clear bytes | 43,250,219 | 43,250,219 | 43,250,219 |
| Tracked completed copy bytes | 125,915 | 125,957 | 126,101 |
| Encoder service calls | 0 | 14 | 62 |
| State service calls | 0 | 1 | 1 |
| Tokenizer/parser VM opcodes | 12,830 / 8,742 | 12,830 / 8,742 | 12,830 / 8,742 |

The bridge clock has a 50 Hz scale: 2,464 ticks is 49.28 recorded guest-clock
seconds; 2,043 ticks is 40.86. These are instrumented emulator intervals, not
host command wall time, A6000 runtime, or calibrated estimates of either.
No repeated unprofiled/observer perturbation experiment has yet established
timing variance. The three different visit limits are not repeated samples.

## Interpretation and next discriminator

- The entire frontend completed before the first abort. Additional statements
  make forward progress through pass one: 15 completed rows (the origin plus
  14 instructions), 14 encoder calls, one state call and 82 additional ticks.
  This does not diagnose later passes or rule out later stalls.
- From visits 16 to 64, another 48 encoder calls add 282 pass-one ticks and
  144 tracked copied bytes. Reads, clears and VM opcodes remain unchanged.
  That is consistent with roughly linear work over this limited interval,
  not a full-workload complexity proof or calibrated throughput.
- The early package/setup bucket is large, but its name is not attribution to
  package I/O. `opforge-cli/run.asm` begins it before assembly-session
  initialization; `source_reader.asm` ends it after package initialization.
  Its tracked clears total 41,224,606 bytes, including the large session arena.
  One bulk package read transfers 368,278 bytes. These counts do not separate
  time spent clearing, validating/loading the package or other setup helpers.
- In the measured pass-one delta, reads and VM opcode counts do not increase;
  encoder service calls do. Sample within that service before attributing its
  cost to a particular loop or representation. Do not infer that other inputs
  have no VM or expression cost.

| Hypothesis | Status | Next discriminator |
|---|---|---|
| Fixed setup is substantial before B03 pass processing | confirmed for these instrumented early captures | Split/sample session initialization versus package processing; measure observer perturbation |
| Early B03 pass-one cost scales with completed instructions | open; two deltas show forward progress and roughly linear cost | Repeat captures and inspect later pass boundaries |
| Later passes or B10 explain the previously observed long runs | open | Capture later pass boundaries and exact integrated B10, then symbolized PC samples |

## Integrated B10: failed capture, not a counter result

`opforge-b10-abort1-2026-09-04.json` records the same command/control applied to
the complete frozen integrated B10 source. The coordinator exceeded its
120,000 ms post-start limit without a completion marker; captured partial
stdout/stderr were empty. The host test exited 101 and the diagnostic wrapper
exited 1 with `capture_ok: false`, `capture: null`, `complete: false` and
`parity_passed: false`. Its 172.92-second host test duration includes building,
boot and teardown, not just assembly. The run tree was removed.

No counter value or reached phase can be inferred from this timeout. In
particular, absence of a returned record is not proof that the first visit was
never reached: a later abort/export path has not independently been excluded.
The next discriminator is bounded read-only PC/live-record capture during the
actual B10 invocation, before terminal return, with symbol/binary provenance.
Do not shorten its sources or extend the deadline merely to manufacture a
successful diagnostic result.

Native executable digests were not captured by this initial adapter; compiler
commit, host, generator digest, command/flags and raw protocol/records are
recorded, but binary provenance and timing calibration remain acceptance gaps.
Item 0f still requires representative corpus attribution, symbolized sampling,
overhead/control evidence, review, full Rust and the complete native wrapper
gates. No optimization or semantic correction is implemented or authorized by
these initial measurements.

Focused validation at this checkpoint: nine corpus/diagnostic metadata tests,
sixteen record-decoder tests, opt-in adapter compilation/skip checks, independent
raw-profile redecoding, explicit invocation/evidence-classification guards,
format/diff checks, plan bundle and `make workflow-gate` (105 tests) pass. The
workflow gate's staged portion had an empty index for these new Item 0f edits;
it is not a staged-commit receipt. Item 0f edits/results remain uncommitted and
have not passed final plan-compliance or the Phase 0 full gates.

## B10 live console continuation

The bounded host sampler now runs the actual frozen B10 case under a PTY with
`console_debugger=1`. It requires the exact fresh START challenge, requests
Cmd+D only for the launched process, and sends fixed read-only commands. It
does not change native assembly, source bytes, package or abort limit. These
runs intentionally stop without guest DONE/exit; the unchanged coordinator
rejects them with test exit 101 and wrapper exit 1. `sample_observed: true`
means a register frame was observed, not assembly completion or parity.

Raw reports, all in this directory:

- `opforge-b10-live30-2026-09-04.json`: frame at 31.014 host seconds after fresh
  START; PC field (debugger Next PC) `0x078E0C6E`, D0 `0x02411F72` (37,822,322).
- `opforge-b10-live60-2026-09-04.json`: host prelaunch failure; the initial Hunk
  reader imposed even relocation offsets although the writer accepts byte
  offsets. No guest/sample exists. The parser correction is host-only and has
  an odd-offset test; the failed attempt is not discarded.
- `opforge-b10-live60-retry-2026-09-04.json`: frame at 60.935 host seconds after
  fresh START; Next PC `0x078E0C6C`, D0 `0x00754908` (7,686,408). All five raw
  counter records are retained in memory-derived JSON. The configured stop is
  60 seconds; debugger entry latency explains the sub-second excess.

Both launched captures used the same 563,268-byte native executable, SHA-256
`b69242f0106a86c86c5861e94ec43bfe5f09d0a6578cb39e5c13e0c9415c50fe`.
The second build's symbol-map digest is
`9eb68678a002ec112f2bc40fde701fd9deddd50a273b69946a7dd737e3a1d2bb`.
Configuration, sampler digests, exact commands and fresh challenges are in the
raw reports. All three case trees were removed; no emulator remains running.

The retry matches 64 contiguous disassembled bytes, checking 56 bytes outside
declared relocation operands, to exactly one CODE location in the actual Hunk:
segment 1, offset `0x271C`, runtime base `0x078DE550`. The emitted symbol
`opasm.amigaos.engine.clearBytes.loop` is at `0x271A`. The raw frame displays
`CLR.B (A1)+`, followed by decrement and backward branch. During the retry's
ephemeral build, the first capture was independently matched against the same
SHA-256 executable: segment 1, offset `0x271E`, 68 contiguous bytes/56 checked.
Plain label files contain multiple sections, so the nearby-symbol candidate
list alone is not authoritative; code bytes and relocation binding supply the
runtime mapping. `Zl` found no tracked segment lists, and was not used as an
address oracle.
The interim reviewer identified the retry's misleading `nearby_symbols` field
name; subsequent captures use `unscoped_label_candidates` with an explicit
unknown-section warning. The original raw receipt is not rewritten.

The stack has the saved original clear request `0x0274FF28` (41,221,928 bytes)
and return address `0x078DF684`. The latter maps to code offset `0x1134`, after
the `clearBytes` call in `opasm.amigaos.engine.initSessionV1` (start `0x111A`).
This source routine clears the full session arena before copying its CPU name.
The actual Hunk allocates 52,662,688 BSS bytes; that is runtime allocation,
not disk executable size or the size of this one clear.

For each counter getter, the sampler verifies a relocated absolute LEA in the
bound CODE segment, reads its runtime operand, then reads only the fixed record
size. The existing strict decoder independently accepted all five snapshots,
including correlated run ID, active state, phase/pass and zero overflow:

| Live field at the 60-second sample | Value |
|---|---:|
| Current phase / pass | package (setup) / 0 |
| Statement visits / retained statements | 0 / 0 |
| VM opcodes / service calls | 0 / 0 |
| Source/package reads and bytes | 0 |
| Source opens / closes | 1 / 1 |
| Session clears requested / completed | 41,221,928 / 0 bytes |
| Earlier state clears completed | 516,003 bytes in 3 calls |
| Remaining clear-loop D0 | 7,686,408 |

The completion counter updates only after the entire clear returns: zero does
not mean no memory has been cleared. The active record's `exit_status: 0` is
an initialized field, **not** a guest exit. Its 26 elapsed ticks are the last
phase-boundary accounting update; the active package interval has not yet been
charged. Neither field can be treated as terminal evidence or current duration.

These two independent launches are consistent with substantial forward work in
the same bounded clear; they are not a continuous trace or calibrated slope.
The 60-second run directly localizes early setup, before source/package reading
or VM execution. It does not determine what happened at 120 seconds in the
ordinary launcher, explain later B10 work, or diagnose the historical multi-hour
self-host. PTY/console-debugger effects and run variance remain uncalibrated;
no physical A6000 throughput or projected completion time is inferred.

| Hypothesis | Status | Next discriminator |
|---|---|---|
| Full session clearing contributes substantial early B10 setup work | confirmed in these instrumented console captures | Compare observer/launcher modes and capture after setup |
| That clear explains the complete ordinary B10 timeout or multi-hour self-host | open | Later-phase captures and repeated observer controls |
| Source reads or VM dispatch explain the sampled early interval | falsified for the 60-second snapshot only | Revisit their counts once frontend begins |

Focused continuation validation: 14 host sampler/corpus tests and 39 coordinator
contract tests pass; the existing record decoder accepts the live snapshot.
These tests establish host capture/identity boundaries, not native parity.
Item 0f, full gates, calibration, broader attribution and final review remain
open. No optimization or guest semantic change has been made.

Continuation gate receipt: `make workflow-gate` passes with 110 host tests and
238 native files unchanged by formatting; plan bundle, Rust formatting,
architecture/inventory, explicit invocation and evidence-classification guards
pass. The staged portion still has an empty index and is not a commit receipt.
Independent reviewer `platform_coverage_review` (Hume) returned interim PASS
after the label-scope correction, explicitly excluding Item 0f closure, parity,
calibration and full-gate approval. All Item 0f changes remain uncommitted.
