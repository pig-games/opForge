# Item 0f: I/O-counter comparison and source-inventory correction

Level E diagnostic evidence only; in progress. Active `AGENTS.md` and the
performance plan remain binding. The plan-authoring skill keeps this work
inside Item 0f and does not waive its full closure gates.

## Corrected measurement invariant

Profiling flags must not change the discoverable guest source graph or CLI
command. A read-only inventory of the first no-I/O probe's ephemeral Work mount
found two identical copies of the 23,662-byte main source:

- `opforge_6502_native_cli_smoke.asm` — the frozen case's main file.
- `opforge_fsuae_smoke_input.asm` — an unintended harness alias.

Both have SHA-256
`930ed70077e9bae5a6f86a45fee7050306bd38de5523b3d047af87533a5eb823`.
The four support files match the frozen case: `modules/helper.asm` (26 bytes),
`modules/math.asm` (62), `includes/inner.inc` (10), `includes/outer.inc` (10).
The inspected tree was `target/fs-uae-hunk-smoke-opforge_cli-1788531118981625000/Work`;
it was removed by the coordinator after the probe.

Cause: `opforge_native_cli_case_define` treated the first assembly define as a
fixture selector. `OPFORGE_DEBUG_CONTRACTS` therefore selected the generic
source fallback; common staging had already written the canonical main file.
The explicit corpus command still selected its named canonical file, but module
discovery recursively scanned the directory containing both copies. This is a
measurement-input artifact, not evidence of a native algorithm regression.

The harness now recognizes only its explicit legacy fixture selectors and
ignores instrumentation defines regardless of ordering. Host checks verify
identical single-main inventories and explicit command interpolation with
observers off/on/I/O-disabled, plus preserved legacy include-selector routing.
Before an opt-in diagnostic guest launches, the sampler additionally verifies
all discoverable `.asm`/`.inc` paths, sizes and SHA-256 values against the frozen
case manifest. Extra aliases, missing/changed inputs, symlinks and oversized
inventories fail closed. The resulting inventory is included in new receipts.
No native assembly algorithm, allocation or I/O implementation changed.

## Earlier receipts retained with a qualification

The prior all-counter B10 snapshots and the first no-I/O receipt
`opforge-b10-noio-live60-resume100-2026-09-04.json` describe the extra-alias
workload. Their raw counters remain valid observations of that staged workload,
but they are not exact-frozen-source-graph performance measurements. Earlier
profiled corpus cases used the same selector path; claims about their discovered
file graph require the same qualification. Their guest-protocol outcomes are
not retroactively converted into successes or erased.

The first no-I/O run had two active, correlated snapshots at 60.961/100.707 host
seconds. The later snapshot reached frontend, completed the 41,221,928-byte
session clear, and recorded 113 VM opcodes and 1,106 copied bytes in 17 calls,
the same coarse work observed in the earlier all-counter run. I/O counters,
including logical lines and module attempts, are disabled in this mode; their
zeros do not mean no work. No overhead percentage follows from these snapshots.

## Comparison protocol

Use unchanged B10 and the existing `OPFORGE_PROGRESS_PLATFORM_NO_IO` kill switch
through `diagnose --diagnostic-profile all-no-io`; the control uses `all`.
All other counter groups and bulk-memory counting remain enabled. The mode is
diagnostic-only: ordinary parity cannot select it, and all five decoded records
must still correlate with zero overflow. Enabled-group bits must match the
requested mode. Source/command/package and 68020/max-speed/JIT-disabled config
are unchanged. Different native executable hashes are expected because the
kill switch removes observer code; it does not implement buffered reading.

Both runs request same-guest stops at 60/100 seconds after fresh START, retaining
the unchanged 120-second coordinator ceiling. This is one run per mode, not a
repeated timing distribution. The disabled mode still has observer call sites
and other counters, so it is not an entirely uninstrumented baseline. Missing
completion/exit remains failure, regardless of accepted snapshots.

## Scan-path audit

The native resolver builds an index once (`ModuleScanIndexBuilt`) and then
looks up `.use` names; this contradicts assuming a complete rescan for every
`.use`. It recursively scans `.asm` and `.inc` files under each configured root.
Root deduplication compares full path strings, not ancestor overlap. For B10,
the input seeds `Work:` and explicit `-M modules` adds `Work:modules`; the two
small module files can therefore be read in both scans. Candidate/index
deduplication happens after reading the file, so it does not remove that I/O.

For the exact five-file graph, a complete index build is expected to inspect
seven candidates and 23,858 bytes: 23,770 unique bytes plus 88 bytes from the
overlapping module root. With one-byte reads and one EOF read per candidate,
that predicts 23,865 module reads. The old extra-main graph instead permits
47,520 bytes across eight candidates. These are source/inventory-derived
predictions until checked against runtime counters; they are not inferred
completed guest work. File-system caching means a DOS read need not access the
physical drive.

## Corrected-input paired results

Raw receipts:

- `opforge-b10-exact-all-live60-resume100-2026-09-04.json`
- `opforge-b10-exact-noio-live60-resume100-2026-09-04.json`

Both prelaunch inventories match all five frozen files exactly, and the
configurations match except for the ephemeral Work mount. The native binaries
are unchanged from the corresponding observer modes before the harness repair:

| Mode | Native bytes | Native SHA-256 |
|---|---:|---|
| all | 563,268 | `b69242f0106a86c86c5861e94ec43bfe5f09d0a6578cb39e5c13e0c9415c50fe` |
| all-no-io | 562,768 | `c615e29d5ffaf595f6ab6375c0efaf383352abbc6fb037510ab815f89d7a1c4c` |

The 500-byte difference is disabled observer code, not a product optimization.
Within each run, both snapshots have matching active run IDs and all five
records decode without overflow. Run IDs are 3,789,456,947 and 3,789,466,346
respectively; mappings are never reused between runs.

| Observation | all | all-no-io |
|---|---:|---:|
| Early frame: host seconds after START | 60.888 | 60.796 |
| Later frame: host seconds after START | 100.702545 | 100.702199 |
| Early/later phase | package / frontend | package / frontend |
| Session clear completed bytes, early/later | 0 / 41,221,928 | 0 / 41,221,928 |
| Tokenizer invocations at later frame | 246 | 303 |
| Parser invocations | 454 | 557 |
| Tokenizer VM opcodes | 12,342 | 15,331 |
| Parser VM opcodes | 4,491 | 5,506 |
| Total VM opcodes | 16,833 | 20,837 |
| Copy calls / completed bytes | 1,783 / 92,500 | 2,193 / 113,575 |
| Statement visits | 0 | 0 |
| Logical source lines | 248 | unavailable: I/O group disabled |
| Module candidates / reads / returned bytes | 7 / 23,865 / 23,858 | unavailable: I/O group disabled |

The all-counter module totals exactly match the seven-candidate inventory
prediction. Source-class reads are 4,345 calls returning 4,343 bytes, and package
loading is one read returning 368,278 bytes. The duplicate-main correction
therefore removed a real measurement confounder; it did not speed up the native
algorithm. Neither ROM PC at the later stops is treated as a symbolized owner;
the counter records use the earlier same-process verified mapping.

With I/O counting disabled, approximately 23.8% more VM opcodes were counted at
essentially the same host stop time. Tokenizer/parser invocation counts and
bulk-copy work also advance further. This is a meaningful observer-perturbation
signal in one paired observation, **not** a 23.8% elapsed-time improvement or a
validated overhead percentage. The programs are at different work positions;
host variance, debugger entry cost and mode-dependent code layout are not
separated. The phase clocks are checkpoints (both total 2,070 ticks), not a
continuous timer at the later sample. No time-share or physical-A6000 speed
claim follows.

Both guests were intentionally interrupted without DONE or an explicit guest
exit. Each test exits 101 and wrapper exits 1; `capture_ok`, `complete`,
`comparison_eligible` and `parity_passed` remain false. This is four verified
Level E snapshots, not two successfully completed assemblies. An independent
host audit reconstructed every record from each pause's raw memory dump and
matched all stored decoded fields, register frames, counter-mode flags, source
inventories, configuration/sampler/driver hashes and time bounds. All three
probe trees from this continuation were removed and no emulator remained.

## Measurement decision and remaining questions

- The early large session clear is real and completes; it is not a stationary
  infinite loop in these observations. It remains a measured startup candidate,
  not an accepted optimization.
- Byte-at-a-time module/source reads are real. The module audit corroborates a
  single index build with a small overlapping-root reread, not repeated whole
  scans per `.use` in this interval.
- Per-read counters visibly perturb frontend progress. Use them for structural
  read-count evidence; do not use their timing alone to rank native hotspots.
  The subsequent full-B03 ABBA common-boundary control reports a small mean
  START-to-DONE difference of 0.203 seconds with identical shared work. See
  `opforge-native-common-boundary-controls-2026-09-04.md`. Use `all-no-io`
  provisionally for compute/bulk attribution and separate `all` runs for I/O
  structure; no B10 overhead correction or percentage is established.
- Backend passes, complete B10 runtime, counter-free overhead and physical
  hardware behavior remain unmeasured. No native fix or buffered-reader change
  is authorized or implemented by this report. Full Item 0f closure is pending.

## Validation and status

- Level A/B host tooling: 36 focused Python tests passed; one Rust define-mode
  test passed; all 41 coordinator tests passed, including inventory equivalence.
- `make workflow-gate` passed (116 host tests, 238 native files format-clean).
  Its staged checks saw an empty index, not a prepared commit.
- Rust formatting and whitespace checks passed. Interim plan-compliance review
  approved the harness correction and exact-inventory reruns.
- This does not prove native parity, completed assembly or hardware throughput.
  Item 0f, the full Rust gate and the complete native wrapper remain open.
- Final bounded-slice review: `platform_coverage_review` (Hume) independently
  reconstructed and strictly decoded all four corrected snapshots and returned
  interim PASS for the result/report. This is not Item 0f closure or commit
  authorization. Plan-bundle and final whitespace checks passed; no files were
  staged, committed or pushed for this unfinished item.
