# Native runtime and memory diagnosis

Current assignment: measure and analyse resource growth before selecting further
optimizations. Parity implementation is paused for this pass. Preserve shared VM,
numeric source and package boundaries; no CPU-specific shortcuts in generic code.

## Fixed workload and baseline

Use source `7a903cf8`, manifest `fnv1a64:ec940cb60e2202f6`: 194 mapped inputs,
2,580,296 bytes including the unchanged 454,890-byte m68020 package. Bootstrap
and output both embed that package. Complete expected Hunk is 706,888 bytes,
SHA-256 `5d5e6f10c15dacf4b8b3840119380032755551d51eafe897bcb668985f4f19ee`.

- Release FS-UAE 68020/74 MiB: 3,512.267856959 seconds (58m32.27s), fresh exit
  zero and exact complete live Rust output.
- Release A6000: 118 seconds, fresh exit zero and the same complete output;
  guest resolution is one second, transfer excluded. Hardware receipt and exact
  case identity are in the [self-host evidence](compact-frontend-vm-boundary.md#physical-a6000-execution).
- Release image: 706,888 bytes; linked reservation: 740,660 bytes. Neither is
  peak process memory.

Reuse this current release baseline. A separate full instrumented run uses the
identical source/package/command/oracle, with allocation tracking, phase clocks,
binding samples, template work counts and input-reader counts. Dense tokenizer
opcode telemetry is disabled. Instrumented time measures attribution and overhead,
not release speed. The instrumented A6000 run now completes with fresh exit zero,
exact complete output and valid telemetry. Its case digest is
`3538c2ea566915b2b5d01c5a6d0f1e1e36cb164219815bcdb8314f0383154baa`.
Receipt: `/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-result-4i2o6ro3/result.json`.
The equivalent emulator diagnostic was cancelled after this hardware completion;
it supplies no completed emulator measurement. Summary:
`/tmp/opforge-native-performance-baseline-20261013.json`.

## Measured costs

Instrumented overall time is 191 seconds versus 118 release seconds: about 62%
additional elapsed time. Guest phase clocks report 155.38 seconds preparation and
35.14 seconds assembly. Do not extrapolate release phase times by scaling these
observations: instrumentation overhead is distributed unevenly.

| Exclusive preparation responsibility | Instrumented seconds |
|---|---:|
| Binding and raw records | 100.79 |
| Source I/O and other | 17.78 |
| Expression preparation | 9.99 |
| Module discovery | 9.44 |
| Tokenization | 9.03 |
| Runtime finalization | 8.25 |
| Package setup | 0.11 |

Binding/raw records account for about 65% of instrumented preparation. Nested
observations include record finalization/append (20.03 seconds), packed
writing/binding (18.36) and conditionals/scopes/imports (15.76). They are already
within the preparation stages and must not be added to the table. Binding sampling
estimates 15.14 seconds of name binding; that estimate is nested too.

| Ownership boundary | Tracked live bytes |
|---|---:|
| Last acquisition sample | 33,588,976 |
| Replay complete, before finalization | 12,957,528 |
| Parameter/export boundary | 13,161,560 |
| Preparation released / assembly entry | 2,895,104 |
| Assembly complete | 4,869,376 |
| Global tracked peak | 46,880,904 |

The peak is 44.71 MiB, versus 2.76 MiB after preparation teardown and 4.64 MiB
at assembly completion. The exact peak falls between snapshots; current telemetry
cannot assign it to a particular owner or distinguish growth overlap from replay
overlap. All 109,772,320 allocated bytes are freed, with zero live storage,
no allocation failures and no profiling errors. This points to transient
preparation ownership rather than a retained leak. These are tracked allocations
from an instrumented build, not total process RAM or an exact release heap peak.

Runtime prefix is 432,626 bytes; prepared packed records are 2,008,891 bytes.
67,723 expressions compile into 264,023 program bytes, with 174,723 evaluations.
Input collection reads 2,626,150 bytes through 1,007 reads; mapped input size and
actual bytes read are different quantities.

## Growth observations and remaining attribution

Earlier successful release A6000 receipts show a workload trend, not isolated
change costs: 103 files / 1,509,170 input bytes took 35 seconds; 121 files /
1,780,288 bytes took 49 seconds; the current 194 files / 2,580,296 bytes took
118 seconds. Input totals include assets, and implementations/capabilities differ.
Growth in time exceeds growth in input bytes across these observations, which
supports investigating repeated work; it does not establish a specific cause.

1. Report preparation, discovery, binding, tokenization, expression preparation,
   finalization and assembly costs; nested observations must not be added to the
   exclusive stage totals.
2. Separate tracked heap live/peak/cumulative allocations from executable/static
   reservation. Identify the peak phase and preparation-to-assembly lifetime drop.
3. Examine captured source, composed names, macro/binding plans, numeric execution
   state and output ownership. Distinguish used bytes, capacity slack, duplicate
   owners and transient growth overlap. Existing aggregate telemetry cannot assign
   peak bytes or copy traffic to individual owners.
4. Compare earlier identified measurements only where input and mode match.
   Smaller older projects and failures after about 40 minutes are not full-build
   baselines. Normalized trends can guide investigation but cannot isolate causes.
5. Rank candidates by measured importance, expected runtime/RAM benefit, code
   complexity and correctness risk. Identify missing evidence before choosing a
   change; use focused representative controls for subsequent before/after runs.

## Ranked candidates

1. **Capture representation and lifetime.** Each retained acquisition line has a
   64-byte header, 20-byte shared TKVM tokens, lexemes and copied candidate plans.
   Some spellings occur in both lexemes and plans. The scheduler retains these
   regions through replay. Measure total used/capacity and header/token/lexeme/plan
   contributions first. A compact acquisition format inflated into existing TKVM
   scratch during replay could lower memory without changing the shared VM ABI.
   Preserve columns, adjacency, wide-number metadata, macro spelling and origins.
2. **Capture construction and bulk memory work.** Construction clears a whole
   record, overwrites it, and copies it from temporary to retained storage; buffer
   growth copies used bytes byte-by-byte. Removing overwritten clears, avoiding
   redundant copies or using suitable bulk-copy operations are smaller candidates
   than a format redesign. Measure cleared/copied/growth bytes, keep padding
   deterministic and preserve overlap, alignment and failure-atomic behavior.
3. **Candidate acquisition work.** Telemetry reports 221,240 descriptor VM runs,
   consistent with unconditional call/header acquisition for 110,620 lines.
   Consider package-authored shared prefix work or eligibility filtering after
   measuring match/no-match costs. Preserve deferred failures until contextual
   replay selects a role; never recreate grammar in native shortcuts.
4. **Binding/finalization owners.** Measure composed spelling, macro/binding plan
   arenas and exported copies individually before choosing ownership transfer,
   early retirement or deduplication. Preserve visibility, longest declaration
   precedence, inactive alternatives, cursor validity and installed-owner checks.
5. **Discovery and assembly traversal.** Discovery is 9.44 instrumented seconds;
   assembly is 35.14. Reader and pass/layout counters should precede changes to
   scanning or reuse. Actual branch, mutable-symbol and relaxation semantics
   continue to require evaluation.

Broad template-dispatch caching is lower priority than the initial audit suggested:
dispatch measures 4.63 seconds and only 462 invocations occur. Searches are a
minority of role/line calls. Zero physical string-plan captures does not mean
fragment planning is unused: retained captured plans bind through another route.

Recommended next slice: bounded owner usage/capacity and construction/copy counts
at acquisition end, replay overlap and capture release. Use those measurements
to select one small construction change or a compact-capture experiment, with
unchanged release input and the complete Rust artifact as correctness reference.
No optimization has been implemented in this diagnostic pass.

## Capture measurement checkpoint

The gated 272-byte capture observer now reports retained header/token/lexeme/plan
composition, used versus reserved storage at replay entry and before release,
explicit cleared bytes, construction/append/materialization copies, global block
growth copies and retained-capacity peak including growth overlap. MEMD remains
2280 bytes; see the [instrumentation schema](../architecture/native-instrumentation-framework.md#bounded-capture-ownership-accounting).
Active expanded includes can acquire additional captures after the initial
snapshot, so the pre-release snapshot describes the final retained frontier.

The focused multi-file macro/include case completes in FS-UAE with fresh exit
zero, exact 29-byte live Rust output, zero accounting errors, all three boundaries
and zero capture ownership after release. The independent real observer probe
preserves all registers, CCR and stack depth. Four host tests and the decoder/
hardware-runner checks also pass. These are component proofs, not a full self-host
measurement.

The full measurement candidate is `/tmp/opforge-a6000-capture-accounting-v3-20261013`:
195 mapped inputs, 2,585,677 bytes, manifest `fnv1a64:cadc24b1921a07ea`.
Its 719,588-byte bootstrap enables only contracts, memory accounting, preparation
progress, capture accounting and local telemetry export; hot nested timing probes
are omitted. Its uninstrumented 706,888-byte oracle is byte-for-byte identical to
the baseline above. The measurement source contains the new gated observer/macros;
elapsed time is not an isolated release before/after comparison.

Run from macOS Terminal:

```sh
python3 scripts/performance/run_a6000_selfhost.py --bundle /tmp/opforge-a6000-capture-accounting-v3-20261013
```

The runner requires valid capture reporting when the capture flag is enabled and
adds `capture_ownership` to its result. Independently decode captured stdout with:

```sh
python3 scripts/performance/capture_telemetry.py /absolute/captured/assembly.stdout
```

The first full emulator attempt and second hardware candidate rejected at physical
file completion during acquisition. A reduced source also failed through the
previously qualified release executable, isolating source classification from
observer effects. Constants, import preamble, empty macro bodies and the first
complete macro all passed fresh native comparisons. The cause was the new
`CAPTURE_APPEND` formal named `block`: `lea .block,a0` was classified as a labeled
`.block`, opening an extra acquisition scope. Renaming it to `arena` makes the
complete include pass through the old executable and the current instrumented
control. This avoids the classifier gap; it does not repair that general gap.

Snapshot identities also now use always-available local constants so call-site
arguments do not depend on gated imports. The third bootstrap is byte-identical
to the earlier instrumented candidates; release output remains exact. Failed
attempts supply partial acquisition counts only, not full workload measurements
or successful self-host proof. Captures remain outside the tree at
`/tmp/opforge-capture-full-fsuae-capture-20261013` and
`/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-result-ddiej4_s`.

Full-workload ownership/copy results are pending from the corrected hardware
candidate. Select the first optimization only after those results; do not infer
its benefit from the small control or failed acquisition.

Preserve failure-atomic allocation, stable numeric identity, longest-owner lookup,
active branch/loop semantics, include provenance and relocation/layout behavior.
Each selected optimization needs its own unchanged-input correctness comparison,
release timing, separate heap measurement and code-quality review. This pass does
not claim complete language/family parity or fit within the 2 MiB target.
