# Native runtime and memory diagnosis

Current assignment: measure resource growth and evaluate the selected capture
clear-elision experiment. Parity implementation is paused for this pass. Preserve shared VM,
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
2. **Capture construction and bulk memory work.** The measured baseline clears a
   whole record, overwrites it, and copies it from temporary to retained storage; buffer
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

The owner/count measurement below selected clear elision as the first small
construction experiment, with unchanged release input and the complete Rust
artifact as correctness reference. A compact-capture experiment remains deferred.

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

The full measurement bundle is `/tmp/opforge-a6000-capture-accounting-v3-20261013`:
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

The corrected bundle now completes on the physical A6000 in **134 guest seconds**
(2m14s, one-second clock resolution), with fresh exit zero, exact complete Rust
output and valid telemetry. Host command time including connection is
133.909075042 seconds. Case digest:
`404fac74ec19b8b65497e0dcc8556e84d3e21f958eb7dbf94999714090063373`.
Receipt: `/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-result-5c03wy1h/result.json`.
The host-generated challenge/script, every captured input, bootstrap, package,
complete Hunk and both decoded records were independently rechecked against this
bundle. Its initial host-only status describes qualification before this run.

Preparation takes 98.18 instrumented seconds and assembly 34.60. Exclusive
binding/raw-record work is 53.02 seconds; other stages are I/O/other 8.40,
package setup 0.10, tokenization 8.80, expressions 10.38, finalization 8.33 and
discovery 9.15. This focused observer omits the previous nested timers, so
134 versus 191 seconds is a change of measurement mode, not an optimization.
The earlier 118-second release run also has slightly different source inputs;
neither comparison establishes isolated instrumentation overhead or a release gain.

| Retained capture component | Bytes | MiB |
|---|---:|---:|
| Headers, 111,292 records | 7,122,688 | 6.79 |
| Shared 20-byte tokens | 8,620,260 | 8.22 |
| Lexemes | 1,980,495 | 1.89 |
| Candidate plans | 5,236,852 | 4.99 |
| Padding | 178,409 | 0.17 |
| Total used | 23,138,704 | 22.07 |
| Total reserved | 31,944,704 | 30.46 |
| Capacity slack | 8,806,000 | 8.40 |

Headers and tokens are 68.0% of used capture storage. Slack is 27.6% of reserved
capacity. All 162 retained regions have identical ownership at initial replay
entry and immediately before release: no additional acquisition changes these
snapshots in this workload. Retained capacity peak including allocation overlap
is 32,010,240 bytes. Whole tracked heap peak is separately 47,077,256 bytes
(44.90 MiB); these peaks have different scopes and are not time-correlated owner
samples. Live bytes after preparation are 2,960,640, and after assembly 4,934,912.
All 110,472,408 allocated bytes are freed; cleanup live bytes and errors are zero.

| Observed memory work | Bytes |
|---|---:|
| Explicit record clearing, 111,387 constructions | 23,195,144 |
| Constructor outcome/payload copies | 19,006,444 |
| Temporary capture to retained region copies | 23,138,704 |
| Configuration scratch materialization | 738,927 |
| Semantic replay scratch materialization | 10,604,657 |
| All successful block-growth copies | 46,963,881 |
| Retained-region growth copies, subset of the preceding row | 31,668,772 |

The five disjoint copy categories total 100,452,613 bytes; the retained-growth
subset must not be added again. Construction payloads, retained append and global
growth account for 85,990,193 bytes passing through current byte-copy loops after
excluding the constructor's seven outcome-long copies. Scratch materialization
already uses long/word/tail copying. These are byte-work totals, not elapsed-time
attribution or counts of unique source data. Clearing excludes Exec allocation
clearing; the allocator still requests cleared allocations.

## First selected experiment: remove overwritten capture clearing

The first candidate is narrowly owned by `binary_capture.create`: stop clearing
the entire record before overwriting its header and payloads. Explicitly zero
`Record.Reserved` and both 0–3-byte alignment gaps. Retain the current record
format, shared TKVM ABI, allocation policy, copied payloads and failure-atomic
publication. Reused storage must not rely on the allocator's initial clearing.

The hypothesis is a runtime reduction from removing almost all 23,195,144
explicit clear-byte operations. Expected remaining zeroing is two bytes per
constructed record plus its padding, approximately 0.4 MB on this workload.
That is a work prediction, not a measured speedup. No heap reduction is expected.
Update the observer argument to count actual cleared bytes, not record extent.

Before editing, freeze the complete input/package/command and generate its live
Rust oracle. Compare baseline and candidate release executables on that same
input, separately from instrumented work/heap runs. Changing the assembler's own
source changes the workload: the fixed-input comparison and a new current-source
self-host qualification are separate cases, each with its own exact Rust output.

Focused validation must poison reused capture storage, exercise both alignment
gaps and all length residues including zero payloads, retain wide-token metadata,
and check invalid-record/allocation-failure behavior and unchanged publication.
The full comparison requires fresh completion, exit zero and exact bytes; count
composition/copies and allocation ownership must remain unchanged while actual
clear counts fall. Retain the change only after separate release timing shows
benefit without correctness, memory or code-quality regressions.

### Clear-elision implementation and separate release measurement

The constructor now writes the reserved word and clears only the two alignment
gaps. Copies, validation-before-publication, record layout and allocation policy
are unchanged. Constructor-local clear accounting uses gated macros with an
explicit counter output; release builds emit no accounting instructions.

The real constructor passes an independent byte-layout oracle on poisoned reused
storage in FS-UAE: all 16 lexeme/plan length residues, empty payloads, 64 token
rows with nonzero metadata, consecutive appends, unchanged published prefixes,
invalid token/line/count bounds, arithmetic overflow and reserve-limit rejection.
This is constructor proof, not a complete current-source self-host run.
The enabled observer/macro preservation probe and the fresh exact 29-byte
macro/include case also pass. The latter reports 1,328 actual cleared bytes for
267 constructions, valid complete ownership snapshots and zero capture ownership
after release. Five focused host checks, 41 A6000 runner/comparison tests and the
native engineering gate pass. These component counts do not replace full
fixed-workload measurements.

Release timing uses the unchanged 11,592-byte two-module control with 1,200 macro
calls and a fresh live Rust oracle, FS-UAE 68020/10 MiB, and telemetry disabled.
The same compiled test executable, package bytes and source generator supply
all four serial cases, ordered baseline/candidate/candidate/baseline.

| Image | START-to-DONE samples, seconds | Mean |
|---|---|---:|
| Baseline | 41.814091250, 41.802031541 | 41.808061396 |
| Clear elision | 41.320402000, 41.569497708 | 41.444949854 |

The mean decreases by **0.3631 seconds (0.87%)**. Both candidate samples are
faster, but this small control does not establish a full self-host speedup.
The release Hunk grows 28 bytes, from 706,888 to 706,916; linked reservation
grows from 740,660 to 740,688. No meaningful heap reduction is expected because
record extents and allocation policy are unchanged. Full-workload instrumented
heap/work have not yet been measured for this candidate. Release timing is
recorded below. Control receipt:
`/tmp/opforge-clear-release-control-20261013.json`.

The physical A6000 comparison deliberately uses a frozen 195-input baseline
manifest `fnv1a64:cadc24b1921a07ea` (2,585,677 bytes). Its fresh Rust-produced
oracle and baseline executable have the 706,888-byte SHA-256 recorded above.
The candidate executable SHA-256 is
`e197fc8e076ab5a706f2415c0bd6d18433ae4a7bc6bdb309900617a78da69b26`.
Both embed the same 454,890-byte m68020 package. The baseline native source root
is frozen at `/tmp/opforge-clear-baseline-native-20261013`.

The [comparison runner](../../scripts/performance/run_a6000_comparison.py)
validates both ordinary exports before constructing explicit benchmark cases.
Only the bootstrap differs in the first two runs; source/package/command/oracle
remain identical. Each case has a distinct identity-bound fresh challenge and
exact output proof. The original self-host bootstrap/oracle guard is unchanged.
The third run uses the candidate's own 195-input source manifest
`fnv1a64:1b1cb47fe4c81f08` (2,587,096 bytes), and its new 706,916-byte oracle.
That third run is the independent current-source self-host case. All three
physical A6000 runs now complete with fresh exit zero and exact complete output:

| Release case | Guest seconds | Output bytes | Host command seconds, including connection |
|---|---:|---:|---:|
| Baseline on frozen input | 117 | 706,888 | 117.844501458 |
| Clear elision on identical frozen input | 117 | 706,888 | 117.371201333 |
| Clear elision on its current source | 117 | 706,916 | 117.909336417 |

The fixed-input pair shows **no measurable full-build gain at one-second guest
resolution**. The host intervals differ by 0.4733 seconds, but include connection
overhead and are single samples; they do not establish a subsecond assembly
speedup. The small emulator control's 0.87% result is not a full-build claim.
Keep the bounded, correctness-tested clear elision as removal of redundant work;
its full-build speed benefit remains unproven. No heap reduction is claimed.

The comparison summary is
`/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-comparison-ng_p4baw/comparison.json`.
Baseline and candidate fixed-input case identities are respectively
`6c05a008f32ab2391497ed747784b66c6e5a79cfbea5b51efb1dfcd6c8372a06` and
`38bc4df412519b7605f800dd975c1e7c7ee64fceaa14c70b6d9b8d9025863ff2`.
The new current-source full self-host case is
`772db9dab2237fad4a77ceb52bc3961adb02673cef56dc28756151f345aa5ade`, with receipt
`/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-result-3tef8gza/result.json`.
Every staged, pre-run and captured input/bootstrap/package/script, regenerated
case identity, fresh challenge, explicit exit, complete output and decoded
duration was independently revalidated. Verification summary:
`/tmp/opforge-clear-hardware-verified-20261013.json`.
The bundle's original host-only status describes its qualification before this
hardware run; this exact release configuration is now qualified on the A6000.

Run the prepared three-case bundle from macOS Terminal:

```sh
sh /tmp/opforge-a6000-clear-elision-20261013/run.sh
```

The wrapper invokes the maintained comparison tool with the baseline and
candidate exports. Add `--dry-run` for local preparation only. Separate results
and `comparison.json` retain guest durations (one-second resolution), bootstrap
and workload identities, fresh completion and exact-output status. Transfers
are outside the guest timing window. A fixed-input candidate success does not
claim that the candidate assembled its own current source.

## Next experiment: native routine hotspot profiling

The assignment now prioritizes actual elapsed hotspots before another
optimization. Large byte-work totals did not establish a measurable full-build
gain for clearing. Binding/raw-record preparation remains the largest coarse
stage, but its 53.02 instrumented seconds do not identify the individual costly
routines. Use the [sampled observer](../architecture/native-instrumentation-framework.md#sampled-native-routine-hotspots)
to separate candidate acquisition, capture construction/append/growth,
replay/preview, packed writing, name/scope/import binding, expression compilation,
record processing, selected-plan binding, tokenization and scope lookup.

Keep the release implementation unchanged. Success requires identical release
bytes, passive register/CCR/stack behavior, correct nested/recursive samples,
fresh exact native output, complete error-free terminal accounting and bounded
observer overhead. Compare sampling periods/phases to detect unstable rankings;
drill into the hottest routine before selecting an optimization. Retain capture
ownership/copy counts alongside timings to assess memory opportunities.

The three host exports use identical source/package/command/oracle: 196 mapped
inputs, 2,590,506 bytes, manifest `fnv1a64:7c0d30514b87a2cd`, both bootstrap and
output embedding the unchanged 454,890-byte m68020 package. The release Hunk
remains exactly 706,916 bytes, SHA-256
`e197fc8e076ab5a706f2415c0bd6d18433ae4a7bc6bdb309900617a78da69b26`.

| Bootstrap | Image bytes | Measurement mode |
|---|---:|---|
| Light baseline | 719,628 | Existing phase/MEMD/capture accounting |
| Sampled hotspots | 721,572 | Same baseline plus period 128 / seed 0 |
| Sensitivity check | 721,572 | Same baseline plus period 256 / seed 37 |

Dense binding, tokenizer, template and input timers/counters remain disabled.
The new observer's fixed static state is 1,244 bytes and performs no allocations.
All three full hardware cases now complete with fresh zero exit, exact complete
706,916-byte Rust output and valid accounting. Host export and component tests
were not substitutes for those hardware runs.

The real observer probe checks nesting, recursion, own versus forced-child
samples, all slot phase/count rules, 64-bit clock carry, error freezing and
register/CCR/stack preservation. Native probes pass for periods 64/128 and
period 256 with seed 7. A separate fresh macro/include CLI run with the real
timer and period 128 produces exact 29-byte Rust output, valid MEMD/capture
records and a complete valid hotspot report (maximum observed depth 7).
Those small-case rankings are not full-self-host hotspot results.

The identical 11,592-byte/1,200-call control runs serially on FS-UAE
68020/10 MiB, with fresh exact output and valid MEMD/capture/hotspot records:

| Instrumented image | START-to-DONE seconds | Extra over light baseline |
|---|---:|---:|
| Light baseline | 43.017769417 | — |
| Period 128 / seed 0 | 44.848765834 | 1.830996417 (4.26%) |
| Period 256 / seed 37 | 44.788869291 | 1.771099874 (4.12%) |

These are one sample each, measuring observer overhead on this control, not
optimization or full-self-host overhead. Both profiles pass the strict decoder,
including sample-phase counts and matching MEMD E-clock frequency. All three
peak at 1,240,712 tracked heap bytes, allocate/free 3,145,864 bytes, end with
zero live ownership and retain the same 27,022 prepared source bytes. Linked
reservation grows from 755,532 to 758,516 bytes (+2,984 static code/storage).
Receipt with fresh timing, memory, image identities and decoded profiles:
`/tmp/opforge-hotspot-overhead-control-20261013.json`.
Three host profiler checks, three real observer configurations, the real-timer
CLI case, 138 performance-tool tests and the native engineering guards pass.
Fresh regeneration after naming the timer vector also matches the exported
period-128 bootstrap byte-for-byte.

### Completed A6000 hotspot measurement

| Full self-host mode | Guest seconds | Preparation / assembly seconds |
|---|---:|---:|
| Light baseline | 136 | 100.68 / 34.52 |
| Period 128 / seed 0 | 141 | 105.34 / 35.06 |
| Period 256 / seed 37 | 140 | 104.36 / 34.78 |

Added hotspot observation costs 5 seconds (3.68%) or 4 seconds (2.94%) over
the identical-input light baseline, at one-second resolution, one run each.
It changes no tracked ownership or byte-work totals. All three peak at
47,862,664 tracked heap bytes (45.65 MiB), allocate and free 112,277,720 bytes,
and finish with zero live ownership and no errors. The two profiles have
maximum nesting 7, no unfinished frames and valid sample counts/frequencies.

| Routine | Calls | Estimated exclusive seconds, period 128 | Period 256 |
|---|---:|---:|---:|
| Record processing | 118,516 | 24.22 | 25.05 |
| Replay orchestration | 111,650 | 18.08 | 18.32 |
| Packed writing | 119,660 | 15.86 | 15.41 |
| Scope lookup | 254,274 | 14.29 | 18.52 |
| Tokenization wrapper | 112,150 | 14.02 | 14.39 |
| Name binding | 266,648 | 13.02 | 13.08 |
| Candidate acquisition | 112,150 | 8.76 | 9.09 |
| Expression compilation | 68,126 | 8.19 | 7.92 |
| Capture construction | 112,150 | 4.22 | 4.09 |
| Retained append | 112,048 | 3.61 | 3.67 |
| Block growth | 38,406 | 2.88 | 3.51 |

These extrapolate independently selected samples of instrumented execution;
they are neither measured release totals nor additive phase components.
Exclusive means outside instrumented child boundaries; uninstrumented callees
remain in the parent. Record processing is the largest stable residual bucket.
Scope lookup is expensive in both profiles but varies about 30%; growth also
varies noticeably. Treat the rankings as targets for investigation, not precise
savings budgets. The tokenization wrapper includes work outside the coarse
tokenization stage, so its estimate must not replace the measured stage clock.
Replay's inclusive estimate is 66.46–67.13 seconds and includes its descendants;
it must not be added to their estimates.

Capture storage remains the largest measured RAM opportunity: 112,048 records,
23,356,036 used bytes (22.27 MiB), 32,731,136 reserved (31.21 MiB), with
9,375,100 slack (8.94 MiB). Headers and tokens occupy 15,841,032 bytes, about
67.8% of used capture. Acquisition and pre-release ownership are identical;
all owners reach zero after release. Actual clear writes total 404,997 bytes.
These counts are identical across all three modes. The increase from the earlier
195-input measurement is a changed source workload/capacity frontier; it is not
an increase caused by enabling the hotspot observer.

The active drill-down slice splits the residual `processRecord` cost across source
control, declaration/conditional/metadata handling, operand/scope handling,
data/expression preparation, publication and graph work. Inspect replay/packed
writing alongside it. Measure scope lookup hashing, chain traversal and string
comparison before choosing a hash-table change: pre-change code used 256 buckets
and folds lexical bytes during hashing/comparison. Also quantify blank/comment
and header-only record populations before proposing capture pruning
or packing. Eliminating redundant replay/binding work could improve both time
and storage, but the present profile does not prove a safe elimination rule.
Preserve shared VM ownership, contextual rebinding, iterator/macro hygiene,
forward declarations and diagnostic/listing provenance. No optimization is
selected from these extrapolated totals alone.

Hardware summary:
`/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-hotspots-unlk2kto/hotspots.json`.
Cases, in table order:
`886fcc6848dbb3c87d9ccf972753f0028e62b7ebcc9e062726c492c90f16d0cd`,
`a40027b33202ed5fab41ac2fcaf5a4547399f17fb822db2e79494aacb0ab9a0f`,
`7a65f015d0e93043b94ed3241c30754593adf5d5d72cbc8b42c3928178514f7f`.
Each staged/pre-run/captured source/bootstrap/package/script, case-bound fresh
challenge, explicit zero exit, whole Hunk, MEMD, capture/hotspot report and clock
was independently revalidated. Verification summary:
`/tmp/opforge-hotspot-hardware-verified-20261013.json`.
These exact instrumented full-self-host configurations are now qualified on
the A6000; their original host-only status describes qualification before these
runs. This establishes neither complete language/family parity nor the 2 MiB
memory target.

Run from macOS Terminal:

```sh
sh /tmp/opforge-a6000-drilldown-20261013/run.sh
```

The [launcher](../../scripts/performance/run_a6000_hotspots.py) validates every
export before connecting, discovers the current address with `ash -l`, and runs
the light baseline followed by both sample settings on `Development:`. A unique
discovered IPv4 address is required; `--host ADDRESS` is an explicit override. It requires exact equality of all non-bootstrap
input bytes, command and Rust oracle, with matching base instrumentation flags.
Each run has its own case identity, fresh challenge, explicit exit and complete
output proof. The current expanded profile additionally needs all 115 valid rows, matching
period/seed and E-clock frequency. Incremental `hotspots.json` retains completed
results if a later transport/run fails. Completed failures do not supply timing
claims. Transfers are outside guest durations (one-second resolution).

Extra time relative to the light baseline measures the added hotspot observer
on identical inputs. It is not a production speed change. Earlier 117-second
release and 134-second instrumented cases have slightly different source;
neither isolates this observer's overhead. Compare both sample rankings and
sample counts before treating scaled timings as actionable. Full-workload
memory ownership and sampled costs will guide the next optimization together.

The new drill-down extends the observer to 32 timing boundaries and 16 exact
work counters. It separately measures `processRecord` handlers and lookup
hashing/chain traversal. Exact lookup work counts cover probes, rejected
namespaces/lengths, eligible name comparisons and compared byte pairs. Inert
candidate counts distinguish retained zero-token captures (with their encoded
bytes), replay visits, header-only ordinary inputs/outputs and consumed controls.
No records are discarded, and header-only controls are not declared side-effect-free.
The [instrumentation contract](../architecture/native-instrumentation-framework.md#sampled-native-routine-hotspots)
defines every boundary and counter.

Validation: the actual native observer passed three period/seed preservation,
sampling, saturation/reset and error probes. A fresh real-native multi-file
macro/include case matched its 29-byte Rust oracle and passed all 115 decoded
rows plus exact lookup/capture accounting relationships. That reduced case
retained 18 zero-token records occupying 1,152 encoded bytes, replayed 18 such
records and processed 348 ordinary/expanded records. Its 529 lookups hashed
6,957 bytes and visited 429 entries: 2 namespace rejects, 127 length rejects and
300 name comparisons (3,721 byte pairs), finishing with 297 hits and 232 misses.
These are component-validation facts, not full self-host workload estimates.

An unchanged 11,592-byte active-include/macro control passed fresh exact native
output in all three modes on the same `68020-10m` emulator profile. The light
baseline took 43.311 s; drill-down128 took 48.687 s (+5.376 s, +12.41%) and
256 took 48.665 s (+5.354 s, +12.36%), one run each. Both expanded profiles
passed strict decoding and exact lookup count reconciliation. This is added
**observer overhead**, not a release regression or an isolated sampling-only
cost: per-byte exact counters run at both densities. Heap peak stayed 1,240,712
bytes, allocated/freed 3,145,864 each, packed source 27,022 and cleanup zero.
Report: `/tmp/opforge-drill-overhead-control-20261013.json`; decoded verification:
`/tmp/opforge-drill-control-verified-20261013.json`. The full A6000 measurements below independently measure observer overhead.

The A6000 cases freeze the same 196 input files (2,595,817 bytes), package,
command and fresh 706,916-byte release Rust oracle, source digest
`fnv1a64:7eec9ace461f7b6b`. The light-baseline bootstrap and complete release
oracle are byte-for-byte identical to the preceding exports; source changes
add probe definitions/sites, so old and new full source-run timings are not an
unchanged-input production comparison. Baseline Hunk is 719,628 bytes / 755,532
linked bytes; both expanded profiles are 723,992 / 760,968. Fixed observer state
increases by 512 to 1,756 bytes, with no observer heap allocation. Release code,
data and BSS are unchanged.

The baseline and profiles (period128/seed0 and period256/seed37) live at
`/tmp/opforge-a6000-drill-baseline-20261013`,
`/tmp/opforge-a6000-drill-128-20261013` and
`/tmp/opforge-a6000-drill-256-20261013`. The source snapshot remains at
`/tmp/opforge-drill-source-20261013` so later repository edits do not change their
inputs. All three exact configurations now complete full self-host assembly
on the physical `V4-A6000 (UniCorn)`, discovered at `192.168.2.4` by `ash -l`.
Each reports fresh completion, exit zero and exact complete Rust Hunk output.
The staged, round-tripped and captured source/bootstrap/package/script bytes,
case-bound markers, full output, dates and independently decoded telemetry were
revalidated against the exports. The original `native_validation=not_run`
manifest records their pre-hardware status; the hardware receipts supply the new
qualification. This qualifies these cases, not full language parity or 2 MiB fit.

| Full self-host mode | Guest duration | Preparation / assembly | Extra observer time |
|---|---:|---:|---:|
| Light MEMD+capture baseline | 136 s | 101.36 / 34.56 s | — |
| Drill-down128 / seed0 | 154 s | 117.80 / 35.26 s | +18 s (+13.24%) |
| Drill-down256 / seed37 | 152 s | 116.28 / 35.12 s | +16 s (+11.76%) |

One run per mode, one-second guest duration resolution; transfers are excluded.
These are added observer costs on identical inputs, not production speed changes.
All three modes have identical heap peak 48,124,808 bytes (45.90 MiB), allocated
and freed 113,107,616 each, packed source 2,028,931 and cleanup zero. Retained
capture is 23,665,468 used / 32,993,280 reserved, with 9,327,812 slack. The peak
increase from the previous source workload is 262,144 bytes; all modes agree,
so enabling drill-down did not add tracked heap ownership.

The exact work counts agree between both sampling configurations:

- 256,738 lookups visit 17,080,007 entries, **66.53 probes per lookup**.
- 16,674,463 entries (97.63%) fail the length check; 639 fail namespace.
  The remaining 404,905 reach name comparison, reading 8,940,352 byte pairs.
  Hashing reads 9,740,928 bytes. Lookups finish with 156,103 hits / 100,635 misses.
- 8,763 zero-token retained captures occupy 560,832 bytes (547.69 KiB):
  7.75% of records, only 2.37% of used capture. Replay visits 8,365 of them.
- Of 120,346 `processRecord` calls, 27,950 ordinary inputs and 40,429 published
  outputs are header-only; conditional handling consumes 8,086 controls.
  Empty publication does not prove a record had no semantic effect.

Sample estimates place hashing around 7.19–7.55 s and chain traversal around
18.71–23.92 s **in the instrumented execution**. Chain ranking varies about 28%.
`processRecord` exclusive estimates agree near 45.9 s, but selected parents
force timing of many direct children. Clock/wrapper margins outside the child
intervals remain in the parent and are extrapolated to all calls. This systematic
inflation is not removed by period/seed agreement; the value cannot establish
45.9 production seconds or a regression against the earlier coarse profile.
Short handlers' estimates have the same observer-margin concern. Prefer exact
work counts for choosing this next experiment.

Recommended first optimization: a bounded, centrally defined lookup bucket-count
experiment (256 baseline, 1,024 and possibly 4,096), retaining identical name
folding, comparison, numeric identities and word links. Raw bucket storage would
increase by 1,536 or 7,680 bytes per binder; measure actual heap/allocation effects.
Test unchanged-input release timing separately from exact work counters, including
collisions, namespaces, hygiene, imports and forward bindings. The experiment
must reduce actual candidate visits and retain exact output before integration.
Zero-token capture pruning has a smaller direct RAM budget (548 KiB) and needs
provenance/control-effect tests; header-only publication is not a pruning rule.
No optimization has been implemented from these results.

Hardware summary:
`/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-hotspots-49hi4wwb/hotspots.json`.
Cases in table order: `27dd2bbb238bbeb631dc06e60887641d7164a84cc004fa94c86dc4f2456f9f11`,
`f45bf080ef34b14ea1def065b556e66bfd395f8e5c086768d22dcb1873e0d957`,
`1d2ec057c822df891a7db77af1bffb912eb508370747f3b98a442a2d6b8db257`.
Independent verification: `/tmp/opforge-drill-hardware-verified-20261013.json`.

### 1,024 symbol hash buckets

The approved experiment increases only the generic native symbol binder's table
from 256 to 1,024 word buckets. `binary_scope_layout` owns the count, mask and
byte extent; initialization and lookup consume those constants. Hash folding,
full-name comparison, namespace/visibility rules and numeric identities are
unchanged. The separate eight-bit template-candidate key remains unchanged.
Hypothesis: fewer collision-chain visits improve preparation at a small scratch
storage cost. Stop on parity failures or disproportionate memory/runtime growth;
keep the frozen 256-bucket code as the comparative reference. Full-workload
qualification and integration judgment await a passing repaired A6000 comparison.

The initial 11,592-byte active-include/macro workload and live Rust oracle ran on
both release executables using the same `68020-10m` FS-UAE profile. Separate
light-accounting and hotspot-count runs used the same input bytes. Every case
completed with fresh zero exit and exact output.

| Measurement | 256 buckets | 1,024 buckets | Difference |
|---|---:|---:|---:|
| Release start-to-DONE | 41.484964 s | 41.241746 s | −0.243218 s (−0.59%) |
| Tracked heap peak | 1,240,712 B | 1,243,784 B | +3,072 B |
| Total allocated / freed | 3,145,864 B each | 3,148,936 B each | +3,072 B each |
| Packed source | 27,022 B | 27,022 B | 0 |
| Cleanup ownership | 0 | 0 | 0 |
| Exact lookup chain probes | 22,901 | 9,597 | −58.09% |
| Name comparisons | 11,922 | 5,694 | −52.24% |
| Compared byte pairs | 81,590 | 43,464 | −46.73% |

One runtime run per executable: the small difference does not establish a
meaningful release speedup. The unchanged counts (4,842 lookups, 53,181 hashed
bytes, 2,412 hits, 2,430 misses and 3,611 processed records) establish equivalent
logical work with fewer candidates. This small control averaged 4.73 probes per
lookup before the change; the full measured self-host averaged 66.53, so its
runtime effect still needs an identical-input physical comparison. Enabled
observer times are excluded from the release speed claim. Heap increase is
measured, not inferred solely from the raw +1,536 bytes per binder. Release Hunk
size and linked reservation remain 706,916 / 740,688 bytes in both builds.

Focused correctness: 1,030 names cover all upper buckets and same-/different-length
collision chains, forward and mixed-case reads; a separate native case covers
imports, namespace shadowing and same-spelling value/template facets. Both match
their literal/live Rust oracles (2,060 and 7 bytes). The native scope identity
growth, word limit and register/CCR checks also pass. Python comparison tooling
validates paired release/accounting exports before transfer, retains each actual
bootstrap identity and separate output oracle, and updates its summary after
every completed run. These focused checks do not qualify the full self-host.
Local measurements: `/tmp/opforge-buckets-control-20261013.json`; independently
decoded count/memory comparison: `/tmp/opforge-buckets-control-verified-20261013.json`.

Run the new A6000 comparison from macOS Terminal:

```sh
sh /tmp/opforge-a6000-buckets-fixed-20261013/run.sh
```

It discovers the current address with `ash -l`, uses `Development:` and performs
five serial fresh runs: release256/release1024 on identical baseline source,
package, command and Rust oracle; light accounting256/accounting1024 on that
same workload; then release1024 on its own current source with its own Rust
oracle. Release timing and heap/capture comparisons are reported separately.
Only the final case can qualify the new executable assembling its current source.
Each run requires complete fresh protocol, explicit zero exit and whole-output
parity; partial captures/timeouts cannot provide a success or timing conclusion.
All five staged plans passed a no-network dry-run. Actual A6000 execution then
qualified the baseline release (118 s, 706,916 bytes) and baseline light accounting
(136 s, tracked heap peak 48,124,808 bytes; total allocated/freed 113,107,616 bytes
each, final ownership zero). Both original candidate fixed-workload cases failed
with exit 20 at `binary_values.asm` line 9, after 35 s release / 43 s accounting.
These partial runs establish neither a runtime gain nor a full-workload heap
difference. Their source/package/bootstrap round trips, fresh markers, case
identities and complete result captures have been independently verified.
The final current-source run has no captured completion; it does not qualify
self-hosting and must not be overlapped with a retry while its guest may be active.

Diagnosis: `binary_imports.findDeclared` reads the shared scope hash table but
retained `andi.l #255`. `experimental.amigaos.binary_memory.Block.Used` is inserted
into bucket 768 yet searched in bucket 0. The struct base itself occupies bucket
65, explaining why its field alone fails. Replace this consumer's mask with
`layout.BUCKET_MASK`; the separate template hash remains eight-bit. A 417-byte
regression uses the actual qualified type/member names in imported `.res` fields,
including a mixed-case alias, and checks explicit offsets `[0,12,24,8]` against
Rust. The original candidate fails this fresh native case with exit 20; the
repaired release image completes with exact bytes. This proves the focused repair,
not the full self-host. Existing full hardware qualification remains baseline-only.
The evidence audit is `/tmp/opforge-buckets-hardware-verified-20261013.json`.

Repeating that identical focused control with the repaired image gives release
41.510722 s baseline / 41.060186 s repaired candidate (−0.450537 s, −1.09%, one
pair; insufficient to establish a reliable runtime gain). Separate light
accounting still gives heap peak 1,240,712 / 1,243,784 bytes (+3,072), allocated
and freed 3,145,864 / 3,148,936 bytes each, packed source 27,022 bytes and final
ownership zero. All four fresh native runs match Rust exactly; release and
accounting Hunk extents remain unchanged. Report:
`/tmp/opforge-bucket-fixed-control-20261013.json`. Both existing imported-struct
reservation tests also pass, including reversed source input order. The corrected
five-run hardware plan passes a no-network dry-run; its full cases remain pending.

Corrected release exports: `/tmp/opforge-buckets-baseline-release-20261013` and
`/tmp/opforge-buckets-fixed-release-20261013`. Accounting exports:
`/tmp/opforge-a6000-drill-baseline-20261013` and
`/tmp/opforge-buckets-fixed-memory-20261013`. Baseline source remains at
`/tmp/opforge-drill-source-20261013` (196 files, 2,595,817 bytes,
`fnv1a64:7eec9ace461f7b6b`); repaired candidate source at
`/tmp/opforge-buckets-fixed-source-20261013` (196 files, 2,596,097 bytes,
`fnv1a64:20a2106b4ce5f83d`). Both package configurations embed only m68020 and
use the same package bytes. Baseline output SHA-256 is
`e197fc8e076ab5a706f2415c0bd6d18433ae4a7bc6bdb309900617a78da69b26`;
candidate current-source output SHA-256 is
`0f6af5842c01df66625583448260da3b4db35422f6d0cbbd5d6d513b23cac918`.
The first four runs use the baseline output, making the bootstrap change the
only execution-variable for each timing/accounting pair.

Wide bulk copying remains a possible small candidate, followed by capacity policy and
a compact capture-format experiment. Growth tightening must account for extra
copies; merely reserving exact size on every append could trade slack for much
more work. Format compaction has the larger structural memory opportunity in the
headers and tokens but requires a broader producer/validator/consumer change.

Preserve failure-atomic allocation, stable numeric identity, longest-owner lookup,
active branch/loop semantics, include provenance and relocation/layout behavior.
Each selected optimization needs its own unchanged-input correctness comparison,
release timing, separate heap measurement and code-quality review. This pass does
not claim complete language/family parity or fit within the 2 MiB target.
