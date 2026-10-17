# Native runtime and memory diagnosis

Status, 2026-10-17: parity remains paused. Validated identity layouts reuse their
packed buffer and reduce identical disk-backed full-build peak tracked heap by
**2.07%**. The initially observed 2.40% release slowdown did **not** reproduce in
controlled follow-up runs. Materialization itself saves about 76 ms; no reproducible
whole-build runtime change is established. Bounded capture, chunked storage and
scaled symbol buckets remain maintained; cache prototypes are removed. Structural
scan, early publication and `.ofm` remain
[design work requiring review](native-fragment-preparation.md).

## Measurement contract

Compare each engine change on identical frozen source/package bytes and commands,
with a newly generated live Rust oracle. Require a fresh native challenge,
START/DONE, exit zero and exact complete output. Compare tracked heap and exact
work separately from uninstrumented runtime. Instrumented seconds are attribution,
not release speed. Two timing pairs below about 2% are inconclusive here; timing
noise is about two seconds. A marginal result above 2% is not automatically a
confirmed effect either; require reproducible controls before assigning a cause.
Reject changes below about 1% of their target resource unless they are a
documented prerequisite. No new observer framework was added.

All new runs use the expanded-memory FS-UAE template, a 64 KiB guest stack and
serial execution. They are neither A6000 timings nor 2 MiB qualification. Tracked
heap excludes the executable, stack, OS and untracked allocations; linked static
reservation is reported separately. Do not compare absolute times across emulator
profiles or changing source snapshots.

## Reuse an already ordered record stream

Expected saving: avoid a 2 MiB replacement allocation and a 2,032,832-byte record
copy on the frozen full workload. The initial estimate of a similar whole-build
peak reduction was too optimistic: removing one overlap exposes a different peak.

`binary_ordered_records.materialize` retains the packed buffer only after normal
span/record validation proves every ordered start equals the preceding cumulative
length and the final length equals the complete stream. Empty modules remain
valid. `RecordLimit` still applies. Origins and module starts use the same metadata
loop; actual reorders and partial selections retain the existing copying path.
No option or alternate preparation implementation was added. The pre-change
executable and `5a7b91e4` remain the reference.

Same 214 files / 2,648,242 source bytes and package bytes on both engines, with
workload SHA-256
`eae5102bd21b3863105a7f550912193dff8e52c8ba2d6ff11b498392b7dca480`.
Both use `--capture-dir Work:`. Every run builds a fresh live Rust oracle and
matches all 708,216 output bytes. Memory images use the same five gates.

| Resource | Copying reference | Identity reuse |
|---|---:|---:|
| Peak tracked heap | 11,839,624 B | 11,594,504 B |
| Allocated/freed capacities, each | 94,180,904 B | 92,083,752 B |
| Prepared live heap | 2,969,088 B | 2,969,088 B |
| Assembly live heap | 4,943,360 B | 4,943,360 B |
| Captured records / logical bytes | 116,855 / 23,852,392 B | unchanged |
| Expression compilations / evaluations | 69,528 / 176,850 | unchanged |

Exact ordered counters show one successful identity reuse, zero actual record-copy
bytes, 2,032,832 avoided copy bytes and 2,097,152 avoided replacement capacity.
Generic allocation counts fall by one; capture IO and language work are unchanged.
All tracked storage is freed with no observer errors. Peak falls only **245,120 B**
(**2.07%**): another allocation overlap now dominates. Prepared/assembly residency
is unchanged; this is not a deferred-processing gain.

The initial two release pairs averaged 102.109 s copying versus 104.559 s reuse
(+2.450 s / 2.40%). Follow-up investigation did not reproduce that regression:

| Fresh uninstrumented run | Seconds |
|---|---:|
| Original pre-change executable | 102.010 |
| Current executable, reuse | 100.998 |
| Current executable, forced copy | 100.539 |

The forced-copy control differs from the current release executable in exactly
one initializer byte, verified by successful source assembly and complete binary
comparison: it forces the existing allocation/copy/replacement path while retaining
all current checks, compiled layout and image size. Its 0.460 s / 0.46% difference
from reuse is within timing noise. The current executable also runs faster than
the fresh original reference here. These single fresh runs establish neither a
new whole-build speedup nor the earlier claimed cost.

To isolate work, temporary diagnostic sources reuse the existing phase/detail
clock APIs at just three finalization boundaries; ordinary per-record telemetry
remains disabled. Matched copy/reuse diagnostic images again differ in one byte.
Each scope is entered once with zero profiling errors:

| Coarse diagnostic phase | Forced copy | Reuse |
|---|---:|---:|
| Other preparation | 62.1224 s | 62.0354 s |
| Graph order and completion binding | 2.4427 s | 2.4454 s |
| Ordered materialization | 100.286 ms | 24.703 ms |
| Postorder index/export/disposal/prefix work | 394.558 ms | 394.429 ms |
| Assembly | 36.86 s | 36.92 s |
| All preparation (DOS clock) | 65.06 s | 64.90 s |

Materialization saves **75.583 ms**; postorder work is effectively identical and
assembly differs by only 0.06 s. There is no measured downstream penalty matching
the earlier 2.45-second gap. The local saving is only about 0.074% of a 102-second
build, too small for these whole-build timings to resolve reliably. The initial
difference remains unexplained variation, not a confirmed copy-removal regression.
Do not assert an OS, memory-bank, cache or code-layout cause without evidence.
No A6000 timing is available.

Diagnostic phase images are not release benchmarks or new memory measurements.
They retain only CLOCK/PHASE/SAVE and six coarse detail calls from the approved
framework; their ordinary memory/work counters are deliberately unmeasured.
Temporary detail IDs 0/1/2 mean order-completion/materialization/postorder, not the
standard binding-detail labels. Other preparation is the residual of the complete
preparation clock and these scopes, including CLI metadata setup. All five fresh
native investigation runs match the complete live Rust 708,216-byte output on the
same frozen source/package bytes. No production switch, observer framework or
implementation change was introduced by the investigation.

Decision: retain automatic validated reuse for its memory saving above the 1%
gate and lower allocation/copy work. Correct the earlier runtime-cost claim:
there is no established whole-build speedup or slowdown from this change.
Release image / linked reservation grows by only 52 B, to 710,392 / 744,828 B.
The module and macro-heavy controls keep peaks of 608,768 and 2,282,424 B: avoided
copies do not reduce every workload's peak. Their BIN/Hunk outputs match live Rust.
The native component probe covers identity plus empty modules, actual reorder,
partial selection, malformed input, identity-limit rejection and preserved
ownership on failure. Disabled macros emit zero bytes with undefined operands.

Further memory work must address the remaining peak, not extrapolate another
2 MiB saving from this counter. Structural publication still requires review.

## Remaining peak and proposed workspace retirement

Investigation only, 2026-10-17: no lifetime or production implementation changed.
A temporary bootstrap uses the existing passive progress interface to identify
new allocation high-water marks and snapshot named block capacities at replay
span/finalization boundaries. No new telemetry format, framework or maintained
switch was added. The diagnostic source and receipts are outside the working tree
at `/tmp/opforge-peak-ownership-20261017`; they are not repository dependencies.

The identical frozen 214-file workload completes with fresh START/DONE, exit zero,
no scratch-file leaks and all 708,216 bytes matching a newly assembled live Rust
oracle. Tracked allocations/frees remain 92,083,752 B each, with zero accounting
errors. Peak remains exactly **11,594,504 B**. Diagnostic elapsed time includes
extra console output and is not a release-performance measurement.

The global peak occurs during semantic replay when `Records` grows from 1 MiB to
2 MiB. The allocator preserves the old records until the copy completes. Other
new high-water marks identify growth of lexical entries, names and the shared
template/call-plan arena. Major capacities surrounding the global peak are:

| Owner | Capacity |
|---|---:|
| Prepared records, old allocation being replaced | 1,048,576 B |
| Prepared records, replacement | 2,097,152 B |
| Lexical identity entries | 2,097,152 B |
| Source-name arena | 2,097,152 B |
| Shared template/call plans | 1,048,576 B |
| Known compile-time values | 524,288 B |
| Template definitions | 262,144 B |

This is a major-owner list, not a complete heap sum. Descriptor snapshots occur
at span boundaries; the allocation event identifies the exact peak and its owner.
The large semantic tables still have live consumers. The 8.6 MB difference between
peak and prepared residency is not all dead storage that can simply be freed.

**Proposed next change, not implemented:** retire finished preparation workspaces
at the transition to semantic replay:

- Allocate the 41,216 B I/O/line/output area separately from initial frontend
  scratch from the outset. After configuration seeding and `ConfigFront.finish`,
  release the 92,336 B configuration scratch. This avoids rebasing current and
  suspended include pointers; the semantic frontend retains its own scratch.
- Release candidate paths (65,536 B), the declaration index (21,512 B) and discovery
  scratch (4,400 B) after the last configuration/discovery consumer, before
  captured semantic replay. Preserve origin filenames, search roots and all
  storage used by active/expanded includes. Preserve the direct streaming path,
  failure diagnostics and idempotent cleanup.

These capacities are measured live at the peak. Combined expected saving is
**183,784 B / 1.585%**, predicting about **11,410,720 B** peak on this frozen input
if no other overlap dominates. Initial combined scratch capacity is unchanged;
one additional I/O allocation replaces shared ownership, with no new source reads,
disk transfers or per-record work. No runtime gain is predicted. Neither part
alone meets the 1% target-resource gate, so evaluate the coherent phase-retirement
change together, against the existing executable, before retaining it. Qualify
late/expanded includes, configuration seeding, discovery/failure diagnostics and
cleanup as well as exact full output, heap and separate release timing.

A larger alternative is disk staging during large record-buffer growth: fully
save the old records, release their RAM, allocate/reload the replacement and only
then publish it. The identified growth offers an upper bound near 1 MiB / 9% peak
saving, before other overlaps and staging costs. It adds roughly two transfers
of the old used bytes and requires a separate private scratch context or an
explicitly owned backing range. After releasing old RAM, an allocation/read error
must abort preparation cleanly; it cannot promise the generic allocator's existing
failure-preserves-old-block contract. Defer this more consequential option until
the smaller retirement proposal is reviewed; neither proposal is implemented.

Even the smaller predicted peak is about 10.88 MiB / 5.44 times the entire 2 MiB
target, excluding executable, stack and OS. It does not establish low-memory fit
or the structural module-publication design.

## Bounded disk-backed capture

Expected saving: replace about 24 MB of resident capture with at most 1 MiB of
payload, including the writer and all pinned readers. Target at least 25% lower
whole-build tracked peak, preserving the eager language pipeline and record bytes.
The original resident executable and the current resident policy are references.

Use the experimental native option `--capture-dir DIR`, where DIR is an existing,
writable, genuinely disk-backed AmigaOS directory. No option retains resident
storage. A `T:` assign may resolve to RAM and would defeat the saving. The 1 MiB
bound covers payload capacity, not metadata, temporary records, the executable or
OS buffers. Records stay contiguous; a single record exceeding the budget, or
insufficient space because of pins, fails explicitly rather than falling back to
unbounded RAM. This is scratch storage, not a persistent module-object format.

One private scratch directory/file stores immutable chunks by logical byte offset.
Counted leases cover nested lookahead, configuration and replay; bytes cannot be
evicted until every borrower releases them. The small resident directory uses
32-byte rows for shift-based indexing. Round-robin eviction avoids repeated scans
from its first row. DOS transport owns exact seeks/transfers and removes only paths
it created. Normal success and failure cleanup close and delete owned storage.
Preparation, binding, expression compilation and module order stay eager.

Identical frozen input: 214 mapped files / 2,648,242 bytes, workload SHA-256
`eae5102bd21b3863105a7f550912193dff8e52c8ba2d6ff11b498392b7dca480`.
All runs generate a fresh live Rust oracle and match all 708,216 output bytes.
Same five instrumentation gates on each memory image:

| Resource | Original resident | Resident at storage checkpoint | Disk at storage checkpoint |
|---|---:|---:|---:|
| Peak tracked heap | 34,517,176 B | 34,533,560 B | 11,839,624 B |
| Peak capture payload capacity | 23,986,176 B | 23,986,176 B | 1,048,576 B |
| Capture directory capacity | 16,384 B | 32,768 B | 32,768 B |
| Captured records | 116,855 | 116,855 | 116,855 |
| Logical captured bytes | 23,852,392 B | 23,852,392 B | 23,852,392 B |
| Expression compilations / evaluations | 69,528 / 176,850 | 69,528 / 176,850 | 69,528 / 176,850 |
| Allocated/freed capacities, each | 64,979,400 B | 65,012,168 B | 94,180,904 B |

Disk backing reduces total peak by **22,677,552 B / 65.7%** versus the original,
without avoiding language work. Reload churn increases total allocation. Its 804
completed writes cover 23,852,392 bytes; 895 successful reloads read 26,807,624 bytes,
with 1,665 evictions. Peak payload is exactly the budget. Every memory run finishes
with zero tracked live bytes, balanced capacities and no observer errors.

Release timing uses no instrumentation, serially in both orders:

| Pair | Original resident | Disk-backed |
|---|---:|---:|
| Reference then disk | 104.747 s | 105.912 s |
| Disk then reference | 104.700 s | 105.669 s |
| Mean | 104.724 s | 105.790 s |

The measured +1.019% / +1.067 s difference is inside the roughly two-second noise:
no established speedup or slowdown. The current resident executable takes 105.145 s
in one control, also within that noise. Release image / linked reservation grows
from 708,216 / 741,956 to 710,340 / 744,776 bytes. No disk timing on A6000 hardware
has been obtained; FS-UAE's host-backed filesystem does not predict physical disk cost.

Decision: retain this single explicit storage policy, with resident storage still
the default. It meets the memory gate; it is not a deferred-preparation or speed
optimization. Small below-budget projects need not save memory: the maintained
module workload stays at 608,768 B, while the macro-heavy workload grows from
2,249,656 to 2,282,424 B because sealing creates another partial chunk. Both modes
match both live Rust BIN/Hunk artifacts. The component probe exercises distinct
chunk contents, reload boundaries, nested pins, budget rejection, invalid scratch
setup, idempotent cleanup and actual file/directory removal.

Existing telemetry reports resident capture ownership; logical record composition
remains separate. Progress events 360/361 count completed reads/writes and bytes;
362 reports fault attempts, evictions and peak payload. The existing decoder accepts
absence for reference images, but rejects partial/duplicate store summaries. No new
observer framework or production workload selector was introduced.

## Telemetry definitions

Expected saving: avoid capturing implementation bodies for about 1,173 inventoried
release-disabled macro definitions from five repeatedly included telemetry files, reducing records and preparation
work. Outer generic conditionals now skip those bodies; small no-op definitions
preserve public call signatures. Ordinary call wrappers and failure behavior remain
active. No filename, benchmark or expected-output selection exists in production.

This necessarily changes the source workload. Frozen before/after snapshots differ
only in the five guards and five no-op includes. Both run the same native engine
and produce the same live Rust output. This is not an identical-source engine
comparison.

| Resource | Before | Guarded |
|---|---:|---:|
| Tracked peak heap | 47,980,680 B | 43,659,400 B |
| Retained records | 117,686 | 116,715 |
| Retained capture used | 24,245,572 B | 23,836,984 B |
| Plan bytes | 6,218,310 B | 5,806,578 B |
| Allocated/freed capacities, each | 117,317,832 B | 107,474,488 B |
| Instrumented preparation | 72.384 s | 71.530 s |

Decision: keep. Heap falls 9.0%, capture payload 1.69% and plans 6.62%; retained
records fall only 0.825%, below the record-count hypothesis. Capacity thresholds
amplify the heap effect; it is not a universal 9% prediction. Preparation timing
shows no established speed gain. Both full runs complete exactly and free all
tracked storage. Disabled and enabled macro combinations retain their behavior.

## Chunked capture

Expected saving: remove capture arena growth copies and excess capacity while
preserving record bytes, logical offsets, source identities and replay behavior.
The shared append-only store is now the sole implementation. The eager preparation
path remains the semantic reference; old executables and frozen comparisons live
outside the tree. See [ownership and switch decisions](native-lazy-preparation.md).

The final comparison removes the old stacked macro switches from both sides.
Identical guarded frozen input: 216 mapped files, workload SHA-256
`1dd34618cfaaa53f84b6bf02267e69fa0fc2ddf1f32bfde482c7d02438ba80da`;
complete live Rust Hunk output 707,204 bytes.

| Resource | Contiguous eager | Chunked eager |
|---|---:|---:|
| Tracked peak heap | 43,659,400 B | 34,324,616 B |
| Retained capture used | 23,836,984 B | 23,836,984 B |
| Retained capture capacity | 33,337,344 B | 23,953,408 B |
| Retained capture slack | 9,500,360 B | 116,424 B |
| Retained growth-copy bytes | 33,064,076 B | 0 B |
| Allocated/freed capacities, each | 107,474,488 B | 64,858,680 B |

Decision: promote for the 21.4% heap reduction and eliminated growth copies.
Records, tokens and retained payload are identical; this is storage efficiency,
not avoided language work. Earlier two-pair release change of 1.4% was noise,
not a speed claim. Component checks cover boundaries, oversized records, shared
regions, cleanup and exact execution. Current-source full self-host also passes.

## Symbol lookup scaling

Expected saving: reduce long collision-chain traversal, the established lookup
hotspot. Test fixed 4,096 buckets, load-scaled buckets, and a full cached hash per
entry independently against fixed 1,024. Existing exact counters are the gate;
only the selected candidate receives release timing. No switches remain in product
code. All four variants complete the same frozen full input with exact Rust output.

| Resource | 1,024 | 4,096 | Scaled | Cached hash |
|---|---:|---:|---:|---:|
| Lookup calls | 267,289 | 267,289 | 267,301 | 267,289 |
| Chain entries visited | 4,433,086 | 1,238,246 | 454,810 | 4,433,086 |
| Entries per lookup | 16.59 | 4.63 | 1.70 | 16.59 |
| Name comparisons | 228,335 | 180,349 | 168,003 | 164,182 |
| Compared byte pairs | 6,407,408 | 5,673,062 | 5,491,166 | 5,428,948 |
| Hash spelling bytes | 10,085,313 | 10,085,313 | 11,421,472 | 10,085,313 |
| Tracked peak heap | 34,324,616 B | 34,336,904 B | 34,353,336 B | 34,324,616 B |

Decision: scaled buckets, initially 256, doubling at indexed load one to a maximum
16,384. It removes 89.7% of chain visits and 14.3% of compared byte pairs for
28,720 extra peak bytes (0.084%). Twelve extra misses follow growth to recompute
the bucket; successful hits are unchanged. Rehash spelling reads are included in
hash-byte totals (+13.2%), but not in the sampled ordinary hash timer.

Rehashing splits existing bucket chains stably, preserving lookup precedence and
numeric entry identities. Unindexed iterator/import proxies are not rethreaded.
Allocation precedes mutation; lifecycle and failure cleanup remain owned by a
separate bucket module. Native controls cover collisions, case folding, lexical
visibility, imported struct members and the final source identity 65,535.

Fixed 4,096 is inferior on the target chain/byte-comparison counts. Cached hashes shorten comparisons but not
chains, and enlarge logical entries from 36 to 40 bytes; allocation rounding hides
that increase in this heap peak. Neither alternative is retained or combined with
the winner.

Uninstrumented frozen-input release pairs, in both run orders:

| Pair | Fixed 1,024 | Scaled |
|---|---:|---:|
| Base then scaled | 101.937 s | 100.766 s |
| Scaled then base | 103.570 s | 101.076 s |
| Mean | 102.754 s | 100.921 s |

The mean difference is −1.78% (−1.833 s), inside the roughly two-second noise.
It is not an established full-build speedup. Keep the change for the exact-work
reduction with a negligible heap cost; do not extrapolate a 90% runtime gain.

## Additional workloads

The maintained [benchmark sources](../../benchmarks/native-preparation/README.md)
add a four-module project and a four-module, 1,000-call macro-heavy project.
Both emit source-directed BIN and Hunk files, compared independently with live
Rust outputs. The local opForge465 project has no macro directives, so it is not
presented as a macro-heavy workload. No parity repair was folded into qualification.

| Scaled versus 1,024 | Small modules | Macro-heavy |
|---|---:|---:|
| Chain visits | 104 → 117 | 48,078 → 14,227 |
| Compared byte pairs | 1,037 → 1,037 | 87,683 → 55,056 |
| Tracked peak heap | 608,768 → 608,768 B | 2,237,064 → 2,249,400 B |
| Hash spelling bytes, including rehash | 2,185 → 2,185 | 238,323 → 427,387 |

The tiny workload incurs 13 extra visits; the macro workload removes 70.4% of
visits and 37.2% of byte pairs for 12,336 extra peak bytes. Rehashing costs are
visible rather than excluded. These are exact-work/heap results, not release-time
claims. Both outputs and all ownership accounting pass.

## Current qualification and distance to target

Fresh complete self-host of the current native CLI with identity reuse and
disk-backed capture: 215 mapped files / 2,669,473 bytes, source manifest
`fnv1a64:adf3b92af9fbbf2d`. The uninstrumented native run finishes in **105.204 s**,
with fresh challenge-bound START/DONE, exit zero, and all **710,392 output bytes**
exactly equal to the live Rust oracle and bootstrap (`fnv1a64:dbef0aa3d2172369`).
It uses an embedded m68020 package, `--capture-dir Work:`, a 64 KiB guest stack and
the expanded-memory A4000/68040 FS-UAE profile. This is the complete current CLI
self-build, separate from the unchanged-input resource comparisons. It qualifies
that configuration, not A6000 hardware or the resident default command.

The A6000 runner also accepts a manifest-bound `capture_mode: "disk"` bundle.
It creates `scratch` in the fresh run directory on `Development:` before timing,
then assembles with `--capture-dir scratch`; results record the storage mode.
The normal `python3 scripts/performance/run_a6000_selfhost.py` command uses the
bundle selected by `/tmp/opforge-a6000-current` and discovers the host through
`ash -l`. The current release bundle contains the same executable, source and
embedded package as the proof above; its hardware command remains unqualified
until a fresh A6000 run completes and matches all Rust output bytes.

The paired disk-backed peak is **11.06 MiB** of tracked heap, about **5.5 times
the entire 2 MiB target**, before executable, stack and OS costs. Adding the
instrumented image's linked reservation gives at least 11.78 MiB. This is the
frozen 214-file memory workload, not a separate memory measurement of the current
215-file self-build. No low-memory fit or full language/CPU parity is claimed.

Component and maintained workload checks, identical-input comparisons and the
complete current-source disk-backed self-build pass. The memory gate passes;
controlled follow-up does not establish a whole-build runtime change. The next larger ownership
decision remains the structural publication contract; this result does not
authorize its implementation.

## Cache experiment decisions

Both cache experiments are dropped; their code/options/tests are in Git history,
not the maintained tree. They do not reduce the acquisition floor.

| Prototype | Observed opportunity | Decision |
|---|---|---|
| Raw include reuse (`7b849ffd`) | 83/204 hits, no avoided preparation, +137,568 B heap; release pair means 101.551 / 101.572 s | Remove: no established runtime benefit. |
| Prepared declaration leaves (`406ebfda`) | Five warm entries / 698 rows avoid 1.73% of expression compilations; heap unchanged at 34,517,176 B; no demonstrated speed gain | Remove: too little benefit and incomplete preparation-observation equivalence. |

The prepared prototype established checked local-ID/schema export/import for its
restricted cases, not general relocatable `.ofm` objects. Global dotted binding can
change final values after earlier preparation consumers have observed them. Module
publication must retain unresolved recipes and ordered observations; freezing final
folded values is insufficient. Waiting for global completion cannot free early
capture. Those requirements remain in the [structural design](native-fragment-preparation.md).
Disk backing addresses residency without relying on either cache prototype.

## Enduring earlier decisions

Keep these facts without retaining the experiment chronology:

- Redundant record clearing was removed. The matched physical release pair was
  117 → 117 seconds: no demonstrated speed gain.
- Moving from 256 to 1,024 buckets gave a physical single pair of 119 → 110 seconds,
  with 3,072 extra peak heap bytes. It is useful evidence, not a confidence interval.
- Retained headers were compacted from 64 to 56 bytes. On 113,035 identical records,
  logical headers fell 904,280 bytes and tracked peak fell 1,130,496 bytes (2.35%).
  The 0.59% release-time difference was noise; retain the memory improvement only.

## Evidence and reproduction

Fresh receipts, manifests, frozen inputs, images and decoded reports are outside
`target`, under `/tmp/opforge-course-telemetry-cases-20261015`,
`/tmp/opforge-course-final-frozen-memory-case-20261015`,
`/tmp/opforge-course-lookup-cases-20261015`,
`/tmp/opforge-course-release-cases-20261015` and
`/tmp/opforge-course-benchmark-native-v4-20261015`.
These are local diagnostic artifacts, not durable repository dependencies. Git
preserves prior implementations and detailed historical evidence; no archive tree
or stale generated results are maintained here.

Use the [FS-UAE environment](../../agents/rules/fs-uae.md) and the ignored
`native_full_selfhost_comparison_fs_uae` test with
`OPFORGE_FULL_COMPARISON_CASE` pointing to a freshly prepared case. Its
[case contract](../../scripts/performance/run_fs_uae_selfhost_comparison.py)
requires an identified bootstrap and source mapping, then generates a live Rust
oracle. Use `native_preparation_benchmark_fs_uae` with the image/workload/result
variables documented beside the benchmark sources. Existing MEMD, capture and
hotspot telemetry provide the memory/work comparisons. Do not substitute archived
or expected bytes for the live oracle.

Current storage receipts and immutable cases are outside `target`:
`/tmp/opforge-disk-summary-20261016.json`,
`/tmp/opforge-disk-cases-v3-20261016`, and
`/tmp/opforge-disk-candidate-v3-20261016`. These local diagnostics are not repository
dependencies. For the full comparison, set case JSON `capture_mode` to `disk` or
`resident` (unset means resident); the disk adapter supplies `--capture-dir Work:`.
For the maintained workloads use `OPFORGE_PREPARATION_CAPTURE_MODE=disk`.

Identity-reuse receipts, both-order release pairs, the component probe and current
self-build are retained outside `target` under
`/tmp/opforge-record-order-cases-20261017`,
`/tmp/opforge-record-order-probe-native-20261017`, and
`/tmp/opforge-record-order-candidate-v1-20261017`, with the local summary at
`/tmp/opforge-record-order-summary-20261017.json`. These are diagnostic artifacts,
not repository dependencies. Progress 363/364 and the existing capture decoder
report ordered copy/reuse work; MEMD separately measures the actual heap peak.

The runtime-attribution investigation retains checked temporary sources, binary
receipts and diagnostic images at `/tmp/opforge-record-regression-build-20261017`,
full identical-input cases at `/tmp/opforge-record-regression-cases-20261017`, and
the decoded local summary at `/tmp/opforge-record-regression-summary-20261017.json`.
The unused old-layout phase image is host-built only, not native qualification.
Production code remains at `8b57e34b`; no new optimization was implemented here.
