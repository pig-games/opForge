# Native runtime and memory diagnosis

Status, 2026-10-18: qualified-source macro register binding is repaired and the
bounded cache reuses page buffers. The current release executable completes its
full disk-backed self-host with exact Rust output. Buffer reuse removes 88.41% of
allocation traffic on the frozen workload without changing its peak heap.
Sequential sealing still costs substantial runtime versus the pre-sealing reference.
Parity remains paused. See [storage ownership](native-lazy-preparation.md) and
[the remaining object boundary](native-fragment-preparation.md).

## Measurement contract

Use identical source/package bytes, commands and build settings for comparisons.
Assemble a live Rust oracle for each case; require fresh challenge-bound START/DONE,
explicit zero exit and exact complete output. A timeout or diagnostic capture is
not completion. Keep instrumented memory/work accounting separate from release
timing. Small differences below about two seconds / 2% are not speed evidence.

These runs use the same expanded-memory A4000/68040 FS-UAE profile and 64 KiB guest
stack, serially. They are not A6000 measurements or a 2 MiB qualification. Heap
excludes executable, stack, OS and untracked allocations. No new observer framework
or experiment selector was added.

## Isolated binding repair

Early tuple scalar checks ran before ordinary binding removed lexical qualification.
Passing that preparation marker directly to shared EXVM rejected valid qualified
macro displacements and left register arguments to bind as scalar symbols.

The repair preserves original/trial tokens. Already accepted syntax keeps the
existing no-copy path. Only rejected spans containing qualified source identities
get a bounded private parsing copy with that marker removed. Package qualifiers
and reserved field/member markers remain intact; the shared grammar still owns
syntax. No CPU-specific parser or register rule was added.

On the frozen full-build input, heap, allocation and language-work counters are
identical before/after this repair. Instrumented duration is 329.314 -> 328.189 s,
inside noise. Do not attribute the later cache gain to this fix. The binding-only
release self-host also completes in resident mode: 123.988 s, exact 728,652 B.

## Frozen buffer-reuse comparison

Expected effect: remove repeated 32 KiB allocation/clearing during page eviction,
while preserving the four-page 128 KiB cache and the sealing heap saving.

Input: **214 files / 2,648,242 staged bytes**, workload SHA-256
`eae5102bd21b3863105a7f550912193dff8e52c8ba2d6ff11b498392b7dca480`.
All completed full comparisons match the live **708,216-byte** Rust Hunk,
`fnv1a64:27c800763d556961`. Pre-reuse means the binding repair is already present.

| Resource | Before buffer reuse | After buffer reuse |
|---|---:|---:|
| Uninstrumented START/DONE | 315.421 s | 200.961 s |
| Peak tracked heap | 6,909,720 B | 6,909,720 B |
| Allocated/freed capacity, each | 826,338,176 B | 95,775,616 B |
| Prepared live heap | 2,969,088 B | 2,969,088 B |
| Assembly live heap | 4,943,360 B | 4,943,360 B |
| Cleanup live heap | 0 B | 0 B |
| Instrumented preparation | 289.94 s | 166.42 s |
| Instrumented assembly | 38.18 s | 37.38 s |
| Instrumented START/DONE | 328.189 s | 203.754 s |

Allocation/free traffic falls **730,562,560 B / 88.41%**. Instrumented preparation
falls 42.60%; that is not a release timing claim. The separate uninstrumented
comparison falls **114.460 s / 36.29%**. This is one completed pair; it establishes
a large gain, not precise repeatability. Heap does not fall further: this change
preserves the earlier sealing saving, while reducing allocator/clearing work. Both memory runs have no allocation
failures, no profiling diagnostics, zero cleanup ownership and no scratch payloads.

Language-work counters are unchanged: 69,528 expression compilations, 176,850
evaluations, 269,945 compiled program bytes, 2,032,832 packed source bytes and
2,816,781 source bytes read. The optimization changes storage work, not language
coverage or required execution.

The earlier pre-sealing reference used 8,525,376 B peak heap and completed this
same frozen release workload in 101.414 s. Sealing therefore retains its
**1,615,656 B / 18.95%** heap saving, but the current disk path still takes about
1.98 times that reference's runtime. Resident lexical access remains available as
the contiguous control; the reference executable is preserved independently.
Earlier 600-second release timeouts were incomplete results, not finite timings;
the fresh pre-reuse comparison above must take precedence when interpreting speed.

## Resident versus disk storage on identical input

The following comparison uses the same 214-file frozen input above, current
release/instrumented executables, embedded package, hardware profile and 64 KiB
stack. Only the existing storage policy differs. Resident mode omits
`--capture-dir Work:`; disk mode enables raw-capture, prepared-record and lexical
backing together, including their existing scheduling differences. This does not
isolate lexical paging alone. No native code or instrumentation changed.

| Resource | Resident | Disk-backed |
|---|---:|---:|
| Separate release START/DONE | 107.562 s | 200.961 s |
| Observed peak tracked heap | 18,831,968 B / 17.96 MiB | 6,909,720 B / 6.59 MiB |
| Prepared live heap | 2,991,824 B | 2,969,088 B |
| Assembly live heap | 4,966,096 B | 4,943,360 B |
| Allocated capacity | 63,745,344 B | 95,775,616 B |
| Terminal cleanup live heap | **22,736 B** | 0 B |
| Instrumented preparation | 78.10 s | 166.42 s |
| Instrumented assembly | 36.90 s | 37.38 s |

Observed disk peak is **11,922,248 B / 11.37 MiB / 63.31% lower**, while release
execution takes **93.399 s / 86.83% longer**. These are single completed pairs,
not precise repeated timing estimates. Preparation accounts for nearly all the
instrumented phase-time difference; those clocks are separate from release timing.

All four runs complete with fresh challenge-bound START/DONE, explicit exit zero
and the exact full 708,216-byte live Rust Hunk. Source-read, packed-source,
expression-compilation, evaluation and compiled-program counters agree. The
resident memory run has no allocation failure or profiling error, but leaves
22,736 B tracked live: allocated/freed capacity is 63,745,344 / 63,722,608 B.
The decoder reports both nonzero terminal ownership and unbalanced capacities.
Its observed peak is reported with that limit; this is **not clean memory
qualification**. A read-only review found an ownership gap: resident
`frontend.indexBlocks` repurposes the retired name-store `First` block for its
numeric index, but `scopes.release` no longer frees that block. Disk cleanup
explicitly releases its separate `PreparedIndex`. This is consistent with the
residual; the receipt does not identify individual allocations, so the exact
22,736-byte allocation is not independently established. No repair is included
in this measurement change. Disk accounting finishes at zero with no diagnostics.

These frozen-input times must not be mixed with the 118.617 / 225.499 s
current-source release results below: that newer input has 223 files. The prior
instrumented current-source disk timeout remains unresolved. Both storage modes
remain above 2 MiB before executable, stack and OS.

## Current-source full self-host

Current source: **223 files / 2,783,726 B**, manifest
`fnv1a64:ae80a45a7feba2d2`. The package is embedded
`m68020--motorola68k.bin`, 454,890 B, `fnv1a64:095cca74317e365b`.
The release bootstrap and complete Rust oracle are both **728,644 B**,
`fnv1a64:355994e45406f3aa`.

| Bootstrap / storage | Result |
|---|---|
| Release / disk | Fresh START/DONE, exit zero, exact full self-host; 225.499 s |
| Release / resident | Fresh START/DONE, exit zero, exact full self-host; 118.617 s |
| Instrumented / disk | No completion within 1,200 s after START; last progress ORDER, before BIND. No final memory receipt. |

The disk command uses the existing `--capture-dir Work:` policy. `Work:` is the
runner's disk-backed case directory, not RAM-backed `T:`. The assembly command
otherwise uses `-i src/experimental/opforge_compact_cli.asm --hunk Work:output.hunk
-M src -I src/debug`. Instrumented bootstraps use only the established five defines:
DEBUG_CONTRACTS, MEMORY_TELEMETRY, PREPARATION_PROGRESS, CAPTURE_TELEMETRY and
MEMORY_TELEMETRY_LOCAL_EXPORT, each prefixed `OPFORGE_`. Target source is assembled
as release code; instrumentation does not change its required oracle.

The extra instrumented full-current-source run remains unqualified. Its CPU remained
busy after the ORDER marker, which brackets global completion before BIND; the
marker does not identify a single ordering routine. A read-only view/telemetry
lifetime review found no concrete cause. Do not infer a release slowdown, a valid
peak, or a cache-caused regression from that timeout. A matching pre-reuse
instrumented current-source control has not been run.

The completed current-source checks qualify their own configurations. They do not replace
the frozen input comparison or imply hardware qualification.

## Smaller maintained controls

Two release repetitions per stage use the maintained multi-module and macro-heavy
projects. Every source row is identical across stages; both BIN and complete Hunk
outputs match fresh live Rust oracles. Separate memory runs use the same sources.

| Control | Release before reuse | Release after reuse | Peak heap, each | Allocated capacity before -> after |
|---|---:|---:|---:|---:|
| Multi-module | Not recorded | Not recorded | 608,768 B | 1,242,696 -> 1,242,696 B |
| Macro-heavy | 8.713 / 8.860 s | 8.106 / 8.088 s | 2,173,848 B | 162,832,960 -> 5,579,328 B |

The tiny module cases complete successfully but the runner records no START/DONE
duration for binding/reuse executables; null is not zero. Pre-binding controls
record 0.259 / 0.254 s. The macro-heavy pre-binding control takes 8.715 / 8.861 s,
confirming no measurable binding-repair effect. Reuse removes 96.57% of its
allocated capacity, but the sub-second release difference is below this phase's
timing-noise threshold: no small-control speed claim. Both memory controls finish
with zero cleanup ownership and no allocation/profiling failures. The earlier
pre-sealing macro control used 2,066,408 B peak: the current 2,173,848 B remains
107,440 B / 5.2% higher. Sealing is not a universal heap win.

## Ownership and validation

The page provider allocates below budget. At budget it flushes an unpinned victim,
then transfers Pointer/Capacity while preserving both logical Used extents. Reserved
is unchanged on transfer; no spare buffer or bigger cache is needed. A failed flush
leaves ownership intact. A failed reload frees its unique destination allocation,
preserves logical extent and reduces Reserved. Appends expose initialized bytes;
short pages never expose a reused buffer's stale tail.

Native component checks cover unique live ownership, dirty writeback, pinned budget
exhaustion, short-page bounds, failed flush/retry, failed reload and cleanup. Pointer
membership alone does not prove absence of allocation; MEMD totals above establish
that saving. The cold allocation path with nonzero retained Used was reviewed
statically; the component ends after its failed-read cleanup.

Both binding-stage resident/disk module matrices and the reuse-stage disk matrix
pass: 14 accepted cases have exact artifacts; four negative cases reject explicitly.
Live Rust oracles, register context/import/list and guarded-register checks pass.
Iterator/value macro controls fail; the iterator failure also reproduces on the
pre-sealing reference. That existing language gap is not repaired in this phase.

Formatting, architecture, instrumentation, no-growth and fresh-proof guards pass;
the workflow suite passes 152 tests. No broad Rust workspace gate or A6000 run was
made. Independent cache/ABI review found no actionable defect. No speculative
production fix was added for incomplete diagnostic results.

## Decision and remaining target

Keep buffer reuse and the qualified-name repair as separate inspectable commits.
Preserve contiguous storage and the pre-sealing executable as references. Do not
start another ownership change before reviewing the remaining runtime tradeoff and
current qualification limits.

**No 2 MiB fit is claimed.** Frozen tracked heap is **6.59 MiB**, exceeding the entire
target by **4.59 MiB** before executable, stack and OS. Instrumented linked reservation
adds 781,252 B, reaching at least **7.33 MiB** before stack/OS. Prepared/assembly live
heap remains 2.83 / 4.71 MiB. Buffer reuse reduces traffic, not these retained owners.

The next runtime candidate to discuss is resident chain/hash metadata to avoid cold
entry/name acquisition during failed lookup and rehash. A 14-byte identity bridge
would cost another 393,210 B at 65,535 IDs; an unchanged peak would leave about
1.166 MiB of the measured sealing saving. This is a projection, not implementation
or approval. Real object-local IDs and relocatable near-output objects remain the
next architectural ownership boundary; macro-only deferral/audits and retired
include/leaf-cache experiments remain removed.

## Enduring earlier decisions

Retain adaptive symbol buckets with shared masks, release-gated telemetry, opaque
asset bytes, compact capture headers and removal of overwritten record clearing.
The clearing change had no measurable full A6000 speed gain; header compaction
reduced the separately measured peak by 1.08 MiB. Their historical hardware cases
and source-specific timings remain in the [performance reference](../performance/vm-efficiency.md).
They do not qualify today's executable or storage configuration.

Chunked captures, bounded prepared records and completed-configuration retirement
remain maintained. Macro-only audits/deferral, token/fragment prototypes, raw
include reuse and prepared leaf caching remain removed. Future work needs
object-local identities and relocation/layout recipes, not another observer.

## Evidence

Current cases, images, live-oracle receipts, MEMD and control logs are outside
`target` at `/tmp/opforge-cache-reuse-20261018`. The preceding sealing comparison
is at `/tmp/opforge-module-sealing-20261018`, with pre-sealing images under
`/tmp/opforge-prepared-objects-20261018/integration-{release,memory}`.
`prepare_cases.py`, `run_native.py` and the batch drivers record reproduction.
These are local evidence paths, not repository dependencies. Git is the archive.
