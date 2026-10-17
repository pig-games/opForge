# Native runtime and memory diagnosis

Status, 2026-10-17: parity remains paused. Paged lexical names reduce identical
full-build tracked peak from 10,946,600 to **10,488,384 B (−4.19%)** and eliminate
2,489,904 growth-copy bytes. Release means are 101.362 / 100.115 s
(-1.23%), within timing noise; no runtime change is established. The full
current-source disk-backed self-host passes with exact live Rust output.
Bounded capture, staged record growth, scaled symbol buckets, identity record
reuse and workspace retirement remain maintained. Structural scan, early
publication and `.ofm` remain [design work requiring review](native-fragment-preparation.md).

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

## Paged lexical name store

Expected saving: replace power-of-two arena slack and large growth overlaps with
64 KiB pages. The late-replay sample suggested about 0.75 MiB less reserved name
storage; the whole-build saving depends on other simultaneous owners. The flat
reference is retained in Git at `5cb0990b` and as an immutable native executable;
the maintained path uses paging without another experiment switch.

`binary_name_store` owns append, trusted pointer resolution, checked diagnostic
resolution and release. The first page grows from 256 bytes up to 64 KiB; later
pages are fixed 64 KiB allocations. A name stays contiguous within one page.
`Entry.Name` remains a numeric handle: high word page, low word byte offset.
Payload accounting excludes sealed tail gaps. The page directory may move, but
full pages do not. Callers copy aliases before appending to a growing first page.

Scope lookup resolves once after namespace/length checks, outside byte-comparison
loops. Module, import, iterator, template and discovery readers use that interface.
After lexical processing, all pages are released before the first descriptor is
repurposed for the numeric block index. Final cleanup handles either ownership.
The diagnostic observer uses a checked resolver callback and retains its bounded
copy/preservation contract.

The identical frozen input remains 214 mapped files / 2,648,242 staged source/package
bytes, SHA-256
`eae5102bd21b3863105a7f550912193dff8e52c8ba2d6ff11b498392b7dca480`.
Both engines use the same package, command and five memory gates. Fresh live Rust
and native execution agree on all 708,216 output bytes in every full comparison.

| Resource | Flat names | Paged names |
|---|---:|---:|
| Peak tracked heap | 10,946,600 B | 10,488,384 B |
| Allocated/freed capacities, each | 92,083,752 B | 89,223,968 B |
| Global growth-copy bytes | 10,928,046 B | 8,438,142 B |
| Global growth calls | 40,850 | 40,878 |
| Prepared / assembly live heap | 2,969,088 / 4,943,360 B | unchanged |

Peak falls **458,216 B / 4.19%**; another live overlap limits the whole-build gain.
Allocation traffic falls 2,859,784 B and growth copies fall 2,489,904 B, despite
28 more small allocations. Captured records/bytes, source reads, expression work,
ordered reuse and capture/record-growth IO are unchanged. All capacities balance,
cleanup reaches zero, profiling/allocation flags are zero and scratch cleanup passes.
These results use existing telemetry; no new observer framework was added.

The small-module control keeps its 608,768 B peak (allocation traffic +136 B).
The macro-heavy control falls from 2,163,152 to 2,097,896 B, saving 65,256 B / 3.02%.
Both controls match the live Rust BIN and complete Hunk outputs with balanced
ownership. They remain separate workloads, not production selectors.

Release timing is independent of the enabled accounting run, in R-C-C-R order:

| Release comparison | Flat names | Paged names |
|---|---:|---:|
| Pair A | 101.774 s | 100.278 s |
| Pair B | 100.950 s | 99.953 s |
| Mean | 101.362 s | 100.115 s |

The mean difference is **-1.247 s / -1.23%**. This is within the
established timing noise; no runtime change is established. Retain paging for
the measured memory/copy-work saving. Release image size grows 732 B to 711,668 B;
linked reservation grows 576 B to 746,420 B. The instrumented image reserves
762,492 B separately from its tracked heap.

The standalone native probe covers small growth, copied aliases, sealed-page gaps,
checked/trusted handles, payload limits and repeated/repurposed cleanup. Isolated
first-page/new-page allocation-failure fixtures preserve existing names/extents;
the latter also verifies ownership of a grown but unpublished directory row.
The existing 65,535-ID boundary and 10,000-name arena probes pass. Independent
interface/register-lifetime review found no issues.

Reproduce the component under the [documented FS-UAE environment](../../agents/rules/fs-uae.md):

```sh
cargo test -p asm native_name_store_fs_uae -- --ignored --nocapture --test-threads=1
```

`OPFORGE_NAME_STORE_FAULT=first|page` selects only an isolated test-source fixture;
production has no fault selector. The host-only `native_name_store_host_assembles`
test optionally saves its image through `OPFORGE_NAME_STORE_SAVE_IMAGE`.
Receipts, immutable images and frozen cases are outside the tree at
`/tmp/opforge-name-store-build-20261017` and `/tmp/opforge-name-store-cases-20261017`.

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

## Retire finished preparation workspaces

Expected saving: **183,784 B / 1.585%** of the frozen full-build tracked peak,
without additional source reads, disk transfers or per-record work. No runtime
gain was predicted. Configuration and physical discovery have no consumers after
configuration seeding and frontend shutdown, so their storage now retires before
semantic replay. Independent I/O/line/output storage survives that transition.

The 41,216 B I/O area is allocated separately from configuration scratch from the
outset. This avoids rebasing active and suspended include pointers. After parameter
seeding and `ConfigFront.finish`, release the 92,336 B configuration scratch,
65,536 B candidate paths, 21,512 B declaration index and 4,400 B discovery scratch.
Keep origin filenames, include roots and semantic scratch. One idempotent discovery
cleanup routine serves normal retirement, direct inputs and partial failure.
There is no new switch, observer framework or alternate preparation path.

Same frozen 214 files / 2,648,242 source bytes, package, disk-capture command and
five telemetry gates as the identity-reuse reference. A fresh live Rust oracle
matches all 708,216 native output bytes, with fresh START/DONE and exit zero.

| Resource | Before retirement | After retirement |
|---|---:|---:|
| Peak tracked heap | 11,594,504 B | 11,410,720 B |
| Allocated/freed capacities, each | 92,083,752 B | unchanged |
| Prepared live heap | 2,969,088 B | unchanged |
| Assembly live heap | 4,943,360 B | unchanged |
| Captured records / logical bytes | 116,855 / 23,852,392 B | unchanged |
| Source bytes read | 2,816,781 B | unchanged |
| Expression compilations / evaluations | 69,528 / 176,850 | unchanged |
| Capture reads / writes | 26,807,624 / 23,852,392 B | unchanged |

The measured reduction is exactly the predicted **183,784 B / 1.585%**. Total
allocation capacity is unchanged: the saving comes from shorter overlapping
lifetimes. Separating I/O adds one reserve call, with unchanged total capacity.
Cleanup reaches zero live tracked bytes, with no accounting errors,
allocation failures or leaked capture files. Release image / linked reservation
grows by 44 / 48 B, to 710,436 / 744,876 B.

The identical small-module control keeps its 608,768 B peak. The macro-heavy
control falls from 2,282,424 to **2,163,152 B** (119,272 B / 5.23%); allocated/freed
capacities stay unchanged on both. Both BIN and Hunk outputs match live Rust.

Uninstrumented full-build runs alternate reference/candidate/candidate/reference,
on the same frozen source and package bytes:

| Release comparison | Before | After |
|---|---:|---:|
| Pair A | 103.065 s | 103.397 s |
| Pair B | 101.832 s | 102.147 s |
| Mean | 102.449 s | 102.772 s |

The mean difference is **+0.323 s / +0.315%**, below the measured noise. All four
runs match the complete fresh Rust output. No whole-build speedup or slowdown is
established. Retain the coherent lifetime reduction for its measured heap saving
above 1%, without claiming reduced preparation work.

Focused native checks cover active/expanded and nested repeated includes on both
68020 and 6502, configured compound values, unavailable/conflicting parameters,
discovery/static include graphs, direct streaming across reader refills, and
expected failures for missing/cyclic includes and forbidden module changes.
Independent ownership review found no issues. Formatting checks cover 165 files.
The optional detailed-counter variant of the direct-input test still fails its
pre-existing tokenization/binding-entry equality assertion: binding stage entries
count multiple calls per line. Its ordinary release correctness variant passes.
The broad redundant-test guard also retains 18 pre-existing autofixable findings;
the changed assembly file has no new findings. Neither limitation was hidden or
repaired as part of this lifetime change.

## Disk-assisted prepared-record growth

Expected saving: remove the old 1 MiB allocation at the measured 1 MiB → 2 MiB
`Records` growth event. The upper-bound whole-build prediction was about 9.2%,
subject to another peak taking over. The existing executable at `8e9e0334` remains
the comparison reference; no new option or experiment selector was added.

With `--capture-dir DIR`, actual growth of a nonempty record block whose existing
capacity is at least 1 MiB uses a private scratch context. Write the used prefix
completely, release old RAM, allocate the normal larger capacity and restore the
exact bytes. Publish `Used` only after a successful read. Small growth and resident
capture retain the ordinary allocator; records remain contiguous and retain their
numeric offsets. Assembly and record lookup acquire no new indirection.

The record owner delegates transport to the existing scratch-file module. It never
reuses arbitrary offsets in the capture-store file. Setup/write failures retain
the original block. Allocation/read failure after release leaves an empty block
and aborts preparation; it cannot retain the allocator's old-block-on-failure
contract. Normal completion retires scratch before assembly, with idempotent final
cleanup and a bounded scratch-growth diagnostic on failure.

Same frozen 214 files / 2,648,242 source bytes, package, disk command and telemetry
gates as the preceding step. Fresh native START/DONE and exit zero accompany all
708,216 output bytes matching a newly generated live Rust oracle.

| Resource | In-memory growth reference | Disk-assisted large growth |
|---|---:|---:|
| Peak tracked heap | 11,410,720 B | 10,946,600 B |
| Allocated/freed capacities, each | 92,083,752 B | unchanged |
| Prepared / assembly live heap | 2,969,088 / 4,943,360 B | unchanged |
| Global growth-copy bytes | 11,976,608 B | 10,928,046 B |
| Completed staged growths | none | 1 |
| Additional scratch writes / reads | none | 1,048,562 / 1,048,562 B |
| Old capacity released before replacement allocation | none | 1,048,576 B |

The measured whole-build saving is **464,120 B / 4.07%**, not the 1 MiB released
at one event. Another overlap now dominates; the earlier trace of record-high
allocation events cannot identify every secondary peak. The large live owners
still include lexical entries and names (2 MiB each), prepared records (2 MiB),
shared template/call plans (1 MiB), known values and template definitions. No larger
retirement or spill policy for these tables is implemented.

Capture-store reads/writes, retained records, source reads, expression compilation
and evaluation are unchanged. New progress events 365/366 report only the added
record-growth work, independently of capture IO and the actual MEMD heap peak.
All tracked capacities balance, cleanup reaches zero, no allocation/profiling
errors occur, and no owned scratch files remain. The small-module and macro-heavy
controls keep peaks of 608,768 and 2,163,152 B and stage no records; both BIN/Hunk
artifacts match live Rust on identical sources.

Uninstrumented comparisons use both run orders on the identical frozen input:

| Release comparison | In-memory growth | Disk-assisted growth |
|---|---:|---:|
| Pair A | 101.497 s | 101.768 s |
| Pair B | 101.612 s | 101.953 s |
| Mean | 101.554 s | 101.860 s |

The mean difference is **+0.306 s / +0.30%**, within the measured noise. No runtime
change is established; this is a measured heap/copy-work saving with extra IO.
All four runs match the complete fresh live Rust output.

The release image grows 500 B to 710,936 B; linked reservation grows 968 B to
745,844 B. Enabled counters add 16 B of BSS under existing capture telemetry gates;
ordinary release contains none of their code or storage. No observer framework
or VM/package semantic change was added.

Independent ownership review found no issues. Real native component probes cover
small/resident bypass, a full 1 MiB longword-index pattern, an appended tail,
over-limit/setup/write failure preservation, allocation/read failure empty-state
cleanup and repeated cleanup. Expected allocation failure is deliberate only in
the component fixture. Separate write/read fault variants replace exactly one
transport call in an isolated source snapshot; production has no fault switch.

Reproduce component images with ignored `native_record_growth_probe_save_image`,
`OPFORGE_RECORD_GROWTH_SAVE_DIR` outside `target`, and optional
`OPFORGE_RECORD_GROWTH_FAULT=write|read`. Normal and both fault images executed
through the fresh native exact-output runner. These are component proofs, separate
from the complete current-source self-host below.

Receipts, frozen cases and images remain outside the tree at
`/tmp/opforge-record-growth-cases-20261017` and
`/tmp/opforge-record-growth-build-20261017`. They are local diagnostics, not
repository dependencies. Heap alone is **10.44 MiB / 5.22 times the entire 2 MiB
target**, before executable, stack and OS. This does not qualify low-memory fit.

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

Fresh complete self-host of the current native CLI with paged names: **217
mapped files / 2,681,428 staged source/package bytes**, staged-input SHA-256
`6cf90ab3c0033c04a4ae3db00e9059d5805ecab589a79d7015db69c16ff20375`.
The uninstrumented native run finishes in **101.911 s**, with fresh
challenge-bound START/DONE, exit zero, no leaked scratch files, and all
**711,668 output bytes** exactly equal to the freshly assembled live Rust oracle
and bootstrap (`fnv1a64:c5334f77de1181e8`).

It uses an embedded m68020 package, `--capture-dir Work:`, a 64 KiB guest stack and
the expanded-memory A4000/68040 FS-UAE profile. This is the complete current CLI
self-build, separate from the identical-input resource comparisons. Its time is
not a speed comparison against earlier changed-source self-builds. It qualifies
that configuration, not A6000 hardware or the resident default command. No new
A6000 run or paged-name hardware bundle is qualified here.

The paired disk-backed peak is **10.00 MiB** of tracked heap, about **5.00 times
the entire 2 MiB target**, before executable, stack and OS costs. Adding the
instrumented image's linked reservation gives at least **10.73 MiB**. This is the
frozen 214-file memory workload, not a separate memory measurement of the current
217-file self-build. No low-memory fit or full language/CPU parity is claimed.

The maintained controls, five native storage/identity component cases,
14 focused host scope tests and complete current-source self-build pass. Rust
formatting, native formatting over 167 production/probe files, workflow/CPU-boundary,
no-growth and proof-contract checks pass. The combined changed-file redundant-test
scan retains two pre-existing autofixable findings in `binary_imports.asm`, verified
against the flat reference, plus advisory call-return findings. New storage/probe
code introduces no autofixable findings. No broad Rust workspace gate was run for
this native storage change.

Paging passes the memory gate without an established runtime change. Preparation
remains eager. Further spill/publication changes require a separate decision.

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

Current receipts, images, frozen inputs and decoded results remain outside `target`
at `/tmp/opforge-name-store-cases-20261017` and
`/tmp/opforge-name-store-build-20261017`. They are local diagnostics, not repository
dependencies. Git retains superseded implementations and detailed older receipts.

Use the [FS-UAE environment](../../agents/rules/fs-uae.md) and ignored
`native_full_selfhost_comparison_fs_uae` test with
`OPFORGE_FULL_COMPARISON_CASE` pointing to a freshly prepared case. The
[case contract](../../scripts/performance/run_fs_uae_selfhost_comparison.py)
identifies bootstrap/source/package bytes and generates a live Rust oracle.
Case JSON `capture_mode: "disk"` supplies `--capture-dir Work:`; `resident` or
unset retains RAM capture. Use `native_preparation_benchmark_fs_uae` with its
image/workload/result variables and `OPFORGE_PREPARATION_CAPTURE_MODE=disk` for
the maintained controls. Existing MEMD and capture telemetry supply separate
memory/work results. Archived expected bytes never replace the live oracle.
