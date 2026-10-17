# Native runtime and memory diagnosis

Status, 2026-10-17: dependency-first publication reduces resident full-build tracked
peak from **33,442,280 to 18,175,240 B (−45.65%)** on identical frozen inputs.
Bounded disk capture keeps complete-graph scheduling because interleaving its
configuration/semantic owners increased peak. Final disk results and release timing
are below. This changes lifetimes, not the amount of language work. Parity remains
paused; general `.ofm` objects and a cheap structural-only scan remain future work.
See [ownership](native-lazy-preparation.md) and the
[implemented publication boundary](native-fragment-preparation.md).

## Measurement contract

Compare engine changes on identical frozen source/package bytes and commands with
a newly generated live Rust oracle. Require fresh challenge-bound START/DONE, exit
zero and exact complete output. Measure tracked heap/exact work separately from
uninstrumented runtime. Instrumented seconds are attribution, not release speed.
Two timing pairs below about 2% are inconclusive; noise is about two seconds, and
marginal larger differences also need reproducible controls. Reject changes below
about 1% of their target resource unless they are a documented prerequisite.

Runs use the expanded-memory A4000/68040 FS-UAE profile and 64 KiB guest stack,
serially. They are not A6000 timings or 2 MiB qualification. Tracked heap excludes
executable, stack, OS and untracked allocations. Report linked reservation separately.
Do not compare absolute times across changing source snapshots or emulator profiles.
No new observer framework was added.

## Dependency-first module publication

Expected saving: release completed modules' raw captures while later dependencies
are still being acquired. Keep the existing engine at `8e0bddc8` as the executable
reference. Use the shared frontend and final global binding/layout; do not add another
parser, VM, benchmark selector or experiment switch.

Resident discovery now prepares dependency-complete modules before later acquisition,
publishes owned numeric records/definitions/templates, then acknowledges and retires
raw capture ranges. Interleaved resident includes retire after replay, before
the parent resumes; complete-graph paths retain them until final capture cleanup.
Disjoint retirement validates pins and preserves logical offsets/tombstones.
Configuration and semantic sessions own independent state; final configuration transfers before its
owner retires. Late section maps retain fixed-size numeric geometry recipes until
global completion. Repeated `.use` parameter sets are checked against Rust semantics.

Identical frozen input is **214 mapped files / 2,648,242 staged source/package bytes**,
SHA-256 `eae5102bd21b3863105a7f550912193dff8e52c8ba2d6ff11b498392b7dca480`.
Every full comparison matches all **708,216 bytes** from a newly generated live Rust
oracle (`fnv1a64:27c800763d556961`). Memory images use the same five gates.

| Resource | Eager reference | Maintained pipeline |
|---|---:|---:|
| Resident tracked heap peak | 33,442,280 B | 18,175,240 B |
| Resident capture allocation peak | 23,986,176 B | 8,028,160 B |
| Resident allocated/freed capacity, each | 60,055,232 B | 62,433,760 B |
| Bounded-disk tracked heap peak | 10,488,384 B | 10,489,408 B |
| Bounded-disk allocated/freed capacity, each | 89,223,968 B | 89,224,992 B |
| Prepared / assembly live heap | 2,969,088 / 4,943,360 B | unchanged |

Resident capture allocation falls **66.53%** and total tracked peak falls **45.65%**.
All **116,855 records / 23,852,392 logical capture bytes**, 69,528 expression
compilations and 176,850 evaluations remain unchanged. Source construction and
configuration/semantic materialization copy counts are unchanged. This is real
lifetime reduction, not skipped language work or lazy expression compilation.
Prepared/assembly live heap remains **2,969,088 / 4,943,360 B**. Cleanup is zero,
capacities balance and profiling/allocation diagnostics are empty.

The all-interleaved disk control peaked at **12,047,240 B**, versus **10,488,384 B**
for complete-graph preparation (+14.86%). Freeing configuration immediately at graph
closure did not lower that peak: growing configuration and semantic owners overlap
earlier. Dropping arbitrary configuration names would invalidate discovery identities
and snapshots. Bounded disk capture therefore keeps the complete-graph schedule and
single-owner transition; its existing 1 MiB payload bound already removes accumulated
capture residency. Both schedules share replay and finalization. No additional option
or benchmark-specific policy is retained.

Release timing is separate, in reference/candidate/candidate/reference order, on the
same frozen input with disk capture and no instrumentation:

| Release comparison | Reference | Candidate |
|---|---:|---:|
| Pair A | 99.712 s | 99.873 s |
| Pair B | 100.050 s | 99.870 s |
| Mean | 99.881 s | 99.872 s |

The mean difference is **−0.010% / −0.010 s**, inside noise: no speedup or
slowdown is established. All four native outputs exactly match live Rust. The
resident memory saving passes the resource gate. Disk scheduling preserves the
previous peak to within **1,024 B / 0.010%**. Resident allocation traffic rises
3.96%; lower simultaneous residency is not lower total allocation work. Release
image/linked reservation grows 3,464 / 3,084 B, to **715,132 / 749,504 B**. The
instrumented image is 729,624 B with 765,816 B linked reservation.

Uninstrumented resident pairs exercise the actual early-publication path on the
same frozen input, again in reference/candidate/candidate/reference order:

| Resident release comparison | Eager reference | Early publication |
|---|---:|---:|
| Pair A | 100.376 s | 101.340 s |
| Pair B | 99.485 s | 100.069 s |
| Mean | 99.931 s | 100.705 s |

The resident mean difference is **+0.774 s / +0.77%**, also inside noise.
All four outputs match live Rust exactly. No release speedup or slowdown is
established for either storage policy; the resident memory improvement is the
reason to retain early publication. Disk timings alone would not qualify the
resident scheduler's runtime impact.

The maintained resident controls also match live Rust BIN and complete Hunk outputs:

| Resource | Small modules reference → candidate | Macro-heavy reference → candidate |
|---|---:|---:|
| Tracked heap peak | 608,768 → 608,768 B | 2,065,128 → 1,935,336 B (−6.28%) |
| Allocated/freed capacity, each | 975,328 → 1,089,280 B | 4,488,920 → 4,537,336 B |
| Instrumented seconds | 0.260 → 0.258 s | 5.885 → 6.145 s |

Expression/evaluation counts and source reads are unchanged, cleanup is zero and
profiling diagnostics are empty. Extra session setup adds allocation traffic;
smaller input does not guarantee a peak reduction. These short enabled-observer
samples are not paired release speed measurements; the macro sample's +0.260 s is
reported, not promoted to a causal runtime claim.

## Current qualification and distance to target

A fresh full native self-host of the final source passes: **218 staged files /
2,710,008 source/package bytes**, source manifest `fnv1a64:e3cae725594c59ef`.
All **715,132 executable bytes** match both the newly assembled live Rust oracle
and the bootstrap, `fnv1a64:e74eb42468318987`. Fresh challenge-bound START/DONE,
exit zero and empty scratch-file residue accompany the proof. The uninstrumented
current-source run takes **103.713 s**, using the embedded m68020 package and
`--capture-dir Work:`. This is the actual complete current CLI self-build; its
changing-source time is separate from the frozen comparison above. It qualifies
this FS-UAE configuration, not A6000 hardware or full language/CPU parity.

The final resident/disk discovery matrix compares the same 14 cases with live Rust:
scalar/typed configuration, definition-relative active includes and retained templates,
diamonds/multiple modules per file, late/consumed/reordered maps, forward layout,
logical geometry and accepted/rejected complete parameter sets. Native graph probes
cover yield/acknowledge, remaining-order closure, missing configuration, cycles and
legacy ordering. Capture probes cover shared-row retirement, pins, overlap rejection,
truncated views, stable appends and repeated cleanup. Additional layout probes cover
BSS, Hunk alignment, unknown-region rejection and 6502 alignment.

Focused Rust oracles, telemetry wrapper/zero-emission tests and 12 capture-decoder
tests pass. Formatting, workflow/CPU-boundary, no-growth and native-proof guards pass.
Independent combined ownership reviews found no unresolved issue. The changed-file
redundant-test scan retains four pre-existing autofixable findings (capture store,
imports and sections), plus advisory call-return checks; no new autofixable finding
was introduced. No broad Rust workspace gate was run for this native pipeline change.

The frozen resident peak is **17.33 MiB of tracked heap / 8.67 times** the entire
2 MiB target; bounded disk is **10.00 MiB / 5.00 times**. Including the instrumented
image's linked reservation gives at least **18.06 / 10.73 MiB**, before stack, OS
and untracked costs. These are frozen-input memory measurements, not a separate
memory measurement of the larger current-source self-build. No 2 MiB fit is claimed.

Global semantic scratch, lexical names, exports/templates and prepared records still
remain live across modules. The next substantial boundary is ownership/remapping of
prepared module data into relocatable objects, enabling its release or spill; another
macro-only observer or final-only cache cannot supply that saving.

## Maintained storage and lookup decisions

These remain one maintained set of policies, not stacked experiments. Detailed
superseded trials and receipts belong in Git history; the facts below explain the
current choices. Earlier measurements used their stated paired workloads and are
not additive percentages or current-source memory measurements.

### Paged lexical name store

Names stay contiguous within 64 KiB pages; `Entry.Name` holds page/offset handles.
Readers resolve once per spelling; diagnostics use a checked callback. First-page
aliases are copied before growth. Pages retire before the numeric block index reuses
the descriptor. This removed 2,489,904 growth-copy bytes and lowered paired disk peak
10,946,600 → 10,488,384 B (−4.19%). Release means 101.362 / 100.115 s were inside
noise. Component/failure checks preserve names, ownership and final ID 65,535.

### Reuse an already ordered record stream

Retain the prepared buffer only after validation proves complete contiguous coverage.
Origins and module starts use the same metadata loop; reorders/partial selections
copy. This avoided 2,032,832 copy bytes and a 2 MiB allocation, but whole peak fell
only 245,120 B / 2.07% because another overlap took over. Materialization itself saved
about 76 ms; an initial whole-build slowdown did not reproduce. No established
runtime effect or speculative cache/memory-bank explanation is retained.

### Retire finished preparation workspaces

Independent 41,216 B I/O storage survives configuration retirement; paths/include
roots needed later also remain. After final seeding, configuration scratch, candidate
paths, declaration index and discovery scratch retire before remaining semantic replay.
The isolated earlier saving was 183,784 B / 1.585%, with no extra language work or
established runtime change. Interleaved resident discovery extends source retirement
to dependency-complete spans, while preserving this closure transition.

### Disk-assisted prepared-record growth

With disk capture, nonempty record buffers at least 1 MiB stage the used prefix before
growth. Complete writes precede old-memory release; restore publishes `Used` only on
success. Setup/write failure preserves the old block; allocation/read failure after
release aborts with empty ownership. Resident/small growth uses the normal allocator.
The isolated peak fell 11,410,720 → 10,946,600 B (−4.07%), with 1 MiB written/read.
Release means 101.554 / 101.860 s were noise. Native component/fault probes cover both
failure ownership contracts. Records remain contiguous with no assembly indirection.

### Bounded disk-backed capture

`--capture-dir DIR` selects a genuinely disk-backed existing writable directory.
A 1 MiB payload budget includes writers and every counted reader lease, excluding
metadata, temporary records and OS costs. Immutable chunks spool to one private file;
eviction skips pins, reloads check exact extents, and errors never fall back to
unbounded RAM. Normal cleanup deletes only owned paths; crashes may leave scratch.
At its original storage checkpoint, paired peak fell 34,517,176 → 11,839,624 B
(−65.7%), with unchanged preparation work. Release +1.02% was inside noise. Physical
A6000 disk costs remain unmeasured; FS-UAE host-backed I/O does not predict them.

### Telemetry definitions

Outer generic guards skip release-disabled implementation bodies; small no-op includes
preserve macro signatures. No filename or workload selects behavior. This changed
source workload, not the engine: paired heap fell 47,980,680 → 43,659,400 B (−9.0%),
capture bytes 1.69%, and plans 6.62%; record reduction was only 0.825%. Capacity
thresholds amplified heap savings. No established preparation-time gain was shown.

### Chunked capture

32 KiB shared chunks and dedicated large records replaced relocating capture arenas.
Numeric logical offsets stay stable while directory descriptors move. On paired
guarded input, payload stayed 23,836,984 B, capacity fell 33,337,344 → 23,953,408 B,
and 33,064,076 growth-copy bytes disappeared. Tracked heap fell 21.4%; the 1.4%
release difference was noise. Module retirement now releases fully dead chunks.

### Symbol lookup scaling

Buckets start at 256, double at indexed load one and stop at 16,384. Stable chain
splits preserve lookup precedence and numeric identities; allocation precedes mutation.
Against fixed 1,024 on paired full input, chain visits fell 4,433,086 → 454,810
(−89.7%), compared byte pairs 6,407,408 → 5,491,166 (−14.3%), at 28,720 extra peak
bytes. Rehash spelling reads are counted (+13.2%); ordinary sampled hash time excludes
them. Two-pair release −1.78% was within noise. Fixed 4,096 was inferior on exact
work; cached hashes left chain length unchanged and enlarged entries. Both alternatives
were removed. The smaller controls expose rehash costs rather than hiding them.

## Additional workloads

The maintained [sources](../../benchmarks/native-preparation/README.md) are a small
four-module project and a four-module, 1,000-call macro-heavy project. Both compare
source-directed BIN and complete Hunk files with live Rust. The local opForge465
project has no macros, so it is not presented as the macro-heavy control. Workload
identity never selects production behavior.

## Cache experiment decisions

Raw include-text reuse was removed: 83/204 hits avoided no preparation, added
137,568 B heap and established no runtime gain. Prepared declaration leaves were
also removed: five warm entries / 698 rows avoided 1.73% of expression compilations
but no peak, with no demonstrated speed gain and incomplete observation consistency.
Their code/options/tests remain recoverable through Git, not archive trees.

The prepared prototype established restricted local-ID/schema export/import, not a
general `.ofm`. An object must preserve unresolved binding/layout recipes and values
observed at the original source-order point. Final folded values alone are insufficient.
Serializing after global completion cannot lower an earlier acquisition peak.
Macro-only first-use binding/candidates/plans, the binary-token prototype and fragment
envelope were removed; none has a retained selector. There is no new observer framework.

## Enduring earlier decisions

Redundant record clearing was removed with no physical speed gain (117 → 117 s).
The physical single pair for 256 → 1,024 symbol buckets was 119 → 110 s for 3,072
extra peak bytes; it is evidence, not a confidence interval. Compacting retained
headers from 64 to 56 bytes saved 904,280 logical header bytes on 113,035 records
and 1,130,496 peak bytes (2.35%); its 0.59% timing difference was noise.

## Evidence and reproduction

Current receipts, immutable images, frozen cases and decoded results live outside
`target` at `/tmp/opforge-module-pipeline-20261017`. The reference executable and
prior storage receipts remain at `/tmp/opforge-name-store-build-20261017` and
`/tmp/opforge-name-store-cases-20261017`. These are local diagnostic artifacts, not
repository dependencies. `ERRATA.txt` records an advisory storage-label typo in the
initial resident reference; its effective `capture_mode`, guest command and raw
native result identify resident storage. Raw receipts were preserved. Git is the archive for superseded code and detailed trials.

Use the [FS-UAE environment](../../agents/rules/fs-uae.md) and ignored
`native_full_selfhost_comparison_fs_uae` with `OPFORGE_FULL_COMPARISON_CASE` naming
a fresh case. The [case contract](../../scripts/performance/run_fs_uae_selfhost_comparison.py)
identifies bootstrap/source/package bytes and generates a live Rust oracle. Case
`capture_mode: "disk"` supplies `--capture-dir Work:`; resident/unset keeps RAM capture.
`OPFORGE_MODULE_PIPELINE_IMAGE` selects a prebuilt image for
`native_module_pipeline_discovery_exact_artifacts` and
`native_module_pipeline_disk_discovery_exact_artifacts`; both require real FS-UAE.

For controls, use `native_preparation_benchmark_fs_uae` with
`OPFORGE_PREPARATION_IMAGE`, `OPFORGE_PREPARATION_WORKLOAD=modules|macros`, a fresh
`OPFORGE_PREPARATION_RESULT_DIR`, and optional
`OPFORGE_PREPARATION_CAPTURE_MODE=disk`. Existing MEMD/capture events supply separate
heap/work results. Component images are saved outside `target` by ignored
`native_graph_publication_probe_save_image` / `OPFORGE_GRAPH_PUBLICATION_SAVE_DIR`
and `native_capture_store_probe_save_image` / `OPFORGE_CAPTURE_STORE_SAVE_DIR`.
Fresh expected artifacts never substitute for native completion.
