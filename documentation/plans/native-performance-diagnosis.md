# Native runtime and memory diagnosis

Status, 2026-10-15: parity remains paused. This pass qualified chunked capture,
removed superseded macro experiments, guarded disabled telemetry definitions and
selected a symbol-table growth policy using exact work counts. Next is the
[structural design review](native-fragment-preparation.md); no structural pipeline
or persistent cache has been implemented.

## Measurement contract

Compare each engine change on identical frozen source/package bytes and commands,
with a newly generated live Rust oracle. Require a fresh native challenge,
START/DONE, exit zero and exact complete output. Compare tracked heap and exact
work separately from uninstrumented runtime. Instrumented seconds are attribution,
not release speed. Two timing pairs below about 2% are inconclusive here; timing
noise is about two seconds. Reject changes below about 1% of their target resource
unless they are a documented prerequisite. No new observer framework was added.

All new runs use the expanded-memory FS-UAE template, a 64 KiB guest stack and
serial execution. They are neither A6000 timings nor 2 MiB qualification. Tracked
heap excludes the executable, stack, OS and untracked allocations; linked static
reservation is reported separately. Do not compare absolute times across emulator
profiles or changing source snapshots.

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

Fresh complete self-host of current native sources: 214 mapped files / 2,648,242
bytes, source manifest `fnv1a64:c57d381aab4b2523`. The uninstrumented native build
finishes in 101.978 s, exit zero, and emits all 708,216 bytes exactly equal to the
fresh Rust oracle and its own bootstrap. This is a full self-host, not a prefix
or successful launcher. Release linked reservation is 741,956 bytes.

The matched frozen-input winner peaks at 34,353,336 tracked heap bytes (32.76 MiB),
about 16.4 times the entire 2 MiB target before executable, stack or OS costs.
The earlier current-source chunked memory run measured 34,390,152 bytes before
bucket scaling; do not substitute that different-source number for a paired delta.
No low-memory qualification is claimed.

Matched-input memory images end with zero tracked live storage, balanced allocated
and freed capacities and no observer errors. Release timing uses no instrumentation.
The current full-source proof, fixed-input comparisons, component tests, focused
Rust checks and engineering guards establish this performance checkpoint, not
full language/CPU parity. A6000 hardware is unavailable for this pass.

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
