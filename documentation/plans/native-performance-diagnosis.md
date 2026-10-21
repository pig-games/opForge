# Native runtime and memory diagnosis

## Current status

Full performance comparisons remain user-paused. The process budget is **6.5 MiB**
with **1.5 MiB** reserved for AmigaOS. There is no runtime, memory-saving, or 8 MiB
fit claim. `--reference-modules` remains available.

Current source identity: manifest `950946ff21b4602d`, 259 files / 3,238,193
bytes; instrumented bootstrap 817,452 B; release image 804,876 B. A fresh
full-source native audit matches the entire live Rust Hunk. It saves all 185
dependency modules as `.ofm` objects: 185 writes, 177 final-emission reads, and
601,759 bytes of objects. The sole unsupported admission is the root entry
module, which remains in the shared final driver. The workload has no remaining
MOVE or binding admission failures; this is not zero fallback across all modules.
The earlier I audit failed output equality; it is superseded by this fresh proof.

## Qualification

The audit uses instrumentation. A separate uninstrumented fixed-point run also
passes: the current 804,876 B native release assembles its complete frozen source
and reproduces itself, identical to the freshly assembled Rust Hunk. It completes
in 189.2 seconds in FS-UAE on the expanded-memory profile; this single run is not
a comparative performance result. Both runs remove ephemeral scratch files.
Full performance comparisons remain paused until the remaining root handling is
resolved or explicitly agreed. No new observer or experiment switch was added.

Focused native checks cover absolute MOVE fields, exact scalar differences,
PC-relative trailing strings, shared word/long deltas, conservative automatic
branch-width intervals, counted loops, and actual previously rejected producers.
The final MOVE matrix passes 12 cases / 24 native policy results, including
conservative rejection controls. Candidate and reference policies match fresh
Rust binary/Hunk oracles. Ordering
checks pass for disk and resident storage, including repeated imports under
different aliases. Package checks pass for 30 VM and 333 assembler tests;
component checks cover field identity and signed-relative image patching.
Rust formatting, Clippy for VM/assembler all targets, native formatting and the
native engineering guards pass. Independent integration review found no blocking
correctness or ownership issue.
The reference policy remains covered and available.

The instrumented full audit reports peak owned heap **6,713,680 B** and zero live
owned bytes after cleanup. Linked static reservations are **858,396 B**; adding
those and the 65,536 B stack gives a process lower bound of **7,637,612 B**
(7.28 MiB), already 821,868 B above the 6.5 MiB budget, before other process costs.
This expanded-memory run does not establish constrained fit or memory savings.
Ephemeral capture files are gone; `.ofm` deliverables survive cleanup.

## Remaining peak ownership

Allocation-identity and capacity captures on the unchanged frozen input locate
the maximum in **private preparation of `binary_app`**, during public-interface
capture, before `retireFrontend`. The main frontend uses disk lexical storage, but each private compiler starts
a separate resident frontend: it has no lexical backing configuration. Disk `.ofm`
outputs do not change this separate owner. The triggering allocation grows
`PublicState.Members` from 4,096 to 8,192 B, with both allocations briefly live.
That small allocation crosses a large existing overlap; `.ofm` image construction
is later and is not the maximum.

| Live owner group at the peak | Capacity |
| --- | ---: |
| Private lexical entry payload | 524,288 B |
| Private name-page payload | 524,288 B |
| Private lexical numeric bridge and page directory | 131,328 B |
| Private preparation plan arena | 262,144 B |
| Private imported values/state | 231,680 B |
| Private module visibility and scope tables | 131,584 B |
| Private template pools/index | 50,176 B |
| Private scratch and line | 93,304 B |
| Private packed records and block index | 264,448 B |
| Private numeric/member seeds and layout maps | 109,784 B |
| Private initial-module directory and closed facet | 696 B |
| Public-interface capture, including growth overlap | 25,248 B |
| Shared background, not individually attributed in this capture | 4,364,712 B |
| **Total tracked heap** | **6,713,680 B** |

Two disposable instrumented bootstraps use existing progress/allocation telemetry
without production lifetime changes or a new observer framework. Both complete
with the same 185 stored modules, exact live Rust Hunk, zero terminal ownership,
and exactly the baseline's peak and 304,976,520 B cumulative allocation/free
counts. Their added diagnostic code/static cells are not a release measurement.
Detailed receipts and the checked capacity sum are retained at
`/tmp/opforge-peak-ownership-20261021/summary.json`.

Late retirement is not the first optimization: packing already reaches
**6,652,760 B** before interface capture. Freeing imports/plans after preparation
can remove 544,000 B at a later point, but cannot lower the whole-build peak by
more than **60,920 B (0.91%)**. Post-layout retirement is later still. Drop those
as headline peak reductions under this phase's approximately 1% threshold.

Correction to the previous phase description: 3,936,632 B is heap **after** final
assembly. Preparation ends at 2,634,536 B. Neither boundary is the build maximum.

## Proposed lifetime reduction — not implemented

Bound the private frontend's lexical payload ownership during preparation,
including dependency-interface installation. Explicitly configure a backing
owner for each private compiler attempt, using the established DOS/scratch-path
ownership, before configuration seeding or dependency installation. Then seal
small working windows of entries/names into the existing backing store and
release their resident payloads. Retain the numeric bridge and bounded owned
reader copies; spilled entries remain mutable through checked writeback. IDs,
name handles, declaration context and template behavior must remain unchanged.
The private compiler selects storage policy; the lexical owner controls sealing,
pins and I/O. Keep `--reference-modules` and the frozen current implementation.
Add no experiment switch or serialized VM version awareness.

This needs an explicit scratch-lifetime sealing operation: ordinary lexical
sealing intentionally keeps public/global facets resident, so simply configuring
disk storage or waiting until the module closes is insufficient. Private lexical
payloads do not escape through owned interface/template capture. Spilling does
not end lexical consumption: interface preparation still reads names/entries,
and frontend cleanup checks storage status. Keep backing live through those
consumers, using checked numeric-bridge read/copy/writeback APIs throughout. This
is storage management, not changing visibility or reachability.
Keep module assembly followed by `.ofm` publication and final linking as it is.

Budget **384 KiB** for active payload, backing-cache payload, retained working
payload and allocation/copy overlap together, plus **8 KiB** for added directory
capacity. Against the measured **1 MiB** lexical payload, this predicts
**647,168 B (632 KiB, 9.6% of heap peak)** net saving at the identified maximum.
The resulting budgeted heap is 6,066,512 B and the process lower bound is
6,990,444 B (6.67 MiB): still above the 6.5 MiB process budget before untracked
costs. This is a proposed bound and savings estimate, not a measured replacement
or fit claim; another phase can become the maximum.

There is no speedup prediction. Sealing adds disk writes and later reads; cache
locality, lease safety and failure cleanup are the principal risks. Qualification
must include aliases, member updates, imported typed values, templates, source
context and exact binary/Hunk output. Check window/cache capacities including
growth overlap, existing I/O counters and focused native runtime against the
frozen path, followed by full-source correctness and memory proof. Full release
performance comparisons remain paused. Review this proposal before implementing.

## Known limits and next steps

Known parity observations include native rejection of `.org` inside a section,
section-relative placed mapped imports in Rust flat output, Rust retaining an
unreferenced named block for a top-level-effects-only import where native omits
it, and native large `.fill` being unsupported under both policies. Rust also has
a 6502 forward-fill/no-retry gap and a nonzero-label loop-count retry gap. A
previous broad workspace run was red; focused results do not establish workspace
qualification.

Resolve the root entry module boundary before resuming full performance
comparisons. The measured lexical-window proposal above is awaiting review;
no implementation or memory saving has been claimed. Do not claim full fallback
closure, runtime/memory saving, or fit before qualification and measurement.
For later performance
comparisons, record source, package, image, inputs, policy, and fresh oracle;
separate release timing from capture/MEMD instrumentation and use three release
repetitions for a speed claim. Memory fit requires the documented 68020-8m
profile. The process budget includes linked code/data/BSS, embedded package,
stack, heap, and other process costs; the OS reserve is separate. See the
[native parity contract](../../agents/rules/native-rust-parity-porting.md),
[FS-UAE guide](../../agents/rules/fs-uae.md), and
[performance reference](../performance/vm-efficiency.md).

Keep one reference engine and explicit ownership. CPU/family semantics remain
package-owned. Generic capture contracts are in
[preparation ownership and capture storage](native-lazy-preparation.md).
