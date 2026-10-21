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

## Proposed ownership replacement — not implemented

The recommended next step is now
[stored module assembly interfaces](native-module-interfaces.md): persist complete
value/type/template interfaces and consume numeric references directly throughout
the CLI, including the root. Replace shared semantic publication and private
dependency snapshot/install copies. Keep the reference path, current linker and
the root's shared final-driver role. This is design work only.

The measured private lexical/name payloads and imported state total **1,280,256 B**.
These include necessary local data, not only redundant imports. A provisional
**512 KiB** combined ceiling for their replacement local working data, references,
interface I/O, directories and growth overlap forecasts **755,968 B (738.25 KiB,
11.3%)** net saving at the identified peak. No credit is assigned to the
unattributed shared background or the plan arena. Keeping current linked storage
and stack would give a **6,881,644 B (6.56 MiB)** process lower bound, still above
budget before other costs and code growth. This is a capacity-based forecast,
not measured savings or a guarantee that the maximum stays in the same phase.

This replaces the standalone lexical-window proposal as the recommended next
implementation. That narrower alternative forecast 632 KiB by spilling private
lexical payloads, but would leave dependency-interface copying in place. Bounded
local lexical storage is included in the new step; ordinary sealing cannot keep
all public/global facets pinned. Backing must remain live through interface
export and cleanup. Implementation and working-set bounds still need review.

Stored interface/index access may add I/O; no numerical speedup is predicted.
Use existing counters, full-source correctness and separate memory capture, with
bounded views and explicit failure cleanup. Comparative release timing remains
paused under the existing root-handling policy. See the design for acceptance
scope and for what remains outside this ownership step.

## Known limits and next steps

Known parity observations include native rejection of `.org` inside a section,
section-relative placed mapped imports in Rust flat output, Rust retaining an
unreferenced named block for a top-level-effects-only import where native omits
it, and native large `.fill` being unsupported under both policies. Rust also has
a 6502 forward-fill/no-retry gap and a nonzero-label loop-count retry gap. A
previous broad workspace run was red; focused results do not establish workspace
qualification.

Resolve or explicitly agree the root entry module handling before resuming full
performance comparisons. The stored-interface implementation proposal is awaiting
review; no implementation or memory saving has been claimed. Do not claim full fallback
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
