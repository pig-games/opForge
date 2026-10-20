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

Stored module payloads retire, but global compile-time values/types, published
template bodies, names/import graphs and linker metadata remain. Each private
compilation imports the required semantic interfaces into its own frontend.
The current module's records, values, layout recipes and emitted bytes overlap
that shared state. Building its immutable image briefly keeps the emitted bytes
and new image together; buffer growth can also overlap old/new capacities.
Final assembly retains its complete output, and Hunk serialization adds another
output buffer. Consumed captures and discovery state already retire; this is not
an assertion that every original module remains resident. Existing boundary
samples do not assign the measured peak to particular simultaneous owners.

## Known limits and next steps

Known parity observations include native rejection of `.org` inside a section,
section-relative placed mapped imports in Rust flat output, Rust retaining an
unreferenced named block for a top-level-effects-only import where native omits
it, and native large `.fill` being unsupported under both policies. Rust also has
a 6502 forward-fill/no-retry gap and a nonzero-label loop-count retry gap. A
previous broad workspace run was red; focused results do not establish workspace
qualification.

Resolve the root entry module boundary before resuming full performance
comparisons. Attribute the remaining compilation peak using existing telemetry
before choosing a memory change. The final assembly phase begins with 3,936,632 B
of tracked heap; the largest post-compilation progress sample is 4,460,480 B.
Those samples are below the 6,713,680 B allocation peak, so they do not identify
the exact simultaneous peak owners. Do not claim full
fallback closure, runtime or memory saving, or fit before qualification and
measurement. For later performance
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
