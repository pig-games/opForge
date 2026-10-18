# Native runtime and memory diagnosis

Status, 2026-10-18: sequential storage sealing reduces the frozen disk-backed
heap peak from **8,525,376 to 6,909,720 B (−18.95%)**, but is **not ready for
release promotion**. The instrumented comparison completes exactly; release
exceeds the 600-second bound versus a 101.414-second reference. Parity remains
paused. Current-source self-hosting also fails at qualified-register macro binding.
Resolve correctness and lookup/storage cost before further ownership changes.
See [storage ownership](native-lazy-preparation.md) and
[the remaining object boundary](native-fragment-preparation.md).

## Measurement contract

Compare identical source/package bytes and commands against a freshly assembled
Rust oracle. Require challenge-bound START/DONE, exit zero and exact complete
output. A timeout, diagnostic capture or launcher exit is not native completion.
Measure tracked heap separately from uninstrumented runtime. Results below about
2% / two seconds do not establish a speed change from two pairs alone.

Runs use the expanded-memory A4000/68040 FS-UAE profile and a 64 KiB guest stack,
serially. These are not A6000 or 2 MiB qualifications. Heap excludes executable,
stack, OS and untracked allocations. No observer framework or new selector was added.

## Sequential module storage sealing

Expected saving: at least **1 MiB of full-build peak heap**. Preserve the executable
from before sealing as the baseline. Resident storage keeps contiguous metadata;
`--capture-dir DIR` also selects bounded lexical backing. Both share the new owned
access interfaces, so resident storage is a control, not the unchanged executable.
Use a disk directory rather than RAM-backed `T:`.

After successful publication, private lexical entries and canonical spellings move
to private backing. Active arenas are physically freed. A stable eight-byte identity
bridge, retained interface entries/names and a four-page 128 KiB cache serve later
readers. Three module metadata arrays remain resident (393,216 B at full capacity).
Private dependencies are reloadable and mutable until global completion; this is
storage sealing, not early semantic finalization or a reusable `.ofm` object.

Frozen input: **214 files / 2,648,242 staged bytes**, SHA-256
`eae5102bd21b3863105a7f550912193dff8e52c8ba2d6ff11b498392b7dca480`.
Both completed instrumented runs produce the exact **708,216-byte** live Rust Hunk,
`fnv1a64:27c800763d556961`, with zero cleanup ownership and no scratch files left.

| Resource | Pre-sealing reference | Sealing candidate |
|---|---:|---:|
| Tracked heap peak | 8,525,376 B | 6,909,720 B |
| Allocated/freed capacity, each | 126,781,224 B | 826,338,176 B |
| Prepared live heap | 2,969,088 B | 2,969,088 B |
| Assembly live heap | 4,943,360 B | 4,943,360 B |
| Cleanup live heap | 0 B | 0 B |
| Expression compilations | 69,528 | 69,528 |
| Expression evaluations | 176,850 | 176,850 |
| Packed source | 2,032,832 B | 2,032,832 B |
| Source bytes read | 2,816,781 B | 2,816,781 B |
| Instrumented preparation | 70.38 s | 290.50 s |
| Instrumented assembly | 37.46 s | 38.68 s |
| Instrumented START/DONE | 108.077 s | 329.314 s |

The **1,615,656 B / 1.541 MiB** saving clears the heap target. Allocation traffic
grows by **699,556,952 B**; this is storage work, not reduced language work.
It is consistent with roughly 21,000 additional 32 KiB allocations, but is not a
measured page-fault count. Scope copy frames are stack storage, outside these figures.

| Separate release control | Reference | Candidate |
|---|---:|---:|
| Resident access-interface control | 104.253 s | 112.429 s |
| Frozen disk-backed full build | 101.414 s, exact | No completion within 600 s |
| Small multi-module project | 0.253 s | 0.255 s |
| Macro-heavy project | 6.108 s | 9.149 s |

The resident pair isolates an approximately **8% access-interface cost** before
spilling. It is one pair, not a precise speed estimate. The disk release timeout
cannot supply a completed timing or an exact-output claim. Small timing differences
are inside noise. Macro-heavy release slows by about **50%** in this control pair.

| Separate memory control | Small modules: reference → candidate | Macro-heavy: reference → candidate |
|---|---:|---:|
| Peak tracked heap | 608,768 → 608,768 B | 2,066,408 → 2,173,848 B |
| Allocated/freed capacity, each | 1,106,920 → 1,242,696 B | 4,523,232 → 162,832,960 B |

Both smaller controls match live Rust BIN and full Hunk output, retain identical
expression/evaluation/source counters, clean up to zero and leave no owned files.
The macro case grows peak by 107,440 B (5.2%); sealing is not a universal heap win.

The frozen comparison images precede the final statement-binding and named-size
cleanup: release/memory images are 728,292 / 742,816 B. The statement-binding repair
has a fresh native exposure proof. A later current-source self-build uses different,
larger source input; its failure below does not invalidate the frozen comparison,
but prevents qualification of the complete candidate.

## What the debugger establishes

One bounded, read-only capture samples a normal **32 KiB `MEMF_CLEAR` allocation**,
with exactly 568 bytes still to clear. Source-name bytes ahead of its cursor are
old contents, not evidence of heap corruption. The exact executable was rebuilt
byte-for-byte with labels before mapping the sampled stack:

`imports.referenceNamespace.find → lexical_store.readEntry → readPayload → locate
→ prepared_store.acquire → resident → memory.reserveExact → grow → AllocMem`.

This is localization evidence, not completion. Source review also shows bucket
rehashing reads names and dirties scattered spilled entries to update chain links.
Ordinary mismatches acquire full entries just for chain/length/namespace metadata.
A four-page cache cannot absorb arbitrary access over many sealed pages. These
paths and allocation traffic support a lookup-induced churn hypothesis; the one
sample does not attribute all elapsed time or prove the release run would finish.

## Current qualification and distance to target

**The current-source full self-host does not pass.** The instrumented attempt
reaches global binding, then exits 20 after 321.888 s with missing symbol
`experimental.amigaos.binary_imports.capturevalue.a6`. That duration is a failed
attempt, not a completed assembly measurement. New storage-access macros combine
a qualified displacement with a register parameter; reduced qualified-offset and
struct-field cases reproduce the failure. Live Rust oracles accept both. Simple
register and unqualified displacement controls pass. Whether this binding gap
predates sealing has not been established.

Package inspection places LEA displacement selection in tuple register/value/arity
projections. A speculative raw-call change did not repair it and was reverted.
Next localization is expanded operand tokens and `tupleLeaf → rawTuple`, not a
CPU-specific workaround. No successful current-source full self-host is claimed.

The original 16-case module matrices pass in both resident and disk modes. The
expanded resident matrix passes those 16 and fails the two new qualified-register
macro cases; the expanded disk matrix was not run after that failure. Coverage includes typed configuration,
active includes, templates, colliding spellings, late template use, diamonds,
multiple modules per file, section maps, forward layout and invalid parameter sets.
The lexical component exercises more than 3 MiB of private backing, eviction, late
writes, aliases, physical arena retirement, sticky errors and cleanup. Fresh native
statement-exposure and full word-sized identity-growth controls pass. Binding/remap
host checks and live Rust oracles pass. No broad Rust workspace gate or A6000 run
was made. Formatting, architecture, no-growth, instrumentation and fresh-proof
guards pass; the workflow suite passes 152 tests. CCR scanning reports three
pre-existing mechanical findings and advisory call-return checks.

**No 2 MiB fit is claimed.** Frozen peak is **6.59 MiB**, exceeding the entire target
by **4.59 MiB**, before executable, stack and OS. Adding the measured instrumented
linked reservation (780,912 B) reaches at least **7.33 MiB** before stack/OS.
Prepared/assembly live heap remains 2.83 / 4.71 MiB. The next frontier must address
these retained owners as well as preparation peak; disk storage alone cannot fit.

## Decision and next step

Keep this as a recovery checkpoint, with the pre-sealing executable and contiguous
control retained. Do not promote based only on the heap saving. Stop for review of
the dramatic runtime cost before implementing another design.

First repair the qualified-register macro binding gap required by the new access
macros and rerun the focused matrix. Then consider **reusing page buffers on eviction**. The current
cache frees a victim and allocates/clears another 32 KiB buffer on the next fault.
A private helper can flush an unpinned victim and transfer its pointer/capacity to
that row, preserving both rows' logical metadata. It needs no spare allocation or
larger cache, so it would preserve the 128 KiB payload bound and measured heap saving.
It targets repeated allocation/zeroing; it does not remove disk reads/writes or
lookup misses. Compare exact allocation counters first, then runtime and peak.
This change is proposed, not implemented; no full runtime recovery is promised.

A second candidate is a **14-byte bridge** with authoritative chain link and
namespace-tagged spelling hash. Rehash would update resident links, and nonmatching
lookups would avoid cold entry/name acquisition. Full spelling comparison still
handles true collisions. At 65,535 identities it adds 393,210 B. If the measured
peak frontier stayed unchanged, saving would fall from 1.541 to approximately
**1.166 MiB**; that is a projection, not a new bound or guaranteed result. Prefer
buffer reuse first because it directly addresses the sampled allocation without
consuming the RAM saving. Both candidates require review before implementation.

Maintain prior decisions: adaptive symbol buckets, guarded release telemetry,
chunked captures, bounded prepared records and finished-configuration retirement.
Macro-only deferral/audits, token/fragment prototypes, raw include reuse and prepared
leaf caches remain removed. Object-local IDs and relocatable near-output objects
remain future work; parity stays paused.

## Evidence

Images, cases, receipts, decoded MEMD, smaller controls and debugger artifacts are at
`/tmp/opforge-module-sealing-20261018`, outside `target`. `prepare_cases.py`,
`run_native.py` and `summarize.py` record this batch's reproduction. Frozen baseline
images remain at `/tmp/opforge-prepared-objects-20261018/integration-{release,memory}`.
The preceding capacity census is at `/tmp/opforge-module-capacity-20261018`.
These are local diagnostic artifacts, not repository dependencies. Git archives
superseded implementation detail and experiment chronology.
