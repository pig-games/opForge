# Native runtime and memory diagnosis

## Current assignment and decision points

The 2026-10-18 course correction replaces the former product target with
**8 MiB total machine RAM (8,388,608 B)**. Lower memory remains useful, but speed
may use the available budget. Parity remains paused. No `.ofm` implementation,
object-local identities, new cache policy or observer framework is authorized in
the budget/repeatability step. Erik has since authorized starting the
[first-reference module approach](native-fragment-preparation.md#first-reference-module-assembly-proposal)
alongside the unchanged hardware batch. Publication/reuse and a restricted native
source-to-image leaf producer are native-tested; general module lowering and CLI
activation are still pending. The [component measurements](native-fragment-preparation.md#native-source-producer-restricted-leaf-boundary)
do not establish a whole-build runtime or peak-memory reduction. The remaining
curve/bridge/adaptive-policy steps below are retained candidates, not changes already
made or a reason to mix results from different storage implementations.

Work in this order, keeping one coherent commit and unchanged-input/live-Rust
comparison per step:

1. Establish a real 8 MiB FS-UAE profile under minimal Shell and Workbench.
   Measure free RAM and existing resident/disk behavior; confirm the process/OS
   split with Erik before using it to choose storage budgets.
2. Prepare serial A6000 repeatability cases: unchanged release resident/disk,
   three runs each on frozen 214-file input; current-source disk once and
   instrumented/progress disk once. Preserve environment, scratch and last marker.
   Audit naming, clock/address/task dependencies, eviction and retry/pin loops.
   Fix an identified hang separately; unexplained variance stays unexplained.
3. Measure the memory/speed curve under 8 MiB, independently ablating capture,
   prepared and lexical backing. Sweep lexical cache 128/256/512/1024 KiB and
   capture payload budgets. Add only the agreed narrow gated I/O counters;
   confirm final candidates on A6000, three runs each.
4. Add resident chain/hash metadata within the planned 393 KB maximum. Repeat
   the curve. If best-budget sealing still costs over about 15% for its roughly
   1.2 MB saving, restore the pre-sealing default and remove sealing from the tree.
5. Select storage/cache budgets from free memory and staged input size, retaining
   `--capture-dir` override. Qualify both automatic choices and clean allocation
   failure. No benchmark/path-specific selection.
6. Refresh the existing hotspot profile at two sample settings. Choose later
   runtime work from exclusive costs and exact counts, not the October 13 table.
7. Census assembly heap and executable/package costs; report cheap candidates
   without implementing them.
8. Estimate incremental reuse upper bound and compare full relocatable module
   objects with Hunk separate assembly/linking. Analysis only; Erik decides
   whether `.ofm` continues.

State expected savings before each optimization; discard changes below about 1%
of the target resource by exact counters. Require at least three timings before
a claim of 2% or more. A6000 is the timing/disk-I/O reference; only a completed
exact native run under the 8 MiB emulator profile establishes fit. Do not overlap
or retry over an incomplete hardware run. At phase completion, consolidate these
three plans here, delete superseded chronology, and propose a remaining time box
and parity-resumption date. The open macro iterator/value controls remain in the
[parity backlog](compact-frontend-vm-boundary.md).

Step 1 establishes the qualification profile; Erik confirmed the process/OS split below. The maintained `68020-8m` profile exposes 2 MiB chip plus
4+2 MiB fast banks; other RAM and RTG are disabled. It pins max CPU speed and
disables cycle-exact execution, so timings are not physical-68020 timings. See
[the qualification recipe](../../agents/rules/fs-uae.md). The first cycle-exact
full trial was deliberately interrupted after discovering the emulator default;
it is neither a memory failure nor a completed timing.

## Step 1: budget qualification

This step changes qualification, not assembler/storage behavior; no runtime or
heap saving is claimed. The reference profile is A4000/68020, max emulation
speed, no cycle-exact execution, 2 MiB chip plus 4+2 MiB fast, no RTG/other RAM,
and 64 KiB Shell stack. FS-UAE 3.1.66 and Kickstart 47.96/Workbench 47.2 report
8,372,224 B registered memory: 2,080,768 B chip and 6,291,456 B fast. The 16 KiB
physical/registered difference also belongs in the OS reserve.

Disposable boots preserve the installed system. Minimal Workbench runs SetPatch
and LoadWB, without installed desktop preferences or WBStartup programs. It must
check LoadWB's exit immediately and retain its success witness before starting
the normal native proof runner. A failed desktop start cannot qualify Workbench.
The budget profile requires a template and canonical generated-config arguments;
selecting an environment label alone is insufficient.

| Environment | Free before assembler load | Largest free block |
|---|---:|---:|
| Minimal Shell, 64 KiB stack selected | 8,161,520 B / 7.78 MiB | 6,082,048 B |
| Minimal Workbench, 64 KiB stack selected | 7,800,856 B / 7.44 MiB | 5,768,160 B |
| Installed desktop, preferences and WBStartup enabled | 3,352,232 B / 3.20 MiB | 2,014,568 B |

The configured-desktop measurement is a separate environment, not the minimal
Workbench baseline. Removing only WBStartup programs still leaves 3,971,648 B
free; preferences/services matter too. The small maintained module control
assembles to the live Rust BIN (104 B) and Hunk (204 B) under each boot tested.
Those small controls do not qualify the full assembler build.

Four release checks use the unchanged current bootstrap (728,656 B,
`fnv1a64:8151a17c6c83a3a9`) on the frozen **214 files / 2,648,242 B** input,
SHA-256 `eae5102bd21b3863105a7f550912193dff8e52c8ba2d6ff11b498392b7dca480`.
Every case assembles the live Rust oracle (708,216 B,
`fnv1a64:27c800763d556961`) before native execution. **All four native builds
fail with fresh START/DONE and exit 20; none produces an exact complete output.**

| Boot | Storage | Observed START to failure | Result |
|---|---|---:|---|
| Shell | Resident | 11.914 s | Rejects in `binary_repeat_inventory.asm` |
| Shell | Disk | 73.374 s | Rejects in `debug/lazy_audit.i` |
| Workbench | Resident | 10.139 s | Rejects in `binary_exvm.asm` |
| Workbench | Disk | 79.685 s | Rejects in `debug/lazy_audit.i` |

These are single failed-run durations, not completed assembly times, speed
comparisons or variance estimates. Disk scratch uses the disk-backed Work:
mount. The generic "unsupported or invalid input" diagnostic does not identify
resource exhaustion adequately.

Separate existing-gate MEMD captures confirm actual allocation failure: Shell
resident rejects a 32,768 B request at a **partial** tracked peak of 7,263,080 B;
Workbench disk rejects a 1,048,576 B growth request, replacing a 524,288 B buffer,
at a **partial** tracked peak of 5,651,096 B. Each records one allocation failure,
profiling flags `0x90`, and zero terminal tracked ownership with allocated/freed
capacity balanced. Neither reaches successful preparation/assembly completion.
These partial peaks are not lower full-build requirements or measured savings.
Growth overlap, untracked OS allocations and contiguous availability matter in
addition to live heap. The separate failure-diagnostic test retains those receipts
with `success: false`, `exact_rust_match: false`, and no verified output; its
passing test status never means successful self-hosting.

**Confirmed split:** reserve **1.5 MiB / 1,572,864 B for OS** and allow at most
**6.5 MiB / 6,815,744 B for the whole assembler process**. The release linked image
reserves 764,484 B, including the 454,890 B embedded package; do not count that
package twice. After a 65,536 B stack, this leaves **5,985,724 B / 5.71 MiB for
heap and other process allocations**. The instrumented image reserves 781,260 B,
leaving 5,968,948 B under the same split. The OS reserve covers registered and
unregistered OS memory, filesystem growth and safety margin; it is not an allowance
for arbitrary installed desktop utilities.

The earlier expanded-profile frozen disk peak of 6,909,720 B already exceeds
the proposed release heap allowance by **923,996 B**, before allowing for
untracked process allocations or fragmentation. That subtraction is a planning
lower bound, not a promise that saving 924 KB will make the full build fit.
**No full-build 8 MiB fit or positive remaining margin is established.**

The last captured A6000 `Avail` reports **2,136,989,696 B** OS-registered memory
(roughly 2 GiB), not an independently verified physical DIMM inventory. Its
launcher cannot restrict available guest RAM. No hardware run was made in this
step; A6000 timings cannot qualify 8 MiB fit. Refresh its environment in step 2.

The separate expanded-profile timeout investigation found the assembler task
waiting in DOS Read during lexical-page restoration, with a processed-packet
marker already set in FS-UAE's filesystem path. This localizes a reply-delivery
stall; it does not establish its cause or a native cache defect. Debugger-enabled
runs also both complete and stall, so enabling the debugger is not a fix. Preserve
this discriminator for the hardware/emulator comparison, without a speculative
production workaround.

Receipts, frozen cases, boot snapshots, configuration captures and failure MEMD
records are outside `target` at `/tmp/opforge-budget-20261018`; prior timeout
captures are at `/tmp/opforge-timeout-20261018`. The budget is confirmed. Next prepare the serial A6000 repeatability bundle;
hardware execution and its results remain outstanding.

## Step 2: hardware repeatability prepared, results pending

This is measurement preparation, with no proposed runtime/heap saving and no
cache-policy change. `scripts/performance/run_a6000_repeatability.py` validates
the frozen schema-1 comparison case and genuine current release/memory exports,
including their package identity, before any network access. It preflights every
derived guest script, including the Amiga Shell line-length limit. The existing
`ash -l` discovery, `Development:` transfer/readback, fresh UUID-bound markers,
explicit exit and complete oracle checks remain authoritative.

The serial order is resident/disk pairs three times on the frozen input, then
current-source disk once, then frozen disk once with the existing light MEMD,
capture accounting and PREPARATION_PROGRESS gates. The latter is diagnostic,
not a release timing. Disk scratch is inside each private Development: run
directory, never assumed to be `T:`.

An exclusive durable journal is claimed before connection and records the actual
remote UUID/directory immediately before Execute. Exceptions, interrupted
connections and incomplete guest protocols leave it unresolved and stop the
batch. The same journal cannot be resumed or retried automatically. Do not bypass
that protection by choosing another output/state path or launching another batch
while a guest may still be running. Completed failures also stop for inspection.
Each completed run records the environment, recent progress markers and remaining
scratch names. Successful cleanup leaves no scratch names; transient names are
not invented from that absence. An unresolved run retains its location for
read-only diagnosis rather than claiming completion from a partial capture.

Only three successful release runs in a configuration produce its min/max,
median and variance. Record guest seconds (one-second resolution), excluding
transfers. A6000 RAM remains unrestricted and cannot prove the 8 MiB target.

Source review found bounded loops, not a proven native hang: private scratch
names use the context address plus a bounded 256-attempt suffix and atomic
CreateDir ownership; allocation/IO errors fail rather than retry indefinitely.
Victim scans and pin exhaustion are bounded; positive partial transfers decrease
remaining bytes. No clock/task-ID dependency selecting assembly or cache behavior
was identified. These are static findings, not proof that every hardware run
terminates. The expanded-profile DOS-read/reply stall remains unexplained pending
the hardware comparison; no speculative production fix was made.

Prepared local bundle: `/tmp/opforge-a6000-repeatability-20261018`.
Fresh host Rust assemblies match both full outputs exactly: frozen 708,216 B
and current-source 728,656 B. The sources, package and current bootstraps are
unchanged; release and memory image identities are those in step 1.
The setup was dry-run locally with no network or hardware execution. Run once
from macOS Terminal:

```sh
sh /tmp/opforge-a6000-repeatability-20261018/run.sh
```

Return `results/state.json` and its referenced local result folders. Step 2 is
not complete until those native results are reviewed. Do not start the memory/
speed curve or chain/hash bridge in place of the missing repeatability evidence.

While that batch runs, the [first-reference module assembly proposal](native-fragment-preparation.md#first-reference-module-assembly-proposal)
formalizes the newly authorized development direction, expected benefits and
explicit resource targets. Its first native component does not alter the CLI or
running cases and supplies no whole-build resource result. Preliminary hardware
timings are identified there as such. Finish the batch before making a repeated
hardware timing claim.

## Existing storage evidence

Qualified-source macro register binding is repaired and the
bounded cache reuses page buffers. The release self-host proofs below match Rust
exactly. The subsequent resident cleanup repair passes the unchanged frozen memory
workload with zero cleanup ownership. Buffer reuse removes 88.41% of allocation
traffic on the frozen workload without changing its peak heap.
Sequential sealing still costs substantial runtime versus the pre-sealing reference.
Small-read cache admission was evaluated and removed: the completed controls show
no measurable saving, and the candidate lacks full-build qualification. Fresh
unchanged-reference retries also time out; the latest full self-host check did not
complete. Earlier successful receipts below do not qualify those incomplete runs.
Parity remains paused. See [storage ownership](native-lazy-preparation.md) and
[the remaining object boundary](native-fragment-preparation.md).

## Measurement contract

Use identical source/package bytes, commands and build settings for comparisons.
Assemble a live Rust oracle for each case; require fresh challenge-bound START/DONE,
explicit zero exit and exact complete output. A timeout or diagnostic capture is
not completion. Keep instrumented memory/work accounting separate from release
timing. Small differences below about two seconds / 2% are not speed evidence.

These runs use the same expanded-memory A4000/68040 FS-UAE profile and 64 KiB guest
stack, serially. They are not A6000 measurements or an 8 MiB qualification. Heap
excludes executable, stack, OS and untracked allocations. No new observer framework
or experiment selector was added.

## Small-read cache admission: not retained

The tested policy copied a first cold lexical read directly into caller-owned
storage, admitting the complete backing page on its second touch. Resident bytes
remained authoritative, including dirty/pinned bytes. The four-page 128 KiB cache
and 32-byte directory rows did not grow; an unused row word held the admission
hint. The hypothesis was a 10–20% release-time saving with unchanged peak heap,
not an established gain.

Fresh native component checks passed cold reads, second-touch admission, pinned
budget exhaustion/retry, dirty bytes, short/failed IO, bounds and complete cleanup.
The maintained module and macro controls were then run separately for release
timing and memory, with identical source/package/command bytes and live Rust BIN
and Hunk oracles:

| Resource | Reference | Small-read candidate |
|---|---:|---:|
| Module release START/DONE | 0.255 s | 0.253 s |
| Module peak tracked heap | 608,768 B | 608,768 B |
| Module allocated/freed capacity, each | 1,242,696 B | 1,242,696 B |
| Macro release START/DONE | 7.839 s | 7.957 s |
| Macro peak tracked heap | 2,173,848 B | 2,173,848 B |
| Macro allocated/freed capacity, each | 5,579,328 B | 5,579,328 B |

All eight controls completed with exact outputs, explicit exit zero, zero cleanup
ownership and no accounting/allocation diagnostics. Deterministic language-work
and storage-accounting fields agree; timing differences are below the acceptance
threshold. These small controls do not establish large-workload cache hit rates.
The candidate release image grew 220 B.

The frozen full-build release reference completed in 191.742 s. The candidate was
still incomplete at an observed 11 min 15 s after emulator launch; it was later
interrupted when terminating its scheduling process also ended the active child.
That is neither a finite native duration nor a completed correctness comparison.
A second run of the unchanged frozen release reference timed out after 600 s.
The separate instrumented reference timed out after 1,200 s without a final memory
receipt. Full-build timing is not repeatable enough for attribution. There is
**no completed full-build candidate timing, memory comparison or full self-host
proof**. No new IO observer was added; extra filesystem-call overhead remains a
hypothesis rather than measured attribution.

The candidate did not establish the required saving or qualification. Its API,
integration and probes are removed; the original cache policy remains, without
another selector or fallback. This decision does not diagnose the incomplete runs
as a specific cache or ownership bug. Images, candidate source, comparison assertions
and receipts are retained outside `target` at
`/tmp/opforge-small-read-20261018`.

The retained current-source release reference was also checked in disk mode:
**223 files / 2,783,840 B**, manifest `fnv1a64:9e2d20b52db3e4eb`, with bootstrap
and fresh Rust oracle both **728,656 B**, `fnv1a64:8151a17c6c83a3a9`. It timed out
after 600 s without DONE, a recorded native exit or a complete output. This step
therefore does **not** qualify a full current-source self-host, even for the
retained reference. Localize this repeatability failure before another cache
policy change; do not interpret it as a measured speed regression or enlarge the
cache to mask an unlocated failure.

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

The original comparison below uses the same 214-file frozen input above, pre-repair
release/instrumented executables, embedded package, hardware profile and 64 KiB
stack. Only the existing storage policy differs. Resident mode omits
`--capture-dir Work:`; disk mode enables raw-capture, prepared-record and lexical
backing together, including their existing scheduling differences. This does not
isolate lexical paging alone. No native code or instrumentation changed.

| Resource | Resident before cleanup repair | Disk-backed |
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

All four original runs complete with fresh challenge-bound START/DONE, explicit
exit zero and the exact full 708,216-byte live Rust Hunk. Source-read, packed-source,
expression-compilation, evaluation and compiled-program counters agree. The
original resident memory run left 22,736 B tracked live: allocated/freed capacity
was 63,745,344 / 63,722,608 B. It was not clean terminal ownership accounting.
Resident `frontend.indexBlocks` repurposes the retired name-store `First` block
for its numeric index. Scope cleanup omitted that owner; disk cleanup already
released its separate `PreparedIndex`.

Scope cleanup now releases the retired name arena, including that numeric index.
A fresh native memory rerun uses identical frozen sources/package, resident policy,
telemetry flags, emulator and stack, with only the repaired bootstrap changed
(`fnv1a64:407da09829e94d1a`). It completes with explicit exit zero and exact full
708,216-byte live Rust output. Results:

- Terminal tracked ownership: **22,736 -> 0 B**.
- Allocated/freed capacity: **63,745,344 / 63,745,344 B**; exactly 22,736 B more freed.
- Peak tracked heap: **18,831,968 B / 17.96 MiB**, unchanged.
- Prepared/assembly live heap: **2,969,088 / 4,943,360 B**, now matching disk.
- No accounting diagnostics, allocation failures or profiling errors; language-work
  counters and total allocated capacity are unchanged.

The instrumented executable grows 12 B, linked reservation 8 B; no retained owner
or per-record work is added. Release timing was not repeated for this cleanup fix,
so the original comparison above remains the release timing reference. The rerun
qualifies this frozen workload, not a new full current-source self-host. Evidence
and comparison assertions are at `/tmp/opforge-resident-cleanup-20261018`.

These frozen-input times must not be mixed with the 118.617 / 225.499 s
current-source release results below: that newer input has 223 files. The prior
instrumented current-source disk timeout remains unresolved. Both storage modes
do not establish 8 MiB fit after executable, stack and OS are included.

## Current-source full self-host before the cleanup repair

Qualified source: **223 files / 2,783,726 B**, manifest
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

**These expanded-memory receipts do not establish 8 MiB fit.** The last completed
frozen disk memory measurement is **6.59 MiB**. That instrumented executable's
linked reservation adds 781,252 B, reaching
at least **7.33 MiB** before stack/OS. Prepared/assembly live heap remains
2.83 / 4.71 MiB. Buffer reuse reduces traffic, not these retained owners.

The ordered work above governs the next changes. Resident chain/hash metadata
is planned to avoid cold entry/name acquisition during failed
lookup and rehash. A 14-byte identity bridge would cost another 393,210 B at 65,535 IDs; an unchanged peak would leave about
1.166 MiB of the measured sealing saving. This is a projection, not implementation
or approval for early implementation. Real object-local IDs and relocatable
near-output module integration remains unimplemented beyond the component described
in the linked proposal; macro-only deferral/audits and retired
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
