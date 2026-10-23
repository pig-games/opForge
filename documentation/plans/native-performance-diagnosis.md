# Native runtime and memory status

## Current measured baseline

The 2026-10-23 bare-CLI external-package release completes cold and unchanged
incremental self-hosts in **275/58 s** on the established 8 MiB installed-Workbench,
plain-background FS-UAE setup, with a 64 KiB stack and unrestricted CPU speed.
Cold runs `opforge src --rebuild`; warm runs `opforge src`. Both use the default
working-directory `build/scratch` and `build/ofm` storage. Cold rebuilds all 190
dependency modules; warm reuses all 190. Both exit zero with fresh completed proof
and exact entire-output equality with live Rust and the bootstrap.

| Identity or measurement | Current bare-CLI snapshot |
| --- | --- |
| Source inputs | 270 files / 2,918,308 B |
| Source manifest | `fnv1a64:9e447609ba8deed3` |
| Hunk executable | 348,824 B; `fnv1a64:039b39206ed8ab98` |
| Linked static reservation | 389,968 B |
| Separate m68020 package | 478,042 B |
| CLI elapsed totals, cold / warm | 275.20 / 57.58 s |
| Shared `.ofm` payload store | 190 images / 1,654,720 B, each with an `.ofi` companion |

Completion proves fit for this particular 8 MiB desktop and workload. It provides
no new peak-heap measurement, physical 68020 timing, isolated speedup or 2 MiB fit
claim. The bare-CLI bundle awaits physical A6000 testing. See the
[native assembler status](native-runtime-reset.md#current-product-state) for
current CLI, package deployment and qualification details.

The completed lazy/work/template, candidate and packing shadow observers and
associated comparison tooling were removed after caller review. General memory,
binding, capture, capture-lifetime component, hotspot, template-work and VM
telemetry remain. The release executable is byte-identical to the pre-cleanup
348,824-byte build (SHA-256
`bbcdd6344f6ba6dd310910278655f7583c3d6f10899c282d63785ff4144cfd4c`).
The self-host source set shrank by eight files / 30,802 B. The earlier identical
release image assembled its pre-cleanup source in 279/58 s; the current 275/58 s
observation does not establish an isolated speedup. No heap saving is inferred
from removing code that was already gated out of release builds.

The preceding external-package snapshot passed the physical A6000 in
**1,066/90 s** cold/warm. It used 278 inputs / 2,947,664 B, source manifest
`fnv1a64:10ba03505347550b`, and a 348,752-byte executable. Both phases had fresh
completed protocol, zero exit/copy status and exact entire Rust Hunk equality;
190 modules rebuilt cold and all 190 were reused warm. Hardware RAM was
unrestricted/unmeasured. Its case identity is
`de108f5e4239829bdc6fcd5326bfece58602155b0bb368e8278974b0895d897c`.
Different inputs and machines prevent interpreting this pair and the current
FS-UAE pair as a performance comparison.

## Memory budget and measured limits

The budget remains **6.5 MiB for the process and 1.5 MiB for AmigaOS** on an
8 MiB machine. Accounted heap plus static reservations plus stack is a floor,
not a total process peak: loader and untracked allocations remain outside it.
Current completion must not be combined with older heap figures to manufacture
a current memory estimate.

The latest retained complete heap-accounting pair belongs to an earlier
**825,428-byte embedded-package output**, not the current external-package image:
275 source files / 3,418,882 B, manifest `fnv1a64:9667cf7b90951a3b`, embedded
m68020 package `fnv1a64:390f6f44df85279b`. Its constrained profile had 2 MiB chip
plus 6 MiB fast RAM, installed Workbench 47.2 and startup utilities, plain
background, disk-backed capture and a 65,536-byte stack. Before that accounting
qualification, `Avail` reported 6,716,384 B free (2,017,352 chip / 4,699,032 fast),
with a 4,595,848 B largest fast block. These are that desktop's pre-run readings,
not the current build's remaining margin.

| Earlier qualified metric | Cold | Unchanged incremental |
| --- | ---: | ---: |
| Separate release duration | 284 s | 61 s |
| Modules rebuilt / reused | 188 / 0 | 0 / 188 |
| Peak tracked heap, accounting pair | 4,146,008 B | 3,671,120 B |
| Accounted process floor | 5,092,448 B (4.86 MiB) | 4,617,560 B (4.40 MiB) |
| Cumulative owned allocation/free | 304,339,376 B | 148,671,432 B |
| Terminal tracked ownership | 0 | 0 |
| Allocation failures / profiling errors | 0 / 0 | 0 / 0 |

The instrumented image reserved 880,904 B. Both accounting runs completed with
exit zero and exact full output; their durations are excluded from release timing.
The cold floor leaves 1,723,296 B below the process budget before untracked costs.
Constrained completion independently establishes fit for that desktop/workload,
not resident capture or arbitrary desktop programs. Current storage contracts
and the completion boundary are described in the
[interface qualification](native-module-interfaces.md#8-mib-workbench-qualification).
No 2 MiB completion is qualified.

## Retained architecture and performance decisions

Stored dependency interfaces, disk-backed payloads and default incremental reuse
are maintained. The root remains in the shared final driver; consumer windows
must not become complete resident semantic tables. The disk-backed reference and
`--reference-modules` serve distinct reference purposes and remain useful for
correctness comparisons. See [module interfaces](native-module-interfaces.md) and
[`.ofm` numeric contracts](native-fragment-preparation.md) for ownership, format
and capacity limits.

Buffered Hunk output borrows section payloads and streams metadata through the
shared 4 KiB transport buffer, avoiding a second full-size Hunk allocation that
prevented constrained completion. The serializer remains a differential-validation
reference. The streaming emitter's caller-owned 44-byte frame borrows stable
parts, fixups and payloads until synchronous completion; it allocates no heap.
No isolated runtime gain is attributed to
this allocation change.

### Retained header-read repair

Covered object-header ranges come from retained link metadata; the existing
reader window serves the assembly directory. A cold interface selection that
fits the window needs one disk range read instead of three. This adds no heap
capacity or resident semantic table.

The identical-input release comparison assembled 265 files / 3,354,956 B,
manifest `e245b6a27e560e09`, package `390f6f44df85279b`, and an 816,564-byte Rust
output. It measured **299.902 s reference versus 275.574 s repaired**, saving
24.329 s / 8.1%; another repaired observation was 274.660 s. Separate four-gate
MEMD runs both peaked at 4,604,064 B, allocated/freed 219,973,288 B and ended
with zero ownership. Linked reservations rose 16 B, from 870,312 to 870,328 B.
Source work counts matched. These expanded-memory emulator observations establish
a gain on that frozen input, not a speedup or heap value for the current build.

## Enduring earlier decisions

The following comparisons explain retained tradeoffs and rejected approaches.
They are historical evidence, not a timing series for the current executable.
Unless otherwise stated, they used expanded-memory A4000/68040 FS-UAE and a
65,536-byte stack; release timing and four-gate memory runs were separate.

### Earlier structural comparison

Stored interfaces reduced peak tracked heap **32.4%**, from 6,814,688 to
4,604,064 B, and cumulative allocation **30.1%**, from 314,495,120 to 219,973,288 B.
They cost runtime: median full-build release time rose from **202.576 to
299.425 s (+47.8%)**. Reference samples were 200.991/202.576/203.039 s;
stored-interface samples were 299.425/298.491/304.256 s.

Both engines assembled the same 265 files / 3,354,956 B, manifest
`e245b6a27e560e09`, package `390f6f44df85279b`, output digest `cb9f3fc2929747e0`.
The command used source-defined Hunk output, `-M src -I src/debug` and
`--capture-dir Work:`, without a duplicate CLI Hunk request. Both produced the
same full output. Instrumented linked reservations rose from 858,396 to
870,312 B; heap plus reservations plus stack fell from 7,738,620 to 5,539,912 B.
These unrestricted runs did not themselves prove 8 MiB fit.

Work counts matched: 146,757 tokenizations, 293,561 binding/record calls,
151,872 expression preparations and 189 discoveries. Instrumented binding/raw
record time rose from 52.39 to 149.53 s while source I/O and other preparation
fell from 115.22 to 100.19 s. This localizes the cost to binding without
identifying individual lookup costs. Header reuse recovered part of it.

### Public-name indexing decision

Two sorted public-export ordinal arrays preserved matching and numeric identity
but added format and lookup machinery. On 186 frozen objects / 10,518 public
names, storage grew 85,632 B (5.2%). A synthetic 19,562-query sweep predicted
93.05% fewer key comparisons but 27.1% more character comparisons; that model
was not an observed native call distribution.

The identical-input disk-backed release pair was **277.572 s reference versus
284.816 s indexed**. Both used repaired source manifest `13b909ce22ee5f6e`,
package `390f6f44df85279b`, explicit CLI Hunk output in addition to source output,
`--capture-dir Work:`, all 186 stored dependencies and an 816,576-byte Rust
oracle. Work counts and exact outputs matched. Separate tracked heap rose
384 B (4,604,064 to 4,604,448 B); accounted process cost rose 1,356 B.
One pair establishes no gain, not a stable regression percentage. The trial was
removed; checkpoint `017ecf0b` preserves it. Numeric entity/member binary search
was also runtime-neutral and removed. Further indexing needs actual call/refill
cost evidence; the synthetic sweep is insufficient.

### Background-payload trial decision

The trial overlapped at most one existing image allocation of at most 64 KiB
using asynchronous DOS payload packets; open, close and rename stayed synchronous.
A synchronous-retention control isolated overlap from reads served by retained
image bytes. On the same repaired frozen input, package, command and 816,576-byte
oracle used by the name-index comparison, release observations were **280.360 s
synchronous versus 279.953 s background**: 0.407 s / 0.15%, insufficient evidence
of useful gain. All 186 objects (1,647,692 B) were byte-identical; work counts
matched and terminal ownership was zero. The accounted process increase was
3,074 B. The code/tests were retired; checkpoint `bdc65e13` preserves the trial.
Do not expand it into a worker process without a demonstrated hardware opportunity.
The delivered A6000 overlap trial used different S-control input manifest
`82f136413f41ee5c`; no completed paired hardware result is recorded here.

### A6000 hardware result

The older 265-file, 816,564-byte stored-interface build completed on A6000 in
**945 s release / 971 s instrumented**, with exact Rust output, fresh completed
protocol and exit zero. Case SHA-256 identities were
`fb9ddbce22b5b1a76be7f8e1f98cf687ca6d35cdd6019ac7581289652f43b4c7`
(release) and `af83221f7f5427b40a88d191501b3d3c9853da07f2ff4298424f216638a0379b`
(memory). Hardware tracked peak heap and allocation/free totals matched the
4,604,064 B and 219,973,288 B emulator measurements. Binding/raw records consumed
571.69 s of the instrumented run; interface reads are included in callers.
There was no matching old-implementation hardware baseline or constrained RAM;
these results establish neither speedup nor 8 MiB hardware fit. Remote captures
were not independently re-read for this record. The 26 s observed instrumentation
difference is one pair, not a stable overhead estimate.

## Remaining boundaries

Current priorities suggested by the evidence are measuring current peak heap,
qualifying the bare CLI on A6000, and locating actual binding lookup/refill costs
before another indexing or overlap experiment. These are opportunities, not
activated implementation tasks. The 2 MiB target and full language/CPU/output
parity remain open; current source contracts and
[qualification status](native-runtime-reset.md#current-product-state) govern
specific gaps rather than an older experiment's parity inventory.

Do not compare different source snapshots, capture modes, output requests or
machines as an isolated optimization. Fresh complete output, DONE protocol and
explicit exit status remain required; timeout, crash or partial capture is not
completion. No full workspace-suite claim follows from these measurements.
The [native parity contract](../../agents/rules/native-rust-parity-porting.md),
[FS-UAE guide](../../agents/rules/fs-uae.md) and
[performance reference](../performance/vm-efficiency.md) define the proof and
timing safeguards. Git history retains superseded experiment detail.
