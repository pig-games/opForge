# Native runtime and memory status

## Current result and decision

Stored dependency interfaces and default-on incremental reuse now pass complete
self-builds under **8 MiB total RAM**, with installed Workbench and startup
utilities, a plain background, and disk-backed capture. The root remains in the
shared final driver. With CLI progress enabled, the latest release cold build
takes **288 s (4m 48s)** and the unchanged incremental build takes **60 s (1m)**. Cold rebuilds all 189
dependency modules; warm reuses all 189 and rebuilds none. Both exit zero and
reproduce the complete **826,760-byte Rust oracle and bootstrap exactly**, with
fresh completed proof. The CLI reports total elapsed times of 288.00 s and
60.32 s. This build contains 277 source files / 3,427,689 bytes, manifest
`fnv1a64:4a1bf704ef6a06ff`, with the unchanged embedded m68020 package
`fnv1a64:390f6f44df85279b`. It is a new source build, so the earlier 284 s / 61 s
pair is not an isolated measurement of reporting overhead.
These FS-UAE durations have one-second guest-clock resolution and unrestricted
CPU speed; they are not physical 68020 timings.

Before progress reporting, a separate complete accounting pair for the
825,428-byte build peaked at **4,146,008 B (3.95 MiB)** tracked
heap cold and **3,671,120 B (3.50 MiB)** warm, with zero allocation failures,
profiling errors and terminal ownership. Instrumented static reservations plus
heap and 64 KiB stack give process floors of **4.86 MiB** and **4.40 MiB**, excluding
loader and untracked costs. The cold floor leaves about 1.64 MiB below the
6.5 MiB process budget before those costs. Constrained emulator completion
establishes fit for this particular OS/workload; the default resident-capture path
remains outside this claim. Those heap peaks have not been remeasured on the
new progress-reporting source build. Reporting itself allocates no heap: its two
fixed frames occupy 1,360 bytes. The new release links 868,488 bytes of static
reservations and its executable grows by 1,332 bytes.

Buffered Hunk output is the default. It borrows existing section bytes and uses
the shared 4 KiB transport buffer for metadata, removing the second full-size Hunk
allocation that prevented constrained completion. The unchanged serializer is
retained for validation and differential tests. The executable grows by 916 bytes;
no isolated runtime improvement is claimed against earlier failed builds or the
older expanded-memory workload. Input identities, accounting limits and focused
proof are in [stored module interfaces](native-module-interfaces.md#8-mib-workbench-qualification).

The earlier A6000 reference pair remains **916 s cold / 98 s incremental**, with
187 modules and an 824,512-byte executable; hardware RAM was unrestricted. It is
a different frozen source/build, not a hardware result for the new streamed writer.
Its case identity and expanded-profile baseline are recorded in the
[incremental qualification](native-module-interfaces.md#incremental-qualification).

The retained header-read repair separately saved **8.1%** on its older frozen
input, without increasing tracked heap. Its unchanged-input comparison below
is distinct from the incremental cold/warm result.

The bounded background-payload writer also passed its complete own-source check,
but its isolated timing difference was only 0.15%, below useful evidence. Its code
and tests are retired from the working tree; checkpoint `bdc65e13` and the delivered
frozen A6000 trial preserve the experiment. Do not expand it into a worker process
without a demonstrated hardware opportunity. Numeric entity/member binary search
was also runtime-neutral and removed. Public-name indexing is retired too: the
fresh disk-backed release pair was 277.572 s reference versus 284.816 s indexed,
with additional object bytes and no demonstrated full-build saving.

Stored interfaces still trade runtime for a substantial memory reduction against
the earlier private compiler/image implementation. Preserve that disk-backed
reference and `--reference-modules`; they are distinct reference mechanisms. See
[stored module interfaces](native-module-interfaces.md) and
[`.ofm` numeric contracts](native-fragment-preparation.md).

## Retained header-read repair

The reader serves covered object-header ranges from retained link metadata and
uses its existing window for the assembly directory. A cold interface selection
that fits the window needs one disk range read instead of three. No additional
heap capacity or resident semantic table was introduced.

Both engines assembled the same 265 files / 3,354,956 bytes, source manifest
`e245b6a27e560e09`, package `390f6f44df85279b`, and 816,564-byte Rust output.
The direct release pair was **299.902 s reference versus 275.574 s repaired**:
24.329 s saved, or 8.1%. The other repaired observation was 274.660 s.

Separate four-gate MEMD runs both peaked at **4,604,064 B**, allocated and freed
219,973,288 B, and ended with zero live ownership. Linked reservations increased
by 16 B, from 870,312 to 870,328 B. Source reads, tokenization/binding calls,
expression counts, and compiled-program bytes were identical. This is less
interface-read work, not omitted assembly work.

The own-source proof uses the repaired source manifest `13b909ce22ee5f6e`,
265 files / 3,355,329 bytes, and output `0a3ca3d4877cedb5`. Its timing is separate
from the fixed older-source comparison above.

## Public-name indexing decision

The trial stored two sorted arrays of public export-row ordinals, for folded full
and leaf names. It preserved exact matching, ambiguity, private closure exclusion,
numeric identities and the existing 64 KiB reader capacity. In-place heapsort
needed no additional sorting allocation. The working tree retains the original
name lookup and OFMS layout; checkpoint `017ecf0b` preserves the trial.

The capacity calculation used 186 frozen objects and 10,518 public names. Index
storage was **85,632 B**, including the larger headers; the largest addition was
3,792 B and every resulting interface still fit one reader window. A constructed
19,562-query sweep predicted 93.05% fewer key comparisons, but 27.1% more compared
character pairs because the old scan rejects unequal lengths immediately. This
was a model, not an observed distribution of native lookup calls.

Both engines assembled the same repaired source snapshot, package
`390f6f44df85279b`, command and 816,576-byte live Rust oracle. These runs used
`--capture-dir Work:`, stored all 186 dependencies, and retained no fallback.

| Identical-input full build | Header reference | Name-index trial |
| --- | ---: | ---: |
| Release START/DONE time | 277.572 s | 284.816 s |
| Peak tracked heap, separate MEMD run | 4,604,064 B | 4,604,448 B |
| Linked reservations, instrumented image | 870,328 B | 871,300 B |
| Cumulative allocation and free | 220,058,968 B | 220,396,776 B |
| Persisted `.ofm` bytes | 1,647,692 B | 1,733,324 B |
| Terminal owned bytes | 0 | 0 |

The release pair observed 7.244 s / 2.61% more time. It establishes no speedup;
one pair does not establish a stable regression percentage. Separate instrumented
binding clocks were 142.469 versus 141.727 s, while source I/O and other preparation
grew from 94.400 to 98.245 s. Work counts matched. The measured heap increase was
384 B and the accounted process increase was 1,356 B; object storage grew 5.2%.
The modeled row reduction did not justify retaining the extra format and code.

The indexed executable separately completed its entire own-source self-host in
**276.192 s (4m 36s)**, producing its **817,564-byte Rust oracle and bootstrap
exactly**, with fresh protocol and exit 0. That snapshot has 267 files /
3,362,867 B, manifest `7a42a50e2c49e8e5`, output `a13bef64b545b64c`. Its time is
not an identical-input comparison with the reference. Native resident/disk
component checks, cross-window and malformed-index checks, small module and
macro-publication comparisons, and an audit of all 186 written indexes passed.

Two preliminary resident controls are excluded from this disk comparison. One
candidate disk attempt hit the independent 300,000 ms overall runner limit and
supplied no completion or timing; its fresh retry passed with both overall and
post-start limits at 900,000 ms. No timeout is counted as native completion.
The comparison-summary decoder now preserves the raw native capture mode instead
of overwriting it with a manifest default; a regression test covers this.

Measurements used the unchanged expanded-memory A4000/68040 FS-UAE configuration
and 65,536-byte stack, with release and memory measured separately. That header
reference's accounted process floor is 5.28 MiB against the 6.5 MiB process budget; loader,
OS and untracked costs remain unqualified. No 8 MiB fit or A6000 result is claimed.
Further lookup work needs evidence of actual call/refill costs; the synthetic
full-coverage name sweep is insufficient to select another optimization.

## Background-payload trial decision

The trial queued at most one existing image allocation of at most 64 KiB. A private
reply port handled asynchronous DOS payload packets; open, close and rename stayed
synchronous. A separate synchronous-payload control retained the same image and
delayed publication, isolating overlap from dependency reads served from that image.

Both controls used the repaired frozen source manifest `13b909ce22ee5f6e`, the
same package and command, and the same 816,576-byte Rust oracle. Release observations
were **280.360 s synchronous versus 279.953 s background**: 0.407 s / 0.15%.
One pair at this scale establishes no performance gain. The synchronous-retention
control's difference from the 283.966 s immediate-release reference is also too
small to establish a retention benefit.

| Separate memory measurement | Header reference | Background trial |
| --- | ---: | ---: |
| Peak tracked heap | 4,604,064 B | 4,604,098 B |
| Cumulative allocation and free | 220,058,968 B | 220,059,002 B |
| Linked reservations, instrumented image | 870,328 B | 873,368 B |
| Terminal owned bytes | 0 | 0 |

The net accounted process increase was 3,074 B. Work counts matched, all 184 queued
writes retired, and two oversized images used synchronous writes. All 186 `.ofm`
files, totaling 1,647,692 B, were byte-identical to the reference; payload
reads/writes/cache hits remained 178/186/1,665. No capture scratch files remained.

The trial's separate complete own-source self-host took **281.456 s (4m 41s)**,
reproducing its **817,780-byte Rust oracle and bootstrap exactly**. That source
snapshot has 267 files / 3,372,126 bytes, manifest `23fbbb6943c87f25`; its time must
not be compared as if it were the reference's unchanged-input build.

The A6000 write-overlap bundle uses identical **S-control** input for its paired
engines because the maintained hardware runner takes the baseline bundle's own
source as its workload. Its source manifest is `82f136413f41ee5c`, different from
the FS-UAE pair. Hardware results for this trial remain pending; do not compare
absolute times across these different inputs or machines.

## Earlier structural comparison

Both implementations assembled the same frozen 265 files / 3,354,956 bytes,
source manifest `e245b6a27e560e09`, with package `390f6f44df85279b` and output
`cb9f3fc2929747e0`. Only the bootstrap implementation changed. The old bootstrap
also produced the new release exactly; its run is a full-build comparison, not
a fixed-point self-host of its own older source.

The command used source-defined Hunk output, with `-M src -I src/debug` and
`--capture-dir Work:`. It did not add a duplicate CLI Hunk request. Measurements
used the same FS-UAE configuration and a 65,536-byte stack. Release timing is
host-observed START/DONE for the entire assembly command, excluding emulator
startup. Memory came from separate runs with the same four-define MEMD profile.
These are emulator measurements, not calibrated hardware timings.

| Full build | Prior implementation | Stored interfaces | Change |
| --- | ---: | ---: | ---: |
| Total release time, median of three | 202.576 s (3m 23s) | 299.425 s (4m 59s) | +47.8% |
| Peak tracked heap | 6,814,688 B | 4,604,064 B | −32.4% |
| Cumulative allocation | 314,495,120 B | 219,973,288 B | −30.1% |
| Linked reservations, instrumented image | 858,396 B | 870,312 B | +11,916 B |
| Heap peak + linked reservations + stack | 7,738,620 B | 5,539,912 B | −28.4% |
| Terminal owned bytes | 0 | 0 | balanced allocation/free |

Reference release samples: 200.991, 202.576, 203.039 s. Stored-interface samples:
299.425, 298.491, 304.256 s. The new release bootstrap is 816,564 B versus
804,876 B for the old bootstrap: 11,688 B of executable growth. Both produced the
same current output during the comparison.

The process budget remains **6.5 MiB**, leaving **1.5 MiB for AmigaOS** on an
8 MiB machine. The new accounted process floor is 5.28 MiB; it excludes loader,
OS and untracked allocations. These unrestricted runs do **not** establish an
8 MiB fit. Do not substitute this accounting for constrained-profile proof.

## What changed and where time went

Tokenization calls (146,757), binding/record calls (293,561), expression preparation
calls (151,872), and discovery calls (189) were identical. Reduced storage does
not come from omitting source work. Existing instrumented stage clocks show:

| Stage | Prior implementation | Stored interfaces |
| --- | ---: | ---: |
| Binding and raw records | 52.39 s | 149.53 s |
| Source I/O and other preparation | 115.22 s | 100.19 s |
| Tokenization | 9.14 s | 9.21 s |
| Expression preparation | 9.44 s | 9.59 s |
| Module discovery | 18.57 s | 18.76 s |

Stage clocks localize the regression to binding; they do not attribute individual
lookup costs. Code review found repeated linear public-name/template scans and
entity/member row searches, plus reader invalidation when providers change.
Header reuse now recovers part of that cost. Numeric row search and public-name
indexing did not produce a demonstrated gain and are not retained. Further lookup
changes must avoid recreating complete resident semantic tables. No new observer
framework was added.

The candidate wrote 186 `.ofm` objects, totaling 1,647,580 B. Their `OFMA`
interfaces total 1,019,012 B; the largest is 45,964 B. They contain 10,517 value
rows and 28 template rows. Final emission read 178 object payloads and recorded
1,665 payload-cache hits. Interface reads are additional bounded accesses, not
included in that final-payload read count. Packed source was 578,506 B and total
source bytes read were 3,584,949. Separate instrumented preflight elapsed time
was 303.609 s; it is not a release timing or fixed-point self-host claim.

## Smaller controls

In the structural comparison, both versions matched the live Rust oracle in the
small multi-module project and the macro-heavy project, including all release
and MEMD cases. Cleanup was zero in both implementations.

| Control peak tracked heap | Prior implementation | Stored interfaces |
| --- | ---: | ---: |
| Small multi-module | 598,784 B | 598,784 B |
| Macro-heavy | 650,072 B | 616,960 B |

Small-project commands sometimes finished before the harness observed START, so
several elapsed values are absent. Macro-heavy median observations were 1.266 s
and 1.016 s. These short controls do not support a runtime-gain claim at the
established timing-noise scale; use them for correctness and resource checks.

## A6000 hardware result

The user supplied a successful uninstrumented disk-backed release self-build
receipt: **945 s (15m 45s)**, explicit exit 0, completed fresh protocol, and an
exact 816,564-byte Rust match. Its case SHA-256
`fb9ddbce22b5b1a76be7f8e1f98cf687ca6d35cdd6019ac7581289652f43b4c7`
matches the delivered 265-file bundle.

The matching four-gate instrumented hardware run completed in **971 s
(16m 11s)**, exit 0, with the same exact output. Its case SHA-256
`af83221f7f5427b40a88d191501b3d3c9853da07f2ff4298424f216638a0379b`
matches the delivered memory bundle. Peak tracked heap was **4,604,064 B**, with
219,973,288 B allocated and freed and zero live bytes at cleanup; those totals
match the FS-UAE result. Preparation took 929.6 s and assembly 40.72 s. Binding
and raw-record processing accounted for 571.69 s, source I/O and other work
308.64 s, discovery 16.94 s, expression preparation 15.15 s, tokenization
11.71 s, and finalization 5.10 s. These clocks include interface reads inside
their callers; they do not isolate object-write latency.

The observed instrumentation difference is 26 s (2.8%) against the release run;
one pair does not establish a stable overhead estimate. Remote captures were
not independently re-read by this agent. Neither hardware run has a matching
old-implementation baseline or constrained RAM, so neither establishes an
A6000 speedup or an 8 MiB fit.

## Remaining boundaries

The new comparisons used the same unrestricted FS-UAE A4000/68040 configuration
and 65,536-byte stack. Release time is fresh guest START/DONE elapsed time,
excluding emulator startup. Both sides of each new pair used the same explicit
CLI Hunk output in addition to source-defined output; the earlier structural
comparison used source-defined output only. Do not mix these timings as one series.
Memory used separate four-gate instrumented runs. The retained path's accounted
process floor is **5,539,928 B (5.28 MiB)**, leaving about 1.22 MiB below the 6.5 MiB
process budget before untracked/loader costs. An 8 MiB full-build fit remains
unqualified.

The stored-interface baseline passed 24 native comparison cases; this change adds
focused header/lease checks and fresh full-source proofs. Aborted attempts without
DONE/exit/output remain failed diagnostic receipts, not timing samples. Their
noncompletion was not explained or claimed fixed; subsequent fresh qualification
runs completed. No full workspace test-suite claim is made.

The root still owns the final carrier stream. Producer serialization still
overlaps complete interface buffers; consumer windows do not bound that writer.
Compound symbolic-address cells remain unsupported. See the interface note for
the exact ownership and capacity limits. Objects are current-build artifacts,
not qualified persistent incremental caches.

Known parity gaps include native rejection of `.org` inside a section, mapped
placed imports remaining section-relative in Rust flat output, and Rust retaining
an unreferenced named block for a top-level-effects-only import where native omits
it. Native large `.fill` is unsupported under both policies. Separate Rust gaps
remain for 6502 forward-fill without a sizing retry and a loop-count retry with
nonzero labels. This qualification does not claim complete language parity or a
passing full workspace test suite.

Fresh native proof and timing rules remain in the
[native parity contract](../../agents/rules/native-rust-parity-porting.md),
[FS-UAE guide](../../agents/rules/fs-uae.md), and
[performance reference](../performance/vm-efficiency.md).
