# Native runtime and memory diagnosis

## Current assignment and decision points

Reduce full-build runtime and peak memory while preserving shared VM/package semantics
and exact native output. Parity remains paused. The confirmed budget is **8 MiB total:
6.5 MiB for the complete assembler process and 1.5 MiB for AmigaOS**. Full-build fit
remains unproven.

The current private-module CLI integration is an **unpromoted checkpoint**. It publishes
canonical public interfaces, retires temporary private owners and links saved fragments
through the ordinary global assembly path. Unsupported modules replay through the
complete reference path; `--reference-modules` preserves an explicit comparison control.
This is configured-module publication, not activation on the first actual reference, and
it is not persistent `.ofm` caching.

The candidate **failed the acceptance gate of at least 1 MiB net full-build peak
saving**. Component success and a smaller final stream do not justify promotion.
Qualification is complete; save the recovery checkpoint, then review course, scope and
ownership with Erik. No further implementation is planned before that review.

## Private-module CLI capacity gate

The comparison uses the same final instrumented executable for both policies, identical
frozen input (223 files / 2,789,855 B, `fnv1a64:d717c6af1a278e75`) and the same prepared
68020 package (`fnv1a64:415d2172932e243a`). The bootstrap source manifest is
`fnv1a64:5c0e1e383e7298b5`; release/instrumented image sizes are 760,176 / 774,748 B.
Instrumentation uses only the five approved light progress/capture/MEMD defines.

| Same-image memory comparison | Private candidate | Explicit reference |
|---|---:|---:|
| Full: privately compiled modules | 15 | 0 |
| Full: peak tracked heap | 20,633,680 B | 19,364,448 B |
| Full: total allocation/free capacity | 364,024,760 B | 69,935,968 B |
| Full: retained packed records | 2,119,792 B | 2,157,589 B |
| Small: peak tracked heap | 598,784 B | 598,784 B |
| Small: total allocation/free capacity | 1,472,560 B | 1,067,808 B |
| Macro: peak tracked heap | 616,960 B | 616,960 B |
| Macro: total allocation/free capacity | 1,963,648 B | 1,356,144 B |

All six runs completed fresh exit zero, matched their live Rust oracles exactly,
finished with zero ownership and passed cleanup. Full output is 729,140 B. The candidate
saves 37,797 packed bytes but increases peak heap by 1,269,232 B and allocation traffic
by 294,088,792 B. It misses the gate plainly.

Existing full-workload counters show repeated preparation despite only 15 admissions:

| Instrumented work counter | Private candidate | Explicit reference |
|---|---:|---:|
| Tokenization calls | 123,970 | 123,970 |
| Binding/raw-record calls | 306,726 | 247,309 |
| Expression-preparation calls | 187,967 | 128,234 |

Instrumented preparation counters report 221.911 s candidate versus 83.236 s reference.
These are instrumented stage measurements, **not release timings**. The
`source_io_and_other` stage is a mixed accounting bucket, not a measurement of physical
disk I/O. The single release pair is complete on identical frozen files and the same
760,176 B release executable: reference **124.115876834 s**, private **256.462333459
s**. Both complete fresh exit zero and match all 729,140 live Rust bytes; both are
frozen comparisons, not current-source self-host. This one pair shows a 2.07× diagnostic
runtime regression, not a speedup or a three-run median. Together with the failed memory
gate, it calls for a course review before further implementation.

Current-source release self-host completes fresh exit zero in 284.992855625
host-observed seconds: 244 files / 3,027,198 source bytes, source manifest
`fnv1a64:5c0e1e383e7298b5`, complete 760,176 B output equal to fresh Rust and the
bootstrap (`fnv1a64:972228fffae4dc91`). This resident-private run uses expanded 74 MiB
diagnostic RAM and finishes cleanup without scratch files. Its input differs from the
frozen comparison, so its interval is not a per-change timing delta. It proves
current-source self-host correctness, not budget fit.

### Ownership diagnosis and limits

The private resident schedule drains the final dependency-ready cohort before
configuration retirement. Configuration frontend, preparation/discovery owners, retained
semantic records/interfaces and temporary private compiler owners can therefore overlap.
The reference schedule retains the final spans, retires configuration, then replays
those bodies. This is a confirmed lifetime difference; its exact contribution to the
peak has not been isolated.

Admission of 15 modules is measured. An exact rejection breakdown is not proven. Do not
attribute the whole penalty to one owner or infer a larger safe cohort from source
shape. The earlier capacity model was a conditional gross opportunity, not a net bound
on active compilers, saved payloads, public closure and remapping. The [next integration
boundary](native-fragment-preparation.md#next-coherent-integration-boundary) requires a
reviewed lifetime decision rather than an automatic implementation.

### Capacity review before the ownership repair

The 2026-10-20 read-only calculation does **not** yet substantiate a positive net
full-build peak saving. No repair or new native measurement was performed; the reference
path remains unchanged. Use the frozen comparison above, not the older disk census.

The retained package has 1,156 dictionary entries. `frontend.scratchSize` requests
82,612 + 8 × 1,156 = 91,860 B; `memory.reserveExact` rounds this to 91,864 B.

| Potentially removed fixed owner | Allocated capacity |
|---|---:|
| Duplicate private frontend plus its 256 B line buffer | 92,120 B |
| Configuration frontend | 91,864 B |
| Discovery graph scratch | 14,368 B |
| Discovery traversal scratch | 4,400 B |
| Fixed subtotal | 202,752 B |

Keep the caller's semantic frontend; it is not another removable duplicate. Private
frontend scratch already retires before private assembly, so it cannot be subtracted
from a peak during assembly. Configuration's variable lexical/template/import owners
and the compiler's variable owners remain unquantified at the actual peak.

For a provisional request-row scenario, 512 rows × 32 B costs 16,384 B, **excluding**
configuration/dependency payload and conversion scratch. Even assuming every named
fixed owner overlapped the old peak and vanished there, the fixed-only net reduction
would be just 186,368 B, leaving 20,447,312 B: still 1,082,864 B above reference. This
is an optimistic scenario, not a predicted repaired peak. Meeting the 18,315,872 B
gate requires another 2,131,440 B of net variable-owner savings in that scenario,
plus any omitted replacement costs; another phase could become the peak.

The 37,797 B packed-length reduction is not a global record-capacity saving. Both
2,119,792 B and 2,157,589 B exceed 2 MiB. Resident `appendPrepared` growth and
`ordered.materialize` use bounded doubling with a 3 MiB limit, so both streams require
the same 3,145,728 B buffer capacity. Cumulative allocation traffic is likewise not
peak storage. Before implementing the proposed prepare-once ownership repair, account
for variable owners at the peak, the owned request payload, prepared state retained
for late fallback, and the acquisition/later-assembly peaks. Do not forecast a megabyte
gain from the fixed buffers or carry forward the older gross lexical-capacity model.

## Configured-module CLI integration: not retained

This anchor remains for existing links. The superseded narrow converter was removed; Git
history holds its implementation and measurements. The current broader integration is a
separate unpromoted checkpoint and has also failed its resource gate. Neither experiment
establishes a retained optimization or an 8 MiB full-build fit.

## Correctness boundary

Focused native proofs exercise canonical scalar/list/schema/value transfer, fragment
addresses, imported absolute references, exact label identities, alignment,
code/data/BSS, whole-block fallthrough and private dead-block omission. An unchanged
real `binary_text_names` module passes the actual CLI with accepted 1 versus reference 0
and exact live Rust binary/Hunk outputs. Component images move and retire their
source/original owners; these checks do not establish a full-build memory benefit.

Package-owned instruction programs and numeric geometry proofs remain authoritative.
Unsupported state controls, text definitions, warnings, templates, layout observations
and file effects preserve the reference path. All five final-image package-state,
text-state, warning, placement and active-include fallback cases pass both policies with
accepted 0, fresh exit zero and exact live Rust binary/Hunk outputs. Warning and include
effects occur exactly once; runner cleanup is enforced. The current-source self-host is
also complete. Partial native captures, timeouts and launcher success are not completion
evidence.

## Process budget distance

The process allowance is 6,815,744 B; the AmigaOS reserve is 1,572,864 B. Adding
measured MEMD heap, the actual instrumented linked reservation 815,008 B and the
configured 65,536 B stack gives:

| Instrumented tracked components | Private candidate | Explicit reference |
|---|---:|---:|
| Heap + linked reservation + stack | 21,514,224 B (20.518 MiB) | 20,244,992 B (19.307 MiB) |
| Above the 6.5 MiB process allowance | 14,698,480 B (14.02 MiB) | 13,429,248 B (12.81 MiB) |

These are instrumented tracked components before OS/loader/untracked process costs, not
a new release-fit measurement. Count the embedded package once in the linked
reservation. The release linked reservation is separately 798,196 B; release heap was
not measured by these MEMD runs. Neither policy approaches 8 MiB machine fit, and these
results do not justify another constrained 8 MiB trial.

## Hardware storage tradeoff

Erik's completed A6000 observations on an older unchanged 214-file / 2,648,242 B
workload were **123 / 124 s resident versus 1,292 / 1,254 s disk**. Both modes matched
the 708,216 B Rust output. Workload SHA-256:
`07043b5fb7dfe72b476e13925249cc313b0ba60e92cbc35495a861d700cc2593`.

These are two observations, not a three-run median. Hardware RAM was unrestricted;
neither mode proves 8 MiB fit. The interrupted remaining hardware batch was not replaced
or declared complete. Current measurements use FS-UAE while the A6000 is unavailable.
The older disk heap result of 6,909,720 B also uses different input and excludes
executable, stack and OS; it is not a current resident comparison.

## Measurement contract

Use the maintained `native_cli_frozen_comparison` tests and explicitly identify
source/package/image manifests, storage policy and module policy. Compare identical
inputs with freshly generated Rust oracles. Memory builds use the five light
instrumentation defines; release timing is separate. Three release repetitions are
required for a speed claim; a single pair remains an observation.

The budget includes linked code/data/BSS, embedded packages, stack, heap and untracked
process costs. Count the embedded package once. Expanded diagnostic RAM and max-speed
FS-UAE do not establish hardware timing or 8 MiB fit. See the [native
environment](../../agents/rules/fs-uae.md) and [fresh native proof
contract](../../agents/rules/native-rust-parity-porting.md). A self-build requires
matching current source manifests and complete output equal to both fresh Rust and its
bootstrap. Instrumented code producing the release image is a full correctness rebuild,
not instrumented self-replication.

## Enduring earlier decisions

Keep adaptive symbol buckets/shared masks, release-gated reusable telemetry, opaque
assets, compact capture headers, clearing elision where owned, chunked capture, bounded
prepared records and completed-configuration retirement. Historical header compaction
and allocation-reuse gains belong to their original inputs; they do not qualify this
full build. See the [performance reference](../performance/vm-efficiency.md).

Macro-only deferral/audits, first-use selector stacks, include-text reuse and the
prepared leaf cache remain removed. Keep one reference engine and explicit ownership; do
not replace package semantics with CLI/native CPU decisions. Capture policies are in
[the storage note](native-lazy-preparation.md), and the intended endpoint is in [the
object proposal](native-fragment-preparation.md).

## Remaining work and evidence limits

The bounded fallback checks and current-source self-host are complete. Checkpoint and
discuss the failed memory gate and large runtime regression. No further implementation
or release breadth is planned before that course review. Broad workspace qualification
remains red: an earlier broad Rust run in this batch reported 2,319 passed, 161 failed
and 667 ignored. Failures include package/reference expectations and older native fixture
assumptions; they were not repaired in this performance checkpoint. Focused tests do not
claim a clean workspace.

Separate baseline discrepancies remain: Rust flat output can leave some placed mapped
imported addresses section-relative; native rejects `.org` inside a section; and Rust
retained an unreferenced named block in a module imported only for its top-level effects
where both native policies omitted it. These were not repaired or claimed fixed by this
memory experiment. State-specific fixtures use mandatory unblocked code to isolate state
preservation, without weakening the explicit private dead-block regression.

Local receipts live outside `target`, including the final export
`/tmp/opforge-private-cli-integration-final-20261019` and the same-image memory log
`/tmp/opforge-private-cli-integration-policy-memory-20261019.log`. They are evidence,
not repository dependencies. Git is the archive.
