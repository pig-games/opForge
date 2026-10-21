# Native runtime and memory status

## Current result and decision

Stored dependency interfaces work through the normal CLI. All three complete
release self-builds reproduced the **816,564-byte Rust oracle and bootstrap
exactly**, with explicit exit 0 and fresh completed protocols. All 186 dependency
modules used stored interfaces; the root remains on the shared final driver.
No dependency fallback was observed. The 24 native comparison cases passed.

These are the last qualified baseline results. The subsequent header-read
repair and bounded background-write experiment are being qualified separately;
their component proofs do not establish full self-host or performance results.

This is a substantial memory reduction with a substantial runtime regression.
The approved next work removes repeated interface lookup/read work before
experimenting with bounded background object writes. Preserve both the
prior disk-backed private compiler/image baseline and `--reference-modules`;
they are distinct reference mechanisms. See
[stored module interfaces](native-module-interfaces.md) and
[`.ofm` numeric contracts](native-fragment-preparation.md).

## Identical-input full-build comparison

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
Bounded identity/row-offset indexing or lookup reuse is the next concrete
candidate to review. It must avoid recreating complete resident semantic tables;
no numerical runtime recovery is established yet. No new observer framework was
added.

The candidate wrote 186 `.ofm` objects, totaling 1,647,580 B. Their `OFMA`
interfaces total 1,019,012 B; the largest is 45,964 B. They contain 10,517 value
rows and 28 template rows. Final emission read 178 object payloads and recorded
1,665 payload-cache hits. Interface reads are additional bounded accesses, not
included in that final-payload read count. Packed source was 578,506 B and total
source bytes read were 3,584,949. Separate instrumented preflight elapsed time
was 303.609 s; it is not a release timing or fixed-point self-host claim.

## Smaller controls

Both versions matched the live Rust oracle in the small multi-module project
and the macro-heavy project, including all release and MEMD cases. Cleanup was
zero in both implementations.

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
