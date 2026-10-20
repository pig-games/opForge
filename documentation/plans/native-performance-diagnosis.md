# Native runtime and memory diagnosis

## Current assignment and decision points

Native parity remains paused. The confirmed budget is **6.5 MiB for the complete
assembler process and 1.5 MiB for AmigaOS**. The current objective is a bulk module
compile path that emits inspectable per-module `.ofm` objects, retires private compile
state, then links objects at the end. The first replacement slice now writes real `.ofm`
files for the eligible module cohort.
It retains only linker metadata for preparation/layout and acquires payload only during
final Hunk emission. This is focused lifecycle qualification, not completion of the
module-at-a-time frontend. Do not continue global paging/cache tuning as the architecture
direction.

The current-source bridge at `c65db50c` is a useful working bridge, but it is not this
architecture. Its full release self-host completed in 421.786289125 s: 245 files /
3,041,871 source bytes, manifest `fnv1a64:a4bcd188014f3e68`, embedded package
455,026 B (`fnv1a64:415d2172932e243a`), and output 762,108 B exactly equal to fresh Rust
and bootstrap (`fnv1a64:3a75baf55ea8329a`). It used expanded 68020-74m memory, max-speed
emulation and a 64 KiB guest stack. This single release observation is not a speed claim.
The full memory run was aborted without a final receipt; the reference release was also
aborted and incomplete. There is no full net memory or timing comparison and no fit claim.

The bridge persists module images in one private capture file. It did not produce actual
per-module `.ofm` deliverables and is not the bulk pipeline. Static release/instrumented
linked reservations including
BSS were 800,340 B / 817,428 B; executable files were 762,108 B / 777,032 B. Expanded
memory does not establish 8 MiB fit.

## Object lifecycle and next coherent slice

The resident layout directory contains Header, BlockRows and RelocRows only. The
replacement flow has `add` retain a complete pending image and copy the
linker prefix; pending state supplies carrier extraction. Store writes each
named `.ofm` once, then retires pending ownership. Names place the module leaf
before its shortened parent qualification, with a build-local ID suffix; the
[object lifecycle note](native-fragment-preparation.md#current-slice-directory-and-object-lifecycle)
defines the classic Amiga filename limits and collision policy.
Compact public semantic owners remain resident for later module compilation. Successful
cleanup preserves `.ofm` deliverables separately from ephemeral captures.

`acquireEmit` is the only payload acquisition path: it runs during final Hunk emission and
keeps one current module cached. The lifecycle gate allows zero reads for unlinked or
BSS-only modules and at most one payload read per selected module. Check N writes, the
read counts, `.ofm` survival after cleanup, exact output and zero remaining private
ownership. This proves read/write lifecycle only; it does not predict a full-build memory
reduction.
The expected resident model is shared runtime + maximum active compile workspace +
retained link/export metadata + output buffer, not the sum of every prepared module.
Do not make a numeric net-saving forecast until retained and replacement capacities are
measured.

## Focused measurements

Fresh expanded-memory FS-UAE runs compare identical source/package inputs against
`--reference-modules`, with exact live Rust binary and Hunk outputs. Release and light
memory runs are separate. The larger page-pressure case deliberately retains ordinary
fallback modules; it tests the new object boundary, not the completed bulk architecture.

| Page-pressure measurement | Object cohort | Reference |
|---|---:|---:|
| Release START-to-DONE, one observation | 32.4003 s | 32.3836 s |
| Peak tracked heap, separate run | 3,205,808 B | 3,204,352 B |
| Final live tracked heap | 0 B | 0 B |
| Completed `.ofm` files / payload reads | 5 / 5 | 0 / 0 |

The +0.0167 s interval is noise, not a speed change. Heap is 1,456 B higher. This is a
lifecycle result, not a memory optimization win. Component checks prove no payload read
before emission and rollback/removal semantics. Normal CLI cases cover scalar/list/schema
exports and code/data/BSS; files survive successful cleanup. The new release image is
763,288 B, with 802,492 B linked reservation including BSS. A 64 KiB stack leaves at most
5,947,716 B for heap and other process overhead within 6.5 MiB. Full-build peak and remaining
excess over that allowance are not yet measured; no full-build fit or saving is claimed.

The next concrete work is extending private frontend ownership to the remaining module
includes/templates/effects, then retiring those modules' global capture/prepared owners.
Do not count a self-host through quiet fallback as completion of that work.

## Qualification and limits

The first coherent slice is metadata plus object-store lifecycle for the currently eligible
cohort. Full per-module frontend compilation—including generic includes, templates and
remaining closure—is still ahead. Extend that frontend until full self-host uses the bulk
path without quiet global fallback. Constants,
types and exported binary macros needed by later compilation belong in a compact shared
public interface; compiled payload does not. Unresolved conditions, loops and relaxation
belong in shared numeric metadata/recipes; converge them on metadata before streaming
final payloads.

Import installation now copies its already-validated export row without revalidating the
whole immutable interface for every value. Focused native scalar/list/schema cases pass;
this is not an isolated timing result. A broader aliased schema projection fails in both
policies during shared binding (`provider.pair.y`), before emission. Its Rust oracle and
explicit native gap test remain; parity repair is paused.

Preserve the live reference route during development. The new object path has no
full-build qualification yet. Full self-host, exact fresh Rust
output, completed native protocol and cleanup remain required for native correctness
claims. A timeout, abort, missing final receipt or
launcher success is not completion.

Use maintained `native_cli_frozen_comparison` / `native_cli_frozen_candidate_comparison`
for frozen full CLI comparisons. Identify source, package, image and input manifests and
module/storage policies; generate fresh Rust oracles. Keep release timing separate from
light MEMD/capture instrumentation; three release repetitions are needed for a speed
claim. Native runs use the configured `OPFORGE_FS_UAE_SMOKE`, `OPFORGE_FS_UAE_BIN`,
`OPFORGE_FS_UAE_CONFIG_TEMPLATE` and `OPFORGE_FS_UAE_ARGS`; current full self-host uses
a 65,536-byte guest stack. Memory fit requires the documented 68020-8m profile; expanded
68020-74m is diagnostic only. See the [native parity contract](../../agents/rules/native-rust-parity-porting.md)
and [FS-UAE environment guide](../../agents/rules/fs-uae.md).

## Enduring earlier decisions

Keep adaptive symbol buckets/shared masks, release-gated telemetry, opaque assets,
compact capture headers, owned clearing elision, chunked capture, bounded prepared
records and completed-configuration retirement where they remain in current code.
Historical gains apply only to their original inputs. Keep one reference engine and
explicit ownership; CPU/family semantics remain package-owned. See the [performance
reference](../performance/vm-efficiency.md).

Macro-only deferral/audits, first-use selector stacks, include-text reuse and prepared
leaf caching remain removed. Generic capture ownership and retirement contracts remain
in [preparation ownership and capture storage](native-lazy-preparation.md). The object
format and bulk-link direction are in [per-module `.ofm` objects](native-fragment-preparation.md).

## Qualification limits

The current-source full self-host proves exact output only for its stated expanded-memory
configuration. A previous broad workspace run was red (2,319 passed, 161 failed, 667
ignored); this work is not a clean workspace qualification. Known unrelated parity
discrepancies remain: some placed mapped imported addresses can stay section-relative in
Rust flat output; native rejects `.org` inside a section; and Rust retained an unreferenced
named block in a module imported only for top-level effects where native omitted it.
These remain unfixed. Full 8 MiB fit and net memory gain remain unproven.
