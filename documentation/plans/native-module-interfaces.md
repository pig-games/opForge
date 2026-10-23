# Stored module assembly interfaces

## Current architecture

The promoted product's current full cold/incremental qualification is recorded in
the [native assembler plan](native-runtime-reset.md#current-product-state). Earlier
measurements below retain their own source/configuration scope.

Current-build `.ofm` objects carry numeric assembly interfaces alongside linker
data. `OFMA` is the checked interface directory and dependency/import map;
`OFMS` stores semantic exports, members, names, and compound-value arenas;
`OFTM` stores closed template and statement preparation data. Serialized offsets
are object-relative or region-relative, and imported identities map through a
dependency ordinal to a provider-local entity ID. Integrity tags and bounds
checks validate data; tags do not select VM contract versions or legacy
executors.

The 48-byte `OFMA` header locates the value/template regions and dependency,
import, and filename directories. Dependency rows are 8 bytes (filename offset
and length); import rows are 8 bytes (external dependency ordinal and entity ID).
Import ordinals are zero-based; template closure origins reserve zero for self
and encode an external ordinal plus one. The checked layouts are defined in
[the interface container](../../native/motorola68000/amigaos/assembler/modules/assembly_interface.asm).

The producer closes and writes each module interface. Consumers use a shared
provider and registry to resolve numeric bindings on demand, while the final
linker retains its fragment, placement, relocation, and Hunk responsibilities.
The root remains in the shared final driver. Dependency interfaces are consumed
through the stored-interface path; the disk-backed private compiler/image path
and `--reference-modules` reference policy remain available as distinct
mechanisms.

The header reader now serves covered object-header ranges from retained link
metadata and validates the assembly directory through its existing window.
For interfaces that fit the window, a cold selection performs one disk range
read rather than two small header reads followed by the window read. This adds
no heap capacity. Fresh full-source qualification passed; the release pair saved
8.1% with unchanged tracked heap. Measurements and their frozen input identities
are in [runtime and memory status](native-performance-diagnosis.md).

The native CLI now keeps reusable `.ofm` images in `build/ofm` by default.
`--incremental-dir DIR` selects a different cache; `--rebuild` forces fresh module
assembly. An `.ofi` companion records acquisition dates/sizes, dependency
build generations, configuration identity and the pointer-free publication
inventory needed to recreate current-build section and linker bindings. The
OFMI/OFMA/OFMS/OFTM layouts themselves remain unchanged.

The native CLI reports module processing and timed assembly/reuse through its
process error stream. Two caller-owned progress frames use 1,360 bytes of static
storage and allocate no heap; DateStamp is sampled only at phase boundaries.
`-q`/`--quiet` suppresses progress and the incremental summary. A private fallback
is reported as deferred; ordinary replay reports preparation by captured source
path. Shared root assembly and output have their own
link phase. This presentation leaves `.ofm` contracts and cache policy unchanged.

The progress-enabled build passes a complete cold/warm self-host pair on the same
8 MiB installed Workbench/plain-background profile below: 288 s cold and 60 s
warm, all 189 dependencies rebuilt/reused respectively, with exact 826,760-byte
Rust/bootstrap output and zero phase exits. Its source manifest is
`fnv1a64:4a1bf704ef6a06ff` (277 files / 3,427,689 bytes). This latest build is not
the input of the older heap-accounting pair below; no new heap peak or isolated
reporting-overhead comparison is claimed. Focused native checks additionally
cover dependency ordering, timed completions, cache reuse, shared reference
replay, both quiet forms, visible argument errors and unchanged artifacts.

Source freshness uses timestamps and file sizes, with no source hashing or
content comparison. Dates must match exactly, so a backdated edit also invalidates
an object. The current acquired input set must match too: inserting an earlier
include-search candidate cannot leave an old input silently selected by the cache.
Rebuilding a dependency invalidates its consumers, even inside one timestamp tick.
Tool and package compatibility use binary identities; rebuilding an identical
assembler executable does not invalidate its module cache just because its date
changed.

Discovery, configuration and final linking still run. A hit skips private module
compilation, restores its owner/section inventory, rebinds provider-local imports
to fresh linker identities, and enters the ordinary current-build registry.
Modules outside private admission, including the shared root driver, execute
normally. The first key includes the complete ambient configuration/lexical
identity table: some unrelated configuration changes conservatively cause extra
misses. The ordered source-origin/path inventory is also part of that identity:
closed templates retain numeric declaration origins, so changing an include's
ordinal must not reuse a template against another source path. The existing
reference policy remains separate and disables cache reuse.

Cache markers are removed before replacing images, and published last. Missing,
stale or structurally invalid cache data is rebuilt. A failed cache write leaves
a valid assembly uncached; an unavailable cache directory falls back to fresh
assembly. Use one writer per cache directory. Cache filenames retain the module
name first and bounded qualification/registry suffix; changing discovery order
can cause safe misses. Removing `build/ofm` resets the cache.

Native discovery canonicalizes candidate paths before assigning opaque source
origins, preserving the entry file in slot zero. Module execution still follows
the dependency graph, and explicit search-root order is unchanged. This makes
unchanged builds reuse the same declaration contexts without weakening the
origin/path key.

### Incremental qualification

The retained expanded-memory FS-UAE baseline passes complete cold/warm self-builds
against a live Rust oracle. Its frozen input contains 273 files / 3,411,207 bytes,
source manifest
`fnv1a64:ce0b2b8f361c5655`. Both phases emit the identical 824,512-byte executable,
including the embedded m68020 package. Measurements use disk-backed capture
(`--capture-dir Work:`) and private module storage; they do not qualify the default
resident-capture path's full-build memory use.

| Metric | Forced fresh (`--rebuild`) | Unchanged incremental |
| --- | ---: | ---: |
| Release assembly duration | 292 s | 62 s |
| Private modules rebuilt | 187 | 0 |
| Private modules reused | 0 | 187 |
| Peak tracked heap, separate accounting run | 4,814,136 bytes | 4,814,136 bytes |
| Cumulative owned allocations | 304,211,056 bytes | 149,132,304 bytes |
| Terminal tracked allocations | 0 | 0 |

Release durations have one-second guest-clock resolution. This single pair shows
79% less runtime (4.71x speedup); instrumented durations are excluded from that
comparison. Both release phases complete with fresh proof, explicit exit zero and
exact output matching the Rust oracle and bootstrap. The separate accounting
phases also complete with exit zero and exact release-oracle output.

The unchanged heap peak comes from acquisition before module reuse: discovery,
tokenization and root-driver work still execute. The instrumented image reserves
879,988 bytes; adding tracked heap and the 65,536-byte stack gives a 5,759,660-byte
(5.49 MiB) process floor. This excludes loader overhead and untracked allocations.
It leaves 1,056,084 bytes below the 6.5 MiB process budget before those costs;
the expanded-memory profile does not establish an 8 MiB fit.

The cache retains 187 `.ofm` images (1,670,040 bytes) and 187 `.ofi` companions
(21,895,504 bytes). The conservative complete-context key currently dominates
cache disk size. These are reusable cache files, separate from transient capture
storage; deleting the cache forces a fresh build.

Focused native tests also cover forced rebuilds, dependency edits, timestamp-only
edits, macro declaration origins after include insertion, and discovery candidate
permutations/duplicates with entry priority preserved. The comparison harness
retains completed timing results before failing reuse assertions, so a future
invalidation regression cannot erase the measurement that exposes it.

For a serial hardware comparison, run
`python3 scripts/performance/run_a6000_incremental.py --bundle DIR` from macOS
Terminal with a release embedded, source-directed self-host bundle. It discovers
the current address with `ash -l`, uploads to `Development:`, then performs a
forced rebuild and unchanged incremental build in the same directory. Each phase
has separate guest timing, fresh markers, return-code/output verification and
cache counts. The cache survives between phases; transfer time is excluded from
guest assembly timing. The hardware runner does not restrict RAM.

Erik's reported A6000 pair on 2026-10-22 confirms complete native self-hosting
and unchanged incremental reuse for the prepared source-directed release bundle:

| Hardware phase | Guest duration | Reused / rebuilt | Output |
| --- | ---: | ---: | --- |
| Cold `--rebuild` | 916 s (15m 16s) | 0 / 187 | 824,512 bytes, exact Rust match |
| Unchanged incremental | 98 s (1m 38s) | 187 / 0 | 824,512 bytes, exact Rust match |

Both phases report fresh protocol completion, assembler exit zero and successful
output copies; cache semantics verification passes with no diagnostics. The
paired host command interval is 1,015.101 s, consistent with the 1,014 s guest
total at one-second clock resolution. This single pair shows 89.3% less runtime
(9.35x speedup). It uses the same 273-file source manifest and embedded m68020
package, release instrumentation disabled, disk capture in `scratch`, and source
output `build/opforge`. Hardware memory remains unmeasured/unrestricted;
the FS-UAE heap figures above are not hardware memory measurements.

The prepared case identity matches the reported
`55e20a706dcc193b6aa48da501fe7ead53b6d74c562422152314df3f1c5f09d3`.
Its remote directory is `Development:opforge-0d62e0387ef9`. The result's
`bundle_prior_native_validation` describes the pre-run state, not the completed
hardware qualification reported here.

### 8 MiB Workbench qualification

Complete cold and unchanged incremental self-builds pass on the constrained
68020 profile: **2 MiB chip plus 6 MiB fast RAM**, with no RTG or additional
memory. The 2026-10-23 run retains installed Workbench 47.2, all startup utilities
and a plain background. Before launch, with a 64 KiB shell stack, `Avail` reports
6,716,384 bytes free (6.41 MiB): 2,017,352 chip and 4,699,032 fast. The largest
fast block is 4,595,848 bytes. The original system drive remains unchanged.

| Metric | Cold `--rebuild` | Unchanged incremental |
| --- | ---: | ---: |
| Release duration | 284 s (4m 44s) | 61 s (1m 1s) |
| Modules rebuilt / reused | 188 / 0 | 0 / 188 |
| Peak tracked heap, separate accounting pair | 4,146,008 B (3.95 MiB) | 3,671,120 B (3.50 MiB) |
| Cumulative owned allocations, accounting pair | 304,339,376 B | 148,671,432 B |
| Terminal tracked ownership | 0 | 0 |
| Allocation failures / profiling errors | 0 / 0 | 0 / 0 |

Both release phases have fresh completed proof, explicit exit zero and complete
**825,428-byte** output identical to the live Rust oracle and bootstrap. Both
accounting phases also complete with exit zero and exactly the same release
output. Their durations are excluded from the release timing above. The frozen
input contains 275 files / 3,418,882 bytes, source manifest
`fnv1a64:9667cf7b90951a3b`; the embedded m68020 package is unchanged
(`fnv1a64:390f6f44df85279b`). Capture remains disk-backed and the root remains
in the shared final driver. This proves fit for this desktop/workload, not the
default resident-capture path or an unrestricted choice of desktop programs.
The emulator uses unrestricted CPU speed: durations are not physical 68020 timings.

The instrumented image reserves 880,904 bytes. Adding tracked heap and the
65,536-byte stack gives accounted process floors of **5,092,448 B (4.86 MiB)**
cold and **4,617,560 B (4.40 MiB)** warm. These exclude loader and other untracked
costs; they are not total process peaks. The cold floor leaves 1,723,296 bytes
below the 6.5 MiB process budget before those costs. Constrained completion
independently proves that the process and this OS setup coexist within 8 MiB.

The CLI now streams Hunk metadata through the shared **4 KiB** transport buffer
and borrows section payloads directly. It never constructs a second full Hunk
image. The generator owns no heap; its 44-byte runtime frame borrows stable
parts/fixups until synchronous output completes. Hunk-specific CLI selection
and relocation adaptation live in
[binary_app_hunk_output.i](../../native/motorola68000/amigaos/assembler/driver_hunk_output.i).
The unchanged `binary_hunk_output.build` remains the validating/reference
serializer; focused native tests compare both implementations and Rust, including
64/65-byte buffers, interleaved relocation targets, padding, BSS and repeated output.
No new cache/container contract or experiment switch is introduced.

The previous materialized output attempts failed under this same memory limit,
so their incomplete peaks/times are not before/after performance baselines. The
new executable grows by 916 bytes; there is no isolated runtime speedup claim.
Plain-background Workbench is the low-memory baseline. The copied system's custom
1280-by-800 backdrop costs about 3.37 MiB more; it is outside this qualification.
See [FS-UAE memory qualification](../../agents/rules/fs-uae.md#native-memory-qualification).

Public-name lookup retains the 40-byte OFMS header and linear export scan. A
two-array full/leaf name-index trial passed native and full self-host checks but
showed no full-build saving, so its format, implementation and tests are retired.
The current measurements and decision are in the runtime and memory status above.

## Bounded consumer access

The interface reader owns one 64 KiB window. Value and template carriers are
independently capped at 64 KiB each. The active consumer's module table uses
8-byte rows and is capped at 64 KiB; its binding table uses 24-byte rows and has
the same cap. These rows do not include the registry's resident link metadata.

| Per-consumer record | Fields | Bytes |
| --- | --- | ---: |
| Module | Local lexical module ID, stored object ID | 8 |
| Binding | Provider, entity, consumer ID, kind, wrapper, ready state | 24 |

Bindings are materialized only when a value/member/template is demanded, and
provider interfaces remain immutable. Private lexical scratch spills combined
active entry/name payload at 64 KiB; its prepared-store payload cache is capped
at 128 KiB (four 32 KiB pages).

These bounds apply to consumer-side lookup and carriers. The producer still
temporarily overlaps full semantic and facet owners, the combined interface, and
the final image. It is not a bounded streaming writer, and the current path does
not guarantee a 512 KiB combined replacement ceiling.

## Supported semantic boundary

The interface preserves public value and template facets independently, nominal
member identities, dependency-local entity IDs, template closure, signatures,
defaults, and declaration context. Alias spellings are consumer-local bindings.
Hashing may accelerate name lookup, but exact spelling comparison resolves
collisions; hashes and equal record contents are not identity. Numeric schema and
member IDs preserve nominal identity through compound cells and template closure.
Provider declaration binding remains separate from caller argument binding and
template hygiene. `.use` selection controls visibility, not block liveness;
complete referenced blocks and internal fall-through remain live. Stored compound
values preserve only supported address and member forms; compound
symbolic-address cells remain unsupported and fail closed. No new CPU, family,
encoding, placement, or relaxation meaning belongs in the interface.

Interface construction is transactional: validation failure leaves the output
destination empty, with no partially published interface.

For object payload, cleanup, and numeric fixup proofs, see
[per-module object contracts](native-fragment-preparation.md). Capture and
retirement rules are in
[preparation ownership and capture storage](native-lazy-preparation.md). Current
full-source qualification and comparative measurements are in
[native runtime and memory status](native-performance-diagnosis.md).

## Storage scheduling decision

The retained CLI writes and publishes each object synchronously. An experiment
overlapped one payload write of at most 64 KiB with preparation of the next module,
using an AmigaDOS packet and retaining the existing immutable image owner. A
separate synchronous-payload control preserved that same image lifetime, so a
cache effect could not be misreported as write overlap. Open, close and rename
remained synchronous; no worker process was created.

Native packet/real-DOS checks and the trial's full own-source self-host passed.
On identical input, release times were 280.360 s synchronous and 279.953 s
background: no demonstrated gain at the established noise level. The trial added
34 B peak tracked heap and 3,040 B instrumented linked reservation, with cleanup
to zero. All 186 serialized objects were byte-identical to the reference.

The extra writer machinery and its tests are removed from the working tree.
Checkpoint `bdc65e13` and the frozen A6000 trial preserve it for a hardware
comparison; no extra maintained switch or worker implementation remains. Resume
this direction only if the hardware result justifies its code and ownership
complexity. Current object, consumer-window and serialization contracts above are
unchanged.
