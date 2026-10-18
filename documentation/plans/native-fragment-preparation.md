# Module preparation and relocatable objects

Follow the [current 8 MiB budget and performance sequence](native-performance-diagnosis.md#current-assignment-and-decision-points).
Module objects and object-local IDs remain analysis-only until Erik decides whether
to continue them; they are not the next implementation assignment.

Status, 2026-10-18: dependency-first preparation now interleaves with module
configuration/discovery with resident capture. Completed module captures retire after their numeric
records and templates have been published into the shared semantic owner.
Disk-backed prepared module ranges now survive global binding/layout and reachability
through bounded views, allowing frontend scratch to retire before assembly input is
materialized. Final binding, layout and emission still run globally. This is an
in-build ownership boundary, not a persistent `.ofm` object implementation. Sequential lexical
sealing now has qualified-source binding repaired and reuses its bounded cache
buffers. Current release self-hosting succeeds exactly; the extra instrumented
current-source run times out during global completion. Frozen resource comparisons
remain valid, but that timeout supplies no final peak or completion. Parity remains
paused.
See [measurements](native-performance-diagnosis.md) and
[capture ownership](native-lazy-preparation.md).

## Implemented publication boundary

The discovery graph can yield a dependency-complete module before a later import
requires another file. A successful shared-frontend replay publishes owned numeric
records, definitions and templates, then acknowledges that graph node and retires
its raw capture interval. A failed replay is never acknowledged. Dependency and
source order, whole-block fallthrough and normal global reachability remain intact.

Configuration and semantic sessions own independent scratch and graph storage.
Only one session is active at the stable frontend address; switching restores
shared VM control. Append-only immutable configuration snapshots are transferred
by spelling into semantic identities. Previously consumed section maps retain
their state. Repeated `.use` configurations must name the same complete set of
keys and equal values, independent of key order, matching Rust.

Before requesting another publication, the scheduler tests whether configuration
has closed. Once it has, it transfers the remaining schedule, seeds final snapshots,
then releases configuration scratch, graph and physical discovery before replaying
remaining bodies. Already published records remain owned. A graph which closes
without early publication uses the shared existing complete-schedule path. No
benchmark/path-specific behavior or new experiment switch selects this boundary.

Bounded disk capture uses complete-graph scheduling. Its payload already stays within
1 MiB, while overlapping growing configuration and semantic owners increased tracked
peak by 14.9% in the measured interleaved control. Keeping the existing single-owner
transition avoids that regression. This is the existing storage policy selecting an
ownership schedule, with the same language implementation on both paths.

Late section maps prevent early commitment to section geometry. Fixed-size numeric
section recipes retain source section slots, kind and alignment until the full map set is
known. Global completion resolves them before region/placement validation and
ordinary binding/layout. No CPU parser, expression evaluator or convergence policy
is duplicated in this scheduler.

## Captures, includes and ownership

Captured source has monotonic logical offsets. Retirement records disjoint checked
intervals; fully retired chunks release payload and cannot fault back from disk.
A partially retired shared chunk stays allocated while live bytes remain. Reader
views stop at retired boundaries; pinned rows cannot retire. Physical file boundaries
seal writers, reducing shared allocation lifetimes. Descriptor movement never changes
logical handles.

An active include suspends its parent source cursor and preserves origin, expansion
and declaration context. During interleaved resident preparation, its acquired
extent retires only after replay, before the parent resumes. Complete-graph paths
keep include captures until final capture cleanup. Templates retain their own binary
bodies/defaults, not borrowed capture pointers. Inactive branches and unexpanded templates do not acquire files. Discovery
paths required by later active includes survive physical discovery cleanup.

Resident and disk-backed capture use this same ownership protocol. Disk backing
still has a bounded resident payload budget and one private append-only scratch file;
retirement reduces RAM ownership, not the file's accumulated length. Use a genuinely
disk-backed volume rather than assuming `T:` saves RAM. Failure cleanup owns both
sessions and every partial allocation.

## Prepared module storage

Checked module spans own dense ranges in a shared prepared-record store; module
boundaries need not waste page space. Disk mode keeps at most four 32 KiB payload
pages, with directory metadata outside that budget. Records remain whole, dirty
pages persist before eviction and pinned pointers cannot move. Logical module-order
views translate offsets and cap reads at module boundaries without reloading all
records. Buffer adapters preserve the contiguous reference path.

Global completion runs its existing layout, output, binding and record-remapping
phases across pages in the original order. Section state spans batches. Metadata-only
ordering builds origins and module starts. Reachability uses the same selector through
views and retains whole-block decisions separately from lexical scratch. Runtime
bindings, typed values, parameters and member IDs transfer before frontend retirement.
The ordered contiguous stream is then rebuilt once, omission applied and backing
storage released. `beginSizing` still sees stable contiguous records. This avoids
prepared-buffer/frontend overlap; it does not avoid semantic preparation work.

Bounded prepared records previously saved 1,964,032 B / 18.72% on frozen input.
Lexical sealing saves another 1,615,656 B / 18.95%. Buffer reuse then removes
88.41% of allocation/free traffic without changing that heap peak. The remaining
runtime cost and qualification limits are documented below.
The [resource results](native-performance-diagnosis.md) separate heap, allocation
traffic and release timing. Global symbol IDs remain a temporary in-build bridge;
this storage cannot yet be exported as a reusable module object.

## What remains eager and resident

The configuration pass still runs shared lexical/configuration processing; this is
not the proposed cheap structural-only scan. Every applicable module body still gets
prepared. Exported definitions/templates, module metadata and remaining semantic owners still
grow across modules. The disk candidate now spills completed private lexical ranges;
late dependencies remain readable through owned copies. Prepared records use bounded backing with disk policy, while resident
policy retains its contiguous buffer. Binding proxies, member
identities and placement-sensitive recipes are finalized across the complete build.
Publication does not skip preparation of unreachable source or emit final bytes early.

Consequently the useful memory bound is shared runtime/packages plus simultaneous
configuration and semantic state, retained prepared data, active module/include
capture, discovery and allocation overlap. It no longer requires every completed
module's raw capture to remain resident. The measured largest overlap, rather than
a speculative object size, determines the current peak. Expanded-memory results do not establish 8 MiB fit.

## Next ownership boundary: real module objects

A reusable near-output object must have object-local IDs and checked offsets,
including nested regions, with no process pointers or build-global symbol IDs.
It must own exported typed values, schemas, templates, provenance and unresolved
binding/layout recipes. Resolving storage offsets, remapping imported identities
and relocating output addresses are separate operations.

All fragments support relocation. Fixed bytes need patches; variable-width encodings,
placement-dependent conditionals and loops need VM/package recipes. Keep recipes
whose dependencies can change even if currently resolved. Retain a loop body and
conditional tree rather than enumerating every iteration or branch combination.
Ordered mutable observations must match source execution. Sparse objects retain
whole referenced `.block` units, preserving internal fallthrough; absent units cause
source preparation or a cache miss. Macros and segments remain templates.

Resident and disk-backed objects should use the same pointer-free representation.
Once a validated object owns every later consumer's data, source-oriented prepared
storage can retire. Merely serializing after global completion cannot lower an
earlier peak. Preserve the no-growth/no-movement assembly frontier at `beginSizing`.
Object-local identities, near-output relocatable fragments and per-module metadata
retirement are not yet implemented or qualified. The bounded prepared-record bridge
above is implemented; it is not a substitute for these object interfaces.

## Sequential lexical storage sealing: current checkpoint

The capacity census identified 50,815 entries, 38,298 unique names and 1,791,485
name bytes over 220 module scopes. Near the old peak, entries, module arrays and
name pages occupied 4,325,888 B. The planning target was at least 1 MiB of measured
whole-build saving, not a guarantee from adding up final retained objects.

The implemented candidate preserves global IDs through an eight-byte bridge and
moves private entry/name payloads behind a four-page 128 KiB cache. Successful
publication seals the newly appended range and frees its active arenas. Interface
facets remain retained. Global completion still reads or mutates private dependencies
through checked owned copies; no raw arena/cache pointer escapes. Names retire at
the established post-binding numeric boundary, before later entry consumers retire.
Resident mode keeps contiguous storage through those same access interfaces.

Module owner/origin/visibility arrays remain resident: 393,216 B at full capacity.
The bridge needs up to 524,280 B for 65,535 identities. Retained typed values,
schemas, templates, imports and plans are unchanged. Closing a module does not
justify deleting its private dependency closure, and this checkpoint does not do so.
Failures latch and flow to frontend publication/cleanup; partial owners release normally.
The existing `--capture-dir` policy selects backing, with no additional selector.

The unchanged frozen sealing comparison reduces peak from 8,525,376 to
6,909,720 B: **1.541 MiB / 18.95% saved**, above the 1 MiB target. The binding repair
leaves heap/work counters unchanged. Reusing four existing 32 KiB cache buffers
then reduces allocated/freed capacity from 826,338,176 to 95,775,616 B, with the
same peak, exact live Rust output and zero cleanup ownership. Instrumented
preparation falls 289.94 to 166.42 s. Separate release timing and smaller controls
are in the [measurement report](native-performance-diagnosis.md).

The current release executable also assembles its entire current source exactly in
both resident and disk modes. The additional instrumented current-source case
does not complete within
1,200 s, after the ORDER marker and before BIND. Do not use that timeout as a peak
measurement or infer it was introduced by cache reuse: an equivalent pre-reuse
instrumented current-source control has not been run. Focused native component and
module checks pass. Keep the contiguous path and preserved pre-sealing executable
as references; review the anomaly before further ownership changes.

Buffer reuse is implemented, with no extra state or larger cache. It transfers
allocation ownership after flushing an unpinned victim, preserving logical metadata;
short views expose only initialized bytes. Dirty writes, pins and failed-I/O cleanup
have fresh native component coverage. The static capacity census remains a forecast,
while the completed frozen MEMD run establishes the actual peak saving.

A resident chain/hash bridge remains a proposal for avoiding cold payload acquisition
during rehash/nonmatching lookup. A 14-byte bridge adds 393,210 B at maximum count,
projecting about 1.166 MiB remaining sealing saving at an unchanged peak frontier.
It is unimplemented and follows the ordered work in the current plan. No 8 MiB
fit is established by these expanded-memory runs: frozen tracked heap
alone is 6.59 MiB, and prepared/assembly live heap remains 2.83 / 4.71 MiB.

Real object-local identities, explicit remapping and near-output relocatable fragments
are analysis-only candidates after these storage and qualification limits are
resolved. Do not start another macro-only investigation or a final-only cache.
The census and current receipts remain outside `target` under
`/tmp/opforge-module-capacity-20261018`, `/tmp/opforge-module-sealing-20261018`
and `/tmp/opforge-cache-reuse-20261018`.
Git preserves the superseded forecast and experiment chronology.
