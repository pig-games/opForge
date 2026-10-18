# Module preparation and relocatable objects

Status, 2026-10-18: dependency-first preparation now interleaves with module
configuration/discovery with resident capture. Completed module captures retire after their numeric
records and templates have been published into the shared semantic owner.
Disk-backed prepared module ranges now survive global binding/layout and reachability
through bounded views, allowing frontend scratch to retire before assembly input is
materialized. Final binding, layout and emission still run globally. This is an
in-build ownership boundary, not a persistent `.ofm` object implementation. Parity remains paused.
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

The measured disk peak saving is 1,964,032 B / 18.72% on identical frozen input.
The [resource results](native-performance-diagnosis.md) separate heap, allocation
traffic and release timing. Global symbol IDs remain a temporary in-build bridge;
this storage cannot yet be exported as a reusable module object.

## What remains eager and resident

The configuration pass still runs shared lexical/configuration processing; this is
not the proposed cheap structural-only scan. Every applicable module body still gets
prepared. Shared semantic scratch and exported definitions/templates remain resident and grow
across modules. Prepared records use bounded backing with disk policy, while resident
policy retains its contiguous buffer. Binding proxies, member
identities and placement-sensitive recipes are finalized across the complete build.
Publication does not skip preparation of unreachable source or emit final bytes early.

Consequently the useful memory bound is shared runtime/packages plus simultaneous
configuration and semantic state, retained prepared data, active module/include
capture, discovery and allocation overlap. It no longer requires every completed
module's raw capture to remain resident. The measured largest overlap, rather than
a speculative object size, determines the current peak. No 2 MiB fit is claimed.

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

## Capacity budget before module sealing

The 2026-10-18 bounded census supports pursuing module-private lexical retirement,
with a **1 MiB measured full-build peak reduction** as the acceptance target.
This is a prediction budget, not an implemented saving or a new peak measurement.
Current shared arrays cannot release module slices: direct reclaim remains zero.

The unchanged frozen self-host input contains 50,815 lexical entries and 38,298
unique stored names, totaling 1,791,485 name bytes. Ownership follows explicit
declaration metadata and checked lexical-parent chains, identifying 220 module
scopes; 1,172 global/unassigned entries remain retained. Names shared between
modules are counted once and retained. The largest module has 3,810 entries.

| Existing allocation near the preparation peak | Capacity |
|---|---:|
| 36-byte lexical entry arena | 2,097,152 B |
| Three per-identity module metadata arrays | 393,216 B |
| Name pages and directory | 1,835,520 B |
| Total potentially replaceable capacity | **4,325,888 B / 4.13 MiB** |

All these allocations coexist at a measured **8,483,280 B** semantic-span
frontier, just 42,096 B below the existing **8,525,376 B** heap peak. Thus the
opportunity exists near the actual peak, rather than only at final cleanup.

The conservative eight-byte identity-bridge model charges the following replacement
storage. It assumes completed private lexical state is sealed/spilled, only one
module's private frontend is active, and later consumers use numeric interfaces.
It retains all current typed-value, schema, template, import, plan and bucket
allocations; no savings from those owners or near-output fragments are credited.

| Replacement owner | Budget |
|---|---:|
| All visible facets and global/unassigned entries, with full entry/module metadata and shared/interface names | 900,784 B |
| Eight-byte numeric bridge for every current identity | 406,520 B |
| Largest active private module, including geometric allocation and name-page slack | 352,512 B |
| Additional bounded object-metadata cache | 131,072 B |
| 256-byte directory allowance per module/global unit | 56,576 B |
| Total steady replacement | **1,847,464 B / 1.76 MiB** |

The retained interface includes 9,981 entries and 481,558 unique name bytes.
The model charges all public value/template/statement facets, without treating
visibility as proof that their private dependencies can be released. Sealing must
resolve those dependencies or retain explicit numeric recipes. Any additional
closure storage reduces the saving byte for byte.

At the measured peak-near frontier, replacing these owners gives a modeled
6,004,856 B. Charge another 352,512 B for a whole-private-module sealing copy and
reserve 128 KiB for additional remapping/workspace: **6,488,440 B / 6.19 MiB**,
or **1.90 MiB below that frontier**. Allowing geometric growth instead of exact
sizing for the bridge, directory and per-module interface entry tables adds
380,528 B, leaving **1.54 MiB** of modeled saving. The 128 KiB workspace is a
proposed implementation budget, not measured existing work. No concurrent sealing
or retained private dependency closure is hidden in these estimates.

These **1.5–1.9 MiB planning estimates apply to the sampled frontier**, not a
guaranteed new full-build peak. Allocation overlap inside spans, concurrent module
lifetimes and closure requirements can move the maximum. A production experiment
must preserve the reference, count retained and released capacities, enforce a
bounded sealing workspace, and run the full unchanged-input peak measurement.
If additional retention consumes the margin above 1 MiB, revise the design before
expanding it. Merely writing today's global tables to an object file cannot realize
this budget. Module sealing/retirement remains unimplemented; parity stays paused.
The current tracked peak is still 8.13 MiB, excluding executable, stack and OS:
no 2 MiB fit is claimed.

Evidence is retained outside the build cache at
`/tmp/opforge-module-capacity-20261018`: `calculate.py`, `capacity-v2.json`,
`decoded-memory.json`, and `census/result/native-result.json`. A temporary source
snapshot uses the existing progress telemetry for a finite read-only census, with
no production code or observer interface changes. The fresh FS-UAE run completes,
exits zero and produces the exact 708,216-byte live Rust oracle on workload
`eae5102bd21b3863105a7f550912193dff8e52c8ba2d6ff11b498392b7dca480`.
Heap peak, allocated/freed totals, packed bytes and expression/evaluation counts
match the reference exactly; cleanup live bytes are zero. Census timing is
diagnostic overhead and establishes no speed improvement. This is frozen-input
proof, not a fresh current-source self-build or A6000 measurement.

A future configured-object fingerprint must cover content, transitive includes and
assets, imported exports/types/templates, compiler/package semantics, relevant options,
search resolution and consumed source-order inputs. Timestamps are hints. Earlier
raw-include and prepared declaration-leaf caches were dropped: they retained captures
and established no peak or runtime improvement. Git preserves those experiments.

Explicit `.cpu`, `.fpu`, `.apollo` and related configuration directives drive future
package demand loading. Instruction spelling must not implicitly select chunks.
Package storage is a separate future interface; CPU semantics remain package-owned.
