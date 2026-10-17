# Module preparation and relocatable objects

Status, 2026-10-17: dependency-first preparation now interleaves with module
configuration/discovery with resident capture. Completed module captures retire after their numeric
records and templates have been published into the shared semantic owner.
Final binding, layout and emission still run globally. This is an early-release
pipeline, not a persistent `.ofm` object implementation. Parity remains paused.
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

## What remains eager and resident

The configuration pass still runs shared lexical/configuration processing; this is
not the proposed cheap structural-only scan. Every applicable module body still gets
prepared. The shared semantic scratch, exported definitions/templates and prepared
record buffer remain resident and grow across modules. Binding proxies, member
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
This next boundary is not yet implemented or qualified.

A future configured-object fingerprint must cover content, transitive includes and
assets, imported exports/types/templates, compiler/package semantics, relevant options,
search resolution and consumed source-order inputs. Timestamps are hints. Earlier
raw-include and prepared declaration-leaf caches were dropped: they retained captures
and established no peak or runtime improvement. Git preserves those experiments.

Explicit `.cpu`, `.fpu`, `.apollo` and related configuration directives drive future
package demand loading. Instruction spelling must not implicitly select chunks.
Package storage is a separate future interface; CPU semantics remain package-owned.
