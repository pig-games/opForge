# Stored module assembly interfaces

## Current architecture

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
[the interface container](../../native/motorola68000/amigaos/experimental/binary_module_iface.asm).

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
no heap capacity; full-build qualification is in progress.

No persistent incremental-cache contract is established. Objects are
current-build artifacts; source/configuration/package fingerprinting and
cross-build invalidation are not part of this interface format.

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

## Bounded write experiment

The experimental CLI can submit one payload write through an asynchronous
AmigaDOS packet while compiling the next module. The filesystem handler already
runs as a separate task; no extra worker process is created. Open, close and
rename remain synchronous, so only payload transfer can overlap CPU work.

The registry remains the sole owner of the immutable image. Its complete
allocation capacity must fit 64 KiB; larger images use the synchronous writer.
There is no queue copy. A stable module ID identifies the pending owner when the
registry table moves. Dependency reads can borrow that image while its write is
outstanding. A separate writer context and private reply port isolate reads,
paths and packet replies. The additional tracked allocation is a 34-byte port;
the retained image can add at most 64 KiB of heap overlap. Writer state and code
also increase linked reservations; measurements must include those costs.

Only an acknowledged complete write followed by close/rename publishes `.ofm`
and releases its image. The next store, final emission, rollback and cleanup
drain the write first. Preparation explicitly drains before reporting success,
including an interface-only last module. Failures remain failures through
cleanup. There is no unsafe cancellation or generic allocation-failure retry;
this first experiment uses one fixed allowance rather than a global memory
pressure scheduler. Serialized objects retain offsets and numeric identities.

Native component checks cover short writes, errors, foreign messages, pending
lease preservation, draining release and real DOS output. Full CLI qualification
and runtime/peak-memory comparisons are pending. The synchronous checkpoint is
preserved as the reference. A separate synchronous-payload control retains the
same image lifetime and delayed publication, so measured gains cannot conflate
write overlap with dependency reads served from that retained image. It is a
frozen experimental source variant, not another maintained selection switch.
