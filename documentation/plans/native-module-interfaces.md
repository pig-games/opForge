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
no heap capacity. Fresh full-source qualification passed; the release pair saved
8.1% with unchanged tracked heap. Measurements and their frozen input identities
are in [runtime and memory status](native-performance-diagnosis.md).

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
