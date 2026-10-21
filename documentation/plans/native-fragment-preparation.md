# Per-module `.ofm` objects and numeric-interface contracts

## `.ofm` lifecycle

Eligible modules compile to inspectable current-build `.ofm` link objects.
Candidate success persists semantic values in `OFMS` and closed templates and
statements in `OFTM`; consumers remap numeric identities and demand only the
needed entities. Interface-only producers create zero-payload-block objects,
without synthetic PCs or sections. Producer-local semantic and facet owners are
retired after publication. In the reference/fallback path, owned capsules and
shared semantic installation retain their existing behavior. The stored `OFMA`
directory connects `OFMS` and `OFTM` to object-local dependency identities.
`--reference-modules` remains available; see the
[stored interface architecture](native-module-interfaces.md).

The resident layout directory contains Header, BlockRows, and RelocRows metadata
only. `add` retains the pending full image and copies the linker prefix; pending
state supplies carrier extraction. Store writes one named `.ofm` per eligible
module and retires pending ownership. `acquireEmit` reads payload only during
final Hunk emission and caches one current module. Unlinked/BSS-only objects may
need zero payload reads; selected payloads are read at most once. Successful
cleanup preserves `.ofm` deliverables and retires ephemeral capture owners.

Object names put the module leaf before parent qualification in a 21-byte stem;
unsafe characters become underscores, and a four-hex-digit build-local ID avoids
collisions within the 30-byte classic Amiga limit. With `--capture-dir DIR`, files
use a newly owned `objects` directory, or `objects-N` if occupied. Failed writes
remove only their owned temporary file; rollback removes only the unpublished
suffix. Serialized containers store numeric offsets and values, never pointers.

## Numeric-interface proofs

Scalar-pair proofs accept only the two reached scalar leaves in the same exact
fragment. The anchor owner must be zero or the source owner. Duplicate inputs are
deduplicated. No wire row is added.

Word/long data deltas require canonical local target/base IDs. Base and delta
fields must share the same exact fragment, and target must remain in the same
section. Values truncate to the output width. The existing Reloc row is reused
with a dedicated shared-program sentinel and endian; final link recomputes low
bits. The representation uses neither raw-value identity nor CPU-specific logic.

A signed-relative field is cancellable only when it is the sole untransformed,
resolved, canonical-target field and its fixup VM supplies offset, width, endian,
and PC bias. The field must have signed range semantics, `GeometryState` must be
clear, and the movement interval must fit without crossing a gap or alignment
boundary. Final source and target must remain in the same section and satisfy the
range. The Reloc row stays unchanged; package lowering owns all encoding meaning.

Dependency publication preserves original source order, including repeated
targets under aliases, for resident and disk storage. Each declared edge is
retained for ordering; the candidate path resolves stored entities on demand
instead of snapshotting every dependency into a duplicate shared semantic table.
The reference/fallback path keeps its established semantic snapshot behavior.

## Ownership boundaries

Keep package and VM ownership of CPU and encoding semantics, exact symbol
identity, relocations, layout/alignment, code/data/BSS, and whole-block
reachability. Unsupported effects require complete owned metadata or reference
handling; publication must not replay effects against partial state. For current
qualification results and limitations, see
[native runtime and memory diagnosis](native-performance-diagnosis.md); capture
and retirement contracts live in
[preparation ownership and capture storage](native-lazy-preparation.md).
