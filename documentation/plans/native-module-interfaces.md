# Stored module assembly interfaces

## Current architecture

Ordinary native CLI assembly uses the bounded module pipeline. `opforge src`
selects `src/main.asm`, with `src` as the default module/include root, temporary
capture in working-directory `build/scratch`, and persistent objects in
working-directory `build/ofm`. Missing storage directories are created. Additional
include paths neither replace the default root nor select another cache. Includes
resolve relative to their containing files. Library callers leaving
`Frame.CaptureDirectory` null retain the resident reference route.

Each admitted private module publishes a current-build `.ofm` containing linker
data and a pointer-free assembly interface. The producer closes values and
templates before publication; consumers resolve numeric bindings through the
shared provider and registry on demand. The final linker owns fragments,
placement, relocations and Hunk output. The root remains in the shared final
driver. The private compiler/image fallback and `--reference-modules` replay
policy remain distinct mechanisms; reference policy disables incremental reuse.

Current full-source proof and input identities are maintained in
[the native assembler state](native-runtime-reset.md#current-product-state) and
[runtime and memory status](native-performance-diagnosis.md). The latest stored
snapshot has **190 objects / 1,655,184 bytes**, with all 190 reused warm. This is
an object inventory, not a peak-memory measurement.

### Serialized layouts and identities

All fields below are 32-bit quantities. Offsets are object-relative or relative
to the containing region, never runtime pointers. Integrity tags and bounds checks
validate containers; they do not select VM versions or legacy executors.

| Region | Header | Contents |
| --- | ---: | --- |
| `OFMA` | 48 bytes | Total size; value/template offsets and sizes; dependency/import counts and offsets; filename arena offset and size |
| `OFMS` | 40 bytes | Total size; export/member counts and offsets; name and compound-value arenas |
| `OFTM` | 44 bytes | Total size; base; binding directory and names; template rows and closed preparation data |

`OFMA` dependency rows are 8 bytes: filename offset and length. Import rows are
8 bytes: zero-based external dependency ordinal and provider-local entity ID.
Template closure origins reserve zero for self and use external ordinal plus one.
`OFMS` export rows are 28 bytes (ID, name/length, kind, low/high payload and
provenance); member rows are 12 bytes (ID and name/length). `OFTM` rows are
28 bytes: closed capsule offset/size, capsule-local name ID, optional statement
metadata offset/size, provider-local template entity and public/private visibility.
The authoritative layouts are
[assembly_interface.asm](../../native/motorola68000/amigaos/assembler/modules/assembly_interface.asm),
[semantic_exports.asm](../../native/motorola68000/amigaos/assembler/modules/semantic_exports.asm)
and [templates.asm](../../native/motorola68000/amigaos/assembler/modules/templates.asm).

Provider-local numeric IDs preserve nominal schema/member identity through
compound cells and template closure. Alias spellings are consumer-local bindings;
equal contents or name hashes do not establish identity. Exact spelling comparison
resolves hash collisions. Public value and template facets remain independent.
Provider declaration binding is separate from caller arguments and template
hygiene; signatures, defaults and declaration context survive publication.

## Bounded consumer access

The reader owns one 64 KiB window. Value and template carriers are independently
capped at 64 KiB each. An active consumer's module table has 8-byte rows and its
binding table has 24-byte rows, each capped at 64 KiB. These exclude the registry's
resident link metadata.

| Per-consumer record | Fields | Bytes |
| --- | --- | ---: |
| Module | Local lexical module ID, stored object ID | 8 |
| Binding | Provider, entity, consumer ID, kind, wrapper, ready state | 24 |

Bindings materialize only on value/member/template demand; provider interfaces
remain immutable. Private lexical scratch spills combined active entry/name
payload at 64 KiB. Its prepared-store payload cache is capped at 128 KiB (four
32 KiB pages). The header reader serves covered object-header ranges from retained
link metadata and validates the assembly directory through the existing window,
without additional heap capacity.

These are consumer bounds. The producer still temporarily overlaps full semantic
and facet owners, the combined interface and final image. It is not a streaming
writer and does not guarantee a 512 KiB combined replacement ceiling. Interface
construction is transactional: validation failure leaves the destination empty;
borrowed input owners remain valid through construction and transfer occurs only
on success.

## Supported semantic boundary

The interface preserves supported public values, nominal members, closed templates
and their declaration dependencies. `.use` selection controls visibility rather
than block liveness: unreferenced `.block` units are omitted regardless of import
selection. A reference to the preceding block label or a qualified symbol inside
it retains the complete block, including internal labels and fall-through.
Supported address/member forms survive stored compound values; compound
symbolic-address cells remain unsupported and fail closed. Unsupported private
admission uses the existing fallback rather than inventing interface semantics.
No CPU, family, encoding, placement or relaxation meaning belongs in this layer.

Object payload, cleanup and numeric fixup contracts are in
[per-module objects](native-fragment-preparation.md); capture and retirement rules
are in [preparation ownership](native-lazy-preparation.md).

## Incremental storage and invalidation

`--incremental-dir DIR` selects another persistent cache; `--rebuild` forces fresh
module assembly. An `.ofi` companion records acquisition dates/sizes, dependency
build generations, configuration identity and the pointer-free publication
inventory needed to restore section/linker bindings. The object/interface layouts
remain unchanged.

Freshness requires exact source timestamps and sizes, not hashes or content
comparison; backdated edits invalidate too. The acquired input set must still
match, so insertion of an earlier include-search candidate cannot reuse the old
selection. A rebuilt dependency invalidates consumers even within one timestamp
tick. Tool/package compatibility uses binary identities: an identical rebuilt
assembler does not miss merely because its file date changed.

The conservative key includes the complete ambient configuration/lexical identity
table and ordered source-origin/path inventory. Unrelated configuration changes
may therefore miss. Closed templates retain numeric declaration origins, so an
include ordinal cannot silently bind to another path. Discovery canonicalizes
candidate paths before assigning opaque origins, preserves the entry file in slot
zero and leaves explicit search-root order and dependency execution order intact.

A hit skips private module compilation, restores owner/section inventory, rebinds
provider-local imports to fresh linker IDs and enters the ordinary registry.
Discovery, configuration and final linking still execute; modules outside private
admission, including the shared root driver, execute normally.

Disk-backed builds use a single persistent `build/ofm` object set for reuse and
final linking. Cache hits and persisted rebuilds transfer the existing image to
the linker registry without another write; unchanged object timestamps remain
untouched. Resident builds retain optional cache writes. Component requests without
persistent caching use a stable `objects` directory under their capture parent.

Replacement uses the shared complete-temporary-file transport. The `.ofi` marker
is invalidated before image mutation and published last. Missing, stale or
structurally invalid data rebuilds; optional metadata failure leaves a module
uncached. Startup cache unavailability permits ordinary resident or scratch-backed
assembly. Once the disk-backed registry adopts the shared directory, invalidation
or object-write failure fails the build, rather than pairing stale metadata with
changed bytes. Registry rollback does not delete adopted persistent objects.
Reuse becoming unavailable after disk configuration still invalidates the matching
marker before replacement. **One writer per directory is required.** Deleting
`build/ofm` resets the cache.

Names fit classic 30-byte filename limits: a 21-byte leaf-first stem, bounded
parent qualification, a four-hex-digit unique registry ID and `.ofm` suffix.
Long leaves retain their final 21 bytes. Unique IDs disambiguate sanitization,
case folding and truncation; full module/configuration identities in the key
validate reuse. Discovery-order changes can cause safe misses. `.ofi` replaces
the final suffix byte. The implementation is
[object_store.asm](../../native/motorola68000/amigaos/assembler/modules/object_store.asm)
and [cache.i](../../native/motorola68000/amigaos/assembler/modules/cache.i).

### Incremental qualification

Focused native checks cover forced rebuild, dependency edits, timestamp-only
edits, template declaration origins after include insertion, and discovery
candidate permutations/duplicates with entry priority retained. Full-source cold
and warm output parity and current measurements live in
[runtime and memory status](native-performance-diagnosis.md); this reference does
not duplicate older bundle-specific timing or heap tables.

### 8 MiB Workbench qualification

The current external-package bare-CLI pair completes cold and warm under the
8 MiB Workbench profile with a 64 KiB stack, exact complete Rust output and full
warm object reuse. Peak heap was not remeasured for that bundle. See
[current product state](native-runtime-reset.md#current-product-state) for the
qualified bundle and [runtime and memory status](native-performance-diagnosis.md)
for authoritative measurements. The engineering budget remains 6.5 MiB process
plus 1.5 MiB OS; this is not a 2 MiB machine claim or a guarantee for arbitrary
desktop workloads.

## Storage scheduling decision

Object publication remains synchronous. Background payload writing and a public
name-index trial showed no demonstrated full-build benefit and their machinery
was removed. `OFMS` retains its 40-byte header and linear export scan. Revisit
these choices only with reproducible benefit sufficient to justify added ownership
and format complexity. Current remaining limits are producer overlap, acquisition
work before reuse, conservative configuration invalidation and the semantic forms
that still require fallback; further work should target those measured boundaries.
