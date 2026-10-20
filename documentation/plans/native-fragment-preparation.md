# Per-module `.ofm` objects and shared capsules

## Current implementation

Eligible modules compile to inspectable current-build `.ofm` link objects. Owned closed-public
capsules cover macros, segments and statements, preserving caller binding and privacy.
Interface-only producers create zero-block objects; values and types stay in shared
semantic owners without fake PCs or sections. Temporary template facets are owned in-memory
capsules, not a persistent cache format.

## Current slice: directory and object lifecycle

The resident layout directory contains Header, BlockRows and RelocRows metadata only.
`add` retains the pending full image and copies the linker prefix; pending state supplies
carrier extraction. Store writes one named `.ofm` per eligible module and retires pending
ownership. Compact public semantic owners remain resident for later module compilation.
Successful cleanup preserves `.ofm` files separately from ephemeral capture files.

Object filenames put the module leaf before its parent qualification. The 21-byte stem
keeps the leaf first, then as much parent qualification as fits; an overlong leaf keeps
its final 21 bytes. Unsafe characters become underscores, and a four-hex-digit build-local
ID prevents case-folding, shortening and sanitization collisions within the 30-byte classic
Amiga limit. With `--capture-dir DIR`, files live in a newly owned `objects` directory,
or `objects-N` if that name exists. Failed writes remove only their owned `.tmp`; rollback
removes only the unpublished suffix. Containers store numeric offsets and values, never
process pointers.

`acquireEmit` is the only payload read path. It runs during final Hunk emission and caches
one current module. The read contract allows zero reads for unlinked or BSS-only modules
and at most one payload read per selected module. Verify object writes, selected-module
reads, deliverable survival, exact artifacts and zero cleanup ownership.

## Focused proof and remaining work

Focused dependency, capsule and same-fragment cancellation checks pass under both native
policies. The four-case CLI matrix has eight passing native results. See the measurement
report for exact output, admission, payload-I/O and cleanup results.
The package-owned cancellation proof counts every fixup field. It accepts only one
resolved, untransformed target-relative field with exact input identities and target, under
the geometry guard, with no Hunk relocation. Layout freezes bytes only when source and
target labels belong to the same fragment. Extra or nonpositional fields remain unproven
and are rejected; this proof does not cover branch, multiple, mixed, transformed or
cross-fragment fixups. Existing replayable branch recipes remain covered by the geometry
regression. Generic synthetic field checks pass in both source orders against explicit
expected bytes; the existing package-encoding identity artifact matches live Rust. The counter-geometry and full
geometry cases pass. See the measurement report for current source/build identity,
unresolved audit cases and qualification limits.

Preserve package/VM ownership of CPU and encoding semantics, exact symbol identity,
relocations, layout/alignment, code/data/BSS and whole-block reachability. Unsupported
effects need complete owned metadata or reference handling; publication must not replay
effects against partial state. See the [measurement report](native-performance-diagnosis.md#current-assignment-and-decision-points),
[native parity contract](../../agents/rules/native-rust-parity-porting.md), and
[FS-UAE guide](../../agents/rules/fs-uae.md).
