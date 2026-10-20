# Per-module `.ofm` objects and shared capsules

## Current implementation

Eligible modules compile to inspectable current-build `.ofm` link objects. The producer
can also publish owned closed-public capsules for macros, segments and statements.
Capsules retain binary bodies, defaults, plans and names, preserve caller binding and
privacy, enter normal shared pools, then allow private frontend retirement. Interface-only
producers create zero-block `.ofm` objects. Values and types remain shared semantic
owners; no fake PC or section is created, and an interface without effects needs no CPU.
Temporary template facets are owned in-memory capsules, not a persistent cache format.

The metadata directory contains Header, BlockRows and RelocRows. `add` retains the pending
full image and copies the linker prefix; pending state supplies carrier extraction. Store
writes named objects and retires pending ownership. Public semantic owners remain
available to later modules. `acquireEmit` reads selected payloads only during final Hunk
emission and caches one current module. Unlinked and BSS-only modules may have zero reads;
each selected payload may be read at most once. Successful cleanup retains `.ofm`
deliverables and releases ephemeral ownership.

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

All seven dependency cases pass both native policies (14 results), with exact live Rust
binary/Hunk artifacts and zero remaining owned bytes. Candidate runs accept two modules;
forced-reference runs accept zero. Dependent value-only imports and combined
macro/segment/default/alias imports pass. Typed-interface cases without a CPU,
odd-statement cases and text-side-effect cases with zero publication also pass under both
policies. The flags-aware classifier resolves the proxy-scope kind-4 identity collision;
its focused identity case passes natively.

A v27 full-build audit started but was intentionally stopped when fallback remained. Its
89 captured private-compiler outcomes (46 OK, 41 EXECUTION_INVALID, 2 binding INVALID)
are an incomplete prefix, not native completion or full-build coverage. Full comparisons
remain paused; do not claim all fallback paths pass, nor infer full-build performance,
memory savings or 8 MiB fit. The next repair is package-owned position/relocation proof
coverage and remaining binding failures. First and repeated execution stop at stage 14,
`markInstructionRelocs`: 38 DBRA/DBF stops lack module-side retention of the selected
relative-fixup proof; three MOVE stops remain unexplained. First prove same-fragment
cancellation using the exact package-selected target. Cross-fragment replay requires a
generic field-fixup contract. Keep the normal root engine where needed and avoid
CPU-specific hard-coding. Resume frozen resource comparisons
after complete current full-native admission and correctness proof.

Preserve package/VM ownership of CPU and encoding semantics, exact symbol identity,
relocations, layout/alignment, code/data/BSS and whole-block reachability. Unsupported
effects need complete owned metadata or reference handling; publication must not replay
effects against partial state. See the [measurement report](native-performance-diagnosis.md#current-assignment-and-decision-points),
[native parity contract](../../agents/rules/native-rust-parity-porting.md), and
[FS-UAE guide](../../agents/rules/fs-uae.md).
