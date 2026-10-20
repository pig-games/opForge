# Per-module `.ofm` objects and bulk linking

## Direction and current state

Compile eligible activated modules to inspectable individual `.ofm` files, retire their
private capture, tokens, plans and compiler owners, then link objects at the end. The
object boundary has focused native qualification; full per-module frontend coverage is
still ahead. The global paging bridge does not implement that complete direction.

The bridge at `c65db50c` is a working intermediate state. It supports disk-backed private
images in one capture file, but produces no actual per-module `.ofm` files. Current-source
expanded-memory full self-host completed at 421.786289125 s with 762,108 B exactly equal
to fresh Rust and bootstrap. The full memory run ended without a final receipt and the
reference release run was aborted. These provide no full net gain, budget fit or paired
performance claim. The import-row fix has focused native coverage, but no isolated
performance result. See the [measurement report](native-performance-diagnosis.md).

## Current slice: directory and object lifecycle

The directory is metadata-only: Header, BlockRows and RelocRows. In
the replacement flow, `add` retains the complete pending image and copies the linker
prefix. Pending state supplies carrier extraction. Store writes one `mXXXXXXXX.ofm` per
eligible module, then retires pending ownership. Compact public semantic owners remain
resident for later module compilation. Successful cleanup keeps `.ofm` files as inspectable deliverables, separate
from ephemeral capture files.

With `--capture-dir DIR`, files live in a freshly owned `DIR/objects` directory (or
`objects-N` when that name exists). Failed writes remove only their owned `.tmp` file;
rollback removes only its unpublished suffix. These are current-build link objects, not
independently reusable module-cache entries: external numeric identities still belong
to the active build. The container contains offsets and numeric values, never process
pointers.

`acquireEmit` is the only payload read path. It runs during final Hunk emission and caches
one current module. The deterministic gate permits zero payload reads for unlinked or
BSS-only modules and at most one read per selected module. Verify writes, selected-module
reads, object survival, exact binary/Hunk output and zero remaining private ownership.
This tests the object lifecycle; it is not a full-build resource claim.

## Compilation and linking boundary

The current path handles an eligible cohort. Extend the per-module frontend until full
self-host uses bulk compilation without quiet global fallback. Generic includes, templates
and remaining semantic closure still need complete per-module ownership. Constants,
types and exported binary macros needed by later
modules stay in a compact shared public interface; compiled payload does not. Unresolved
conditions, loops and relaxation stay in shared numeric metadata/recipes. Reach convergence
using metadata before streaming final payloads once.

The target resident model is shared runtime + maximum active module workspace + retained
link/export metadata + output buffer. It is not the total prepared payload of every
module. Keep an explicit reference path and measure actual owner capacities before making
net-savings estimates. Preserve package/VM ownership of CPU and encoding semantics, exact
symbol identity, relocations, layout/alignment, code/data/BSS and whole-block reachability.
Unsupported effects require explicit owned metadata or reference handling; partial
publication must not replay effects.

## Qualification

The current object path has no full-build qualification yet. Use fresh per-case Rust
oracles and exact native binary/Hunk outputs with fresh guest completion and explicit
exit. Keep release timing separate from memory instrumentation.
The budget remains 6.5 MiB process plus 1.5 MiB OS; full 8 MiB fit remains unproven.
Expanded 68020-74m proves functional execution only. The [measurement report](native-performance-diagnosis.md#current-assignment-and-decision-points)
owns current evidence and limits; generic backing lifetimes are in [capture acquisition
and retirement](native-lazy-preparation.md#capture-acquisition-and-retirement).
