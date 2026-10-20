# Per-module `.ofm` objects and bulk linking

## Current status

Eligible modules compile to inspectable `.ofm` objects, retire pending private state and
join a final bulk link. The object lifecycle has focused native qualification; full
per-module frontend coverage and current full-build qualification remain incomplete.

The latest focused fix covers `binary_memory.asm` and generated macro hygiene. Both
cases privately accept one module, write and read one `.ofm`, and match fresh Rust binary
and Hunk outputs. Forced-reference runs accept zero modules; cleanup ownership is zero
for both policies. `binary_memory.asm` plus seven real includes totals 41,210 source
bytes. Current v20 host output was 766,268 B. The complete current code has not passed a
full self-host yet. The [measurement report](native-performance-diagnosis.md) owns the
remaining evidence and qualification limits.

## Current slice: directory and object lifecycle

The resident layout directory contains metadata only: Header, BlockRows and RelocRows.
`add` retains the complete pending image and copies the linker prefix. Pending state
supplies carrier extraction. Store writes one named `.ofm` per eligible module and retires
pending ownership. Compact public semantic owners remain resident for later module
compilation. Successful cleanup preserves `.ofm` files separately from ephemeral capture
files.

Object filenames put the module leaf before its parent qualification, then shorten to the
classic Amiga 30-byte limit and add a unique build-local ID. The registry tracks the
filename and numeric metadata; the container stores offsets and values, never process
pointers. With `--capture-dir DIR`, object files live in the owned `objects` directory;
failed writes and rollback remove only their owned temporary or unpublished files.
The 21-byte stem keeps the leaf then its parent qualification; an overlong leaf keeps
its final 21 bytes. Unsafe characters become underscores and a four-hex-digit build-local
ID prevents shortening, sanitization and case-folding collisions. For example,
`experimental.amigaos.binary_frontend` becomes `binary_frontend.exper-0000.ofm`.
The directory uses `objects-N` if `objects` already exists. These are current-build link
objects; external numeric identities are not yet independently reusable cache identities.

`acquireEmit` is the only payload read path. It runs during final Hunk emission and caches
one current module. The read contract allows zero reads for unlinked or BSS-only modules
and at most one payload read per selected module. Focused native cases verify one write
and one read for each privately selected module, zero accepted modules in forced-reference
mode, exact fresh Rust artifacts, and zero cleanup ownership.

## Compilation boundary

The current path covers an eligible cohort. Extend the per-module frontend to generic
includes, templates and remaining effects until full self-host uses bulk compilation
without quiet global fallback. Constants, types and exported binary macros required by
later modules remain in compact shared public semantic owners; compiled payload does not.
Unresolved conditions, loops and relaxation stay in shared numeric metadata/recipes and
converge before final payload streaming.

Two independent blockers in `binary_memory.asm` are repaired: structural projection
mismatches no longer poison otherwise absolute instruction geometry, and generated
macro hygiene scopes no longer claim runtime address values. Named macro invocation
labels and nested real declarations remain values. Focused native regressions pass;
this does not establish resource savings or full fallback coverage. Public template
publication and interface-only modules remain substantive producer gaps.

## Qualification

There is no current-code full-build resource result. Full comparisons are paused. The
prior full self-host at `21c33d6b+cec` was exact but mixed bulk/fallback; it is not evidence
for the current code. The old 7,439,536 B frozen reference heap peak used another image
and is not a paired gain. The candidate heap run timed out without a valid receipt. The
budget remains 6.5 MiB process plus 1.5 MiB OS; full 8 MiB fit is unproven.

See the [native parity contract](../../agents/rules/native-rust-parity-porting.md),
[FS-UAE guide](../../agents/rules/fs-uae.md), and [current measurement report](native-performance-diagnosis.md#current-assignment-and-decision-points).
