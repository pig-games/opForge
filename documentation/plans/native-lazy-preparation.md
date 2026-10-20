# Preparation ownership and capture storage

This note records maintained capture and ownership contracts relevant to module
compilation. The active architecture direction is individual `.ofm` objects followed by
one bulk link; see [per-module objects and bulk linking](native-fragment-preparation.md).
Full 8 MiB fit is unproven. The budget is 6.5 MiB for the process plus 1.5 MiB for
AmigaOS.

## Maintained storage policies

| Mechanism | Contract |
|---|---|
| Resident capture | Default; completed captures may retire after owned publication. |
| Complete-graph replay | Disk inputs retain the same semantic engine and closed schedule. |
| `--capture-dir DIR` | Selects disk capture, prepared backing and lexical backing; use a real disk volume. |
| Raw capture | Records are contiguous within their backing extents; the logical arena need not be one allocation. |
| Prepared backing | Checked module-bounded views and bounded payload cache through finalization/reachability. |
| Lexical backing | Stable numeric identities, owned reader copies and bounded page cache. |
| Compiled eligible modules | Individual immutable `.ofm` files; metadata-only layout and one payload read at final Hunk emission. Full frontend coverage remains incomplete. |

## Capture acquisition and retirement

Chunk directories may move, but logical offsets do not. Physical file boundaries and
dedicated large records seal writers. Disk capture spools immutable chunks to one owned
DOS file; its 1 MiB payload budget includes writers and counted reader leases, not
executable, stack, directories or OS.

Eviction skips pins. Checked reload may move payload; I/O failures, oversized records and
pin exhaustion fail explicitly. `T:` may be RAM and does not demonstrate disk memory
savings. Retirement validates disjoint ranges and pins before mutation. Fully retired
chunks release payload and become checked tombstones; partially shared chunks stay until
every live byte retires. Views cannot cross retirement or reload retired data. Normal
cleanup removes owned files; a crash may leave private scratch for recovery.

## Configuration, publication and includes

Configuration and semantic frontends own separate scratch and graph state. Switching
restores shared VM control at a stable frontend address; borrowed lines do not cross a
switch. Complete immutable parameter snapshots transfer by spelling; repeated imports
must agree on keys and values. Consumed section maps retain their state.

Ordinary replay owns numeric records, values and binary templates before graph
acknowledgement and capture retirement. Active includes suspend the parent cursor and
preserve origins, expansion context and definition-relative paths. Inactive branches and
unexpanded templates do not acquire files. Active file effects occur once; template
bodies/defaults have explicit ownership.

The CLI builds an owned request pool before retiring configuration/discovery and compiles
an eligible dependency-ordered cohort. The object store uses `add` to retain the pending
full image and copy the linker
prefix; pending state supplies carrier extraction; store writes a named `.ofm` once and
retires pending ownership. The metadata-only layout directory retains Header, BlockRows
and RelocRows, while compact public semantic owners remain available for later compiles.
`.ofm` deliverables survive successful cleanup. Full per-module frontend coverage for
generic includes, templates and remaining closure is still ahead.
Names put the module leaf before its parent qualification, shortening to the classic
30-byte limit and appending a unique build-local ID. See the
[object lifecycle note](native-fragment-preparation.md#current-slice-directory-and-object-lifecycle)
for the exact naming policy.

## Ownership and verification boundaries

Reader APIs return owned entry/name copies; mutations write back explicitly, and page
pointers do not escape leases. Public/interface facets and later-needed metadata remain
owned. Numeric serialized identities and offsets contain no process pointers. Dirty
writeback precedes transfer of an unpinned page buffer; failed flush/reload preserves
explicit owners and sticky I/O errors. Names retire at the post-binding frontier; entries
survive later numeric consumers.

`materializePreparedStore` reconstructs one contiguous global ordered `Records` stream,
then retires the prepared store and index before assembly passes. The retained global
`Records` stream remains resident during assembly; the private module image cache is a
separate owner and remains available for fragment runtime. `beginSizing` remains a
no-growth boundary. These bridge details do not establish the target per-object lifetime.

`acquireEmit` is the only payload read path and runs at final Hunk emission with one
current module cached. The read gate allows zero reads for unlinked or BSS-only modules
and at most one read per selected module. Check writes, reads, object survival and zero
private ownership after cleanup. Preserve deliverables separately from ephemeral capture
files. Do not claim a numeric memory benefit until workspace and retained metadata
capacities are measured; the new path has no full-build qualification yet. See the
[measurement report](native-performance-diagnosis.md#current-assignment-and-decision-points)
for current proof limits.
