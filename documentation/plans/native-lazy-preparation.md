# Preparation ownership and capture storage

This note records maintained capture and ownership contracts relevant to module
compilation. The active direction is eligible per-module `.ofm` objects followed by bulk
linking. Full frontend coverage and full-build resource qualification remain incomplete.
The confirmed budget is 6.5 MiB process plus 1.5 MiB AmigaOS. See [per-module objects
and bulk linking](native-fragment-preparation.md).

## Maintained storage policies

| Mechanism | Contract |
|---|---|
| Resident capture | Default; completed captures may retire after owned publication. |
| Complete-graph replay | Disk inputs retain the same semantic engine and closed schedule. |
| `--capture-dir DIR` | Selects disk capture, prepared backing and lexical backing; use a real disk volume. |
| Raw capture | Records are contiguous within backing extents; the logical arena need not be one allocation. |
| Prepared backing | Checked module-bounded views and bounded payload cache through finalization/reachability. |
| Lexical backing | Stable numeric identities, owned reader copies and bounded page cache. |
| `.ofm` objects | Named deliverables survive successful cleanup; ephemeral capture files have separate cleanup ownership. |

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
cleanup removes owned files; a crash may leave private scratch for recovery. Successful
cleanup preserves named `.ofm` deliverables.

## Configuration and suspended source execution

Configuration and semantic frontends own separate scratch and graph state. Switching
reactivates shared VM control at the stable frontend address; borrowed lines never cross
the switch. Immutable configuration snapshots transfer by spelling and repeated imports
must agree. Active includes preserve the suspended parent cursor, expansion context and
definition-relative origins. Inactive branches and unexpanded templates acquire no files.
Ordinary replay transfers numeric records, values and templates before acknowledging
the graph and retiring its capture. Private compilation must own the same later-needed
facets before releasing its frontend; unavailable facets still force reference handling.

## Current ownership and verification boundary

The focused `binary_memory.asm` and macro-hygiene native cases each privately accept
one module, write/read one `.ofm`, and match fresh Rust binary/Hunk output. Forced-reference
runs accept zero modules; both policies clean up to zero ownership. The memory case uses
`binary_memory.asm` plus seven real includes (41,210 B unchanged). Current v20 host
output was 766,268 B, but current full self-host and resource comparison are pending.

The layout directory retains only Header, BlockRows and RelocRows. Pending image state
carries linker-prefix copies and carrier extraction until the store writes each object
once and retires pending ownership. Compact public semantic owners remain available for
later compilation. `acquireEmit` reads only at final Hunk emission and caches one current
module. Unlinked or BSS-only modules may have zero payload reads; selected modules may be
read at most once.

Reader APIs return owned entry/name copies; mutations write back explicitly, and page
pointers do not escape leases. Public/interface facets and later-needed metadata remain
owned. Numeric serialized identities and offsets contain no process pointers. Dirty
writeback precedes transfer of an unpinned page buffer; failed flush/reload preserves
explicit owners and sticky I/O errors. Names retire at the post-binding frontier; entries
survive later numeric consumers.

`materializePreparedStore` reconstructs one contiguous global ordered `Records` stream,
then retires the prepared store and index before assembly passes. The retained global
`Records` stream remains resident during assembly; the private module image cache is a
separate owner available for fragment runtime. This is existing bridge behavior, not the
final per-object lifetime. See the [measurement report](native-performance-diagnosis.md#current-assignment-and-decision-points)
for full qualification limits.
