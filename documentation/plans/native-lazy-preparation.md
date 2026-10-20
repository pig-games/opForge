# Preparation ownership and capture storage

This note records maintained capture and ownership contracts used by module compilation.
The active path publishes eligible current-build `.ofm` objects and owned in-memory
capsules for closed public macro, segment and statement facets. A full self-host has not
run on this slice; full performance comparisons remain user-paused.
The budget is 6.5 MiB for the process plus 1.5 MiB for AmigaOS. See [per-module objects and shared capsules](native-fragment-preparation.md).

## Maintained storage policies

| Mechanism | Contract |
|---|---|
| Resident capture | Default; completed captures may retire after owned publication. |
| Complete-graph replay | Disk inputs retain the same semantic engine and closed schedule. |
| `--capture-dir DIR` | Selects disk capture, prepared backing and lexical backing; use a real disk volume. |
| Raw capture | Records are contiguous within backing extents; the logical arena need not be one allocation. |
| Prepared backing | Checked module-bounded views and bounded payload cache through finalization/reachability. |
| Lexical backing | Stable numeric identities, owned reader copies and bounded page cache. |
| `.ofm` objects | Current-build link objects survive successful cleanup; ephemeral capture files have separate cleanup ownership. |
| Public capsules | Owned in-memory closed-public facets retain bodies, defaults, plans and names; they are not a persistent cache format. |

## Configuration and suspended source execution

Configuration and semantic frontends own separate scratch and graph state. Switching
frontends restores shared VM control at a stable frontend address; borrowed source lines
do not cross the switch. Immutable configuration snapshots transfer by spelling, and
repeated imports must agree on every key and value. Consumed section maps keep their
state.

An active include suspends the parent source cursor and preserves origin, expansion
context and definition-relative path. Inactive branches and unexpanded templates do not
open files. Active file effects run once; template bodies and defaults retain explicit
ownership before the capture retires.

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
cleanup removes owned scratch files; a crash may leave private scratch for recovery.

## Publication and ownership boundaries

Closed-public macro, segment and statement capsules retain binary bodies, defaults, plans
and names. They preserve caller binding and privacy, publish into normal shared pools,
then permit private frontend retirement. Interface-only producers create zero-block
`.ofm` objects; values and types stay in shared semantic owners without fake PCs/sections.
An interface-only producer with no effects needs no CPU.

The `.ofm` layout directory retains Header, BlockRows and RelocRows. Pending image state
carries linker-prefix copies and carrier extraction until store writes each named object
and retires pending ownership. `acquireEmit` reads only during final Hunk emission and
caches one current module. Unlinked or BSS-only objects can have zero reads; selected
payloads are read at most once. Deliverables survive successful cleanup while ephemeral
owners retire.

Reader APIs return owned entry/name copies; mutations write back explicitly, and page
pointers do not escape leases. Public/interface facets and later-needed metadata remain
owned. Numeric serialized identities and offsets contain no process pointers. Dirty
writeback precedes transfer of an unpinned page buffer; failed flush/reload preserves
explicit owners and sticky I/O errors. Names retire at the post-binding frontier; entries
survive later numeric consumers.

The existing `materializePreparedStore` bridge reconstructs one contiguous global ordered
`Records` stream, then retires the prepared store and index before assembly passes. That
global `Records` allocation remains resident during assembly; it is still an important
memory owner while the new per-module path is incomplete. The private module image cache
is a separate owner available for fragment runtime. `beginSizing` remains a no-growth
boundary.

The focused dependency, capsule and same-fragment cancellation checks pass under both
native policies. The four-case CLI matrix has eight passing native results. Exact output, admission,
payload-I/O, cleanup and geometry evidence is maintained in the
[measurement report](native-performance-diagnosis.md#current-assignment-and-decision-points)
and [native parity contract](../../agents/rules/native-rust-parity-porting.md).
