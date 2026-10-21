# Preparation ownership and capture storage

This note records maintained capture and ownership contracts for module
compilation. Current qualification, budget, and result status lives in
[native runtime and memory diagnosis](native-performance-diagnosis.md).

## Storage policies

| Mechanism | Contract |
|---|---|
| Resident capture | Default; completed captures may retire after owned publication. |
| Complete-graph replay | Disk inputs retain the same semantic engine and closed schedule. |
| `--capture-dir DIR` | Selects disk capture, prepared backing, and lexical backing; use a real disk volume. |
| Raw capture | Records are contiguous within backing extents; the logical arena need not be one allocation. |
| Prepared backing | Checked module-bounded views; four 32 KiB pages cap resident payload at 128 KiB through finalization/reachability. |
| Lexical backing | Stable numeric identities and owned reader copies; private disk scratch spills combined entry/name payload at 64 KiB and uses the prepared store's 128 KiB cache. |
| `.ofm` objects | Current-build link objects survive successful cleanup; ephemeral capture files have separate cleanup ownership. |
| Stored interfaces | Candidate `.ofm` objects persist `OFMS` values and `OFTM` template/statement data for bounded, on-demand numeric resolution. |

## Configuration and suspended source execution

Configuration and semantic frontends own separate scratch and graph state.
Switching frontends restores shared VM control at a stable frontend address;
borrowed source lines do not cross the switch. Immutable configuration snapshots
transfer by spelling, and repeated imports must agree on every key and value.
Consumed section maps keep their state.

An active include suspends the parent source cursor and preserves origin,
expansion context, and definition-relative path. Inactive branches and unexpanded
templates do not open files. Active file effects run once; template bodies and
defaults retain explicit ownership before capture retires.

## Capture acquisition and retirement

Chunk directories may move, but logical offsets do not. Physical file boundaries
and dedicated large records seal writers. Disk capture spools immutable chunks to
one owned DOS file; its 1 MiB payload budget includes writers and counted reader
leases, not executable, stack, directories, or OS.

Eviction skips pins. Checked reload may move payload; I/O failures, oversized
records, and pin exhaustion fail explicitly. `T:` may be RAM and does not
demonstrate disk memory savings. Retirement validates disjoint ranges and pins
before mutation. Fully retired chunks release payload and become checked
tombstones; partially shared chunks stay until every live byte retires. Views
cannot cross retirement or reload retired data. Normal cleanup removes owned
scratch files; a crash may leave private scratch for recovery.

## Publication ownership

Candidate success writes `OFMS` semantic values and `OFTM` closed template and
statement facets into the `.ofm` object, then retires producer-local semantic and
facet owners. Consumers resolve stored identities on demand; they do not build a
second shared copy of every dependency. The reference/fallback path retains its
owned capsules and shared semantic installation. Dependency edges preserve
source order and aliases under both resident and disk storage. See the
numeric-interface and `.ofm` contracts in
[per-module objects and shared capsules](native-fragment-preparation.md).

The stored-interface reader and capture store are separate owners. Current
stored-interface contracts are in
[stored module interfaces](native-module-interfaces.md).

Reader APIs return owned entry/name copies; mutations write back explicitly, and
page pointers do not escape leases. Numeric serialized identities and offsets
contain no process pointers. Dirty writeback precedes transfer of an unpinned
page buffer; failed flush/reload preserves explicit owners and sticky I/O errors.
Names retire at the post-binding frontier; entries survive later numeric
consumers.

`materializePreparedStore` reconstructs a contiguous global ordered `Records`
stream, then retires the prepared store and index before assembly passes. That
`Records` allocation remains resident during assembly. The private module image
cache is a separate owner for fragment runtime; `beginSizing` remains a no-growth
boundary.
