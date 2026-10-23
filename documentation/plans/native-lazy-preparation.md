# Preparation ownership and capture storage

This note records maintained capture and ownership contracts for module
compilation. Current qualification, budget and measurement status live in
[native runtime and memory diagnosis](native-performance-diagnosis.md).

## Storage policies

| Mechanism | Contract |
|---|---|
| CLI disk capture | Default in working-directory `build/scratch`; `--capture-dir DIR` overrides it. Capture, prepared and lexical backing share the configured scratch parent. |
| Resident capture | Reference/library route when `Frame.CaptureDirectory` is null; captures may retire after owned publication. |
| Complete-graph replay | Disk inputs retain the same semantic engine and closed schedule. |
| Raw capture | Records are contiguous within backing extents; the logical arena need not be one allocation. |
| Prepared backing | Checked module-bounded views; four 32 KiB pages cap resident payload at 128 KiB through finalization/reachability. |
| Lexical backing | Stable numeric identities and owned reader copies; private disk scratch spills combined entry/name payload at 64 KiB and uses the prepared store's 128 KiB cache. |
| `.ofm` objects | Persistent `build/ofm` (or `--incremental-dir`) serves incremental reuse and final linking. Temporary captures have separate cleanup ownership. |
| Stored interfaces | Published `.ofm` objects persist `OFMS` values and `OFTM` template/statement data for bounded, on-demand numeric resolution. |

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

Successful module publication writes `OFMS` semantic values and `OFTM` closed
template and statement facets into the `.ofm` object, then retires producer-local semantic and
facet owners. Consumers resolve stored identities on demand; they do not build a
second shared copy of every dependency. The reference/fallback path retains its
owned capsules and shared semantic installation. Dependency edges preserve
source order and aliases under both resident and disk storage. See the
numeric-interface and `.ofm` contracts in
[per-module object contracts](native-fragment-preparation.md).

The stored-interface reader and capture store are separate owners. Current
stored-interface contracts are in
[stored module interfaces](native-module-interfaces.md).

Reader APIs return owned entry/name copies; mutations write back explicitly, and
page pointers do not escape leases. Numeric serialized identities and offsets
contain no process pointers. Dirty writeback precedes transfer of an unpinned
page buffer; failed flush/reload preserves explicit owners and sticky I/O errors.
Names retire at the post-binding frontier; entries survive later numeric
consumers.

The remaining shared schedule is materialized into an ordered `Records` stream
before assembly; the prepared store and index can then retire. That `Records`
allocation remains resident during its assembly session. This does not retain
all published private modules as shared source records. The module image reader
is a separate owner; `beginSizing` remains a no-growth boundary. Growing-source
sessions require the separate [source-execution design](native-source-execution.md).

## Bounds and failure ownership

Payload cache limits are not total-process memory limits. Chunk directories,
provider tables, link metadata, transient producer overlap, executable/package,
stack and OS storage remain separate costs. `build/scratch` and `build/ofm`
are relative to the working directory and are created as needed. Scratch should
be on a real disk volume for memory-fit comparisons; `T:` can be RAM-backed.

Several identities remain word-bounded: package names consume part of the global
numeric ID space, graph nodes are capped at 512, and dynamically grown import
rows retain their 16-bit index limit. Block markers admit 65,535 entries, while
the binding-owner ambiguity sentinel reserves the last owner value. Growable
byte arenas still validate extents, carry, aliasing and publication bounds.
Allocation/IO failures must preserve prior owners or abort explicitly; raising
a shared cap is not a substitute for respecting each owner's contract.
