# Preparation ownership and capture storage

Current checkpoint, 2026-10-19. The [measurement report](native-performance-diagnosis.md#current-assignment-and-decision-points) owns the
confirmed 6.5/1.5 MiB process/OS budget and resource decision. Parity remains paused;
full-build 8 MiB fit is unproven.

The current private-module CLI candidate is **unpromoted**. It attempts eligible
configured modules before ordinary semantic publication, transfers one canonical public
interface and retains compact fragment images. The same-image full memory comparison
accepts 15 modules but peaks at 20,633,680 B versus 19,364,448 B reference, missing the
required at least 1 MiB net reduction. Smaller packed records and exact native output do
not establish a memory benefit.

Unsupported modules preserve complete ordinary replay. `--reference-modules` is the
explicit comparison control. This checkpoint is neither activation on first actual reference nor a persistent `.ofm` cache. The [module
proposal](native-fragment-preparation.md#first-reference-module-assembly-proposal)
remains an intended endpoint, not a promotion claim.

## Maintained storage policies

| Mechanism | Decision |
|---|---|
| Resident capture | Default storage; dependency-first publication can retire completed captures. |
| Complete-graph replay | Disk inputs and closed schedules retain the same semantic engine. |
| `--capture-dir DIR` | One disk policy for capture, prepared records and lexical backing; use a real disk volume. |
| Raw capture | 32 KiB shared chunks; dedicated large records. |
| Prepared backing | Checked module-bounded views and a 128 KiB payload cache through finalization/reachability. |
| Lexical backing | Stable numeric identities, owned reader copies and a 128 KiB page cache. |
| Experimental private candidate | Canonical public closure plus numeric fragment images; failed resource gate, unpromoted. |
| Removed experiments | Macro-only deferral/audits, first-use selectors, include-text reuse and prepared leaf caching. |

## Capture acquisition and retirement

Records remain contiguous. Chunk directories may move, but logical offsets do not.
Physical file boundaries and dedicated large records seal writers. Disk mode spools
immutable chunks to one owned DOS file; its 1 MiB payload budget includes writers and
counted reader leases, not executable, stack, directories or OS.

Eviction skips pins. Checked reloads may move payload; I/O failures, oversized records
and pin exhaustion fail explicitly. `T:` may be RAM and is not evidence of disk memory
savings. Retirement validates disjoint ranges and pins before mutation. Fully retired
chunks release payload and become checked tombstones; partially shared chunks remain
until every live byte retires. Views cannot cross retirement or reload retired data.
Normal cleanup removes owned files; crashes may leave private scratch files for
recovery.

## Configuration, publication and includes

Configuration and semantic frontends own separate scratch/graph state. Switching
restores shared VM control at a stable frontend address; no borrowed line crosses a
switch. Complete immutable parameter snapshots transfer by spelling; repeated imports
must agree on all keys and values. Consumed section maps keep their state.

Ordinary successful replay owns numeric records, values and binary templates before
graph acknowledgement and capture retirement. Active includes suspend the parent cursor
and preserve origins, expansion context and definition-relative paths. Inactive branches
and unexpanded templates do not acquire files. Active file effects occur once; template
bodies/defaults have explicit ownership.

The private candidate compiles a separate temporary frontend/assembly session. Canonical
public scalar/list/schema/value closure enters the global known-value owner once; hidden
nominal identities and member keys use explicit numeric maps. Private names and records
do not enter the global semantic namespace. Saved images retire their temporary semantic
payload after transfer. Unsupported inputs fall back before namespace publication;
post-publication failures are errors, not a second replay against partial ownership.

## Current lifetime limitation

Reference final-cohort replay retires configuration scratch, graph and discovery owners
before preparing those bodies. The private resident schedule instead drains the final
READY cohort while configuration remains live. Its maximum configuration can therefore
overlap retained semantic records/interfaces/images and a temporary private compiler.
This confirmed lifetime difference has no isolated byte attribution yet; admission is
measured at 15 modules, with no proven complete rejection breakdown.

The current private initializer still reads configuration module names, imports and
snapshots. Earlier retirement requires a reviewed ownership/remapping change; simply
moving a free is unsafe. No such redesign is planned before the failed-gate course
review. Disk policy keeps its existing complete-graph scheduling to avoid overlapping
two growing sessions. Failure cleanup owns every partial allocation/session.

## Lexical and prepared storage boundaries

Publication can seal newly appended lexical ranges. Reader APIs return owned entry/name
copies; mutations write back explicitly, and page pointers do not escape leases.
Public/interface facets and later-needed metadata remain retained. Numeric serialized
IDs/offsets contain no process pointers. Dirty writeback precedes transfer of an
unpinned page buffer; failed flush/reload preserves explicit owners and sticky I/O
errors. Names retire at the post-binding frontier; entries survive later numeric
consumers.

Prepared records remain whole and retain original traversal order. Late section recipes
survive until maps close. Reachability selects whole blocks, preserving internal labels
and fallthrough. Runtime values, configuration and member IDs transfer before frontend
retirement; ordered input is reconstructed afterward. Prepared backing releases before
assembly; `beginSizing` remains a no-growth boundary.

Buffer reuse reduces allocation traffic, not necessarily peak retained ownership. The
older frozen disk result saved 88.41% of allocation capacity while keeping a 6,909,720 B
heap peak; it is not a current full resident result. The current failed resource gate
and next decision are documented in the [measurement report](native-performance-diagnosis.md#private-module-cli-capacity-gate).
