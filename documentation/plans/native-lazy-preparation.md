# Preparation ownership and capture storage

Current maintained policies, 2026-10-19. The [measurement report](native-performance-diagnosis.md#current-assignment-and-decision-points)
owns the confirmed 6.5/1.5 MiB process/OS budget, current results and decisions.
Parity remains paused; full-build 8 MiB fit is unproven.

The [first-reference module proposal](native-fragment-preparation.md#first-reference-module-assembly-proposal)
has standalone image/publication/leaf components. Its first CLI converter failed
the resource gate and was removed; it does not change the storage policies below.
Global binding, layout, reachability and emission remain authoritative. The
private leaf producer now preserves owned scalar/list/schema exports after its
frontend retires. It also saves independently relocatable blocks from a shared
section using exact label ownership and package-owned branch replay; no CPU
semantics move into the producer. These components do not retire any additional
CLI owner. Address/import mapping and template dependency closure still prevent
general CLI image publication.

## Maintained storage policies

| Mechanism | Current decision |
|---|---|
| Resident capture | Default. Dependency-first publication can retire completed captures before later acquisition. |
| Complete-graph replay | Used for disk inputs, explicit closed manifests and graphs closing before first publication. Same semantic frontend. |
| `--capture-dir DIR` | One disk policy for capture, prepared records and lexical backing; no selector stack. Use a real disk volume. |
| Raw capture | 32 KiB shared chunks and dedicated large records. |
| Prepared records | Disk payload cache capped at 128 KiB through global finalization/reachability; ordered contiguous input reconstructed after frontend retirement. |
| Lexical entries/names | Resident entries with paged names, or disk sealing behind stable IDs and a 128 KiB cache. Readers return owned copies. |
| Module metadata | Owner/origin/visibility arrays, typed values, schemas, templates and import state remain build-wide. |
| Macro-only deferral/audits and first-use switches | Removed; no demonstrated whole-build benefit. Ordinary binary macro semantics remain. |
| Include-text reuse and prepared leaf cache | Removed; insufficient savings or incomplete equivalence. |
| Instruction-only closed-module CLI converter | Removed after no heap benefit; checkpoint `cf78c259` is its recovery point. |

## Capture acquisition and retirement

Writers use 32 KiB chunks. Records larger than 4 KiB receive dedicated allocations
rounded to eight bytes. Dedicated records and physical file boundaries seal writers.
Directory rows may move; logical offsets never do. Each record stays contiguous.
Sequential resolution caches a row; other resolution uses binary search.

Disk mode spools immutable chunks to one private DOS file. Its 1 MiB payload budget
includes writers and counted reader leases. Directory, temporary records, executable,
stack and OS are outside that budget. Eviction skips pins; checked reloads can move
payload. I/O failure, oversized records and pin exhaustion fail explicitly rather
than falling back to unbounded RAM. `T:` may be RAM and is not a memory-saving disk.

Retirement validates disjoint ranges and pinned rows before mutation. Fully retired
chunks release payload and become checked tombstones; partially shared chunks stay
until all live bytes retire. Views cannot cross retired boundaries or reload retired
payload. Scratch-file length need not shrink when RAM ownership retires. Normal
cleanup removes only owned files; crashes may leave private files for recovery.

## Publication, configuration and includes

The scheduler preserves dependency and source order. Configuration and semantic
frontends own separate state. Switching restores shared VM control at a stable
frontend address; no borrowed line crosses a switch. Complete immutable parameter
snapshots transfer by spelling. Repeated imports require equal key/value sets,
independent of key order. Consumed section maps retain their state.

Successful replay owns records, values and templates before graph acknowledgement
and capture retirement. Active includes suspend their parent cursor and preserve
origin/expansion context. Resident interleaved replay retires included captures
before parent continuation; complete-graph paths keep them until capture cleanup.
Inactive branches and unexpanded templates do not acquire files. Definition-relative
paths survive for later active includes; templates own their binary bodies/defaults.

After configuration closes, final snapshots seed before configuration scratch,
graph and physical discovery retire. Disk policy retains complete-graph scheduling:
overlapping two growing sessions previously increased its peak. Failure cleanup
owns every partial session/allocation.

## Sequential lexical storage sealing

Publication seals the newly appended lexical range. Public/interface facets remain
retained; private entry/name payloads move to checked backing offsets. An eight-byte
row per global identity resolves entry/spelling locations. Successful publication
frees active arenas. Serialized IDs/offsets contain no process pointers.

Closing a module does not close its semantic dependencies. Later imports, templates,
members and binding still read/change private metadata. Callers receive owned
entry/name copies and mutations write back explicitly; page pointers do not escape
leases. Canonical-name sharing survives aliasing and retirement.

Four 32 KiB cache pages have single ownership. At capacity, dirty writeback precedes
transfer of an unpinned buffer's Pointer/Capacity, while logical Used extents remain
with their rows. Failed flush preserves ownership. Failed reload frees its destination
and reduces Reserved. Appends expose initialized bytes only. Errors are sticky and
checked before successful publication/cleanup; absence cannot hide I/O failure.
Names retire at the existing post-binding frontier; entries survive later numeric
consumers. The resident control uses the same APIs without spooling.

Buffer reuse removes allocation/clearing traffic, not retained ownership. Its
older frozen disk comparison saved 88.41% of allocation capacity while keeping
6,909,720 B peak. This storage boundary is not semantic finalization, local-ID
remapping or persistent `.ofm` serialization.

## Prepared records and the remaining frontier

Prepared-module views cap reads at module boundaries. Records stay whole, dirty
pages write before eviction and leases protect readers. Completion, layout, binding
and remapping traverse batches in original order. Late section recipes preserve
geometry until maps close. Reachability keeps whole-block decisions independently,
preserving internal-label fallthrough.

Runtime bindings, typed values, configuration and member IDs transfer before frontend
retirement. Ordered records are then reconstructed and omissions applied. Prepared
backing releases before assembly; `beginSizing` remains a no-growth boundary.
Validated resident identity layouts can reuse complete streams; reordered/partial
layouts copy.

Preparation still processes applicable bodies with full configuration semantics.
A useful near-output module must own exports, templates, local identities and all
remaining relocation/layout/observation recipes before source-oriented owners can
retire. The rejected instruction converter did not establish that closure. Its
small prepared-byte reduction did not reduce peak heap. The next semantic boundary
and acceptance conditions are in [the object proposal](native-fragment-preparation.md).
