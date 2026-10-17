# Preparation ownership and capture storage

Status, 2026-10-17: module discovery/configuration can now publish dependency-complete
modules through the shared semantic frontend with resident capture and retire their raw captures before
acquiring later dependencies. Complete graphs keep the existing scheduled path.
Bounded disk capture keeps complete-graph scheduling to avoid overlapping growing
session owners. Resident capture remains the default; `--capture-dir DIR` selects bounded disk
capture and staged growth of large prepared-record buffers. Lexical names are paged.
Parity remains paused. See [separate resource results](native-performance-diagnosis.md)
and the [implemented boundary and remaining object design](native-fragment-preparation.md).

## Maintained decisions

| Mechanism | Decision |
|---|---|
| Dependency-first publication | Default for resident open-graph discovery; shared replay publishes owned numeric records/templates, then retires captures. Global binding/layout remain. No new selector. |
| Complete-graph replay | Retained for all bounded-disk inputs, explicit closed manifests and resident inputs closing before first publication. Uses the same replay helper and normal global completion. |
| Paged lexical names | Default storage: names stay contiguous within 64 KiB pages; entries hold numeric handles and diagnostics use checked resolution. |
| Disk-assisted record growth | With disk capture, large nonempty prepared buffers stage their used prefix before growth; normal growth remains for resident/small buffers. |
| Finished configuration/discovery | Release after final seeding, before remaining semantic replay. Include paths and independent I/O survive. |
| Identity record layout reuse | After full validation, keep an already contiguous complete stream. Reordered/partial selections still copy. |
| Bounded disk backing | One explicit directory/storage policy, not another language implementation or persistent cache. |
| Chunked capture | Sole raw-capture representation, shared by resident/disk modes. |
| Macro-only deferral, first-use candidates/plans, binary token prototype, fragment envelope | Removed with switches and experiment-only tests; no established runtime gain. |
| In-build include-text reuse | Removed: no preparation avoided, extra heap and no demonstrated speed gain. |
| Persistent prepared declaration leaves | Removed: only 1.73% fewer expression compilations, no peak benefit, no demonstrated speed gain, and incomplete observation equivalence. |

Normal packed macros, declaration context and shared VM/package boundaries remain.
Git is the archive for removed implementations. No macro/cache experiment switch stack
remains. The binary token prototype is not a prerequisite of this pipeline.

## Capture storage and lifetimes

The append-only store uses 32 KiB chunks; records larger than 4 KiB receive dedicated
allocations rounded to eight bytes. Dedicated allocations and physical file boundaries
seal shared writers. Regions can span chunks, and chunks can contain several regions.
Directory rows may move; logical offsets never do. A record stays contiguous.
Sequential resolution caches a row; other resolution uses binary search. There is
no per-token storage indirection or original-text replay.

Disk mode spools immutable chunks into one process-private DOS file. A 1 MiB payload
budget includes writers and counted reader leases; directories, temporary records,
executable, stack and OS buffers are outside it. Eviction skips pins and checked
reloads may use different physical addresses. Oversized records, pin exhaustion or
I/O failure produce an explicit error, never an unbounded RAM fallback.

Use an existing writable disk directory, such as
`--capture-dir Development:opforge-scratch`; `T:` may be RAM. The resident policy
performs no scratch-file I/O. Normal cleanup removes only owned paths. A crash can
leave private scratch files for manual recovery.

## Publication and retirement

The scheduler preserves dependency/source order and requests a module only when its
configuration dependencies are ready. Configuration and semantic frontends own
independent storage. Session switching restores shared VM state at a stable frontend
address. Immutable parameter snapshots seed incrementally; complete key sets and
values must agree across repeated imports. Consumed maps are not overwritten.

Shared replay owns every retained record, typed value and template before returning.
Only then is its graph node acknowledged and its raw range retired. During
interleaved resident replay, includes retire
their acquired ranges after replay, before the suspended parent resumes. Complete-graph
paths retain them until final capture cleanup. Definition-relative
lookup and macro declaration context remain unchanged. Final section-map geometry
uses retained numeric recipes, so a later importing module can still map an earlier
published logical section.

Retirement validates disjoint ranges and rejects pinned rows before mutation.
Fully retired rows release payload and become checked tombstones; partially shared
chunks retain live bytes. Views cannot cross retired boundaries. Disk payload is
not reloaded after full retirement, while spool offsets remain stable. Unselected
or still-shared capture gaps survive until final capture cleanup. This is not a
claim that all lexical storage retires immediately after each module.

Once configuration closes, the remaining schedule transfers to semantic ownership.
Final snapshots seed before configuration scratch/graph and physical discovery retire.
Origin paths and include roots remain for active includes. Both partial sessions have
normal owned failure cleanup. Existing capture phase markers now follow the active
session; no new observer or record schema was introduced.

## Retained state and the next boundary

This pipeline shortens source lifetimes; it does not avoid configuration or semantic
work. It does not yet replace configuration with a cheap structural-only scan.
Global semantic scratch, names, exports/templates and prepared records still grow
across modules. `frontend.complete` resolves binding proxies/member identities and
section recipes against the full build before ordinary layout/emission.

A real `.ofm` owner must replace build-global identities with object-local IDs,
checked offsets and explicit remapping. It must preserve unresolved expressions,
relocation/relaxation recipes and ordered preparation observations. Only after all
later consumers have transferred can source-oriented prepared state retire. A common
resident/disk object representation and sparse whole-block support remain future work.
The [object design](native-fragment-preparation.md) describes these constraints.

No 2 MiB fit is claimed. Resource comparisons report tracked heap independently
from executable/stack/OS costs; see the [current distance to target](native-performance-diagnosis.md#current-qualification-and-distance-to-target).
